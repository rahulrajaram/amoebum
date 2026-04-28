(in-package :amoebum/test)

;;; ============================================================
;;; I253: Inter-user Coordination and Delegation
;;; ============================================================

(def-suite user-coordination-suite :in amoebum-suite)
(in-suite user-coordination-suite)

(defmacro with-fresh-user-coordination (() &body body)
  `(let ((amoebum::*user-session-registry* (sw4rm-sdk:make-local-registry))
         (amoebum::*user-session->agent-id* (make-hash-table :test #'equal))
         (amoebum::*user-session->user-id* (make-hash-table :test #'equal))
         (amoebum::*user-agent->session-id* (make-hash-table :test #'equal))
         (amoebum::*user-negotiation-room-participants* (make-hash-table :test #'equal))
         (amoebum::*user-handoff-client* nil)
         (amoebum::*user-negotiation-client* nil)
         (amoebum::*user-handoff-sequence* 0)
         (amoebum::*user-handoff-details* (make-hash-table :test #'equal))
         (amoebum::*user-coordination-lock*
          (bordeaux-threads:make-lock "user-coordination-test-lock")))
     ,@body))

(test user-session-registration-creates-sw4rm-peer
  "Each user session should register as a SW4RM local peer with session-bound ID."
  (with-fresh-user-coordination ()
    (let ((config (amoebum:register-user-session-peer
                   "session-a"
                   :user-id "alice"
                   :capabilities '("code-review" "handoff"))))
      (is (typep config 'sw4rm-sdk:agent-config))
      (is (= 1 (sw4rm-sdk:local-registry-size amoebum::*user-session-registry*)))
      (let ((found (amoebum:find-user-session-peer "session-a")))
        (is (typep found 'sw4rm-sdk:agent-config))
        (is-true (search "session-a" (sw4rm-sdk:agent-config-agent-id found)))
        (is (equal '("code-review" "handoff")
                   (sw4rm-sdk:agent-config-capabilities found)))))
    (let ((peers (amoebum:list-user-session-peers)))
      (is (= 1 (length peers)))
      (is (string= "session-a" (getf (first peers) :session-id)))
      (is (string= "alice" (getf (first peers) :user-id))))))

(test handoff-requests-route-between-registered-users
  "Handoff requests should route from one session peer to another."
  (with-fresh-user-coordination ()
    (amoebum:register-user-session-peer "session-from" :user-id "alice")
    (amoebum:register-user-session-peer "session-to" :user-id "bob")
    (let* ((response (amoebum:handoff-between-users
                      "session-from"
                      "session-to"
                      "Need a second reviewer"))
           (handoff-id (getf response :handoff-id))
           (pending (amoebum:get-user-pending-handoffs "session-to")))
      (is (stringp handoff-id))
      (is (eq :pending (getf response :status)))
      (is (= 1 (length pending)))
      (let ((request (first pending)))
        (is (string= "session-from" (getf request :from-session-id)))
        (is (string= "session-to" (getf request :to-session-id))))
      (let ((accepted (amoebum:accept-user-handoff handoff-id)))
        (is (eq :accepted (getf accepted :status))))
      (is (null (amoebum:get-user-pending-handoffs "session-to")))
      (let ((completed (amoebum:complete-user-handoff handoff-id)))
        (is (eq :completed (getf completed :status))))
      (let ((status (amoebum:user-handoff-status handoff-id)))
        (is (eq :completed (getf status :status)))))))

(test completed-handoff-status-carries-mergeable-result-and-provenance
  "Completed handoffs should expose structured result, trace, and conversation-merge payloads."
  (with-fresh-user-coordination ()
    (amoebum:register-user-session-peer "session-from" :user-id "alice")
    (amoebum:register-user-session-peer "session-to" :user-id "bob")
    (let* ((response (amoebum:handoff-between-users
                      "session-from"
                      "session-to"
                      "Need a delegate reviewer"
                      :context '(:notes "preserve provenance")))
           (handoff-id (getf response :handoff-id))
           (_accepted (amoebum:accept-user-handoff handoff-id))
           (completed (amoebum:complete-user-handoff
                       handoff-id
                       :summary "Delegate landed a review plan."
                       :result-payload '(:summary "Delegate landed a review plan."
                                         :artifacts ("plan.md"))
                       :diagnostics '((:severity :warning :message "One flaky check remains"))
                       :budget-spent '(:tokens 42 :wall-time-ms 900)
                       :provenance '(:worker-id "delegate-7" :backend :codex)))
           (status (amoebum:user-handoff-status handoff-id))
           (merge (getf status :conversation-merge))
           (trace (getf status :delegation-trace)))
      (declare (ignore _accepted))
      (is (eq :completed (getf completed :status)))
      (is (string= "Delegate landed a review plan." (getf status :summary)))
      (is (equal '(:summary "Delegate landed a review plan."
                   :artifacts ("plan.md"))
                 (getf status :result-payload)))
      (is (= 1 (length (getf status :diagnostics))))
      (is (= 42 (getf (getf status :budget-spent) :tokens)))
      (is (listp trace))
      (is (= 3 (length trace)))
      (is (eq :completed (getf merge :status)))
      (is (string= "Delegate landed a review plan." (getf merge :summary)))
      (is (= 1 (length (getf merge :messages))))
      (is (string= "assistant"
                   (getf (first (getf merge :messages)) :role)))
      (is (string= "delegate-7"
                   (getf (getf (getf status :delegation-provenance) :outcome)
                         :worker-id))))))

(test rejected-handoff-status-carries-partial-result-diagnostics
  "Rejected handoffs should keep partial results and diagnostics for fallback handling."
  (with-fresh-user-coordination ()
    (amoebum:register-user-session-peer "session-from" :user-id "alice")
    (amoebum:register-user-session-peer "session-to" :user-id "bob")
    (let* ((response (amoebum:handoff-between-users
                      "session-from"
                      "session-to"
                      "Need a fallback reviewer"))
           (handoff-id (getf response :handoff-id))
           (rejected (amoebum:reject-user-handoff
                      handoff-id
                      "overloaded"
                      :rejection-code sw4rm-sdk:+overloaded+
                      :partial-result '(:summary "Collected partial notes."
                                        :notes ("failing-spec"))
                      :diagnostics '((:severity :error :message "delegate overloaded"))
                      :provenance '(:worker-id "delegate-9")))
           (status (amoebum:user-handoff-status handoff-id))
           (merge (getf status :conversation-merge)))
      (is (eq :rejected (getf rejected :status)))
      (is (equal '(:summary "Collected partial notes."
                   :notes ("failing-spec"))
                 (getf status :partial-result)))
      (is (= 1 (length (getf status :diagnostics))))
      (is (eq :rejected (getf merge :status)))
      (is (equal (getf status :partial-result)
                 (getf merge :partial-result)))
      (is (string= "delegate-9"
                   (getf (getf (getf status :delegation-provenance) :outcome)
                         :worker-id))))))

(test handoff-rejection-preserves-sw4rm-metadata
  "Rejected handoffs should keep rejection metadata and clear the pending queue."
  (with-fresh-user-coordination ()
    (amoebum:register-user-session-peer "session-from" :user-id "alice")
    (amoebum:register-user-session-peer "session-to" :user-id "bob")
    (let* ((response (amoebum:handoff-between-users
                      "session-from"
                      "session-to"
                      "Need a fallback reviewer"))
           (handoff-id (getf response :handoff-id))
           (rejected (amoebum:reject-user-handoff
                      handoff-id
                      "overloaded"
                      :rejection-code sw4rm-sdk:+overloaded+
                      :retry-after-ms 250
                      :redirect-to-agent-id "user/carol/session/session-carol"))
           (status (amoebum:user-handoff-status handoff-id)))
      (is (stringp handoff-id))
      (is (eq :rejected (getf rejected :status)))
      (is (string= "overloaded" (getf rejected :reason)))
      (is (= sw4rm-sdk:+overloaded+ (getf rejected :rejection-code)))
      (is (= 250 (getf rejected :retry-after-ms)))
      (is (string= "user/carol/session/session-carol"
                   (getf rejected :redirect-to-agent-id)))
      (is (null (amoebum:get-user-pending-handoffs "session-to")))
      (is (eq :rejected (getf status :status)))
      (is (= sw4rm-sdk:+overloaded+ (getf status :rejection-code)))
      (is (string= "overloaded" (getf status :rejection-reason)))
      (is (= 250 (getf status :retry-after-ms)))
      (is (string= "user/carol/session/session-carol"
                   (getf status :redirect-to-agent-id))))))

(test handoff-between-users-signals-missing-peer
  "Delegation should fail fast when the target peer is not registered."
  (with-fresh-user-coordination ()
    (amoebum:register-user-session-peer "session-from" :user-id "alice")
    (let ((message
            (handler-case
                (progn
                  (amoebum:handoff-between-users
                   "session-from"
                   "session-missing"
                   "Need help")
                  nil)
              (error (condition)
                (princ-to-string condition)))))
      (is (stringp message))
      (is (search "No user peer registered for session-id"
                  message
                  :test #'char-equal))
      (is (search "session-missing"
                  message
                  :test #'char-equal)))))

(test user-provider-secrets-are-agent-scoped-and-sanitized-in-delegation
  "Provider secrets should stay agent-local and be stripped from delegated context."
  (with-fresh-user-coordination ()
    (amoebum:register-user-session-peer
     "session-alice"
     :user-id "alice"
     :provider-secrets '(:openai_api_key "alice-openai-key"))
    (amoebum:register-user-session-peer
     "session-bob"
     :user-id "bob"
     :provider-secrets '((:openai_api_key . "bob-openai-key")))
    (let* ((alice-agent (sw4rm-sdk:agent-config-agent-id
                         (amoebum:find-user-session-peer "session-alice")))
           (bob-agent (sw4rm-sdk:agent-config-agent-id
                       (amoebum:find-user-session-peer "session-bob"))))
      (is (string= "alice-openai-key"
                   (sw4rm-sdk:local-registry-resolve-provider-secret
                    amoebum::*user-session-registry*
                    alice-agent
                    alice-agent
                    "OPENAI_API_KEY")))
      (signals sw4rm-sdk:provider-secret-access-denied
        (sw4rm-sdk:local-registry-resolve-provider-secret
         amoebum::*user-session-registry*
         bob-agent
         alice-agent
         "OPENAI_API_KEY"))
      (amoebum:handoff-between-users
       "session-alice"
       "session-bob"
       "Please review"
       :context '(:provider-secrets (:openai_api_key "do-not-leak")
                  :notes "safe"))
      (let* ((pending (amoebum:get-user-pending-handoffs "session-bob"))
             (request (first pending))
             (context (getf request :context-packet)))
        (is (= 1 (length pending)))
        (is (listp context))
        (is (string= "coding-task-context" (getf context :packet-kind)))
        (is (null (getf (getf context :extras) :provider-secrets)))
        (is (string= "safe" (getf (getf context :extras) :notes)))))))


(test negotiation-room-supports-multi-user-code-review
  "Negotiation room flow should support artifact submission and critique."
  (with-fresh-user-coordination ()
    (amoebum:register-user-session-peer "session-author" :user-id "author")
    (amoebum:register-user-session-peer "session-reviewer" :user-id "reviewer")
    (let ((room (amoebum:create-user-negotiation-room
                 "room-i253"
                 '("session-author" "session-reviewer")
                 :description "I253 code review room")))
      (is (string= "room-i253" (getf room :room-id)))
      (is (= 2 (length (getf room :participant-session-ids)))))
    (let ((artifact-id
            (amoebum:submit-user-negotiation-artifact
             "room-i253"
             "session-author"
             "artifact-1"
             "Patch for coordination feature."
             :requested-critic-session-ids '("session-reviewer")
             :timeout-ms 5000)))
      (is (string= "artifact-1" artifact-id))
      (is (null (amoebum:add-user-negotiation-critique
                 "room-i253"
                 "artifact-1"
                 "session-reviewer"
                 t
                 :score 0.98
                 :details "Looks good.")))
      (let ((decision (amoebum:wait-for-user-negotiation-decision
                       "artifact-1"
                       :timeout-s 1.0
                       :poll-interval-s 0.01)))
        (is (member (getf decision :outcome) '(:approved :rejected :escalated) :test #'eq)))
      (let ((status (amoebum:get-user-negotiation-room-status "room-i253")))
        (is (= 1 (getf status :completed-decisions)))
        (is (= 0 (getf status :pending-proposals)))
        (is (= 2 (length (getf status :participant-session-ids))))))))

(test negotiation-timeout-surfaces-sw4rm-rpc-timeout
  "Waiting without critiques should raise the SW4RM timeout error and preserve pending status."
  (with-fresh-user-coordination ()
    (amoebum:register-user-session-peer "session-author" :user-id "author")
    (amoebum:register-user-session-peer "session-reviewer" :user-id "reviewer")
    (amoebum:create-user-negotiation-room
     "room-timeout"
     '("session-author" "session-reviewer")
     :description "Timeout path room")
    (let ((artifact-id
            (amoebum:submit-user-negotiation-artifact
             "room-timeout"
             "session-author"
             "artifact-timeout"
             "Patch waiting for critique."
             :requested-critic-session-ids '("session-reviewer")
             :timeout-ms 50)))
      (declare (ignore artifact-id))
      (let ((message
              (handler-case
                  (progn
                    (amoebum:wait-for-user-negotiation-decision
                     "artifact-timeout"
                     :timeout-s 0.05
                     :poll-interval-s 0.01)
                    nil)
                (sw4rm-sdk::rpc-timeout (condition)
                  (princ-to-string condition)))))
        (is (stringp message))
        (is (search "Timed out waiting for decision"
                    message
                    :test #'char-equal))
        (let ((status (amoebum:get-user-negotiation-room-status "room-timeout")))
          (is (= 1 (getf status :pending-proposals)))
          (is (= 0 (getf status :completed-decisions)))
          (is (equal '("session-reviewer")
                     (getf status :active-critic-session-ids))))))))

(test user-coordination-events-feed-agent-activity-and-journal
  "SW4RM handoff and negotiation events should land in agent activity and the event journal."
  (with-fresh-user-coordination ()
    (let* ((original-bus amoebum:*event-bus*)
           (bus (amoebum:make-event-bus :capacity 256))
           (dir (merge-pathnames
                 (format nil "amoebum-user-coordination-journal-~D/" (get-universal-time))
                 #P"/tmp/"))
           (journal (amoebum:make-event-journal-instance
                     :directory dir
                     :max-segment-bytes 4096)))
      (unwind-protect
           (progn
             (setf amoebum:*event-bus* bus)
             (amoebum:clear-agent-activity-stream)
             (amoebum:ensure-agent-activity-stream bus)
             (amoebum:start-event-journal :journal journal :event-bus bus)
             (amoebum:register-user-session-peer "session-author" :user-id "author")
             (amoebum:register-user-session-peer "session-reviewer" :user-id "reviewer")
             (let* ((handoff (amoebum:handoff-between-users
                              "session-author"
                              "session-reviewer"
                              "Review this patch"))
                    (handoff-id (getf handoff :handoff-id)))
               (amoebum:accept-user-handoff handoff-id)
               (amoebum:complete-user-handoff handoff-id))
             (amoebum:create-user-negotiation-room
              "room-i253-events"
              '("session-author" "session-reviewer")
              :description "Event propagation room")
             (amoebum:submit-user-negotiation-artifact
              "room-i253-events"
              "session-author"
              "artifact-events"
              "Patch for event coverage."
              :requested-critic-session-ids '("session-reviewer")
              :timeout-ms 5000)
             (amoebum:add-user-negotiation-critique
              "room-i253-events"
              "artifact-events"
              "session-reviewer"
              t
              :score 0.99
              :details "Looks good.")
             (let* ((decision (amoebum:wait-for-user-negotiation-decision
                               "artifact-events"
                               :timeout-s 1.0
                               :poll-interval-s 0.01))
                    (author-entries (amoebum:list-agent-activity :agent-id "user/author/session/session-author" :limit 20))
                    (reviewer-entries (amoebum:list-agent-activity :agent-id "user/reviewer/session/session-reviewer" :limit 20)))
               (is (member (getf decision :outcome) '(:approved :rejected :escalated) :test #'eq))
               (is (find amoebum:+event-type-user-handoff-requested+
                         author-entries
                         :key #'amoebum:agent-activity-entry-source-event-type
                         :test #'eq))
               (is (find amoebum:+event-type-user-negotiation-artifact-submitted+
                         author-entries
                         :key #'amoebum:agent-activity-entry-source-event-type
                         :test #'eq))
               (is (find amoebum:+event-type-user-negotiation-critique-added+
                         reviewer-entries
                         :key #'amoebum:agent-activity-entry-source-event-type
                         :test #'eq))
               (amoebum:stop-event-journal journal)
               (let ((content (uiop:read-file-string (first (amoebum:journal-segment-paths journal)))))
                 (is (search "USER:HANDOFF-REQUESTED" (string-upcase content)))
                 (is (search "USER:NEGOTIATION-ARTIFACT-SUBMITTED" (string-upcase content)))
                 (is (search "USER:NEGOTIATION-DECISION" (string-upcase content))))))
        (setf amoebum:*event-bus* original-bus)
        (ignore-errors (amoebum:stop-event-journal journal))
        (ignore-errors
          (dolist (path (directory (merge-pathnames "*.jsonl" dir)))
            (delete-file path))
          (uiop:delete-empty-directory dir))))))

(test user-coordination-smoke-sentinel
  "Smoke sentinel for tranche I253."
  (format t "USER_COORDINATION_SMOKE_OK~%")
  (is-true t))
