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

(test handoff-context-packet-captures-structured-coding-state
  "Delegated coding handoffs should ship a structured context packet."
  (with-fresh-user-coordination ()
    (let* ((tmp-root (%make-temp-directory "handoff-context-packet"))
           (project-root (uiop:ensure-directory-pathname
                          (merge-pathnames #P"project/" tmp-root)))
           (global-memory (merge-pathnames #P"home/.amoebum/memory/MEMORY.md" tmp-root))
           (project-memory (merge-pathnames #P".amoebum/MEMORY.md" project-root))
           (backend (amoebum:make-file-memory-backend
                     :project-root project-root
                     :global-path global-memory
                     :project-path project-memory))
           (conversation (amoebum:make-conversation-state
                          :project-root project-root
                          :session-id "session-author"))
           (worktree (amoebum:make-worktree-metadata
                      :id "wt-378"
                      :branch "sw4rm/nxt-378/context-packet"
                      :path (namestring project-root)))
           (amoebum:*ide-context*
             (amoebum:make-ide-context
              :active-file "amoebum/src/swarm.lisp"
              :open-files '("amoebum/src/swarm.lisp" "amoebum/test/user-coordination-test.lisp")
              :selections '((:file "amoebum/src/swarm.lisp"
                             :start 740
                             :end 810
                             :text "handoff-between-users"))
              :diagnostics '((:severity "warning" :message "context packet missing")))))
      (unwind-protect
           (progn
             (amoebum:conversation-state-add-message
              conversation
              (pseudopod:make-message :role "user"
                                      :content "Please review the handoff seam.")
              :save-p nil)
             (amoebum:conversation-state-add-message
              conversation
              (pseudopod:make-message :role "assistant"
                                      :content "I am collecting git, memory, and worktree context.")
              :save-p nil)
             (amoebum:memory-store backend "handoff-policy" "Ship structured packets." :scope :project :source :test)
             (amoebum:memory-store backend "verification" "Run focused suites first." :scope :session :source :test)
             (amoebum:register-user-session-peer "session-author" :user-id "author")
             (amoebum:register-user-session-peer "session-reviewer" :user-id "reviewer")
             (amoebum:handoff-between-users
              "session-author"
              "session-reviewer"
              "Review the delegation packet"
              :context (list :conversation conversation
                             :memory-backend backend
                             :worktree worktree
                             :project-root project-root
                             :notes "keep context structured")
              :budget (list :deadline-epoch-ms (+ (sw4rm-sdk::current-time-ms) 10000)
                            :context-max-bytes 8192))
             (let* ((pending (amoebum:get-user-pending-handoffs "session-reviewer"))
                    (request (first pending))
                    (snapshot (getf request :context-snapshot))
                    (packet (getf request :context-packet))
                    (conversation-packet (getf packet :conversation))
                    (files-packet (getf packet :files))
                    (git-packet (getf packet :git))
                    (memory-packet (getf packet :memory))
                    (worktree-packet (getf packet :worktree)))
               (is (= 1 (length pending)))
               (is (stringp snapshot))
               (is (listp packet))
               (is (= 1 (getf packet :schema-version)))
               (is (string= "coding-task-context" (getf packet :packet-kind)))
               (is (listp conversation-packet))
               (is (= 2 (getf conversation-packet :entry-count)))
               (is (= 2 (length (getf conversation-packet :entries))))
               (is (string= "amoebum/src/swarm.lisp"
                            (getf files-packet :active-file)))
               (is (stringp (getf git-packet :branch)))
               (is (listp memory-packet))
               (is (>= (getf memory-packet :project-count) 1))
               (is (>= (getf memory-packet :session-count) 1))
               (is (string= "wt-378" (getf worktree-packet :id)))
               (is (string= "keep context structured"
                            (getf (getf packet :extras) :notes)))))
        (%delete-directory-tree-safe tmp-root)))))

(test handoff-context-packet-stays-deserializable-under-tight-budget
  "Budget-aware compression should keep the handoff packet structured and parseable."
  (with-fresh-user-coordination ()
    (let* ((tmp-root (%make-temp-directory "handoff-context-tight"))
           (project-root (uiop:ensure-directory-pathname
                          (merge-pathnames #P"project/" tmp-root)))
           (global-memory (merge-pathnames #P"home/.amoebum/memory/MEMORY.md" tmp-root))
           (project-memory (merge-pathnames #P".amoebum/MEMORY.md" project-root))
           (backend (amoebum:make-file-memory-backend
                     :project-root project-root
                     :global-path global-memory
                     :project-path project-memory))
           (conversation (amoebum:make-conversation-state
                          :project-root project-root
                          :session-id "session-author"))
           (amoebum:*ide-context*
             (amoebum:make-ide-context
              :active-file "amoebum/src/swarm.lisp"
              :open-files '("amoebum/src/swarm.lisp")
              :selections (loop repeat 8
                                collect (list :file "amoebum/src/swarm.lisp"
                                              :start 1
                                              :end 20
                                              :text (make-string 160 :initial-element #\S)))
              :diagnostics (loop repeat 8
                                 collect (list :severity "warning"
                                               :message (make-string 160 :initial-element #\D))))))
      (unwind-protect
           (progn
             (loop repeat 10
                   for index from 1 do
                     (amoebum:conversation-state-add-message
                      conversation
                      (pseudopod:make-message
                       :role (if (oddp index) "user" "assistant")
                       :content (make-string 220 :initial-element
                                             (if (oddp index) #\U #\A)))
                      :save-p nil))
             (loop repeat 8
                   for index from 1 do
                     (amoebum:memory-store backend
                                           (format nil "entry-~D" index)
                                           (make-string 120 :initial-element #\M)
                                           :scope :project
                                           :source :test))
             (amoebum:register-user-session-peer "session-author" :user-id "author")
             (amoebum:register-user-session-peer "session-reviewer" :user-id "reviewer")
             (amoebum:handoff-between-users
              "session-author"
              "session-reviewer"
              "Compress this handoff"
              :context (list :conversation conversation
                             :memory-backend backend
                             :project-root project-root
                             :notes (make-string 400 :initial-element #\N))
              :budget (list :deadline-epoch-ms (+ (sw4rm-sdk::current-time-ms) 10000)
                            :context-max-bytes 1400))
             (let* ((request (first (amoebum:get-user-pending-handoffs "session-reviewer")))
                    (snapshot (getf request :context-snapshot))
                    (packet (getf request :context-packet))
                    (conversation-packet (getf packet :conversation)))
               (is (stringp snapshot))
               (is (<= (length snapshot) 1400))
               (is (listp packet))
               (is (string= "coding-task-context" (getf packet :packet-kind)))
               (is (<= (length (or (getf conversation-packet :entries) '()))
                       (getf conversation-packet :entry-count)))
               (is (<= (length (getf (getf packet :memory) :project))
                       (getf (getf packet :memory) :project-count)))))
        (%delete-directory-tree-safe tmp-root)))))

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
