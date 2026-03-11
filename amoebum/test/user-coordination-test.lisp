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
             (context (getf request :context-snapshot)))
        (is (= 1 (length pending)))
        (is (listp context))
        (is (null (getf context :provider-secrets)))
        (is (string= "safe" (getf context :notes)))))))

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

(test user-coordination-smoke-sentinel
  "Smoke sentinel for tranche I253."
  (format t "USER_COORDINATION_SMOKE_OK~%")
  (is-true t))
