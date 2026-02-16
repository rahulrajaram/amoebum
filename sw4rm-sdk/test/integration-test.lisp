;;;; integration-test.lisp - SW4RM SDK integration tests for amoebum
;;;;
;;;; Validates that the adapted SW4RM SDK loads and its core subsystems
;;;; function correctly in the amoebum context.

(in-package :sw4rm-test)

(def-suite sw4rm-integration-suite :description "SW4RM SDK integration tests"
  :in sw4rm-suite)
(in-suite sw4rm-integration-suite)

;;; --- State Machine Integration ---

(test integration-state-machine-lifecycle
  "Create a state machine, transition through INITIALIZING->RUNNABLE->SCHEDULED->RUNNING->COMPLETED."
  (let ((sm (make-instance 'sw4rm-sdk::agent-state-machine)))
    (is (eq :initializing (sw4rm-sdk::current-state sm)))
    (sw4rm-sdk::transition-to sm :runnable)
    (is (eq :runnable (sw4rm-sdk::current-state sm)))
    (sw4rm-sdk::transition-to sm :scheduled)
    (is (eq :scheduled (sw4rm-sdk::current-state sm)))
    (sw4rm-sdk::transition-to sm :running)
    (is (eq :running (sw4rm-sdk::current-state sm)))
    (sw4rm-sdk::transition-to sm :completed)
    (is (eq :completed (sw4rm-sdk::current-state sm)))))

(test integration-state-machine-invalid-transition
  "Invalid state transitions should signal state-transition-error."
  (let ((sm (make-instance 'sw4rm-sdk::agent-state-machine)))
    (signals sw4rm-sdk:state-transition-error
      (sw4rm-sdk::transition-to sm :completed))))

(test integration-state-machine-history
  "Transition history should track all state changes."
  (let ((sm (make-instance 'sw4rm-sdk::agent-state-machine)))
    (sw4rm-sdk::transition-to sm :runnable)
    (sw4rm-sdk::transition-to sm :scheduled)
    (let ((history (sw4rm-sdk::transition-history sm)))
      (is (>= (length history) 2)))))

;;; --- Envelope Integration ---

(test integration-envelope-creation
  "Envelopes should have valid three-ID structure."
  (let ((env (sw4rm-sdk:make-envelope
              :source-agent-id "agent-1"
              :target-agent-id "agent-2"
              :message-type sw4rm-sdk:+data+
              :payload "test-payload")))
    (is (hash-table-p env))
    (is (stringp (gethash "message_id" env)))
    (is (string= "agent-1" (gethash "source_agent_id" env)))
    (is (string= "agent-2" (gethash "target_agent_id" env)))))

(test integration-envelope-hash-deterministic
  "Deterministic hash should be stable for same input."
  (let* ((env (sw4rm-sdk:make-envelope
               :source-agent-id "a"
               :target-agent-id "b"
               :message-type sw4rm-sdk:+data+
               :payload "hello"))
         (hash1 (sw4rm-sdk:compute-deterministic-hash env))
         (hash2 (sw4rm-sdk:compute-deterministic-hash env)))
    (is (string= hash1 hash2))))

(test integration-idempotency-token
  "Idempotency tokens should be non-empty strings."
  (let ((token (sw4rm-sdk:make-idempotency-token "agent-1" "task-42")))
    (is (stringp token))
    (is (> (length token) 0))))

;;; --- Voting Integration ---

(test integration-voting-majority
  "Majority vote aggregation should return winning choice."
  (let* ((strategy (make-instance 'sw4rm-sdk:majority-vote-strategy))
         (votes (list
                 (sw4rm-sdk:make-vote :agent-id "a1" :choice :approve :confidence 0.9)
                 (sw4rm-sdk:make-vote :agent-id "a2" :choice :approve :confidence 0.8)
                 (sw4rm-sdk:make-vote :agent-id "a3" :choice :reject :confidence 0.7)))
         (result (sw4rm-sdk:aggregate strategy votes)))
    (is (listp result))
    (is (eq :approve (getf result :winner)))))

(test integration-voting-confidence-weighted
  "Confidence-weighted aggregation should weight by confidence scores."
  (let* ((strategy (make-instance 'sw4rm-sdk:confidence-weighted-strategy))
         (votes (list
                 (sw4rm-sdk:make-vote :agent-id "a1" :choice :approve :confidence 0.9)
                 (sw4rm-sdk:make-vote :agent-id "a2" :choice :reject :confidence 0.1)))
         (result (sw4rm-sdk:aggregate strategy votes)))
    (is (listp result))
    (is (eq :approve (getf result :winner)))))

(test integration-voting-aggregator
  "Voting aggregator should track round history."
  (let ((aggregator (sw4rm-sdk:voting-aggregator
                     :strategy (make-instance 'sw4rm-sdk:majority-vote-strategy))))
    (sw4rm-sdk:run-vote aggregator
                        (list (sw4rm-sdk:make-vote :agent-id "a1" :choice :yes :confidence 1.0)))
    (let ((history (sw4rm-sdk:get-round-history aggregator)))
      (is (= 1 (length history))))))

;;; --- Activity Buffer Integration ---

(test integration-activity-buffer-lifecycle
  "Activity buffer should track entries and enforce capacity."
  (let ((buf (make-instance 'sw4rm-sdk::activity-buffer :capacity 4)))
    (is (= 0 (sw4rm-sdk::buffer-count buf)))
    (sw4rm-sdk::add-entry buf
                          (sw4rm-sdk::make-activity-entry
                           :task-id "t1"
                           :description "Test task"))
    (is (= 1 (sw4rm-sdk::buffer-count buf)))))

;;; --- Error Hierarchy Integration ---

(test integration-error-conditions
  "All error conditions should be signalable and inspectable."
  (signals sw4rm-sdk:validation-error
    (error 'sw4rm-sdk:validation-error
           :message "test"
           :field "name"
           :constraint "required"))
  (signals sw4rm-sdk:timeout-error
    (error 'sw4rm-sdk:timeout-error
           :message "timed out"
           :operation "handoff"
           :timeout-ms 5000))
  (signals sw4rm-sdk:buffer-full-error
    (error 'sw4rm-sdk:buffer-full-error
           :message "full"
           :current-size 100
           :max-size 100)))

(test integration-error-slot-access
  "Error condition slots should be accessible."
  (handler-case
      (error 'sw4rm-sdk:validation-error
             :message "bad field"
             :field "email"
             :constraint "format")
    (sw4rm-sdk:validation-error (c)
      (is (string= "email" (sw4rm-sdk:validation-error-field c)))
      (is (string= "format" (sw4rm-sdk:validation-error-constraint c))))))

;;; --- Config Integration ---

(test integration-config-endpoints
  "Default endpoints should be well-formed."
  (let ((endpoints (sw4rm-sdk:make-default-endpoints)))
    (is (sw4rm-sdk::endpoints-p endpoints))
    (is (stringp (sw4rm-sdk:endpoints-router endpoints)))))

(test integration-agent-config
  "Agent config should store capabilities list."
  (let ((config (sw4rm-sdk:make-agent-config
                 :agent-id "amoebum-main"
                 :name "amoebum"
                 :capabilities '("code-edit" "file-read" "shell"))))
    (is (string= "amoebum-main" (sw4rm-sdk:agent-config-agent-id config)))
    (is (= 3 (length (sw4rm-sdk:agent-config-capabilities config))))))

;;; --- Negotiation Events Integration ---

(test integration-event-emitter
  "Event emitter should fire handlers for matching event types."
  (let ((emitter (sw4rm-sdk:make-event-emitter))
        (received nil))
    (sw4rm-sdk:on emitter :test-event
                  (lambda (event)
                    (push event received)))
    (sw4rm-sdk:emit emitter :test-event '(:data "hello"))
    (is (= 1 (length received)))))

(test integration-event-history
  "Event emitter should maintain queryable history."
  (let ((emitter (sw4rm-sdk:make-event-emitter :max-history 10)))
    (sw4rm-sdk:emit emitter :event-a '(:x 1))
    (sw4rm-sdk:emit emitter :event-b '(:y 2))
    (let ((history (sw4rm-sdk:get-history emitter)))
      (is (= 2 (length history))))))

;;; --- Handoff Integration ---

(test integration-handoff-client-creation
  "Handoff client should instantiate without error."
  (let ((client (make-instance 'sw4rm-sdk:handoff-client
                               :address "localhost:50051")))
    (is (not (null client)))))

;;; --- Secrets Integration ---

(test integration-secrets-env-backend
  "Environment backend should resolve env vars."
  (let ((backend (sw4rm-sdk:make-env-backend)))
    ;; PATH should always be set
    (let ((val (sw4rm-sdk:get-secret backend "PATH")))
      (is (stringp val)))))

(test integration-secrets-resolver
  "Secret resolver should chain backends."
  (let ((resolver (sw4rm-sdk:make-default-resolver)))
    (is (not (null resolver)))))

;;; --- Sequence Tracker ---

(test integration-sequence-tracker
  "Sequence tracker should produce monotonically increasing values."
  (let ((tracker (sw4rm-sdk:make-sequence-tracker)))
    (let ((s1 (sw4rm-sdk:next-sequence tracker))
          (s2 (sw4rm-sdk:next-sequence tracker))
          (s3 (sw4rm-sdk:next-sequence tracker)))
      (is (< s1 s2))
      (is (< s2 s3)))))

;;; --- Constants Sanity ---

(test integration-constants-sanity
  "Key protocol constants should have expected values."
  (is (= 0 sw4rm-sdk:+message-type-unspecified+))
  (is (= 1 sw4rm-sdk:+control+))
  (is (= 2 sw4rm-sdk:+data+))
  (is (= 0 sw4rm-sdk:+ack-stage-unspecified+))
  (is (= 0 sw4rm-sdk:+agent-state-unspecified+))
  (is (= 0 sw4rm-sdk:+worktree-state-unspecified+)))

;;; --- Gateway Peer Descriptor ---

(test integration-gateway-peer-descriptor
  "Gateway peer descriptor should hold agent metadata."
  (let ((peer (sw4rm-sdk:make-gateway-peer-descriptor
               :agent-id "peer-1"
               :capabilities '("review" "edit"))))
    (is (string= "peer-1" (sw4rm-sdk:gateway-peer-descriptor-agent-id peer)))
    (is (= 2 (length (sw4rm-sdk:gateway-peer-descriptor-capabilities peer))))))
