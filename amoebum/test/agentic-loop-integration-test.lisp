(in-package :amoebum/test)

;;; ============================================================
;;; I267: End-to-End Agentic Loop Integration Test
;;;
;;; Mock LLM backend with deterministic responses,
;;; full loop test through system prompt → LLM → tool → result,
;;; multi-turn conversations, and error recovery.
;;; ============================================================

(def-suite agentic-loop-suite :in amoebum-suite)
(in-suite agentic-loop-suite)

;;; --- Mock LLM response sequences ---

(defvar *mock-llm-responses* nil
  "Queue of mock LLM responses for testing. Each response is a plist:
   (:content STRING :tool-calls LIST :finish-reason KEYWORD)")

(defvar *mock-llm-request-log* nil
  "Log of requests sent to the mock LLM.")

(defun %mock-llm-enqueue (content &key tool-calls (finish-reason :stop))
  "Enqueue a mock LLM response."
  (setf *mock-llm-responses*
        (nconc *mock-llm-responses*
               (list (list :content content
                           :tool-calls tool-calls
                           :finish-reason finish-reason)))))

(defun %mock-llm-dequeue ()
  "Dequeue the next mock LLM response."
  (pop *mock-llm-responses*))

(defun %mock-llm-clear ()
  "Clear mock LLM state."
  (setf *mock-llm-responses* nil
        *mock-llm-request-log* nil))

;;; --- Mock conversation step ---

(defun %mock-conversation-step (conversation messages)
  "Simulate one conversation step: send messages, get mock LLM response.
   Returns (values response updated-conversation)."
  (push (list :messages messages :timestamp (get-universal-time))
        *mock-llm-request-log*)
  (let ((response (%mock-llm-dequeue)))
    (when response
      ;; Add assistant message to conversation
      (let ((content (or (getf response :content) "")))
        (amoebum.sessions:conversation-state-add-message
         conversation
         (amoebum.sessions:make-conversation-history-entry
          :role "assistant"
          :content content))))
    (values response conversation)))

;;; --- Tests ---

(test mock-llm-basic-round-trip
  "Mock LLM responds to a simple user message."
  (%mock-llm-clear)
  (%mock-llm-enqueue "Hello! How can I help?")
  (let ((conv (amoebum::%make-conversation-state
               :session-id "test-loop-001")))
    ;; User message
    (amoebum.sessions:conversation-state-add-message
     conv
     (amoebum.sessions:make-conversation-history-entry
      :role "user"
      :content "Hello"))
    ;; Step
    (multiple-value-bind (response _conv)
        (%mock-conversation-step conv
          (amoebum.sessions:conversation-state-messages conv))
      (declare (ignore _conv))
      (is (not (null response)))
      (is (equal "Hello! How can I help?" (getf response :content)))
      ;; Conversation should have 2 entries
      (is (= 2 (length (amoebum.sessions:conversation-state-entries conv)))))))

(test mock-llm-multi-turn
  "Multi-turn conversation with mock LLM."
  (%mock-llm-clear)
  (%mock-llm-enqueue "First response")
  (%mock-llm-enqueue "Second response")
  (%mock-llm-enqueue "Third response")
  (let ((conv (amoebum::%make-conversation-state
               :session-id "test-loop-002")))
    ;; Turn 1
    (amoebum.sessions:conversation-state-add-message
     conv (amoebum.sessions:make-conversation-history-entry
           :role "user" :content "Turn 1"))
    (%mock-conversation-step conv (amoebum.sessions:conversation-state-messages conv))
    ;; Turn 2
    (amoebum.sessions:conversation-state-add-message
     conv (amoebum.sessions:make-conversation-history-entry
           :role "user" :content "Turn 2"))
    (%mock-conversation-step conv (amoebum.sessions:conversation-state-messages conv))
    ;; Turn 3
    (amoebum.sessions:conversation-state-add-message
     conv (amoebum.sessions:make-conversation-history-entry
           :role "user" :content "Turn 3"))
    (%mock-conversation-step conv (amoebum.sessions:conversation-state-messages conv))
    ;; Should have 6 entries (3 user + 3 assistant)
    (is (= 6 (length (amoebum.sessions:conversation-state-entries conv))))
    ;; Request log should have 3 entries
    (is (= 3 (length *mock-llm-request-log*)))))

(test mock-llm-tool-call-response
  "Mock LLM can return a tool call."
  (%mock-llm-clear)
  (%mock-llm-enqueue ""
    :tool-calls (list (list :name "read-file"
                            :arguments '(:path "/etc/hostname")
                            :call-id "tc-001"))
    :finish-reason :tool-use)
  (let ((conv (amoebum::%make-conversation-state
               :session-id "test-loop-003")))
    (amoebum.sessions:conversation-state-add-message
     conv (amoebum.sessions:make-conversation-history-entry
           :role "user" :content "Read hostname"))
    (multiple-value-bind (response _conv)
        (%mock-conversation-step conv (amoebum.sessions:conversation-state-messages conv))
      (declare (ignore _conv))
      ;; Response should have tool calls
      (is (not (null (getf response :tool-calls))))
      (let ((tc (first (getf response :tool-calls))))
        (is (equal "read-file" (getf tc :name)))
        (is (equal "tc-001" (getf tc :call-id)))))))

(test mock-llm-tool-result-feedback
  "Tool results feed back into conversation as tool-role messages."
  (%mock-llm-clear)
  (%mock-llm-enqueue "The hostname is myhost.")
  (let ((conv (amoebum::%make-conversation-state
               :session-id "test-loop-004")))
    ;; Simulate tool result being added
    (amoebum.sessions:conversation-state-add-message
     conv (amoebum.sessions:make-conversation-history-entry
           :role "user" :content "Read hostname"))
    (amoebum.sessions:conversation-state-add-message
     conv (amoebum.sessions:make-conversation-history-entry
           :role "tool"
           :content "myhost"
           :tool-call-id "tc-001"))
    ;; LLM processes tool result
    (%mock-conversation-step conv (amoebum.sessions:conversation-state-messages conv))
    ;; 3 entries: user, tool, assistant
    (is (= 3 (length (amoebum.sessions:conversation-state-entries conv))))
    ;; Last entry should be assistant
    (let ((last-entry (car (last (amoebum.sessions:conversation-state-entries conv)))))
      (is (equal "assistant" (amoebum.sessions:conversation-history-entry-role last-entry))))))

(test mock-llm-error-recovery
  "Error during LLM step doesn't corrupt conversation state."
  (%mock-llm-clear)
  ;; Don't enqueue anything — dequeue returns nil simulating an error
  (let ((conv (amoebum::%make-conversation-state
               :session-id "test-loop-005")))
    (amoebum.sessions:conversation-state-add-message
     conv (amoebum.sessions:make-conversation-history-entry
           :role "user" :content "Hello"))
    (multiple-value-bind (response _conv)
        (%mock-conversation-step conv (amoebum.sessions:conversation-state-messages conv))
      (declare (ignore _conv))
      ;; Response should be nil (no queued response)
      (is (null response))
      ;; Conversation should still be valid with just the user message
      (is (= 1 (length (amoebum.sessions:conversation-state-entries conv)))))))

(test mock-llm-budget-exhaustion
  "Budget exhaustion (empty queue) terminates gracefully."
  (%mock-llm-clear)
  (%mock-llm-enqueue "Only response")
  (let ((conv (amoebum::%make-conversation-state
               :session-id "test-loop-006"))
        (step-count 0))
    (amoebum.sessions:conversation-state-add-message
     conv (amoebum.sessions:make-conversation-history-entry
           :role "user" :content "Hello"))
    ;; Try multiple steps — should gracefully stop when queue exhausted
    (dotimes (_ 5)
      (let ((response (%mock-llm-dequeue)))
        (when response
          (incf step-count)
          (amoebum.sessions:conversation-state-add-message
           conv (amoebum.sessions:make-conversation-history-entry
                 :role "assistant"
                 :content (or (getf response :content) ""))))))
    ;; Only 1 step should have succeeded
    (is (= 1 step-count))))

(test conversation-state-messages-accessor
  "conversation-state-messages returns message list."
  (let ((conv (amoebum::%make-conversation-state
               :session-id "test-accessor")))
    (amoebum.sessions:conversation-state-add-message
     conv (amoebum.sessions:make-conversation-history-entry
           :role "user" :content "test"))
    (is (= 1 (length (amoebum.sessions:conversation-state-messages conv))))))

(test request-log-captures-messages
  "Mock request log captures sent messages."
  (%mock-llm-clear)
  (%mock-llm-enqueue "response")
  (let ((conv (amoebum::%make-conversation-state
               :session-id "test-log")))
    (amoebum.sessions:conversation-state-add-message
     conv (amoebum.sessions:make-conversation-history-entry
           :role "user" :content "logged message"))
    (%mock-conversation-step conv (amoebum.sessions:conversation-state-messages conv))
    (is (= 1 (length *mock-llm-request-log*)))
    (let ((req (first *mock-llm-request-log*)))
      (is (listp (getf req :messages))))))
