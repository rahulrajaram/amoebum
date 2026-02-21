(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Agentic tool loop integration test — exercises 20 iterations of the
;;; tool-call → execute → result → continuation cycle using a mock runner.
;;; ---------------------------------------------------------------------------

(defparameter *agentic-loop-tool-counter* 0
  "Incremented each time the agentic-loop-probe tool executes.")

(defparameter +agentic-loop-target-iterations+ 20
  "Number of tool-call iterations the mock runner should produce.")

(defun %count-tool-result-messages (messages)
  "Count messages with role \"tool\" in a message list."
  (count-if (lambda (msg)
              (and (pseudopod:message-p msg)
                   (string= (pseudopod:message-role msg) "tool")))
            messages))

(defun %count-assistant-messages (messages)
  "Count messages with role \"assistant\" in a message list."
  (count-if (lambda (msg)
              (and (pseudopod:message-p msg)
                   (string= (pseudopod:message-role msg) "assistant")))
            messages))

(defun %mock-agentic-runner (stream-state prompt messages
                             &key system-prompt client tools)
  "Mock stream runner that emits tool-call events for the first N iterations,
then emits a text-only response to terminate the loop.

Iteration counting: the number of tool-result messages already in MESSAGES
tells us how many iterations have completed."
  (declare (ignore prompt system-prompt client tools))
  (let ((completed-iterations (%count-tool-result-messages messages)))
    (if (< completed-iterations +agentic-loop-target-iterations+)
        ;; Emit a tool call — the :tool-call-started event populates the
        ;; preview table and :tool-call-argument-complete triggers execution.
        (let* ((call-id (format nil "call_~D" (1+ completed-iterations)))
               (tool-call (pseudopod:make-tool-call
                           :id call-id
                           :name "agentic_loop_probe"
                           :arguments (format nil "{\"step\": ~D}"
                                              (1+ completed-iterations)))))
          (amoebum::token-stream-emit-tool-call-started stream-state tool-call)
          (amoebum::token-stream-emit-tool-call-argument-complete
           stream-state tool-call))
        ;; All iterations done — emit a final text response
        (amoebum::token-stream-emit-chunk
         stream-state
         (format nil "All ~D tool calls completed successfully."
                 +agentic-loop-target-iterations+)))))

(test agentic-loop-20-iteration-chain
  "Exercise the full agentic tool loop for 20 iterations:
tool-call → execute-tool → result message → continuation → repeat."
  (let ((original-toolset amoebum:*toolset*)
        (original-metadata amoebum:*tool-metadata*)
        (original-hooks amoebum:*hook-registry*)
        (original-event-bus amoebum:*event-bus*)
        (original-rules amoebum:*permission-rules*))
    (unwind-protect
        (progn
          (setf amoebum:*toolset* (pseudopod:make-toolset)
                amoebum:*tool-metadata* (make-hash-table :test #'equal)
                amoebum:*hook-registry* (make-hash-table :test #'equal)
                amoebum:*event-bus* (amoebum:make-event-bus :capacity 256)
                amoebum:*permission-rules* nil
                *agentic-loop-tool-counter* 0)
          ;; Register the test tool
          (eval
           '(amoebum:deftool agentic_loop_probe
                ((step integer :description "Step number." :required t))
              "Agentic loop probe tool for integration testing."
              (:permission :auto)
              (:dangerous nil)
              (:category :smoke)
              (:timeout 5)
              (incf amoebum/test::*agentic-loop-tool-counter*)
              (format nil "step_~D_ok" step)))
          ;; Build chat-state with mock runner
          (let* ((bus amoebum:*event-bus*)
                 (chat-state
                   (amoebum:ensure-chat-ui-state
                    (amoebum:make-chat-ui-state
                     :stream-runner #'%mock-agentic-runner
                     :status-bar-state
                     (amoebum:make-status-bar-state
                      :event-bus bus
                      :model-name "test-model"
                      :branch-name "test-branch")))))
            ;; Set permission mode to full-auto so tools execute without prompts
            (let ((old-mode (amoebum:config-permission-mode
                             (amoebum:current-config))))
              (amoebum:setconfig :permission-mode :full-auto)
              (unwind-protect
                  (progn
                    ;; Submit a user message to kick off the loop.
                    ;; Use chat-ui-submit-input to go through the normal
                    ;; :idle -> :user-input transition path.
                    (setf (amoebum:chat-ui-state-input-text chat-state)
                          "Please execute the probe tool 20 times.")
                    (let ((user-msg (amoebum:chat-ui-submit-input chat-state)))
                      (amoebum::%start-streaming-assistant-response
                       chat-state user-msg))
                    ;; Drain events in a polling loop until the conversation
                    ;; goes idle or we time out.
                    (let ((max-polls 500)
                          (poll-count 0)
                          (idle-p nil))
                      (loop while (and (< poll-count max-polls) (not idle-p))
                            do (sleep 0.05)
                               (amoebum::%drain-stream-events chat-state)
                               (incf poll-count)
                               ;; Check if conversation transitioned to idle
                               (let* ((conv (amoebum::chat-ui-state-conversation
                                             chat-state))
                                      (state (and conv
                                                  (amoebum::conversation-state-state
                                                   conv))))
                                 (when (eq state :idle)
                                   (setf idle-p t))))
                      ;; --- Assertions ---
                      (let* ((messages (amoebum:chat-ui-state-messages chat-state))
                             (tool-msgs (%count-tool-result-messages messages))
                             (asst-msgs (%count-assistant-messages messages))
                             (iteration-count
                               (amoebum::chat-ui-state-agentic-iteration-count
                                chat-state)))
                        ;; Should have reached idle
                        (is-true idle-p
                                 "Expected conversation to reach :idle state within timeout.")
                        ;; Tool should have executed 20 times
                        (is (= *agentic-loop-tool-counter*
                                +agentic-loop-target-iterations+)
                            "Expected ~D tool executions, got ~D."
                            +agentic-loop-target-iterations+
                            *agentic-loop-tool-counter*)
                        ;; Should have 20 tool result messages
                        (is (= tool-msgs +agentic-loop-target-iterations+)
                            "Expected ~D tool result messages, got ~D."
                            +agentic-loop-target-iterations+
                            tool-msgs)
                        ;; Should have 21 assistant messages (20 with tool calls + 1 final)
                        (is (= asst-msgs (1+ +agentic-loop-target-iterations+))
                            "Expected ~D assistant messages, got ~D."
                            (1+ +agentic-loop-target-iterations+)
                            asst-msgs)
                        ;; Iteration counter should match
                        (is (= iteration-count +agentic-loop-target-iterations+)
                            "Expected iteration count ~D, got ~D."
                            +agentic-loop-target-iterations+
                            iteration-count)
                        ;; Final message should contain the completion text
                        (let* ((final-msg (car (last messages)))
                               (final-text
                                 (and (pseudopod:message-p final-msg)
                                      (amoebum::%message-content->text
                                       final-msg))))
                          (is-true
                           (and final-text
                                (search "All 20 tool calls completed"
                                        final-text))
                           "Expected final message to contain completion text.")))))
                (amoebum:setconfig :permission-mode old-mode)))))
      ;; Restore globals
      (setf amoebum:*toolset* original-toolset
            amoebum:*tool-metadata* original-metadata
            amoebum:*hook-registry* original-hooks
            amoebum:*event-bus* original-event-bus
            amoebum:*permission-rules* original-rules))))
