(in-package :amoebum/test)

(def-suite stream-completion-ordering-suite :in amoebum-suite)
(in-suite stream-completion-ordering-suite)

(test stream-complete-before-argument-complete-defers-finalization-until-result
  "Completion should wait for the late tool result when :complete arrives before tool execution finishes."
  (let* ((stream-state (amoebum:make-token-stream-state))
         (chat-state (amoebum.ui:make-chat-ui-state
                      :stream-runner nil
                      :stream-client (pseudopod:make-client :api-key "stub")
                      :stream-state stream-state))
         (tool-call (pseudopod:make-tool-call
                     :id "late-order:0"
                     :name "read-file"
                     :arguments "{\"path\":\"README.md\"}"))
         (finalize-calls 0)
         (original-finalize-fn
           (symbol-function 'amoebum::%maybe-finalize-streaming-assistant-on-complete))
         (original-execute-fn
           (symbol-function 'amoebum::%execute-stream-tool-call!)))
    (unwind-protect
        (progn
          (setf (symbol-function 'amoebum::%maybe-finalize-streaming-assistant-on-complete)
                (lambda (_chat-state)
                  (setf (amoebum::chat-ui-state-stream-completion-pending-p _chat-state) nil)
                  (incf finalize-calls)
                  t))
          (setf (symbol-function 'amoebum::%execute-stream-tool-call!)
                (lambda (state event)
                  (let* ((preview-entry
                           (amoebum::%update-stream-tool-call-preview! state event))
                         (preview-key
                           (and (listp preview-entry) (getf preview-entry :key))))
                    (amoebum::%set-stream-tool-call-execution-status!
                     state preview-key :executed-p t)
                    nil)))
          (amoebum:token-stream-emit-tool-call-started stream-state tool-call)
          (amoebum:token-stream-mark-complete stream-state)
          (amoebum:token-stream-emit-tool-call-argument-complete stream-state tool-call)
          (amoebum::%drain-stream-events chat-state)
          (is (= finalize-calls 0))
          (is-true (amoebum::chat-ui-state-stream-completion-pending-p chat-state))
          (amoebum:token-stream-emit-tool-call-result
           stream-state
           :tool-call tool-call
           :result "{\"ok\":true}")
          (amoebum::%drain-stream-events chat-state)
          (is (= finalize-calls 1))
          (is-false (amoebum::chat-ui-state-stream-completion-pending-p chat-state)))
      (setf (symbol-function 'amoebum::%maybe-finalize-streaming-assistant-on-complete)
            original-finalize-fn
            (symbol-function 'amoebum::%execute-stream-tool-call!)
            original-execute-fn))))

(test stream-tool-result-before-complete-finalizes-on-complete
  "If the tool result arrives first, the eventual :complete should finalize immediately."
  (let* ((stream-state (amoebum:make-token-stream-state))
         (chat-state (amoebum.ui:make-chat-ui-state
                      :stream-runner nil
                      :stream-client (pseudopod:make-client :api-key "stub")
                      :stream-state stream-state))
         (tool-call (pseudopod:make-tool-call
                     :id "result-first:0"
                     :name "exec_command"
                     :arguments "{\"cmd\":\"pwd\"}"))
         (finalize-calls 0)
         (original-finalize-fn
           (symbol-function 'amoebum::%maybe-finalize-streaming-assistant-on-complete))
         (original-execute-fn
           (symbol-function 'amoebum::%execute-stream-tool-call!)))
    (unwind-protect
        (progn
          (setf (symbol-function 'amoebum::%maybe-finalize-streaming-assistant-on-complete)
                (lambda (_chat-state)
                  (setf (amoebum::chat-ui-state-stream-completion-pending-p _chat-state) nil)
                  (incf finalize-calls)
                  t))
          (setf (symbol-function 'amoebum::%execute-stream-tool-call!)
                (lambda (state event)
                  (let* ((preview-entry
                           (amoebum::%update-stream-tool-call-preview! state event))
                         (preview-key
                           (and (listp preview-entry) (getf preview-entry :key))))
                    (amoebum::%set-stream-tool-call-execution-status!
                     state preview-key :executed-p t)
                    nil)))
          (amoebum:token-stream-emit-tool-call-started stream-state tool-call)
          (amoebum:token-stream-emit-tool-call-argument-complete stream-state tool-call)
          (amoebum:token-stream-emit-tool-call-result
           stream-state
           :tool-call tool-call
           :result "{\"cwd\":\"/workspace\"}")
          (amoebum:token-stream-mark-complete stream-state)
          (amoebum::%drain-stream-events chat-state)
          (is (= finalize-calls 1))
          (is-false (amoebum::chat-ui-state-stream-completion-pending-p chat-state)))
      (setf (symbol-function 'amoebum::%maybe-finalize-streaming-assistant-on-complete)
            original-finalize-fn
            (symbol-function 'amoebum::%execute-stream-tool-call!)
            original-execute-fn))))
