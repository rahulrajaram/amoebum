(in-package :amoebum/test)

(def-suite llm-hooks-suite
  :description "I219 LLM interception hooks."
  :in amoebum-suite)

(in-suite llm-hooks-suite)

(defun %restore-hook-entries (entries)
  (dolist (entry entries)
    (amoebum:register-hook
     (amoebum::hook-entry-hook-point entry)
     (amoebum::hook-entry-hook-id entry)
     (amoebum::hook-entry-handler entry)
     :priority (amoebum::hook-entry-priority entry)
     :async (amoebum::hook-entry-async-p entry)
     :max-ms (amoebum::hook-entry-max-ms entry)
     :on-error (amoebum::hook-entry-on-error entry)
     :failure-threshold (amoebum::hook-entry-failure-threshold entry)
     :docstring (amoebum::hook-entry-docstring entry)
     :source-file (amoebum::hook-entry-source-file entry)
     :source-line (amoebum::hook-entry-source-line entry)))
  t)

(defun %with-cleared-llm-hooks (thunk)
  (let ((existing-pre (amoebum:list-hooks :pre-llm-send))
        (existing-post (amoebum:list-hooks :post-llm-receive)))
    (unwind-protect
        (progn
          (amoebum:clear-hooks :pre-llm-send)
          (amoebum:clear-hooks :post-llm-receive)
          (funcall thunk))
      (amoebum:clear-hooks :pre-llm-send)
      (amoebum:clear-hooks :post-llm-receive)
      (%restore-hook-entries existing-pre)
      (%restore-hook-entries existing-post))))

(defun %make-step-result (&key history final-message)
  (funcall (symbol-function (find-symbol "%MAKE-STEP-RESULT" :pseudopod))
           :steps 1
           :history history
           :final-message final-message
           :last-message final-message
           :max-steps-reached nil
           :tool-results '()))

(test llm-hooks-fire-during-chat-step-and-pre-hook-can-modify-messages
  (%with-cleared-llm-hooks
   (lambda ()
     (let* ((base-message (pseudopod:make-message :role "user" :content "hello"))
            (chat-state (amoebum.ui:make-chat-ui-state
                         :messages (list base-message)
                         :stream-client (pseudopod:make-client :api-key "test-key")
                         :stream-runner nil))
            (captured-step-messages nil)
            (post-response nil)
            (post-usage :unset)
            (post-model nil))
       (amoebum:register-hook
        :pre-llm-send
        'llm-hooks-pre-modifier
        (lambda (messages tools model)
          (declare (ignore tools model))
          (cons (pseudopod:make-message :role "system" :content "pre-hook system")
                messages)))
       (amoebum:register-hook
        :post-llm-receive
        'llm-hooks-post-observer
        (lambda (response usage model)
          (setf post-response response
                post-usage usage
                post-model model)
          :ok))
       (let ((old-step (symbol-function 'pseudopod:step)))
         (unwind-protect
             (progn
               (setf (symbol-function 'pseudopod:step)
                     (lambda (client &key messages &allow-other-keys)
                       (declare (ignore client))
                       (setf captured-step-messages messages)
                       (%make-step-result
                        :history messages
                        :final-message (pseudopod:make-message
                                        :role "assistant"
                                        :content "ok"))))
               (amoebum::%start-step-loop-assistant-response chat-state))
           (setf (symbol-function 'pseudopod:step) old-step)))
       (is (listp captured-step-messages))
       (is (= 2 (length captured-step-messages)))
       (is (string= "system"
                    (pseudopod:message-role (first captured-step-messages))))
       (let* ((content (pseudopod:message-content (first captured-step-messages)))
              (text (cond
                      ((stringp content) content)
                      ((and (listp content)
                            (pseudopod:content-part-p (first content)))
                       (pseudopod:content-part-text (first content)))
                      (t ""))))
         (is (string= "pre-hook system" text)))
       (is (typep post-response 'pseudopod:step-result))
       (is (null post-usage))
       (is (stringp post-model))
       (is (plusp (length post-model)))))))

(test llm-hooks-pre-send-block-cancels-chat-step
  (%with-cleared-llm-hooks
   (lambda ()
     (let* ((chat-state (amoebum.ui:make-chat-ui-state
                         :messages (list (pseudopod:make-message
                                          :role "user"
                                          :content "blocked"))
                         :stream-client (pseudopod:make-client :api-key "test-key")
                         :stream-runner nil))
            (step-called-p nil))
       (amoebum:register-hook
        :pre-llm-send
        'llm-hooks-pre-block
        (lambda (messages tools model)
          (declare (ignore messages tools model))
          :block))
       (let ((old-step (symbol-function 'pseudopod:step)))
         (unwind-protect
             (progn
               (setf (symbol-function 'pseudopod:step)
                     (lambda (&rest _)
                       (declare (ignore _))
                       (setf step-called-p t)
                       (%make-step-result :history '() :final-message nil)))
               (amoebum::%start-step-loop-assistant-response chat-state))
           (setf (symbol-function 'pseudopod:step) old-step)))
       (is-false step-called-p)))))

(test llm-hooks-smoke-sentinel
  (is-true t)
  (format t "LLM_HOOKS_SMOKE_OK~%"))
