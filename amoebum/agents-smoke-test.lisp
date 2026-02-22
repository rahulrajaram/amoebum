(let* ((smoke-file (or *load-truename* *compile-file-truename*))
       (amoebum-dir (and smoke-file (make-pathname :name nil :type nil :defaults smoke-file)))
       (repo-root (and amoebum-dir (truename (merge-pathnames #P"../" amoebum-dir)))))
  (unless repo-root
    (error "Unable to resolve repository root from ~S" smoke-file))

  (load (merge-pathnames #P"ptui/.tools/quicklisp/setup.lisp" repo-root))
  (require :asdf)

  (let* ((asdf-pkg (or (find-package "ASDF")
                       (error "Missing package ASDF")))
         (load-asd-sym (or (find-symbol "LOAD-ASD" asdf-pkg)
                           (error "Missing symbol LOAD-ASD in ASDF package")))
         (load-system-sym (or (find-symbol "LOAD-SYSTEM" asdf-pkg)
                              (error "Missing symbol LOAD-SYSTEM in ASDF package")))
         (load-asd-fn (symbol-function load-asd-sym))
         (load-system-fn (symbol-function load-system-sym)))
    (funcall load-asd-fn (merge-pathnames #P"pseudopod/pseudopod.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"sw4rm-sdk/sw4rm-sdk.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"ptui/ptui.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"amoebum/amoebum.asd" repo-root))
    (funcall load-system-fn "amoebum"))

  (let* ((amoebum-pkg (or (find-package "AMOEBUM")
                          (error "Missing package AMOEBUM after load.")))
         (pseudopod-pkg (or (find-package "PSEUDOPOD")
                            (error "Missing package PSEUDOPOD after load.")))
         (types-pkg (or (find-package "PTUI.CORE.TYPES")
                        (error "Missing package PTUI.CORE.TYPES after load.")))
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn-in
           (lambda (name package)
             (symbol-function (funcall symbol-in name package))))
         (clear-agents-fn (funcall fn-in "CLEAR-AGENTS" amoebum-pkg))
         (spawn-agent-fn (funcall fn-in "SPAWN-AGENT" amoebum-pkg))
         (find-agent-fn (funcall fn-in "FIND-AGENT" amoebum-pkg))
         (agent-id-fn (funcall fn-in "AGENT-RECORD-ID" amoebum-pkg))
         (agent-status-fn (funcall fn-in "AGENT-RECORD-STATUS" amoebum-pkg))
         (agent-output-fn (funcall fn-in "AGENT-OUTPUT" amoebum-pkg))
         (dispatch-command-fn (funcall fn-in "DISPATCH-SLASH-COMMAND" amoebum-pkg))
         (result-output-fn (funcall fn-in "SLASH-COMMAND-RESULT-OUTPUT" amoebum-pkg))
         (chat-state-fn (funcall fn-in "MAKE-CHAT-UI-STATE" amoebum-pkg))
         (chat-messages-fn (funcall fn-in "CHAT-UI-STATE-MESSAGES" amoebum-pkg))
         (render-chat-fn (funcall fn-in "RENDER-CHAT-UI-BUFFER" amoebum-pkg))
         (make-size-fn (funcall fn-in "MAKE-SIZE" types-pkg))
         (message-role-fn (funcall fn-in "MESSAGE-ROLE" pseudopod-pkg))
         (message-content-fn (funcall fn-in "MESSAGE-CONTENT" pseudopod-pkg))
         (content-part-text-fn (funcall fn-in "CONTENT-PART-TEXT" pseudopod-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (contains-text-p (haystack needle)
               (and (stringp haystack)
                    (search needle haystack :test #'char-equal)))
             (message-text (message)
               (with-output-to-string (out)
                 (loop for part in (funcall message-content-fn message)
                       for index from 0 do
                         (when (> index 0)
                           (write-char #\Newline out))
                         (write-string (or (funcall content-part-text-fn part) "") out))))
             (wait-until (predicate &key (timeout-seconds 2.0d0) (interval-seconds 0.01d0))
               (let* ((start (get-internal-real-time))
                      (units-per-second internal-time-units-per-second)
                      (timeout-units (truncate (* timeout-seconds units-per-second))))
                 (loop
                   (when (funcall predicate)
                     (return t))
                   (when (> (- (get-internal-real-time) start) timeout-units)
                     (return nil))
                   (sleep interval-seconds)))))
      (funcall clear-agents-fn)

      (let* ((runner-agent
               (funcall spawn-agent-fn
                        "capture smoke"
                        :agent-type :research
                        :runner (lambda (agent)
                                  (declare (ignore agent))
                                  (format t "captured stdout line~%")
                                  "capture-result")))
             (runner-id (funcall agent-id-fn runner-agent)))
        (assert-true (wait-until (lambda ()
                                   (let ((agent (funcall find-agent-fn runner-id)))
                                     (and agent
                                          (member (funcall agent-status-fn agent)
                                                  '(:completed :failed :cancelled)
                                                  :test #'eq)))))
                     "Timed out waiting for agent ~A completion." runner-id)
        (multiple-value-bind (captured-output status)
            (funcall agent-output-fn runner-id)
          (assert-true (eq status :completed)
                       "Expected completed status for capture agent, got ~S." status)
          (assert-true (contains-text-p captured-output "captured stdout line")
                       "Expected captured stdout in agent output, got ~S."
                       captured-output)))

      (let* ((cancel-agent
               (funcall spawn-agent-fn
                        "cancel smoke"
                        :agent-type :test
                        :runner (lambda (agent)
                                  (dotimes (_ 100)
                                    (declare (ignore _))
                                    (sleep 0.01)
                                    (funcall (fn-in "AGENT-CHECK-CANCEL" amoebum-pkg) agent))
                                  "should-not-complete")))
             (cancel-id (funcall agent-id-fn cancel-agent)))
        (multiple-value-bind (handledp list-result)
            (funcall dispatch-command-fn "/agents")
          (assert-true handledp "Expected /agents to be handled.")
          (assert-true (contains-text-p (funcall result-output-fn list-result) cancel-id)
                       "Expected /agents output to include running agent ~A, got ~S."
                       cancel-id
                       (funcall result-output-fn list-result)))
        (multiple-value-bind (handledp cancel-result)
            (funcall dispatch-command-fn (format nil "/agent ~A cancel" cancel-id))
          (assert-true handledp "Expected /agent <id> cancel to be handled.")
          (assert-true (contains-text-p (funcall result-output-fn cancel-result) "Cancel requested")
                       "Expected cancel acknowledgement, got ~S."
                       (funcall result-output-fn cancel-result)))
        (assert-true (wait-until (lambda ()
                                   (let ((agent (funcall find-agent-fn cancel-id)))
                                     (and agent
                                          (eq (funcall agent-status-fn agent) :cancelled)))))
                     "Timed out waiting for cancelled status on agent ~A." cancel-id)
        (multiple-value-bind (handledp output-result)
            (funcall dispatch-command-fn (format nil "/agent ~A output" cancel-id))
          (assert-true handledp "Expected /agent <id> output to be handled.")
          (assert-true (contains-text-p (funcall result-output-fn output-result) "cancelled")
                       "Expected /agent output to include cancelled state, got ~S."
                       (funcall result-output-fn output-result))))

      (funcall clear-agents-fn)
      (let* ((chat-state (funcall chat-state-fn :stream-runner nil))
             (inject-agent
               (funcall spawn-agent-fn
                        "inject smoke"
                        :agent-type :implement
                        :runner (lambda (agent)
                                  (declare (ignore agent))
                                  "inject-result")))
             (inject-id (funcall agent-id-fn inject-agent)))
        (assert-true (wait-until (lambda ()
                                   (let ((agent (funcall find-agent-fn inject-id)))
                                     (and agent
                                          (eq (funcall agent-status-fn agent) :completed)))))
                     "Timed out waiting for inject agent completion.")
        (funcall render-chat-fn chat-state (funcall make-size-fn 100 20))
        (let* ((messages (funcall chat-messages-fn chat-state))
               (last-message (car (last messages))))
          (assert-true last-message
                       "Expected injected agent completion message in chat.")
          (assert-true (string= (funcall message-role-fn last-message) "tool")
                       "Expected injected completion role to be tool, got ~S."
                       (funcall message-role-fn last-message))
          (let ((text (message-text last-message)))
            (assert-true (contains-text-p text inject-id)
                         "Expected injected completion text to include agent id ~A, got ~S."
                         inject-id
                         text)
            (assert-true (contains-text-p text "inject-result")
                         "Expected injected completion to include result payload, got ~S."
                         text))))))

  (format t "AMOEBUM_AGENTS_SMOKE_OK~%"))
