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
         (dispatch-command-fn (funcall fn-in "DISPATCH-SLASH-COMMAND" amoebum-pkg))
         (result-output-fn (funcall fn-in "SLASH-COMMAND-RESULT-OUTPUT" amoebum-pkg))
         (complete-fn (funcall fn-in "COMPLETE-SLASH-COMMAND-INPUT" amoebum-pkg))
         (check-permission-fn (funcall fn-in "CHECK-PERMISSION" amoebum-pkg))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (config-value-fn (funcall fn-in "CONFIG-VALUE" amoebum-pkg))
         (current-config-fn (funcall fn-in "CURRENT-CONFIG" amoebum-pkg))
         (clear-agents-fn (funcall fn-in "CLEAR-AGENTS" amoebum-pkg))
         (list-agents-fn (funcall fn-in "LIST-AGENTS" amoebum-pkg))
         (find-agent-fn (funcall fn-in "FIND-AGENT" amoebum-pkg))
         (agent-id-fn (funcall fn-in "AGENT-RECORD-ID" amoebum-pkg))
         (agent-status-fn (funcall fn-in "AGENT-RECORD-STATUS" amoebum-pkg))
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
      (let ((old-mcp-permissions
              (funcall config-value-fn
                       :mcp-server-permissions
                       (funcall current-config-fn))))
        (unwind-protect
            (progn
              (funcall setconfig-fn :mcp-server-permissions nil)
              (funcall clear-agents-fn)

              (multiple-value-bind (handledp hooks-result)
                  (funcall dispatch-command-fn "/hooks")
                (assert-true handledp "Expected /hooks to be handled.")
                (let ((output (funcall result-output-fn hooks-result)))
                  (assert-true (or (contains-text-p output "Registered hooks")
                                   (contains-text-p output "No hooks registered"))
                               "Expected /hooks output to include registry summary, got ~S."
                               output)))
              (multiple-value-bind (handledp hooks-trace-result)
                  (funcall dispatch-command-fn "/hooks trace 5")
                (assert-true handledp "Expected /hooks trace to be handled.")
                (let ((output (funcall result-output-fn hooks-trace-result)))
                  (assert-true (or (contains-text-p output "Hook trace")
                                   (contains-text-p output "empty"))
                               "Expected /hooks trace output, got ~S."
                               output)))

              (multiple-value-bind (handledp list-result)
                  (funcall dispatch-command-fn "/mcp-auth")
                (assert-true handledp "Expected /mcp-auth to be handled.")
                (assert-true (contains-text-p (funcall result-output-fn list-result)
                                              "MCP authorization rules")
                             "Expected /mcp-auth output header, got ~S."
                             (funcall result-output-fn list-result)))
              (multiple-value-bind (handledp _result)
                  (funcall dispatch-command-fn "/mcp-auth set github allow")
                (declare (ignore _result))
                (assert-true handledp "Expected /mcp-auth set github allow to be handled."))
              (assert-true
               (eq (funcall check-permission-fn
                            :tool "mcp/github/echo"
                            :permission-mode :full-auto)
                   :allow)
               "Expected mcp/github permission to be :allow after /mcp-auth set.")
              (multiple-value-bind (handledp _result)
                  (funcall dispatch-command-fn "/mcp-auth set github deny")
                (declare (ignore _result))
                (assert-true handledp "Expected /mcp-auth set github deny to be handled."))
              (assert-true
               (eq (funcall check-permission-fn
                            :tool "mcp/github/echo"
                            :permission-mode :full-auto)
                   :deny)
               "Expected mcp/github permission to be :deny after /mcp-auth set.")
              (multiple-value-bind (handledp _result)
                  (funcall dispatch-command-fn "/mcp-auth clear github")
                (declare (ignore _result))
                (assert-true handledp "Expected /mcp-auth clear github to be handled."))
              (assert-true
               (eq (funcall check-permission-fn
                            :tool "mcp/github/echo"
                            :permission-mode :full-auto)
                   :prompt)
               "Expected mcp/github permission to return to :prompt after clear.")

              (multiple-value-bind (replacement suggestions)
                  (funcall complete-fn "/mcp-a")
                (assert-true (string= replacement "/mcp-auth ")
                             "Expected /mcp-a completion to /mcp-auth, got ~S."
                             replacement)
                (assert-true (and (listp suggestions)
                                  (member "/mcp-auth" suggestions :test #'string=))
                             "Expected /mcp-a suggestions to include /mcp-auth, got ~S."
                             suggestions))

              (let* ((chat-state (funcall chat-state-fn :stream-runner nil)))
                (multiple-value-bind (handledp spawn-result)
                    (funcall dispatch-command-fn "/spawn claude parity smoke" :chat-state chat-state)
                  (assert-true handledp "Expected /spawn to be handled.")
                  (assert-true (contains-text-p (funcall result-output-fn spawn-result) "Spawned agent")
                               "Expected /spawn output to include spawned agent id, got ~S."
                               (funcall result-output-fn spawn-result)))
                (assert-true
                 (wait-until (lambda ()
                               (let ((agents (funcall list-agents-fn :include-completed-p t)))
                                 (and agents
                                      (let ((agent (car (last agents))))
                                        (member (funcall agent-status-fn agent)
                                                '(:completed :cancelled :failed)
                                                :test #'eq)))))
                             :timeout-seconds 3.0d0)
                 "Timed out waiting for /spawn-created agent completion.")
                (let* ((agents (funcall list-agents-fn :include-completed-p t))
                       (agent (car (last agents)))
                       (agent-id (funcall agent-id-fn agent)))
                  (multiple-value-bind (handledp agent-output-result)
                      (funcall dispatch-command-fn
                               (format nil "/agent ~A output" agent-id)
                               :chat-state chat-state)
                    (assert-true handledp "Expected /agent output to be handled.")
                    (assert-true (contains-text-p (funcall result-output-fn agent-output-result) agent-id)
                                 "Expected /agent output to include id ~A, got ~S."
                                 agent-id
                                 (funcall result-output-fn agent-output-result)))
                  (assert-true (funcall find-agent-fn agent-id)
                               "Expected spawned agent id ~A to exist in registry."
                               agent-id)
                  (funcall render-chat-fn chat-state (funcall make-size-fn 120 24))
                  (let* ((messages (funcall chat-messages-fn chat-state))
                         (last-message (car (last messages))))
                    (assert-true last-message
                                 "Expected chat to receive merged subagent completion message.")
                    (assert-true (string= (funcall message-role-fn last-message) "tool")
                                 "Expected merged subagent completion role tool, got ~S."
                                 (funcall message-role-fn last-message))
                    (let ((text (message-text last-message)))
                      (assert-true (contains-text-p text "subagent")
                                   "Expected merged completion text to mention subagent, got ~S."
                                   text)
                      (assert-true (contains-text-p text agent-id)
                                   "Expected merged completion text to include agent id ~A, got ~S."
                                   agent-id
                                   text))))))
          (funcall setconfig-fn :mcp-server-permissions old-mcp-permissions)
          (funcall clear-agents-fn)))))

  (format t "AMOEBUM_CLAUDE_PARITY_SMOKE_OK~%"))
