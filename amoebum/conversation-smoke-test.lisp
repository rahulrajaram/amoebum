(require :asdf)

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
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn-in
           (lambda (name package)
             (symbol-function (funcall symbol-in name package))))
         (make-conversation-state-fn (funcall fn-in "MAKE-CONVERSATION-STATE" amoebum-pkg))
         (conversation-state-state-fn (funcall fn-in "CONVERSATION-STATE-STATE" amoebum-pkg))
         (conversation-transition-fn (funcall fn-in "CONVERSATION-TRANSITION!" amoebum-pkg))
         (invalid-transition-sym (funcall symbol-in "INVALID-CONVERSATION-TRANSITION" amoebum-pkg))
         (conversation-add-message-fn (funcall fn-in "CONVERSATION-STATE-ADD-MESSAGE" amoebum-pkg))
         (conversation-state-path-fn (funcall fn-in "CONVERSATION-STATE-SESSION-PATH" amoebum-pkg))
         (conversation-load-latest-fn (funcall fn-in "CONVERSATION-LOAD-LATEST" amoebum-pkg))
         (conversation-state-entries-fn (funcall fn-in "CONVERSATION-STATE-ENTRIES" amoebum-pkg))
         (entry-role-fn (funcall fn-in "CONVERSATION-HISTORY-ENTRY-ROLE" amoebum-pkg))
         (entry-content-fn (funcall fn-in "CONVERSATION-HISTORY-ENTRY-CONTENT" amoebum-pkg))
         (entry-timestamp-fn (funcall fn-in "CONVERSATION-HISTORY-ENTRY-TIMESTAMP" amoebum-pkg))
         (conversation-state-messages-fn (funcall fn-in "CONVERSATION-STATE-MESSAGES" amoebum-pkg))
         (make-chat-ui-state-fn (funcall fn-in "MAKE-CHAT-UI-STATE" amoebum-pkg))
         (chat-ui-restore-latest-session-fn (funcall fn-in "CHAT-UI-RESTORE-LATEST-SESSION" amoebum-pkg))
         (chat-ui-state-messages-fn (funcall fn-in "CHAT-UI-STATE-MESSAGES" amoebum-pkg))
         (dispatch-fn (funcall fn-in "DISPATCH-SLASH-COMMAND" amoebum-pkg))
         (result-output-fn (funcall fn-in "SLASH-COMMAND-RESULT-OUTPUT" amoebum-pkg))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (make-message-fn (funcall fn-in "MAKE-MESSAGE" pseudopod-pkg))
         (tmp-root (uiop:ensure-directory-pathname
                    (merge-pathnames
                     (make-pathname :directory
                                    `(:relative
                                      ,(format nil "amoebum-i59-~D-~D"
                                               (get-universal-time)
                                               (random 1000000))))
                     (uiop:ensure-directory-pathname (uiop:temporary-directory))))))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (contains-text-p (haystack needle)
               (and (stringp haystack)
                    (search needle haystack :test #'char-equal))))
      (ensure-directories-exist (merge-pathnames #P".keep" tmp-root))
      (funcall setconfig-fn :project-root tmp-root)

      (let* ((base-timestamp (get-universal-time))
             (conversation
               (funcall make-conversation-state-fn
                        :project-root tmp-root
                        :session-id "i59-conversation-smoke"))
             (user-message
               (funcall make-message-fn
                        :role "user"
                        :content "release-checklist item alpha"))
             (assistant-message
               (funcall make-message-fn
                        :role "assistant"
                        :content "assistant-note beta"))
             (invalid-transition-signaled nil))
        (assert-true (eq (funcall conversation-state-state-fn conversation) :idle)
                     "Expected new conversation state to start at :IDLE.")

        (funcall conversation-transition-fn conversation :user-input)
        (assert-true (eq (funcall conversation-state-state-fn conversation) :user-input)
                     "Expected state transition :IDLE -> :USER-INPUT.")
        (funcall conversation-transition-fn conversation :idle)

        (handler-case
            (funcall conversation-transition-fn conversation :tool-executing)
          (error (condition)
            (when (typep condition invalid-transition-sym)
              (setf invalid-transition-signaled t))))
        (assert-true invalid-transition-signaled
                     "Expected invalid transition :IDLE -> :TOOL-EXECUTING to be rejected.")

        (funcall conversation-add-message-fn conversation user-message :timestamp base-timestamp)
        (funcall conversation-add-message-fn conversation assistant-message :timestamp (1+ base-timestamp))
        (funcall conversation-transition-fn conversation :idle)

        (let ((session-path (funcall conversation-state-path-fn conversation)))
          (assert-true (and session-path (probe-file session-path))
                       "Expected conversation state to persist session file, got ~S."
                       session-path))

        (let* ((restored (funcall conversation-load-latest-fn :project-root tmp-root))
               (entries (and restored (funcall conversation-state-entries-fn restored))))
          (assert-true restored
                       "Expected conversation-load-latest to return restored state.")
          (assert-true (= (length entries) 2)
                       "Expected restored history entry count 2, got ~S."
                       (length entries))
          (assert-true (string= (funcall entry-role-fn (first entries)) "user")
                       "Expected first restored entry role user.")
          (assert-true (contains-text-p (funcall entry-content-fn (first entries))
                                        "release-checklist")
                       "Expected first restored entry content to include release-checklist.")
          (assert-true (= (funcall entry-timestamp-fn (second entries))
                          (1+ base-timestamp))
                       "Expected second restored timestamp to match saved turn.")
          (assert-true (= (length (funcall conversation-state-messages-fn restored)) 2)
                       "Expected restored conversation to hydrate back into 2 chat messages.")

          (let ((chat-state (funcall make-chat-ui-state-fn
                                     :stream-runner nil
                                     :conversation restored)))
            (multiple-value-bind (handledp result)
                (funcall dispatch-fn
                         "/history release-checklist role=user --limit 5"
                         :chat-state chat-state)
              (let ((output (funcall result-output-fn result)))
                (assert-true handledp "Expected /history query to be handled.")
                (assert-true (contains-text-p output "USER:")
                             "Expected /history output to include USER role line, got ~S."
                             output)
                (assert-true (contains-text-p output "release-checklist")
                             "Expected /history output to include content match, got ~S."
                             output)))
            (multiple-value-bind (handledp result)
                (funcall dispatch-fn
                         (format nil "/history role=assistant since=~D"
                                 (1+ base-timestamp))
                         :chat-state chat-state)
              (let ((output (funcall result-output-fn result)))
                (assert-true handledp
                             "Expected /history assistant+since query to be handled.")
                (assert-true (contains-text-p output "ASSISTANT:")
                             "Expected /history output to include ASSISTANT role line, got ~S."
                             output)
                (assert-true (contains-text-p output "assistant-note")
                             "Expected /history output to include assistant content, got ~S."
                             output))))

          (let* ((restored-chat (funcall chat-ui-restore-latest-session-fn
                                         (funcall make-chat-ui-state-fn
                                                  :stream-runner nil)))
                 (messages (funcall chat-ui-state-messages-fn restored-chat)))
            (assert-true (= (length messages) 2)
                         "Expected chat-ui-restore-latest-session to restore two persisted messages."))))))

  (format t "AMOEBUM_CONVERSATION_SMOKE_OK~%"))
