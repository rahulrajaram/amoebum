(require :asdf)

(let* ((smoke-file (or *load-truename* *compile-file-truename*))
       (amoebum-dir (and smoke-file
                         (make-pathname :name nil :type nil :defaults smoke-file)))
       (repo-root (and amoebum-dir
                       (truename (merge-pathnames #P"../" amoebum-dir)))))
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
    (funcall load-asd-fn (merge-pathnames #P"ptui/ptui.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"amoebum/amoebum.asd" repo-root))
    (funcall load-system-fn "amoebum"))

  (let* ((amoebum-pkg (or (find-package "AMOEBUM")
                          (error "Missing package AMOEBUM after load.")))
         (pseudopod-pkg (or (find-package "PSEUDOPOD")
                            (error "Missing package PSEUDOPOD after load.")))
         (symbol-in (lambda (name package)
                      (or (find-symbol name package)
                          (error "Missing symbol ~A in package ~A."
                                 name
                                 (package-name package)))))
         (fn-in (lambda (name package)
                  (symbol-function (funcall symbol-in name package))))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (dispatch-fn (funcall fn-in "DISPATCH-SLASH-COMMAND" amoebum-pkg))
         (result-output-fn (funcall fn-in "SLASH-COMMAND-RESULT-OUTPUT" amoebum-pkg))
         (make-chat-ui-state-fn (funcall fn-in "MAKE-CHAT-UI-STATE" amoebum-pkg))
         (chat-ui-add-message-fn (funcall fn-in "CHAT-UI-ADD-MESSAGE" amoebum-pkg))
         (chat-ui-state-conversation-fn (funcall fn-in "CHAT-UI-STATE-CONVERSATION" amoebum-pkg))
         (chat-ui-state-messages-fn (funcall fn-in "CHAT-UI-STATE-MESSAGES" amoebum-pkg))
         (chat-ui-restore-latest-session-fn (funcall fn-in "CHAT-UI-RESTORE-LATEST-SESSION" amoebum-pkg))
         (conversation-active-fork-name-fn (funcall fn-in "CONVERSATION-ACTIVE-FORK-NAME" amoebum-pkg))
         (conversation-list-forks-fn (funcall fn-in "CONVERSATION-LIST-FORKS" amoebum-pkg))
         (conversation-state-session-id-fn (funcall fn-in "CONVERSATION-STATE-SESSION-ID" amoebum-pkg))
         (conversation-state-entries-fn (funcall fn-in "CONVERSATION-STATE-ENTRIES" amoebum-pkg))
         (conversation-history-entry-content-fn (funcall fn-in "CONVERSATION-HISTORY-ENTRY-CONTENT" amoebum-pkg))
         (conversation-fork-path-fn (funcall fn-in "CONVERSATION-FORK-PATH" amoebum-pkg))
         (make-event-bus-fn (funcall fn-in "MAKE-EVENT-BUS" amoebum-pkg))
         (subscribe-fn (funcall fn-in "SUBSCRIBE" amoebum-pkg))
         (event-type-fn (funcall fn-in "EVENT-TYPE" amoebum-pkg))
         (conversation-forked-type (symbol-value
                                    (funcall symbol-in
                                             "+EVENT-TYPE-CONVERSATION-FORKED+"
                                             amoebum-pkg)))
         (event-bus-var (funcall symbol-in "*EVENT-BUS*" amoebum-pkg))
         (make-message-fn (funcall fn-in "MAKE-MESSAGE" pseudopod-pkg))
         (tmp-root (uiop:ensure-directory-pathname
                    (merge-pathnames
                     (make-pathname :directory
                                    `(:relative
                                      ,(format nil "amoebum-i73-~D-~D"
                                               (get-universal-time)
                                               (random 1000000))))
                     (uiop:ensure-directory-pathname (uiop:temporary-directory))))))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (contains-text-p (haystack needle)
               (and (stringp haystack)
                    (search needle haystack :test #'char-equal)))
             (entry-contents (conversation)
               (mapcar conversation-history-entry-content-fn
                       (funcall conversation-state-entries-fn conversation)))
             (has-content-p (conversation needle)
               (some (lambda (text)
                       (contains-text-p text needle))
                     (entry-contents conversation)))
             (fork-record-by-name (forks name)
               (find name forks
                     :test #'string-equal
                     :key (lambda (record) (or (getf record :name) ""))))
             (assert-command (chat-state input)
               (multiple-value-bind (handledp result)
                   (funcall dispatch-fn input :chat-state chat-state)
                 (assert-true handledp "Expected command ~S to be handled." input)
                 (funcall result-output-fn result))))
      (ensure-directories-exist (merge-pathnames #P".keep" tmp-root))
      (funcall setconfig-fn :project-root tmp-root)

      (let* ((bus (funcall make-event-bus-fn))
             (events '())
             (chat-state (funcall make-chat-ui-state-fn :stream-runner nil)))
        (setf (symbol-value event-bus-var) bus)
        (funcall subscribe-fn bus conversation-forked-type
                 (lambda (event)
                   (push event events)))

        (funcall chat-ui-add-message-fn chat-state "user" "fork smoke alpha")
        (funcall chat-ui-add-message-fn chat-state "assistant" "fork smoke beta")
        (funcall chat-ui-add-message-fn chat-state "user" "fork smoke gamma")

        (let* ((fork-output (assert-command chat-state "/fork feature-a 1"))
               (conversation (funcall chat-ui-state-conversation-fn chat-state))
               (forks (funcall conversation-list-forks-fn conversation))
               (feature-record (fork-record-by-name forks "feature-a")))
          (assert-true (contains-text-p fork-output "Created fork")
                       "Expected /fork output to confirm creation, got ~S."
                       fork-output)
          (assert-true (= (length events) 1)
                       "Expected one conversation:forked event, got ~D."
                       (length events))
          (assert-true (eq (funcall event-type-fn (first events))
                           conversation-forked-type)
                       "Expected event type ~S, got ~S."
                       conversation-forked-type
                       (funcall event-type-fn (first events)))
          (assert-true feature-record
                       "Expected fork metadata for feature-a, got ~S."
                       forks)
          (assert-true (= (or (getf feature-record :branch-point) -2) 1)
                       "Expected feature-a branch-point 1, got ~S."
                       (getf feature-record :branch-point))
          (assert-true (= (or (getf feature-record :message-count) -1) 2)
                       "Expected feature-a message-count 2, got ~S."
                       (getf feature-record :message-count)))

        (funcall chat-ui-add-message-fn chat-state "assistant" "main-only marker")

        (let ((switch-output (assert-command chat-state "/switch-fork feature-a")))
          (assert-true (contains-text-p switch-output "Switched to fork feature-a")
                       "Expected /switch-fork output to confirm feature-a, got ~S."
                       switch-output))
        (let ((fork-conversation (funcall chat-ui-state-conversation-fn chat-state)))
          (assert-true (string= (funcall conversation-active-fork-name-fn fork-conversation)
                                "feature-a")
                       "Expected active fork feature-a after switch.")
          (assert-true (not (has-content-p fork-conversation "main-only marker"))
                       "Expected feature-a to exclude main-only marker."))

        (funcall chat-ui-add-message-fn chat-state "assistant" "feature-only marker")

        (let ((switch-output (assert-command chat-state "/switch-fork main")))
          (assert-true (contains-text-p switch-output "Switched to fork main")
                       "Expected /switch-fork output to confirm main, got ~S."
                       switch-output))
        (let* ((main-conversation (funcall chat-ui-state-conversation-fn chat-state))
               (fork-list-output (assert-command chat-state "/forks")))
          (assert-true (string= (funcall conversation-active-fork-name-fn main-conversation)
                                "main")
                       "Expected active fork main after switch.")
          (assert-true (has-content-p main-conversation "main-only marker")
                       "Expected main fork to contain main-only marker.")
          (assert-true (not (has-content-p main-conversation "feature-only marker"))
                       "Expected main fork to exclude feature-only marker.")
          (assert-true (contains-text-p fork-list-output "feature-a")
                       "Expected /forks output to include feature-a, got ~S."
                       fork-list-output)
          (assert-true (contains-text-p fork-list-output "branch-point: 1")
                       "Expected /forks output to include branch-point, got ~S."
                       fork-list-output))

        (let* ((restored (funcall chat-ui-restore-latest-session-fn
                                  (funcall make-chat-ui-state-fn :stream-runner nil)))
               (restored-conversation (funcall chat-ui-state-conversation-fn restored))
               (session-id (funcall conversation-state-session-id-fn restored-conversation))
               (fork-path (funcall conversation-fork-path-fn
                                   session-id
                                   "feature-a"
                                   :project-root tmp-root)))
          (assert-true (probe-file fork-path)
                       "Expected persisted fork file at ~A."
                       (namestring fork-path))
          (assert-command restored "/switch-fork feature-a")
          (let ((switched (funcall chat-ui-state-conversation-fn restored)))
            (assert-true (string= (funcall conversation-active-fork-name-fn switched)
                                  "feature-a")
                         "Expected restored switch to feature-a to update active fork.")
            (assert-true (has-content-p switched "feature-only marker")
                         "Expected feature-a fork to retain feature-only marker after restore.")
            (assert-true (not (has-content-p switched "main-only marker"))
                         "Expected feature-a fork to remain independent after restore."))
          (assert-true (plusp (length (funcall chat-ui-state-messages-fn restored)))
                       "Expected restored chat state to expose non-empty messages.")))))

  (format t "AMOEBUM_FORK_SMOKE_OK~%"))
