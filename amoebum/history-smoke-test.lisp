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
         (events-pkg (or (find-package "PTUI.CORE.EVENTS")
                         (error "Missing package PTUI.CORE.EVENTS after load.")))
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
         (conversation-add-message-fn (funcall fn-in "CONVERSATION-STATE-ADD-MESSAGE" amoebum-pkg))
         (history-search-fn (funcall fn-in "HISTORY-SEARCH" amoebum-pkg))
         (result-entry-fn (funcall fn-in "CONVERSATION-HISTORY-SEARCH-RESULT-ENTRY" amoebum-pkg))
         (result-before-fn (funcall fn-in "CONVERSATION-HISTORY-SEARCH-RESULT-BEFORE" amoebum-pkg))
         (entry-role-fn (funcall fn-in "CONVERSATION-HISTORY-ENTRY-ROLE" amoebum-pkg))
         (entry-name-fn (funcall fn-in "CONVERSATION-HISTORY-ENTRY-NAME" amoebum-pkg))
         (dispatch-fn (funcall fn-in "DISPATCH-SLASH-COMMAND" amoebum-pkg))
         (result-output-fn (funcall fn-in "SLASH-COMMAND-RESULT-OUTPUT" amoebum-pkg))
         (result-action-fn (funcall fn-in "SLASH-COMMAND-RESULT-ACTION" amoebum-pkg))
         (make-chat-ui-state-fn (funcall fn-in "MAKE-CHAT-UI-STATE" amoebum-pkg))
         (chat-input-fn (funcall fn-in "CHAT-UI-STATE-INPUT-TEXT" amoebum-pkg))
         (chat-history-mode-fn (funcall fn-in "CHAT-UI-STATE-HISTORY-SEARCH-ACTIVE-P" amoebum-pkg))
         (chat-picker-state-fn (funcall fn-in "CHAT-UI-STATE-FUZZY-PICKER-STATE" amoebum-pkg))
         (picker-active-fn (funcall fn-in "FUZZY-PICKER-STATE-ACTIVE-P" amoebum-pkg))
         (picker-label-fn (funcall fn-in "FUZZY-PICKER-STATE-CONTEXT-LABEL" amoebum-pkg))
         (handle-chat-ui-event-fn (funcall fn-in "HANDLE-CHAT-UI-EVENT" amoebum-pkg))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (make-message-fn (funcall fn-in "MAKE-MESSAGE" pseudopod-pkg))
         (make-key-event-fn (funcall fn-in "MAKE-KEY-EVENT" events-pkg))
         (tmp-root (uiop:ensure-directory-pathname
                    (merge-pathnames
                     (make-pathname :directory
                                    `(:relative
                                      ,(format nil "amoebum-i74-~D-~D"
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

      (let* ((base-ts (get-universal-time))
             (conversation
               (funcall make-conversation-state-fn
                        :project-root tmp-root
                        :session-id "i74-history-smoke")))
        (funcall conversation-add-message-fn conversation
                 (funcall make-message-fn :role "user" :content "deploy alpha") :timestamp base-ts)
        (funcall conversation-add-message-fn conversation
                 (funcall make-message-fn :role "assistant" :content "ack deploy alpha")
                 :timestamp (1+ base-ts))
        (funcall conversation-add-message-fn conversation
                 (funcall make-message-fn :role "tool" :name "read-file" :content "opened src/main.lisp")
                 :timestamp (+ base-ts 2))
        (funcall conversation-add-message-fn conversation
                 (funcall make-message-fn :role "user" :content "deploy beta")
                 :timestamp (+ base-ts 3))

        (let ((content-results (funcall history-search-fn conversation :query "deploy" :limit 5))
              (role-results (funcall history-search-fn conversation :role "tool" :limit 5))
              (range-results (funcall history-search-fn conversation
                                       :since (1+ base-ts)
                                       :until (+ base-ts 2)
                                       :limit 5))
              (tool-results (funcall history-search-fn conversation :tool "read-file" :limit 5)))
          (assert-true (>= (length content-results) 2)
                       "Expected content search to return deploy messages.")
          (assert-true (string= (funcall entry-role-fn
                                         (funcall result-entry-fn (first role-results)))
                                "tool")
                       "Expected role=tool search to return tool entries.")
          (assert-true (= (length range-results) 2)
                       "Expected timestamp range search to return exactly two entries.")
          (assert-true (and tool-results
                            (string-equal (or (funcall entry-name-fn
                                                       (funcall result-entry-fn (first tool-results)))
                                              "")
                                          "read-file"))
                       "Expected tool filter to match tool name read-file.")
          (assert-true (funcall result-before-fn (first tool-results))
                       "Expected ranked history result to include surrounding context."))

        (let ((chat-state (funcall make-chat-ui-state-fn
                                   :stream-runner nil
                                   :conversation conversation)))
          (multiple-value-bind (handledp result)
              (funcall dispatch-fn
                       (format nil "/history deploy --role user --since ~D --until ~D --limit 5"
                               base-ts
                               (+ base-ts 3))
                       :chat-state chat-state)
            (let ((output (funcall result-output-fn result)))
              (assert-true handledp "Expected /history to be handled.")
              (assert-true (contains-text-p output "role=user")
                           "Expected /history output to include role filter.")
              (assert-true (contains-text-p output "USER:")
                           "Expected /history output to include matching user entries.")))

          (multiple-value-bind (handledp result)
              (funcall dispatch-fn
                       (format nil "/history --tool read-file --since ~D --until ~D"
                               base-ts
                               (+ base-ts 3))
                       :chat-state chat-state)
            (let ((output (funcall result-output-fn result)))
              (assert-true handledp "Expected /history tool-filter query to be handled.")
              (assert-true (contains-text-p output "tool=read-file")
                           "Expected /history output to include tool filter.")
              (assert-true (contains-text-p output "TOOL:")
                           "Expected /history tool-filter query to include tool entry.")))

          (multiple-value-bind (handledp result)
              (funcall dispatch-fn
                       (format nil "/history --role=tool --limit=1 tool=read-file since=~D until=~D"
                               base-ts
                               (+ base-ts 3))
                       :chat-state chat-state)
            (let ((output (funcall result-output-fn result)))
              (assert-true handledp "Expected /history key=value query to be handled.")
              (assert-true (contains-text-p output "role=tool")
                           "Expected /history key=value query to include normalized role filter.")
              (assert-true (contains-text-p output "limit=1")
                           "Expected /history key=value query to include explicit limit.")
              (assert-true (contains-text-p output "TOOL:")
                           "Expected /history key=value query to include tool entry.")))

          (multiple-value-bind (handledp result)
              (funcall dispatch-fn
                       (format nil "/history --since ~D --until ~D --limit nope"
                               (+ base-ts 3)
                               base-ts)
                       :chat-state chat-state)
            (let ((output (funcall result-output-fn result)))
              (assert-true handledp "Expected invalid /history query to still be handled.")
              (assert-true (contains-text-p output "Invalid integer \"nope\" for --limit.")
                           "Expected invalid /history query to report bad limit, got ~S."
                           output)
              (assert-true (contains-text-p output "Timestamp range is invalid")
                           "Expected invalid /history query to report bad timestamp range, got ~S."
                           output)
              (assert-true (contains-text-p output "Usage: /history")
                           "Expected invalid /history query to include usage, got ~S."
                           output)))

          (multiple-value-bind (handledp result)
              (funcall dispatch-fn "/clear" :chat-state chat-state)
            (assert-true handledp "Expected /clear prompt to be handled.")
            (assert-true (eq (funcall result-action-fn result) :none)
                         "Expected /clear without --yes to require confirmation."))
          (multiple-value-bind (handledp result)
              (funcall dispatch-fn "/clear --yes" :chat-state chat-state)
            (assert-true handledp "Expected /clear --yes to be handled.")
            (assert-true (eq (funcall result-action-fn result) :clear-chat)
                         "Expected /clear --yes to emit :clear-chat action.")))

        (let* ((chat-state (funcall make-chat-ui-state-fn
                                    :stream-runner nil
                                    :conversation conversation
                                    :input-text "draft prompt"))
               (opened (funcall handle-chat-ui-event-fn chat-state
                                (funcall make-key-event-fn :ctrl-r :ctrlp t))))
          (assert-true (funcall chat-history-mode-fn opened)
                       "Expected Ctrl-R to activate history fuzzy search mode.")
          (assert-true (funcall picker-active-fn (funcall chat-picker-state-fn opened))
                       "Expected history fuzzy picker to be active after Ctrl-R.")
         (assert-true (string= (funcall picker-label-fn
                                         (funcall chat-picker-state-fn opened))
                                "history")
                       "Expected Ctrl-R picker to reuse fuzzy picker in history mode.")
          (let ((closed (funcall handle-chat-ui-event-fn opened
                                 (funcall make-key-event-fn :escape))))
            (assert-true (not (funcall chat-history-mode-fn closed))
                         "Expected Escape to close Ctrl-R history search.")
            (assert-true (string= (funcall chat-input-fn closed) "draft prompt")
                         "Expected Escape to restore pre-search input text."))))))

  (format t "AMOEBUM_HISTORY_SMOKE_OK~%"))
