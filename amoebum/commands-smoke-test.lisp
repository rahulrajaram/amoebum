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
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn-in
           (lambda (name package)
             (symbol-function (funcall symbol-in name package))))
         (list-commands-fn (funcall fn-in "LIST-SLASH-COMMANDS" amoebum-pkg))
         (command-name-fn (funcall fn-in "SLASH-COMMAND-NAME" amoebum-pkg))
         (find-command-fn (funcall fn-in "FIND-SLASH-COMMAND" amoebum-pkg))
         (parse-command-fn (funcall fn-in "PARSE-SLASH-COMMAND" amoebum-pkg))
         (parse-args-fn (funcall fn-in "PARSE-SLASH-COMMAND-ARGUMENTS" amoebum-pkg))
         (dispatch-fn (funcall fn-in "DISPATCH-SLASH-COMMAND" amoebum-pkg))
         (result-output-fn (funcall fn-in "SLASH-COMMAND-RESULT-OUTPUT" amoebum-pkg))
         (result-action-fn (funcall fn-in "SLASH-COMMAND-RESULT-ACTION" amoebum-pkg))
         (result-echo-fn (funcall fn-in "SLASH-COMMAND-RESULT-ECHO-INPUT-P" amoebum-pkg))
         (result-payload-fn (funcall fn-in "SLASH-COMMAND-RESULT-PAYLOAD" amoebum-pkg))
         (complete-fn (funcall fn-in "COMPLETE-SLASH-COMMAND-INPUT" amoebum-pkg))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (current-config-fn (funcall fn-in "CURRENT-CONFIG" amoebum-pkg))
         (config-mode-fn (funcall fn-in "CONFIG-PERMISSION-MODE" amoebum-pkg))
         (config-model-fn (funcall fn-in "CONFIG-MODEL" amoebum-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (contains-text-p (haystack needle)
               (and (stringp haystack)
                    (search needle haystack :test #'char-equal)))
             (symbol-downcase (value)
               (string-downcase (if (symbolp value)
                                    (symbol-name value)
                                    (princ-to-string value)))))
      (let* ((commands (funcall list-commands-fn))
             (names (mapcar (lambda (command)
                              (symbol-downcase (funcall command-name-fn command)))
                            commands)))
        (dolist (required '("help" "mode" "model" "config" "memory" "clear" "compact" "history" "sounds" "lint" "permissions"))
          (assert-true (member required names :test #'string=)
                       "Expected built-in slash command /~A to be registered. Names=~S"
                       required
                       names)))

      (let ((plan-command (funcall find-command-fn "plan")))
        (assert-true plan-command
                     "Expected built-in slash command /plan to be registered."))

      (multiple-value-bind (handledp result)
          (funcall dispatch-fn "/help")
        (assert-true handledp "Expected /help to be handled.")
        (assert-true (contains-text-p (funcall result-output-fn result) "/mode")
                     "Expected /help output to include /mode usage."))

      (funcall setconfig-fn :permission-mode :supervised)
      (multiple-value-bind (handledp result)
          (funcall dispatch-fn "/mode auto-edit")
        (assert-true handledp "Expected /mode auto-edit to be handled.")
        (assert-true (contains-text-p (funcall result-output-fn result) "auto-edit")
                     "Expected /mode output to mention auto-edit."))
      (let ((cfg (funcall current-config-fn)))
        (assert-true (eq (funcall config-mode-fn cfg) :auto-edit)
                     "Expected /mode to update runtime config permission mode."))

      (multiple-value-bind (handledp result)
          (funcall dispatch-fn "/model smoke-model-128k")
        (assert-true handledp "Expected /model to be handled.")
        (assert-true (contains-text-p (funcall result-output-fn result) "smoke-model-128k")
                     "Expected /model output to mention selected model."))
      (let ((cfg (funcall current-config-fn)))
        (assert-true (string= (funcall config-model-fn cfg) "smoke-model-128k")
                     "Expected /model to update runtime model in config."))

      (multiple-value-bind (handledp result)
          (funcall dispatch-fn "/config")
        (assert-true handledp "Expected /config to be handled.")
        (assert-true (contains-text-p (funcall result-output-fn result) "source:")
                     "Expected /config output to include source attribution."))

      (multiple-value-bind (handledp result)
          (funcall dispatch-fn "/sounds")
        (assert-true handledp "Expected /sounds to be handled.")
        (assert-true (contains-text-p (funcall result-output-fn result) "Sound themes")
                     "Expected /sounds output to list themes."))

      (multiple-value-bind (handledp result)
          (funcall dispatch-fn "/sounds set minimal")
        (assert-true handledp "Expected /sounds set minimal to be handled.")
        (assert-true (contains-text-p (funcall result-output-fn result) "minimal")
                     "Expected /sounds set output to mention minimal theme."))

      (multiple-value-bind (handledp result)
          (funcall dispatch-fn "/memory show")
        (assert-true handledp "Expected /memory show to be handled.")
        (assert-true (contains-text-p (funcall result-output-fn result) "Memory backend:")
                     "Expected /memory show output header, got ~S."
                     (funcall result-output-fn result)))

      (multiple-value-bind (handledp result)
          (funcall dispatch-fn "/clear")
        (assert-true handledp "Expected /clear to be handled.")
        (assert-true (eq (funcall result-action-fn result) :none)
                     "Expected /clear without confirmation to produce no action, got ~S."
                     (funcall result-action-fn result))
        (assert-true (contains-text-p (funcall result-output-fn result) "/clear --yes")
                     "Expected /clear output to include confirmation prompt."))

      (multiple-value-bind (handledp result)
          (funcall dispatch-fn "/clear --yes")
        (assert-true handledp "Expected /clear --yes to be handled.")
        (assert-true (eq (funcall result-action-fn result) :clear-chat)
                     "Expected /clear --yes action :clear-chat, got ~S."
                     (funcall result-action-fn result))
        (assert-true (not (funcall result-echo-fn result))
                     "Expected /clear --yes to suppress command echo."))

      (multiple-value-bind (handledp result)
          (funcall dispatch-fn "/compact 7")
        (assert-true handledp "Expected /compact to be handled.")
        (assert-true (eq (funcall result-action-fn result) :compact-chat)
                     "Expected /compact action :compact-chat, got ~S."
                     (funcall result-action-fn result))
        (assert-true (= (funcall result-payload-fn result) 7)
                     "Expected /compact payload 7, got ~S."
                     (funcall result-payload-fn result)))

      (let* ((mode-command (funcall find-command-fn "mode"))
             (mode-invocation (funcall parse-command-fn "/mode auto-edit")))
        (multiple-value-bind (args errors)
            (funcall parse-args-fn mode-command mode-invocation)
          (assert-true (null errors)
                       "Expected no parse errors for /mode auto-edit, got ~S." errors)
          (assert-true (eq (gethash :MODE args) :auto-edit)
                       "Expected typed keyword :AUTO-EDIT, got ~S."
                       (gethash :MODE args))))

      (let* ((compact-command (funcall find-command-fn "compact"))
             (compact-invocation (funcall parse-command-fn "/compact 9")))
        (multiple-value-bind (args errors)
            (funcall parse-args-fn compact-command compact-invocation)
          (assert-true (null errors)
                       "Expected no parse errors for /compact 9, got ~S." errors)
          (assert-true (= (gethash :KEEP-LAST args) 9)
                       "Expected typed integer 9, got ~S."
                       (gethash :KEEP-LAST args))))

      (multiple-value-bind (replacement suggestions)
          (funcall complete-fn "/mem")
        (assert-true (string= replacement "/memory ")
                     "Expected /mem to complete to /memory, got replacement ~S."
                     replacement)
        (assert-true (and (listp suggestions)
                          (= (length suggestions) 1)
                          (string= (first suggestions) "/memory"))
                     "Expected /mem suggestions to be (/memory), got ~S."
                     suggestions))

      (multiple-value-bind (replacement suggestions)
          (funcall complete-fn "/mo")
        (assert-true (null replacement)
                     "Expected /mo to stay ambiguous with no replacement, got ~S."
                     replacement)
        (assert-true (and (member "/mode" suggestions :test #'string=)
                          (member "/model" suggestions :test #'string=))
                     "Expected /mo suggestions to include /mode and /model, got ~S."
                     suggestions))

      (multiple-value-bind (replacement suggestions)
          (funcall complete-fn "/mode au")
        (assert-true (or (string= replacement "/mode auto-edit ")
                         (member "auto-edit" suggestions :test #'string=))
                     "Expected /mode au completion towards auto-edit, got replacement=~S suggestions=~S."
                     replacement
                     suggestions))))

  (format t "AMOEBUM_COMMANDS_SMOKE_OK~%"))
