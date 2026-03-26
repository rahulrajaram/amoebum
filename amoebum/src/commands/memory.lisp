(in-package :amoebum)

(defun %memory-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((tail (or (gethash :ARGS arguments) ""))
         (line (if (%slash-blank-p tail)
                   "/memory"
                   (format nil "/memory ~A" (%slash-trim tail))))
         (backend (or (slash-command-context-memory-backend context)
                      (current-memory-backend))))
    (multiple-value-bind (handledp response)
        (run-memory-command line :backend backend)
      (declare (ignore handledp))
      (make-slash-command-result
       :output response
       :echo-input-p t))))

(defun %memory-import-export-completions (index fragment option backend)
  (cond
    ((= index 1)
     (let ((prefix (%slash-trim fragment)))
       (if (%starts-with-ci-p prefix option)
           (list option)
           '())))
    ((= index 2)
     (let ((prefix (%slash-trim fragment)))
       (if (%starts-with-ci-p prefix backend)
           (list backend)
           '())))
    (t
     '())))

(defun %memory-arg-completer (_command _invocation index fragment prefix-tokens)
  (declare (ignore _command _invocation))
  (if (= index 0)
      (let ((prefix (%slash-trim fragment)))
        (loop for subcommand in *memory-command-subcommands*
              when (%starts-with-ci-p prefix subcommand)
                collect subcommand))
      (let ((head (and prefix-tokens (string-downcase (first prefix-tokens)))))
        (cond
          ((member head '("show" "edit" "clear") :test #'string=)
           '())
          ((string= head "import")
           (%memory-import-export-completions index fragment "--to" "haake"))
          ((string= head "export")
           (%memory-import-export-completions index fragment "--from" "haake"))
          (t
           nil)))))

(defun register-memory-slash-commands ()
  (register-slash-command
   (make-slash-command
    :name "memory"
    :description "Memory controls: show/edit/clear/remember/forget/import/export."
    :usage "/memory [show|edit|clear|remember <text>|forget <key>|import --to haake|export --from haake]"
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Memory subcommand and arguments."))
    :handler #'%memory-handler
    :completer #'%memory-arg-completer))
  t)
