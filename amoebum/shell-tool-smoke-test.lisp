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
         (uiop-pkg (or (find-package "UIOP")
                       (find-package "ASDF/UTILITY")
                       (error "Missing UIOP package after requiring ASDF.")))
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn-in
           (lambda (name package)
             (symbol-function (funcall symbol-in name package))))
         (temporary-directory-fn (funcall fn-in "TEMPORARY-DIRECTORY" uiop-pkg))
         (ensure-directory-pathname-fn (funcall fn-in "ENSURE-DIRECTORY-PATHNAME" uiop-pkg))
         (find-tool-fn (funcall fn-in "FIND-TOOL" pseudopod-pkg))
         (tool-definition-fn-fn (funcall fn-in "TOOL-DEFINITION-FN" pseudopod-pkg))
         (toolset-sym (funcall symbol-in "*TOOLSET*" amoebum-pkg))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (clear-permission-rules-fn (funcall fn-in "CLEAR-PERMISSION-RULES" amoebum-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (contains-substring-p (needle haystack)
               (not (null (search needle haystack :test #'char=))))
             (normalize-dir-text (value)
               (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value))
                      (len (length trimmed)))
                 (if (and (> len 1)
                          (char= (char trimmed (1- len)) #\/))
                     (subseq trimmed 0 (1- len))
                     trimmed)))
             (make-args (&rest key-values)
               (let ((args (make-hash-table :test #'equal)))
                 (loop for (key value) on key-values by #'cddr do
                       (setf (gethash key args) value))
                 args))
             (invoke-tool (tool-name &rest key-values)
               (let* ((toolset (symbol-value toolset-sym))
                      (tool (funcall find-tool-fn toolset tool-name)))
                 (assert-true tool "Expected tool ~S to be registered." tool-name)
                 (funcall (funcall tool-definition-fn-fn tool)
                          (apply #'make-args key-values))))
             (wait-for-task (task-id)
               (loop repeat 60 do
                     (let ((poll (invoke-tool "bash-exec" "task-id" task-id)))
                       (when (member (getf poll :status) '(:completed :timeout) :test #'eq)
                         (return poll)))
                     (sleep 0.1)
                     finally (error "Timed out waiting for background task ~A." task-id))))
      (funcall setconfig-fn :permission-mode :full-auto)
      (funcall clear-permission-rules-fn)

      (let* ((tmp-root
               (funcall ensure-directory-pathname-fn
                        (merge-pathnames
                         (make-pathname :directory `(:relative ,(format nil "amoebum-i29-~A"
                                                                        (get-universal-time))))
                         (funcall temporary-directory-fn))))
             (tmp-root-text (normalize-dir-text (namestring tmp-root))))
        (ensure-directories-exist (merge-pathnames #P"workspace/" tmp-root))

        (let ((result (invoke-tool "bash-exec"
                                   "command" "echo out; echo err 1>&2; exit 7"
                                   "cwd" (namestring tmp-root)
                                   "timeout-seconds" 10)))
          (assert-true (eq (getf result :status) :completed)
                       "Expected foreground bash-exec to complete.")
          (assert-true (= (getf result :exit-code) 7)
                       "Expected bash-exec to return child exit code.")
          (assert-true (contains-substring-p "out" (getf result :stdout))
                       "Expected bash-exec stdout capture.")
          (assert-true (contains-substring-p "err" (getf result :stderr))
                       "Expected bash-exec stderr capture."))

        (let* ((pwd-result (invoke-tool "bash-exec"
                                        "command" "pwd"))
               (pwd-text (normalize-dir-text (getf pwd-result :stdout))))
          (assert-true (string= pwd-text tmp-root-text)
                       "Expected bash-exec to persist working directory between calls."))

        (let ((truncation-result
                (invoke-tool "bash-exec"
                             "command" "printf 1234567890; printf abcdef 1>&2"
                             "max-output-chars" 4)))
          (assert-true (string= (getf truncation-result :stdout) "1234")
                       "Expected stdout truncation to configured size.")
          (assert-true (string= (getf truncation-result :stderr) "abcd")
                       "Expected stderr truncation to configured size.")
          (assert-true (getf truncation-result :stdout-truncated-p)
                       "Expected stdout truncation flag to be true.")
          (assert-true (getf truncation-result :stderr-truncated-p)
                       "Expected stderr truncation flag to be true."))

        (let ((timeout-result (invoke-tool "bash-exec"
                                           "command" "sleep 2"
                                           "timeout-seconds" 1)))
          (assert-true (eq (getf timeout-result :status) :timeout)
                       "Expected timeout status for long-running command.")
          (assert-true (getf timeout-result :timed-out)
                       "Expected timed-out flag for long-running command."))

        (let ((saw-prompt nil))
          (handler-case
              (invoke-tool "bash-exec"
                           "command" "git push --force origin main")
            (error ()
              (setf saw-prompt t)))
          (assert-true saw-prompt
                       "Expected dangerous command to require approval in full-auto mode."))

        (funcall setconfig-fn :permission-mode :yolo)
        (let ((yolo-result (invoke-tool "bash-exec"
                                        "command" "git push --force origin main"
                                        "timeout-seconds" 10)))
          (assert-true (eq (getf yolo-result :status) :completed)
                       "Expected yolo mode to execute dangerous command without prompt.")
          (assert-true (integerp (getf yolo-result :exit-code))
                       "Expected dangerous command execution to return an exit code."))

        (funcall setconfig-fn :permission-mode :full-auto)
        (let* ((launch-result
                 (invoke-tool "bash-exec"
                              "command" "echo bg-out; echo bg-err 1>&2; sleep 1"
                              "background" t
                              "timeout-seconds" 10))
               (task-id (getf launch-result :task-id))
               (final-result (wait-for-task task-id))
               (task-payload (getf final-result :result)))
          (assert-true (eq (getf launch-result :status) :running)
                       "Expected background launch to return running task status.")
          (assert-true (and (stringp task-id) (> (length task-id) 0))
                       "Expected background launch to return task-id.")
          (assert-true (eq (getf final-result :status) :completed)
                       "Expected background task to complete.")
          (assert-true (contains-substring-p "bg-out" (getf task-payload :stdout))
                       "Expected background task to capture stdout.")
          (assert-true (contains-substring-p "bg-err" (getf task-payload :stderr))
                       "Expected background task to capture stderr.")))))

  (format t "AMOEBUM_SHELL_TOOL_SMOKE_OK~%"))
