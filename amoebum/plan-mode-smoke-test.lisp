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
         (dispatch-fn (funcall fn-in "DISPATCH-SLASH-COMMAND" amoebum-pkg))
         (result-output-fn (funcall fn-in "SLASH-COMMAND-RESULT-OUTPUT" amoebum-pkg))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (current-config-fn (funcall fn-in "CURRENT-CONFIG" amoebum-pkg))
         (config-value-fn (funcall fn-in "CONFIG-VALUE" amoebum-pkg))
         (make-status-bar-state-fn (funcall fn-in "MAKE-STATUS-BAR-STATE" amoebum-pkg))
         (status-bar-line-fn (funcall fn-in "STATUS-BAR-LINE" amoebum-pkg))
         (clear-steps-fn (funcall fn-in "CLEAR-PLAN-MODE-STEPS" amoebum-pkg))
         (add-plan-step-fn (funcall fn-in "ADD-PLAN-STEP" amoebum-pkg))
         (current-plan-state-fn (funcall fn-in "CURRENT-PLAN-MODE-STATE" amoebum-pkg))
         (plan-output-path-fn (funcall fn-in "PLAN-MODE-STATE-LAST-OUTPUT-PATH" amoebum-pkg))
         (make-context-fn (funcall fn-in "MAKE-AMOEBUM-CONTEXT" amoebum-pkg))
         (execute-tool-fn (funcall fn-in "EXECUTE-TOOL" amoebum-pkg))
         (toolset-sym (funcall symbol-in "*TOOLSET*" amoebum-pkg))
         (permission-denied-sym (funcall symbol-in "TOOL-PERMISSION-DENIED" amoebum-pkg))
         (make-tool-call-fn (funcall fn-in "MAKE-TOOL-CALL" pseudopod-pkg))
         (temporary-directory-fn (funcall fn-in "TEMPORARY-DIRECTORY" uiop-pkg))
         (ensure-directory-pathname-fn (funcall fn-in "ENSURE-DIRECTORY-PATHNAME" uiop-pkg))
         (read-file-string-fn (funcall fn-in "READ-FILE-STRING" uiop-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (contains-text-p (haystack needle)
               (and (stringp haystack)
                    (search needle haystack :test #'char-equal)))
             (bool-true-p (value)
               (not (null value))))
      (funcall setconfig-fn :plan-mode nil)
      (funcall setconfig-fn :permission-mode :full-auto)
      (funcall clear-steps-fn)

      (let ((status-state (funcall make-status-bar-state-fn
                                   :config (funcall current-config-fn))))
        (multiple-value-bind (handledp plan-on-result)
            (funcall dispatch-fn "/plan on")
          (assert-true handledp "Expected /plan on to be handled.")
          (assert-true (contains-text-p (funcall result-output-fn plan-on-result) "Plan mode enabled")
                       "Expected /plan on output to mention enabled state, got ~S."
                       (funcall result-output-fn plan-on-result)))
        (assert-true (bool-true-p (funcall config-value-fn :plan-mode (funcall current-config-fn)))
                     "Expected :plan-mode config value to be true after /plan on.")
        (assert-true (contains-text-p (funcall status-bar-line-fn status-state) "PLAN MODE -- read-only")
                     "Expected status bar to show plan mode banner while active."))

      (funcall add-plan-step-fn
               "Review target implementation files."
               :file-paths (list "amoebum/src/plan-mode.lisp"
                                 "amoebum/src/commands.lisp")
               :risk :low)

      (let* ((tmp-root
               (funcall ensure-directory-pathname-fn
                        (merge-pathnames
                         (make-pathname :directory
                                        `(:relative ,(format nil "amoebum-i40-~A"
                                                              (get-universal-time))))
                         (funcall temporary-directory-fn))))
             (read-target (merge-pathnames #P"plan-mode-read.txt" tmp-root))
             (write-target (merge-pathnames #P"plan-mode-write.txt" tmp-root))
             (context (funcall make-context-fn
                               :toolset (symbol-value toolset-sym)
                               :permission-mode :full-auto)))
        (ensure-directories-exist read-target)
        (with-open-file (stream read-target
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
          (write-line "hello plan mode" stream))

        (let* ((read-call (funcall make-tool-call-fn
                                   :name "read-file"
                                   :arguments (format nil "{\"path\":\"~A\"}"
                                                      (namestring read-target))))
               (read-output (funcall execute-tool-fn read-call context)))
          (assert-true (contains-text-p read-output "hello plan mode")
                       "Expected read-file to remain allowed in plan mode, got ~S."
                       read-output))

        (let ((write-call (funcall make-tool-call-fn
                                   :name "write-file"
                                   :arguments (format nil
                                                      "{\"path\":\"~A\",\"content\":\"blocked\"}"
                                                      (namestring write-target))))
              (saw-denied nil))
          (handler-case
              (funcall execute-tool-fn write-call context)
            (error (condition)
              (when (typep condition permission-denied-sym)
                (setf saw-denied t))))
          (assert-true saw-denied
                       "Expected write-file to be blocked during plan mode.")))

      (let ((status-state (funcall make-status-bar-state-fn
                                   :config (funcall current-config-fn))))
        (multiple-value-bind (handledp plan-off-result)
            (funcall dispatch-fn "/plan off")
          (assert-true handledp "Expected /plan off to be handled.")
          (assert-true (contains-text-p (funcall result-output-fn plan-off-result)
                                        "Plan mode disabled")
                       "Expected /plan off output to mention disabled state, got ~S."
                       (funcall result-output-fn plan-off-result)))
        (assert-true (not (bool-true-p (funcall config-value-fn :plan-mode (funcall current-config-fn))))
                     "Expected :plan-mode config value to be false after /plan off.")
        (assert-true (not (contains-text-p (funcall status-bar-line-fn status-state)
                                           "PLAN MODE -- read-only"))
                     "Expected status bar to hide plan mode banner after exit."))

      (let* ((plan-state (funcall current-plan-state-fn))
             (output-path (funcall plan-output-path-fn plan-state))
             (output-text (and output-path
                               (probe-file output-path)
                               (funcall read-file-string-fn output-path))))
        (assert-true (and output-path (probe-file output-path))
                     "Expected plan mode exit to write plan output file, got ~S."
                     output-path)
        (assert-true (contains-text-p output-text "# Amoebum Plan")
                     "Expected plan output markdown header, got ~S."
                     output-text)
        (assert-true (contains-text-p output-text "Review target implementation files.")
                     "Expected plan output to include captured step, got ~S."
                     output-text))))

  (format t "AMOEBUM_PLAN_MODE_SMOKE_OK~%"))
