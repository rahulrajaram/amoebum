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
    (funcall load-asd-fn (merge-pathnames #P"ptui/ptui.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"amoebum/amoebum.asd" repo-root))
    (funcall load-system-fn "amoebum"))

  (let* ((amoebum-pkg (or (find-package "AMOEBUM")
                          (error "Missing package AMOEBUM after load.")))
         (ptui-defwidget-pkg (or (find-package "PTUI.WIDGETS.DEFWIDGET")
                                 (error "Missing package PTUI.WIDGETS.DEFWIDGET after load.")))
         (uiop-pkg (or (find-package "UIOP")
                       (find-package "ASDF/UTILITY")
                       (error "Missing UIOP package after load.")))
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn-in
           (lambda (name package)
             (symbol-function (funcall symbol-in name package))))
         (deftool-sym (funcall symbol-in "DEFTOOL" amoebum-pkg))
         (defhook-sym (funcall symbol-in "DEFHOOK" amoebum-pkg))
         (defwidget-sym (funcall symbol-in "DEFWIDGET" ptui-defwidget-pkg))
         (reset-deftool-fn (funcall fn-in "RESET-DEFTOOL-COMPILE-VALIDATION-STATE" amoebum-pkg))
         (run-macro-lint-fn (funcall fn-in "RUN-MACRO-LINT" amoebum-pkg))
         (format-macro-lint-report-fn (funcall fn-in "FORMAT-MACRO-LINT-REPORT" amoebum-pkg))
         (dispatch-command-fn (funcall fn-in "DISPATCH-SLASH-COMMAND" amoebum-pkg))
         (slash-output-fn (funcall fn-in "SLASH-COMMAND-RESULT-OUTPUT" amoebum-pkg))
         (temporary-directory-fn (funcall fn-in "TEMPORARY-DIRECTORY" uiop-pkg))
         (ensure-directory-pathname-fn (funcall fn-in "ENSURE-DIRECTORY-PATHNAME" uiop-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (contains-text-p (haystack needle)
               (and (stringp haystack)
                    (search needle haystack :test #'char-equal)))
             (write-text-file (path content)
               (ensure-directories-exist path)
               (with-open-file (stream path
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create
                                       :external-format :utf-8)
                 (write-string content stream))))
      (funcall reset-deftool-fn)

      (let ((deftool-warning-seen nil))
        (handler-bind
            ((warning
               (lambda (condition)
                 (when (contains-text-p (princ-to-string condition)
                                        "Required parameter is missing :description")
                   (setf deftool-warning-seen t))
                 (let ((restart (find-restart 'muffle-warning condition)))
                   (when restart
                     (invoke-restart restart))))))
          (macroexpand-1
           `(,deftool-sym i77-warn-tool
              ((path pathname :required t))
              "I77 validation warning probe."
              (:permission :auto)
              (:timeout 1)
              path)))
        (assert-true deftool-warning-seen
                     "Expected DEFTTOOL to warn when required parameter description is missing."))

      (let ((invalid-type-signaled nil))
        (handler-case
            (macroexpand-1
             `(,deftool-sym i77-invalid-type
                ((value (bogus-type) :description "Value" :required t))
                "I77 invalid type probe."
                (:permission :auto)
                value))
          (error ()
            (setf invalid-type-signaled t)))
        (assert-true invalid-type-signaled
                     "Expected DEFTTOOL invalid type spec to signal an error."))

      (let ((invalid-hook-pattern-signaled nil))
        (handler-case
            (macroexpand-1
             `(,defhook-sym pre-tool-use (tool-name args)
                (:match (:args (:pattern 123))
                 :allow)))
          (error ()
            (setf invalid-hook-pattern-signaled t)))
        (assert-true invalid-hook-pattern-signaled
                     "Expected DEFHOOK invalid pattern syntax to signal an error."))

      (let* ((tmp-root
               (funcall ensure-directory-pathname-fn
                        (merge-pathnames
                         (make-pathname :directory
                                        `(:relative
                                          ,(format nil "amoebum-i77-~A" (get-universal-time))))
                         (funcall temporary-directory-fn))))
             (fixture (merge-pathnames #P"compile-lint-fixture.lisp" tmp-root)))
        (write-text-file
         fixture
         "(in-package :amoebum)
(deftool i77-lint-tool ((path pathname :required t))
  \"Missing description should warn.\"
  (:permission :auto)
  path)
(defhook unknown-hook-point (tool-name args)
  (:match t :allow))
(in-package :ptui.widgets.defwidget)
(defwidget i77-bad-widget (unused-prop)
  42)
")
        (let* ((report (funcall run-macro-lint-fn :paths (list fixture)))
               (warning-count (getf report :warning-count 0))
               (error-count (getf report :error-count 0))
               (text-report (funcall format-macro-lint-report-fn report)))
          (assert-true (>= warning-count 2)
                       "Expected at least 2 lint warnings, got ~S (~S)."
                       warning-count
                       text-report)
          (assert-true (>= error-count 1)
                       "Expected at least 1 lint error, got ~S (~S)."
                       error-count
                       text-report)
          (assert-true (contains-text-p text-report "unknown hook point")
                       "Expected lint report to include unknown hook point warning, got ~S."
                       text-report))

        (multiple-value-bind (handledp result)
            (funcall dispatch-command-fn (format nil "/lint ~A" (namestring fixture)))
          (let ((output (and result (funcall slash-output-fn result))))
            (assert-true handledp "Expected /lint command to be handled.")
            (assert-true (contains-text-p output "Macro lint found")
                         "Expected /lint output to report issues, got ~S."
                         output)
            (assert-true (contains-text-p output "deftool")
                         "Expected /lint output to include deftool issue details, got ~S."
                         output)))))

    (format t "AMOEBUM_COMPILE_VALIDATION_SMOKE_OK~%")))
