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
         (pseudopod-pkg (or (find-package "PSEUDOPOD")
                            (error "Missing package PSEUDOPOD after load.")))
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn-in
           (lambda (name package)
             (symbol-function (funcall symbol-in name package))))
         (make-toolset-fn (funcall fn-in "MAKE-TOOLSET" pseudopod-pkg))
         (deftool-sym (funcall symbol-in "DEFTOOL" amoebum-pkg))
         (toolset-sym (funcall symbol-in "*TOOLSET*" amoebum-pkg))
         (metadata-sym (funcall symbol-in "*TOOL-METADATA*" amoebum-pkg))
         (execute-fn (funcall fn-in "EXECUTE-TOOL-WITH-RESTARTS" amoebum-pkg))
         (condition-context-fn (funcall fn-in "CONDITION-TO-LLM-CONTEXT" amoebum-pkg))
         (tool-error-sym (funcall symbol-in "TOOL-ERROR" amoebum-pkg))
         (tool-timeout-sym (funcall symbol-in "TOOL-TIMEOUT" amoebum-pkg))
         (tool-not-found-sym (funcall symbol-in "TOOL-NOT-FOUND" amoebum-pkg))
         (tool-argument-error-sym (funcall symbol-in "TOOL-ARGUMENT-ERROR" amoebum-pkg))
         (retry-restart-sym (or (find-symbol "RETRY-TOOL" amoebum-pkg)
                                (error "Missing restart symbol RETRY-TOOL in package AMOEBUM.")))
         (skip-restart-sym (or (find-symbol "SKIP-TOOL" amoebum-pkg)
                               (error "Missing restart symbol SKIP-TOOL in package AMOEBUM.")))
         (use-value-restart-sym (or (find-symbol "USE-VALUE" amoebum-pkg)
                                    (error "Missing restart symbol USE-VALUE in package AMOEBUM.")))
         (selector-sym (funcall symbol-in "*SUPERVISED-RESTART-SELECTOR*" amoebum-pkg))
         (retry-count-sym (intern "*I32-RETRY-COUNT*" amoebum-pkg))
         (*package* amoebum-pkg)
         )
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (make-args (&rest pairs)
               (let ((table (make-hash-table :test #'equal)))
                 (loop for (key value) on pairs by #'cddr do
                       (setf (gethash key table) value))
                 table)))
      (setf (symbol-value toolset-sym) (funcall make-toolset-fn))
      (clrhash (symbol-value metadata-sym))
      (eval `(defparameter *i32-retry-count* 0))

      (eval
       `(,deftool-sym i32-flaky
          ()
          "Flaky tool for retry restart smoke."
          (:permission :auto)
          (:dangerous nil)
          (:category :smoke)
          (:timeout 5)
          (incf *i32-retry-count*)
          (if (= *i32-retry-count* 1)
              (error "flaky failure")
              "retried-ok")))

      (eval
       `(,deftool-sym i32-fail
          ()
          "Always failing tool for restart smoke."
          (:permission :auto)
          (:dangerous nil)
          (:category :smoke)
          (:timeout 5)
          (error "always-fail")))

      (eval
       `(,deftool-sym i32-timeout
          ()
          "Timeout tool for condition smoke."
          (:permission :auto)
          (:dangerous nil)
          (:category :smoke)
          (:timeout 1)
          (sleep 2)
          "timeout-unexpected"))

      (eval
       `(,deftool-sym i32-arg
          ((count integer :description "Count value" :required t))
          "Argument validation tool for condition smoke."
          (:permission :auto)
          (:dangerous nil)
          (:category :smoke)
          (:timeout 5)
          (format nil "count=~D" count)))

      (let ((seen-not-found nil))
        (handler-case
            (funcall execute-fn "missing-tool" (make-args) :permission-mode :full-auto)
          (error (condition)
            (when (typep condition tool-not-found-sym)
              (setf seen-not-found t))))
        (assert-true seen-not-found
                     "Expected missing tool call to signal TOOL-NOT-FOUND."))

      (let ((result
              (handler-bind
                  ((error
                     (lambda (condition)
                       (when (typep condition tool-error-sym)
                         (invoke-restart skip-restart-sym)))))
                (funcall execute-fn "i32-fail" (make-args) :permission-mode :full-auto))))
        (assert-true (and (stringp result)
                          (search "skipped" result :test #'char-equal))
                     "Expected skip-tool restart to return skip message, got ~S." result))

      (let ((result
              (handler-bind
                  ((error
                     (lambda (condition)
                       (when (typep condition tool-error-sym)
                         (invoke-restart use-value-restart-sym "fallback")))))
                (funcall execute-fn "i32-fail" (make-args) :permission-mode :full-auto))))
        (assert-true (string= result "fallback")
                     "Expected use-value restart to return replacement value."))

      (setf (symbol-value retry-count-sym) 0)
      (let ((retry-issued nil)
            (retry-invoked nil))
        (flet ((handle-retry (condition)
                 (when (typep condition tool-error-sym)
                   (let ((context (funcall condition-context-fn condition)))
                     (assert-true (search "retry-tool" context :test #'char-equal)
                                  "Expected condition-to-llm-context to mention retry-tool.")
                     (if retry-issued
                         (invoke-restart use-value-restart-sym "retry-failed")
                         (progn
                           (setf retry-issued t)
                           (setf retry-invoked t)
                           (invoke-restart retry-restart-sym)))))))
          (let ((result (handler-bind ((error #'handle-retry))
                          (funcall execute-fn
                                   "i32-flaky"
                                   (make-args)
                                   :permission-mode :full-auto))))
            (assert-true retry-invoked
                         "Expected retry-tool restart to be invoked.")
            (assert-true (stringp result)
                         "Expected retry path to return a string result.")))

      (let ((seen-argument-error nil))
        (handler-case
            (funcall execute-fn "i32-arg" (make-args) :permission-mode :full-auto)
          (error (condition)
            (when (typep condition tool-argument-error-sym)
              (setf seen-argument-error t))))
        (assert-true seen-argument-error
                     "Expected missing required argument to signal TOOL-ARGUMENT-ERROR."))

      (let ((seen-timeout nil))
        (handler-case
            (funcall execute-fn "i32-timeout" (make-args) :permission-mode :full-auto)
          (error (condition)
            (when (or (typep condition tool-timeout-sym)
                      (typep condition tool-error-sym))
              (setf seen-timeout t))))
        (assert-true seen-timeout
                     "Expected timeout tool to signal a typed tool condition."))

      (let ((selector-called nil))
        (let ((original-selector (symbol-value selector-sym)))
          (unwind-protect
               (progn
                 (setf (symbol-value selector-sym)
                       (lambda (condition)
                         (declare (ignore condition))
                         (setf selector-called t)
                         '(use-value "supervised-value")))
                 (let ((result (funcall execute-fn "i32-fail"
                                        (make-args)
                                        :permission-mode :supervised)))
                   (assert-true selector-called
                                "Expected supervised selector to be invoked.")
                   (assert-true (string= result "supervised-value")
                                "Expected supervised selector to apply use-value restart.")))
            (setf (symbol-value selector-sym) original-selector))))))

  (format t "AMOEBUM_CONDITIONS_SMOKE_OK~%")))
