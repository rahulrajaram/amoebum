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
         (sandbox-read-from-string-fn (funcall fn-in "SANDBOX-READ-FROM-STRING" amoebum-pkg))
         (safe-open-fn (funcall fn-in "SAFE-OPEN" amoebum-pkg))
         (make-rule-fn (funcall fn-in "MAKE-PERMISSION-RULE" amoebum-pkg))
         (truncate-output-fn (funcall fn-in "TRUNCATE-SANDBOX-OUTPUT" amoebum-pkg))
         (max-output-size (symbol-value (funcall symbol-in "+SANDBOX-MAX-OUTPUT-SIZE+" amoebum-pkg)))
         (sandbox-read-eval-disabled-sym (funcall symbol-in "SANDBOX-READ-EVAL-DISABLED" amoebum-pkg))
         (sandbox-violation-sym (funcall symbol-in "SANDBOX-VIOLATION" amoebum-pkg))
         (sandbox-read-size-exceeded-sym (funcall symbol-in "SANDBOX-READ-SIZE-EXCEEDED" amoebum-pkg))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (config-value-fn (funcall fn-in "CONFIG-VALUE" amoebum-pkg))
         (current-config-fn (funcall fn-in "CURRENT-CONFIG" amoebum-pkg))
         (make-toolset-fn (funcall fn-in "MAKE-TOOLSET" pseudopod-pkg))
         (make-tool-call-fn (funcall fn-in "MAKE-TOOL-CALL" pseudopod-pkg))
         (make-context-fn (funcall fn-in "MAKE-AMOEBUM-CONTEXT" amoebum-pkg))
         (execute-tool-fn (funcall fn-in "EXECUTE-TOOL" amoebum-pkg))
         (ensure-directory-pathname-fn (funcall fn-in "ENSURE-DIRECTORY-PATHNAME" uiop-pkg))
         (temporary-directory-fn (funcall fn-in "TEMPORARY-DIRECTORY" uiop-pkg))
         (deftool-sym (funcall symbol-in "DEFTOOL" amoebum-pkg))
         (toolset-sym (funcall symbol-in "*TOOLSET*" amoebum-pkg))
         (metadata-sym (funcall symbol-in "*TOOL-METADATA*" amoebum-pkg))
         (*package* amoebum-pkg))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args)))))
      (let ((blocked-read-eval nil))
        (handler-case
            (funcall sandbox-read-from-string-fn "#.(+ 1 2)")
          (error (condition)
            (when (typep condition sandbox-read-eval-disabled-sym)
              (setf blocked-read-eval t))))
        (assert-true blocked-read-eval
                     "Expected sandbox readtable to block #. read-eval forms."))

      (let* ((tmp-root (funcall ensure-directory-pathname-fn
                                (merge-pathnames
                                 (make-pathname :directory
                                                `(:relative
                                                  ,(format nil "amoebum-i78-~D-~D"
                                                           (get-universal-time)
                                                           (random 1000000))))
                                 (funcall ensure-directory-pathname-fn
                                          (funcall temporary-directory-fn)))))
             (allowed-path (merge-pathnames #P"allowed.txt" tmp-root))
             (denied-path (merge-pathnames #P"denied.txt" tmp-root))
             (tmp-root-text (namestring tmp-root))
             (rules (list (funcall make-rule-fn
                                   :effect :allow
                                   :path (format nil "~A**" tmp-root-text)
                                   :tool :read-file
                                   :source :project)
                          (funcall make-rule-fn
                                   :effect :deny
                                   :path (namestring denied-path)
                                   :tool :read-file
                                   :source :project))))
        (ensure-directories-exist allowed-path)
        (with-open-file (stream allowed-path
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :external-format :utf-8)
          (write-line "allowed" stream))
        (with-open-file (stream denied-path
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :external-format :utf-8)
          (write-line "denied" stream))

        (let ((saw-deny nil))
          (handler-case
              (with-open-stream (stream (funcall safe-open-fn denied-path
                                                 :tool :read-file
                                                 :direction :input
                                                 :permission-mode :full-auto
                                                 :rules rules))
                (declare (ignore stream)))
            (error (condition)
              (when (typep condition sandbox-violation-sym)
                (setf saw-deny t))))
          (assert-true saw-deny
                       "Expected safe-open to reject denied path by permission rule."))

        (with-open-stream (stream (funcall safe-open-fn allowed-path
                                           :tool :read-file
                                           :direction :input
                                           :permission-mode :full-auto
                                           :rules rules
                                           :external-format :utf-8))
          (assert-true (string= (or (read-line stream nil nil) "") "allowed")
                       "Expected safe-open to allow and read authorized file."))

        (let ((saw-max-read nil))
          (handler-case
              (with-open-stream (stream (funcall safe-open-fn allowed-path
                                                 :tool :read-file
                                                 :direction :input
                                                 :permission-mode :full-auto
                                                 :rules rules
                                                 :max-read-size 1
                                                 :external-format :utf-8))
                (declare (ignore stream)))
            (error (condition)
              (when (typep condition sandbox-read-size-exceeded-sym)
                (setf saw-max-read t))))
          (assert-true saw-max-read
                       "Expected safe-open to enforce max-read-size guard.")))

      (let* ((payload (make-string (+ max-output-size 256) :initial-element #\x))
             (truncated nil)
             (truncated-p nil))
        (multiple-value-setq (truncated truncated-p)
          (funcall truncate-output-fn payload))
        (assert-true truncated-p
                     "Expected truncate-sandbox-output to report truncation.")
        (assert-true (= (length truncated) max-output-size)
                     "Expected truncated output length ~D, got ~D."
                     max-output-size
                     (length truncated)))

      (setf (symbol-value toolset-sym) (funcall make-toolset-fn))
      (clrhash (symbol-value metadata-sym))
      (eval
       `(,deftool-sym i78-sandbox-output-tool
          ()
          "I78 sandbox output guard smoke tool."
          (:permission :auto)
          (:dangerous nil)
          (:category :smoke)
          (:timeout 5)
          (make-string (+ ,max-output-size 64) :initial-element #\a)))

      (let ((old-policy (funcall config-value-fn :sandbox-policy (funcall current-config-fn))))
        (unwind-protect
            (progn
              (funcall setconfig-fn :sandbox-policy :strict)
              (let* ((context (funcall make-context-fn
                                       :toolset (symbol-value toolset-sym)
                                       :permission-mode :full-auto
                                       :initialize-notifications-p nil))
                     (call (funcall make-tool-call-fn
                                    :id "i78-sandbox-strict"
                                    :name "i78-sandbox-output-tool"
                                    :arguments "{}"))
                     (result (funcall execute-tool-fn call context)))
                (assert-true (= (length result) max-output-size)
                             "Expected strict sandbox policy to truncate tool output."))

              (funcall setconfig-fn :sandbox-policy :off)
              (let* ((context (funcall make-context-fn
                                       :toolset (symbol-value toolset-sym)
                                       :permission-mode :full-auto
                                       :initialize-notifications-p nil))
                     (call (funcall make-tool-call-fn
                                    :id "i78-sandbox-off"
                                    :name "i78-sandbox-output-tool"
                                    :arguments "{}"))
                     (result (funcall execute-tool-fn call context)))
                (assert-true (> (length result) max-output-size)
                             "Expected :sandbox-policy :off to skip output truncation.")))
          (funcall setconfig-fn :sandbox-policy old-policy)))))

  (format t "AMOEBUM_SANDBOX_SMOKE_OK~%"))
