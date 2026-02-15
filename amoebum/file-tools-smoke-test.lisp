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
         (temporary-directory-fn (funcall fn-in "TEMPORARY-DIRECTORY" uiop-pkg))
         (ensure-directory-pathname-fn (funcall fn-in "ENSURE-DIRECTORY-PATHNAME" uiop-pkg))
         (read-file-string-fn (funcall fn-in "READ-FILE-STRING" uiop-pkg))
         (find-tool-fn (funcall fn-in "FIND-TOOL" pseudopod-pkg))
         (tool-definition-fn-fn (funcall fn-in "TOOL-DEFINITION-FN" pseudopod-pkg))
         (toolset-sym (funcall symbol-in "*TOOLSET*" amoebum-pkg))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (clear-permission-rules-fn (funcall fn-in "CLEAR-PERMISSION-RULES" amoebum-pkg))
         (add-permission-rule-fn (funcall fn-in "ADD-PERMISSION-RULE" amoebum-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (contains-substring-p (needle haystack)
               (not (null (search needle haystack :test #'char=))))
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
                          (apply #'make-args key-values)))))
      (funcall setconfig-fn :permission-mode :full-auto)
      (funcall clear-permission-rules-fn)

      (let* ((tmp-root
               (funcall ensure-directory-pathname-fn
                        (merge-pathnames
                         (make-pathname :directory `(:relative ,(format nil "amoebum-i27-~A"
                                                                        (get-universal-time))))
                         (funcall temporary-directory-fn))))
             (read-source (merge-pathnames #P"fixtures/read-source.txt" tmp-root))
             (write-target (merge-pathnames #P"out/write-target.txt" tmp-root))
             (edit-target (merge-pathnames #P"out/edit-target.txt" tmp-root))
             (deny-target (merge-pathnames #P"blocked/deny.txt" tmp-root)))
        (ensure-directories-exist read-source)
        (with-open-file (stream read-source
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :external-format :utf-8)
          (write-line "alpha" stream)
          (write-line "beta" stream)
          (write-line "gamma" stream)
          (write-line "delta" stream))

        (let ((read-result (invoke-tool "read-file"
                                        "path" (namestring read-source)
                                        "offset" 1
                                        "limit" 2)))
          (assert-true (contains-substring-p (format nil "2~Cbeta" #\Tab) read-result)
                       "Expected read-file output to include line 2 with content.")
          (assert-true (contains-substring-p (format nil "3~Cgamma" #\Tab) read-result)
                       "Expected read-file output to include line 3 with content.")
          (assert-true (not (contains-substring-p (format nil "1~Calpha" #\Tab) read-result))
                       "Expected read-file offset to skip earlier lines."))

        (invoke-tool "write-file"
                     "path" (namestring write-target)
                     "content" (format nil "one~%two~%"))
        (assert-true (probe-file write-target)
                     "Expected write-file to create target file.")
        (assert-true (string= (funcall read-file-string-fn write-target :external-format :utf-8)
                              (format nil "one~%two~%"))
                     "Expected write-file to persist provided content.")

        (invoke-tool "write-file"
                     "path" (namestring edit-target)
                     "content" (format nil "one~%two~%two~%"))
        (invoke-tool "edit-file"
                     "path" (namestring edit-target)
                     "old-string" "two"
                     "new-string" "TWO")
        (let ((edited (funcall read-file-string-fn edit-target :external-format :utf-8)))
          (assert-true (contains-substring-p "TWO" edited)
                       "Expected edit-file to apply replacement.")
          (assert-true (not (contains-substring-p "two" edited))
                       "Expected edit-file to replace all exact matches."))

        (funcall clear-permission-rules-fn)
        (funcall add-permission-rule-fn
                 :effect :deny
                 :tool :write-file
                 :path (namestring deny-target)
                 :source :project)
        (let ((saw-deny nil))
          (handler-case
              (invoke-tool "write-file"
                           "path" (namestring deny-target)
                           "content" "blocked")
            (error ()
              (setf saw-deny t)))
          (assert-true saw-deny
                       "Expected path deny rule to block write-file execution.")))))

  (format t "AMOEBUM_FILE_TOOLS_SMOKE_OK~%"))
