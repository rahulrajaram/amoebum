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
         (find-tool-fn (funcall fn-in "FIND-TOOL" pseudopod-pkg))
         (tool-definition-fn-fn (funcall fn-in "TOOL-DEFINITION-FN" pseudopod-pkg))
         (toolset-sym (funcall symbol-in "*TOOLSET*" amoebum-pkg))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (clear-permission-rules-fn (funcall fn-in "CLEAR-PERMISSION-RULES" amoebum-pkg))
         (add-permission-rule-fn (funcall fn-in "ADD-PERMISSION-RULE" amoebum-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (write-text-file (path content)
               (ensure-directories-exist path)
               (with-open-file (stream path
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create
                                       :external-format :utf-8)
                 (write-string content stream)))
             (normalize-path (path)
               (namestring (truename (pathname path))))
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
                         (make-pathname :directory `(:relative ,(format nil "amoebum-i28-~A"
                                                                        (get-universal-time))))
                         (funcall temporary-directory-fn))))
             (glob-old (merge-pathnames #P"src/sub/old.lisp" tmp-root))
             (glob-new (merge-pathnames #P"src/sub/newer/new.lisp" tmp-root))
             (glob-ignore (merge-pathnames #P"src/sub/ignore.txt" tmp-root))
             (context-file (merge-pathnames #P"ctx/context.txt" tmp-root))
             (grep-old (merge-pathnames #P"logs/old.txt" tmp-root))
             (grep-new (merge-pathnames #P"logs/new.txt" tmp-root))
             (blocked (merge-pathnames #P"blocked/deny.txt" tmp-root)))
        (write-text-file glob-old "(defun old ())~%")
        (sleep 1)
        (write-text-file glob-new "(defun new ())~%")
        (write-text-file glob-ignore "skip~%")
        (write-text-file context-file (format nil "alpha~%target~%omega~%"))
        (write-text-file grep-old "needle old~%")
        (sleep 1)
        (write-text-file grep-new "needle new~%")
        (write-text-file blocked "blocked needle~%")

        (let* ((glob-result (invoke-tool "glob-files"
                                         "pattern" "src/**/*.lisp"
                                         "root" (namestring tmp-root)
                                         "limit" 10))
               (glob-matches (getf glob-result :matches)))
          (assert-true (= (length glob-matches) 2)
                       "Expected glob-files to return only matching .lisp files.")
          (assert-true (string= (normalize-path (getf (first glob-matches) :path))
                                (normalize-path glob-new))
                       "Expected glob-files results sorted by modification time descending.")
          (assert-true (string= (normalize-path (getf (second glob-matches) :path))
                                (normalize-path glob-old))
                       "Expected glob-files second result to be older match."))

        (let* ((grep-result (invoke-tool "grep-content"
                                         "pattern" "target"
                                         "path-glob" "ctx/*.txt"
                                         "root" (namestring tmp-root)
                                         "before" 1
                                         "after" 1
                                         "limit" 10))
               (grep-matches (getf grep-result :matches))
               (first-match (first grep-matches)))
          (assert-true (= (length grep-matches) 1)
                       "Expected grep-content to find one target match in context file.")
          (assert-true (= (getf first-match :line) 2)
                       "Expected grep-content to return the matching line number.")
          (assert-true (string= (getf first-match :text) "target")
                       "Expected grep-content to return the matching line text.")
          (assert-true (string= (getf first-match :matched-text) "target")
                       "Expected grep-content to expose matched-text from PTUI search widget output.")
          (assert-true (equal (getf (first (getf first-match :context-before)) :line) 1)
                       "Expected grep-content to include context before the match.")
          (assert-true (equal (getf (first (getf first-match :context-after)) :line) 3)
                       "Expected grep-content to include context after the match."))

        (let* ((grep-order-result (invoke-tool "grep-content"
                                               "pattern" "needle"
                                               "path-glob" "logs/*.txt"
                                               "root" (namestring tmp-root)
                                               "before" 0
                                               "after" 0
                                               "limit" 10))
               (order-matches (getf grep-order-result :matches)))
          (assert-true (= (length order-matches) 2)
                       "Expected grep-content to find both log matches.")
          (assert-true (string= (normalize-path (getf (first order-matches) :path))
                                (normalize-path grep-new))
                       "Expected grep-content results sorted by file modification time."))

        (let* ((grep-ci-result (invoke-tool "grep-content"
                                            "pattern" "NEEDLE"
                                            "path-glob" "logs/*.txt"
                                            "root" (namestring tmp-root)
                                            "before" 0
                                            "after" 0
                                            "limit" 10
                                            "case-insensitive" t))
               (ci-matches (getf grep-ci-result :matches)))
          (assert-true (= (length ci-matches) 2)
                       "Expected case-insensitive grep-content to match uppercase query."))

        (funcall clear-permission-rules-fn)
        (funcall add-permission-rule-fn
                 :effect :deny
                 :tool :glob-files
                 :path (namestring blocked)
                 :source :project)
        (let ((saw-deny nil))
          (handler-case
              (invoke-tool "glob-files"
                           "pattern" "blocked/*.txt"
                           "root" (namestring tmp-root)
                           "limit" 10)
            (error ()
              (setf saw-deny t)))
          (assert-true saw-deny
                       "Expected deny rule to block glob-files on forbidden paths.")))))

  (format t "AMOEBUM_SEARCH_TOOLS_SMOKE_OK~%"))
