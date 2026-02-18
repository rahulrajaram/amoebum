#.(progn (require :asdf) nil)

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
         (clear-permission-rules-fn (funcall fn-in "CLEAR-PERMISSION-RULES" amoebum-pkg)))
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
             (string-suffix-p (suffix value)
               (let ((suffix-length (length suffix))
                     (value-length (length value)))
                 (and (<= suffix-length value-length)
                      (string= suffix value
                               :start1 0
                               :end1 suffix-length
                               :start2 (- value-length suffix-length)
                               :end2 value-length))))
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
                         (make-pathname :directory `(:relative ,(format nil "amoebum-i122-~A"
                                                                        (get-universal-time))))
                         (funcall temporary-directory-fn))))
             (old-content (merge-pathnames #P"src/old-content.lisp" tmp-root))
             (path-only (merge-pathnames #P"docs/needle-guide.md" tmp-root))
             (new-content (merge-pathnames #P"src/new-content.lisp" tmp-root))
             (noise (merge-pathnames #P"docs/noise.txt" tmp-root)))
        (write-text-file old-content (format nil "alpha~%needle old~%omega~%"))
        (sleep 1)
        (write-text-file path-only (format nil "documentation only~%"))
        (sleep 1)
        (write-text-file new-content (format nil "one~%needle new~%two~%"))
        (write-text-file noise (format nil "no match here~%"))

        (let* ((result-a (invoke-tool "search-project"
                                      "query" "needle"
                                      "root" (namestring tmp-root)
                                      "path-glob" "**/*"
                                      "before" 1
                                      "after" 1
                                      "limit" 20))
              (result-b (invoke-tool "search-project"
                                      "query" "needle"
                                      "root" (namestring tmp-root)
                                      "path-glob" "**/*"
                                      "before" 1
                                      "after" 1
                                      "limit" 20))
               (results (getf result-a :results))
               (backend-counts (getf result-a :backend-counts)))
          (assert-true (equal results (getf result-b :results))
                       "Expected deterministic merged output across repeated runs.")
          (assert-true (= (getf result-a :count) 3)
                       "Expected three merged hits (2 content + 1 file), got ~S."
                       (getf result-a :count))
          (assert-true (= (or (getf backend-counts :content) -1) 2)
                       "Expected content backend count to be 2, got ~S in ~S."
                       backend-counts result-a)
          (assert-true (= (or (getf backend-counts :files) -1) 1)
                       "Expected file backend count to be 1, got ~S in ~S."
                       backend-counts result-a)
          (let ((first (first results))
                (second (second results))
                (third (third results)))
            (assert-true (eq (getf first :backend) :content)
                         "Expected newest hit to be from content backend.")
            (assert-true (eq (getf first :kind) :content)
                         "Expected first result kind to be :content.")
            (assert-true (string= (normalize-path (getf first :path))
                                  (normalize-path new-content))
                         "Expected newest content file first in merged ordering.")
            (assert-true (string= (getf first :matched-text) "needle")
                         "Expected shaped content result to include :matched-text.")
            (assert-true (eq (getf second :kind) :file)
                         "Expected path-only match to be emitted as :file hit.")
            (assert-true (string= (normalize-path (getf second :path))
                                  (normalize-path path-only))
                         "Expected middle hit to be path-only docs match.")
            (assert-true (eq (getf third :backend) :content)
                         "Expected oldest hit to be content backend.")
            (assert-true (string= (normalize-path (getf third :path))
                                  (normalize-path old-content))
                         "Expected oldest content file to sort last.")))

        (let* ((extension-filtered (invoke-tool "search-project"
                                                "query" "needle"
                                                "root" (namestring tmp-root)
                                                "path-glob" "**/*"
                                                "extensions" "lisp"
                                                "limit" 20))
               (filtered-results (getf extension-filtered :results)))
          (assert-true (= (getf extension-filtered :count) 2)
                       "Expected extension filter to keep only .lisp results.")
          (assert-true (every (lambda (entry)
                                (string-suffix-p ".lisp" (getf entry :path)))
                              filtered-results)
                       "Expected all extension-filtered paths to end in .lisp."))

        (let ((file-only (invoke-tool "search-project"
                                      "query" "needle"
                                      "root" (namestring tmp-root)
                                      "path-glob" "**/*"
                                      "include-content" nil
                                      "include-files" t
                                      "limit" 20)))
          (assert-true (= (getf file-only :count) 1)
                       "Expected file-only backend mode to emit one path hit.")
          (assert-true (eq (getf (first (getf file-only :results)) :kind) :file)
                       "Expected file-only mode result kind to be :file."))

        (let ((content-only-ci (invoke-tool "search-project"
                                            "query" "NEEDLE"
                                            "root" (namestring tmp-root)
                                            "path-glob" "**/*"
                                            "include-content" t
                                            "include-files" nil
                                            "case-insensitive" t
                                            "limit" 20)))
          (assert-true (= (getf content-only-ci :count) 2)
                       "Expected case-insensitive content backend search to match both files.")))))

  (format t "AMOEBUM_SEARCH_ORCHESTRATION_SMOKE_OK~%"))
