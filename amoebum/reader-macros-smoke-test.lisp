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
         (uiop-pkg (or (find-package "UIOP")
                       (find-package "ASDF/UTILITY")
                       (error "Missing UIOP package after requiring ASDF.")))
         (named-readtables-pkg (or (find-package "NAMED-READTABLES")
                                   (error "Missing NAMED-READTABLES package after load.")))
         (cl-ppcre-pkg (or (find-package "CL-PPCRE")
                           (error "Missing CL-PPCRE package after load.")))
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn-in
           (lambda (name package)
             (symbol-function (funcall symbol-in name package))))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (activate-readtable-fn (funcall fn-in "ACTIVATE-AMOEBUM-READTABLE" amoebum-pkg))
         (find-readtable-fn (funcall fn-in "FIND-READTABLE" named-readtables-pkg))
         (malformed-regex-class (funcall symbol-in "MALFORMED-REGEX" amoebum-pkg))
         (scan-fn (funcall fn-in "SCAN" cl-ppcre-pkg))
         (temporary-directory-fn (funcall fn-in "TEMPORARY-DIRECTORY" uiop-pkg))
         (ensure-directory-pathname-fn (funcall fn-in "ENSURE-DIRECTORY-PATHNAME" uiop-pkg)))
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
             (normalized-path (path)
               (namestring (truename (pathname path)))))
      (let* ((tmp-root
               (funcall ensure-directory-pathname-fn
                        (merge-pathnames
                         (make-pathname :directory `(:relative ,(format nil "amoebum-i76-~A"
                                                                        (get-universal-time))))
                         (funcall temporary-directory-fn))))
             (main-file (merge-pathnames #P"src/main.lisp" tmp-root))
             (util-file (merge-pathnames #P"src/lib/util.lisp" tmp-root))
             (ignore-file (merge-pathnames #P"src/lib/skip.txt" tmp-root))
             (amoebum-readtable (funcall find-readtable-fn :amoebum-readtable)))
        (write-text-file main-file "(defun main () :ok)")
        (write-text-file util-file "(defun util () :ok)")
        (write-text-file ignore-file "ignore")

        (funcall setconfig-fn :project-root tmp-root)
        (funcall activate-readtable-fn)

        (let ((*package* amoebum-pkg)
              (*readtable* amoebum-readtable))
          (let* ((resolved (read-from-string "#p\"src/main.lisp\""))
                 (expected (merge-pathnames #P"src/main.lisp" tmp-root)))
            (assert-true (pathnamep resolved)
                         "Expected #p to produce a pathname, got ~S." resolved)
            (assert-true (string= (normalized-path resolved)
                                  (normalized-path expected))
                         "Expected #p to resolve relative to project root. Expected ~A got ~A."
                         (normalized-path expected)
                         (normalized-path resolved)))

          (let* ((globbed (read-from-string "#g\"src/**/*.lisp\""))
                 (paths (mapcar #'normalized-path globbed)))
            (assert-true (= (length globbed) 2)
                         "Expected #g to return two .lisp matches, got ~D (~S)."
                         (length globbed)
                         paths)
            (assert-true (every #'pathnamep globbed)
                         "Expected #g to return only pathnames, got ~S."
                         globbed)
            (assert-true (member (normalized-path main-file) paths :test #'string=)
                         "Expected #g results to include src/main.lisp, got ~S."
                         paths)
            (assert-true (member (normalized-path util-file) paths :test #'string=)
                         "Expected #g results to include src/lib/util.lisp, got ~S."
                         paths))

          (let ((scanner (read-from-string "#r\"foo.*bar\"")))
            (assert-true (funcall scan-fn scanner "foo works bar")
                         "Expected #r scanner to match sample text."))

          (let ((signaled-malformed-regex nil))
            (handler-case
                (read-from-string "#r\"[unterminated\"")
              (condition (condition)
                (setf signaled-malformed-regex
                      (typep condition malformed-regex-class))))
            (assert-true signaled-malformed-regex
                         "Expected malformed regex input to signal MALFORMED-REGEX."))))))

  (format t "AMOEBUM_READER_MACROS_SMOKE_OK~%"))
