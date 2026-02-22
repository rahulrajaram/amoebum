(require :asdf)

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
    (funcall load-asd-fn (merge-pathnames #P"sw4rm-sdk/sw4rm-sdk.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"amoebum/amoebum.asd" repo-root))
    (funcall load-system-fn "amoebum"))

  (let* ((amoebum-pkg (or (find-package "AMOEBUM")
                          (error "Missing package AMOEBUM after load.")))
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
         (clear-path-approvals-fn (funcall fn-in "CLEAR-PATH-APPROVALS" amoebum-pkg))
         (remember-path-approval-fn (funcall fn-in "REMEMBER-PATH-APPROVAL" amoebum-pkg))
         (forget-path-approval-fn (funcall fn-in "FORGET-PATH-APPROVAL" amoebum-pkg))
         (list-path-approvals-fn (funcall fn-in "LIST-PATH-APPROVALS" amoebum-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args)))))
      (let* ((tmp-root
               (funcall ensure-directory-pathname-fn
                        (merge-pathnames
                         (make-pathname :directory `(:relative ,(format nil "amoebum-i131-~A"
                                                                        (get-universal-time))))
                         (funcall temporary-directory-fn))))
             (path (merge-pathnames #P"docs/notes.txt" tmp-root)))
        (funcall clear-path-approvals-fn :include-persistent t :project-root tmp-root)
        (let ((entry
                (funcall remember-path-approval-fn
                         :tool :read-file
                         :path (namestring path)
                         :scope :session
                         :persist-p nil
                         :project-root tmp-root)))
          (assert-true entry "Expected remember-path-approval to return entry."))
        (let ((entries (funcall list-path-approvals-fn)))
          (assert-true (= 1 (length entries))
                       "Expected 1 path approval entry, got ~S." (length entries)))
        (let ((removed
                (funcall forget-path-approval-fn
                         :tool :read-file
                         :path (namestring path)
                         :scope :session
                         :persist-p nil
                         :project-root tmp-root)))
          (assert-true (= 1 removed)
                       "Expected forget-path-approval to remove 1 entry, got ~S."
                       removed))
        (let ((entries (funcall list-path-approvals-fn)))
          (assert-true (= 0 (length entries))
                       "Expected approvals to be cleared, got ~S entries."
                       (length entries))))))

  (format t "AMOEBUM_PERMISSION_PATH_MEMORY_SMOKE_OK~%"))
