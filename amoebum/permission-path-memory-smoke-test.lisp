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
         (ensure-directory-pathname-fn (funcall fn-in "ENSURE-DIRECTORY-PATHNAME" uiop-pkg))
         (clear-path-approvals-fn (funcall fn-in "CLEAR-PATH-APPROVALS" amoebum-pkg))
         (remember-path-approval-fn (funcall fn-in "REMEMBER-PATH-APPROVAL" amoebum-pkg))
         (forget-path-approval-fn (funcall fn-in "FORGET-PATH-APPROVAL" amoebum-pkg))
         (list-path-approvals-fn (funcall fn-in "LIST-PATH-APPROVALS" amoebum-pkg))
         (check-permission-fn (funcall fn-in "CHECK-PERMISSION" amoebum-pkg))
         (dispatch-slash-command-fn (funcall fn-in "DISPATCH-SLASH-COMMAND" amoebum-pkg))
         (slash-command-result-output-fn (funcall fn-in "SLASH-COMMAND-RESULT-OUTPUT" amoebum-pkg))
         (path-approval-store-path-fn (funcall fn-in "PATH-APPROVAL-STORE-PATH" amoebum-pkg))
         (load-path-approvals-fn (funcall fn-in "LOAD-PATH-APPROVALS" amoebum-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args)))))
      (let* ((tmp-root
               (funcall ensure-directory-pathname-fn
                        (merge-pathnames
                         (make-pathname :directory `(:relative ".tmp-permissions-smokes"
                                                             ,(format nil "amoebum-i131-~A"
                                                                      (get-universal-time))))
                         repo-root)))
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
                       (length entries)))

        ;; Allow-once approvals should auto-consume after one follow-up check.
        (funcall remember-path-approval-fn
                 :tool :write-file
                 :path (namestring path)
                 :scope :once
                 :persist-p nil
                 :project-root tmp-root)
        (assert-true
         (eq (funcall check-permission-fn
                      :tool :write-file
                      :path (namestring path)
                      :permission-mode :supervised
                      :rules nil)
             :allow)
         "Expected first allow-once check to be :allow.")
        (assert-true
         (eq (funcall check-permission-fn
                      :tool :write-file
                      :path (namestring path)
                      :permission-mode :supervised
                      :rules nil)
             :prompt)
         "Expected second allow-once check to fall back to :prompt.")

        ;; Session memory should be visible in /permissions session output.
        (funcall remember-path-approval-fn
                 :tool :read-file
                 :path (namestring path)
                 :scope :session
                 :persist-p nil
                 :project-root tmp-root)
        (multiple-value-bind (handled slash-result)
            (funcall dispatch-slash-command-fn "/permissions session")
          (assert-true handled "Expected /permissions session to be handled.")
          (let ((output (or (funcall slash-command-result-output-fn slash-result) "")))
            (assert-true (search "Session path approvals" output :test #'char-equal)
                         "Expected /permissions session output to mention session approvals.")
            (assert-true (search (namestring path) output :test #'char-equal)
                         "Expected /permissions session output to include remembered path.")))

        ;; "Always" approvals should persist to disk and reload.
        (funcall clear-path-approvals-fn :include-persistent t :project-root tmp-root)
        (funcall remember-path-approval-fn
                 :tool :read-file
                 :path (namestring path)
                 :scope :always
                 :persist-p t
                 :project-root tmp-root)
        (let ((store-path (funcall path-approval-store-path-fn :project-root tmp-root)))
          (assert-true (probe-file store-path)
                       "Expected persisted path-approval store to exist at ~A."
                       store-path))
        (setf (symbol-value (funcall symbol-in "*PATH-APPROVAL-MEMORY*" amoebum-pkg)) '()
              (symbol-value (funcall symbol-in "*PATH-APPROVAL-MEMORY-LOADED-P*" amoebum-pkg)) nil)
        (assert-true (= 1 (funcall load-path-approvals-fn :project-root tmp-root))
                     "Expected load-path-approvals to reload 1 persisted entry."))))

  (format t "AMOEBUM_PERMISSION_PATH_MEMORY_SMOKE_OK~%"))
