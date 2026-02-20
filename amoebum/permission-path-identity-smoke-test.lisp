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
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn
           (lambda (name)
             (symbol-function (funcall symbol-in name amoebum-pkg))))
         (make-rule (funcall fn "MAKE-PERMISSION-RULE"))
         (check-permission (funcall fn "CHECK-PERMISSION")))
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
             (separator-variant (path)
               (let* ((mixed (substitute #\\ #\/ path))
                      (pivot (position #\\ mixed :start 1)))
                 (if pivot
                     (concatenate 'string
                                  (subseq mixed 0 pivot)
                                  "\\\\"
                                  (subseq mixed pivot))
                     mixed))))
      (let* ((tmp-root
               (uiop:ensure-directory-pathname
                (merge-pathnames
                 (make-pathname :directory `(:relative ,(format nil "amoebum-i130-~A"
                                                                (get-universal-time))))
                 (uiop:ensure-directory-pathname (uiop:temporary-directory)))))
             (secret-file (merge-pathnames #P"real/secret.txt" tmp-root))
             (public-file (merge-pathnames #P"real/public.txt" tmp-root))
             (symlink-path (merge-pathnames #P"links/secret-link.txt" tmp-root)))
        (unwind-protect
            (progn
              (write-text-file secret-file "sensitive")
              (write-text-file public-file "public")
              (ensure-directories-exist symlink-path)
              (multiple-value-bind (stdout stderr exit-code)
                  (uiop:run-program (list "ln"
                                          "-sfn"
                                          (namestring (truename secret-file))
                                          (namestring symlink-path))
                                    :ignore-error-status t
                                    :output :string
                                    :error-output :string)
                (declare (ignore stdout))
                (unless (zerop (or exit-code 1))
                  (error "Unable to create symlink for I130 smoke: ~A" stderr)))
              (let* ((secret-canonical (namestring (truename secret-file)))
                     (public-canonical (namestring (truename public-file)))
                     (workspace-glob (format nil "~A**" (namestring tmp-root)))
                     (deny-rules
                       (list
                        (funcall make-rule
                                 :effect :allow
                                 :path workspace-glob
                                 :tool :write-file
                                 :source :global)
                        (funcall make-rule
                                 :effect :deny
                                 :path secret-canonical
                                 :tool :write-file
                                 :source :project))))
                ;; False-allow prevention: equivalent identity variants of secret path
                ;; must still hit the exact deny rule.
                (assert-true
                 (eq (funcall check-permission
                              :tool :write-file
                              :path secret-canonical
                              :permission-mode :full-auto
                              :rules deny-rules)
                     :deny)
                 "Expected canonical secret path to be denied.")
                (assert-true
                 (eq (funcall check-permission
                              :tool :write-file
                              :path (namestring symlink-path)
                              :permission-mode :full-auto
                              :rules deny-rules)
                     :deny)
                 "Expected symlink path to resolve to denied target identity.")
                (assert-true
                 (eq (funcall check-permission
                              :tool :write-file
                              :path (separator-variant secret-canonical)
                              :permission-mode :full-auto
                              :rules deny-rules)
                     :deny)
                 "Expected mixed-separator variant of secret path to be denied.")
                (uiop:with-current-directory (tmp-root)
                  (assert-true
                   (eq (funcall check-permission
                                :tool :write-file
                                :path "real/./../real/secret.txt"
                                :permission-mode :full-auto
                                :rules deny-rules)
                       :deny)
                   "Expected relative-dot variant of secret path to be denied."))
                ;; False-deny prevention: equivalent identity variants of allow path
                ;; must still match exact allow when mode default would otherwise prompt.
                (let ((allow-rules
                        (list
                         (funcall make-rule
                                  :effect :allow
                                  :path public-canonical
                                  :tool :write-file
                                  :source :project))))
                  (assert-true
                   (eq (funcall check-permission
                                :tool :write-file
                                :path public-canonical
                                :permission-mode :supervised
                                :rules allow-rules)
                       :allow)
                   "Expected canonical public path to be allowed.")
                  (assert-true
                   (eq (funcall check-permission
                                :tool :write-file
                                :path (separator-variant public-canonical)
                                :permission-mode :supervised
                                :rules allow-rules)
                       :allow)
                   "Expected mixed-separator public path to match exact allow.")
                  (uiop:with-current-directory (tmp-root)
                    (assert-true
                     (eq (funcall check-permission
                                  :tool :write-file
                                  :path "real/./public.txt"
                                  :permission-mode :supervised
                                  :rules allow-rules)
                         :allow)
                     "Expected relative public path to match exact allow.")))))
          (ignore-errors
            (uiop:delete-directory-tree tmp-root :validate t :if-does-not-exist :ignore))))))

  (format t "AMOEBUM_PERMISSION_PATH_IDENTITY_SMOKE_OK~%"))
