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
         (evaluate-command-permission (funcall fn "EVALUATE-COMMAND-PERMISSION"))
         (check-permission (funcall fn "CHECK-PERMISSION")))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args)))))
      (let ((rules
              (list
               (funcall make-rule :effect :allow :tool :bash :command "docker" :source :project)
               (funcall make-rule :effect :deny :tool :bash :command "docker"
                                 :arguments '("--privileged")
                                 :source :project)
               (funcall make-rule :effect :allow :tool :bash :command "curl" :source :project)
               (funcall make-rule :effect :deny :tool :bash :command "curl"
                                 :arguments '("http://internal-api/*")
                                 :source :project))))
        (assert-true
         (eq (funcall evaluate-command-permission
                      :tool :bash
                      :command "docker run ubuntu"
                      :rules rules)
             :allow)
         "Expected docker command-family rule to allow docker run.")
        (assert-true
         (eq (funcall check-permission
                      :tool :bash
                      :command "docker run --privileged ubuntu"
                      :permission-mode :supervised
                      :rules rules)
             :deny)
         "Expected argument-level deny to override command-family allow.")
        (assert-true
         (eq (funcall check-permission
                      :tool :bash
                      :command "curl http://internal-api/health"
                      :permission-mode :supervised
                      :rules rules)
             :deny)
         "Expected wildcard argument deny shape for internal API URL.")
        (assert-true
         (eq (funcall check-permission
                      :tool :bash
                      :command "curl http://example.com/health"
                      :permission-mode :supervised
                      :rules rules)
             :allow)
         "Expected non-matching curl argument to remain allowed by command family."))))

  (format t "AMOEBUM_PERMISSION_ARGUMENT_GRANULARITY_SMOKE_OK~%"))
