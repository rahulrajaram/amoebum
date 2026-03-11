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
         (check-permission (funcall fn "CHECK-PERMISSION"))
         (evaluate-command-permission (funcall fn "EVALUATE-COMMAND-PERMISSION")))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args)))))
      (let ((rules (list (funcall make-rule
                                :effect :allow
                                :tool :bash
                                :command "git *"
                                :source :global)
                         (funcall make-rule
                                  :effect :deny
                                  :tool :bash
                                  :command "git push --force"
                                  :source :project)
                         (funcall make-rule
                                  :effect :deny
                                  :tool :bash
                                  :command "re:rm\\s+-rf\\s+.*"
                                  :source :project))))
        (assert-true
         (eq (funcall evaluate-command-permission
                      :tool :bash
                      :command "git status"
                      :rules rules)
             :allow)
         "Expected prefix allow rule to match git status.")
        (assert-true
         (eq (funcall evaluate-command-permission
                      :tool :bash
                      :command "echo prep | git status"
                      :rules rules)
             :allow)
         "Expected prefix allow rule to match git status pipeline segment.")
        (assert-true
         (eq (funcall evaluate-command-permission
                      :tool :bash
                      :command "echo prep | git push --force"
                      :rules rules)
             :deny)
         "Expected exact deny to match git pipeline segment.")
        (assert-true
         (eq (funcall evaluate-command-permission
                      :tool :bash
                      :command "git push --force"
                      :rules rules)
             :deny)
         "Expected exact deny to override git prefix allow.")
        (assert-true
         (eq (funcall check-permission
                      :tool :bash
                      :command "cat data.txt | rm -rf /tmp/demo"
                      :permission-mode :full-auto
                      :rules rules)
             :deny)
         "Expected regex deny rule to match destructive pipeline segment.")
        (assert-true
         (eq (funcall check-permission
                      :tool :bash
                      :command "git diff"
                      :permission-mode :supervised
                      :rules rules)
             :allow)
         "Expected explicit command allow to override supervised default prompt.")
        (assert-true
         (eq (funcall check-permission
                      :tool :bash
                      :command "rm -rf /tmp/demo"
                      :permission-mode :full-auto
                      :rules (list (funcall make-rule
                                            :effect :allow
                                            :tool :bash
                                            :command "rm -rf *"
                                            :source :project)))
             :prompt)
         "Expected dangerous escalation to prompt despite allow rule in full-auto.")
        (assert-true
         (eq (funcall evaluate-command-permission
                      :tool :bash
                      :command "echo prep | git push --force"
                      :rules (list (funcall make-rule
                                            :effect :allow
                                            :tool :bash
                                            :command "git push --force"
                                            :source :project)
                                   (funcall make-rule
                                            :effect :deny
                                            :tool :bash
                                            :command "git push --force"
                                            :source :project)))
             :deny)
         "Expected deterministic deny precedence for conflicting exact command rules."))))

  (format t "AMOEBUM_PERMISSION_COMMAND_SMOKE_OK~%"))
