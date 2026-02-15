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
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn
           (lambda (name)
             (symbol-function (funcall symbol-in name amoebum-pkg))))
         (make-rule (funcall fn "MAKE-PERMISSION-RULE"))
         (check-permission (funcall fn "CHECK-PERMISSION"))
         (dangerous-command-p (funcall fn "DANGEROUS-COMMAND-P")))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args)))))
      (let* ((rules
               (list
                (funcall make-rule
                         :effect :allow
                         :path "/tmp/project/**"
                         :tool :write-file
                         :source :global)
                (funcall make-rule
                         :effect :deny
                         :path "/tmp/project/.env"
                         :tool :write-file
                         :source :global)
                (funcall make-rule
                         :effect :allow
                         :path "/tmp/project/src/*.lisp"
                         :tool :write-file
                         :source :project)
                (funcall make-rule
                         :effect :deny
                         :path "/tmp/project/src/*.lisp"
                         :tool :write-file
                         :source :global)
                (funcall make-rule
                         :effect :allow
                         :path "/tmp/project/docs/*"
                         :tool :write-file
                         :source :global)
                (funcall make-rule
                         :effect :deny
                         :path "/tmp/project/docs/*"
                         :tool :write-file
                         :source :global))))
        ;; Mode defaults.
        (assert-true
         (eq (funcall check-permission :tool :read-file
                      :path "/tmp/project/src/main.lisp"
                      :permission-mode :supervised)
             :prompt)
         "Expected supervised mode to prompt all operations.")
        (assert-true
         (eq (funcall check-permission :tool :read-file
                      :path "/tmp/project/src/main.lisp"
                      :permission-mode :auto-edit)
             :allow)
         "Expected auto-edit to allow file operations.")
        (assert-true
         (eq (funcall check-permission :tool :bash
                      :command "git status"
                      :permission-mode :auto-edit)
             :prompt)
         "Expected auto-edit to prompt shell commands.")
        (assert-true
         (eq (funcall check-permission :tool :bash
                      :command "git status"
                      :permission-mode :full-auto)
             :allow)
         "Expected full-auto to allow non-destructive operations.")
        (assert-true
         (eq (funcall check-permission :tool :bash
                      :command "rm -rf tmp"
                      :permission-mode :full-auto)
             :prompt)
         "Expected full-auto dangerous command escalation to prompt.")
        (assert-true
         (eq (funcall check-permission :tool :bash
                      :command "rm -rf tmp"
                      :permission-mode :yolo)
             :allow)
         "Expected yolo mode to skip dangerous-operation escalation.")

        ;; Path-level matching and precedence.
        (assert-true
         (eq (funcall check-permission :tool :write-file
                      :path "/tmp/project/.env"
                      :permission-mode :full-auto
                      :rules rules)
             :deny)
         "Expected exact deny rule to override broader allow rule.")
        (assert-true
         (eq (funcall check-permission :tool :write-file
                      :path "/tmp/project/src/main.lisp"
                      :permission-mode :full-auto
                      :rules rules)
             :allow)
         "Expected project-scope allow to beat global-scope deny on tie.")
        (assert-true
         (eq (funcall check-permission :tool :write-file
                      :path "/tmp/project/docs/readme.md"
                      :permission-mode :full-auto
                      :rules rules)
             :deny)
         "Expected deny to win at equal specificity and scope.")

        ;; Escalation can still force prompt when operation is otherwise auto-approved.
        (assert-true
         (eq (funcall check-permission :tool :write-file
                      :path "/tmp/project/src/main.lisp"
                      :permission-mode :full-auto
                      :rules rules
                      :dangerous-p t)
             :prompt)
         "Expected dangerous flag escalation to require explicit approval.")

        ;; Dangerous command pattern library.
        (assert-true
         (funcall dangerous-command-p "git push --force origin main")
         "Expected dangerous command matcher to catch git push --force.")
        (assert-true
         (not (funcall dangerous-command-p "git status"))
         "Expected dangerous command matcher to ignore benign commands."))))

  (format t "AMOEBUM_PERMISSIONS_SMOKE_OK~%"))
