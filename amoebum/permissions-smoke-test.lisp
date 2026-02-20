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
         (normalize-permission-path (funcall fn "NORMALIZE-PERMISSION-PATH"))
         (dangerous-command-p (funcall fn "DANGEROUS-COMMAND-P"))
         (setconfig-fn (funcall fn "SETCONFIG"))
         (config-value-fn (funcall fn "CONFIG-VALUE"))
         (current-config-fn (funcall fn "CURRENT-CONFIG")))
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
        (assert-true
         (string=
          (funcall normalize-permission-path "C:\\Work\\repo\\src\\..\\src\\main.lisp")
          "c:/Work/repo/src/main.lisp")
         "Expected canonical normalization to collapse windows-style separators and dot segments.")
        (assert-true
         (eq (funcall check-permission :tool :write-file
                      :path "C:\\tmp\\project\\src\\..\\src\\main.lisp"
                      :permission-mode :full-auto
                      :rules (list (funcall make-rule
                                            :effect :deny
                                            :path "c:/tmp/project/src/main.lisp"
                                            :tool :write-file
                                            :source :project)))
             :deny)
         "Expected permission decision to canonicalize platform-style equivalent paths before rule matching.")

        ;; Specificity ordering: exact > glob > directory > wildcard.
        (let ((specificity-rules
                (list
                 (funcall make-rule
                          :effect :allow
                          :path "**/*"
                          :tool :write-file
                          :source :global)
                 (funcall make-rule
                          :effect :deny
                          :path "/tmp/project/src/"
                          :tool :write-file
                          :source :global)
                 (funcall make-rule
                          :effect :allow
                          :path "/tmp/project/src/**/*.lisp"
                          :tool :write-file
                          :source :global)
                 (funcall make-rule
                          :effect :deny
                          :path "/tmp/project/src/core/blocked.lisp"
                          :tool :write-file
                          :source :global))))
          (assert-true
           (eq (funcall check-permission :tool :write-file
                        :path "/tmp/project/src/main.lisp"
                        :permission-mode :full-auto
                        :rules specificity-rules)
               :allow)
           "Expected ** glob allow to match direct child path and beat directory deny.")
          (assert-true
           (eq (funcall check-permission :tool :write-file
                        :path "/tmp/project/src/core/helper.lisp"
                        :permission-mode :full-auto
                        :rules specificity-rules)
               :allow)
           "Expected glob allow to beat directory deny for nested .lisp paths.")
          (assert-true
           (eq (funcall check-permission :tool :write-file
                        :path "/tmp/project/src/core/readme.md"
                        :permission-mode :full-auto
                        :rules specificity-rules)
               :deny)
           "Expected directory deny to beat wildcard allow for non-glob matches.")
          (assert-true
           (eq (funcall check-permission :tool :write-file
                        :path "/tmp/project/docs/readme.md"
                        :permission-mode :full-auto
                        :rules specificity-rules)
               :allow)
           "Expected wildcard allow outside denied directory subtree.")
          (assert-true
           (eq (funcall check-permission :tool :write-file
                        :path "/tmp/project/src/core/blocked.lisp"
                        :permission-mode :full-auto
                        :rules specificity-rules)
               :deny)
           "Expected exact deny to beat broader glob allow."))

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
         (funcall dangerous-command-p "git reset --hard HEAD~1")
         "Expected dangerous command matcher to catch git reset --hard.")
        (assert-true
         (funcall dangerous-command-p "git checkout -- .")
         "Expected dangerous command matcher to catch git checkout . destructive reset.")
        (assert-true
         (funcall dangerous-command-p "git clean -fd")
         "Expected dangerous command matcher to catch git clean -f variants.")
        (assert-true
         (eq (funcall check-permission :tool :bash
                      :command "git checkout -- ."
                      :permission-mode :full-auto)
             :prompt)
         "Expected full-auto git checkout . to escalate to prompt.")
        (assert-true
         (eq (funcall check-permission :tool :bash
                      :command "git clean -fd"
                      :permission-mode :full-auto)
             :prompt)
         "Expected full-auto git clean -f to escalate to prompt.")
        (assert-true
         (not (funcall dangerous-command-p "git status"))
         "Expected dangerous command matcher to ignore benign commands.")

        ;; MCP tool defaults and per-server overrides.
        (let ((old-mcp-permissions
                (funcall config-value-fn
                         :mcp-server-permissions
                         (funcall current-config-fn))))
          (unwind-protect
              (progn
                (funcall setconfig-fn :mcp-server-permissions nil)
                (assert-true
                 (eq (funcall check-permission :tool "mcp/github/echo"
                              :permission-mode :full-auto)
                     :prompt)
                 "Expected MCP tools to default to prompt decision in full-auto mode.")
                (funcall setconfig-fn
                         :mcp-server-permissions
                         (list (cons "github" :allow)
                               (cons "database" :deny)))
                (assert-true
                 (eq (funcall check-permission :tool "mcp/github/echo"
                              :permission-mode :full-auto)
                     :allow)
                 "Expected per-server allow override for MCP tool namespace.")
                (assert-true
                 (eq (funcall check-permission :tool "mcp/database/query"
                              :permission-mode :full-auto)
                     :deny)
                 "Expected per-server deny override for MCP tool namespace."))
            (funcall setconfig-fn :mcp-server-permissions old-mcp-permissions))))))

  (format t "AMOEBUM_PERMISSIONS_SMOKE_OK~%"))
