(in-package :amoebum/test)

;;; ============================================================
;;; I237: Multi-layer Configuration Loading and Merging
;;; ============================================================

(def-suite config-loader-suite :in amoebum-suite)
(in-suite config-loader-suite)

;;; --- Base config with defaults ---

(test base-config-has-defaults
  "Base config should have all default keys populated."
  (let ((cfg (amoebum.config:load-config :project-root "/tmp/"
                                    :global-config-path "/nonexistent/global.lisp"
                                    :project-config-path "/nonexistent/project.lisp"
                                    :environment-values nil
                                    :cli-values nil)))
    (is (amoebum.config:config-p cfg))
    (is (stringp (amoebum.config:config-value :model cfg)))
    (is (keywordp (amoebum.config:config-value :permission-mode cfg)))
    (is (keywordp (amoebum.config:config-value :memory-backend cfg)))
    (is (pathnamep (amoebum.config:config-project-root cfg)))))

(test base-config-default-sources
  "All default values should come from :built-in source."
  (let ((cfg (amoebum.config:load-config :project-root "/tmp/"
                                    :global-config-path "/nonexistent/global.lisp"
                                    :project-config-path "/nonexistent/project.lisp"
                                    :environment-values nil
                                    :cli-values nil)))
    (is (eq :built-in (amoebum.config:config-layer-source :model cfg)))
    (is (eq :built-in (amoebum.config:config-layer-source :permission-mode cfg)))
    (is (eq :built-in (amoebum.config:config-layer-source :memory-backend cfg)))))

;;; --- Layer priority ---

(test global-layer-overrides-defaults
  "Global config layer should override built-in defaults."
  (let* ((tmp-dir (%make-temp-directory "amoebum-config-loader"))
         (global-path (merge-pathnames #P"global-config.lisp" tmp-dir)))
    (unwind-protect
         (progn
           (%write-text-file global-path
                             "(configure :model \"global-model\")")
           (let ((cfg (amoebum.config:load-config
                       :project-root "/tmp/"
                       :global-config-path global-path
                       :project-config-path "/nonexistent/project.lisp"
                       :environment-values nil
                       :cli-values nil)))
             (is (string= "global-model" (amoebum.config:config-value :model cfg)))
             (is (eq :global (amoebum.config:config-layer-source :model cfg)))))
      (%delete-directory-tree-safe tmp-dir))))

(test project-layer-overrides-global
  "Project config layer should override global layer."
  (let* ((tmp-dir (%make-temp-directory "amoebum-config-loader"))
         (global-path (merge-pathnames #P"global-config.lisp" tmp-dir))
         (project-path (merge-pathnames #P"project-config.lisp" tmp-dir)))
    (unwind-protect
         (progn
           (%write-text-file global-path
                             "(configure :model \"global-model\")")
           (%write-text-file project-path
                             "(configure :model \"project-model\")")
           (let ((cfg (amoebum.config:load-config
                       :project-root "/tmp/"
                       :global-config-path global-path
                       :project-config-path project-path
                       :environment-values nil
                       :cli-values nil)))
             (is (string= "project-model" (amoebum.config:config-value :model cfg)))
             (is (eq :project (amoebum.config:config-layer-source :model cfg)))))
      (%delete-directory-tree-safe tmp-dir))))

(test env-layer-overrides-project
  "Environment layer should override project layer."
  (let* ((tmp-dir (%make-temp-directory "amoebum-config-loader"))
         (project-path (merge-pathnames #P"project-config.lisp" tmp-dir))
         (env-values (make-hash-table :test 'eq)))
    (setf (gethash :model env-values) "env-model")
    (unwind-protect
         (progn
           (%write-text-file project-path
                             "(configure :model \"project-model\")")
           (let ((cfg (amoebum.config:load-config
                       :project-root "/tmp/"
                       :global-config-path "/nonexistent/global.lisp"
                       :project-config-path project-path
                       :environment-values env-values
                       :cli-values nil)))
             (is (string= "env-model" (amoebum.config:config-value :model cfg)))
             (is (eq :env (amoebum.config:config-layer-source :model cfg)))))
      (%delete-directory-tree-safe tmp-dir))))

(test directory-layer-overrides-project
  "Directory config layer should override project and global before env/cli."
  (let* ((tmp-dir (%make-temp-directory "amoebum-config-loader"))
         (project-root (merge-pathnames #P"repo/" tmp-dir))
         (directory-root (merge-pathnames #P"repo/nested/work/" tmp-dir))
         (global-path (merge-pathnames #P"global-config.lisp" tmp-dir))
         (project-path (merge-pathnames #P"repo/.amoebum/config.lisp" tmp-dir))
         (directory-path (merge-pathnames #P"repo/nested/.amoebum/config.lisp" tmp-dir)))
    (unwind-protect
         (progn
           (ensure-directories-exist project-path)
           (ensure-directories-exist directory-path)
           (ensure-directories-exist (merge-pathnames #P".keep" directory-root))
           (%write-text-file global-path
                             "(configure :model \"global-model\")")
           (%write-text-file project-path
                             "(configure :model \"project-model\")")
           (%write-text-file directory-path
                             "(configure :model \"directory-model\")")
           (let ((cfg (amoebum.config:load-config
                       :project-root project-root
                       :directory-root directory-root
                       :global-config-path global-path
                       :project-config-path project-path
                       :environment-values nil
                       :cli-values nil)))
             (is (string= "directory-model" (amoebum.config:config-value :model cfg)))
             (is (eq :directory (amoebum.config:config-layer-source :model cfg)))))
      (%delete-directory-tree-safe tmp-dir))))

(test cli-layer-overrides-all
  "CLI layer should have highest priority."
  (let* ((tmp-dir (%make-temp-directory "amoebum-config-loader"))
         (global-path (merge-pathnames #P"global-config.lisp" tmp-dir))
         (project-path (merge-pathnames #P"project-config.lisp" tmp-dir))
         (env-values (make-hash-table :test 'eq))
         (cli-values (make-hash-table :test 'eq)))
    (setf (gethash :model env-values) "env-model")
    (setf (gethash :model cli-values) "cli-model")
    (unwind-protect
         (progn
           (%write-text-file global-path "(configure :model \"global-model\")")
           (%write-text-file project-path "(configure :model \"project-model\")")
           (let ((cfg (amoebum.config:load-config
                       :project-root "/tmp/"
                       :global-config-path global-path
                       :project-config-path project-path
                       :environment-values env-values
                       :cli-values cli-values)))
             (is (string= "cli-model" (amoebum.config:config-value :model cfg)))
             (is (eq :cli (amoebum.config:config-layer-source :model cfg)))))
      (%delete-directory-tree-safe tmp-dir))))

(test list-merge-directives-append-and-prepend
  "List-valued keys should support :append and :prepend merge directives."
  (let* ((tmp-dir (%make-temp-directory "amoebum-config-loader"))
         (project-root (merge-pathnames #P"repo/" tmp-dir))
         (directory-root (merge-pathnames #P"repo/nested/work/" tmp-dir))
         (global-path (merge-pathnames #P"global-config.lisp" tmp-dir))
         (project-path (merge-pathnames #P"repo/.amoebum/config.lisp" tmp-dir))
         (directory-path (merge-pathnames #P"repo/nested/.amoebum/config.lisp" tmp-dir))
         (env-values (make-hash-table :test 'eq))
         (cli-values (make-hash-table :test 'eq)))
    (setf (gethash :web-search-allow-domains env-values)
          '(:append ("env.example")))
    (setf (gethash :web-search-allow-domains cli-values)
          '(:append "cli.example"))
    (unwind-protect
         (progn
           (ensure-directories-exist project-path)
           (ensure-directories-exist directory-path)
           (%write-text-file global-path
                             "(configure :web-search-allow-domains '(\"global.example\"))")
           (%write-text-file project-path
                             "(configure :web-search-allow-domains '(:append (\"project.example\")))")
           (%write-text-file directory-path
                             "(configure :web-search-allow-domains '(:prepend (\"directory.example\")))")
           (let ((cfg (amoebum.config:load-config
                       :project-root project-root
                       :directory-root directory-root
                       :global-config-path global-path
                       :project-config-path project-path
                       :environment-values env-values
                       :cli-values cli-values)))
             (is (equal '("directory.example"
                          "global.example"
                          "project.example"
                          "env.example"
                          "cli.example")
                        (amoebum.config:config-value :web-search-allow-domains cfg)))
             (is (eq :cli
                     (amoebum.config:config-layer-source :web-search-allow-domains cfg)))))
      (%delete-directory-tree-safe tmp-dir))))

(test cli-arguments-parsing
  "CLI arguments should be parsed correctly."
  (let ((cfg (amoebum.config:load-config
              :project-root "/tmp/"
              :global-config-path "/nonexistent/global.lisp"
              :project-config-path "/nonexistent/project.lisp"
              :environment-values nil
              :cli-arguments '("--model" "cli-arg-model"
                               "--permission-mode" "full-auto"))))
    (is (string= "cli-arg-model" (amoebum.config:config-value :model cfg)))
    (is (eq :full-auto (amoebum.config:config-value :permission-mode cfg)))))

(test per-key-replacement-works
  "Each layer can override individual keys without affecting others."
  (let* ((tmp-dir (%make-temp-directory "amoebum-config-loader"))
         (global-path (merge-pathnames #P"global-config.lisp" tmp-dir))
         (project-path (merge-pathnames #P"project-config.lisp" tmp-dir)))
    (unwind-protect
         (progn
           (%write-text-file global-path
                             "(configure :model \"global-model\" :permission-mode :yolo)")
           (%write-text-file project-path
                             "(configure :model \"project-model\")")
           (let ((cfg (amoebum.config:load-config
                       :project-root "/tmp/"
                       :global-config-path global-path
                       :project-config-path project-path
                       :environment-values nil
                       :cli-values nil)))
             ;; Model overridden by project
             (is (string= "project-model" (amoebum.config:config-value :model cfg)))
             ;; Permission mode from global (not overridden by project)
             (is (eq :yolo (amoebum.config:config-value :permission-mode cfg)))
             (is (eq :global (amoebum.config:config-layer-source :permission-mode cfg)))))
      (%delete-directory-tree-safe tmp-dir))))

(test configure-function-requires-context
  "CONFIGURE should error outside of config file loading context."
  (signals error
    (amoebum.config:configure :model "should-fail")))

(test reload-config-updates-current
  "reload-config should update *current-config*."
  (let ((old-config amoebum::*current-config*))
    (unwind-protect
         (progn
           (amoebum::reload-config :project-root "/tmp/"
                                    :global-config-path "/nonexistent/g.lisp"
                                    :project-config-path "/nonexistent/p.lisp"
                                    :environment-values nil
                                    :cli-values nil)
           (is (amoebum.config:config-p amoebum::*current-config*))
           (is (not (eq old-config amoebum::*current-config*))))
      (setf amoebum::*current-config* old-config))))
