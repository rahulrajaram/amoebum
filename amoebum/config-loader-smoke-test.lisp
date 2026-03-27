(require :asdf)

(let* ((smoke-file (or *load-truename* *compile-file-truename*))
       (amoebum-dir (and smoke-file (make-pathname :name nil :type nil :defaults smoke-file)))
       (repo-root (and amoebum-dir (truename (merge-pathnames #P"../" amoebum-dir)))))
  (unless repo-root
    (error "Unable to resolve repository root from ~S" smoke-file))

  (load (merge-pathnames #P"ptui/.tools/quicklisp/setup.lisp" repo-root))

  (asdf:load-asd (merge-pathnames #P"pseudopod/pseudopod.asd" repo-root))
  (asdf:load-asd (merge-pathnames #P"sw4rm-sdk/sw4rm-sdk.asd" repo-root))
  (asdf:load-asd (merge-pathnames #P"ptui/ptui.asd" repo-root))
  (asdf:load-asd (merge-pathnames #P"amoebum/amoebum.asd" repo-root))
  (asdf:load-system "amoebum")

  (let* ((amoebum-pkg (or (find-package "AMOEBUM")
                          (error "Missing package AMOEBUM after load.")))
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (load-config-fn
           (symbol-function (funcall symbol-in "LOAD-CONFIG" amoebum-pkg)))
         (config-value-fn
           (symbol-function (funcall symbol-in "CONFIG-VALUE" amoebum-pkg)))
         (config-layer-source-fn
           (symbol-function (funcall symbol-in "CONFIG-LAYER-SOURCE" amoebum-pkg))))
    (flet ((assert-true (condition format-string &rest format-args)
             (unless condition
               (error (apply #'format nil format-string format-args)))))
    (let* ((tmp-root (uiop:ensure-directory-pathname
                      (merge-pathnames
                       (make-pathname :directory `(:relative ,(format nil "amoebum-i237-~A" (get-universal-time))))
                       (uiop:ensure-directory-pathname (uiop:temporary-directory)))))
           (project-root (merge-pathnames #P"project/" tmp-root))
           (directory-root (merge-pathnames #P"project/nested/work/" tmp-root))
           (global-config (merge-pathnames #P"global-config.lisp" tmp-root))
           (project-config (merge-pathnames #P"project/.amoebum/config.lisp" tmp-root))
           (directory-config (merge-pathnames #P"project/nested/.amoebum/config.lisp" tmp-root))
           (env-values (make-hash-table :test 'eq))
           (cli-values (make-hash-table :test 'eq)))
      (ensure-directories-exist project-config)
      (ensure-directories-exist directory-config)
      (ensure-directories-exist (merge-pathnames #P".keep" directory-root))

      (with-open-file (stream global-config
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (write-line "(configure :model \"global-model\" :web-search-allow-domains '(\"global.example\"))"
                    stream))
      (with-open-file (stream project-config
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (write-line "(configure :model \"project-model\" :web-search-allow-domains '(:append (\"project.example\")))"
                    stream))
      (with-open-file (stream directory-config
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (write-line "(configure :model \"directory-model\" :web-search-allow-domains '(:prepend (\"directory.example\")))"
                    stream))

      (setf (gethash :model env-values) "env-model"
            (gethash :web-search-allow-domains env-values) '(:append ("env.example"))
            (gethash :swarm-delegation-mode env-values) :local)
      (setf (gethash :model cli-values) "cli-model"
            (gethash :web-search-allow-domains cli-values) '(:append "cli.example")
            (gethash :swarm-delegation-mode cli-values) :networked)

      (let ((cfg (funcall load-config-fn
                          :project-root project-root
                          :directory-root directory-root
                          :global-config-path global-config
                          :project-config-path project-config
                          :environment-values env-values
                          :cli-values cli-values)))
        (assert-true (string= "cli-model" (funcall config-value-fn :model cfg))
                     "Expected CLI to win for :model.")
        (assert-true (eq :cli (funcall config-layer-source-fn :model cfg))
                     "Expected :model source to be :cli.")
        (assert-true (eq :networked
                         (funcall config-value-fn :swarm-delegation-mode cfg))
                     "Expected CLI to win for :swarm-delegation-mode.")
        (assert-true (eq :cli
                         (funcall config-layer-source-fn :swarm-delegation-mode cfg))
                     "Expected :swarm-delegation-mode source to be :cli.")
        (assert-true (equal '("directory.example"
                              "global.example"
                              "project.example"
                              "env.example"
                              "cli.example")
                            (funcall config-value-fn :web-search-allow-domains cfg))
                     "List merge directives did not resolve in expected order: ~S"
                     (funcall config-value-fn :web-search-allow-domains cfg)))

      (let ((cfg (funcall load-config-fn
                          :project-root project-root
                          :directory-root directory-root
                          :global-config-path global-config
                          :project-config-path project-config
                          :environment-values env-values
                          :cli-values nil)))
        (assert-true (eq :local
                         (funcall config-value-fn :swarm-delegation-mode cfg))
                     "Expected environment to drive :swarm-delegation-mode when CLI value is absent.")
        (assert-true (eq :env
                         (funcall config-layer-source-fn :swarm-delegation-mode cfg))
                     "Expected :swarm-delegation-mode source to be :env without CLI override.")))))

  (format t "CONFIG_LOADER_SMOKE_OK~%"))
