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
    (funcall load-asd-fn (merge-pathnames #P"sw4rm-sdk/sw4rm-sdk.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"ptui/ptui.asd" repo-root))
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
         (ensure-directory-pathname
           (symbol-function (funcall symbol-in "ENSURE-DIRECTORY-PATHNAME" uiop-pkg)))
         (temporary-directory
           (symbol-function (funcall symbol-in "TEMPORARY-DIRECTORY" uiop-pkg)))
         (reload-config-fn
           (symbol-function (funcall symbol-in "RELOAD-CONFIG" amoebum-pkg)))
         (config-model-fn
           (symbol-function (funcall symbol-in "CONFIG-MODEL" amoebum-pkg)))
         (config-permission-mode-fn
           (symbol-function (funcall symbol-in "CONFIG-PERMISSION-MODE" amoebum-pkg)))
         (config-memory-backend-fn
           (symbol-function (funcall symbol-in "CONFIG-MEMORY-BACKEND" amoebum-pkg)))
         (config-project-root-fn
           (symbol-function (funcall symbol-in "CONFIG-PROJECT-ROOT" amoebum-pkg)))
         (config-layer-source-fn
           (symbol-function (funcall symbol-in "CONFIG-LAYER-SOURCE" amoebum-pkg)))
         (setconfig-fn
           (symbol-function (funcall symbol-in "SETCONFIG" amoebum-pkg)))
         (current-config-fn
           (symbol-function (funcall symbol-in "CURRENT-CONFIG" amoebum-pkg)))
         (dispatch-slash-command-fn
           (symbol-function (funcall symbol-in "DISPATCH-SLASH-COMMAND" amoebum-pkg)))
         (slash-command-result-output-fn
           (symbol-function (funcall symbol-in "SLASH-COMMAND-RESULT-OUTPUT" amoebum-pkg)))
         (configuration-error-sym
           (funcall symbol-in "CONFIGURATION-ERROR" amoebum-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (contains-text-p (haystack needle)
               (and (stringp haystack)
                    (search needle haystack :test #'char-equal))))
      (let* ((tmp-root
               (funcall ensure-directory-pathname
                        (merge-pathnames
                         (make-pathname :directory `(:relative ,(format nil "amoebum-i24-~A" (get-universal-time))))
                         (funcall temporary-directory))))
             (project-root (merge-pathnames #P"project/" tmp-root))
             (directory-root (merge-pathnames #P"work/deep/" project-root))
             (fake-home (merge-pathnames #P"home/" tmp-root))
             (global-config (merge-pathnames #P".amoebum/config.lisp" fake-home))
             (project-config (merge-pathnames #P".amoebum/config.lisp" project-root))
             (directory-config (merge-pathnames #P"work/.amoebum/config.lisp" project-root)))
        (ensure-directories-exist global-config)
        (ensure-directories-exist project-config)
        (ensure-directories-exist directory-config)
        (with-open-file (stream global-config
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
          (write-line "(configure :model \"global-model\" :permission-mode :auto-edit :memory-backend :haake-cli)" stream))
        (with-open-file (stream project-config
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
          (write-line "(configure :model \"project-model\" :memory-backend :file)" stream))
        (with-open-file (stream directory-config
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
          (write-line "(configure :model \"directory-model\" :permission-mode :full-auto)" stream))

        (let ((cfg (funcall reload-config-fn
                            :project-root project-root
                            :directory-root directory-root
                            :global-config-path global-config
                            :project-config-path project-config
                            :environment-values '(:model "env-model" :permission-mode :yolo)
                            :cli-arguments '("--model" "cli-model" "--permission-mode=no-confirm"))))
          (assert-true (string= (funcall config-model-fn cfg) "cli-model")
                       "Expected CLI value to override env/directory/project/global for :model.")
          (assert-true (eq (funcall config-permission-mode-fn cfg) :no-confirm)
                       "Expected CLI value to override env/directory/global for :permission-mode.")
          (assert-true (eq (funcall config-memory-backend-fn cfg) :file)
                       "Expected project value to override global/system for :memory-backend.")
          (assert-true (equal (truename project-root)
                              (truename (funcall config-project-root-fn cfg)))
                       "Expected project root to be retained in config.")
          (assert-true (eq (funcall config-layer-source-fn :model cfg) :cli)
                       "Expected :model source to be :cli, got ~S."
                       (funcall config-layer-source-fn :model cfg))
          (assert-true (eq (funcall config-layer-source-fn :permission-mode cfg) :cli)
                       "Expected :permission-mode source to be :cli, got ~S."
                       (funcall config-layer-source-fn :permission-mode cfg))
          (assert-true (eq (funcall config-layer-source-fn :memory-backend cfg) :project)
                       "Expected :memory-backend source to be :project, got ~S."
                       (funcall config-layer-source-fn :memory-backend cfg))
          (assert-true (eq (funcall config-layer-source-fn :plan-mode cfg) :built-in)
                       "Expected untouched :plan-mode source to remain :built-in, got ~S."
                       (funcall config-layer-source-fn :plan-mode cfg))

          (let ((directory-precedence-cfg
                  (funcall reload-config-fn
                           :project-root project-root
                           :directory-root directory-root
                           :global-config-path global-config
                           :project-config-path project-config)))
            (assert-true (string= (funcall config-model-fn directory-precedence-cfg) "directory-model")
                         "Expected directory layer to override project/global for :model.")
            (assert-true (eq (funcall config-layer-source-fn :model directory-precedence-cfg) :directory)
                         "Expected directory-only merge to report :directory source."))

          (multiple-value-bind (handledp result)
              (funcall dispatch-slash-command-fn "/config" :config cfg)
            (assert-true handledp "Expected /config to be handled.")
            (let ((output (funcall slash-command-result-output-fn result)))
              (assert-true (contains-text-p output "model = \"cli-model\" (source: cli)")
                           "Expected /config output to include model source attribution. Output=~S"
                           output)
              (assert-true (contains-text-p output "memory-backend = :FILE (source: project)")
                           "Expected /config output to include memory-backend source attribution. Output=~S"
                           output)))

          (assert-true (string= (funcall setconfig-fn :model "runtime-model")
                                "runtime-model")
                       "Expected setconfig to return and apply the new value immediately.")
          (assert-true (string= (funcall config-model-fn (funcall current-config-fn))
                                "runtime-model")
                       "Expected model slot to change immediately after setconfig.")
          (assert-true (eq (funcall config-layer-source-fn :model (funcall current-config-fn)) :runtime)
                       "Expected setconfig to mark model source as :runtime.")

          (let ((saw-error nil)
                (saw-restart nil))
            (handler-bind
                ((error
                   (lambda (condition)
                     (when (typep condition configuration-error-sym)
                       (setf saw-error t)
                       (let ((restart (find-if (lambda (candidate)
                                                 (and (restart-name candidate)
                                                      (string= (string (restart-name candidate))
                                                               "USE-DEFAULT")))
                                               (compute-restarts condition))))
                         (when restart
                           (setf saw-restart t)
                           (invoke-restart restart)))))))
              (funcall setconfig-fn :permission-mode :invalid-mode))
            (assert-true saw-error
                         "Expected invalid config update to signal configuration-error.")
            (assert-true saw-restart
                         "Expected use-default restart to be available for configuration-error.")
            (assert-true (eq (funcall config-permission-mode-fn (funcall current-config-fn))
                             :supervised)
                         "Expected use-default restart to restore :permission-mode default."))))))

  (format t "AMOEBUM_CONFIG_SMOKE_OK~%"))
