#.(progn (require :asdf) nil)

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
         (pseudopod-pkg (or (find-package "PSEUDOPOD")
                            (error "Missing package PSEUDOPOD after load.")))
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
         (load-config-fn (funcall fn-in "LOAD-CONFIG" amoebum-pkg))
         (reload-config-fn (funcall fn-in "RELOAD-CONFIG" amoebum-pkg))
         (current-config-fn (funcall fn-in "CURRENT-CONFIG" amoebum-pkg))
         (config-permission-mode-fn (funcall fn-in "CONFIG-PERMISSION-MODE" amoebum-pkg))
         (config-layer-source-fn (funcall fn-in "CONFIG-LAYER-SOURCE" amoebum-pkg))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (check-permission-fn (funcall fn-in "CHECK-PERMISSION" amoebum-pkg))
         (dispatch-slash-command-fn (funcall fn-in "DISPATCH-SLASH-COMMAND" amoebum-pkg))
         (slash-command-result-output-fn (funcall fn-in "SLASH-COMMAND-RESULT-OUTPUT" amoebum-pkg))
         (make-event-bus-fn (funcall fn-in "MAKE-EVENT-BUS" amoebum-pkg))
         (subscribe-fn (funcall fn-in "SUBSCRIBE" amoebum-pkg))
         (event-payload-fn (funcall fn-in "EVENT-PAYLOAD" amoebum-pkg))
         (tool-invoked-payload-permission-mode-fn
           (funcall fn-in "TOOL-INVOKED-PAYLOAD-PERMISSION-MODE" amoebum-pkg))
         (make-context-fn (funcall fn-in "MAKE-AMOEBUM-CONTEXT" amoebum-pkg))
         (execute-tool-fn (funcall fn-in "EXECUTE-TOOL" amoebum-pkg))
         (make-toolset-fn (funcall fn-in "MAKE-TOOLSET" pseudopod-pkg))
         (register-tool-function-fn (funcall fn-in "REGISTER-TOOL-FUNCTION" pseudopod-pkg))
         (make-tool-call-fn (funcall fn-in "MAKE-TOOL-CALL" pseudopod-pkg))
         (tool-invoked-event-type
           (symbol-value (funcall symbol-in "+EVENT-TYPE-TOOL-INVOKED+" amoebum-pkg)))
         (tool-permission-denied-sym
           (funcall symbol-in "TOOL-PERMISSION-DENIED" amoebum-pkg)))
    (labels
        ((assert-true (condition format-string &rest format-args)
           (unless condition
             (error (apply #'format nil format-string format-args))))
         (contains-substring-p (needle haystack)
           (and (stringp haystack)
                (search needle haystack :test #'char-equal)))
         (write-text-file (path content)
           (ensure-directories-exist path)
           (with-open-file (stream path
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
             (write-string content stream))))
      (let* ((tmp-root
               (funcall ensure-directory-pathname-fn
                        (merge-pathnames
                         (make-pathname :directory `(:relative
                                                     ,(format nil "amoebum-i127-~A-~A"
                                                              (get-universal-time)
                                                              (random 1000000))))
                         (funcall temporary-directory-fn))))
             (default-root (merge-pathnames #P"default-project/" tmp-root))
             (project-root (merge-pathnames #P"mode-project/" tmp-root))
             (missing-global-config (merge-pathnames #P"global-config.lisp" tmp-root))
             (project-config-file (merge-pathnames #P".amoebum/config.lisp" project-root)))
        (ensure-directories-exist default-root)
        (ensure-directories-exist project-root)

        ;; Global default mode remains supervised in a clean project.
        (let ((default-cfg
                (funcall load-config-fn
                         :project-root default-root
                         :global-config-path missing-global-config
                         :environment-values '())))
          (assert-true (eq (funcall config-permission-mode-fn default-cfg) :supervised)
                       "Expected clean-project default permission mode :supervised, got ~S."
                       (funcall config-permission-mode-fn default-cfg)))

        ;; Project-level override is respected.
        (write-text-file project-config-file "(configure :permission-mode :auto-edit)")
        (let ((project-cfg
                (funcall load-config-fn
                         :project-root project-root
                         :global-config-path missing-global-config
                         :environment-values '())))
          (assert-true (eq (funcall config-permission-mode-fn project-cfg) :auto-edit)
                       "Expected project config override to set :auto-edit, got ~S."
                       (funcall config-permission-mode-fn project-cfg)))

        ;; CLI layer can select a mode and wins over project defaults.
        (let ((cli-cfg
                (funcall load-config-fn
                         :project-root project-root
                         :global-config-path missing-global-config
                         :environment-values '()
                         :cli-arguments '("--permission-mode" "full-auto"))))
          (assert-true (eq (funcall config-permission-mode-fn cli-cfg) :full-auto)
                       "Expected --permission-mode full-auto CLI override, got ~S."
                       (funcall config-permission-mode-fn cli-cfg))
          (assert-true (eq (funcall config-layer-source-fn :permission-mode cli-cfg) :cli)
                       "Expected permission-mode source :cli for CLI override, got ~S."
                       (funcall config-layer-source-fn :permission-mode cli-cfg)))

        ;; Runtime switching via /mode is immediate and changes policy outcomes.
        (funcall reload-config-fn
                 :project-root project-root
                 :global-config-path missing-global-config
                 :environment-values '())
        (multiple-value-bind (handledp result)
            (funcall dispatch-slash-command-fn "/mode full-auto")
          (assert-true handledp "Expected /mode full-auto to be handled.")
          (assert-true (contains-substring-p "full-auto"
                                             (funcall slash-command-result-output-fn result))
                       "Expected /mode output to mention full-auto, got ~S."
                       (funcall slash-command-result-output-fn result)))
        (assert-true
         (eq (funcall check-permission-fn :tool :bash :command "git status") :allow)
         "Expected full-auto mode to auto-allow git status shell command.")

        (multiple-value-bind (handledp result)
            (funcall dispatch-slash-command-fn "/mode auto-edit")
          (assert-true handledp "Expected /mode auto-edit to be handled.")
          (assert-true (contains-substring-p "auto-edit"
                                             (funcall slash-command-result-output-fn result))
                       "Expected /mode output to mention auto-edit, got ~S."
                       (funcall slash-command-result-output-fn result)))
        (assert-true
         (eq (funcall check-permission-fn :tool :bash :command "git status") :prompt)
         "Expected auto-edit mode to require prompt for shell commands.")
        (assert-true
         (eq (funcall check-permission-fn
                      :tool :read-file
                      :path (namestring (merge-pathnames #P"src/main.lisp" project-root)))
             :allow)
         "Expected auto-edit mode to auto-allow path-scoped file reads.")

        ;; Contexts without explicit mode follow runtime mode changes per call.
        (let* ((bus (funcall make-event-bus-fn :capacity 16))
               (toolset (funcall make-toolset-fn))
               (invoked-modes '()))
          (funcall register-tool-function-fn
                   toolset
                   :name "i127-mode-tool"
                   :description "I127 runtime permission mode smoke tool."
                   :parameters "{\"type\":\"object\",\"properties\":{}}"
                   :fn (lambda (_arguments &optional _tool-call)
                         (declare (ignore _arguments _tool-call))
                         "mode-ok"))
          (funcall subscribe-fn
                   bus
                   tool-invoked-event-type
                   (lambda (event)
                     (push (funcall tool-invoked-payload-permission-mode-fn
                                    (funcall event-payload-fn event))
                           invoked-modes)))
          (let* ((context (funcall make-context-fn :toolset toolset :event-bus bus))
                 (tool-call (funcall make-tool-call-fn
                                     :name "i127-mode-tool"
                                     :arguments "{\"path\":\"src/main.lisp\"}"))
                 (denied-p nil))
            (funcall setconfig-fn :permission-mode :supervised)
            (handler-case
                (funcall execute-tool-fn tool-call context)
              (error (condition)
                (when (typep condition tool-permission-denied-sym)
                  (setf denied-p t))))
            (assert-true denied-p
                         "Expected supervised mode to deny tool execution in shared context.")
            (funcall setconfig-fn :permission-mode :full-auto)
            (assert-true (string= (funcall execute-tool-fn tool-call context) "mode-ok")
                         "Expected full-auto mode to allow tool execution in shared context.")
            (funcall setconfig-fn :permission-mode :auto-edit)
            (assert-true (string= (funcall execute-tool-fn tool-call context) "mode-ok")
                         "Expected auto-edit mode to allow path-scoped tool execution in shared context.")
            (assert-true (equal (nreverse invoked-modes) '(:full-auto :auto-edit))
                         "Expected emitted tool-invoked permission modes (:full-auto :auto-edit), got ~S."
                         (nreverse invoked-modes)))))
      ;; Leave global runtime mode in supervised state after smoke assertions.
      (funcall setconfig-fn :permission-mode :supervised)
      (assert-true
       (eq (funcall config-permission-mode-fn (funcall current-config-fn)) :supervised)
       "Expected runtime config to remain in supervised mode after cleanup.")))

  (format t "AMOEBUM_PERMISSION_MODE_SMOKE_OK~%"))
