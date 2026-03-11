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
    (funcall load-asd-fn (merge-pathnames #P"sw4rm-sdk/sw4rm-sdk.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"ptui/ptui.asd" repo-root))
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
         (assemble-system-prompt-fn (funcall fn-in "ASSEMBLE-SYSTEM-PROMPT" amoebum-pkg))
         (global-layer-candidates-fn
           (funcall fn-in "%SYSTEM-PROMPT-GLOBAL-LAYER-CANDIDATES" amoebum-pkg))
         (make-toolset-fn (funcall fn-in "MAKE-TOOLSET" pseudopod-pkg))
         (make-tool-definition-fn (funcall fn-in "MAKE-TOOL-DEFINITION" pseudopod-pkg))
         (register-tool-fn (funcall fn-in "REGISTER-TOOL" pseudopod-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (contains-substring-p (needle haystack)
               (and (stringp haystack)
                    (search needle haystack :test #'char-equal)))
             (token-position (needle haystack)
               (or (search needle haystack :test #'char-equal)
                   -1))
             (set-home-env (value)
               (setf (uiop:getenv "HOME") value))
             (write-text-file (path content)
               (ensure-directories-exist path)
               (with-open-file (stream path
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create
                                       :external-format :utf-8)
                 (write-string content stream)))
             (run-command (command directory)
               (multiple-value-bind (stdout stderr exit-code)
                   (uiop:run-program command
                                     :directory directory
                                     :ignore-error-status t
                                     :output :string
                                     :error-output :string)
                 (values (or stdout "") (or stderr "") (or exit-code 0))))
             (assert-command-ok (command directory)
               (multiple-value-bind (stdout stderr exit-code)
                   (run-command command directory)
                 (assert-true (= exit-code 0)
                              "Command ~S failed (exit ~D) stdout=~S stderr=~S"
                              command
                              exit-code
                              stdout
                              stderr)))
             (toolset-with-sentinel-tool ()
               (let* ((toolset (funcall make-toolset-fn))
                      (tool (funcall make-tool-definition-fn
                                     :name "sysprompt-sentinel-tool"
                                     :description "Sentinel prompt test tool."
                                     :parameters (let ((schema (make-hash-table :test #'equal))
                                                        (props (make-hash-table :test #'equal)))
                                                   (setf (gethash "type" schema) "object")
                                                   (setf (gethash "properties" schema) props)
                                                   schema)
                                     :fn (lambda (_args &optional _call)
                                           (declare (ignore _args _call))
                                           "ok"))))
                 (funcall register-tool-fn toolset tool)
                 toolset)))
      (let* ((tmp-root
               (funcall ensure-directory-pathname-fn
                        (merge-pathnames
                         (make-pathname :directory `(:relative ,(format nil "amoebum-i60-~A"
                                                                        (get-universal-time))))
                         (funcall temporary-directory-fn))))
             (fake-home (merge-pathnames #P"home/" tmp-root))
             (project-root (merge-pathnames #P"project/" tmp-root))
             (working-dir (merge-pathnames #P"src/" project-root))
             (global-prompt (merge-pathnames #P".amoebum/SYSTEM_PROMPT.md" fake-home))
             (project-prompt (merge-pathnames #P".amoebum/SYSTEM_PROMPT.md" project-root))
             (directory-prompt (merge-pathnames #P".amoebum/prompts/src.md" project-root))
             (root-agents (merge-pathnames #P"AGENTS.md" project-root))
             (directory-agents (merge-pathnames #P"src/AGENTS.md" project-root))
             (fallback-global (merge-pathnames #P"fallback/AGENTS.md" tmp-root))
             (missing-global (merge-pathnames #P"missing/AGENTS.md" tmp-root))
             (missing-directory (merge-pathnames #P"missing-dir/AGENTS.md" tmp-root))
             (codex-home (merge-pathnames #P"codex-home/" tmp-root))
             (codex-global (merge-pathnames #P".codex/AGENTS.md" codex-home))
             (original-home (uiop:getenv "HOME"))
             (toolset (toolset-with-sentinel-tool)))
        (ensure-directories-exist working-dir)
        (write-text-file global-prompt "GLOBAL_LAYER_SENTINEL")
        (write-text-file project-prompt "PROJECT_LAYER_SENTINEL")
        (write-text-file directory-prompt "DIRECTORY_LAYER_SENTINEL")
        (write-text-file root-agents "ROOT_DIRECTORY_SENTINEL POLICY=base")
        (write-text-file directory-agents "NESTED_DIRECTORY_SENTINEL POLICY=override")
        (write-text-file fallback-global "FALLBACK_GLOBAL_SENTINEL")
        (write-text-file codex-global "CODEX_USER_DEFAULT_SENTINEL")
        (write-text-file (merge-pathnames #P"README.md" project-root) "demo")

        (assert-command-ok '("git" "init") project-root)
        (assert-command-ok '("git" "config" "user.email" "i60-smoke@example.com") project-root)
        (assert-command-ok '("git" "config" "user.name" "I60 Smoke") project-root)
        (assert-command-ok '("git" "add" "README.md") project-root)
        (assert-command-ok '("git" "commit" "-m" "seed commit for system prompt smoke") project-root)

        (let* ((assembled
                 (funcall assemble-system-prompt-fn
                          :project-root project-root
                          :cwd working-dir
                          :toolset toolset
                          :global-layer-path global-prompt
                          :project-layer-path project-prompt
                          :directory-layer-paths (list directory-prompt)))
               (global-pos (search "GLOBAL_LAYER_SENTINEL" assembled :test #'char-equal))
               (project-pos (search "PROJECT_LAYER_SENTINEL" assembled :test #'char-equal))
               (directory-pos (search "DIRECTORY_LAYER_SENTINEL" assembled :test #'char-equal)))
          (assert-true (stringp assembled)
                       "Expected assembled system prompt to be a string.")
          (assert-true global-pos "Expected global layer sentinel in assembled prompt.")
          (assert-true project-pos "Expected project layer sentinel in assembled prompt.")
          (assert-true directory-pos "Expected directory layer sentinel in assembled prompt.")
          (assert-true (< global-pos project-pos directory-pos)
                       "Expected layer precedence order global->project->directory.")
          (assert-true (contains-substring-p "Git Context" assembled)
                       "Expected dynamic Git context section.")
          (assert-true (contains-substring-p "Environment Context" assembled)
                       "Expected dynamic environment context section.")
          (assert-true (contains-substring-p "Available Tools" assembled)
                       "Expected available tools section in dynamic context.")
          (assert-true (contains-substring-p "sysprompt-sentinel-tool" assembled)
                       "Expected custom tool to be listed in dynamic context.")
          (assert-true (contains-substring-p "seed commit for system prompt smoke" assembled)
                       "Expected recent commit history to be injected."))

        ;; Candidate discovery should include codex-style user defaults.
        (let* ((candidate-paths (mapcar #'namestring (funcall global-layer-candidates-fn)))
               (home (user-homedir-pathname))
               (codex-path (namestring (merge-pathnames #P".codex/AGENTS.md" home)))
               (config-codex-path (namestring (merge-pathnames #P".config/codex/AGENTS.md" home))))
          (assert-true (find codex-path candidate-paths :test #'string=)
                       "Expected global candidate list to include ~/.codex/AGENTS.md.")
          (assert-true (find config-codex-path candidate-paths :test #'string=)
                       "Expected global candidate list to include ~/.config/codex/AGENTS.md."))

        ;; Missing files should be skipped while preserving deterministic ordering.
        (let* ((assembled
                 (funcall assemble-system-prompt-fn
                          :project-root project-root
                          :cwd working-dir
                          :toolset toolset
                          :global-layer-path (list missing-global fallback-global)
                          :project-layer-path project-prompt
                          :directory-layer-paths (list root-agents missing-directory directory-agents)))
               (fallback-pos (token-position "FALLBACK_GLOBAL_SENTINEL" assembled))
               (root-pos (token-position "ROOT_DIRECTORY_SENTINEL" assembled))
               (nested-pos (token-position "NESTED_DIRECTORY_SENTINEL" assembled))
               (base-policy-pos (token-position "POLICY=base" assembled))
               (override-policy-pos (token-position "POLICY=override" assembled)))
          (assert-true (>= fallback-pos 0)
                       "Expected fallback global file to be ingested when earlier candidate is missing.")
          (assert-true (and (>= root-pos 0) (>= nested-pos 0) (< root-pos nested-pos))
                       "Expected directory instruction merge order root->nested.")
          (assert-true (and (>= base-policy-pos 0)
                            (>= override-policy-pos 0)
                            (< base-policy-pos override-policy-pos))
                       "Expected nested instruction text to appear after parent instruction text."))

        ;; Default global lookup should ingest codex fallback when HOME points at codex-only defaults.
        (unwind-protect
            (progn
              (set-home-env (namestring codex-home))
              (let ((assembled
                      (funcall assemble-system-prompt-fn
                               :project-root project-root
                               :cwd working-dir
                               :toolset toolset
                               :project-layer-path project-prompt
                               :directory-layer-paths (list directory-prompt))))
                (assert-true (contains-substring-p "CODEX_USER_DEFAULT_SENTINEL" assembled)
                             "Expected codex user defaults fallback to be ingested from ~/.codex/AGENTS.md.")))
          (set-home-env original-home)))))

  (format t "AMOEBUM_SYSPROMPT_SMOKE_OK~%"))
