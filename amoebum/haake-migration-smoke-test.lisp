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
         (ensure-directory-pathname
           (symbol-function (funcall symbol-in "ENSURE-DIRECTORY-PATHNAME" uiop-pkg)))
         (temporary-directory
           (symbol-function (funcall symbol-in "TEMPORARY-DIRECTORY" uiop-pkg)))
         (reload-config-fn (funcall fn-in "RELOAD-CONFIG" amoebum-pkg))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (run-memory-command-fn (funcall fn-in "RUN-MEMORY-COMMAND" amoebum-pkg))
         (make-file-memory-backend-fn (funcall fn-in "MAKE-FILE-MEMORY-BACKEND" amoebum-pkg))
         (availability-runner-sym
          (funcall symbol-in "*HAAKE-CLI-AVAILABILITY-RUNNER*" amoebum-pkg))
         (status-runner-sym
          (funcall symbol-in "*HAAKE-CLI-STATUS-RUNNER*" amoebum-pkg))
         (capability-runner-sym
          (funcall symbol-in "*HAAKE-CLI-CAPABILITY-RUNNER*" amoebum-pkg))
         (command-runner-sym
          (funcall symbol-in "*HAAKE-CLI-COMMAND-RUNNER*" amoebum-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (contains-ci (text needle)
               (and (stringp text)
                    (stringp needle)
                    (search needle text :test #'char-equal)))
             (write-lines (path lines)
               (ensure-directories-exist path)
               (with-open-file (stream path
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
                 (dolist (line lines)
                   (write-line line stream)))
               path)
             (read-text (path)
               (with-open-file (stream path :direction :input)
                 (with-output-to-string (out)
                   (loop for line = (read-line stream nil nil)
                         while line do
                           (write-line line out)))))
             (option-value (argv option)
               (loop for item in argv
                     for rest on argv
                     when (string= item option)
                       do (return (second rest))))
             (metadata-values (argv)
               (loop for item in argv
                     for rest on argv
                     when (string= item "--metadata")
                       collect (second rest))))
      (let* ((tmp-root
               (funcall ensure-directory-pathname
                        (merge-pathnames
                         (make-pathname :directory `(:relative ,(format nil "amoebum-i54-~A" (get-universal-time))))
                         (funcall temporary-directory))))
             (project-root (funcall ensure-directory-pathname (merge-pathnames #P"project/" tmp-root)))
             (global-memory (merge-pathnames #P"home/.amoebum/memory/MEMORY.md" tmp-root))
             (project-memory (merge-pathnames #P".amoebum/MEMORY.md" project-root))
             (topic-memory (merge-pathnames #P".amoebum/memory/style.md" project-root))
             (export-path (merge-pathnames #P".amoebum/memory/haake-export-MEMORY.md" project-root))
             (state-path (merge-pathnames #P".amoebum/memory/haake-import-state-v1.sexp" project-root))
             (failure-log-path (merge-pathnames #P".amoebum/memory/haake-import-failures.log" project-root))
             (backend (funcall make-file-memory-backend-fn
                               :project-root project-root
                               :global-path global-memory
                               :project-path project-memory))
             (old-availability-runner (symbol-value availability-runner-sym))
             (old-status-runner (symbol-value status-runner-sym))
             (old-capability-runner (symbol-value capability-runner-sym))
             (old-command-runner (symbol-value command-runner-sym))
             (insert-commands '())
             (remote-by-scope (make-hash-table :test #'equal))
             (import-id-counter 0))
        (unwind-protect
             (progn
               (write-lines global-memory
                            '("# Amoebum Memory"
                              ""
                              "- [package-manager] Use bun everywhere"))
               (write-lines project-memory
                            '("# Amoebum Memory"
                              ""
                              "- [tests] Run smoke first"))
               (write-lines topic-memory
                            '("# Topic memory"
                              ""
                              "- [naming] Use snake_case"))

               (funcall reload-config-fn :project-root project-root)
               (funcall setconfig-fn :haake-command "haake")
               (funcall setconfig-fn :haake-project-id "smoke-project")
               (funcall setconfig-fn :haake-agent "amoebum-smoke")
               (funcall setconfig-fn :haake-autodetect t)
               (funcall setconfig-fn :memory-backend :auto)

               (setf (symbol-value availability-runner-sym)
                     (lambda (command)
                       (string= command "haake")))
               (setf (symbol-value status-runner-sym)
                     (lambda (command &key directory)
                       (declare (ignore directory))
                       (string= command "haake")))
               (setf (symbol-value capability-runner-sym)
                     (lambda (command &key directory)
                       (declare (ignore directory))
                       (if (string= command "haake")
                           (list :exit-code 0
                                 :stdout "insert query list delete clear"
                                 :stderr "")
                           (list :exit-code 1
                                 :stdout ""
                                 :stderr ""))))
               (setf (symbol-value command-runner-sym)
                     (lambda (arguments &key directory input)
                       (declare (ignore directory input))
                       (let ((action (third arguments)))
                         (cond
                           ((string= action "insert")
                            (let* ((scope (fourth arguments))
                                   (value (fifth arguments))
                                   (key (option-value arguments "--key")))
                              (push arguments insert-commands)
                              (incf import-id-counter)
                              (setf (gethash scope remote-by-scope)
                                    (append (gethash scope remote-by-scope)
                                            (list (list :key key :value value))))
                              (list :exit-code 0
                                    :stdout (format nil "id~Cimport-~D~%" #\Tab import-id-counter)
                                    :stderr "")))
                           ((string= action "list")
                            (let* ((scope (fourth arguments))
                                   (entries (gethash scope remote-by-scope)))
                              (list :exit-code 0
                                    :stdout (with-output-to-string (out)
                                              (dolist (entry entries)
                                                (format out "~A~C~A~%"
                                                        (or (getf entry :key) "")
                                                        #\Tab
                                                        (or (getf entry :value) ""))))
                                    :stderr "")))
                           (t
                            (error "Unexpected Haake action in migration smoke: ~S" action))))))

               (multiple-value-bind (handledp first-output)
                   (funcall run-memory-command-fn
                            "/memory import --to haake"
                            :backend backend)
                 (assert-true handledp
                              "Expected first /memory import command to be handled.")
                 (assert-true (contains-ci first-output "imported 3")
                              "Expected first import to insert 3 entries, got ~S."
                              first-output)
                 (assert-true (contains-ci first-output "skipped 0")
                              "Expected first import skip count 0, got ~S."
                              first-output)
                 (assert-true (contains-ci first-output "failed 0")
                              "Expected first import failure count 0, got ~S."
                              first-output))

               (assert-true (= (length insert-commands) 3)
                            "Expected exactly 3 insert calls from first import, got ~D."
                            (length insert-commands))
               (assert-true (some (lambda (argv)
                                    (member "global/preferences" argv :test #'string=))
                                  insert-commands)
                            "Expected one import into global/preferences.")
               (assert-true (some (lambda (argv)
                                    (member "project/smoke-project/preferences" argv :test #'string=))
                                  insert-commands)
                            "Expected one import into project preferences scope.")
               (assert-true (some (lambda (argv)
                                    (member "project/smoke-project/topic/style" argv :test #'string=))
                                  insert-commands)
                            "Expected one import into topic/style scope.")
               (dolist (argv insert-commands)
                 (let ((metadata (metadata-values argv)))
                   (assert-true (some (lambda (item)
                                        (contains-ci item "source_path="))
                                      metadata)
                                "Expected metadata to include source_path, argv=~S"
                                argv)
                   (assert-true (some (lambda (item)
                                        (contains-ci item "source_hash="))
                                      metadata)
                                "Expected metadata to include source_hash, argv=~S"
                                argv)
                   (assert-true (some (lambda (item)
                                        (contains-ci item "import_batch_id="))
                                      metadata)
                                "Expected metadata to include import_batch_id, argv=~S"
                                argv)))

               (assert-true (probe-file state-path)
                            "Expected idempotency state file at ~S."
                            state-path)
               (assert-true (not (probe-file failure-log-path))
                            "Did not expect failure log when import succeeds.")

               (multiple-value-bind (handledp second-output)
                   (funcall run-memory-command-fn
                            "/memory import --to haake"
                            :backend backend)
                 (assert-true handledp
                              "Expected second /memory import command to be handled.")
                 (assert-true (contains-ci second-output "imported 0")
                              "Expected second import to be idempotent, got ~S."
                              second-output)
                 (assert-true (contains-ci second-output "skipped 3")
                              "Expected second import to skip all entries, got ~S."
                              second-output)
                 (assert-true (= (length insert-commands) 3)
                              "Expected no additional insert calls after idempotent re-import, got ~D."
                              (length insert-commands)))

               (multiple-value-bind (handledp export-output)
                   (funcall run-memory-command-fn
                            "/memory export --from haake"
                            :backend backend)
                 (assert-true handledp
                              "Expected /memory export command to be handled.")
                 (assert-true (contains-ci export-output "haake-export-MEMORY.md")
                              "Expected export output to mention snapshot path, got ~S."
                              export-output))

               (assert-true (probe-file export-path)
                            "Expected exported markdown snapshot at ~S."
                            export-path)
               (let ((snapshot (read-text export-path)))
                 (assert-true (contains-ci snapshot "## Global")
                              "Expected export to include global section.")
                 (assert-true (contains-ci snapshot "## Project")
                              "Expected export to include project section.")
                 (assert-true (contains-ci snapshot "## Topic: style")
                              "Expected export to include topic section for style.")
                 (assert-true (contains-ci snapshot "Use bun everywhere")
                              "Expected global entry to round-trip in export.")
                 (assert-true (contains-ci snapshot "Run smoke first")
                              "Expected project entry to round-trip in export.")
                 (assert-true (contains-ci snapshot "Use snake_case")
                              "Expected topic entry to round-trip in export.")))
          (setf (symbol-value availability-runner-sym) old-availability-runner
                (symbol-value status-runner-sym) old-status-runner
                (symbol-value capability-runner-sym) old-capability-runner
                (symbol-value command-runner-sym) old-command-runner)))))

  (format t "AMOEBUM_HAAKE_MIGRATION_SMOKE_OK~%"))
