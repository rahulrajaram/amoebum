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
         (fn-in
           (lambda (name package)
             (symbol-function (funcall symbol-in name package))))
         (ensure-directory-pathname
           (symbol-function (funcall symbol-in "ENSURE-DIRECTORY-PATHNAME" uiop-pkg)))
         (temporary-directory
           (symbol-function (funcall symbol-in "TEMPORARY-DIRECTORY" uiop-pkg)))
         (make-file-memory-backend-fn (funcall fn-in "MAKE-FILE-MEMORY-BACKEND" amoebum-pkg))
         (reset-memory-backend-fn (funcall fn-in "RESET-MEMORY-BACKEND" amoebum-pkg))
         (memory-store-fn (funcall fn-in "MEMORY-STORE" amoebum-pkg))
         (memory-list-fn (funcall fn-in "MEMORY-LIST" amoebum-pkg))
         (memory-entry-key-fn (funcall fn-in "MEMORY-ENTRY-KEY" amoebum-pkg))
         (memory-entry-value-fn (funcall fn-in "MEMORY-ENTRY-VALUE" amoebum-pkg))
         (run-memory-command-fn (funcall fn-in "RUN-MEMORY-COMMAND" amoebum-pkg))
         (session-memory-entries-fn (funcall fn-in "SESSION-MEMORY-ENTRIES" amoebum-pkg))
         (extract-durable-memory-candidate-fn
           (funcall fn-in "EXTRACT-DURABLE-MEMORY-CANDIDATE" amoebum-pkg))
         (memory-candidate-kind-fn (funcall fn-in "MEMORY-CANDIDATE-KIND" amoebum-pkg))
         (memory-editor-runner-sym (funcall symbol-in "*MEMORY-EDITOR-RUNNER*" amoebum-pkg))
         (session-memory-sym (funcall symbol-in "*SESSION-MEMORY-ENTRIES*" amoebum-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (contains-ci (text needle)
               (and (stringp text)
                    (stringp needle)
                    (search needle text :test #'char-equal))))
      (let* ((tmp-root
               (funcall ensure-directory-pathname
                        (merge-pathnames
                         (make-pathname :directory `(:relative ,(format nil "amoebum-i38-~A" (get-universal-time))))
                         (funcall temporary-directory))))
             (project-root (funcall ensure-directory-pathname (merge-pathnames #P"project/" tmp-root)))
             (global-memory (merge-pathnames #P"home/.amoebum/memory/MEMORY.md" tmp-root))
             (project-memory (merge-pathnames #P".amoebum/MEMORY.md" project-root))
             (backend (funcall make-file-memory-backend-fn
                               :project-root project-root
                               :global-path global-memory
                               :project-path project-memory))
             (edit-capture nil)
             (old-editor-runner (symbol-value memory-editor-runner-sym)))
        (funcall reset-memory-backend-fn backend)
        (setf (symbol-value session-memory-sym) '())

        (funcall memory-store-fn backend "package-manager" "Use npm for this repo" :scope :global :source :smoke)
        (funcall memory-store-fn backend "package-manager" "Use bun for this repo" :scope :project :source :smoke)
        (assert-true (probe-file global-memory)
                     "Expected global MEMORY.md to exist at ~S."
                     global-memory)
        (assert-true (probe-file project-memory)
                     "Expected project MEMORY.md to exist at ~S."
                     project-memory)

        (let* ((reloaded (funcall make-file-memory-backend-fn
                                  :project-root project-root
                                  :global-path global-memory
                                  :project-path project-memory))
               (effective (funcall memory-list-fn reloaded :scope :effective))
               (entry (find "package-manager"
                            effective
                            :key memory-entry-key-fn
                            :test #'string=)))
          (assert-true entry
                       "Expected effective memory to include package-manager key.")
          (assert-true (string= (funcall memory-entry-value-fn entry)
                                "Use bun for this repo")
                       "Expected project memory to override global memory for same key."))

        (multiple-value-bind (handledp show-output)
            (funcall run-memory-command-fn "/memory show" :backend backend)
          (assert-true handledp "Expected /memory show command to be handled.")
          (assert-true (contains-ci show-output "Use bun for this repo")
                       "Expected /memory show output to include effective memory entry, got ~S."
                       show-output))

        (multiple-value-bind (handledp remember-output)
            (funcall run-memory-command-fn
                     "/memory remember Always run tests before commit"
                     :backend backend)
          (assert-true handledp "Expected /memory remember command to be handled.")
          (assert-true (contains-ci remember-output "Remembered")
                       "Expected /memory remember confirmation, got ~S."
                       remember-output))

        (assert-true (> (length (funcall session-memory-entries-fn)) 0)
                     "Expected session memory list to include remembered entries.")

        (multiple-value-bind (handledp clear-output)
            (funcall run-memory-command-fn "/memory clear" :backend backend)
          (assert-true handledp "Expected /memory clear command to be handled.")
          (assert-true (contains-ci clear-output "Cleared")
                       "Expected /memory clear output, got ~S."
                       clear-output))
        (assert-true (= (length (funcall session-memory-entries-fn)) 0)
                     "Expected /memory clear to reset session memory entries.")

        (unwind-protect
             (progn
               (setf (symbol-value memory-editor-runner-sym)
                     (lambda (editor path)
                       (setf edit-capture (list editor path))
                       0))
               (multiple-value-bind (handledp edit-output)
                   (funcall run-memory-command-fn
                            "/memory edit"
                            :backend backend
                            :editor "fake-editor")
                 (assert-true handledp "Expected /memory edit command to be handled.")
                 (assert-true (contains-ci edit-output "Opened")
                              "Expected /memory edit output to acknowledge editor launch, got ~S."
                              edit-output)))
          (setf (symbol-value memory-editor-runner-sym) old-editor-runner))

        (assert-true edit-capture
                     "Expected /memory edit to invoke editor runner.")
        (assert-true (and (string= (first edit-capture) "fake-editor")
                          (string= (second edit-capture) (namestring project-memory)))
                     "Expected /memory edit runner args to target project MEMORY.md, got ~S."
                     edit-capture)

        (let ((remember-candidate
                (funcall extract-durable-memory-candidate-fn
                         "Remember that I always use bun instead of npm"))
              (preference-candidate
                (funcall extract-durable-memory-candidate-fn
                         "I prefer snake_case variable names."))
              (forget-candidate
                (funcall extract-durable-memory-candidate-fn
                         "Forget the bun preference")))
          (assert-true remember-candidate
                       "Expected remember-style statement to produce a memory candidate.")
          (assert-true (eq (funcall memory-candidate-kind-fn remember-candidate) :remember)
                       "Expected remember candidate kind :remember.")
          (assert-true preference-candidate
                       "Expected preference-style statement to produce a memory candidate.")
          (assert-true (eq (funcall memory-candidate-kind-fn preference-candidate) :preference)
                       "Expected preference candidate kind :preference.")
          (assert-true forget-candidate
                       "Expected forget-style statement to produce a memory candidate.")
          (assert-true (eq (funcall memory-candidate-kind-fn forget-candidate) :forget)
                       "Expected forget candidate kind :forget.")))))

  (format t "AMOEBUM_MEMORY_SMOKE_OK~%"))
