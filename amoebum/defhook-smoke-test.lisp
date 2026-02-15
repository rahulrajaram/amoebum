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
         (clear-hooks-fn (funcall fn "CLEAR-HOOKS"))
         (list-hooks-fn (funcall fn "LIST-HOOKS"))
         (run-hooks-fn (funcall fn "RUN-HOOKS"))
         (hook-entry-priority-fn (funcall fn "HOOK-ENTRY-PRIORITY"))
         (hook-registry-sym (funcall symbol-in "*HOOK-REGISTRY*" amoebum-pkg))
         (defhook-sym (funcall symbol-in "DEFHOOK" amoebum-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (make-args (&rest pairs)
               (let ((table (make-hash-table :test #'equal)))
                 (loop for (key value) on pairs by #'cddr
                       do (setf (gethash key table) value))
                 table)))
      (funcall clear-hooks-fn)

      (let ((deny-hook-id
              (eval `(,defhook-sym pre-tool-use (tool-name args)
                       "Deny dangerous recursive removal."
                       (:priority 200)
                       (:async nil)
                       (:match (:tool "bash" :args (:pattern "rm -rf.*"))
                         :deny)))))
        (assert-true (symbolp deny-hook-id)
                     "Expected defhook to return a hook-id symbol.")
        (assert-true (gethash (cons :pre-tool-use deny-hook-id)
                              (symbol-value hook-registry-sym))
                     "Expected registry key (hook-point . hook-id) to be present."))

      (eval `(,defhook-sym pre-tool-use (tool-name args)
               "Catch-all allow."
               (:priority 10)
               (:async nil)
               (:match t
                 :allow)))

      (multiple-value-bind (decision results)
          (funcall run-hooks-fn
                   :pre-tool-use
                   "bash"
                   (make-args "command" "rm -rf /tmp/example"))
        (assert-true (eq decision :deny)
                     "Expected blocking pre-tool-use chain to short-circuit with :deny.")
        (assert-true (= (length results) 1)
                     "Expected :deny short-circuit to stop later hooks."))

      (multiple-value-bind (decision results)
          (funcall run-hooks-fn
                   :pre-tool-use
                   "bash"
                   (make-args "command" "ls -la"))
        (assert-true (eq decision :allow)
                     "Expected non-denied pre-tool-use chain to return :allow.")
        (assert-true (= (length results) 2)
                     "Expected both hooks to run when no clause returns :deny.")
        (assert-true (null (cdar results))
                     "Expected first high-priority hook to not match harmless command.")
        (assert-true (eq (cdadr results) :allow)
                     "Expected catch-all pre-tool-use hook to allow operation."))

      (funcall clear-hooks-fn :pre-tool-use)
      (eval `(,defhook-sym pre-tool-use (tool-name args)
               "Path matcher for env files."
               (:priority 100)
               (:match (:tool "write-file" :args (:path "*.env"))
                 :deny)))

      (multiple-value-bind (decision results)
          (funcall run-hooks-fn
                   :pre-tool-use
                   "write-file"
                   (make-args "path" ".env"))
        (assert-true (eq decision :deny)
                     "Expected :args :path glob matcher to deny .env writes.")
        (assert-true (= (length results) 1)
                     "Expected single path hook result for write-file test."))

      (funcall clear-hooks-fn)
      (eval `(,defhook-sym post-tool-use (tool-name result elapsed-ms)
               "Post hook one."
               (:priority 100)
               (:match t
                 :deny)))
      (eval `(,defhook-sym post-tool-use (tool-name result elapsed-ms)
               "Post hook two."
               (:priority 10)
               (:match t
                 :ok)))

      (multiple-value-bind (decision results)
          (funcall run-hooks-fn :post-tool-use "bash" :ignored 4)
        (assert-true (eq decision :completed)
                     "Expected non-blocking hook-point to always complete.")
        (assert-true (= (length results) 2)
                     "Expected all non-blocking hooks to run.")
        (assert-true (eq (cdar results) :deny)
                     "Expected first post hook return value to be preserved.")
        (assert-true (eq (cdadr results) :ok)
                     "Expected second post hook return value to be preserved."))

      (let* ((post-hooks (funcall list-hooks-fn :post-tool-use))
             (first-priority (funcall hook-entry-priority-fn (first post-hooks)))
             (second-priority (funcall hook-entry-priority-fn (second post-hooks))))
        (assert-true (= first-priority 100)
                     "Expected post-tool-use hooks sorted by descending priority.")
        (assert-true (= second-priority 10)
                     "Expected lower priority hook to appear later."))))

  (format t "AMOEBUM_DEFHOOK_SMOKE_OK~%"))
