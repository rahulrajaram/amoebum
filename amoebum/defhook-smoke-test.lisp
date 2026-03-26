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
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn
           (lambda (name)
             (symbol-function (funcall symbol-in name amoebum-pkg))))
         (clear-hooks-fn (funcall fn "CLEAR-HOOKS"))
         (list-hooks-fn (funcall fn "LIST-HOOKS"))
         (register-hook-fn (funcall fn "REGISTER-HOOK"))
         (run-hooks-fn (funcall fn "RUN-HOOKS"))
         (hook-metrics-fn (funcall fn "HOOK-METRICS"))
         (hook-trace-fn (funcall fn "HOOK-TRACE"))
         (clear-hook-trace-fn (funcall fn "CLEAR-HOOK-TRACE"))
         (dispatch-command-fn (funcall fn "DISPATCH-SLASH-COMMAND"))
         (result-output-fn (funcall fn "SLASH-COMMAND-RESULT-OUTPUT"))
         (hook-entry-priority-fn (funcall fn "HOOK-ENTRY-PRIORITY"))
         (make-chat-ui-state-fn (funcall fn "MAKE-CHAT-UI-STATE"))
         (chat-ui-state-hook-warnings-fn
           (funcall fn "CHAT-UI-STATE-HOOK-WARNINGS"))
         (chat-ui-state-var (funcall symbol-in "*CHAT-UI-STATE*" amoebum-pkg))
         (hook-registry-sym (funcall symbol-in "*HOOK-REGISTRY*" amoebum-pkg))
         (defhook-sym (funcall symbol-in "DEFHOOK" amoebum-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (contains-text-p (haystack needle)
               (and (stringp haystack)
                    (search needle haystack :test #'char-equal)))
             (make-args (&rest pairs)
               (let ((table (make-hash-table :test #'equal)))
                 (loop for (key value) on pairs by #'cddr
                       do (setf (gethash key table) value))
                 table)))
      (funcall clear-hooks-fn)
      (funcall clear-hook-trace-fn)

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
                     "Expected lower priority hook to appear later."))

      (funcall clear-hooks-fn)
      (funcall clear-hook-trace-fn)
      (let ((reentrant-count 0))
        (funcall register-hook-fn
                 :on-idle
                 'i67-reentrant-guard-hook
                 (lambda ()
                   (incf reentrant-count)
                   (funcall run-hooks-fn :on-idle)
                   :ok))
        #+sbcl
        (sb-ext:with-timeout 1
          (multiple-value-bind (decision results)
              (funcall run-hooks-fn :on-idle)
            (assert-true (eq decision :completed)
                         "Expected re-entrant guarded on-idle chain to complete.")
            (assert-true (eq (cdar results) :ok)
                         "Expected outer re-entrant guard hook to return :ok.")))
        #-sbcl
        (multiple-value-bind (decision results)
            (funcall run-hooks-fn :on-idle)
          (assert-true (eq decision :completed)
                       "Expected re-entrant guarded on-idle chain to complete.")
          (assert-true (eq (cdar results) :ok)
                       "Expected outer re-entrant guard hook to return :ok."))
        (assert-true (= reentrant-count 1)
                     "Expected re-entrant guard to skip nested self-invocation."))

      (funcall clear-hooks-fn)
      (funcall clear-hook-trace-fn)
      (funcall register-hook-fn
               :pre-tool-use
               'i67-budget-timeout-hook
               (lambda (tool-name args)
                 (declare (ignore tool-name args))
                 (sleep 0.05)
                 :allow)
               :max-ms 5
               :on-error :log-and-continue
               :failure-threshold 3)
      (multiple-value-bind (decision results)
          (funcall run-hooks-fn :pre-tool-use "bash" (make-args "command" "echo ok"))
        (assert-true (eq decision :allow)
                     "Expected timeout policy :log-and-continue to preserve :allow decision.")
        (assert-true (eq (cdar results) :hook-timeout)
                     "Expected timeout result marker from budget-enforced hook."))
      (let* ((metrics (funcall hook-metrics-fn :pre-tool-use 'i67-budget-timeout-hook))
             (entry (first metrics)))
        (assert-true entry
                     "Expected hook-metrics entry for budget timeout hook.")
        (assert-true (= (getf entry :failure-count) 1)
                     "Expected failure-count=1 after timeout, got ~S."
                     (and entry (getf entry :failure-count)))
        (assert-true (= (getf entry :timeout-count) 1)
                     "Expected timeout-count=1 after timeout, got ~S."
                     (and entry (getf entry :timeout-count))))

      (funcall clear-hooks-fn)
      (funcall clear-hook-trace-fn)
      (let ((failing-count 0))
        (funcall register-hook-fn
                 :on-idle
                 'i67-circuit-breaker-hook
                 (lambda ()
                   (incf failing-count)
                   (error "i67 forced failure"))
                 :on-error :log-and-continue
                 :failure-threshold 2)
        (multiple-value-bind (decision first-results)
            (funcall run-hooks-fn :on-idle)
          (assert-true (eq decision :completed)
                       "Expected on-idle hook chain to continue on first failure.")
          (assert-true (eq (cdar first-results) :hook-error)
                       "Expected first failing hook run to return :hook-error marker."))
        (multiple-value-bind (decision second-results)
            (funcall run-hooks-fn :on-idle)
          (assert-true (eq decision :completed)
                       "Expected on-idle hook chain to continue on second failure.")
          (assert-true (eq (cdar second-results) :hook-error)
                       "Expected second failing hook run to return :hook-error marker."))
        (multiple-value-bind (decision third-results)
            (funcall run-hooks-fn :on-idle)
          (assert-true (eq decision :completed)
                       "Expected disabled hook run to complete.")
          (assert-true (eq (cdar third-results) :disabled)
                       "Expected circuit-breaker to disable hook after threshold failures."))
        (assert-true (= failing-count 2)
                     "Expected circuit-breaker to stop executing hook after second failure.")
        (let* ((metrics (funcall hook-metrics-fn :on-idle 'i67-circuit-breaker-hook))
               (entry (first metrics)))
          (assert-true entry "Expected metrics for circuit-breaker hook.")
          (assert-true (not (getf entry :enabled-p))
                       "Expected circuit-breaker hook to be disabled.")
          (assert-true (= (getf entry :failure-count) 2)
                       "Expected two recorded failures before disable, got ~S."
                       (and entry (getf entry :failure-count)))))

      (funcall clear-hooks-fn)
      (let ((previous-chat-state (and (boundp chat-ui-state-var)
                                      (symbol-value chat-ui-state-var)))
            (chat-state (funcall make-chat-ui-state-fn)))
        (unwind-protect
             (progn
               (setf (symbol-value chat-ui-state-var) chat-state)
               (funcall register-hook-fn
                        :post-tool-use
                        'i67-warning-capture-hook
                        (lambda (tool-name result elapsed-ms)
                          (declare (ignore tool-name result elapsed-ms))
                          (error "i67 captured warning"))
                        :on-error :log-and-continue)
               (multiple-value-bind (decision results)
                   (funcall run-hooks-fn :post-tool-use "bash" :ignored 1)
                 (assert-true (eq decision :completed)
                              "Expected warning-capture hook failure to remain non-blocking.")
                 (assert-true (eq (cdar results) :hook-error)
                              "Expected warning-capture hook to report :hook-error."))
               (let ((warnings (funcall chat-ui-state-hook-warnings-fn chat-state)))
                 (assert-true (= (length warnings) 1)
                              "Expected one captured hook warning, got ~S."
                              warnings)
                 (assert-true (contains-text-p (first warnings) "i67 captured warning")
                              "Expected captured hook warning text, got ~S."
                              warnings)))
          (setf (symbol-value chat-ui-state-var) previous-chat-state)))

      (multiple-value-bind (handledp list-result)
          (funcall dispatch-command-fn "/hooks")
        (assert-true handledp "Expected /hooks command to be handled.")
        (assert-true (contains-text-p (funcall result-output-fn list-result) "Registered hooks")
                     "Expected /hooks list output to include registry header."))
      (multiple-value-bind (handledp trace-result)
          (funcall dispatch-command-fn "/hooks trace 5")
        (assert-true handledp "Expected /hooks trace command to be handled.")
        (assert-true (contains-text-p (funcall result-output-fn trace-result) "Hook trace")
                     "Expected /hooks trace output to include trace header."))))

  (format t "AMOEBUM_DEFHOOK_SMOKE_OK~%"))
