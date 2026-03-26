(in-package :amoebum)

(defun %extensions-usage ()
  "/extensions [list|reload|enable <all|name|path>|disable <all|name|path>]")

(defun %extension-scope-label (scope)
  (case scope
    (:global "global")
    (:project "project")
    (otherwise (string-downcase (symbol-name scope)))))

(defun %extension-status-label (status)
  (case status
    (:loaded "loaded")
    (:error "error")
    (:disabled "disabled")
    (otherwise (string-downcase (symbol-name status)))))

(defun %render-extensions-list ()
  (let* ((report (list-extension-report))
         (summary (extension-report-summary report))
         (extensions (list-extensions)))
    (if (null extensions)
        "No extension scan has run yet. Use /extensions reload."
        (with-output-to-string (out)
          (format out "Extensions: total=~D loaded=~D errors=~D disabled=~D~%"
                  (getf summary :total 0)
                  (getf summary :loaded 0)
                  (getf summary :errors 0)
                  (getf summary :disabled 0))
          (dolist (entry extensions)
            (let ((status (getf entry :status))
                  (scope (getf entry :scope))
                  (name (getf entry :name))
                  (version (or (getf entry :version) "0.0.0"))
                  (tool-count (or (getf entry :tool-count) 0))
                  (hook-count (or (getf entry :hook-count) 0))
                  (path (or (getf entry :path) (getf entry :entry-point)))
                  (message (getf entry :message)))
              (format out "- [~A/~A] ~A~@[ v~A~] tools=~D hooks=~D~@[ path=~A~]~@[ -- ~A~]~%"
                      (%extension-status-label status)
                      (%extension-scope-label scope)
                      (or name "<unnamed>")
                      version
                      tool-count
                      hook-count
                      path
                      message)))))))

(defun %extensions-join (tokens)
  (with-output-to-string (out)
    (loop for token in tokens
          for index from 0 do
            (when (> index 0)
              (write-char #\Space out))
            (write-string token out))))

(defun %extensions-known-targets ()
  (let ((targets (copy-list (known-user-extension-paths))))
    (when (null targets)
      (multiple-value-bind (global project)
          (discover-user-extension-files)
        (setf targets
              (append (mapcar #'namestring global)
                      (mapcar #'namestring project)))))
    (setf targets (append targets (known-user-extension-names)))
    (let ((seen (make-hash-table :test #'equal))
          (result '()))
      (labels ((remember (value)
                 (let ((trimmed (%slash-trim value)))
                   (when (plusp (length trimmed))
                     (let ((key (string-downcase trimmed)))
                       (unless (gethash key seen)
                         (setf (gethash key seen) t)
                         (push trimmed result)))))))
        (dolist (target targets)
          (remember target)
          (remember (file-namestring (pathname target)))))
      (nreverse result))))

(defun %extensions-matching-target (target &optional (extensions (list-extensions)))
  (let ((needle (%slash-trim target)))
    (if (or (%slash-blank-p needle)
            (string-equal needle "all"))
        extensions
        (remove-if-not
         (lambda (entry)
           (or (%extension-match-target-p needle (or (getf entry :name) ""))
               (%extension-match-target-p needle (or (getf entry :path) ""))
               (%extension-match-target-p needle (or (getf entry :entry-point) ""))
               (%extension-match-target-p needle (or (getf entry :manifest-path) ""))))
         extensions))))

(defun %count-extensions-by-status (extensions status)
  (count status extensions :key (lambda (entry) (getf entry :status)) :test #'eq))

(defun %extension-command-project-root (context)
  (let ((cfg (or (slash-command-context-config context)
                 (%current-config-safe))))
    (and (config-p cfg)
         (config-project-root cfg))))

(defun %extensions-invalid-usage (&optional details)
  (make-slash-command-result
   :echo-input-p t
   :output (format nil "~@[~A~%~]Usage: ~A"
                   details
                   (%extensions-usage))))

(defun %extensions-enable-disable-result (verb target paths count miss-message)
  (if (zerop count)
      (make-slash-command-result
       :echo-input-p t
       :output (format nil miss-message target))
      (make-slash-command-result
       :echo-input-p t
       :output (format nil "~A ~D extension(s). Reload to apply.~%~{~A~%~}"
                       verb
                       count
                       paths))))

(defun %extensions-handle-list (tokens)
  (if (> (length tokens) 1)
      (%extensions-invalid-usage (format nil "Unexpected argument ~S." (second tokens)))
      (make-slash-command-result
       :echo-input-p t
       :output (%render-extensions-list))))

(defun %extensions-handle-reload (tokens context)
  (if (> (length tokens) 1)
      (%extensions-invalid-usage (format nil "Unexpected argument ~S." (second tokens)))
      (let* ((project-root (%extension-command-project-root context))
             (report (reload-user-extensions :project-root project-root))
             (summary (extension-report-summary report)))
        (make-slash-command-result
         :echo-input-p t
         :output (format nil "Reloaded extensions: loaded=~D errors=~D disabled=~D."
                         (getf summary :loaded 0)
                         (getf summary :errors 0)
                         (getf summary :disabled 0))))))

(defun %extensions-handle-disable (tokens)
  (let ((target (%extensions-join (rest tokens))))
    (if (%slash-blank-p target)
        (%extensions-invalid-usage "Specify extension target or 'all'.")
        (multiple-value-bind (disabled-paths disabled-count)
            (disable-user-extension target)
          (%extensions-enable-disable-result "Disabled"
                                             target
                                             disabled-paths
                                             disabled-count
                                             "No extensions matched ~S.")))))

(defun %extensions-handle-enable (tokens)
  (let ((target (%extensions-join (rest tokens))))
    (if (%slash-blank-p target)
        (%extensions-invalid-usage "Specify extension target or 'all'.")
        (multiple-value-bind (enabled-paths enabled-count)
            (enable-user-extension target)
          (%extensions-enable-disable-result "Enabled"
                                             target
                                             enabled-paths
                                             enabled-count
                                             "No disabled extensions matched ~S.")))))

(defun %extensions-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((raw (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments raw))
         (action-token (if tokens
                           (string-downcase (first tokens))
                           "list")))
    (cond
      ((member action-token '("list" "ls") :test #'string=)
       (%extensions-handle-list tokens))
      ((string= action-token "reload")
       (%extensions-handle-reload tokens context))
      ((string= action-token "disable")
       (%extensions-handle-disable tokens))
      ((string= action-token "enable")
       (%extensions-handle-enable tokens))
      (t
       (%extensions-invalid-usage (format nil "Unknown /extensions action ~S." action-token))))))

(defun %ext-load-usage ()
  "/ext-load <all|name|path>")

(defun %ext-unload-usage ()
  "/ext-unload <all|name|path>")

(defun %ext-reload-usage ()
  "/ext-reload [all|name|path]")

(defun %extension-command-target-summary (verb target matched)
  (let ((loaded-count (%count-extensions-by-status matched :loaded))
        (error-count (%count-extensions-by-status matched :error))
        (disabled-count (%count-extensions-by-status matched :disabled)))
    (make-slash-command-result
     :echo-input-p t
     :output (format nil "~A ~S: matched=~D loaded=~D errors=~D disabled=~D."
                     verb
                     target
                     (length matched)
                     loaded-count
                     error-count
                     disabled-count))))

(defun %extension-command-invalid-target (usage)
  (make-slash-command-result
   :echo-input-p t
   :output (format nil "Usage: ~A" usage)))

(defun %ext-load-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let ((target (%slash-trim (or (gethash :TARGET arguments) ""))))
    (if (%slash-blank-p target)
        (%extension-command-invalid-target (%ext-load-usage))
        (progn
          (enable-user-extension target)
          (reload-user-extensions :project-root (%extension-command-project-root context))
          (let ((matched (%extensions-matching-target target)))
            (if (null matched)
                (make-slash-command-result
                 :echo-input-p t
                 :output (format nil "No extensions matched ~S." target))
                (%extension-command-target-summary "ext-load" target matched)))))))

(defun %ext-unload-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let ((target (%slash-trim (or (gethash :TARGET arguments) ""))))
    (if (%slash-blank-p target)
        (%extension-command-invalid-target (%ext-unload-usage))
        (progn
          (disable-user-extension target)
          (reload-user-extensions :project-root (%extension-command-project-root context))
          (let ((matched (%extensions-matching-target target)))
            (if (null matched)
                (make-slash-command-result
                 :echo-input-p t
                 :output (format nil "No extensions matched ~S." target))
                (%extension-command-target-summary "ext-unload" target matched)))))))

(defun %ext-reload-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((target (%slash-trim (or (gethash :TARGET arguments) "")))
         (target-label (if (%slash-blank-p target) "all" target)))
    (reload-user-extensions :project-root (%extension-command-project-root context))
    (let ((matched (%extensions-matching-target target)))
      (if (null matched)
          (make-slash-command-result
           :echo-input-p t
           :output (format nil "No extensions matched ~S." target-label))
          (%extension-command-target-summary "ext-reload" target-label matched)))))

(defun %extensions-arg-completer (_command _invocation index fragment prefix-tokens)
  (declare (ignore _command _invocation))
  (let ((head (and prefix-tokens (string-downcase (first prefix-tokens))))
        (prefix (%slash-trim fragment)))
    (cond
      ((= index 0)
       (loop for option in '("list" "reload" "enable" "disable")
             when (%starts-with-ci-p prefix option)
               collect option))
      ((and (member head '("enable" "disable") :test #'string=) (= index 1))
       (let ((targets (append '("all") (%extensions-known-targets))))
         (loop for option in targets
               when (%starts-with-ci-p prefix option)
                 collect option)))
      (t
       nil))))

(defun %ext-command-target-completions (fragment)
  (let ((prefix (%slash-trim fragment))
        (targets (append '("all") (%extensions-known-targets))))
    (loop for option in targets
          when (%starts-with-ci-p prefix option)
            collect option)))

(defun %ext-load-arg-completer (_command _invocation index fragment _prefix-tokens)
  (declare (ignore _command _invocation _prefix-tokens))
  (and (= index 0)
       (%ext-command-target-completions fragment)))

(defun %ext-unload-arg-completer (_command _invocation index fragment _prefix-tokens)
  (declare (ignore _command _invocation _prefix-tokens))
  (and (= index 0)
       (%ext-command-target-completions fragment)))

(defun %ext-reload-arg-completer (_command _invocation index fragment _prefix-tokens)
  (declare (ignore _command _invocation _prefix-tokens))
  (and (= index 0)
       (%ext-command-target-completions fragment)))

(defun register-extension-slash-commands ()
  (register-slash-command
   (make-slash-command
    :name "extensions"
    :description "List, reload, enable, or disable user extensions."
    :usage (%extensions-usage)
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional subcommand and target."))
    :handler #'%extensions-handler
    :completer #'%extensions-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "ext-load"
    :description "Load one or more user extensions without restarting the image."
    :usage (%ext-load-usage)
    :parameters
    (list (make-slash-command-parameter
           :name "target"
           :type :string
           :required-p t
           :greedy-p t
           :description "Extension name/path or all."))
    :handler #'%ext-load-handler
    :completer #'%ext-load-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "ext-unload"
    :description "Unload (disable) one or more user extensions without restarting the image."
    :usage (%ext-unload-usage)
    :parameters
    (list (make-slash-command-parameter
           :name "target"
           :type :string
           :required-p t
           :greedy-p t
           :description "Extension name/path or all."))
    :handler #'%ext-unload-handler
    :completer #'%ext-unload-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "ext-reload"
    :description "Reload all user extensions or a matched target."
    :usage (%ext-reload-usage)
    :parameters
    (list (make-slash-command-parameter
           :name "target"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional extension name/path; defaults to all."))
    :handler #'%ext-reload-handler
    :completer #'%ext-reload-arg-completer))
  t)
