(in-package :amoebum)

(defparameter *pipeline-start-time-ms* nil)
(defparameter *pipeline-current-result* nil)
(defparameter *pipeline-current-tool-name* nil)
(defparameter *pipeline-current-arguments* nil)
(defparameter *pipeline-current-request-id* nil)

(defclass tool-execution-context ()
  ((toolset :initarg :toolset
            :initform *toolset*
            :accessor context-toolset)
   (permission-mode :initarg :permission-mode
                    :initform nil
                    :accessor context-permission-mode)
   (event-bus :initarg :event-bus
              :initform nil
              :accessor context-event-bus)
   (hook-registry :initarg :hook-registry
                  :initform nil
                  :accessor context-hook-registry)
   (metrics :initarg :metrics
            :initform (make-hash-table :test #'equal)
            :accessor context-metrics)
   (result-cache :initarg :result-cache
                 :initform (make-hash-table :test #'equal)
                 :accessor context-result-cache)
   (logger :initarg :logger
           :initform nil
           :accessor context-logger))
  (:documentation "Execution context for execute-tool method combinations."))

(defclass amoebum-context (tool-execution-context) ()
  (:documentation "Default amoebum tool execution context."))

(defgeneric execute-tool (tool-call context)
  (:documentation "Execute TOOL-CALL in CONTEXT via method-combination pipeline."))

(defun make-amoebum-context (&key
                               (toolset *toolset*)
                               permission-mode
                               event-bus
                               hook-registry
                               metrics
                               result-cache
                               logger
                               (initialize-notifications-p t))
  (let ((context
          (make-instance 'amoebum-context
                         :toolset toolset
                         :permission-mode permission-mode
                         :event-bus event-bus
                         :hook-registry hook-registry
                         :metrics (or metrics (make-hash-table :test #'equal))
                         :result-cache (or result-cache (make-hash-table :test #'equal))
                         :logger logger)))
    (when initialize-notifications-p
      (ensure-notification-manager :event-bus (or event-bus (current-event-bus))))
    context))

(defun %pipeline-normalize-tool-name (tool-name)
  (string-downcase
   (string-trim '(#\Space #\Tab #\Newline #\Return)
                (if (symbolp tool-name)
                    (symbol-name tool-name)
                    (princ-to-string tool-name)))))

(defun %tool-call-name-string (call)
  (%pipeline-normalize-tool-name (pseudopod:tool-call-name call)))

(defun %tool-call-request-id (call)
  (let ((id (pseudopod:tool-call-id call)))
    (when id
      (princ-to-string id))))

(defun %make-equal-hash-table ()
  (make-hash-table :test #'equal))

(defun %copy-arguments-to-hash-table (arguments)
  (let ((table (%make-equal-hash-table)))
    (cond
      ((null arguments)
       table)
      ((hash-table-p arguments)
       (maphash (lambda (key value)
                  (setf (gethash key table) value))
                arguments)
       table)
      ((and (listp arguments)
            (every #'consp arguments))
       (dolist (pair arguments table)
         (setf (gethash (car pair) table) (cdr pair))))
      ((listp arguments)
       (unless (evenp (length arguments))
         (error "Tool argument plist must have even length, got ~S." arguments))
       (loop for (key value) on arguments by #'cddr do
             (setf (gethash key table) value))
       table)
      (t
       (error "Unsupported tool argument payload: ~S." arguments)))))

(defun %parse-json-arguments (json-text)
  (let* ((jonathan-package (find-package :jonathan))
         (parse-symbol (and jonathan-package
                            (find-symbol "PARSE" jonathan-package))))
    (unless (and parse-symbol (fboundp parse-symbol))
      (error "JSON parser unavailable for tool argument payload."))
    (funcall (symbol-function parse-symbol) json-text :as :hash-table)))

(defun %encode-json-arguments (arguments)
  (let* ((jonathan-package (find-package :jonathan))
         (to-json-symbol (and jonathan-package
                              (find-symbol "TO-JSON" jonathan-package))))
    (unless (and to-json-symbol (fboundp to-json-symbol))
      (error "JSON encoder unavailable for tool argument payload."))
    (funcall (symbol-function to-json-symbol) arguments)))

(defun %decode-tool-call-arguments (call)
  (let ((arguments (pseudopod:tool-call-arguments call))
        (tool-name (%tool-call-name-string call)))
    (handler-case
        (cond
          ((or (null arguments)
               (hash-table-p arguments)
               (listp arguments))
           (%copy-arguments-to-hash-table arguments))
          ((stringp arguments)
           (let ((parsed (%parse-json-arguments arguments)))
             (%copy-arguments-to-hash-table parsed)))
          (t
           (error "Unsupported tool argument payload type ~S."
                  (type-of arguments))))
      (error (condition)
        (error 'tool-argument-error
               :tool-name tool-name
               :arguments (if (hash-table-p arguments) arguments nil)
               :message (princ-to-string condition)
               :reason (princ-to-string condition)
               :cause condition)))))

(defun %call-arguments (call)
  (or *pipeline-current-arguments*
      (%decode-tool-call-arguments call)))

(defun %pipeline-tool-metadata-for (tool-name)
  (and (boundp '*tool-metadata*)
       (hash-table-p *tool-metadata*)
       (gethash (%pipeline-normalize-tool-name tool-name) *tool-metadata*)))

(defun %metadata-timeout-seconds (tool-name)
  (let ((metadata (%pipeline-tool-metadata-for tool-name)))
    (and metadata
         (tool-metadata-timeout-seconds metadata))))

(defun %argument-key-candidates (key-name)
  (let* ((raw (if (symbolp key-name)
                  (symbol-name key-name)
                  (princ-to-string key-name)))
         (lower (string-downcase raw))
         (upper (string-upcase raw)))
    (list lower upper raw (intern upper :keyword))))

(defun %argument-present-p (arguments key-name)
  (loop for candidate in (%argument-key-candidates key-name)
        thereis (nth-value 1 (gethash candidate arguments))))

(defun %argument-value (arguments key-name)
  (loop for candidate in (%argument-key-candidates key-name)
        do (multiple-value-bind (value present-p)
               (gethash candidate arguments)
             (when present-p
               (return value)))
        finally (return nil)))

(defun %member-argument-match-p (value members)
  (or (member value members :test #'equal)
      (and (stringp value)
           (loop for candidate in members
                 thereis (string= value
                                  (string-downcase
                                   (if (symbolp candidate)
                                       (symbol-name candidate)
                                       (princ-to-string candidate))))))))

(defun %argument-type-ok-p (value type-spec)
  (cond
    ((equal type-spec 't) t)
    ((and (consp type-spec) (eq (first type-spec) 'or))
     (some (lambda (branch)
             (%argument-type-ok-p value branch))
           (rest type-spec)))
    ((equal type-spec 'null) (null value))
    ((equal type-spec 'pathname)
     (or (pathnamep value) (stringp value)))
    ((equal type-spec 'integer)
     (or (integerp value)
         (and (stringp value)
              (ignore-errors
                (parse-integer value)
                t))))
    ((equal type-spec 'boolean)
     (or (eq value t)
         (eq value nil)
         (stringp value)
         (numberp value)))
    ((and (consp type-spec) (eq (first type-spec) 'member))
     (%member-argument-match-p value (rest type-spec)))
    (t
     (ignore-errors (typep value type-spec)))))

(defun %validate-tool-arguments (tool-name arguments)
  (let ((metadata (%pipeline-tool-metadata-for tool-name)))
    (when metadata
      (dolist (parameter (tool-metadata-parameter-specs metadata))
        (let* ((name (getf parameter :name))
               (type-spec (getf parameter :type))
               (required-p (not (null (getf parameter :required))))
               (present-p (%argument-present-p arguments name))
               (value (%argument-value arguments name)))
          (when (and required-p (not present-p))
            (error 'tool-missing-argument
                   :tool-name tool-name
                   :arguments arguments
                   :argument-name name
                   :message (format nil "Missing required argument ~S." name)
                   :reason "missing required argument"))
          (when (and present-p
                     (not (%argument-type-ok-p value type-spec)))
            (error 'tool-type-mismatch
                   :tool-name tool-name
                   :arguments arguments
                   :argument-name name
                   :message (format nil "Argument ~S failed type validation." name)
                   :reason (format nil "expected type ~S" type-spec))))))))

(defun %extract-path-argument (arguments)
  (or (%argument-value arguments "path")
      (%argument-value arguments "file")
      (%argument-value arguments "target")))

(defun %extract-command-argument (arguments)
  (or (%argument-value arguments "command")
      (%argument-value arguments "cmd")))

(defun %coerce-path-string (value)
  (typecase value
    (null nil)
    (pathname (namestring value))
    (string value)
    (symbol (symbol-name value))
    (t (princ-to-string value))))

(defun %coerce-command-string (value)
  (typecase value
    (null nil)
    (string value)
    (symbol (symbol-name value))
    (pathname (namestring value))
    (t (princ-to-string value))))

(defun %effective-event-bus (context)
  (or (context-event-bus context)
      (current-event-bus)))

(defun %context-effective-permission-mode (context)
  (%effective-permission-mode (context-permission-mode context)))

(defun %publish-permission-prompted (context tool-name arguments decision
                                   &key reason reason-code)
  (publish (%effective-event-bus context)
           (make-permission-prompted-event
            :tool-name tool-name
            :path (%coerce-path-string (%extract-path-argument arguments))
            :command (%coerce-command-string (%extract-command-argument arguments))
            :reason (or reason (format nil "permission decision ~A" decision))
            :reason-code reason-code
            :permission-mode (%context-effective-permission-mode context))))

(defun %publish-permission-blocked (context tool-name arguments decision
                                  &key reason actionable-reason reason-code)
  (let ((resolved-reason (or reason (format nil "permission decision ~A" decision))))
    (publish (%effective-event-bus context)
             (make-permission-blocked-event
              :tool-name tool-name
              :path (%coerce-path-string (%extract-path-argument arguments))
              :command (%coerce-command-string (%extract-command-argument arguments))
              :reason resolved-reason
              :actionable-reason (or actionable-reason resolved-reason)
              :reason-code reason-code
              :permission-mode (%context-effective-permission-mode context)))))

(defun %check-permission-or-signal (tool-name arguments context)
  (let* ((effective-mode (%context-effective-permission-mode context))
         (metadata (%pipeline-tool-metadata-for tool-name))
         (dangerous-p (and metadata (tool-metadata-dangerous-p metadata)))
         (decision (check-permission :tool tool-name
                                     :path (%coerce-path-string
                                            (%extract-path-argument arguments))
                                     :command (%coerce-command-string
                                               (%extract-command-argument arguments))
                                     :dangerous-p dangerous-p
                                     :permission-mode effective-mode))
         (trace (last-permission-decision-trace))
         (reason-code (and (listp trace) (getf trace :reason-code)))
         (decision-reason (or (and (listp trace) (getf trace :reason))
                              (format nil "permission decision ~A" decision)))
         (actionable-reason (or (and (listp trace) (getf trace :actionable-reason))
                                decision-reason)))
    (unless (eq decision :allow)
      (when (eq decision :prompt)
        (%publish-permission-prompted context
                                      tool-name
                                      arguments
                                      decision
                                      :reason decision-reason
                                      :reason-code reason-code))
      (when (eq decision :deny)
        (%publish-permission-blocked context
                                     tool-name
                                     arguments
                                     decision
                                     :reason decision-reason
                                     :actionable-reason actionable-reason
                                     :reason-code reason-code))
      (error 'tool-permission-denied
             :tool-name tool-name
             :arguments arguments
             :reason-code reason-code
             :message (format nil "Permission decision ~A for tool ~S: ~A."
                              decision
                              tool-name
                              actionable-reason)
             :reason actionable-reason))))

(defun %pipeline-monotonic-milliseconds ()
  (truncate (* 1000
               (/ (coerce (get-internal-real-time) 'double-float)
                  (coerce internal-time-units-per-second 'double-float)))))

(defun %elapsed-milliseconds ()
  (if *pipeline-start-time-ms*
      (max 0 (- (%pipeline-monotonic-milliseconds) *pipeline-start-time-ms*))
      0))

(defun %ensure-tool-registered (context tool-name arguments)
  (unless (pseudopod:find-tool (context-toolset context) tool-name)
    (error 'tool-not-found
           :tool-name tool-name
           :arguments arguments
           :message (format nil "No registered tool named ~S." tool-name)
           :reason "tool not found")))

(defun %maybe-log-invocation (context tool-name arguments)
  (let ((logger (context-logger context)))
    (when (functionp logger)
      (funcall logger
               :tool-invoked
               :tool-name tool-name
               :arguments arguments
               :permission-mode (%context-effective-permission-mode context)))))

(defun %run-hook-dispatch (context hook-point &rest args)
  (let ((*hook-registry* (or (context-hook-registry context)
                             *hook-registry*)))
    (apply #'run-hooks hook-point args)))

(defun %record-tool-metrics (context tool-name elapsed-ms status)
  (let* ((table (context-metrics context))
         (key (%pipeline-normalize-tool-name tool-name))
         (entry (or (gethash key table)
                    (list :count 0 :error-count 0 :total-ms 0 :last-ms 0 :last-status :ok))))
    (incf (getf entry :count 0))
    (incf (getf entry :total-ms 0) elapsed-ms)
    (setf (getf entry :last-ms) elapsed-ms
          (getf entry :last-status) status)
    (when (eq status :error)
      (incf (getf entry :error-count 0)))
    (setf (gethash key table) entry)
    entry))

(defun %arguments-cache-signature (arguments)
  (with-output-to-string (stream)
    (let ((pairs
            (sort
             (loop for key being the hash-keys of arguments using (hash-value value)
                   collect (cons (string-downcase (princ-to-string key))
                                 (prin1-to-string value)))
             #'string<
             :key #'car)))
      (dolist (pair pairs)
        (format stream "~A=~A;" (car pair) (cdr pair))))))

(defun %cache-key (tool-name arguments)
  (list (%pipeline-normalize-tool-name tool-name)
        (%arguments-cache-signature arguments)))

(defun %cache-tool-result (context tool-name arguments result)
  (setf (gethash (%cache-key tool-name arguments)
                 (context-result-cache context))
        result))

(defun context-tool-metrics (context tool-name)
  (copy-list (gethash (%pipeline-normalize-tool-name tool-name)
                      (context-metrics context))))

(defun cached-tool-result (context tool-name arguments)
  (gethash (%cache-key tool-name (%copy-arguments-to-hash-table arguments))
           (context-result-cache context)))

(defun clear-tool-result-cache (context)
  (clrhash (context-result-cache context)))

(defun clear-tool-metrics (context)
  (clrhash (context-metrics context)))

(defun %coerce-tool-error (tool-name arguments condition timeout-seconds)
  (cond
    ((typep condition 'tool-error)
     condition)
    #+sbcl
    ((typep condition 'sb-ext:timeout)
     (make-condition 'tool-timeout
                     :tool-name tool-name
                     :arguments arguments
                     :timeout-seconds timeout-seconds
                     :message (format nil "Tool ~S timed out." tool-name)
                     :reason (format nil "timed out after ~A seconds" timeout-seconds)
                     :cause condition))
    (t
     (make-condition 'tool-execution-error
                     :tool-name tool-name
                     :arguments arguments
                     :message (princ-to-string condition)
                     :reason (princ-to-string condition)
                     :cause condition))))

(defun %clone-tool-call-with-arguments (call arguments)
  (pseudopod:make-tool-call
   :id (pseudopod:tool-call-id call)
   :name (pseudopod:tool-call-name call)
   :arguments (%encode-json-arguments
               (%copy-arguments-to-hash-table arguments))
   :extras (pseudopod:tool-call-extras call)))

(defmethod execute-tool :around ((call pseudopod:tool-call)
                                 (context tool-execution-context))
  (let* ((*pipeline-current-tool-name* (%tool-call-name-string call))
         (*pipeline-current-arguments* (%decode-tool-call-arguments call))
         (*pipeline-current-request-id* (%tool-call-request-id call))
         (*pipeline-start-time-ms* nil)
         (*pipeline-current-result* nil)
         (timeout-seconds (%metadata-timeout-seconds *pipeline-current-tool-name*)))
(restart-case
        (handler-case
            #+sbcl
            (if (and timeout-seconds (> timeout-seconds 0))
                (sb-ext:with-timeout timeout-seconds
                  (call-next-method))
                (call-next-method))
            #-sbcl
            (call-next-method)
          (error (condition)
            (let* ((tool-error
                     (%coerce-tool-error *pipeline-current-tool-name*
                                         *pipeline-current-arguments*
                                         condition
                                         timeout-seconds))
                   (elapsed-ms (%elapsed-milliseconds)))
              (%record-tool-metrics context *pipeline-current-tool-name* elapsed-ms :error)
                     (publish (%effective-event-bus context)
                       (make-tool-error-event
                        :tool-name *pipeline-current-tool-name*
                        :args *pipeline-current-arguments*
                        :condition-reason-code (tool-error-reason-code tool-error)
                        :condition (princ-to-string tool-error)
                        :elapsed-ms elapsed-ms
                        :request-id *pipeline-current-request-id*))
              (error tool-error))))
      (retry-tool (&optional (new-arguments *pipeline-current-arguments*))
        :report "Retry tool execution."
        (execute-tool (%clone-tool-call-with-arguments call new-arguments)
                      context))
      (skip-tool ()
        :report "Skip this tool and continue."
        (format nil "Tool ~A skipped by pipeline restart." *pipeline-current-tool-name*))
      (use-value (value)
        :report "Provide a replacement value."
        value)
      (abort-tool ()
        :report "Abort tool execution and propagate failure."
        (error 'amoebum-error
               :message (format nil "Tool ~A aborted by pipeline restart."
                                *pipeline-current-tool-name*))))))

(defmethod execute-tool :before ((call pseudopod:tool-call)
                                 (context tool-execution-context))
  (let* ((tool-name *pipeline-current-tool-name*)
         (arguments (%call-arguments call))
         (effective-mode (%context-effective-permission-mode context)))
    (%ensure-tool-registered context tool-name arguments)
    (%validate-tool-arguments tool-name arguments)
    (%check-permission-or-signal tool-name arguments context)
    (sandbox-check-tool-call tool-name arguments
                             :permission-mode effective-mode)
    (%maybe-log-invocation context tool-name arguments)
    (multiple-value-bind (decision details)
        (%run-hook-dispatch context :pre-tool-use tool-name arguments)
      (declare (ignore details))
      (when (eq decision :deny)
        (error 'tool-permission-denied
               :tool-name tool-name
               :arguments arguments
               :message (format nil "Tool ~S blocked by pre-tool-use hook."
                                tool-name)
               :reason "blocked by pre-tool-use hook")))
    (setf *pipeline-start-time-ms* (%pipeline-monotonic-milliseconds))
    (publish (%effective-event-bus context)
             (make-tool-invoked-event
              :tool-name tool-name
              :args arguments
              :permission-mode effective-mode
              :request-id *pipeline-current-request-id*))))

(defmethod execute-tool ((call pseudopod:tool-call)
                         (context tool-execution-context))
  (let* ((toolset (context-toolset context))
         (arguments (%call-arguments call))
         (prepared-call (%clone-tool-call-with-arguments call arguments))
         (result (pseudopod:invoke-tool-call toolset prepared-call)))
    (setf result (apply-sandbox-output-guard result))
    (setf *pipeline-current-result* result)
    result))

(defmethod execute-tool :after ((call pseudopod:tool-call)
                                (context tool-execution-context))
  (declare (ignore call))
  (let* ((tool-name *pipeline-current-tool-name*)
         (arguments *pipeline-current-arguments*)
         (elapsed-ms (%elapsed-milliseconds)))
    (%record-tool-metrics context tool-name elapsed-ms :ok)
    (%cache-tool-result context tool-name arguments *pipeline-current-result*)
    (%run-hook-dispatch context :post-tool-use tool-name *pipeline-current-result* elapsed-ms)
    (publish (%effective-event-bus context)
             (make-tool-completed-event
              :tool-name tool-name
              :args arguments
              :result *pipeline-current-result*
              :elapsed-ms elapsed-ms
              :request-id *pipeline-current-request-id*))))
