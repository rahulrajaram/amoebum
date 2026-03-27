(in-package :amoebum)

(defparameter *pipeline-start-time-ms* nil)
(defparameter *pipeline-current-result* nil)
(defparameter *pipeline-current-tool-name* nil)
(defparameter *pipeline-current-arguments* nil)
(defparameter *pipeline-current-request-id* nil)
(defvar *tool-error-llm-recovery-function* nil
  "When non-nil, a function called with (condition tool-name arguments) to attempt LLM-driven recovery from tool errors.")
(defparameter +missing-tool-argument-recovery-modes+
  '(:prompt :structured-error :disabled))
(defparameter *missing-tool-argument-recovery-mode* :prompt
  "Controls execute-tool recovery for TOOL-MISSING-ARGUMENT.
:prompt asks the user for missing required args in supervised interactive runs.
:structured-error returns a stable JSON error payload instead of signaling.
:disabled preserves raw tool-missing-argument signaling.")

(defclass tool-execution-context (pseudopod:tool-execution-context)
  ((permission-mode :initarg :permission-mode
                    :initform nil
                    :accessor context-permission-mode)
   (permission-cancel-thunk :initarg :permission-cancel-thunk
                           :initform nil
                           :accessor context-permission-cancel-thunk)
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

(defun %effective-missing-tool-argument-recovery-mode ()
  (let ((mode *missing-tool-argument-recovery-mode*))
    (if (member mode +missing-tool-argument-recovery-modes+ :test #'eq)
        mode
        :prompt)))

(defun %coerce-argument-name-string (name)
  (let ((text (string-downcase
               (if (symbolp name)
                   (symbol-name name)
                   (princ-to-string (or name ""))))))
    (when (plusp (length text))
      text)))

(defun %query-io-readable-p (query-io)
  (and (streamp query-io)
       (open-stream-p query-io)))

(defun %prompt-for-missing-tool-argument (condition arguments &key (query-io *query-io*))
  (let* ((argument-name
           (%coerce-argument-name-string
            (tool-argument-error-argument-name condition)))
         (tool-name (tool-error-tool-name condition)))
    (when (and argument-name
               (%query-io-readable-p query-io))
      (format query-io "~&Tool ~A is missing required argument ~A.~%"
              (or tool-name "unknown")
              argument-name)
      (format query-io "Enter value for ~A (blank to keep error): " argument-name)
      (finish-output query-io)
      (let ((line (handler-case
                      (read-line query-io nil nil)
                    (error () nil))))
        (when (and (stringp line)
                   (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                               line))))
          (let ((updated (%copy-arguments-to-hash-table arguments)))
            (setf (gethash argument-name updated) line)
            updated))))))

(defun %missing-tool-argument-result-payload (condition)
  (let ((payload (%make-equal-hash-table)))
    (setf (gethash "kind" payload) "tool_error"
          (gethash "error_type" payload) "missing_tool_argument"
          (gethash "tool" payload) (or (tool-error-tool-name condition) "")
          (gethash "argument" payload)
          (or (%coerce-argument-name-string
               (tool-argument-error-argument-name condition))
              "")
          (gethash "reason_code" payload) "missing_required_argument"
          (gethash "message" payload) (princ-to-string condition))
    payload))

(defun %missing-tool-argument-result-text (condition)
  (%encode-json-arguments (%missing-tool-argument-result-payload condition)))

(defun %missing-tool-argument-recovery (condition arguments context)
  (declare (ignore context))
  (case (%effective-missing-tool-argument-recovery-mode)
    (:prompt
     (let ((updated (%prompt-for-missing-tool-argument condition arguments)))
       (when updated
         (list :retry updated))))
    (:structured-error
     (list :use-value (%missing-tool-argument-result-text condition)))
    (otherwise nil)))

(defun context-toolset (context)
  (pseudopod:context-toolset context))

(defun (setf context-toolset) (value context)
  (setf (pseudopod:context-toolset context) value))

(defun execute-tool (tool-call context)
  (pseudopod:execute-tool tool-call context))

(defun %effective-pipeline-event-bus (event-bus)
  (or event-bus
      (and (boundp '*event-bus*) *event-bus*)
      (current-event-bus)))

(defun %make-restart-context (toolset permission-mode)
  (make-amoebum-context
   :toolset toolset
   :permission-mode permission-mode
   :event-bus (%effective-pipeline-event-bus nil)
   :hook-registry (and (boundp '*hook-registry*) *hook-registry*)
   :initialize-notifications-p nil))

(defun %normalize-restart-arguments (arguments)
  (%copy-arguments-to-hash-table arguments))

(defun %make-restart-tool-call (tool-name arguments)
  (pseudopod:make-tool-call
   :id (format nil "restart-~A-~D"
               (normalize-name tool-name)
               (get-universal-time))
   :name (normalize-name tool-name)
   :arguments (%encode-json-arguments (%normalize-restart-arguments arguments))))

(defun %handle-tool-error-via-llm (condition tool-name arguments)
  "Delegate tool error recovery to the LLM recovery function.
The recovery function should inspect the condition and available restarts,
then invoke one of: retry-with-modified-args, use-alternative-tool,
skip-tool-call, abort-step, or ask-user."
  (when (functionp *tool-error-llm-recovery-function*)
    (funcall *tool-error-llm-recovery-function*
             condition tool-name arguments)))

(defun %run-on-error-hooks (context condition tool-name)
  (let ((*hook-registry* (or (context-hook-registry context)
                             *hook-registry*)))
    (run-hooks :on-error condition tool-name)))

(defun %execute-tool-with-restarts (tool-name arguments toolset permission-mode)
  (let* ((normalized-tool-name (normalize-name tool-name))
         (normalized-arguments (%normalize-restart-arguments arguments))
         (context (%make-restart-context toolset permission-mode))
         (tool-call (%make-restart-tool-call normalized-tool-name normalized-arguments)))
    (handler-bind
        ((tool-error
           (lambda (condition)
             (%run-on-error-hooks context condition normalized-tool-name)
             (cond
               ((and (eq (%effective-permission-mode permission-mode) :supervised)
                     (functionp *supervised-restart-selector*))
                (let ((decision (funcall *supervised-restart-selector* condition)))
                  (when decision
                    (%invoke-restart-decision condition decision))))
               ((functionp *tool-error-llm-recovery-function*)
                (%handle-tool-error-via-llm condition
                                            normalized-tool-name
                                            normalized-arguments))))))
      (restart-case
          (restart-case
              (execute-tool tool-call context)
            (retry-with-modified-args (new-arguments)
              :report "Retry this tool with modified arguments."
              (%execute-tool-with-restarts normalized-tool-name
                                           new-arguments
                                           toolset
                                           permission-mode))
            (use-alternative-tool (alternative-tool-name alternative-arguments)
              :report "Switch to an alternative tool for this step."
              (%execute-tool-with-restarts alternative-tool-name
                                           (or alternative-arguments normalized-arguments)
                                           toolset
                                           permission-mode))
            (retry-tool (&optional (new-arguments normalized-arguments))
              :report "Compatibility alias for retry-with-modified-args."
              (invoke-restart 'retry-with-modified-args new-arguments))
            (use-value (value)
              :report "Compatibility alias that returns a replacement value."
              value))
        (skip-tool-call ()
          :report "Skip this tool call and continue."
          (format nil "Tool ~A skipped by recovery policy." normalized-tool-name))
        (abort-step ()
          :report "Abort the current step and propagate failure."
          (error 'amoebum-error
                 :message (format nil "Tool ~A aborted current step." normalized-tool-name)))
        (ask-user ()
          :report "Ask user for guidance before retrying."
          (format nil "Tool ~A requires user guidance to continue."
                  normalized-tool-name))
        (skip-tool ()
          :report "Compatibility alias for skip-tool-call."
          (invoke-restart 'skip-tool-call))
        (abort-tool ()
          :report "Compatibility alias for abort-step."
          (invoke-restart 'abort-step))))))

(defun execute-tool-with-restarts (tool-name arguments
                                     &key (toolset *toolset*)
                                       permission-mode)
  "Invoke TOOL-NAME through execute-tool with recovery restarts."
  (%execute-tool-with-restarts tool-name arguments toolset permission-mode))

(defun make-amoebum-context (&key
                               (toolset *toolset*)
                               permission-mode
                               event-bus
                               hook-registry
                               metrics
                               result-cache
                               permission-cancel-thunk
                               logger
                               (initialize-notifications-p t))
  (let ((context
          (make-instance 'amoebum-context
                         :toolset toolset
                         :permission-mode permission-mode
                         :permission-cancel-thunk permission-cancel-thunk
                         :event-bus event-bus
                         :hook-registry hook-registry
                         :metrics (or metrics (make-hash-table :test #'equal))
                         :result-cache (or result-cache (make-hash-table :test #'equal))
                         :logger logger)))
    (when initialize-notifications-p
      (ensure-notification-manager :event-bus (or event-bus (current-event-bus))))
    context))

;; Name normalization delegated to normalize-name in util.lisp

(defun %tool-call-name-string (call)
  (normalize-name (pseudopod:tool-call-name call)))

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

;; Tool metadata lookup delegated to find-tool-metadata in deftool.lisp

(defun %metadata-timeout-seconds (tool-name)
  (let ((metadata (find-tool-metadata tool-name)))
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
  (let ((metadata (find-tool-metadata tool-name)))
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
                   :reason-code :missing-required-argument
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

(defun %permission-evaluation-effect (evaluation tool-name arguments)
  (check-type evaluation permission-evaluation)
  (let* ((context (permission-evaluation-context evaluation))
         (decision-context (permission-evaluation-decision-context evaluation))
         (resolved-path (%coerce-path-string (%extract-path-argument arguments)))
         (resolved-command (%coerce-command-string (%extract-command-argument arguments)))
         (decision (permission-evaluation-decision evaluation))
         (reason (or (permission-evaluation-reason evaluation)
                     (format nil "permission decision ~A" decision))))
    (list :kind decision
          :decision-id (permission-check-context-decision-id context)
          :tool-name tool-name
          :path resolved-path
          :command resolved-command
          :reason reason
          :actionable-reason (or (permission-evaluation-actionable-reason evaluation)
                                 reason)
          :reason-code (permission-evaluation-reason-code evaluation)
          :decision-context (and decision-context
                                 (policy-decision-context-plist decision-context))
          :trace (permission-evaluation-trace evaluation))))

(defun %check-permission-or-signal (tool-name arguments context)
  (let* ((path-arg (%coerce-path-string
                    (%extract-path-argument arguments)))
         (command-arg (%coerce-command-string
                       (%extract-command-argument arguments)))
         (effective-mode (%context-effective-permission-mode context))
         (metadata (find-tool-metadata tool-name))
         (dangerous-p (and metadata (tool-metadata-dangerous-p metadata)))
         (evaluation
           (evaluate-permission-decision
            :tool tool-name
            :path path-arg
            :command command-arg
            :dangerous-p dangerous-p
            :permission-mode effective-mode))
         (_materialized
           (%materialize-permission-evaluation! evaluation :record-history-p t))
         (effect (%permission-evaluation-effect evaluation tool-name arguments)))
    (declare (ignore _materialized))
    (cond
      ((eq (getf effect :kind) :allow) nil)
      ((eq (getf effect :kind) :prompt)
       ;; Publish the event so the TUI can show the approval dialog,
       ;; then block until the user resolves it.
       (%publish-permission-prompted context
                                     tool-name
                                     arguments
                                     (getf effect :kind)
                                     :reason (getf effect :reason)
                                     :reason-code (getf effect :reason-code))
       (let* ((pa (wait-for-pending-approval
                   tool-name arguments
                   :path (getf effect :path)
                   :command (getf effect :command)
                   :reason (getf effect :reason)
                   :decision-id (getf effect :decision-id)
                   :cancel-thunk (context-permission-cancel-thunk context)))
              (user-decision (pending-approval-decision pa))
              (decision-source (pending-approval-decision-source pa))
              (decision-reason-text
                (case decision-source
                  (:timeout "approval request timed out")
                  (:cancelled "approval was cancelled")
                  (:ui-error "approval dialog failed")
                  (:noninteractive "approval UI was inactive in non-interactive mode")
                  (otherwise "denied by user"))))
         ;; Track exact path approvals in session memory, and persist only
         ;; explicit "always allow" decisions.
         (when (and (eq user-decision :allow)
                    (stringp path-arg)
                    (plusp (length path-arg)))
           (remember-path-approval
            :tool tool-name
            :path path-arg
            :scope (if (pending-approval-remember-p pa) :always :session)
            :persist-p (pending-approval-remember-p pa)))
         ;; Retain deny memory and non-path "always allow" behavior in rules.
         (when (pending-approval-remember-p pa)
           (when (or (eq user-decision :deny)
                     (or (null path-arg)
                         (zerop (length path-arg))))
             (add-permission-rule :effect user-decision
                                  :tool tool-name
                                  :source :user-approval)))
         (unless (eq user-decision :allow)
           (error 'tool-permission-denied
                  :tool-name tool-name
                  :arguments arguments
                  :reason-code (getf effect :reason-code)
                  :message (format nil "Tool ~S denied: ~A."
                                   tool-name
                                   decision-reason-text)
                  :reason decision-reason-text))))
      (t
       ;; :deny or any other non-allow decision
       (when (eq (getf effect :kind) :deny)
         (%publish-permission-blocked context
                                      tool-name
                                      arguments
                                      (getf effect :kind)
                                      :reason (getf effect :reason)
                                      :actionable-reason (getf effect :actionable-reason)
                                      :reason-code (getf effect :reason-code)))
       (error 'tool-permission-denied
              :tool-name tool-name
              :arguments arguments
              :reason-code (getf effect :reason-code)
              :message (format nil "Permission decision ~A for tool ~S: ~A."
                               (getf effect :kind)
                               tool-name
                               (getf effect :actionable-reason))
              :reason (getf effect :actionable-reason))))))

;; Monotonic time delegated to monotonic-ms in util.lisp

(defun %elapsed-milliseconds ()
  (if *pipeline-start-time-ms*
      (max 0 (- (monotonic-ms) *pipeline-start-time-ms*))
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
         (key (normalize-name tool-name))
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
  (list (normalize-name tool-name)
        (%arguments-cache-signature arguments)))

(defun %cache-tool-result (context tool-name arguments result)
  (setf (gethash (%cache-key tool-name arguments)
                 (context-result-cache context))
        result))

(defun context-tool-metrics (context tool-name)
  (copy-list (gethash (normalize-name tool-name)
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

(defun %post-tool-success (context)
  "Record metrics, cache result, run post-hooks, and publish completed event.
Called from :around after *pipeline-current-result* is set, because CLOS
:after methods run before :around can assign the result variable."
  (let* ((tool-name *pipeline-current-tool-name*)
         (arguments *pipeline-current-arguments*)
         (elapsed-ms (%elapsed-milliseconds)))
    (usdt-probe-tool-exit tool-name
                          *pipeline-current-request-id*
                          elapsed-ms
                          :status :ok)
    (ignore-errors
      (note-tool-profiling-sample tool-name elapsed-ms))
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

(defmethod pseudopod:execute-tool :around ((call pseudopod:tool-call)
                                           (context tool-execution-context))
  (let* ((*pipeline-current-tool-name* (%tool-call-name-string call))
         (*pipeline-current-arguments* (%decode-tool-call-arguments call))
         (*pipeline-current-request-id* (%tool-call-request-id call))
         (*pipeline-start-time-ms* nil)
         (*pipeline-current-result* nil)
         (timeout-seconds (%metadata-timeout-seconds *pipeline-current-tool-name*)))
    (flet ((%signal-tool-error (condition)
             (let* ((tool-error
                      (%coerce-tool-error *pipeline-current-tool-name*
                                          *pipeline-current-arguments*
                                          condition
                                          timeout-seconds))
                    (elapsed-ms (%elapsed-milliseconds)))
               (usdt-probe-tool-exit *pipeline-current-tool-name*
                                     *pipeline-current-request-id*
                                     elapsed-ms
                                     :status :error)
               (ignore-errors
                 (note-tool-profiling-sample *pipeline-current-tool-name* elapsed-ms))
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
      (restart-case
          (handler-bind
              ((tool-missing-argument
                 (lambda (condition)
                   (let ((recovery
                           (%missing-tool-argument-recovery
                            condition
                            *pipeline-current-arguments*
                            context)))
                     (when recovery
                       (case (first recovery)
                         (:retry
                          (invoke-restart 'retry-tool (second recovery)))
                         (:use-value
                          (invoke-restart 'use-value (second recovery)))))))))
            (handler-case
                (let* ((raw-result
                         #+sbcl
                         (if (and timeout-seconds (> timeout-seconds 0))
                             (sb-ext:with-timeout timeout-seconds
                               (call-next-method))
                             (call-next-method))
                         #-sbcl
                         (call-next-method))
                       (guarded-result (apply-sandbox-output-guard raw-result)))
                  (setf *pipeline-current-result* guarded-result)
                  (%post-tool-success context)
                  guarded-result)
              #+sbcl
              (sb-ext:timeout (condition)
                (%signal-tool-error condition))
              (error (condition)
                (%signal-tool-error condition))))
        (retry-tool (&optional (new-arguments *pipeline-current-arguments*))
          :report "Retry tool execution."
          (pseudopod:execute-tool (%clone-tool-call-with-arguments call new-arguments)
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
                                  *pipeline-current-tool-name*)))))))

(defmethod pseudopod:execute-tool :before ((call pseudopod:tool-call)
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
    (setf *pipeline-start-time-ms* (monotonic-ms))
    (usdt-probe-tool-enter tool-name *pipeline-current-request-id*)
    (publish (%effective-event-bus context)
             (make-tool-invoked-event
              :tool-name tool-name
              :args arguments
              :permission-mode effective-mode
              :request-id *pipeline-current-request-id*))))

(defmethod pseudopod:execute-tool :after ((call pseudopod:tool-call)
                                          (context tool-execution-context))
  ;; Post-success work (metrics, cache, hooks, events) moved to
  ;; %post-tool-success called from :around, because CLOS :after runs
  ;; inside call-next-method before :around can set *pipeline-current-result*.
  (declare (ignore call context))
  (when (and (plan-mode-active-p)
             (plan-mode-exploration-tool-p *pipeline-current-tool-name*))
    (record-plan-mode-exploration *pipeline-current-tool-name* :successful-p t))
  (values))
