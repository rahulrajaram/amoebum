(in-package :amoebum)

(defparameter *pipeline-start-time-ms* nil)
(defparameter *pipeline-current-result* nil)
(defparameter *pipeline-current-tool-name* nil)
(defparameter *pipeline-current-arguments* nil)
(defparameter *pipeline-current-request-id* nil)

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

(defun context-toolset (context)
  (pseudopod:context-toolset context))

(defun (setf context-toolset) (value context)
  (setf (pseudopod:context-toolset context) value))

(defun execute-tool (tool-call context)
  (pseudopod:execute-tool tool-call context))

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

(defun %elapsed-milliseconds ()
  (if *pipeline-start-time-ms*
      (max 0 (- (monotonic-ms) *pipeline-start-time-ms*))
      0))

(defun %ensure-tool-registered (context tool-name arguments)
  (unless (pseudopod:find-tool (context-toolset context) tool-name)
    (error 'capability-gap
           :tool-name tool-name
           :arguments arguments
           :capability-name tool-name
           :recovery-contract (%capability-gap-contract tool-name arguments
                                                        :capability-name tool-name
                                                        :reason-code :capability-gap)
           :reason-code :capability-gap
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

(defun %clone-tool-call-with-arguments (call arguments)
  (pseudopod:make-tool-call
   :id (pseudopod:tool-call-id call)
   :name (pseudopod:tool-call-name call)
   :arguments (%encode-json-arguments
               (%copy-arguments-to-hash-table arguments))
   :extras (pseudopod:tool-call-extras call)))
