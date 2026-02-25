(in-package :amoebum)

(defparameter +tool-restart-names+
  '(retry-with-modified-args
    use-alternative-tool
    skip-tool-call
    abort-step
    ask-user
    retry-tool
    skip-tool
    use-value
    abort-tool))

(defparameter *supervised-restart-selector*
  nil)

(defparameter *tool-error-llm-recovery-function*
  nil)

(define-condition amoebum-error (error)
  ((message :initarg :message
            :initform nil
            :reader amoebum-error-message))
  (:report (lambda (condition stream)
             (let ((message (amoebum-error-message condition)))
               (if message
                   (write-string message stream)
                   (write-string "Amoebum error." stream))))))

(define-condition tool-error (amoebum-error)
  ((tool-name :initarg :tool-name
              :reader tool-error-tool-name)
   (arguments :initarg :arguments
              :initform nil
              :reader tool-error-arguments)
   (reason-code :initarg :reason-code
               :initform nil
               :reader tool-error-reason-code)
   (cause :initarg :cause
          :initform nil
          :reader tool-error-cause)
   (reason :initarg :reason
           :initform nil
           :reader tool-error-reason))
  (:report (lambda (condition stream)
             (let ((name (tool-error-tool-name condition))
                   (reason (or (tool-error-reason condition)
                               (amoebum-error-message condition))))
               (if reason
                   (format stream "Tool ~S failed: ~A." name reason)
                   (format stream "Tool ~S failed." name))))))

(define-condition tool-execution-error (tool-error) ())

(define-condition tool-timeout (tool-execution-error)
  ((timeout-seconds :initarg :timeout-seconds
                    :initform nil
                    :reader tool-timeout-seconds))
  (:report (lambda (condition stream)
             (let ((name (tool-error-tool-name condition))
                   (seconds (tool-timeout-seconds condition)))
               (if (and seconds (> seconds 0))
                   (format stream "Tool ~S timed out after ~D seconds." name seconds)
                   (format stream "Tool ~S timed out." name))))))

(define-condition tool-timeout-error (tool-timeout) ())

(define-condition tool-permission-denied (tool-execution-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Permission denied for tool ~S: ~A."
                     (tool-error-tool-name condition)
                     (or (tool-error-reason condition) "policy blocked execution")))))

(define-condition tool-not-found (tool-execution-error)
  ()
  (:report (lambda (condition stream)
             (format stream "No registered tool named ~S."
                     (tool-error-tool-name condition)))))

(define-condition tool-not-found-error (tool-not-found) ())

(define-condition tool-argument-error (tool-error)
  ((argument-name :initarg :argument-name
                  :initform nil
                  :reader tool-argument-error-argument-name))
  (:report (lambda (condition stream)
             (let ((name (tool-error-tool-name condition))
                   (arg (tool-argument-error-argument-name condition))
                   (reason (or (tool-error-reason condition)
                               (amoebum-error-message condition)
                               "invalid arguments")))
               (if arg
                   (format stream "Invalid argument ~S for tool ~S: ~A." arg name reason)
                   (format stream "Invalid arguments for tool ~S: ~A." name reason))))))

(define-condition tool-missing-argument (tool-argument-error) ())

(define-condition tool-type-mismatch (tool-argument-error) ())

(define-condition hook-execution-error (amoebum-error)
  ((hook-id :initarg :hook-id
            :initform nil
            :reader hook-execution-error-hook-id)
   (hook-point :initarg :hook-point
               :initform nil
               :reader hook-execution-error-hook-point)
   (cause :initarg :cause
          :initform nil
          :reader hook-execution-error-cause))
  (:report (lambda (condition stream)
             (format stream "Hook ~S on ~S failed."
                     (hook-execution-error-hook-id condition)
                     (hook-execution-error-hook-point condition)))))

(define-condition context-overflow-error (amoebum-error)
  ((used-tokens :initarg :used-tokens
                :initform nil
                :reader context-overflow-used-tokens)
   (max-tokens :initarg :max-tokens
               :initform nil
               :reader context-overflow-max-tokens))
  (:report (lambda (condition stream)
             (format stream "Context window exceeded (~A/~A tokens)."
                     (context-overflow-used-tokens condition)
                     (context-overflow-max-tokens condition)))))

(define-condition budget-exceeded-error (amoebum-error)
  ((kind :initarg :kind
         :initform :token
         :reader budget-exceeded-kind)
   (used :initarg :used
         :initform nil
         :reader budget-exceeded-used)
   (budget :initarg :budget
           :initform nil
           :reader budget-exceeded-budget))
  (:report (lambda (condition stream)
             (format stream "~A budget exceeded (~A/~A)."
                     (string-capitalize
                      (string-downcase (symbol-name (budget-exceeded-kind condition))))
                     (budget-exceeded-used condition)
                     (budget-exceeded-budget condition)))))

(defgeneric condition-to-llm-context (condition)
  (:documentation "Convert CONDITION to compact context text for LLM recovery."))

(defun %condition-type-name (condition)
  (let ((name (class-name (class-of condition))))
    (if (symbolp name)
        (string-downcase (symbol-name name))
        (string-downcase (princ-to-string name)))))

(defun %condition-message (condition)
  (with-output-to-string (stream)
    (princ condition stream)))

(defun %tool-restarts (condition)
  (remove-if-not
   (lambda (restart)
     (let ((name (restart-name restart)))
       (and name (member name +tool-restart-names+ :test #'eq))))
   (compute-restarts condition)))

(defun %restart-summary (condition)
  (let ((restarts (%tool-restarts condition)))
    (if restarts
        (format nil "~{[~A]~^, ~}"
                (mapcar (lambda (restart)
                          (string-downcase (symbol-name (restart-name restart))))
                        restarts))
        "none")))

(defun %format-llm-condition-context (condition suggested-action)
  (format nil
          "Condition type: ~A~%Message: ~A~%Available restarts: ~A~%Suggested action: ~A"
          (%condition-type-name condition)
          (%condition-message condition)
          (%restart-summary condition)
          suggested-action))

(defmethod condition-to-llm-context ((condition condition))
  (%format-llm-condition-context
   condition
   "Inspect the error context and choose the safest available restart; ask user if uncertainty remains."))

(defmethod condition-to-llm-context ((condition amoebum-error))
  (%format-llm-condition-context
   condition
   "Summarize the failure and recover conservatively; prefer skip or user confirmation over risky retries."))

(defmethod condition-to-llm-context ((condition tool-error))
  (%format-llm-condition-context
   condition
   (format nil "Review tool '~A' inputs and retry only after fixing arguments or selecting a safer alternative."
           (tool-error-tool-name condition))))

(defmethod condition-to-llm-context ((condition tool-execution-error))
  (%format-llm-condition-context
   condition
   (format nil "Tool '~A' failed during execution; retry if transient, otherwise skip and continue."
           (tool-error-tool-name condition))))

(defmethod condition-to-llm-context ((condition tool-permission-denied))
  (%format-llm-condition-context
   condition
   (format nil "Permission denied for '~A'; request approval, adjust permission mode, or choose a non-dangerous fallback."
           (tool-error-tool-name condition))))

(defmethod condition-to-llm-context ((condition tool-not-found))
  (%format-llm-condition-context
   condition
   (format nil "Tool '~A' is unavailable; choose an alternative tool or skip this step."
           (tool-error-tool-name condition))))

(defmethod condition-to-llm-context ((condition tool-not-found-error))
  (%format-llm-condition-context
   condition
   (format nil "Tool '~A' is unavailable; use an alternate capability and preserve task continuity."
           (tool-error-tool-name condition))))

(defmethod condition-to-llm-context ((condition tool-timeout))
  (%format-llm-condition-context
   condition
   (format nil "Tool '~A' timed out after ~A seconds; retry with smaller scope or skip to avoid blocking."
           (tool-error-tool-name condition)
           (or (tool-timeout-seconds condition) "unknown"))))

(defmethod condition-to-llm-context ((condition tool-timeout-error))
  (%format-llm-condition-context
   condition
   (format nil "Timeout from tool '~A'; prefer reduced workload retries, then fallback/skip if repeated."
           (tool-error-tool-name condition))))

(defmethod condition-to-llm-context ((condition tool-argument-error))
  (%format-llm-condition-context
   condition
   (format nil "Arguments for tool '~A' are invalid; correct schema/values before retry."
           (tool-error-tool-name condition))))

(defmethod condition-to-llm-context ((condition tool-missing-argument))
  (%format-llm-condition-context
   condition
   (format nil "Supply required argument(s) for tool '~A' and retry."
           (tool-error-tool-name condition))))

(defmethod condition-to-llm-context ((condition tool-type-mismatch))
  (%format-llm-condition-context
   condition
   (format nil "Argument type mismatch for tool '~A'; coerce or replace values with the expected type."
           (tool-error-tool-name condition))))

(defmethod condition-to-llm-context ((condition hook-execution-error))
  (%format-llm-condition-context
   condition
   (format nil "Hook ~S at ~S failed; disable or bypass the hook, then continue with guarded execution."
           (hook-execution-error-hook-id condition)
           (hook-execution-error-hook-point condition))))

(defmethod condition-to-llm-context ((condition context-overflow-error))
  (%format-llm-condition-context
   condition
   "Context window exceeded; compress context, drop low-priority messages, then retry."))

(defmethod condition-to-llm-context ((condition budget-exceeded-error))
  (%format-llm-condition-context
   condition
   (format nil "~A budget exceeded; reduce scope/cost and continue with smaller operations."
           (string-downcase (symbol-name (budget-exceeded-kind condition))))))

(defun %llm-message->text (message)
  (if (pseudopod:message-p message)
      (with-output-to-string (stream)
        (loop for part in (pseudopod:message-content message)
              for index from 0 do
                (when (> index 0)
                  (write-char #\Newline stream))
                (let ((type (string-downcase
                             (or (pseudopod:content-part-type part) "text"))))
                  (write-string
                   (cond
                     ((string= type "text")
                      (or (pseudopod:content-part-text part) ""))
                     ((string= type "think")
                      (or (pseudopod:content-part-think part) ""))
                     (t
                      (or (pseudopod:content-part-text part)
                          (pseudopod:content-part-think part)
                          "")))
                   stream))))
      (princ-to-string message)))

(defun %strip-json-code-fence (text)
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) (or text ""))))
    (if (and (>= (length trimmed) 6)
             (uiop:string-prefix-p "```" trimmed)
             (uiop:string-suffix-p "```" trimmed))
        (let ((middle (subseq trimmed 3 (- (length trimmed) 3))))
          (string-trim
           '(#\Space #\Tab #\Newline #\Return)
           (if (uiop:string-prefix-p "json" (string-downcase middle))
               (subseq middle 4)
               middle)))
        trimmed)))

(defun %format-restart-options-for-llm (condition)
  (loop for restart in (%tool-restarts condition)
        for name = (restart-name restart)
        when name
          collect (list :name (string-downcase (symbol-name name))
                        :symbol name)))

(defun %default-tool-error-llm-recovery-function (condition tool-name arguments restart-options)
  (let* ((model (or (ignore-errors (config-model (current-config)))
                    "moonshot-v1-128k"))
         (client (pseudopod:make-client :model model))
         (error-context (condition-to-llm-context condition))
         (prompt
           (format nil
                   "Choose the best recovery restart for this tool error.~%\
Tool: ~A~%\
Arguments: ~S~2%\
~A~2%\
Available restarts:~%~{  - ~A~%~}~%\
Return ONLY JSON with keys `restart` and optional `args`."
                   tool-name
                   arguments
                   error-context
                   (mapcar (lambda (entry) (getf entry :name))
                           restart-options)))
         (response
           (pseudopod:chat-completion*
            client
            prompt
            :system-prompt
            "You are an error recovery agent. Return valid JSON only.")))
    (%llm-message->text response)))

(unless (functionp *tool-error-llm-recovery-function*)
  (setf *tool-error-llm-recovery-function*
        #'%default-tool-error-llm-recovery-function))

(defun %restart-name-from-json (value)
  (cond
    ((symbolp value) value)
    ((stringp value)
     (let ((needle (string-downcase value)))
       (loop for candidate in +tool-restart-names+
             when (string= needle (string-downcase (symbol-name candidate)))
               do (return candidate))))
    (t nil)))

(defun %parse-llm-restart-decision (response-text)
  (let* ((payload (%strip-json-code-fence response-text))
         (decoded (jonathan:parse payload :as :hash-table))
         (restart-value (and (hash-table-p decoded) (gethash "restart" decoded)))
         (args (and (hash-table-p decoded)
                    (or (gethash "args" decoded)
                        (gethash "arguments" decoded))))
         (tool (and (hash-table-p decoded)
                    (or (gethash "tool" decoded)
                        (gethash "tool_name" decoded)
                        (gethash "alternative_tool" decoded)))))
    (unless (and (hash-table-p decoded) restart-value)
      (error "Missing restart selection JSON payload."))
    (list :restart (%restart-name-from-json restart-value)
          :args args
          :tool tool)))

(defun %find-first-restart (condition names)
  (loop for name in names
        for restart = (find-restart name condition)
        when restart
          do (return restart)))

(defun %invoke-ask-user-fallback (condition)
  (let ((restart (%find-first-restart condition '(ask-user))))
    (when restart
      (invoke-restart restart)
      t)))

(defun %apply-llm-restart-decision (condition tool-name arguments decision)
  (let* ((selected (getf decision :restart))
         (new-args (or (getf decision :args) arguments))
         (alternative-tool (or (getf decision :tool) tool-name)))
    (case selected
      ((retry-with-modified-args retry-tool)
       (let ((restart (%find-first-restart condition '(retry-with-modified-args retry-tool))))
         (when restart
           (invoke-restart restart new-args)
           t)))
      (use-alternative-tool
       (let ((restart (%find-first-restart condition '(use-alternative-tool))))
         (when restart
           (invoke-restart restart alternative-tool new-args)
           t)))
      ((skip-tool-call skip-tool)
       (let ((restart (%find-first-restart condition '(skip-tool-call skip-tool))))
         (when restart
           (invoke-restart restart)
           t)))
      ((abort-step abort-tool)
       (let ((restart (%find-first-restart condition '(abort-step abort-tool))))
         (when restart
           (invoke-restart restart)
           t)))
      (ask-user
       (%invoke-ask-user-fallback condition))
      (otherwise
       nil))))

(defun %handle-tool-error-via-llm (condition tool-name arguments)
  (let* ((restart-options (%format-restart-options-for-llm condition))
         (response
           (and restart-options
                (functionp *tool-error-llm-recovery-function*)
                (funcall *tool-error-llm-recovery-function*
                         condition
                         tool-name
                         arguments
                         restart-options))))
    (handler-case
        (let ((decision (%parse-llm-restart-decision response)))
          (or (%apply-llm-restart-decision condition tool-name arguments decision)
              (%invoke-ask-user-fallback condition)))
      (error ()
        (%invoke-ask-user-fallback condition)))))

(defun %argument-as-string (arguments key)
  (let ((value (and (hash-table-p arguments)
                    (gethash key arguments))))
    (when value
      (typecase value
        (string value)
        (pathname (namestring value))
        (symbol (symbol-name value))
        (t (princ-to-string value))))))

(defun %tool-metadata-for (tool-name)
  (and (boundp '*tool-metadata*)
       (hash-table-p *tool-metadata*)
       (gethash (string-downcase (princ-to-string tool-name)) *tool-metadata*)))

(defun %likely-missing-argument-p (message)
  (or (search "Missing required tool argument" message :test #'char-equal)
      (search "required argument" message :test #'char-equal)))

(defun %likely-type-mismatch-p (message)
  (or (search "is not of type" message :test #'char-equal)
      (search "type" message :test #'char-equal)))

(defun %tool-error-from-condition (tool-name arguments condition)
  (let ((message (princ-to-string condition)))
    (cond
      ((typep condition 'tool-error) condition)
      ((typep condition 'type-error)
       (make-condition 'tool-type-mismatch
                       :tool-name tool-name
                       :arguments arguments
                       :message message
                       :reason message
                       :cause condition))
      ((%likely-missing-argument-p message)
       (make-condition 'tool-missing-argument
                       :tool-name tool-name
                       :arguments arguments
                       :message message
                       :reason message
                       :cause condition))
      ((%likely-type-mismatch-p message)
       (make-condition 'tool-type-mismatch
                       :tool-name tool-name
                       :arguments arguments
                       :message message
                       :reason message
                       :cause condition))
      (t
       (make-condition 'tool-execution-error
                       :tool-name tool-name
                       :arguments arguments
                       :message message
                       :reason message
                       :cause condition)))))

(defun %normalize-tool-name (tool-name)
  (string-downcase
   (string-trim '(#\Space #\Tab #\Newline #\Return)
                (if (symbolp tool-name)
                    (symbol-name tool-name)
                    (princ-to-string tool-name)))))

(defun %invoke-tool-core (tool-name arguments toolset permission-mode)
  (let* ((metadata (%tool-metadata-for tool-name))
         (timeout-seconds (and metadata (tool-metadata-timeout-seconds metadata)))
         (dangerous-p (and metadata (tool-metadata-dangerous-p metadata)))
         (path (or (%argument-as-string arguments "path")
                   (%argument-as-string arguments "file")
                   (%argument-as-string arguments "target")))
         (command (or (%argument-as-string arguments "command")
                      (%argument-as-string arguments "cmd")))
         (permission (check-permission :tool tool-name
                                       :path path
                                       :command command
                                       :dangerous-p dangerous-p
                                       :permission-mode permission-mode)))
    (unless (pseudopod:find-tool toolset tool-name)
      (error 'tool-not-found
             :tool-name tool-name
             :arguments arguments
             :message (format nil "No registered tool named ~S." tool-name)
             :reason "tool not found"))
    (unless (eq permission :allow)
      (error 'tool-permission-denied
             :tool-name tool-name
             :arguments arguments
             :message (format nil "Permission decision ~A for tool ~S."
                              permission
                              tool-name)
             :reason (format nil "permission decision ~A" permission)))
    (handler-case
        #+sbcl
        (if (and timeout-seconds (> timeout-seconds 0))
            (sb-ext:with-timeout timeout-seconds
              (pseudopod:invoke-tool-call
               toolset
               (pseudopod:make-tool-call :name tool-name
                                         :arguments arguments)))
            (pseudopod:invoke-tool-call
             toolset
             (pseudopod:make-tool-call :name tool-name
                                       :arguments arguments)))
        #-sbcl
        (pseudopod:invoke-tool-call
         toolset
         (pseudopod:make-tool-call :name tool-name
                                   :arguments arguments))
      #+sbcl
      (sb-ext:timeout (condition)
        (error 'tool-timeout
               :tool-name tool-name
               :arguments arguments
               :timeout-seconds timeout-seconds
               :message (format nil "Tool ~S timed out." tool-name)
               :reason (format nil "timed out after ~A seconds" timeout-seconds)
               :cause condition))
      (error (condition)
        (error (%tool-error-from-condition tool-name arguments condition))))))

(defun %normalize-restart-decision (decision)
  (let ((name (if (consp decision) (first decision) decision))
        (args (if (consp decision) (rest decision) nil)))
    (values
     (cond
       ((symbolp name) name)
       ((stringp name)
        (intern (string-upcase name) (find-package :amoebum)))
       (t nil))
     args)))

(defun %invoke-restart-decision (condition decision)
  (multiple-value-bind (name args)
      (%normalize-restart-decision decision)
    (when name
      (let ((restart (find-restart name condition)))
        (when restart
          (apply #'invoke-restart restart args))))))

(defun default-supervised-restart-selector (condition)
  "Interactive selector for supervised mode; returns restart designator."
  (let* ((restarts (%tool-restarts condition))
         (query-io *query-io*))
    (cond
      ((null restarts)
       nil)
      ((not (and (streamp query-io) (open-stream-p query-io)))
       'skip-tool)
      (t
       (format query-io "~&Tool error: ~A~%" (%condition-message condition))
       (loop for restart in restarts
             for idx from 1 do
               (format query-io "  ~D) ~A~%"
                       idx
                       (string-downcase (symbol-name (restart-name restart)))))
       (format query-io "Choose restart [1-~D] (default skip): " (length restarts))
       (finish-output query-io)
       (let* ((line (handler-case
                        (read-line query-io nil nil)
                      (error () nil)))
              (parsed (ignore-errors
                        (and line (parse-integer line :junk-allowed t)))))
         (if (and parsed (>= parsed 1) (<= parsed (length restarts)))
             (let ((chosen (nth (1- parsed) restarts)))
               (if (eq (restart-name chosen) 'use-value)
                   (progn
                     (format query-io "Value for use-value restart: ")
                     (finish-output query-io)
                     (let ((value (handler-case
                                      (read-line query-io nil "")
                                    (error () ""))))
                       (list 'use-value value)))
                   (restart-name chosen)))
             'skip-tool))))))

(unless (functionp *supervised-restart-selector*)
  (setf *supervised-restart-selector* #'default-supervised-restart-selector))

(defun %execute-tool-with-restarts (tool-name arguments toolset permission-mode)
  (handler-bind
      ((tool-error
         (lambda (condition)
           (when (and (eq (%effective-permission-mode permission-mode) :supervised)
                      (functionp *supervised-restart-selector*))
             (let ((decision (funcall *supervised-restart-selector* condition)))
               (when decision
                 (%invoke-restart-decision condition decision)))))))
    (restart-case
        (%invoke-tool-core tool-name arguments toolset permission-mode)
      (retry-tool (&optional (new-arguments arguments))
        :report "Retry tool execution."
        (%execute-tool-with-restarts tool-name new-arguments toolset permission-mode))
      (skip-tool ()
        :report "Skip this tool and continue."
        (format nil "Tool ~A skipped by error recovery." tool-name))
      (use-value (value)
        :report "Provide a replacement value."
        value)
      (abort-tool ()
        :report "Abort tool execution and propagate failure."
        (error 'amoebum-error
               :message (format nil "Tool ~A aborted by recovery restart." tool-name))))))

(defun execute-tool-with-restarts (tool-name arguments
                                     &key (toolset *toolset*)
                                       permission-mode)
  "Invoke TOOL-NAME with restart protocol and typed amoebum conditions."
  (%execute-tool-with-restarts (%normalize-tool-name tool-name)
                               arguments
                               toolset
                               permission-mode))
