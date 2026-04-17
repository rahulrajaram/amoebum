(in-package :amoebum)

(defparameter +tool-restart-names+
  '(retry-with-modified-args
    use-alternative-tool
    delegate-capability-gap
    install-missing-capability
    skip-tool-call
    abort-step
    ask-user
    retry-tool
    skip-tool
    use-value
    abort-tool))

(defparameter +budget-restart-names+
  '(extend-budget
    summarize-and-finish
    abort-task))

(defparameter +budget-partial-output-max-chars+ 320)

(defparameter *supervised-restart-selector*
  nil)

(defparameter *budget-exhaustion-restart-selector*
  nil)

(defun %json-function (name)
  (let* ((package (find-package :jonathan))
         (symbol (and package (find-symbol name package))))
    (unless (and symbol (fboundp symbol))
      (error "Jonathan function ~A unavailable." name))
    (symbol-function symbol)))

(defun %hash-value (table candidates)
  (when (hash-table-p table)
    (loop for candidate in candidates do
      (multiple-value-bind (value present-p)
          (gethash candidate table)
        (when present-p
          (return value))))))

(defun %canonical-restart-name (name)
  (cond
    ((null name) nil)
    ((symbolp name)
     (let ((base (string-downcase (symbol-name name))))
       (substitute #\- #\_ base)))
    ((stringp name)
     (substitute #\- #\_ (string-downcase name)))
    (t
     (%canonical-restart-name (princ-to-string name)))))

(defun %normalize-restart-name (name)
  (let ((canonical (%canonical-restart-name name)))
    (when (and canonical (plusp (length canonical)))
      (intern (string-upcase canonical) (find-package :amoebum)))))

(defun %normalize-restart-args (value)
  (cond
    ((null value) '())
    ((listp value) value)
    (t (list value))))

(defun %extract-json-fragment (payload)
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) (or payload ""))))
    (when (plusp (length text))
      (let ((start (position #\{ text))
            (end (position #\} text :from-end t)))
        (when (and start end (< start end))
          (subseq text start (1+ end)))))))

(defun parse-recovery-decision (payload)
  "Parse LLM recovery JSON and return plist (:restart <symbol> :args <list>) or NIL."
  (let ((fragment (%extract-json-fragment payload)))
    (when fragment
      (handler-case
          (let* ((parse (or (ignore-errors (%json-function "PARSE")) nil))
                 (decoded (and parse
                               (handler-case
                                   (funcall parse fragment :as :hash-table :junk-allowed t)
                                 (error ()
                                   (funcall parse fragment :as :hash-table)))))
                 (restart-name
                   (%hash-value decoded
                                '("restart" "restart_name" "restart-name"
                                  :restart :restart_name :restart-name)))
                 (args
                   (%hash-value decoded
                                '("args" "arguments" "restart_args" "restart-args"
                                  :args :arguments :restart_args :restart-args))))
            (when restart-name
              (list :restart (%normalize-restart-name restart-name)
                    :args (%normalize-restart-args args))))
        (error ()
          nil)))))

(defun %decision-restart-and-args (decision)
  (cond
    ((null decision)
     (values nil '()))
    ((hash-table-p decision)
     (values (%normalize-restart-name
              (%hash-value decision
                           '("restart" "restart_name" "restart-name"
                             :restart :restart_name :restart-name)))
             (%normalize-restart-args
              (%hash-value decision
                           '("args" "arguments" "restart_args" "restart-args"
                             :args :arguments :restart_args :restart-args)))))
    ((and (listp decision) (getf decision :restart))
     (values (%normalize-restart-name (getf decision :restart))
             (%normalize-restart-args (or (getf decision :args)
                                          (getf decision :arguments)))))
    ((consp decision)
     (values (%normalize-restart-name (first decision))
             (rest decision)))
    (t
     (values (%normalize-restart-name decision) '()))))

(defun %restart-choice-from-input (input restarts)
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (or input "")))
         (index (ignore-errors (parse-integer trimmed :junk-allowed nil))))
    (cond
      ((and index (>= index 1) (<= index (length restarts)))
       (restart-name (nth (1- index) restarts)))
      ((plusp (length trimmed))
       (%normalize-restart-name trimmed))
      (t nil))))

(defun %recovery-menu-widget (condition restarts)
  (let* ((header (ptui.widgets.core:make-text-widget
                  "Tool Recovery"
                  :id :recovery-header
                  :role :heading))
         (message (ptui.widgets.core:make-text-widget
                   (%condition-message condition)
                   :id :recovery-message))
         (options
           (loop for restart in restarts
                 for index from 1
                 collect (ptui.widgets.core:make-text-widget
                          (format nil "~D) ~A"
                                  index
                                  (string-downcase
                                   (symbol-name (restart-name restart))))
                          :id (list :recovery-option index)
                          :metadata (list :restart (restart-name restart)))))
         (prompt (ptui.components.prompt-box:make-prompt-box-widget
                  ""
                  :id :recovery-prompt
                  :min-width 24
                  :cursor-visible-p t)))
    (ptui.widgets.core:make-box-widget
     (ptui.widgets.core:make-stack-widget
      (append (list header message) options (list prompt))
      :id :recovery-stack
      :gap 1)
     :id :recovery-menu
     :borderp t
     :padding 1)))

(defun %prompt-user-recovery-decision (condition restarts query-io)
  (let* ((runtime (ptui.ui.runtime:make-runtime))
         (widget (%recovery-menu-widget condition restarts)))
    (ignore-errors
      (ptui.ui.runtime:update-runtime runtime widget))
    (cond
      ((not (and (streamp query-io) (open-stream-p query-io)))
       (and (find 'skip-tool restarts :key #'restart-name :test #'eq)
            'skip-tool))
      (t
       (format query-io "~&Tool error: ~A~%" (%condition-message condition))
       (loop for restart in restarts
             for index from 1 do
               (format query-io "  ~D) ~A~%"
                       index
                       (string-downcase (symbol-name (restart-name restart)))))
       (format query-io "Choose restart [1-~D] (default skip): " (length restarts))
       (finish-output query-io)
       (let* ((line (handler-case (read-line query-io nil nil)
                      (error () nil)))
              (choice (%restart-choice-from-input line restarts)))
         (or (and choice
                  (find choice restarts :key #'restart-name :test #'eq)
                  choice)
             (and (find 'skip-tool restarts :key #'restart-name :test #'eq)
                  'skip-tool)
             (and restarts (restart-name (first restarts)))))))))

(defun apply-user-recovery-decision (decision condition &key (query-io *query-io*))
  "Apply DECISION for CONDITION, prompting via a PTUI widget menu when needed."
  (let ((restarts (%tool-restarts condition)))
    (when restarts
      (multiple-value-bind (desired-restart desired-args)
          (%decision-restart-and-args decision)
        (let* ((selected
                 (or (and desired-restart
                          (find desired-restart restarts
                                :key #'restart-name
                                :test #'eq))
                     (let ((choice (%prompt-user-recovery-decision
                                    condition
                                    restarts
                                    query-io)))
                       (and choice
                            (find choice restarts
                                  :key #'restart-name
                                  :test #'eq)))))
               (restart-name (and selected (restart-name selected))))
          (when restart-name
            (apply #'invoke-restart selected desired-args)))))))

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

(define-condition capability-gap (tool-not-found-error)
  ((capability-name :initarg :capability-name
                    :initform nil
                    :reader capability-gap-capability-name)
   (recovery-contract :initarg :recovery-contract
                      :initform nil
                      :reader capability-gap-recovery-contract))
  (:report (lambda (condition stream)
             (format stream "Capability gap for ~S~@[ (~A)~]."
                     (tool-error-tool-name condition)
                     (or (capability-gap-capability-name condition)
                         "missing capability")))))

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
             (let ((cause (hook-execution-error-cause condition)))
               (format stream "Hook ~S on ~S failed~@[ (~A)~]."
                       (hook-execution-error-hook-id condition)
                       (hook-execution-error-hook-point condition)
                       (and cause (princ-to-string cause)))))))

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

(define-condition budget-exhausted-condition (amoebum-error)
  ((kind :initarg :kind
         :initform :token
         :reader budget-exhausted-kind)
   (used :initarg :used
         :initform 0
         :reader budget-exhausted-used)
   (budget :initarg :budget
           :initform 0
           :reader budget-exhausted-budget)
   (context-summary :initarg :context-summary
                    :initform nil
                    :reader budget-exhausted-context-summary))
  (:report (lambda (condition stream)
             (format stream "~A budget exhausted (~A/~A)."
                     (string-capitalize
                      (string-downcase
                       (symbol-name (budget-exhausted-kind condition))))
                     (budget-exhausted-used condition)
                     (budget-exhausted-budget condition)))))

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

(defmethod condition-to-llm-context ((condition capability-gap))
  (%format-llm-condition-context
   condition
   (format nil
           "Capability '~A' is unavailable; prefer [delegate-capability-gap] or [install-missing-capability], then [ask-user] if delegation policy is unclear."
           (or (capability-gap-capability-name condition)
               (tool-error-tool-name condition)))))

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

(defmethod condition-to-llm-context ((condition budget-exhausted-condition))
  (%format-llm-condition-context
   condition
   "Budget exhausted; choose extend-budget, summarize-and-finish, or abort-task based on risk and user preference."))

(defun %normalize-inline-budget-text (value)
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return)
                           (if (stringp value)
                               value
                               (princ-to-string (or value ""))))))
    (with-output-to-string (out)
      (let ((previous-space-p nil))
        (loop for char across text do
          (if (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=)
              (unless previous-space-p
                (write-char #\Space out)
                (setf previous-space-p t))
              (progn
                (write-char char out)
                (setf previous-space-p nil))))))))

(defun %coerce-max-partial-output-chars (value)
  (if (and (integerp value) (> value 0))
      value
      +budget-partial-output-max-chars+))

(defun %bounded-budget-output (text max-chars)
  (let* ((safe-max (%coerce-max-partial-output-chars max-chars))
         (normalized (%normalize-inline-budget-text text))
         (length (length normalized)))
    (cond
      ((<= length safe-max)
       normalized)
      ((<= safe-max 3)
       (subseq normalized 0 safe-max))
      (t
       (concatenate 'string
                    (subseq normalized 0 (- safe-max 3))
                    "...")))))

(defun %budget-default-partial-output (kind used budget context-summary max-chars)
  (%bounded-budget-output
   (if (and (stringp context-summary)
            (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                        context-summary))))
       (format nil "Budget exhausted (~A ~A/~A). Partial summary: ~A"
               (string-downcase (symbol-name kind))
               used
               budget
               context-summary)
       (format nil "Budget exhausted (~A ~A/~A). Partial summary unavailable."
               (string-downcase (symbol-name kind))
               used
               budget))
   max-chars))

(defun default-budget-exhaustion-restart-selector (condition)
  "Default budget restart policy: summarize-and-finish."
  (declare (ignore condition))
  'summarize-and-finish)

(defun %invoke-budget-restart-decision (condition decision)
  (multiple-value-bind (name args)
      (%decision-restart-and-args decision)
    (when (and name
               (member name +budget-restart-names+ :test #'eq))
      (let ((restart (find-restart name condition)))
        (when restart
          (apply #'invoke-restart restart args))))))

(defun handle-budget-exhaustion (&key
                                   (kind :token)
                                   (used 0)
                                   (budget 0)
                                   context-summary
                                   (max-partial-output-chars
                                     +budget-partial-output-max-chars+))
  "Signal a recoverable budget exhaustion condition with restart options.

Returns a plist resolution with :ACTION set to one of:
- :extend-budget
- :summarize-and-finish
- :abort-task"
  (let* ((safe-max (%coerce-max-partial-output-chars max-partial-output-chars))
         (default-summary (%budget-default-partial-output kind
                                                          used
                                                          budget
                                                          context-summary
                                                          safe-max)))
    (handler-bind
        ((budget-exhausted-condition
           (lambda (condition)
             (let ((selector *budget-exhaustion-restart-selector*))
               (when (functionp selector)
                 (%invoke-budget-restart-decision condition
                                                  (funcall selector condition)))))))
      (restart-case
          (error 'budget-exhausted-condition
                 :kind kind
                 :used used
                 :budget budget
                 :context-summary context-summary
                 :message (format nil "~A budget exhausted (~A/~A)."
                                  (string-downcase (symbol-name kind))
                                  used
                                  budget))
        (extend-budget (&optional
                          (extra-budget
                            (max 1 (truncate (max 1 (if (integerp budget)
                                                         budget
                                                         1))
                                             2))))
          :report "Increase budget and continue."
          (list :action :extend-budget
                :kind kind
                :used used
                :budget budget
                :extra-budget (max 1 (if (and (integerp extra-budget)
                                              (> extra-budget 0))
                                         extra-budget
                                         1))))
        (summarize-and-finish (&optional (partial-output default-summary))
          :report "Produce bounded partial output and finish."
          (list :action :summarize-and-finish
                :kind kind
                :used used
                :budget budget
                :max-partial-output-chars safe-max
                :partial-output (%bounded-budget-output partial-output safe-max)))
        (abort-task (&optional (reason "Budget exhausted; task aborted."))
          :report "Abort the current task."
          (list :action :abort-task
                :kind kind
                :used used
                :budget budget
                :reason (if (stringp reason)
                            reason
                            (princ-to-string reason))))))))

(defun %argument-as-string (arguments key)
  (let ((value (and (hash-table-p arguments)
                    (gethash key arguments))))
    (when value
      (typecase value
        (string value)
        (pathname (namestring value))
        (symbol (symbol-name value))
        (t (princ-to-string value))))))

;; Tool metadata lookup delegated to find-tool-metadata in deftool.lisp

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

;; Name normalization delegated to normalize-name in util.lisp

;; %invoke-tool-core removed — all tool execution routes through the pipeline
;; (execute-tool) which enforces permissions via %check-permission-or-signal.

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
    (if (null restarts)
        nil
        (%prompt-user-recovery-decision condition restarts query-io))))

(unless (functionp *supervised-restart-selector*)
  (setf *supervised-restart-selector* #'default-supervised-restart-selector))

(unless (functionp *budget-exhaustion-restart-selector*)
  (setf *budget-exhaustion-restart-selector*
        #'default-budget-exhaustion-restart-selector))

;; %execute-tool-with-restarts and execute-tool-with-restarts removed from
;; conditions.lisp — canonical versions live in pipeline.lisp and route through
;; (execute-tool) for permission enforcement and event emission.
