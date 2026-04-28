(in-package :amoebum)

(defvar *capability-gap-delegation-function* nil
  "When non-nil, called with (condition action options) to delegate a capability gap.")
(defvar *capability-gap-install-function* nil
  "When non-nil, called with (condition action options) to request/install a missing capability.")
(defvar *tool-error-llm-recovery-function* nil
  "When non-nil, a function called with (condition tool-name arguments) to attempt LLM-driven recovery from tool errors.")
(defparameter +missing-tool-argument-recovery-modes+
  '(:prompt :structured-error :disabled))
(defparameter *missing-tool-argument-recovery-mode* :prompt
  "Controls execute-tool recovery for TOOL-MISSING-ARGUMENT.
:prompt asks the user for missing required args in supervised interactive runs.
:structured-error returns a stable JSON error payload instead of signaling.
:disabled preserves raw tool-missing-argument signaling.")

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

(defun %capability-gap-action-specs ()
  (list (list :restart "delegate-capability-gap"
              :action "delegate"
              :description "Delegate the missing capability to a peer or specialized agent.")
        (list :restart "install-missing-capability"
              :action "install"
              :description "Install or enable the missing capability before retrying.")
        (list :restart "ask-user"
              :action "ask-user"
              :description "Ask the user which recovery path to take.")))

(defun %capability-gap-contract (tool-name arguments &key capability-name reason-code)
  (list :kind "capability_gap"
        :tool-name (or tool-name "")
        :capability-name (or capability-name tool-name "")
        :reason-code (string-downcase (symbol-name (or reason-code :capability-gap)))
        :arguments (%copy-arguments-to-hash-table arguments)
        :available-actions (%capability-gap-action-specs)))

(defun %capability-gap-recovery-payload (condition status action options)
  (let ((payload (%make-equal-hash-table))
        (contract (capability-gap-recovery-contract condition)))
    (setf (gethash "kind" payload) "capability_gap_recovery"
          (gethash "status" payload) status
          (gethash "action" payload) action
          (gethash "tool" payload) (or (tool-error-tool-name condition) "")
          (gethash "capability" payload)
          (or (capability-gap-capability-name condition)
              (tool-error-tool-name condition)
              "")
          (gethash "message" payload)
          (princ-to-string condition))
    (when contract
      (setf (gethash "contract" payload) contract))
    (when options
      (setf (gethash "options" payload) options))
    payload))

(defun %capability-gap-recovery-result-text (condition status action &optional options)
  (%encode-json-arguments
   (%capability-gap-recovery-payload condition status action options)))

(defun %delegate-capability-gap (condition &optional options)
  (if (functionp *capability-gap-delegation-function*)
      (or (funcall *capability-gap-delegation-function*
                   condition
                   :delegate
                   options)
          (%capability-gap-recovery-result-text
           condition
           "delegated"
           "delegate"
           options))
      (%capability-gap-recovery-result-text
       condition
       "delegation-requested"
       "delegate"
       options)))

(defun %install-missing-capability (condition &optional options)
  (if (functionp *capability-gap-install-function*)
      (or (funcall *capability-gap-install-function*
                   condition
                   :install
                   options)
          (%capability-gap-recovery-result-text
           condition
           "install-requested"
           "install"
           options))
      (%capability-gap-recovery-result-text
       condition
       "install-requested"
       "install"
       options)))

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
         (tool-call (%make-restart-tool-call normalized-tool-name normalized-arguments))
         (captured-condition nil))
    (handler-bind
        ((tool-error
           (lambda (condition)
             (setf captured-condition condition)
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
        (delegate-capability-gap (&optional options)
          :report "Delegate this missing capability while preserving task continuity."
          (%delegate-capability-gap
           (or captured-condition
               (error 'amoebum-error
                      :message (format nil "Tool ~A has no captured failure context for delegation."
                                       normalized-tool-name)))
           options))
        (install-missing-capability (&optional options)
          :report "Install or enable the missing capability before retrying."
          (%install-missing-capability
           (or captured-condition
               (error 'amoebum-error
                      :message (format nil "Tool ~A has no captured failure context for installation recovery."
                                       normalized-tool-name)))
           options))
        (skip-tool-call ()
          :report "Skip this tool call and continue."
          (format nil "Tool ~A skipped by recovery policy." normalized-tool-name))
        (abort-step ()
          :report "Abort the current step and propagate failure."
          (error 'amoebum-error
                 :message (format nil "Tool ~A aborted current step." normalized-tool-name)))
        (ask-user ()
          :report "Ask user for guidance before retrying."
          (if (typep captured-condition 'capability-gap)
              (%capability-gap-recovery-result-text
               captured-condition
               "user-guidance-required"
               "ask-user")
              (format nil "Tool ~A requires user guidance to continue."
                      normalized-tool-name)))
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
