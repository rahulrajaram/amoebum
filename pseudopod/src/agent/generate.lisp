(in-package :pseudopod)

(defparameter +default-step-max-steps+ 8)

(defstruct (generate-result (:constructor %make-generate-result))
  (id nil :type (or null string))
  (message nil)
  (usage nil)
  (response nil))

(defstruct (step-result (:constructor %make-step-result))
  (steps 0 :type integer)
  (history nil :type list)
  (final-message nil)
  (last-message nil)
  (max-steps-reached nil)
  (tool-results nil :type list))

(defstruct (agent-step-context
            (:constructor make-agent-step-context
                (&key
                 (system-prompt "You are a helpful assistant.")
                 (user-prompt "")
                 messages
                 toolset
                 tools
                 (max-steps +default-step-max-steps+)
                 on-tool-call
                 on-tool-result
                 on-tool-error)))
  (system-prompt "You are a helpful assistant." :type string)
  (user-prompt "" :type string)
  (messages nil)
  (toolset nil)
  (tools nil)
  (max-steps +default-step-max-steps+ :type integer)
  (on-tool-call nil)
  (on-tool-result nil)
  (on-tool-error nil)
  (history nil :type list)
  (new-messages nil :type list)
  (resolved-toolset nil)
  (tool-results nil :type list)
  (steps 0 :type integer)
  (last-message nil))

(defun %coerce-history-message (message)
  (cond
    ((message-p message) message)
    ((hash-table-p message) (hash-to-message message))
    (t (error "Expected message struct or hash-table, got ~S" message))))

(defun %agent-sequence->list (sequence)
  (cond
    ((null sequence) nil)
    ((listp sequence) sequence)
    ((vectorp sequence)
     (loop for item across sequence collect item))
    (t (error "Expected a list or vector, got ~S" sequence))))

(defun %normalize-step-history (system-prompt user-prompt messages)
  (if messages
      (mapcar #'%coerce-history-message (%agent-sequence->list messages))
      (list (make-message :role "system" :content system-prompt)
            (make-message :role "user" :content user-prompt))))

(defun %agent-coerce-request-tool (tool)
  (cond
    ((tool-definition-p tool) (tool-definition-to-hash tool))
    ((hash-table-p tool) tool)
    (t (error "Expected tool-definition or hash-table, got ~S" tool))))

(defun %normalize-generate-tools (tools toolset)
  (let ((source (or tools
                    (and (toolset-p toolset) (toolset-tools toolset)))))
    (when source
      (mapcar #'%agent-coerce-request-tool (%agent-sequence->list source)))))

(defun %assistant-message-from-response (response)
  (let* ((choices (and (hash-table-p response) (gethash "choices" response)))
         (choice (%first-item choices))
         (raw-message (and (hash-table-p choice) (gethash "message" choice))))
    (if (hash-table-p raw-message)
        (hash-to-message raw-message)
        (error 'pseudopod-parse-error
               :message "Moonshot response missing assistant message."
               :payload response))))

(defun generate (client &key
                          (system-prompt "You are a helpful assistant.")
                          (user-prompt "")
                          messages
                          tools
                          toolset)
  "Generate a single assistant message from the model."
  (let* ((resolved-tools (%normalize-generate-tools tools toolset))
         (response (chat-completion client
                                    user-prompt
                                    :system-prompt system-prompt
                                    :messages messages
                                    :tools resolved-tools))
         (message (%assistant-message-from-response response)))
    (when (and (null (message-content message))
               (null (message-tool-calls message)))
      (error 'pseudopod-parse-error
             :message "Moonshot response was empty (no content and no tool calls)."
             :payload response))
    (%make-generate-result
     :id (let ((id (and (hash-table-p response) (gethash "id" response))))
           (and (stringp id) id))
     :message message
     :usage (extract-usage response)
     :response response)))

(defun %make-tool-result-record (tool-call output)
  (list :id (tool-call-id tool-call)
        :name (tool-call-name tool-call)
        :output output))

(defun %prepare-agent-step-context (context)
  (unless (agent-step-context-p context)
    (error "Expected AGENT-STEP-CONTEXT, got ~S" context))
  (unless (and (integerp (agent-step-context-max-steps context))
               (>= (agent-step-context-max-steps context) 1))
    (error "MAX-STEPS must be an integer >= 1, got ~S"
           (agent-step-context-max-steps context)))
  (setf (agent-step-context-history context)
        (%normalize-step-history (agent-step-context-system-prompt context)
                                 (agent-step-context-user-prompt context)
                                 (agent-step-context-messages context)))
  (setf (agent-step-context-resolved-toolset context)
        (or (agent-step-context-toolset context) (make-toolset)))
  (setf (agent-step-context-new-messages context) nil)
  (setf (agent-step-context-tool-results context) nil)
  (setf (agent-step-context-steps context) 0)
  (setf (agent-step-context-last-message context) nil)
  context)

(defun %step-current-history (context)
  (let ((new-messages (agent-step-context-new-messages context)))
    (if new-messages
        (append (agent-step-context-history context)
                (nreverse (copy-list new-messages)))
        (agent-step-context-history context))))

(defun %step-push-message (context message)
  (push message (agent-step-context-new-messages context))
  context)

(defun %finalize-step-history (context)
  (let ((new-messages (agent-step-context-new-messages context)))
    (when new-messages
      (setf (agent-step-context-history context)
            (append (agent-step-context-history context)
                    (nreverse new-messages)))
      (setf (agent-step-context-new-messages context) nil)))
  (agent-step-context-history context))

(defun %dispatch-tool-call (context tool-call)
  (multiple-value-bind (handled-p handled-output)
      (if (agent-step-context-on-tool-call context)
          (funcall (agent-step-context-on-tool-call context) tool-call)
          (values nil nil))
    (if (eq handled-p t)
        handled-output
        (invoke-tool-call (agent-step-context-resolved-toolset context)
                          tool-call))))

(defun %handle-tool-call-error (context tool-call condition)
  (when (agent-step-context-on-tool-error context)
    (ignore-errors
      (funcall (agent-step-context-on-tool-error context) tool-call condition)))
  (format nil "Tool ~S failed: ~A"
          (tool-call-name tool-call)
          condition))

(defun %execute-tool-calls (context tool-calls)
  (dolist (tool-call tool-calls)
    (let* ((output (handler-case
                       (%dispatch-tool-call context tool-call)
                     (error (condition)
                       (%handle-tool-call-error context tool-call condition))))
           ;; I369: Sanitize tool output to prevent ANSI escape codes reaching LLM APIs
           (sanitized-output (sanitize-string-for-llm output))
           (result-record (%make-tool-result-record tool-call sanitized-output))
           (tool-message (make-message
                          :role "tool"
                          :name (tool-call-name tool-call)
                          :tool-call-id (tool-call-id tool-call)
                          :content sanitized-output)))
      (push result-record (agent-step-context-tool-results context))
      (%step-push-message context tool-message)
      (when (agent-step-context-on-tool-result context)
        (funcall (agent-step-context-on-tool-result context) result-record))))
  context)

(defun %assemble-step-response (context &key final-message max-steps-reached)
  (%make-step-result
   :steps (agent-step-context-steps context)
   :history (%finalize-step-history context)
   :final-message final-message
   :last-message (or final-message (agent-step-context-last-message context))
   :max-steps-reached max-steps-reached
   :tool-results (nreverse (agent-step-context-tool-results context))))

(defun step (client context)
  "Run a tool-aware generation loop until final output or MAX-STEPS."
  (%prepare-agent-step-context context)
  (loop while (< (agent-step-context-steps context)
                 (agent-step-context-max-steps context))
        do
           (incf (agent-step-context-steps context))
           (let* ((result (generate client
                                    :messages (%step-current-history context)
                                    :tools (agent-step-context-tools context)
                                    :toolset (agent-step-context-resolved-toolset context)))
                  (assistant-message (generate-result-message result))
                  (tool-calls (message-tool-calls assistant-message)))
             (setf (agent-step-context-last-message context) assistant-message)
             (%step-push-message context assistant-message)
             (unless tool-calls
               (return-from step
                 (%assemble-step-response context :final-message assistant-message)))
             (%execute-tool-calls context tool-calls)))
  (%assemble-step-response context :max-steps-reached t))
