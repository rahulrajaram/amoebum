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

(defun step (client &key
                      (system-prompt "You are a helpful assistant.")
                      (user-prompt "")
                      messages
                      toolset
                      tools
                      (max-steps +default-step-max-steps+)
                      on-tool-call
                      on-tool-result)
  "Run a tool-aware generation loop until final output or MAX-STEPS."
  (let ((history (%normalize-step-history system-prompt user-prompt messages))
        (resolved-toolset (or toolset (make-toolset)))
        (tool-results nil)
        (steps 0)
        (last-message nil)
        (new-messages nil))
    (unless (and (integerp max-steps) (>= max-steps 1))
      (error "MAX-STEPS must be an integer >= 1, got ~S" max-steps))
    (labels ((%current-history ()
               (if new-messages
                   (append history (nreverse (copy-list new-messages)))
                   history))
             (%push-message (msg)
               (push msg new-messages))
             (%finalize-history ()
               (when new-messages
                 (setf history (append history (nreverse new-messages)))
                 (setf new-messages nil))
               history))
      (loop while (< steps max-steps) do
        (incf steps)
        (let* ((result (generate client
                                 :messages (%current-history)
                                 :tools tools
                                 :toolset resolved-toolset))
               (assistant-message (generate-result-message result))
               (tool-calls (message-tool-calls assistant-message)))
          (setf last-message assistant-message)
          (%push-message assistant-message)
          (unless tool-calls
            (return-from step
              (%make-step-result
               :steps steps
               :history (%finalize-history)
               :final-message assistant-message
               :last-message assistant-message
               :max-steps-reached nil
               :tool-results (nreverse tool-results))))
          (dolist (tool-call tool-calls)
            (when on-tool-call
              (funcall on-tool-call tool-call))
            (let* ((output (invoke-tool-call resolved-toolset tool-call))
                   (result-record (%make-tool-result-record tool-call output))
                   (tool-message (make-message
                                  :role "tool"
                                  :name (tool-call-name tool-call)
                                  :tool-call-id (tool-call-id tool-call)
                                  :content output)))
              (push result-record tool-results)
              (%push-message tool-message)
              (when on-tool-result
                (funcall on-tool-result result-record))))))
      (%make-step-result
       :steps steps
       :history (%finalize-history)
       :final-message nil
       :last-message last-message
       :max-steps-reached t
       :tool-results (nreverse tool-results)))))
