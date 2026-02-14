(in-package :pseudopod)

(defstruct (conversation (:constructor %make-conversation))
  (client nil :type client)
  (system-prompt "You are a helpful assistant." :type string)
  (history nil :type list)
  (toolset nil))

(defun %conversation-sequence->list (sequence)
  (cond
    ((null sequence) nil)
    ((listp sequence) sequence)
    ((vectorp sequence)
     (loop for item across sequence collect item))
    (t
     (error "Expected a list or vector, got ~S" sequence))))

(defun %coerce-conversation-message (message)
  (cond
    ((message-p message) message)
    ((hash-table-p message) (hash-to-message message))
    (t
     (error "Expected message struct or hash-table, got ~S" message))))

(defun %normalize-conversation-history (history)
  (mapcar #'%coerce-conversation-message (%conversation-sequence->list history)))

(defun make-conversation (&key
                            client
                            (system-prompt "You are a helpful assistant.")
                            history
                            toolset)
  "Create mutable conversation state for multi-turn interactions."
  (unless (client-p client)
    (error "CLIENT must be a pseudopod client, got ~S" client))
  (%make-conversation
   :client client
   :system-prompt system-prompt
   :history (%normalize-conversation-history history)
   :toolset toolset))

(defun %append-conversation-message (conversation message)
  (unless (conversation-p conversation)
    (error "Expected conversation struct, got ~S" conversation))
  (let ((normalized (%coerce-conversation-message message)))
    (setf (conversation-history conversation)
          (append (conversation-history conversation)
                  (list normalized)))
    normalized))

(defun %conversation-request-messages (conversation)
  (append
   (list (make-message
          :role "system"
          :content (conversation-system-prompt conversation)))
   (conversation-history conversation)))

(defun %resolve-conversation-tools (conversation tools)
  (cond
    ((null tools)
     (let ((toolset (conversation-toolset conversation)))
       (and (toolset-p toolset)
            (toolset-tools toolset))))
    ((toolset-p tools)
     (toolset-tools tools))
    (t tools)))

(defun %strip-leading-system-message (history)
  (let ((messages (mapcar #'%coerce-conversation-message history)))
    (if (and messages
             (string= "system" (message-role (first messages))))
        (rest messages)
        messages)))

(defun conversation-add-user (conversation content &key name)
  "Append a user message to CONVERSATION history."
  (%append-conversation-message
   conversation
   (make-message :role "user" :name name :content content)))

(defun conversation-add-assistant (conversation content &key name tool-calls)
  "Append an assistant message to CONVERSATION history."
  (%append-conversation-message
   conversation
   (make-message :role "assistant"
                 :name name
                 :content content
                 :tool-calls tool-calls)))

(defun %normalize-tool-result-name-and-id (tool-call-or-name tool-call-id)
  (cond
    ((tool-call-p tool-call-or-name)
     (values (tool-call-name tool-call-or-name)
             (or tool-call-id (tool-call-id tool-call-or-name))))
    ((or (stringp tool-call-or-name)
         (symbolp tool-call-or-name))
     (values (princ-to-string tool-call-or-name)
             tool-call-id))
    (t
     (error "Expected TOOL-CALL-OR-NAME to be tool-call/string/symbol, got ~S"
            tool-call-or-name))))

(defun conversation-add-tool-result (conversation tool-call-or-name content
                                     &key tool-call-id)
  "Append a tool-role message to CONVERSATION history."
  (multiple-value-bind (name id)
      (%normalize-tool-result-name-and-id tool-call-or-name tool-call-id)
    (%append-conversation-message
     conversation
     (make-message :role "tool"
                   :name name
                   :tool-call-id id
                   :content content))))

(defun conversation-complete (conversation &key user-prompt user-name tools)
  "Run one model completion over conversation history and append the assistant reply."
  (when user-prompt
    (conversation-add-user conversation user-prompt :name user-name))
  (let* ((response-message
           (chat-completion*
            (conversation-client conversation)
            ""
            :messages (%conversation-request-messages conversation)
            :tools (%resolve-conversation-tools conversation tools))))
    (%append-conversation-message conversation response-message)))

(defun conversation-step (conversation &key
                                         user-prompt
                                         user-name
                                         tools
                                         toolset
                                         (max-steps +default-step-max-steps+)
                                         on-tool-call
                                         on-tool-result)
  "Run tool-loop step orchestration and replace conversation history with the result."
  (when user-prompt
    (conversation-add-user conversation user-prompt :name user-name))
  (let* ((resolved-toolset (or toolset
                               (conversation-toolset conversation)
                               (make-toolset)))
         (result
           (step (conversation-client conversation)
                 :messages (%conversation-request-messages conversation)
                 :tools (%resolve-conversation-tools conversation tools)
                 :toolset resolved-toolset
                 :max-steps max-steps
                 :on-tool-call on-tool-call
                 :on-tool-result on-tool-result)))
    (setf (conversation-toolset conversation) resolved-toolset)
    (setf (conversation-history conversation)
          (%strip-leading-system-message (step-result-history result)))
    result))

(defun conversation-clear (conversation)
  "Reset conversation history."
  (unless (conversation-p conversation)
    (error "Expected conversation struct, got ~S" conversation))
  (setf (conversation-history conversation) nil)
  conversation)
