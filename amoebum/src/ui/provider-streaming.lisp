(in-package :amoebum)

(defun %provider-message->hash-table (message)
  (cond
    ((hash-table-p message) message)
    ((pseudopod:message-p message) (pseudopod:message-to-hash message))
    (t message)))

(defun %provider-tool-call->struct (tool-call)
  (cond
    ((pseudopod:tool-call-p tool-call) tool-call)
    ((hash-table-p tool-call)
     (handler-case
         (pseudopod:hash-to-tool-call tool-call)
       (error () nil)))
    (t nil)))

(defun %provider-tool-call-vector->list (tool-calls)
  (cond
    ((null tool-calls) '())
    ((listp tool-calls) tool-calls)
    ((vectorp tool-calls) (coerce tool-calls 'list))
    (t (list tool-calls))))

(defun %emit-provider-tool-call-events (stream-state tool-call index)
  (token-stream-emit-tool-call-started stream-state tool-call)
  (token-stream-emit-tool-call-delta
   stream-state
   (list :type :tool-call-delta
         :index index
         :tool-call tool-call
         :tool-call-id (pseudopod:tool-call-id tool-call)
         :tool-name (pseudopod:tool-call-name tool-call)
         :arguments (pseudopod:tool-call-arguments tool-call)
         :arguments-complete-p t))
  (token-stream-emit-tool-call-argument-complete stream-state tool-call))

(defun stream-provider-chat (stream-state prompt messages
                            &key
                              (system-prompt +chat-stream-default-system-prompt+)
                              client
                              tools)
  (let ((provider (or (and (typep client 'pseudopod:provider) client)
                      (ignore-errors (resolve-provider)))))
    (if (or (null provider)
            (not (and (pseudopod:provider-api-key provider)
                      (stringp (pseudopod:provider-api-key provider))
                      (plusp (length (pseudopod:provider-api-key provider)))))
        (stream-pseudopod-chat stream-state
                              prompt
                              messages
                              :system-prompt system-prompt
                              :client nil
                              :tools tools)
        (let* ((provider-messages (mapcar #'%provider-message->hash-table messages))
               (result (pseudopod:send-streaming-completion
                        provider
                        provider-messages
                        (lambda (chunk)
                          (cond
                            ((stringp chunk)
                             (token-stream-emit-chunk stream-state chunk))
                            ((and (hash-table-p chunk)
                                  (eq (getf chunk :type) :tool-call-delta)
                                  (getf chunk :tool-call))
                             (token-stream-emit-tool-call-delta
                              stream-state chunk))
                            ((and (hash-table-p chunk)
                                  (eq (getf chunk :type) :tool-call-started)
                                  (getf chunk :tool-call))
                             (token-stream-emit-tool-call-started
                              stream-state (getf chunk :tool-call)))
                            ((and (hash-table-p chunk)
                                  (eq (getf chunk :type) :tool-call-argument-complete)
                                  (getf chunk :tool-call))
                             (token-stream-emit-tool-call-argument-complete
                              stream-state (getf chunk :tool-call)))))
                        :system-prompt system-prompt
                        :tools tools))
               (tool-calls (and (hash-table-p result)
                                (gethash "tool_calls" result))))
          (let ((index 0))
            (dolist (tool-call-raw (%provider-tool-call-vector->list tool-calls))
              (let ((tool-call (%provider-tool-call->struct tool-call-raw)))
                (when (and tool-call (pseudopod:tool-call-p tool-call))
                  (%emit-provider-tool-call-events stream-state tool-call index)
                  (incf index)))))))))
