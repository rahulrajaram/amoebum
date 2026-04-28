(in-package :amoebum)

;;; NXT-279: chat streaming subsystem extracted from chat.lisp.
;;; Both files share the :amoebum package, so call sites in chat.lisp
;;; (and elsewhere) continue to resolve unchanged. This file contains
;;; the stream event handler pipeline, tool-call tracking/execution,
;;; budget enforcement, and status summary helpers. It deliberately
;;; excludes: rendering helpers (preview lines, overlay widget),
;;; styled-line cache mutations, and public thinking-overlay API.
;;; Must load immediately after src/ui/chat-state and before src/ui/chat.

(defun %start-agent-continuation-stream (chat-state)
  "Start a new streaming response to continue the agentic tool loop.
Like %start-streaming-assistant-response but without adding a new user message."
  (when (token-stream-active-p (chat-ui-state-stream-state chat-state))
    (return-from %start-agent-continuation-stream nil))
  (%start-streaming-turn
   chat-state
   :prompt ""
   :history (copy-list (chat-ui-state-messages chat-state))
   :system-prompt (chat-ui-state-stream-system-prompt chat-state)))

(defun %resolve-chat-tools (chat-state)
  "Return the tool definitions list to pass to the streaming API.
Falls back to the global *toolset* when stream-tools is nil."
  (or (chat-ui-state-stream-tools chat-state)
      (and (boundp '*toolset*)
           (pseudopod:toolset-p *toolset*)
           (pseudopod:toolset-tools *toolset*))))

(defun %resolve-chat-toolset (chat-state)
  (let ((stream-tools (chat-ui-state-stream-tools chat-state)))
    (cond
      ((pseudopod:toolset-p stream-tools)
       stream-tools)
      ((and (boundp '*toolset*)
            (pseudopod:toolset-p *toolset*))
       *toolset*)
      (t
       (pseudopod:make-toolset)))))

(defun %chat-permission-mode ()
  (let ((config (%chat-config)))
    (and (config-p config)
         (config-permission-mode config))))

(defun %append-step-history-delta! (chat-state step-history)
  (let ((existing-count (length (chat-ui-state-messages chat-state))))
    (dolist (message (nthcdr existing-count (or step-history '())))
      (when (pseudopod:message-p message)
        (chat-ui-append-message chat-state message)))))

(defun %invoke-pseudopod-step (client step-messages tools toolset chat-state context)
  (handler-case
      (pseudopod:step
       client
       (pseudopod:make-agent-step-context
        :messages step-messages
        :tools tools
        :toolset toolset
        :max-steps (%chat-effective-max-iterations chat-state)
        :on-tool-call
        (lambda (tool-call)
          (values t (execute-tool tool-call context)))
        :on-tool-error
        (lambda (tool-call condition)
          (unless (typep condition 'tool-error)
            (publish (%context-event-bus chat-state)
                     (make-tool-error-event
                      :tool-name (pseudopod:tool-call-name tool-call)
                      :args (ignore-errors
                              (%decode-tool-call-arguments tool-call))
                      :condition (princ-to-string condition)))))))
    (program-error (condition)
      (if (search "odd number of &KEY arguments"
                  (princ-to-string condition)
                  :test #'char-equal)
          (pseudopod:step
           client
           :messages step-messages
           :tools tools
           :toolset toolset
           :max-steps (%chat-effective-max-iterations chat-state)
           :on-tool-call
           (lambda (tool-call)
             (values t (execute-tool tool-call context)))
           :on-tool-error
           (lambda (tool-call condition)
             (unless (typep condition 'tool-error)
               (publish (%context-event-bus chat-state)
                        (make-tool-error-event
                         :tool-name (pseudopod:tool-call-name tool-call)
                         :args (ignore-errors
                                 (%decode-tool-call-arguments tool-call))
                         :condition (princ-to-string condition))))))
          (error condition)))))

(defun %start-step-loop-assistant-response (chat-state)
  (let ((client (chat-ui-state-stream-client chat-state)))
    (when (pseudopod:client-p client)
      (let* ((toolset (%resolve-chat-toolset chat-state))
             (tools (%resolve-chat-tools chat-state))
             (model (pseudopod:client-model client))
             (base-url (pseudopod:client-base-url client))
             (default-messages (copy-list (chat-ui-state-messages chat-state)))
             (context (make-amoebum-context
                       :toolset toolset
                       :permission-mode (%chat-permission-mode)
                       :event-bus (%context-event-bus chat-state))))
        (multiple-value-bind (pre-status pre-results)
            (run-hooks :pre-llm-send default-messages tools model)
          (when (member pre-status '(:block :deny) :test #'eq)
            (conversation-transition! (%ensure-chat-conversation-state chat-state)
                                      :idle)
            (return-from %start-step-loop-assistant-response nil))
          (let* ((step-messages (%resolve-pre-llm-messages default-messages pre-results))
                 (step-request-id (format nil "step-~D" (%usdt-now-ms)))
                 (step-start-ms (%usdt-now-ms))
                 (step-status :ok)
                 (llm-probe-start
                   (usdt-probe-llm-request-start model base-url :step-loop step-request-id))
                 (step-result
                   (unwind-protect
                        (handler-case
                            (%invoke-pseudopod-step
                             client
                             step-messages
                             tools
                             toolset
                             chat-state
                             context)
                          (error (condition)
                            (setf step-status :error)
                            (error condition)))
                     (usdt-probe-llm-request-end model
                                                 base-url
                                                 :step-loop
                                                 step-request-id
                                                 (max 0 (- (%usdt-now-ms) step-start-ms))
                                                 :status step-status)))
                 (response (or (pseudopod:step-result-final-message step-result)
                               (pseudopod:step-result-last-message step-result))))
            (declare (ignore llm-probe-start))
            (run-hooks :on-step-complete
                       (pseudopod:step-result-steps step-result)
                       (max 0
                            (- (length (or (pseudopod:step-result-history step-result) '()))
                               (length (chat-ui-state-messages chat-state))))
                       (length (or (pseudopod:step-result-tool-results step-result) '())))
            (%append-step-history-delta!
             chat-state
             (pseudopod:step-result-history step-result))
            (%emit-post-llm-receive-hook step-result nil model)
            (%emit-post-receive-hook response)
            (conversation-transition! (%ensure-chat-conversation-state chat-state)
                                      :idle)))))))

(defun %resolve-chat-system-prompt (chat-state)
  (let* ((config (%chat-config))
         (project-root (and (config-p config)
                            (config-project-root config)))
         (tools (or (chat-ui-state-stream-tools chat-state)
                    *toolset*))
         (working-directory (or (ignore-errors (uiop:getcwd))
                                *default-pathname-defaults*))
         (assembled
           (ignore-errors
             (assemble-system-prompt
              :project-root project-root
              :cwd working-directory
              :toolset tools))))
    (or assembled
        (chat-ui-state-stream-system-prompt chat-state)
        +chat-stream-default-system-prompt+)))

(defun %make-stream-chunk-hook-callback ()
  (let ((chunk-index 0)
        (total-tokens 0))
    (lambda (chunk)
      (when (and (stringp chunk) (plusp (length chunk)))
        (incf chunk-index)
        (incf total-tokens (%token-stream-estimate-token-count chunk))
        (hook-chain :on-stream-chunk chunk chunk-index total-tokens))
      nil)))

(defun %prepare-streaming-turn-history (chat-state &key user-message continuationp)
  (let ((history (copy-list (chat-ui-state-messages chat-state))))
    (if continuationp
        history
        (remove-if
         (lambda (message)
           (and (pseudopod:message-p message)
                (string-equal (or (pseudopod:message-role message) "")
                              "assistant")
                (%blank-string-p (%message-content->text message))))
         history))))

(defun %prepare-streaming-turn-request (chat-state &key user-message continuationp)
  (let* ((prompt (if continuationp
                     ""
                     (%message-content->text user-message)))
         (history (%prepare-streaming-turn-history
                   chat-state
                   :user-message user-message
                   :continuationp continuationp))
         (system-prompt (if continuationp
                            (chat-ui-state-stream-system-prompt chat-state)
                            (%resolve-chat-system-prompt chat-state))))
    (list :prompt prompt
          :history history
          :system-prompt system-prompt
          :target-index (length history))))

(defun %streaming-turn-request-values (request)
  (values (getf request :prompt)
          (getf request :history)
          (getf request :system-prompt)))

(defun %begin-streaming-turn! (chat-state request)
  (%maybe-trim-demo-transcript!
   chat-state
   :keep-last-messages (max 1 (1- (%chat-ui-render-message-limit chat-state))))
  (setf (chat-ui-state-stream-scroll-follow-p chat-state) t)
  (when (getf request :system-prompt)
    (setf (chat-ui-state-stream-system-prompt chat-state)
          (getf request :system-prompt)))
  (streaming-markdown-renderer-reset
   (chat-ui-state-stream-markdown-renderer chat-state))
  (setf (chat-ui-state-stream-response-chunks chat-state) '())
  (conversation-transition! (%ensure-chat-conversation-state chat-state)
                            :streaming)
  (chat-ui-add-message chat-state "assistant" "" :partial t)
  (%clear-stream-tool-tracking! chat-state)
  request)

(defun %streaming-turn-runner-args (request)
  (multiple-value-bind (prompt history system-prompt)
      (%streaming-turn-request-values request)
    (list prompt history system-prompt)))

(defun %launch-streaming-turn! (chat-state request)
  (let ((runner (chat-ui-state-stream-runner chat-state))
        (stream-state (chat-ui-state-stream-state chat-state)))
    (%begin-streaming-turn! chat-state request)
    (token-stream-start
     stream-state
     (lambda (active-stream-state)
       (let ((*stream-chunk-hook-callback* (%make-stream-chunk-hook-callback)))
         (destructuring-bind (prompt history system-prompt)
             (%streaming-turn-runner-args request)
           (funcall runner
                    active-stream-state
                    prompt
                    history
                    :system-prompt system-prompt
                    :client (chat-ui-state-stream-client chat-state)
                    :tools (%resolve-chat-tools chat-state)))))
     :target-message-index (getf request :target-index)
     :budget-abort-threshold-percent
     (%stream-budget-abort-threshold-percent chat-state))))

(defun %start-streaming-turn (chat-state &key prompt history system-prompt)
  (let ((runner (chat-ui-state-stream-runner chat-state)))
    (when (functionp runner)
      (%launch-streaming-turn!
       chat-state
       (list :prompt prompt
             :history history
             :system-prompt system-prompt
             :target-index (length history))))))

(defun %maybe-start-streaming-runner-turn! (chat-state request)
  (when (functionp (chat-ui-state-stream-runner chat-state))
    (%launch-streaming-turn! chat-state request)
    t))

(defun %start-streaming-assistant-response (chat-state user-message)
  (when (and (pseudopod:message-p user-message)
             (not (token-stream-active-p (chat-ui-state-stream-state chat-state))))
    (let ((request (%prepare-streaming-turn-request
                    chat-state
                    :user-message user-message
                    :continuationp nil)))
      (unless (%maybe-start-streaming-runner-turn! chat-state request)
        (%start-step-loop-assistant-response chat-state)))))

;;; NXT-428: tool-completion ordering, event ingestion, and status/output
;;; helpers now live in src/ui/chat-stream/{tool-completion,stream-events,
;;; status-output}. This residual coordinator keeps streamed turn setup and
;;; step-loop orchestration only.
