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
         (tools (%resolve-chat-tools chat-state))
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

;;; ---- block-A: stream tool-call preview tracking (originally chat.lisp:991-1134) ----
(defun %stream-tool-call-preview-key (index tool-call-id tool-name arguments)
  (cond
    ((and (stringp tool-call-id) (plusp (length tool-call-id)))
     (concatenate 'string "id:" tool-call-id))
    ((and (stringp tool-name) (plusp (length tool-name)))
     (concatenate 'string "name:" tool-name))
    ((and (stringp arguments) (plusp (length arguments)))
     (concatenate 'string "args:" arguments))
    ((integerp index) index)
    (t
     :unknown)))

(defun %ensure-stream-tool-call-preview (chat-state key &optional index)
  (let* ((table (chat-ui-state-stream-tool-calls chat-state))
         (entry (and (hash-table-p table) (gethash key table)))
         (stable-entry
           (or entry
               (and (hash-table-p table)
                    (integerp index)
                    (not (eq key index))
                    (gethash index table)))))
    (if (and stable-entry (not entry) (integerp index) (not (eq key index)))
        (progn
          (remhash index table)
          (setf (gethash key table) stable-entry)
          stable-entry)
        (or entry
            (let ((fresh (list :key key
                               :index nil
                               :tool-name nil
                               :tool-call-id nil
                               :arguments nil
                               :started-p nil
                               :arguments-complete-p nil
                               :executed-p nil
                               :completed-p nil
                               :execution-error nil
                               :result nil
                               :malformed-p nil)))
              (setf (gethash key table) fresh)
              fresh)))))

(defun %find-stream-tool-call-preview (chat-state key &optional index)
  (let ((table (chat-ui-state-stream-tool-calls chat-state)))
    (and (hash-table-p table)
         (or (gethash key table)
             (and (integerp index)
                  (not (eq key index))
                  (gethash index table))))))

(defun %normalize-stream-tool-name (tool-name)
  (let ((value (if (symbolp tool-name)
                   (symbol-name tool-name)
                   tool-name)))
    (and (stringp value)
         (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                      (string-downcase value)))
                (normalized (if (find #\_ trimmed) (substitute #\- #\_ trimmed)
                              trimmed)))
           (and (plusp (length normalized))
                normalized)))))

(defun %normalize-stream-tool-call (tool-call)
  (if (pseudopod:tool-call-p tool-call)
      (let ((normalized-name (%normalize-stream-tool-name
                             (pseudopod:tool-call-name tool-call)))
            (name (pseudopod:tool-call-name tool-call)))
        (if (and (stringp normalized-name)
                 (not (string= name normalized-name)))
            (pseudopod:make-tool-call
             :id (pseudopod:tool-call-id tool-call)
             :name normalized-name
             :arguments (pseudopod:tool-call-arguments tool-call)
             :extras (pseudopod:tool-call-extras tool-call))
            tool-call))
      nil))

(defun %stream-tool-call-from-event (event)
  (let ((tool-call (getf event :tool-call)))
    (if (pseudopod:tool-call-p tool-call)
        (%normalize-stream-tool-call tool-call)
        (let* ((tool-name (getf event :tool-name))
               (normalized-name (%normalize-stream-tool-name tool-name))
               (arguments (getf event :arguments))
               (tool-call-id (getf event :tool-call-id)))
          (when (and (stringp normalized-name) (plusp (length normalized-name)))
            (pseudopod:make-tool-call
             :id (and (stringp tool-call-id) tool-call-id)
             :name normalized-name
             :arguments (and (stringp arguments) arguments)))))))

(defun %stream-tool-call-preview-signature (chat-state)
  (let (items)
    (maphash (lambda (key value)
               (declare (ignore key))
               (when (listp value)
                 (push (list (getf value :index)
                             (getf value :tool-name)
                             (getf value :tool-call-id)
                             (getf value :arguments)
                             (not (null (getf value :started-p)))
                             (not (null (getf value :arguments-complete-p)))
                             (not (null (getf value :executed-p)))
                             (getf value :execution-error))
                       items)))
             (chat-ui-state-stream-tool-calls chat-state))
    (sort items #'string<
          :key (lambda (item)
                 (with-output-to-string (out)
                   (dolist (field item)
                     (write-string (princ-to-string field) out)
                     (write-char #\| out)))))))

(defun %update-stream-tool-call-preview! (chat-state event)
  (let* ((tool-call (%stream-tool-call-from-event event))
         (index (getf event :index))
         (tool-name (or (and (pseudopod:tool-call-p tool-call)
                             (pseudopod:tool-call-name tool-call))
                        (getf event :tool-name)))
         (tool-call-id (or (and (pseudopod:tool-call-p tool-call)
                                (pseudopod:tool-call-id tool-call))
                           (getf event :tool-call-id)))
         (arguments (or (and (pseudopod:tool-call-p tool-call)
                             (pseudopod:tool-call-arguments tool-call))
                        (getf event :arguments)))
         (key (%stream-tool-call-preview-key index tool-call-id tool-name arguments))
         (entry (%ensure-stream-tool-call-preview chat-state key index))
         (kind (or (getf event :type) (getf event :kind))))
    (when (integerp index)
      (setf (getf entry :index) index))
    (when (and (stringp tool-name) (plusp (length tool-name)))
      (setf (getf entry :tool-name) tool-name))
    (when (and (stringp tool-call-id) (plusp (length tool-call-id)))
      (setf (getf entry :tool-call-id) tool-call-id))
    (when (stringp arguments)
      (setf (getf entry :arguments) arguments))
    (setf (getf entry :started-p)
          (or (getf entry :started-p)
              (eq kind :tool-call-started)
              (eq kind :tool-call-argument-complete)))
    (setf (getf entry :arguments-complete-p)
          (or (getf entry :arguments-complete-p)
              (eq kind :tool-call-argument-complete)))
    entry))

;;; ---- block-B: clear-stream-tool-tracking (originally chat.lisp:1148-1163) ----
(defun %clear-stream-tool-tracking! (chat-state)
  (let ((tool-calls (chat-ui-state-stream-tool-calls chat-state))
        (executed (chat-ui-state-stream-executed-tool-call-keys chat-state))
        (journal (chat-ui-state-stream-event-journal chat-state)))
    (when (hash-table-p tool-calls)
      (clrhash tool-calls))
    (when (hash-table-p executed)
      (clrhash executed))
    (when (stream-event-journal-p journal)
      (stream-event-journal-clear! journal))
    (when (typep (chat-ui-state-stream-turn-snapshot chat-state)
                 'pseudopod:stream-turn-snapshot)
      (pseudopod:reset-stream-turn-snapshot!
       (chat-ui-state-stream-turn-snapshot chat-state)))
    (setf (chat-ui-state-stream-completion-pending-p chat-state) nil)
    chat-state))

;;; ---- block-C: execution status through has-id-p (originally chat.lisp:1260-1393) ----
(defun %set-stream-tool-call-execution-status! (chat-state preview-key
                                                &key executed-p execution-error
                                                     result malformed-p
                                                     completed-p)
  (let* ((table (chat-ui-state-stream-tool-calls chat-state))
         (entry (and (hash-table-p table)
                     (gethash preview-key table))))
    (when entry
      (when executed-p
        (setf (getf entry :executed-p) t))
      (when execution-error
        (setf (getf entry :execution-error) execution-error))
      (when completed-p
        (setf (getf entry :completed-p) t))
      (when result
        (setf (getf entry :result) result))
      (when malformed-p
        (setf (getf entry :malformed-p) t))
      (setf (gethash preview-key table) entry))
    entry))

(defun %stream-tool-call-completion-pending-p (chat-state)
  (let ((table (chat-ui-state-stream-tool-calls chat-state))
        (pending nil))
    (maphash
     (lambda (_key entry)
       (declare (ignore _key))
       (when (and (listp entry)
                  (getf entry :executed-p)
                  (not (getf entry :completed-p)))
         (setf pending t)))
     table)
    pending))

(defun %maybe-finalize-streaming-completion-pending-state (chat-state)
  (when (and (chat-ui-state-stream-completion-pending-p chat-state)
             (not (%stream-tool-call-completion-pending-p chat-state)))
    (%maybe-finalize-streaming-assistant-on-complete chat-state)))

(defun %set-tool-call-result! (chat-state event)
  (let* ((tool-call (%stream-tool-call-from-event event))
         (preview-entry (%update-stream-tool-call-preview! chat-state event))
         (preview-key (or (getf event :preview-key)
                          (and (listp preview-entry) (getf preview-entry :key)))))
    (%set-stream-tool-call-execution-status!
     chat-state
     preview-key
     :result (or (getf event :result) "")
     :execution-error (getf event :execution-error)
     :completed-p t)
    chat-state))

(defun %maybe-finalize-streaming-assistant-on-complete (chat-state)
  (let* ((conversation (%ensure-chat-conversation-state chat-state))
         (tool-call-entries (%collect-stream-tool-calls chat-state))
         (malformed-names (%collect-malformed-tool-calls chat-state)))
    (setf (chat-ui-state-stream-completion-pending-p chat-state) nil)
    (when tool-call-entries
      (%set-assistant-message-tool-calls! chat-state tool-call-entries))
    (%finalize-streaming-assistant-message chat-state :partialp nil)
    (let ((assistant-response (%stream-target-assistant-response chat-state)))
      (when (and assistant-response
                 (plan-mode-active-p))
        (ignore-errors
          (capture-plan-steps-from-response
           (%message-content->text assistant-response)
           :state (current-plan-mode-state))))
      (%resolve-stream-terminal-outcome
       chat-state
       conversation
       assistant-response
       tool-call-entries
       malformed-names))))

(defun %stream-turn-can-continue-p (chat-state)
  (< (chat-ui-state-agentic-iteration-count chat-state)
     (%chat-effective-max-iterations chat-state)))

(defun %stream-terminal-phase-context (chat-state conversation assistant-response
                                       tool-call-entries malformed-names)
  (list :chat-state chat-state
        :conversation conversation
        :assistant-response assistant-response
        :tool-call-entries tool-call-entries
        :malformed-names malformed-names
        :can-continue-p (%stream-turn-can-continue-p chat-state)))

(defun %append-tool-results-and-clear! (chat-state tool-call-entries)
  (when tool-call-entries
    (%append-tool-result-messages! chat-state tool-call-entries))
  (%clear-stream-tool-tracking! chat-state))

(defun %start-tool-continuation! (chat-state tool-call-entries)
  (%append-tool-results-and-clear! chat-state tool-call-entries)
  (incf (chat-ui-state-agentic-iteration-count chat-state))
  (%start-agent-continuation-stream chat-state))

(defun %start-tool-retry! (chat-state tool-call-entries malformed-names)
  (%append-tool-results-and-clear! chat-state tool-call-entries)
  (chat-ui-add-message chat-state "user"
                       (%malformed-tool-call-retry-message malformed-names))
  (incf (chat-ui-state-agentic-iteration-count chat-state))
  (%start-agent-continuation-stream chat-state))

(defun %finish-stream-turn-with-max-iterations! (chat-state conversation tool-call-entries)
  (%append-tool-results-and-clear! chat-state tool-call-entries)
  (chat-ui-add-message chat-state "assistant"
                       "[Agentic loop stopped: max iterations reached]")
  (conversation-transition! conversation :idle)
  (%checkpoint-after-turn chat-state conversation))

(defun %finish-stream-turn-with-answer! (chat-state conversation assistant-response)
  (%clear-stream-tool-tracking! chat-state)
  (%emit-post-receive-hook assistant-response)
  (conversation-transition! conversation :idle)
  (%checkpoint-after-turn chat-state conversation))

(defun %stream-terminal-outcome-kind (context)
  (let ((tool-call-entries (getf context :tool-call-entries))
        (malformed-names (getf context :malformed-names))
        (can-continue-p (getf context :can-continue-p)))
    (cond
      ((and malformed-names can-continue-p) :retry)
      ((and tool-call-entries can-continue-p) :tool-continuation)
      (tool-call-entries :max-iterations)
      (t :answer))))

(defun %apply-stream-terminal-outcome! (context)
  (let ((chat-state (getf context :chat-state))
        (conversation (getf context :conversation))
        (assistant-response (getf context :assistant-response))
        (tool-call-entries (getf context :tool-call-entries))
        (malformed-names (getf context :malformed-names)))
    (case (%stream-terminal-outcome-kind context)
      (:retry
       (%start-tool-retry! chat-state tool-call-entries malformed-names))
      (:tool-continuation
       (%start-tool-continuation! chat-state tool-call-entries))
      (:max-iterations
       (%finish-stream-turn-with-max-iterations! chat-state conversation tool-call-entries))
      (otherwise
       (%finish-stream-turn-with-answer! chat-state conversation assistant-response)))))

(defun %resolve-stream-terminal-outcome (chat-state conversation assistant-response
                                         tool-call-entries malformed-names)
  (%apply-stream-terminal-outcome!
   (%stream-terminal-phase-context
    chat-state
    conversation
    assistant-response
    tool-call-entries
    malformed-names)))

(defun %checkpoint-after-turn (chat-state conversation)
  "Fire an auto-checkpoint after a completed agent interaction turn."
  (declare (ignore chat-state))
  (ignore-errors
    (checkpoint-session :conversation conversation
                        :trigger :turn-complete
                        :auto-p t)))

(defun %stream-tool-call-execution-key (tool-call preview-key)
  (or (and (pseudopod:tool-call-p tool-call)
           (pseudopod:tool-call-id tool-call)
           (plusp (length (pseudopod:tool-call-id tool-call)))
           (concatenate 'string "id:" (pseudopod:tool-call-id tool-call)))
      (and (pseudopod:tool-call-p tool-call)
           (pseudopod:tool-call-name tool-call)
           (plusp (length (pseudopod:tool-call-name tool-call)))
           (concatenate 'string
                        "call:"
                        (pseudopod:tool-call-name tool-call)
                        ":"
                        (let ((arguments (pseudopod:tool-call-arguments tool-call)))
                          (if (stringp arguments)
                              arguments
                              (princ-to-string (or arguments "")))))
      preview-key)))

(defun %tool-call-has-id-p (tool-call)
  "Return T if TOOL-CALL has a non-empty tool-call-id."
  (and (pseudopod:tool-call-p tool-call)
       (stringp (pseudopod:tool-call-id tool-call))
       (plusp (length (pseudopod:tool-call-id tool-call)))))

;;; ---- block-D: serial tool executor + execution (originally chat.lisp:1395-1552) ----
;;; ---- Serial tool executor ----
;;; Tools run on a single background thread to serialize approval dialogs.
;;; The approval mechanism uses a single-slot *pending-approval*, so concurrent
;;; tool threads would race and cause timeouts.

(defvar *tool-executor-lock* (bt:make-lock "tool-executor-lock"))
(defvar *tool-executor-queue* '())
(defvar *tool-executor-condvar* (bt:make-condition-variable :name "tool-executor-cv"))
(defvar *tool-executor-thread* nil)

(defun %tool-executor-loop ()
  "Background loop: dequeue and run tool workers one at a time."
  (loop
    (let ((worker nil))
      (bt:with-lock-held (*tool-executor-lock*)
        (loop while (null *tool-executor-queue*)
              do (bt:condition-wait *tool-executor-condvar*
                                    *tool-executor-lock*
                                    :timeout 2))
        (when *tool-executor-queue*
          (setf worker (pop *tool-executor-queue*))))
      (when worker
        (handler-case (funcall worker)
          (error (c)
            (ptui.util.log:log-warn "tool-executor error: ~A" c)))))))

(defun %ensure-tool-executor-thread ()
  "Start the serial tool executor thread if not running."
  (bt:with-lock-held (*tool-executor-lock*)
    (when (or (null *tool-executor-thread*)
              (not (bt:thread-alive-p *tool-executor-thread*)))
      (setf *tool-executor-thread*
            (bt:make-thread #'%tool-executor-loop
                            :name "tool-executor")))))

(defun %enqueue-tool-worker (worker)
  "Add a tool worker to the serial execution queue."
  (%ensure-tool-executor-thread)
  (bt:with-lock-held (*tool-executor-lock*)
    (setf *tool-executor-queue*
          (append *tool-executor-queue* (list worker)))
    (bt:condition-notify *tool-executor-condvar*)))

(defun %stream-tool-call-execution-context (chat-state tool-call)
  (let* ((toolset (or (chat-ui-state-stream-tools chat-state) *toolset*))
         (config (%chat-config))
         (permission-mode (and (config-p config)
                               (config-permission-mode config)))
         (stream-state (chat-ui-state-stream-state chat-state))
         (tool-name (and (pseudopod:tool-call-p tool-call)
                         (pseudopod:tool-call-name tool-call))))
    (list :toolset toolset
          :permission-mode permission-mode
          :stream-state stream-state
          :tool-name tool-name)))

(defun %stream-tool-call-cancelled-p (context)
  (let ((stream-state (getf context :stream-state)))
    (and (typep stream-state 'token-stream-state)
         (token-stream-cancel-requested-p stream-state))))

(defun %execute-stream-tool-call-now (chat-state tool-call preview-key execution-key context)
  (let ((result-text "")
        (execution-error nil))
    (if (%stream-tool-call-cancelled-p context)
        (setf execution-error "Tool execution cancelled."
              result-text execution-error)
        (handler-case
            (let ((toolset (getf context :toolset))
                  (tool-name (getf context :tool-name))
                  (permission-mode (getf context :permission-mode))
                  (stream-state (getf context :stream-state)))
              (if (pseudopod:find-tool toolset tool-name)
                  (let ((result
                          (execute-tool
                           tool-call
                           (make-amoebum-context
                            :toolset toolset
                            :permission-mode permission-mode
                            :event-bus (%context-event-bus chat-state)
                            :permission-cancel-thunk
                            (lambda ()
                              (%stream-tool-call-cancelled-p context))))))
                    (setf result-text (sanitize-string-for-llm
                                        (if (stringp result)
                                            result
                                            (princ-to-string (or result ""))))))
                  (let ((err-msg (format nil "Unregistered tool ~A."
                                         (or tool-name "<unknown>"))))
                    (setf execution-error err-msg
                          result-text err-msg))))
          (error (condition)
            (setf execution-error (sanitize-string-for-llm (princ-to-string condition))
                  result-text execution-error))))
    (token-stream-emit-tool-call-result
     (getf context :stream-state)
     :tool-call tool-call
     :preview-key preview-key
     :execution-key execution-key
     :result result-text
     :execution-error execution-error)))

(defun %make-stream-tool-call-worker (chat-state tool-call preview-key execution-key context)
  (lambda ()
    (%execute-stream-tool-call-now
     chat-state
     tool-call
     preview-key
     execution-key
     context)))

(defun %dispatch-stream-tool-call-worker! (chat-state tool-call preview-key execution-key context)
  (let ((worker (%make-stream-tool-call-worker
                 chat-state
                 tool-call
                 preview-key
                 execution-key
                 context)))
    (if (eq (getf context :permission-mode) :full-auto)
        (funcall worker)
        (%enqueue-tool-worker worker))))

(defun %prepare-stream-tool-call-execution! (chat-state event)
  (let* ((tool-call (%stream-tool-call-from-event event))
         (preview-entry (%update-stream-tool-call-preview! chat-state event))
         (preview-key (and (listp preview-entry) (getf preview-entry :key)))
         (execution-key (%stream-tool-call-execution-key tool-call preview-key))
         (executed-table (chat-ui-state-stream-executed-tool-call-keys chat-state)))
    (cond
      ((not (and (pseudopod:tool-call-p tool-call) execution-key))
       nil)
      ((gethash execution-key executed-table)
       nil)
      ((not (%tool-call-has-id-p tool-call))
       ;; The :complete handler will ask the LLM to re-issue with proper IDs.
       (setf (gethash execution-key executed-table) t)
       (%set-stream-tool-call-execution-status!
        chat-state preview-key :malformed-p t)
       nil)
      (t
       (setf (gethash execution-key executed-table) t)
       (%set-stream-tool-call-execution-status! chat-state preview-key :executed-p t)
       (list :tool-call tool-call
             :preview-key preview-key
             :execution-key execution-key
             :context (%stream-tool-call-execution-context chat-state tool-call))))))

(defun %execute-stream-tool-call! (chat-state event)
  (let ((execution (%prepare-stream-tool-call-execution! chat-state event)))
    (unless execution
      (return-from %execute-stream-tool-call! nil))
    (%dispatch-stream-tool-call-worker!
     chat-state
     (getf execution :tool-call)
     (getf execution :preview-key)
     (getf execution :execution-key)
     (getf execution :context))
    t))

;;; ---- block-E: stream event handler table + handlers + dispatch (originally chat.lisp:1554-1707) ----
(defun %make-stream-event-handler-table ()
  (let ((table (make-hash-table :test #'eq)))
    (labels ((register (k fn)
               (setf (gethash k table) fn)))
      (register :text-delta '%handle-stream-textish-event)
      (register :chunk '%handle-stream-textish-event)
      (register :reasoning '%handle-stream-reasoning-event)
      (register :tool-call-delta '%handle-stream-tool-call-preview-event)
      (register :tool-call-started '%handle-stream-tool-call-started-event)
      (register :tool-call-argument-complete '%handle-stream-tool-call-argument-complete-event)
      (register :tool-call-result '%handle-stream-tool-call-result-event)
      (register :complete '%handle-stream-complete-event)
      (register :cancelled '%handle-stream-cancelled-event)
      (register :failed '%handle-stream-failed-event))
    table))

(defun %handle-stream-reasoning-event (chat-state event conversation)
  "Handle reasoning/thinking content from kimi k2.5."
  (declare (ignore conversation))
  (let ((chunk (getf event :text)))
    (when (and (stringp chunk) (plusp (length chunk)))
      (chat-ui-append-thinking-chunk chat-state chunk))))

(defparameter *chat-stream-event-handlers* (%make-stream-event-handler-table))

(defun %handle-stream-textish-event (chat-state event conversation)
  (declare (ignore conversation))
  (%append-streaming-assistant-chunk chat-state (getf event :text))
  (%emit-stream-chunk-token-events chat-state event)
  (%emit-stream-budget-warning-if-needed chat-state)
  (%enforce-stream-token-budget-if-needed chat-state))

(defun %handle-stream-tool-call-preview-event (chat-state event conversation)
  (declare (ignore conversation))
  (%update-stream-tool-call-preview! chat-state event))

(defun %stream-tool-call-event-metadata (chat-state event)
  (let* ((tool-call (%stream-tool-call-from-event event))
         (index (getf event :index))
         (tool-name (or (and (pseudopod:tool-call-p tool-call)
                             (pseudopod:tool-call-name tool-call))
                        (getf event :tool-name)))
         (tool-call-id (or (and (pseudopod:tool-call-p tool-call)
                                (pseudopod:tool-call-id tool-call))
                           (getf event :tool-call-id)))
         (arguments (or (and (pseudopod:tool-call-p tool-call)
                             (pseudopod:tool-call-arguments tool-call))
                        (getf event :arguments)))
         (key (%stream-tool-call-preview-key index tool-call-id tool-name arguments))
         (prior-entry (%find-stream-tool-call-preview chat-state key index))
         (prior-started-p (and (listp prior-entry)
                               (not (null (getf prior-entry :started-p)))))
         (prior-arguments-complete-p
           (and (listp prior-entry)
                (not (null (getf prior-entry :arguments-complete-p)))))
         (entry (%update-stream-tool-call-preview! chat-state event)))
    (list :prior-started-p prior-started-p
          :prior-arguments-complete-p prior-arguments-complete-p
          :entry entry
          :tool-name (or (getf event :tool-name)
                         (and (listp entry) (getf entry :tool-name)))
          :tool-call-id (or (getf event :tool-call-id)
                            (and (listp entry) (getf entry :tool-call-id)))
          :arguments (or (getf event :arguments)
                         (and (listp entry) (getf entry :arguments)))
          :index (or (getf event :index)
                     (and (listp entry) (getf entry :index))))))

(defun %handle-stream-tool-call-started-event (chat-state event conversation)
  (declare (ignore conversation))
  (let ((metadata (%stream-tool-call-event-metadata chat-state event)))
    (unless (getf metadata :prior-started-p)
      (publish (%context-event-bus chat-state)
               (make-tool-call-started-event
                :tool-name (getf metadata :tool-name)
                :tool-call-id (getf metadata :tool-call-id)
                :arguments (getf metadata :arguments)
                :index (getf metadata :index))))))

(defun %handle-stream-tool-call-argument-complete-event (chat-state event conversation)
  (declare (ignore conversation))
  (let ((metadata (%stream-tool-call-event-metadata chat-state event)))
    (unless (getf metadata :prior-arguments-complete-p)
      (publish (%context-event-bus chat-state)
               (make-tool-call-argument-complete-event
                :tool-name (getf metadata :tool-name)
                :tool-call-id (getf metadata :tool-call-id)
                :arguments (getf metadata :arguments)
                :index (getf metadata :index)))
      (%execute-stream-tool-call! chat-state event))))

(defun %handle-stream-tool-call-result-event (chat-state event conversation)
  (declare (ignore conversation))
  (%set-tool-call-result! chat-state event)
  (%maybe-finalize-streaming-completion-pending-state chat-state))

(defun %handle-stream-complete-event (chat-state event conversation)
  (declare (ignore event conversation))
  (setf (chat-ui-state-stream-completion-pending-p chat-state) t))

(defun %prefer-chat-input-focus! (chat-state)
  (let ((runtime (and chat-state
                      (chat-ui-state-runtime chat-state))))
    (when runtime
      ;; Preserve the operator's ability to resume typing after overlays or
      ;; stream interruptions unwind on the next render pass.
      (setf (ptui.ui.runtime:runtime-focus-id runtime) :chat-input)))
  chat-state)

(defun %handle-stream-cancelled-event (chat-state event conversation)
  (declare (ignore event))
  (%finalize-streaming-terminal-cancellation! chat-state conversation))

(defun %handle-stream-failed-event (chat-state event conversation)
  (declare (ignore event))
  (%finalize-streaming-terminal-failure! chat-state conversation))

(defun %finalize-streaming-terminal! (chat-state conversation
                                      &key partialp next-state system-message)
  (%finalize-streaming-assistant-message chat-state :partialp partialp)
  (when (and (stringp system-message)
             (plusp (length system-message)))
    (chat-ui-add-message chat-state "system" system-message))
  (%clear-stream-tool-tracking! chat-state)
  (%prefer-chat-input-focus! chat-state)
  (conversation-transition! conversation next-state))

(defun %finalize-streaming-terminal-cancellation! (chat-state conversation)
  (%finalize-streaming-terminal! chat-state conversation
                                 :partialp t
                                 :next-state :idle))

(defun %finalize-streaming-terminal-failure! (chat-state conversation)
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (summary (token-stream-progress-summary stream-state))
         (error-message (getf summary :error-message)))
    (%finalize-streaming-terminal!
     chat-state
     conversation
     :partialp t
     :next-state :error-recovery
     :system-message
     (and (stringp error-message)
          (plusp (length (%trim-chat-error-text error-message)))
          (%format-stream-failure-message error-message)))))

(defun %record-chat-stream-event! (chat-state event)
  (let ((journal (and chat-state
                      (chat-ui-state-stream-event-journal chat-state)))
        (snapshot (and chat-state
                       (chat-ui-state-stream-turn-snapshot chat-state))))
    (when (stream-event-journal-p journal)
      (stream-event-journal-append! journal event))
    (when (typep snapshot 'pseudopod:stream-turn-snapshot)
      (pseudopod:stream-turn-apply-event! snapshot event)))
  chat-state)

(defun %classify-streamed-turn-events (events)
  (pseudopod:stream-turn-snapshot-terminal-outcome
   (%stream-turn-snapshot-from-events events)))

(defun %dispatch-stream-event (chat-state event conversation)
  (%record-chat-stream-event! chat-state event)
  (let* ((handler-name (gethash (or (getf event :type) (getf event :kind))
                                *chat-stream-event-handlers*))
         (handler (and handler-name (symbol-function handler-name))))
    (when handler
      (funcall handler chat-state event conversation))))

;;; ---- block-F: stream status summary helpers (originally chat.lisp:1709-1754) ----
(defun %stream-status-summary (chat-state)
  (token-stream-progress-summary (chat-ui-state-stream-state chat-state)))

(defun %stream-summary-publish-key (summary)
  (let ((status (or (getf summary :status) :idle))
        (tokens (or (getf summary :tokens) 0))
        (chunks (or (getf summary :chunks) 0))
        (budget-warning-emitted-p (not (null (getf summary :budget-warning-emitted-p))))
        (cancel-requested-p (not (null (getf summary :cancel-requested-p))))
        (tokens-per-second (or (getf summary :tokens-per-second) 0.0d0))
        (elapsed-ms (or (getf summary :elapsed-ms) 0)))
    (list status
          tokens
          chunks
          budget-warning-emitted-p
          cancel-requested-p
          (if (eq status :running)
              (truncate (* 10 tokens-per-second))
              elapsed-ms))))

(defun %publish-status-bar-stream-summary-if-needed (chat-state)
  (let* ((summary (%stream-status-summary chat-state))
         (publish-key (%stream-summary-publish-key summary)))
    (unless (equal publish-key (chat-ui-state-stream-status-publish-key chat-state))
      (publish-status-bar-stream-summary
       summary
       :event-bus (status-bar-state-event-bus
                   (chat-ui-state-status-bar-state chat-state)))
      (setf (chat-ui-state-stream-status-publish-key chat-state) publish-key))
    summary))

(defun %stream-tree-key (chat-state)
  (let* ((summary (%stream-status-summary chat-state))
         (status (getf summary :status))
         (elapsed-ms (or (getf summary :elapsed-ms) 0)))
    (list (status-bar-render-key (chat-ui-state-status-bar-state chat-state))
          status
          (getf summary :tokens)
          (getf summary :chunks)
          (getf summary :budget-warning-emitted-p)
          (if (eq status :running)
              (truncate elapsed-ms 100)
              elapsed-ms)
          (getf summary :cancel-requested-p)
          (getf summary :error-message)
          (%stream-tool-call-preview-signature chat-state))))

;;; ---- block-H: post-receive hooks + budget helpers + enforce (originally chat.lisp:2031-2186) ----
(defun %emit-post-receive-hook (response)
  (when response
    (ignore-errors
      (run-hooks :post-receive response)))
  t)

(defun %emit-post-llm-receive-hook (response usage model)
  (when response
    (ignore-errors
      (run-hooks :post-llm-receive response usage model)))
  t)

(defun %resolve-pre-llm-messages (default-messages hook-results)
  (let ((resolved default-messages))
    (dolist (entry (or hook-results '()) resolved)
      (let ((value (cdr entry)))
        (when (listp value)
          (setf resolved value))))))

(defun %emit-stream-budget-warning-if-needed (chat-state)
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (used-tokens (chat-ui-state-context-used-tokens chat-state))
         (limit-tokens (chat-ui-state-context-window-limit chat-state)))
    (when (and (token-stream-active-p stream-state)
               (integerp used-tokens)
               (integerp limit-tokens)
               (> limit-tokens 0))
      (let ((warning
              (token-stream-maybe-budget-warning stream-state
                                                 used-tokens
                                                 limit-tokens)))
        (when warning
          (publish (%context-event-bus chat-state)
                   (make-stream-budget-warning-event
                    :used-tokens (getf warning :used-tokens)
                    :limit-tokens (getf warning :limit-tokens)
                    :usage-percent (getf warning :usage-percent)
                    :threshold-percent (getf warning :threshold-percent))))
        warning))))

(defun %stream-budget-abort-threshold-percent (chat-state)
  (let ((value (cfg :stream-budget-abort-threshold-percent)))
    (if (and (integerp value) (>= value 1) (<= value 100))
        value
        +stream-budget-abort-threshold-percent+)))

(defun %stream-budget-threshold-limit (limit threshold-percent)
  (truncate (* (max 0 limit)
               (/ (max 1 threshold-percent) 100.0d0))))

(defun %stream-tokenize-chunk (chunk)
  (let ((value (if (stringp chunk) chunk "")))
    (remove-if (lambda (token)
                 (or (null token)
                     (zerop (length token))))
               (cl-ppcre:split "\\s+" value))))

(defun %budget-summary-window-messages (chat-state &key (max-messages 8))
  (let* ((messages (chat-ui-state-messages chat-state))
         (safe-max (max 1 (if (and (integerp max-messages) (> max-messages 0))
                              max-messages
                              8)))
         (count (length messages))
         (start (max 0 (- count safe-max))))
    (subseq messages start count)))

(defun %budget-exhaustion-context-summary (chat-state)
  (let* ((window (%budget-summary-window-messages chat-state :max-messages 8)))
    (if (null window)
        "No conversation context available."
        (%compression-summary-text window))))

(defun %apply-stream-budget-exhaustion-resolution (chat-state stream-state resolution)
  (let ((action (getf resolution :action)))
    (case action
      (:extend-budget
       (let* ((extra (max 1 (or (getf resolution :extra-budget) 1)))
              (new-limit (+ (chat-ui-state-context-window-limit chat-state) extra))
              (status-state (chat-ui-state-status-bar-state chat-state)))
         (setf (chat-ui-state-context-window-limit chat-state) new-limit)
         (when (typep status-state 'status-bar-state)
           (setf (status-bar-state-context-max-tokens status-state) new-limit))
         nil))
      (:summarize-and-finish
       (ignore-errors
         (%compress-chat-history! chat-state :trigger :budget-exhausted))
       (let ((partial-output (or (getf resolution :partial-output)
                                 "Budget exhausted. Returning a bounded partial result.")))
         (%append-streaming-assistant-chunk
          chat-state
          (format nil "~%[budget exhausted] ~A~%" partial-output))
         (%materialize-streaming-assistant-message! chat-state :partialp t))
       (token-stream-abort stream-state :budget-exhausted)
       t)
      (:abort-task
       (let ((reason (or (getf resolution :reason)
                         "Budget exhausted; task aborted.")))
         (%append-streaming-assistant-chunk
          chat-state
          (format nil "~%[budget exhausted] ~A~%" reason))
         (%materialize-streaming-assistant-message! chat-state :partialp t))
       (token-stream-abort stream-state :budget-exhausted)
       t)
      (otherwise
       (token-stream-abort stream-state :budget-exhausted)
       t))))

(defun %emit-stream-chunk-token-events (chat-state event)
  (let* ((chunk (getf event :text))
         (tokens (%stream-tokenize-chunk chunk))
         (token-count (or (getf event :token-count) 0)))
    (when (plusp (length tokens))
      (let* ((summary (token-stream-progress-summary (chat-ui-state-stream-state chat-state)))
             (total-tokens (or (getf summary :tokens) 0))
             (chunk-index (or (getf summary :chunks) 0))
             (base-total (max 0 (- total-tokens token-count))))
        (loop for token in tokens
              for token-index from 1 do
                (publish (%context-event-bus chat-state)
                         (make-llm-stream-chunk-event
                          :token token
                          :chunk-index chunk-index
                          :token-index token-index
                          :total-tokens (+ base-total token-index))))))))

(defun %enforce-stream-token-budget-if-needed (chat-state)
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (limit (chat-ui-state-context-window-limit chat-state))
         (threshold-percent (%stream-budget-abort-threshold-percent chat-state))
         (summary (token-stream-progress-summary stream-state))
         (stream-tokens (or (getf summary :tokens) 0))
         (aborted-p (not (null (getf summary :aborted-p)))))
    (when (and (token-stream-active-p stream-state)
               (integerp limit)
               (> limit 0)
               (not aborted-p))
      (let ((threshold-limit (%stream-budget-threshold-limit limit threshold-percent)))
        (when (> stream-tokens threshold-limit)
          ;; Only publish if the warning-level event wasn't already emitted
          (unless (token-stream-state-budget-warning-emitted-p stream-state)
            (publish (%context-event-bus chat-state)
                     (make-stream-budget-warning-event
                      :used-tokens stream-tokens
                      :limit-tokens limit
                      :usage-percent (truncate (/ (* stream-tokens 100.0d0)
                                                  (max 1 limit)))
                      :threshold-percent threshold-percent)))
          (%apply-stream-budget-exhaustion-resolution
           chat-state
           stream-state
           (handle-budget-exhaustion
            :kind :token
            :used stream-tokens
            :budget threshold-limit
            :context-summary (%budget-exhaustion-context-summary chat-state)
            :max-partial-output-chars 280)))))))

;;; ---- block-I: collect tool calls + append results (originally chat.lisp:2188-2264) ----
(defun %collect-stream-tool-calls (chat-state)
  "Collect pseudopod:tool-call structs from the stream preview table."
  (let ((calls '()))
    (maphash
     (lambda (key entry)
       (declare (ignore key))
       (when (and (listp entry)
                  (getf entry :executed-p)
                  (getf entry :completed-p))
         (let ((tool-name (getf entry :tool-name))
               (tool-call-id (getf entry :tool-call-id))
               (arguments (getf entry :arguments))
               (result (getf entry :result)))
           (push (list :tool-call (pseudopod:make-tool-call
                                   :id tool-call-id
                                   :name (or tool-name "")
                                   :arguments arguments)
                       :result (or result ""))
                 calls))))
     (chat-ui-state-stream-tool-calls chat-state))
    (nreverse calls)))

(defun %collect-malformed-tool-calls (chat-state)
  "Collect tool call names from the preview table that were marked malformed
(missing tool_call_id)."
  (let ((names '()))
    (maphash
     (lambda (key entry)
       (declare (ignore key))
       (when (and (listp entry) (getf entry :malformed-p))
         (push (or (getf entry :tool-name) "<unknown>") names)))
     (chat-ui-state-stream-tool-calls chat-state))
    (nreverse names)))

(defun %malformed-tool-call-retry-message (malformed-names)
  "Build a user message asking the LLM to re-issue malformed tool calls."
  (format nil "Your tool call~P for ~{~A~^, ~} ~
               ~[~;was~:;were~] missing a tool_call_id. ~
               Each tool call must include an id field. ~
               Please re-issue ~[~;it~:;them~]."
          (length malformed-names)
          malformed-names
          (length malformed-names)
          (length malformed-names)))

(defun %append-tool-result-messages! (chat-state tool-call-entries)
  "Append tool-result messages to the conversation for each executed tool call.
Sanitizes ANSI escape codes from tool results to prevent LLM API errors."
  (dolist (entry tool-call-entries)
    (let* ((tc (getf entry :tool-call))
           (result (getf entry :result))
           (tool-call-id (and (pseudopod:tool-call-p tc)
                              (pseudopod:tool-call-id tc)))
           (tool-name (and (pseudopod:tool-call-p tc)
                           (pseudopod:tool-call-name tc)))
           ;; Sanitize ANSI escape codes to prevent 'invalid character \\x1b' errors
           (sanitized-result (sanitize-string-for-llm (or result "")))
           (message (pseudopod:make-message
                     :role "tool"
                     :content sanitized-result
                     :name tool-name
                     :tool-call-id tool-call-id)))
      (chat-ui-append-message chat-state message))))

(defun %set-assistant-message-tool-calls! (chat-state tool-call-entries)
  "Set tool-calls on the current streaming assistant message."
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (target-index (token-stream-state-target-message-index stream-state))
         (messages (chat-ui-state-messages chat-state)))
    (when (and (integerp target-index)
               (>= target-index 0)
               (< target-index (length messages)))
      (let ((message (nth target-index messages))
            (tool-calls (mapcar (lambda (entry) (getf entry :tool-call))
                                tool-call-entries)))
        (when (and (pseudopod:message-p message) tool-calls)
          (setf (pseudopod:message-tool-calls message) tool-calls))))))

;;; ---- block-J: drain stream events (originally chat.lisp:2303-2309) ----
(defun %drain-stream-events (chat-state)
  (let ((conversation (%ensure-chat-conversation-state chat-state)))
    (token-stream-drain-events
     (chat-ui-state-stream-state chat-state)
     (lambda (event)
       (%dispatch-stream-event chat-state event conversation)))
    (%maybe-finalize-streaming-completion-pending-state chat-state)))

;;; ---- block-K: stream status fragment (originally chat.lisp:2311-2341) ----
(defun %stream-status-fragment (chat-state)
  (let* ((summary (%stream-status-summary chat-state))
         (status (getf summary :status))
         (tokens (or (getf summary :tokens) 0))
         (elapsed-ms (or (getf summary :elapsed-ms) 0))
         (tps (or (getf summary :tokens-per-second) 0.0d0))
         (error-message (getf summary :error-message)))
    (case status
      (:running
       (format nil "stream ~D tok @ ~,2f tok/s ~,1fs"
               tokens
               tps
               (/ elapsed-ms 1000.0d0)))
      (:cancelled
       (if (getf summary :aborted-p)
           (format nil "stream aborted (~D tok, ~A)"
                   tokens
                   (or (getf summary :abort-reason) :unknown))
           (format nil "stream cancelled (~D tok, ~,1fs)"
                   tokens
                   (/ elapsed-ms 1000.0d0))))
      (:completed
       (format nil "stream complete (~D tok, ~,1fs)"
               tokens
               (/ elapsed-ms 1000.0d0)))
      (:failed
       (if (and (stringp error-message) (plusp (length error-message)))
           (format nil "stream failed: ~A" error-message)
           "stream failed"))
      (otherwise
       nil))))
