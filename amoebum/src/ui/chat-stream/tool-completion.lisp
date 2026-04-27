(in-package :amoebum)

;;; NXT-428: tool-preview, execution-ordering, and completion finalization
;;; extracted from ui/chat-stream.lisp so the residual coordinator can stay
;;; focused on turn setup and step-loop orchestration.

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
    (amoebum.fp:match (%stream-terminal-outcome-kind context)
      (:retry
       (%start-tool-retry! chat-state tool-call-entries malformed-names))
      (:tool-continuation
       (%start-tool-continuation! chat-state tool-call-entries))
      (:max-iterations
       (%finish-stream-turn-with-max-iterations! chat-state conversation tool-call-entries))
      (:answer
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
                              (princ-to-string (or arguments ""))))))
      preview-key))

(defun %tool-call-has-id-p (tool-call)
  "Return T if TOOL-CALL has a non-empty tool-call-id."
  (and (pseudopod:tool-call-p tool-call)
       (stringp (pseudopod:tool-call-id tool-call))
       (plusp (length (pseudopod:tool-call-id tool-call)))))

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
      (setf (ptui.ui.runtime:runtime-focus-id runtime) :chat-input)))
  chat-state)

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
