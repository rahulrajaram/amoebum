(in-package :amoebum)

;;; NXT-428: stream event ingestion extracted from ui/chat-stream.lisp.
;;; This module owns the journal/snapshot append path plus the event-dispatch
;;; table that coordinates text, tool, and terminal events.

(defun %handle-stream-reasoning-event (chat-state event conversation)
  "Handle reasoning/thinking content from kimi k2.5."
  (declare (ignore conversation))
  (let ((chunk (getf event :text)))
    (when (and (stringp chunk) (plusp (length chunk)))
      (chat-ui-append-thinking-chunk chat-state chunk))))

(defun %handle-stream-textish-event (chat-state event conversation)
  (declare (ignore conversation))
  (%append-streaming-assistant-chunk chat-state (getf event :text))
  (%emit-stream-chunk-token-events chat-state event)
  (%emit-stream-budget-warning-if-needed chat-state)
  (%enforce-stream-token-budget-if-needed chat-state))

(defun %handle-stream-tool-call-preview-event (chat-state event conversation)
  (declare (ignore conversation))
  (%update-stream-tool-call-preview! chat-state event))

(defun %handle-stream-cancelled-event (chat-state event conversation)
  (declare (ignore event))
  (%finalize-streaming-terminal-cancellation! chat-state conversation))

(defun %handle-stream-failed-event (chat-state event conversation)
  (declare (ignore event))
  (%finalize-streaming-terminal-failure! chat-state conversation))

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

(defparameter *chat-stream-event-handlers* (%make-stream-event-handler-table))

(defun %dispatch-stream-event (chat-state event conversation)
  (%record-chat-stream-event! chat-state event)
  (let* ((handler-name (gethash (or (getf event :type) (getf event :kind))
                                *chat-stream-event-handlers*))
         (handler (and handler-name (symbol-function handler-name))))
    (when handler
      (funcall handler chat-state event conversation))))

(defun %drain-stream-events (chat-state)
  (let ((conversation (%ensure-chat-conversation-state chat-state)))
    (token-stream-drain-events
     (chat-ui-state-stream-state chat-state)
     (lambda (event)
       (%dispatch-stream-event chat-state event conversation)))
    (%maybe-finalize-streaming-completion-pending-state chat-state)))
