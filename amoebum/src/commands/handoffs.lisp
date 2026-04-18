(in-package :amoebum)

;;; ---- NXT-004: /handoffs ----

(defun %format-handoff-entry (handoff)
  (let ((handoff-id (or (getf handoff :handoff-id)
                        (getf handoff :request-id) "?"))
        (status (or (getf handoff :status) :pending))
        (from (or (getf handoff :from-session-id)
                  (getf handoff :from-agent) "?"))
        (to (or (getf handoff :to-session-id)
                (getf handoff :to-agent) "?"))
        (reason (or (getf handoff :reason) ""))
        (summary (or (getf handoff :summary)
                     (getf (getf handoff :conversation-merge) :summary)
                     ""))
        (diagnostics (getf handoff :diagnostics)))
    (format nil "  ~A | ~A | from: ~A -> to: ~A~@[ | ~A~]~@[ | summary: ~A~]~@[ | diagnostics=~D~]"
            handoff-id status from to
            (when (plusp (length reason)) reason)
            (when (plusp (length summary)) summary)
            (and (listp diagnostics) (length diagnostics)))))

(defun %handoffs-handler (_invocation _arguments context)
  (declare (ignore _invocation _arguments))
  (let* ((chat-state (slash-command-context-chat-state context))
         (conversation (and (typep chat-state 'chat-ui-state)
                            (chat-ui-state-conversation chat-state)))
         (session-id (and (typep conversation 'conversation-state)
                          (conversation-state-session-id conversation))))
    (unless session-id
      (return-from %handoffs-handler
        (make-slash-command-result
         :echo-input-p t
         :output "No active session - cannot query handoffs.")))
    (handler-case
        (let ((pending (get-user-pending-handoffs session-id)))
          (make-slash-command-result
           :echo-input-p t
           :output (if (null pending)
                       "No pending handoffs for this session."
                       (with-output-to-string (out)
                         (format out "Pending handoffs (~D):~%" (length pending))
                         (dolist (entry pending)
                           (format out "~A~%" (%format-handoff-entry entry)))))))
      (error (condition)
        (make-slash-command-result
         :echo-input-p t
         :output (format nil "Failed to query handoffs: ~A" condition))))))

;;; ---- NXT-005: /handoff-accept, /handoff-reject, /handoff-complete ----

(defun %handoff-accept-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let ((handoff-id (gethash :ID arguments)))
    (when (%slash-blank-p handoff-id)
      (return-from %handoff-accept-handler
        (make-slash-command-result
         :echo-input-p t
         :output "Usage: /handoff-accept <handoff-id>")))
    (handler-case
        (let ((result (accept-user-handoff handoff-id)))
          (make-slash-command-result
           :echo-input-p t
           :output (format nil "Accepted handoff ~A. Status: ~A"
                           handoff-id
                           (or (getf result :status) :accepted))))
      (error (condition)
        (make-slash-command-result
         :echo-input-p t
         :output (format nil "Failed to accept handoff ~A: ~A"
                         handoff-id
                         condition))))))

(defun %handoff-reject-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let ((args (or (gethash :ARGS arguments) "")))
    (let* ((tokens (%tokenize-command-arguments args))
           (handoff-id (first tokens))
           (reason (or (format nil "~{~A~^ ~}" (rest tokens)) "rejected")))
      (when (%slash-blank-p handoff-id)
        (return-from %handoff-reject-handler
          (make-slash-command-result
           :echo-input-p t
           :output "Usage: /handoff-reject <handoff-id> [reason...]")))
      (handler-case
          (let ((result (reject-user-handoff handoff-id reason)))
            (make-slash-command-result
             :echo-input-p t
             :output (format nil "Rejected handoff ~A. Status: ~A"
                             handoff-id
                             (or (getf result :status) :rejected))))
        (error (condition)
          (make-slash-command-result
           :echo-input-p t
           :output (format nil "Failed to reject handoff ~A: ~A"
                           handoff-id
                           condition)))))))

(defun %handoff-complete-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let ((handoff-id (gethash :ID arguments)))
    (when (%slash-blank-p handoff-id)
      (return-from %handoff-complete-handler
        (make-slash-command-result
         :echo-input-p t
         :output "Usage: /handoff-complete <handoff-id>")))
    (handler-case
        (let ((result (complete-user-handoff handoff-id)))
          (make-slash-command-result
           :echo-input-p t
           :output (format nil "Completed handoff ~A. Status: ~A~@[ | summary: ~A~]~@[ | diagnostics=~D~]"
                           handoff-id
                           (or (getf result :status) :completed)
                           (let ((summary (or (getf result :summary)
                                              (getf (getf result :conversation-merge) :summary))))
                             (and (stringp summary)
                                  (plusp (length summary))
                                  summary))
                           (let ((diagnostics (getf result :diagnostics)))
                             (and (listp diagnostics)
                                  (length diagnostics))))))
      (error (condition)
        (make-slash-command-result
         :echo-input-p t
         :output (format nil "Failed to complete handoff ~A: ~A"
                         handoff-id
                         condition))))))

(defun register-agent-handoff-slash-commands ()
  (register-slash-command
   (make-slash-command
    :name "handoffs"
    :description "List pending handoffs for the current session."
    :usage "/handoffs"
    :handler #'%handoffs-handler))
  (register-slash-command
   (make-slash-command
    :name "handoff-accept"
    :description "Accept a pending handoff request."
    :usage "/handoff-accept <handoff-id>"
    :parameters
    (list (make-slash-command-parameter
           :name "id"
           :type :string
           :required-p t
           :description "Handoff identifier."))
    :handler #'%handoff-accept-handler))
  (register-slash-command
   (make-slash-command
    :name "handoff-reject"
    :description "Reject a pending handoff request."
    :usage "/handoff-reject <handoff-id> [reason...]"
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p t
           :greedy-p t
           :description "Handoff ID and optional rejection reason."))
    :handler #'%handoff-reject-handler))
  (register-slash-command
   (make-slash-command
    :name "handoff-complete"
    :description "Mark a handoff as complete."
    :usage "/handoff-complete <handoff-id>"
    :parameters
    (list (make-slash-command-parameter
           :name "id"
           :type :string
           :required-p t
           :description "Handoff identifier."))
    :handler #'%handoff-complete-handler))
  t)
