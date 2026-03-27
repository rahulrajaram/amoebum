(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; Approval Dialog — interactive tool-call approval for supervised mode
;;;
;;; When the pipeline encounters a :prompt permission decision, it blocks
;;; on a condition variable.  The TUI subscribes to permission-prompted
;;; events, activates this dialog, and the user presses a key to approve
;;; or deny.  The decision is written back through the pending-approval
;;; struct and the condvar is notified.
;;; ---------------------------------------------------------------------------

;;; ---- Pending approval (shared between pipeline thread and TUI thread) ----

(defstruct (pending-approval
            (:constructor %make-pending-approval
                (&key tool-name arguments path command reason decision-id)))
  (tool-name "" :type string)
  (arguments nil)
  (path nil)
  (command nil)
  (reason "" :type string)
  (decision-id "" :type string)
  (created-at-ms (monotonic-ms) :type integer)
  (resolved-at-ms nil)
  (decision-source nil)
  (decision nil)       ; nil while waiting, :allow or :deny when resolved
  (remember-p nil :type boolean))   ; t → add to permanent allow/deny list

(defvar *pending-approval-lock* (bt:make-lock "pending-approval-lock"))
(defvar *pending-approval-condvar* (bt:make-condition-variable :name "pending-approval-cv"))
(defvar *pending-approval* nil
  "Current pending-approval struct while the pipeline is waiting, or NIL.")
(defvar *approval-ui-active-p* nil
  "Set to T by the TUI when it starts.  When NIL, wait-for-pending-approval
falls through immediately with :deny (headless / test mode).")

(defparameter +approval-wait-timeout-seconds+ 300)
(defparameter +approval-wait-poll-seconds+ 1.0)

(defun %approval-wait-timeout-seconds ()
  (let ((text (%runtime-log-trimmed-env "AMOEBUM_APPROVAL_WAIT_TIMEOUT_SECONDS")))
    (handler-case
        (let ((value (and text
                          (parse-integer text :junk-allowed nil))))
          (if (and value (>= value 0))
              value
              +approval-wait-timeout-seconds+))
      (error ()
        +approval-wait-timeout-seconds+))))

(defun %approval-wait-poll-seconds ()
  (let ((text (%runtime-log-trimmed-env "AMOEBUM_APPROVAL_WAIT_POLL_SECONDS")))
    (handler-case
        (let* ((*read-eval* nil)
               (value (and text (read-from-string text))))
          (if (and (realp value)
                   (> value 0))
              (coerce value 'double-float)
              +approval-wait-poll-seconds+))
      (error ()
        +approval-wait-poll-seconds+))))

(defun %approval-log-details (pending-approval &key extra)
  (append
   (when pending-approval
     (list :tool-name (pending-approval-tool-name pending-approval)
           :decision-id (pending-approval-decision-id pending-approval)
           :path (pending-approval-path pending-approval)
           :command (pending-approval-command pending-approval)
           :reason (pending-approval-reason pending-approval)
           :created-at-ms (pending-approval-created-at-ms pending-approval)
           :resolved-at-ms (pending-approval-resolved-at-ms pending-approval)
           :decision-source (pending-approval-decision-source pending-approval)))
   extra))

(defun %resolve-pending-approval! (pending-approval decision &key remember-p source)
  (setf (pending-approval-decision pending-approval) decision
        (pending-approval-remember-p pending-approval) remember-p
        (pending-approval-decision-source pending-approval) source
        (pending-approval-resolved-at-ms pending-approval) (monotonic-ms))
  pending-approval)

(defun %approval-ui-error-details (&key operation dialog-state condition)
  (list :operation operation
        :tool-name (and dialog-state
                        (approval-dialog-state-tool-name dialog-state))
        :decision-id (and dialog-state
                          (approval-dialog-state-decision-id dialog-state))
        :condition (and condition
                        (princ-to-string condition))))

(defun submit-pending-approval (decision &key remember-p (source :user))
  "Called from the TUI thread to resolve the current pending approval.
DECISION is :allow or :deny.  REMEMBER-P adds the rule permanently."
  (bt:with-lock-held (*pending-approval-lock*)
    (when *pending-approval*
      (%resolve-pending-approval! *pending-approval*
                                  decision
                                  :remember-p remember-p
                                  :source source)
      (log-runtime-event :level :info
                         :kind "approval-resolved"
                         :source :approval-dialog
                         :message "Interactive tool approval resolved."
                         :details (%approval-log-details
                                   *pending-approval*
                                   :extra (list :decision decision
                                                :source source
                                                :remember-p remember-p)))
      (bt:condition-notify *pending-approval-condvar*))))

(defun wait-for-pending-approval (tool-name arguments
                                  &key path command reason decision-id
                                       cancel-thunk)
  "Called from the pipeline thread.  Blocks until the TUI resolves the
pending approval.  Returns the pending-approval struct with decision set.
  When no TUI is active (*approval-ui-active-p* is NIL), immediately returns
with :deny — this prevents blocking in headless/test mode."
  (let* ((timeout-seconds (%approval-wait-timeout-seconds))
         (poll-seconds (%approval-wait-poll-seconds))
         (pa (%make-pending-approval
              :tool-name (or tool-name "unknown")
              :arguments arguments
              :path path
              :command command
              :reason (or reason "approval required")
              :decision-id (or decision-id (%next-permission-decision-id)))))
    (unless *approval-ui-active-p*
      ;; No TUI running — deny immediately (headless/test mode)
      (log-runtime-event :level :warn
                         :kind "approval-headless-deny"
                         :source :approval-dialog
                         :message "Approval request denied because no approval UI was active."
                         :details (%approval-log-details
                                   pa
                                   :extra (list :timeout-seconds timeout-seconds
                                                :poll-seconds poll-seconds)))
      (%resolve-pending-approval! pa :deny :source :noninteractive)
      (return-from wait-for-pending-approval pa))
    (handler-case
        (unwind-protect
             (bt:with-lock-held (*pending-approval-lock*)
               (log-runtime-event :level :info
                                  :kind "approval-requested"
                                  :source :approval-dialog
                                  :message "Tool approval requested."
                                  :details (%approval-log-details
                                            pa
                                            :extra (list :timeout-seconds timeout-seconds
                                                         :poll-seconds poll-seconds)))
               (setf *pending-approval* pa)
               (let ((deadline (+ (get-internal-real-time)
                                  (* timeout-seconds
                                     internal-time-units-per-second))))
                 (loop until (pending-approval-decision pa)
                       do (bt:condition-wait *pending-approval-condvar*
                                             *pending-approval-lock*
                                             :timeout poll-seconds)
                          (when (> (get-internal-real-time) deadline)
                            (log-runtime-event :level :warn
                                               :kind "approval-timeout-deny"
                                               :source :approval-dialog
                                               :message "Tool approval timed out; denying by default."
                                               :details (%approval-log-details
                                                         pa
                                                         :extra (list :timeout-seconds timeout-seconds
                                                                      :poll-seconds poll-seconds)))
                            (%resolve-pending-approval! pa :deny :source :timeout)
                            (return))
                          (when (and (functionp cancel-thunk)
                                     (funcall cancel-thunk))
                            (log-runtime-event :level :warn
                                               :kind "approval-cancel-deny"
                                               :source :approval-dialog
                                               :message "Tool approval cancelled; denying by default."
                                               :details (%approval-log-details
                                                         pa
                                                         :extra (list :timeout-seconds timeout-seconds
                                                                      :poll-seconds poll-seconds)))
                            (%resolve-pending-approval! pa :deny :source :cancelled)
                            (return)))))
          (bt:with-lock-held (*pending-approval-lock*)
            (when (eq *pending-approval* pa)
              (setf *pending-approval* nil))))
      (error (condition)
        (%resolve-pending-approval! pa :deny :source :ui-error)
        (log-runtime-condition condition
                               :kind "approval-wait-failed"
                               :source :approval-dialog
                               :message "Approval wait failed; denying by default."
                               :details (%approval-log-details pa)
                               :path (crash-log-path)
                               :include-backtrace-p t)))
    pa))

;;; ---- TUI dialog state (lives on the chat-ui-state) ----

(defstruct (approval-dialog-state
            (:constructor make-approval-dialog-state
                (&key (active-p nil)
                      (selected-option :approve))))
  (active-p nil :type boolean)
  (selected-option :approve :type keyword)  ; :approve :deny :remember-allow :remember-deny
  (tool-name "" :type string)
  (command nil)
  (path nil)
  (reason "" :type string)
  (decision-id "" :type string))

(defparameter +approval-dialog-options+
  '((:approve       . "[y] Approve")
    (:deny          . "[n] Deny")
    (:remember-allow . "[a] Always allow this tool")
    (:remember-deny  . "[d] Always deny this tool")))

(defun %approval-option-index (option)
  (position option +approval-dialog-options+ :key #'car :test #'eq))

(defun %approval-option-at (index)
  (car (nth (mod index (length +approval-dialog-options+))
            +approval-dialog-options+)))

(defun approval-dialog-activate! (dialog-state tool-name
                                  &key command path reason decision-id)
  "Populate and activate the approval dialog."
  (setf (approval-dialog-state-active-p dialog-state) t
        (approval-dialog-state-selected-option dialog-state) :approve
        (approval-dialog-state-tool-name dialog-state) (or tool-name "unknown")
        (approval-dialog-state-command dialog-state) command
        (approval-dialog-state-path dialog-state) path
        (approval-dialog-state-reason dialog-state) (or reason "")
        (approval-dialog-state-decision-id dialog-state) (or decision-id "")))

(defun approval-dialog-deactivate! (dialog-state)
  (setf (approval-dialog-state-active-p dialog-state) nil))

(defun approval-dialog-move-selection! (dialog-state delta)
  (let* ((current (%approval-option-index
                   (approval-dialog-state-selected-option dialog-state)))
         (new-index (mod (+ (or current 0) delta)
                         (length +approval-dialog-options+))))
    (setf (approval-dialog-state-selected-option dialog-state)
          (%approval-option-at new-index))))

(defun approval-dialog-confirm! (dialog-state)
  "User confirmed their selection.  Submit the decision and deactivate."
  (let ((option (approval-dialog-state-selected-option dialog-state)))
    (case option
      (:approve
       (submit-pending-approval :allow))
      (:deny
       (submit-pending-approval :deny))
      (:remember-allow
       (submit-pending-approval :allow :remember-p t))
      (:remember-deny
       (submit-pending-approval :deny :remember-p t)))
    (approval-dialog-deactivate! dialog-state)))

;;; ---- Key handler (called from chat.lisp event routing) ----

(defparameter +approval-dialog-key-handlers+
  (list (cons :up (lambda (dialog-state)
                    (approval-dialog-move-selection! dialog-state -1)
                    t))
        (cons :left (lambda (dialog-state)
                      (approval-dialog-move-selection! dialog-state -1)
                      t))
        (cons :down (lambda (dialog-state)
                      (approval-dialog-move-selection! dialog-state 1)
                      t))
        (cons :right (lambda (dialog-state)
                       (approval-dialog-move-selection! dialog-state 1)
                       t))
        (cons :enter (lambda (dialog-state)
                       (approval-dialog-confirm! dialog-state)
                       t))
        (cons :return (lambda (dialog-state)
                        (approval-dialog-confirm! dialog-state)
                        t))
        (cons :escape (lambda (dialog-state)
                        (submit-pending-approval :deny)
                        (approval-dialog-deactivate! dialog-state)
                        t))
        (cons :text (lambda (_dialog-state)
                      (declare (ignore _dialog-state))
                      nil)))
  "Key dispatch table for approval-dialog keyboard navigation.")

(defun %approval-dialog-key-handler (key)
  (cdr (assoc key +approval-dialog-key-handlers+ :test #'eq)))

(defun approval-dialog-handle-key! (dialog-state key)
  "Handle a key press when the approval dialog is active.
Returns T if the key was consumed, NIL otherwise."
  (handler-case
      (when (approval-dialog-state-active-p dialog-state)
        (let ((handler (%approval-dialog-key-handler key)))
          (if handler
              (funcall handler dialog-state)
              t)))
    (error (condition)
      (log-runtime-condition condition
                             :kind "approval-key-handler-failed"
                             :source :approval-dialog
                             :message "Approval dialog key handling failed; denying by default."
                             :details (%approval-ui-error-details
                                       :operation :key
                                       :dialog-state dialog-state
                                       :condition condition)
                             :path (crash-log-path))
      (ignore-errors (submit-pending-approval :deny :source :ui-error))
      (ignore-errors (approval-dialog-deactivate! dialog-state))
      t)))

(defun approval-dialog-handle-text! (dialog-state text)
  "Handle text input shortcuts: y/n/a/d."
  (handler-case
      (when (and (approval-dialog-state-active-p dialog-state)
                 (stringp text)
                 (= (length text) 1))
        (let ((char (char-downcase (char text 0))))
          (case char
            (#\y
             (submit-pending-approval :allow)
             (approval-dialog-deactivate! dialog-state)
             t)
            (#\n
             (submit-pending-approval :deny)
             (approval-dialog-deactivate! dialog-state)
             t)
            (#\a
             (submit-pending-approval :allow :remember-p t)
             (approval-dialog-deactivate! dialog-state)
             t)
            (#\d
             (submit-pending-approval :deny :remember-p t)
             (approval-dialog-deactivate! dialog-state)
             t)
            (otherwise t))))  ; consume all text while dialog active
    (error (condition)
      (log-runtime-condition condition
                             :kind "approval-text-handler-failed"
                             :source :approval-dialog
                             :message "Approval dialog text handling failed; denying by default."
                             :details (%approval-ui-error-details
                                       :operation :text
                                       :dialog-state dialog-state
                                       :condition condition)
                             :path (crash-log-path))
      (ignore-errors (submit-pending-approval :deny :source :ui-error))
      (ignore-errors (approval-dialog-deactivate! dialog-state))
      t)))

;;; ---- Widget (rendered in chat-ui-build-tree) ----

(ptui.widgets.defwidget:defwidget make-approval-dialog-widget (state)
  (:memoize nil)
  (let* ((tool-name (getf state :tool-name "unknown"))
         (command (getf state :command))
         (path (getf state :path))
         (reason (getf state :reason ""))
         (selected (getf state :selected-option :approve)))
    (box
     (vstack
      (text (format nil "Tool approval required: ~A" tool-name)
            :id :approval-title :role :warning)
      (when-widget (and (stringp command) (plusp (length command)))
        (text ""
              :id :approval-command-detail
              :role :meta
              :styled-segments (list (list :text " " :role :meta)
                                     (list :text "command: " :role :approval-label)
                                     (list :text command :role :approval-command))))
      (when-widget (and (stringp path) (plusp (length path)))
        (text ""
              :id :approval-path-detail
              :role :meta
              :styled-segments (list (list :text " " :role :meta)
                                     (list :text "path: " :role :approval-label)
                                     (list :text path :role :approval-path))))
      (when-widget (plusp (length reason))
        (text reason :id :approval-reason :role :meta))
      (text "────────────" :id :approval-separator :role :meta)
      (map-widget
       (lambda (entry)
         (let* ((option (car entry))
                (label (cdr entry))
                (selected-p (eq option selected)))
           (text (if selected-p
                     (format nil "  > ~A" label)
                     (format nil "    ~A" label))
                 :id (intern (format nil "APPROVAL-OPT-~A" option) :keyword)
                 :role (if selected-p :assistant-label :meta))))
       +approval-dialog-options+))
      :id :approval-dialog
      :border t)))
