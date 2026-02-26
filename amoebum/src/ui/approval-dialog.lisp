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
  (decision nil)       ; nil while waiting, :allow or :deny when resolved
  (remember-p nil :type boolean))   ; t → add to permanent allow/deny list

(defvar *pending-approval-lock* (bt:make-lock "pending-approval-lock"))
(defvar *pending-approval-condvar* (bt:make-condition-variable :name "pending-approval-cv"))
(defvar *pending-approval* nil
  "Current pending-approval struct while the pipeline is waiting, or NIL.")
(defvar *approval-ui-active-p* nil
  "Set to T by the TUI when it starts.  When NIL, wait-for-pending-approval
falls through immediately with :deny (headless / test mode).")

(defun submit-pending-approval (decision &key remember-p)
  "Called from the TUI thread to resolve the current pending approval.
DECISION is :allow or :deny.  REMEMBER-P adds the rule permanently."
  (bt:with-lock-held (*pending-approval-lock*)
    (when *pending-approval*
      (setf (pending-approval-decision *pending-approval*) decision
            (pending-approval-remember-p *pending-approval*) remember-p)
      (bt:condition-notify *pending-approval-condvar*))))

(defun wait-for-pending-approval (tool-name arguments
                                  &key path command reason decision-id)
  "Called from the pipeline thread.  Blocks until the TUI resolves the
pending approval.  Returns the pending-approval struct with decision set.
When no TUI is active (*approval-ui-active-p* is NIL), immediately returns
with :deny — this prevents blocking in headless/test mode."
  (let ((pa (%make-pending-approval
             :tool-name (or tool-name "unknown")
             :arguments arguments
             :path path
             :command command
             :reason (or reason "approval required")
             :decision-id (or decision-id (%next-permission-decision-id)))))
    (unless *approval-ui-active-p*
      ;; No TUI running — deny immediately (headless/test mode)
      (setf (pending-approval-decision pa) :deny)
      (return-from wait-for-pending-approval pa))
    (bt:with-lock-held (*pending-approval-lock*)
      (setf *pending-approval* pa)
      (let ((deadline (+ (get-internal-real-time)
                         (* 300 internal-time-units-per-second))))  ; 5 minute timeout
        (loop until (pending-approval-decision pa)
              do (bt:condition-wait *pending-approval-condvar*
                                    *pending-approval-lock*
                                    :timeout 1)  ; wake every 1s to check deadline
                 (when (> (get-internal-real-time) deadline)
                   (setf (pending-approval-decision pa) :deny)
                   (return))))
      (setf *pending-approval* nil))
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

(defun approval-dialog-handle-key! (dialog-state key)
  "Handle a key press when the approval dialog is active.
Returns T if the key was consumed, NIL otherwise."
  (when (approval-dialog-state-active-p dialog-state)
    (case key
      ((:up)
       (approval-dialog-move-selection! dialog-state -1)
       t)
      ((:down)
       (approval-dialog-move-selection! dialog-state 1)
       t)
      ((:enter :return)
       (approval-dialog-confirm! dialog-state)
       t)
      ((:escape)
       ;; Escape = deny (safe default)
       (submit-pending-approval :deny)
       (approval-dialog-deactivate! dialog-state)
       t)
      ;; Single-key shortcuts
      ((:text)
       nil)  ; let the text handler below catch it
      (otherwise
       t))))  ; consume unknown keys while dialog is active

(defun approval-dialog-handle-text! (dialog-state text)
  "Handle text input shortcuts: y/n/a/d."
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
        (otherwise t)))))  ; consume all text while dialog active

;;; ---- Widget (rendered in chat-ui-build-tree) ----

(ptui.widgets.defwidget:defwidget make-approval-dialog-widget (state)
  (:memoize nil)
  (let* ((tool-name (getf state :tool-name "unknown"))
         (command (getf state :command))
         (path (getf state :path))
         (reason (getf state :reason ""))
         (selected (getf state :selected-option :approve))
         (detail (cond
                   (command (format nil "command: ~A" command))
                   (path    (format nil "path: ~A" path))
                   (t       ""))))
    (box
     (vstack
      (text (format nil "Tool approval required: ~A" tool-name)
            :id :approval-title :role :warning)
      (when-widget (plusp (length detail))
        (text detail :id :approval-detail :role :meta))
      (when-widget (plusp (length reason))
        (text reason :id :approval-reason :role :meta))
      (text "" :id :approval-spacer :role :meta)
      (map-widget
       (lambda (entry)
         (let* ((option (car entry))
                (label (cdr entry))
                (selected-p (eq option selected)))
           (text (if selected-p
                     (format nil "  > ~A" label)
                     (format nil "    ~A" label))
                 :id (intern (format nil "APPROVAL-OPT-~A" option) :keyword)
                 :role (if selected-p :user :meta))))
       +approval-dialog-options+))
     :id :approval-dialog
     :border t)))
