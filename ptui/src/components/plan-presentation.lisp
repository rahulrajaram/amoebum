(defpackage :ptui.components.plan-presentation
  (:use :cl)
  (:export
   #:plan-presentation-step
   #:plan-presentation-step-p
   #:make-plan-presentation-step
   #:plan-presentation-step-index
   #:plan-presentation-step-description
   #:plan-presentation-step-approved-p
   #:plan-presentation-step-risk
   #:plan-presentation-step-status
   #:make-plan-mode-presentation-widget))

(in-package :ptui.components.plan-presentation)

(defstruct (plan-presentation-step
            (:constructor make-plan-presentation-step
                (&key index
                      description
                      (approved-p nil)
                      (risk :medium)
                      (status :pending))))
  index
  description
  (approved-p nil :type boolean)
  (risk :medium)
  (status :pending))

(defun %safe-string (value &optional (fallback ""))
  (cond
    ((and (stringp value)
          (plusp (length value)))
     value)
    ((null value)
     fallback)
    (t
     (princ-to-string value))))

(defun %normalize-risk (risk)
  (let ((value (intern (string-upcase (%safe-string risk "medium")) :keyword)))
    (if (member value '(:low :medium :high) :test #'eq)
        value
        :medium)))

(defun %normalize-status (status)
  (let ((value (intern (string-upcase (%safe-string status "pending")) :keyword)))
    (if (member value '(:pending :approved :running :blocked :done) :test #'eq)
        value
        :pending)))

(defun %normalize-steps (steps)
  (loop for step in (or steps '())
        for raw-index = (if (typep step 'plan-presentation-step)
                            (plan-presentation-step-index step)
                            (getf step :index))
        for numeric-index = (if (integerp raw-index) raw-index 0)
        collect (if (typep step 'plan-presentation-step)
                    (make-plan-presentation-step
                     :index numeric-index
                     :description (%safe-string (plan-presentation-step-description step)
                                                "Describe this step.")
                     :approved-p (not (null (plan-presentation-step-approved-p step)))
                     :risk (%normalize-risk (plan-presentation-step-risk step))
                     :status (%normalize-status (plan-presentation-step-status step)))
                    (make-plan-presentation-step
                     :index numeric-index
                     :description (%safe-string (getf step :description) "Describe this step.")
                     :approved-p (not (null (getf step :approved-p)))
                     :risk (%normalize-risk (getf step :risk :medium))
                     :status (%normalize-status (getf step :status :pending))))))

(defun %text-widget (text &key id (role :meta))
  (ptui.widgets.core:make-text-widget (%safe-string text "")
                                      :id id
                                      :role role))

(defun %make-section-widget (title lines &key id (empty-message "[empty]"))
  (let* ((resolved-lines
           (if lines
               lines
               (list (cons empty-message :meta))))
         (rows
           (cons (%text-widget title :role :system)
                 (loop for entry in resolved-lines
                       for line = (if (consp entry) (car entry) entry)
                       for role = (if (consp entry) (cdr entry) :meta)
                       collect (%text-widget line :role role))))
         (content (ptui.widgets.core:make-stack-widget
                   rows
                   :id (and id (list id :content))
                   :direction :column
                   :gap 0)))
    (ptui.widgets.core:make-box-widget content
                                       :id id
                                       :borderp t
                                       :padding 0)))

(defun %step-line (step)
  (check-type step plan-presentation-step)
  (let ((approval (if (plan-presentation-step-approved-p step) "[x]" "[ ]"))
        (risk (string-downcase (symbol-name (%normalize-risk (plan-presentation-step-risk step)))))
        (status (string-downcase (symbol-name (%normalize-status
                                               (plan-presentation-step-status step))))))
    (format nil "~A ~D. ~A (risk: ~A, status: ~A)"
            approval
            (or (plan-presentation-step-index step) 0)
            (%safe-string (plan-presentation-step-description step) "Describe this step.")
            risk
            status)))

(defun %steps-panel (steps)
  (%make-section-widget
   "Plan Steps"
   (loop for step in (%normalize-steps steps)
         collect (cons (%step-line step)
                       (if (plan-presentation-step-approved-p step)
                           :assistant
                           :meta)))
   :id :plan-presentation-steps
   :empty-message "No plan steps captured yet."))

(defun %terminal-panel (output-lines output-empty-message output-viewport-height)
  (let* ((state (ptui.components.terminal-pane:make-terminal-pane-state
                 :title "Plan Output"
                 :lines (loop for line in (or output-lines '())
                              collect (%safe-string line ""))
                 :empty-message output-empty-message
                 :stdin-capture-policy :disabled)))
    (ptui.components.terminal-pane:make-terminal-pane-widget
     state
     :id :plan-presentation-output
     :viewport-height output-viewport-height)))

(defun %context-panel (context-lines context-empty-message)
  (%make-section-widget
   "Context Inspector"
   (loop for line in (or context-lines '())
         collect (cons (%safe-string line "") :meta))
   :id :plan-presentation-context
   :empty-message context-empty-message))

(defun make-plan-mode-presentation-widget (&key
                                             steps
                                             output-lines
                                             context-lines
                                             id
                                             key
                                             (output-viewport-height 4)
                                             (output-empty-message "No plan output yet.")
                                             (context-empty-message "No context details yet."))
  (let* ((root-id (or id :plan-presentation))
         (panels (list (%steps-panel steps)
                       (%terminal-panel output-lines
                                        output-empty-message
                                        output-viewport-height)
                       (%context-panel context-lines context-empty-message)))
         (content (ptui.widgets.core:make-stack-widget
                   (cons (%text-widget "Plan Mode Workspace" :role :system)
                         panels)
                   :id (list root-id :stack)
                   :direction :column
                   :gap 0)))
    (ptui.widgets.core:make-box-widget
     content
     :id root-id
     :key key
     :borderp t
     :padding 0)))
