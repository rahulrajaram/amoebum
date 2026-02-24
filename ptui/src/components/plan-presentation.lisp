(defpackage :ptui.components.plan-presentation
  (:use :cl)
  (:export
   #:plan-presentation-step
   #:plan-presentation-step-p
   #:make-plan-presentation-step
   #:plan-presentation-step-index
   #:plan-presentation-step-description
   #:plan-presentation-step-approved-p
   #:plan-presentation-step-file-paths
   #:plan-presentation-step-rationale-snippet
   #:plan-presentation-step-risk
   #:plan-presentation-step-status
   #:make-plan-mode-presentation-widget))

(in-package :ptui.components.plan-presentation)

(defstruct (plan-presentation-step
            (:constructor make-plan-presentation-step
                (&key index
                      description
                      (approved-p nil)
                      (file-paths '())
                      rationale-snippet
                      (risk :medium)
                      (status :pending))))
  index
  description
  (approved-p nil :type boolean)
  (file-paths '() :type list)
  rationale-snippet
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

(defun %normalize-path-list (values)
  (remove nil
          (loop for value in (or values '())
                for text = (typecase value
                             (pathname (namestring value))
                             (string value)
                             (symbol (symbol-name value))
                             (t (princ-to-string value)))
                for trimmed = (string-trim '(#\Space #\Tab #\Newline #\Return)
                                           (%safe-string text ""))
                when (plusp (length trimmed))
                  collect trimmed)))

(defun %normalize-rationale-snippet (rationale description)
  (let ((normalized (string-trim '(#\Space #\Tab #\Newline #\Return)
                                 (%safe-string rationale ""))))
    (if (plusp (length normalized))
        normalized
        (%safe-string description ""))))

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
                     :file-paths (%normalize-path-list
                                  (plan-presentation-step-file-paths step))
                     :rationale-snippet (%normalize-rationale-snippet
                                         (plan-presentation-step-rationale-snippet step)
                                         (plan-presentation-step-description step))
                     :risk (%normalize-risk (plan-presentation-step-risk step))
                     :status (%normalize-status (plan-presentation-step-status step)))
                    (make-plan-presentation-step
                     :index numeric-index
                     :description (%safe-string (getf step :description) "Describe this step.")
                     :approved-p (not (null (getf step :approved-p)))
                     :file-paths (%normalize-path-list (getf step :file-paths))
                     :rationale-snippet (%normalize-rationale-snippet
                                         (getf step :rationale-snippet)
                                         (getf step :description))
                     :risk (%normalize-risk (getf step :risk :medium))
                     :status (%normalize-status (getf step :status :pending))))))

(defun %text-widget (text &key id key (role :meta))
  (ptui.widgets.core:make-text-widget (%safe-string text "")
                                      :key key
                                      :id id
                                      :role role))

(defun %make-section-widget (title lines &key id (empty-message "[empty]"))
  (let* ((resolved-lines
           (if lines
               lines
               (list (cons empty-message :meta))))
         (rows
           (cons (%text-widget title
                               :id (and id (list id :title))
                               :role :system)
                 (loop for entry in resolved-lines
                       for index from 0
                       for line = (if (consp entry) (car entry) entry)
                       for role = (if (consp entry) (cdr entry) :meta)
                       collect (%text-widget line
                                             :id (and id (list id :line index))
                                             :role role))))
         (content (ptui.widgets.core:make-stack-widget
                   rows
                   :id (and id (list id :content))
                   :direction :column
                   :gap 0)))
    (ptui.widgets.core:make-box-widget content
                                       :id id
                                       :borderp t
                                       :padding 0)))

(defun %step-line (step selected-p)
  (check-type step plan-presentation-step)
  (let ((approval (if (plan-presentation-step-approved-p step) "[x]" "[ ]"))
        (selection (if selected-p ">" " "))
        (risk (string-downcase (symbol-name (%normalize-risk (plan-presentation-step-risk step)))))
        (status (string-downcase (symbol-name (%normalize-status
                                               (plan-presentation-step-status step))))))
    (format nil "~A ~A ~D. ~A (risk: ~A, status: ~A)"
            selection
            approval
            (or (plan-presentation-step-index step) 0)
            (%safe-string (plan-presentation-step-description step) "Describe this step.")
            risk
            status)))

(defun %resolve-selected-step-index (steps selected-step-index)
  (let ((indexes
          (loop for step in (or steps '())
                for index = (plan-presentation-step-index step)
                when (integerp index)
                  collect index)))
    (cond
      ((and (integerp selected-step-index)
            (member selected-step-index indexes :test #'=))
       selected-step-index)
      (indexes
       (first indexes))
      (t
       nil))))

(defun %find-selected-step (steps selected-step-index)
  (when (integerp selected-step-index)
    (find selected-step-index
          (or steps '())
          :key #'plan-presentation-step-index
          :test #'=)))

(defun %steps-panel (steps selected-step-index)
  (%make-section-widget
   "Plan Steps"
   (loop for step in (or steps '())
         for step-index = (plan-presentation-step-index step)
         for selected-p = (and (integerp selected-step-index)
                               (integerp step-index)
                               (= selected-step-index step-index))
         collect (cons (%step-line step selected-p)
                       (cond
                         (selected-p :system)
                         ((plan-presentation-step-approved-p step) :assistant)
                         (t :meta))))
   :id :plan-presentation-steps
   :empty-message "No plan steps captured yet."))

(defun %normalize-recovery-actions (recovery-actions)
  (remove nil
          (loop for action in (or recovery-actions '())
                for text = (string-trim '(#\Space #\Tab #\Newline #\Return)
                                        (%safe-string action ""))
                when (plusp (length text))
                  collect text)))

(defun %normalize-output-line-entry (entry)
  (cond
    ((stringp entry)
     (list :text (%safe-string entry "")
           :severity :info
           :style :plain
           :step-index nil
           :recovery-actions '()))
    ((and (listp entry)
          (or (getf entry :text)
              (getf entry :line)))
     (list :text (%safe-string (or (getf entry :text)
                                   (getf entry :line))
                               "")
           :severity (or (getf entry :severity) :info)
           :style (or (getf entry :style) :plain)
           :step-index (and (integerp (getf entry :step-index))
                            (getf entry :step-index))
           :recovery-actions (%normalize-recovery-actions
                              (getf entry :recovery-actions))))
    (t
     (list :text (%safe-string entry "")
           :severity :info
           :style :plain
           :step-index nil
           :recovery-actions '()))))

(defun %normalize-output-line-entries (output-lines output-line-entries)
  (let ((raw-entries (if output-line-entries
                         output-line-entries
                         (or output-lines '()))))
    (loop for entry in raw-entries
          for normalized = (%normalize-output-line-entry entry)
          when (plusp (length (getf normalized :text "")))
            collect normalized)))

(defun %terminal-panel (entries
                        output-empty-message
                        output-viewport-height
                        output-stdin-capture-policy)
  (let ((state (ptui.components.terminal-pane:make-terminal-pane-state
                :title "Plan Output"
                :lines (loop for entry in entries
                             collect (getf entry :text))
                :line-metadata (loop for entry in entries
                                     collect (list :severity (or (getf entry :severity) :info)
                                                   :style (or (getf entry :style) :plain)
                                                   :step-index (getf entry :step-index)
                                                   :recovery-actions (copy-list
                                                                      (or (getf entry :recovery-actions)
                                                                          '()))))
                :empty-message output-empty-message
                :stdin-capture-policy output-stdin-capture-policy)))
    (ptui.components.terminal-pane:make-terminal-pane-widget
     state
     :id :plan-presentation-output
     :viewport-height output-viewport-height)))

(defun %selected-step-context-lines (selected-step)
  (if (null selected-step)
      (list (cons "Selected step: none" :meta)
            (cons "File references: none" :meta)
            (cons "Rationale snippet: none" :meta))
      (let* ((file-paths (plan-presentation-step-file-paths selected-step))
             (rationale (%safe-string
                         (plan-presentation-step-rationale-snippet selected-step)
                         (plan-presentation-step-description selected-step)))
             (lines (list (cons (format nil "Selected step: ~D"
                                        (or (plan-presentation-step-index selected-step) 0))
                                :system)
                          (cons (format nil "Rationale snippet: ~A"
                                        (%safe-string rationale "none"))
                                :assistant))))
        (if file-paths
            (append lines
                    (list (cons "File references:" :system))
                    (loop for path in file-paths
                          collect (cons (format nil "  - ~A" path) :meta)))
            (append lines
                    (list (cons "File references: none" :meta)))))))

(defun %latest-failure-entry (entries)
  (loop for entry in (reverse (or entries '()))
        for severity = (getf entry :severity :info)
        for step-index = (getf entry :step-index)
        when (and (member severity '(:error :critical) :test #'eq)
                  (integerp step-index))
          do (return entry)
        finally (return nil)))

(defun %failure-drilldown-lines (steps entries)
  (let ((failure-entry (%latest-failure-entry entries)))
    (when failure-entry
      (let* ((step-index (getf failure-entry :step-index))
             (step (%find-selected-step steps step-index))
             (description (%safe-string
                           (and step
                                (plan-presentation-step-description step))
                           "No matching plan step description available."))
             (error-line (%safe-string (getf failure-entry :text) "unknown failure"))
             (recovery-actions (%normalize-recovery-actions
                                (getf failure-entry :recovery-actions))))
        (append
         (list (cons (format nil "Failure drill-down: step ~D" step-index) :system)
               (cons (format nil "Originating step: ~D. ~A" step-index description) :assistant)
               (cons (format nil "Terminal error: ~A" error-line) :meta))
         (if recovery-actions
             (append
              (list (cons "Suggested recovery actions:" :system))
              (loop for action in recovery-actions
                    collect (cons (format nil "  - ~A" action) :meta)))
             (list (cons "Suggested recovery actions: none provided." :meta))))))))

(defun %context-panel (selected-step steps entries context-lines context-empty-message)
  (%make-section-widget
   "Context Inspector"
   (append
    (%selected-step-context-lines selected-step)
    (%failure-drilldown-lines steps entries)
    (when context-lines
      (cons (cons "Summary:" :system)
            (loop for line in (or context-lines '())
                  collect (cons (%safe-string line "") :meta)))))
   :id :plan-presentation-context
   :empty-message context-empty-message))

(defun make-plan-mode-presentation-widget (&key
                                             steps
                                             selected-step-index
                                             output-lines
                                             output-line-entries
                                             context-lines
                                             id
                                             key
                                             (output-viewport-height 4)
                                             (output-stdin-capture-policy :disabled)
                                             (output-empty-message "No plan output yet.")
                                             (context-empty-message "No context details yet."))
  (let* ((root-id (or id :plan-presentation))
         (normalized-steps (%normalize-steps steps))
         (normalized-output-entries (%normalize-output-line-entries
                                     output-lines
                                     output-line-entries))
         (resolved-selected-step-index
           (%resolve-selected-step-index normalized-steps selected-step-index))
         (selected-step
           (%find-selected-step normalized-steps resolved-selected-step-index))
         (panels (list (%steps-panel normalized-steps
                                     resolved-selected-step-index)
                       (%terminal-panel normalized-output-entries
                                        output-empty-message
                                        output-viewport-height
                                        output-stdin-capture-policy)
                       (%context-panel selected-step
                                       normalized-steps
                                       normalized-output-entries
                                       context-lines
                                       context-empty-message)))
         (content (ptui.widgets.core:make-stack-widget
                   (cons (%text-widget "Plan Mode Workspace"
                                      :id (list root-id :title)
                                      :role :system)
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
