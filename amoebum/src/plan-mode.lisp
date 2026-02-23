(in-package :amoebum)

(defparameter *known-plan-step-risk-levels*
  '(:low :medium :high))

(defparameter *known-plan-review-decisions*
  '(:pending :approved :rejected :modification-requested))

(defstruct (plan-step
            (:constructor make-plan-step
                (&key index
                 description
                 (file-paths '())
                 (risk :medium)
                 (depends-on '()))))
  index
  description
  (file-paths '() :type list)
  (risk :medium)
  (depends-on '() :type list))

(defstruct (plan-mode-state
            (:constructor %make-plan-mode-state
                (&key
                   (active-p nil)
                   entered-at
                   exited-at
                   (steps '())
                   (review-pending-p nil)
                   (review-decision :pending)
                   review-notes
                   review-decided-at
                   review-last-presented-at
                   last-plan-markdown
                   last-output-path
                   last-exit-reason)))
  (active-p nil :type boolean)
  entered-at
  exited-at
  (steps '() :type list)
  (review-pending-p nil :type boolean)
  (review-decision :pending)
  review-notes
  review-decided-at
  review-last-presented-at
  last-plan-markdown
  last-output-path
  last-exit-reason)

(defparameter *plan-mode-state* (%make-plan-mode-state))

(defun %timestamp-compact (&optional (timestamp (get-universal-time)))
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time timestamp)
    (format nil "~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0D"
            year month day hour minute second)))

(defun %timestamp-iso8601 (&optional (timestamp (get-universal-time)))
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time timestamp)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour minute second)))

(defun %safe-plan-string (value &optional (fallback ""))
  (cond
    ((and (stringp value)
          (plusp (length value)))
     value)
    ((null value)
     fallback)
    (t
     (princ-to-string value))))

(defun %normalize-plan-risk (value)
  (let* ((risk-text (typecase value
                      (symbol (symbol-name value))
                      (string value)
                      (t nil)))
         (risk (if (and (stringp risk-text)
                        (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                    risk-text))))
                   (intern (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                       risk-text))
                           :keyword)
                   :medium)))
    (if (member risk *known-plan-step-risk-levels* :test #'eq)
        risk
        :medium)))

(defun %normalize-plan-review-decision (value)
  (let* ((decision-text (typecase value
                          (symbol (symbol-name value))
                          (string value)
                          (t nil)))
         (decision (if (and (stringp decision-text)
                            (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                        decision-text))))
                       (intern (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                           decision-text))
                               :keyword)
                       :pending)))
    (if (member decision *known-plan-review-decisions* :test #'eq)
        decision
        :pending)))

(defun set-plan-review-decision (decision &key notes (state (current-plan-mode-state)))
  (check-type state plan-mode-state)
  (let ((normalized-notes (string-trim '(#\Space #\Tab #\Newline #\Return)
                                       (%safe-plan-string notes "")))
        (normalized-decision (%normalize-plan-review-decision decision)))
    (setf (plan-mode-state-review-decision state) normalized-decision
          (plan-mode-state-review-notes state)
          (and (plusp (length normalized-notes)) normalized-notes)
          (plan-mode-state-review-decided-at state) (get-universal-time)
          (plan-mode-state-review-pending-p state)
          (not (null (member normalized-decision
                             '(:pending :modification-requested)
                             :test #'eq)))))
  state)

(defun %normalize-path-list (values)
  (loop for value in values
        for text = (typecase value
                     (pathname (namestring value))
                     (string value)
                     (symbol (symbol-name value))
                     (t (princ-to-string value)))
        when (and (stringp text)
                  (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
          collect text))

(defun %string-contains-digits-p (value)
  (and (stringp value)
       (loop for char across value
             thereis (digit-char-p char))))

(defun %extract-first-integer (value)
  (let* ((text (%safe-plan-string value ""))
         (length (length text)))
    (loop with start = nil
          for index from 0 below length
          for char = (char text index) do
            (cond
              ((digit-char-p char)
               (unless start
                 (setf start index)))
              (start
               (return (parse-integer text :start start :end index))))
          finally (when start
                    (return (parse-integer text :start start :end length))))))

(defun %normalize-dependency-list (depends-on max-index)
  (let ((result '()))
    (dolist (entry (or depends-on '()))
      (let ((index
              (cond
                ((integerp entry)
                 entry)
                ((and (stringp entry)
                      (%string-contains-digits-p entry))
                 (%extract-first-integer entry))
                ((symbolp entry)
                 (%extract-first-integer (symbol-name entry)))
                (t
                 nil))))
        (when (and (integerp index)
                   (>= index 1)
                   (<= index max-index))
          (push index result))))
    (sort (remove-duplicates result :test #'=) #'<)))

(defun %description-references-step-indexes (description max-index)
  (let ((text (string-downcase (%safe-plan-string description "")))
        (result '()))
    (loop for index from 1 to max-index do
      (let ((token (format nil "step ~D" index)))
        (when (search token text :test #'char=)
          (push index result))))
    (sort (remove-duplicates result :test #'=) #'<)))

(defun %description-sequential-cue-p (description)
  (let ((text (string-downcase (%safe-plan-string description ""))))
    (or (search " then " (format nil " ~A " text) :test #'char=)
        (search " next " (format nil " ~A " text) :test #'char=)
        (search " after " (format nil " ~A " text) :test #'char=)
        (search " once " (format nil " ~A " text) :test #'char=)
        (search " following " (format nil " ~A " text) :test #'char=))))

(defun %infer-step-dependencies (description depends-on next-index)
  (let* ((max-prior-index (1- next-index))
         (normalized-explicit (%normalize-dependency-list depends-on max-prior-index))
         (inferred-by-reference (%description-references-step-indexes description max-prior-index)))
    (cond
      (normalized-explicit
       normalized-explicit)
      (inferred-by-reference
       inferred-by-reference)
      ((and (> next-index 1)
            (%description-sequential-cue-p description))
       (list max-prior-index))
      (t
       '()))))

(defun current-plan-mode-state ()
  (or *plan-mode-state*
      (setf *plan-mode-state* (%make-plan-mode-state))))

(defun plan-mode-active-p (&optional (state (current-plan-mode-state)))
  (and (plan-mode-state-p state)
       (plan-mode-state-active-p state)))

(defun clear-plan-mode-steps (&optional (state (current-plan-mode-state)))
  (check-type state plan-mode-state)
  (setf (plan-mode-state-steps state) '())
  state)

(defun add-plan-step (description &key file-paths (risk :medium) depends-on
                                     (state (current-plan-mode-state)))
  (check-type state plan-mode-state)
  (let ((next-index (1+ (length (plan-mode-state-steps state)))))
    (push (make-plan-step :index next-index
                          :description (%safe-plan-string description "Describe the step.")
                          :file-paths (%normalize-path-list file-paths)
                          :risk (%normalize-plan-risk risk)
                          :depends-on (%infer-step-dependencies description
                                                                depends-on
                                                                next-index))
          (plan-mode-state-steps state))
    (setf (plan-mode-state-steps state)
          (sort (copy-list (plan-mode-state-steps state)) #'< :key #'plan-step-index)))
  state)

(defun %plan-step-markdown (step stream)
  (format stream "~D. ~A~%"
          (or (plan-step-index step) 0)
          (%safe-plan-string (plan-step-description step) "Describe the step."))
  (format stream "   - risk: ~A~%"
          (string-downcase (symbol-name (%normalize-plan-risk (plan-step-risk step)))))
  (when (plan-step-file-paths step)
    (format stream "   - files: ~{`~A`~^, ~}~%"
            (%normalize-path-list (plan-step-file-paths step))))
  (when (plan-step-depends-on step)
    (format stream "   - depends_on: ~{~A~^, ~}~%"
            (plan-step-depends-on step))))

(defun %plan-markdown (state reason)
  (with-output-to-string (stream)
    (let ((steps (plan-mode-state-steps state)))
      (format stream "# Amoebum Plan~%~%")
      (format stream "- generated_at: ~A~%" (%timestamp-iso8601))
      (format stream "- exit_reason: ~A~%"
              (if reason
                  (%safe-plan-string reason "manual-exit")
                  "manual-exit"))
      (format stream "- step_count: ~D~%~%" (length steps))
      (format stream "## Steps~%~%")
      (if steps
          (dolist (step steps)
            (%plan-step-markdown step stream))
          (format stream "1. No explicit steps captured.~%")))))

(defun plan-markdown (&key
                        (state (current-plan-mode-state))
                        reason)
  (check-type state plan-mode-state)
  (%plan-markdown state reason))

(defun default-plan-output-path (&key project-root (timestamp (get-universal-time)))
  (let* ((root-path
           (uiop:ensure-directory-pathname
            (or project-root
                (ignore-errors (config-project-root (current-config)))
                *default-pathname-defaults*)))
         (output-dir (merge-pathnames #P".amoebum/plans/" root-path))
         (filename (format nil "plan-~A.md" (%timestamp-compact timestamp)))
         (output-path (merge-pathnames filename output-dir)))
    (ensure-directories-exist output-path)
    output-path))

(defun write-plan-output (&key
                            (state (current-plan-mode-state))
                            output-path
                            reason)
  (check-type state plan-mode-state)
  (let ((resolved-output-path (or output-path
                                  (default-plan-output-path))))
    (ensure-directories-exist resolved-output-path)
    (with-open-file (stream resolved-output-path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string (%plan-markdown state reason) stream))
    (setf (plan-mode-state-last-output-path state) resolved-output-path
          (plan-mode-state-last-exit-reason state) reason)
    resolved-output-path))

(defun enter-plan-mode (&key
                          (state (current-plan-mode-state))
                          (clear-steps-p t))
  (check-type state plan-mode-state)
  (when clear-steps-p
    (clear-plan-mode-steps state))
  (setf (plan-mode-state-active-p state) t
        (plan-mode-state-entered-at state) (get-universal-time)
        (plan-mode-state-exited-at state) nil
        (plan-mode-state-review-pending-p state) nil
        (plan-mode-state-review-decision state) :pending
        (plan-mode-state-review-notes state) nil
        (plan-mode-state-review-decided-at state) nil
        (plan-mode-state-review-last-presented-at state) nil
        (plan-mode-state-last-plan-markdown state) nil)
  state)

(defun exit-plan-mode (&key
                         (state (current-plan-mode-state))
                         output-path
                         (reason :user-approved-plan)
                         (write-output-p t))
  (check-type state plan-mode-state)
  (when (plan-mode-state-active-p state)
    (let ((captured-plan (%plan-markdown state reason)))
      (setf (plan-mode-state-last-plan-markdown state) captured-plan
            (plan-mode-state-review-pending-p state) t
            (plan-mode-state-review-decision state) :pending
            (plan-mode-state-review-notes state) nil
            (plan-mode-state-review-decided-at state) nil)))
  (let ((written-output-path
          (and (plan-mode-state-active-p state)
               write-output-p
               (write-plan-output :state state
                                  :output-path output-path
                                  :reason reason))))
    (when (and (plan-mode-state-active-p state)
               (not write-output-p))
      (setf (plan-mode-state-last-output-path state) nil
            (plan-mode-state-last-exit-reason state) reason))
    (setf (plan-mode-state-active-p state) nil
          (plan-mode-state-exited-at state) (get-universal-time))
    (values state written-output-path)))

(defun toggle-plan-mode (&key
                           (state (current-plan-mode-state))
                           output-path
                           (reason :toggle)
                           (write-output-p t))
  (check-type state plan-mode-state)
  (if (plan-mode-state-active-p state)
      (multiple-value-bind (updated-state written-output-path)
          (exit-plan-mode :state state
                          :output-path output-path
                          :reason reason
                          :write-output-p write-output-p)
        (values updated-state :disabled written-output-path))
      (values (enter-plan-mode :state state :clear-steps-p t) :enabled nil)))
