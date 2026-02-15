(in-package :amoebum)

(defparameter *known-plan-step-risk-levels*
  '(:low :medium :high))

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
                   last-output-path
                   last-exit-reason)))
  (active-p nil :type boolean)
  entered-at
  exited-at
  (steps '() :type list)
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
  (let ((risk (if (symbolp value)
                  (intern (string-upcase (symbol-name value)) :keyword)
                  :medium)))
    (if (member risk *known-plan-step-risk-levels* :test #'eq)
        risk
        :medium)))

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
                          :depends-on (copy-list (or depends-on '())))
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
        (plan-mode-state-exited-at state) nil)
  state)

(defun exit-plan-mode (&key
                         (state (current-plan-mode-state))
                         output-path
                         (reason :user-approved-plan)
                         (write-output-p t))
  (check-type state plan-mode-state)
  (let ((written-output-path
          (and (plan-mode-state-active-p state)
               write-output-p
               (write-plan-output :state state
                                  :output-path output-path
                                  :reason reason))))
    (setf (plan-mode-state-active-p state) nil
          (plan-mode-state-exited-at state) (get-universal-time))
    (values state written-output-path)))

(defun toggle-plan-mode (&key
                           (state (current-plan-mode-state))
                           output-path
                           (reason :toggle))
  (check-type state plan-mode-state)
  (if (plan-mode-state-active-p state)
      (multiple-value-bind (updated-state written-output-path)
          (exit-plan-mode :state state
                          :output-path output-path
                          :reason reason
                          :write-output-p t)
        (values updated-state :disabled written-output-path))
      (values (enter-plan-mode :state state :clear-steps-p t) :enabled nil)))
