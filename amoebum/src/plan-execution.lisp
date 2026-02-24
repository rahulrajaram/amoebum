(in-package :amoebum)

(defparameter *known-plan-execution-statuses*
  '(:idle :ready :running :paused :completed :failed :aborted))

(defstruct (plan-execution-step
            (:constructor make-plan-execution-step
                (&key index
                      description
                      (file-paths '())
                      (risk :medium)
                      (depends-on '())
                      (approved-p nil)
                      (status :pending)
                      started-at
                      finished-at)))
  index
  description
  (file-paths '() :type list)
  (risk :medium)
  (depends-on '() :type list)
  (approved-p nil :type boolean)
  (status :pending)
  started-at
  finished-at)

(defstruct (plan-execution-state
            (:constructor %make-plan-execution-state
                (&key run-id
                      (status :idle)
                      created-at
                      started-at
                      finished-at
                      source-plan-exited-at
                      source-plan-exit-reason
                      (steps '())
                      (ordered-step-indexes '())
                      (approved-step-indexes '())
                      (pending-step-indexes '())
                      (completed-step-indexes '())
                      current-step-index
                      failure-reason
                      abort-reason)))
  run-id
  (status :idle)
  created-at
  started-at
  finished-at
  source-plan-exited-at
  source-plan-exit-reason
  (steps '() :type list)
  (ordered-step-indexes '() :type list)
  (approved-step-indexes '() :type list)
  (pending-step-indexes '() :type list)
  (completed-step-indexes '() :type list)
  current-step-index
  failure-reason
  abort-reason)

(defparameter *plan-execution-state* (%make-plan-execution-state))

(defun %normalize-plan-execution-status (value)
  (let* ((status-text (typecase value
                        (keyword (symbol-name value))
                        (symbol (symbol-name value))
                        (string value)
                        (t nil)))
         (status (if (and (stringp status-text)
                          (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                      status-text))))
                     (intern (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                         status-text))
                             :keyword)
                     :idle)))
    (if (member status *known-plan-execution-statuses* :test #'eq)
        status
        :idle)))

(defun %plan-execution-terminal-status-p (status)
  (member (%normalize-plan-execution-status status)
          '(:completed :failed :aborted)
          :test #'eq))

(defun %find-plan-execution-step (state step-index)
  (find step-index
        (plan-execution-state-steps state)
        :key #'plan-execution-step-index
        :test #'=))

(defun %mark-plan-execution-step-running (step)
  (check-type step plan-execution-step)
  (setf (plan-execution-step-status step) :running
        (plan-execution-step-started-at step) (get-universal-time)
        (plan-execution-step-finished-at step) nil)
  step)

(defun %mark-plan-execution-step-completed (step)
  (check-type step plan-execution-step)
  (setf (plan-execution-step-status step) :completed
        (plan-execution-step-finished-at step) (get-universal-time))
  step)

(defun %mark-plan-execution-step-pending (step)
  (check-type step plan-execution-step)
  (setf (plan-execution-step-status step) :pending
        (plan-execution-step-started-at step) nil
        (plan-execution-step-finished-at step) nil)
  step)

(defun %complete-plan-execution-if-finished (state)
  (check-type state plan-execution-state)
  (when (null (plan-execution-state-pending-step-indexes state))
    (setf (plan-execution-state-status state) :completed
          (plan-execution-state-current-step-index state) nil
          (plan-execution-state-finished-at state) (get-universal-time)))
  state)

(defun %plan-step-order (steps)
  (loop for step in (or steps '())
        for index = (plan-step-index step)
        when (integerp index)
          collect index))

(defun %approved-step-indexes-for-execution (plan-state ordered-indexes)
  (remove-if-not (lambda (index)
                   (member index ordered-indexes :test #'=))
                 (or (plan-mode-state-approved-step-indexes plan-state) '())))

(defun %plan-step->execution-step (step approved-p)
  (make-plan-execution-step
   :index (plan-step-index step)
   :description (plan-step-description step)
   :file-paths (copy-list (or (plan-step-file-paths step) '()))
   :risk (plan-step-risk step)
   :depends-on (copy-list (or (plan-step-depends-on step) '()))
   :approved-p (not (null approved-p))
   :status :pending))

(defun %next-plan-execution-run-id ()
  (format nil "plan-exec-~D-~D"
          (get-universal-time)
          (get-internal-real-time)))

(defun current-plan-execution-state ()
  (or *plan-execution-state*
      (setf *plan-execution-state* (%make-plan-execution-state))))

(defun reset-plan-execution-state (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (setf (plan-execution-state-run-id state) nil
        (plan-execution-state-status state) :idle
        (plan-execution-state-created-at state) nil
        (plan-execution-state-started-at state) nil
        (plan-execution-state-finished-at state) nil
        (plan-execution-state-source-plan-exited-at state) nil
        (plan-execution-state-source-plan-exit-reason state) nil
        (plan-execution-state-steps state) '()
        (plan-execution-state-ordered-step-indexes state) '()
        (plan-execution-state-approved-step-indexes state) '()
        (plan-execution-state-pending-step-indexes state) '()
        (plan-execution-state-completed-step-indexes state) '()
        (plan-execution-state-current-step-index state) nil
        (plan-execution-state-failure-reason state) nil
        (plan-execution-state-abort-reason state) nil)
  state)

(defun initialize-plan-execution (&key
                                    (plan-state (current-plan-mode-state))
                                    (state (current-plan-execution-state))
                                    run-id)
  (check-type plan-state plan-mode-state)
  (check-type state plan-execution-state)
  (let* ((steps (copy-list (or (plan-mode-state-steps plan-state) '())))
         (ordered-indexes (%plan-step-order steps))
         (approved-step-indexes (%approved-step-indexes-for-execution plan-state
                                                                      ordered-indexes)))
    (unless ordered-indexes
      (error "No plan steps are available for execution."))
    (unless approved-step-indexes
      (error "No approved plan steps are available for execution."))
    (setf (plan-execution-state-run-id state) (or run-id (%next-plan-execution-run-id))
          (plan-execution-state-status state) :ready
          (plan-execution-state-created-at state) (get-universal-time)
          (plan-execution-state-started-at state) nil
          (plan-execution-state-finished-at state) nil
          (plan-execution-state-source-plan-exited-at state) (plan-mode-state-exited-at plan-state)
          (plan-execution-state-source-plan-exit-reason state)
          (plan-mode-state-last-exit-reason plan-state)
          (plan-execution-state-steps state)
          (loop for step in steps
                collect (%plan-step->execution-step
                         step
                         (member (plan-step-index step)
                                 approved-step-indexes
                                 :test #'=)))
          (plan-execution-state-ordered-step-indexes state) ordered-indexes
          (plan-execution-state-approved-step-indexes state) (copy-list approved-step-indexes)
          (plan-execution-state-pending-step-indexes state) (copy-list approved-step-indexes)
          (plan-execution-state-completed-step-indexes state) '()
          (plan-execution-state-current-step-index state) nil
          (plan-execution-state-failure-reason state) nil
          (plan-execution-state-abort-reason state) nil)
    state))

(defun plan-execution-ready-p (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (eq :ready (plan-execution-state-status state)))

(defun start-plan-execution (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (let ((status (%normalize-plan-execution-status (plan-execution-state-status state))))
    (unless (member status '(:ready :paused) :test #'eq)
      (error "Plan execution cannot start from status ~S." status))
    (when (null (plan-execution-state-started-at state))
      (setf (plan-execution-state-started-at state) (get-universal-time)))
    (setf (plan-execution-state-status state) :running
          (plan-execution-state-finished-at state) nil)
    state))

(defun pause-plan-execution (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (unless (eq :running (%normalize-plan-execution-status (plan-execution-state-status state)))
    (error "Plan execution can only be paused while running."))
  (setf (plan-execution-state-status state) :paused)
  state)

(defun resume-plan-execution (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (unless (eq :paused (%normalize-plan-execution-status (plan-execution-state-status state)))
    (error "Plan execution can only be resumed from paused state."))
  (start-plan-execution state))

(defun abort-plan-execution (&key
                               (state (current-plan-execution-state))
                               reason)
  (check-type state plan-execution-state)
  (when (%plan-execution-terminal-status-p (plan-execution-state-status state))
    (error "Plan execution is already terminal (~S)."
           (plan-execution-state-status state)))
  (setf (plan-execution-state-status state) :aborted
        (plan-execution-state-abort-reason state) reason
        (plan-execution-state-finished-at state) (get-universal-time))
  state)

(defun plan-execution-next-step-index (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (first (plan-execution-state-pending-step-indexes state)))

(defun execute-next-approved-plan-step (executor &key (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (unless (functionp executor)
    (error "Plan execution step executor must be a function."))
  (let ((status (%normalize-plan-execution-status (plan-execution-state-status state))))
    (when (%plan-execution-terminal-status-p status)
      (error "Plan execution is already terminal (~S)." status))
    (when (eq :idle status)
      (error "Plan execution has not been initialized."))
    (when (eq :paused status)
      (error "Plan execution is paused. Resume it before executing steps."))
    (when (eq :ready status)
      (start-plan-execution state))
    (unless (eq :running (plan-execution-state-status state))
      (error "Plan execution cannot execute steps from status ~S."
             (plan-execution-state-status state)))
    (let ((next-step-index (plan-execution-next-step-index state)))
      (unless next-step-index
        (%complete-plan-execution-if-finished state)
        (return-from execute-next-approved-plan-step
          (values state nil nil t)))
      (let ((step (%find-plan-execution-step state next-step-index)))
        (unless step
          (error "Missing execution step for approved index ~D." next-step-index))
        (setf (plan-execution-state-current-step-index state) next-step-index)
        (%mark-plan-execution-step-running step)
        (let ((result nil)
              (completed-p nil))
          (unwind-protect
               (progn
                 (setf result (funcall executor step)
                       completed-p t)
                 (%mark-plan-execution-step-completed step)
                 (setf (plan-execution-state-pending-step-indexes state)
                       (rest (plan-execution-state-pending-step-indexes state))
                       (plan-execution-state-completed-step-indexes state)
                       (append (plan-execution-state-completed-step-indexes state)
                               (list next-step-index))
                       (plan-execution-state-current-step-index state) nil)
                 (%complete-plan-execution-if-finished state))
            (unless completed-p
              (%mark-plan-execution-step-pending step)
              (setf (plan-execution-state-current-step-index state) nil)))
          (values state
                  step
                  result
                  (null (plan-execution-state-pending-step-indexes state))))))))

(defun execute-approved-plan-steps (executor &key (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (unless (functionp executor)
    (error "Plan execution executor must be a function."))
  (let ((execution-results '()))
    (loop
      (multiple-value-bind (_ step result done-p)
          (execute-next-approved-plan-step executor :state state)
        (declare (ignore _))
        (when step
          (push (cons (plan-execution-step-index step) result) execution-results))
        (when done-p
          (return (values state (nreverse execution-results))))))))
