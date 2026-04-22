;;;; amoebum/src/plan-execution-helpers.lisp
;;;;
;;;; NXT-415: Helpers extracted from amoebum/src/plan-execution.lisp.
;;;;
;;;; This file holds the low-level pure / near-pure helpers that the rest of
;;;; the plan-execution machinery (state machine, rollback, lifecycle, loop)
;;;; layers on top of. No state-machine knowledge lives here — only:
;;;;   - status keyword normalization & terminal detection
;;;;   - per-step lookup and lifecycle marker mutators
;;;;   - completion check + run-id generation
;;;;   - plan-step → execution-step conversion
;;;;   - safe-string fallback
;;;;   - status-event publication helpers
;;;;   - git command runner used by rollback baseline / rollback restore
;;;;
;;;; Loaded immediately after src/plan-execution.lisp (which defines the
;;;; structs and shared defparameters) so these helpers can refer to the
;;;; struct accessors without forward declarations.

(in-package :amoebum)

;;; --- Status normalization & terminal detection -------------------------

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

;;; --- Step lookup + lifecycle marker mutators ---------------------------

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

(defun %mark-plan-execution-step-blocked (step)
  (check-type step plan-execution-step)
  (let ((now (get-universal-time)))
    (setf (plan-execution-step-status step) :blocked
          (plan-execution-step-started-at step)
          (or (plan-execution-step-started-at step) now)
          (plan-execution-step-finished-at step) now))
  step)

;;; --- Completion check --------------------------------------------------

(defun %complete-plan-execution-if-finished (state)
  (check-type state plan-execution-state)
  (when (null (plan-execution-state-pending-step-indexes state))
    (unless (eq :completed (plan-execution-state-status state))
      (setf (plan-execution-state-status state) :completed
            (plan-execution-state-current-step-index state) nil
            (plan-execution-state-finished-at state) (get-universal-time))
      (plan-execution-append-output
       "LIVE> All approved steps completed."
       :phase :execution
       :style :success
       :state state)))
  state)

;;; --- Plan-step → execution-step conversions ---------------------------

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

;;; --- Safe-string fallback ---------------------------------------------

(defun %safe-plan-execution-string (value &optional (fallback ""))
  (cond
    ((and (stringp value)
          (plusp (length value)))
     value)
    ((null value)
     fallback)
    (t
     (princ-to-string value))))

;;; --- Status-event publication ----------------------------------------

(defun %publish-plan-step-status-event (state step &key status)
  (check-type state plan-execution-state)
  (check-type step plan-execution-step)
  (let ((step-index (plan-execution-step-index step))
        (run-id (plan-execution-state-run-id state)))
    (when (and (integerp step-index)
               (stringp run-id)
               (plusp (length run-id)))
      (publish (current-event-bus)
               (make-plan-step-status-event
                :run-id run-id
                :step-index step-index
                :status (or status
                            (plan-execution-step-status step)
                            :pending)
                :description (plan-execution-step-description step)))))
  state)

(defun %publish-plan-step-status-snapshot (state)
  (check-type state plan-execution-state)
  (dolist (step (plan-execution-state-steps state))
    (%publish-plan-step-status-event state step))
  state)

;;; --- Run-id generation ------------------------------------------------

(defun %next-plan-execution-run-id ()
  (format nil "plan-exec-~D-~D"
          (get-universal-time)
          (get-internal-real-time)))

;;; --- Git command runner used by rollback ----------------------------

(defun default-plan-execution-git-command-runner (directory args)
  (handler-case
      (multiple-value-bind (stdout stderr exit-code)
          (uiop:run-program (append (list "git") args)
                            :directory directory
                            :ignore-error-status t
                            :output :string
                            :error-output :string)
        (list :stdout (or stdout "")
              :stderr (or stderr "")
              :exit-code (or exit-code 0)))
    (error (condition)
      (list :stdout ""
            :stderr (princ-to-string condition)
            :exit-code 127))))

(defun %plan-execution-run-git (directory args)
  (let ((runner (or *plan-execution-git-command-runner*
                    #'default-plan-execution-git-command-runner)))
    (funcall runner directory args)))

(defun %plan-execution-git-ok-p (result)
  (and (listp result)
       (zerop (or (getf result :exit-code) 1))))

(defun %plan-execution-git-output (result)
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (format nil "~A~@[ ~A~]"
                       (or (getf result :stdout) "")
                       (let ((stderr (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                  (or (getf result :stderr) ""))))
                         (and (plusp (length stderr))
                              stderr)))))
