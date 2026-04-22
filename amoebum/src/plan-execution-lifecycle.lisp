;;;; amoebum/src/plan-execution-lifecycle.lisp
;;;;
;;;; NXT-415: Public lifecycle entry points extracted from
;;;; amoebum/src/plan-execution.lisp.
;;;;
;;;; Each entry point dispatches through the declarative transition table
;;;; in src/plan-execution-state-machine.lisp and applies the resulting
;;;; transition via %apply-plan-execution-transition! from
;;;; src/plan-execution-effects.lisp.
;;;;
;;;; The functions live here (not in the residual facade) so that the
;;;; thin coordinator surface in src/plan-execution.lisp stays focused on
;;;; struct definitions, the global *plan-execution-state* defparameter,
;;;; and the public agentic-loop entry points
;;;; (execute-next-approved-plan-step, execute-approved-plan-steps).

(in-package :amoebum)

;;; --- Reset / initialize / status checks ----------------------------

(defun reset-plan-execution-state (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (let ((transition (%dispatch-plan-execution-transition state :reset)))
    (%apply-plan-execution-transition! state transition))
  ;; Also clear fields not covered by the transition table state-updates
  (setf (plan-execution-state-run-id state) nil
        (plan-execution-state-created-at state) nil
        (plan-execution-state-source-plan-exited-at state) nil
        (plan-execution-state-source-plan-exit-reason state) nil
        (plan-execution-state-steps state) '()
        (plan-execution-state-ordered-step-indexes state) '()
        (plan-execution-state-approved-step-indexes state) '()
        (plan-execution-state-pending-step-indexes state) '()
        (plan-execution-state-completed-step-indexes state) '()
        (plan-execution-state-continuity-output state) '()
        (plan-execution-state-rollback-baseline-stash state) nil
        (plan-execution-state-rollback-baseline-directory state) nil
        (plan-execution-state-rollback-attempted-p state) nil
        (plan-execution-state-rollback-succeeded-p state) nil
        (plan-execution-state-rollback-notes state) nil)
  state)

(defun initialize-plan-execution (&key
                                    (plan-state (current-plan-mode-state))
                                    (state (current-plan-execution-state))
                                    run-id)
  (check-type plan-state plan-mode-state)
  (check-type state plan-execution-state)
  (when (%plan-execution-terminal-status-p
         (plan-execution-state-status state))
    (reset-plan-execution-state state))
  (let ((transition (%dispatch-plan-execution-transition state :initialize
                      :plan-state plan-state :run-id run-id)))
    (%apply-plan-execution-transition! state transition)
    state))

(defun plan-execution-ready-p (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (eq :ready (plan-execution-state-status state)))

;;; --- Elapsed-time + progress reporting -----------------------------

(defun %plan-execution-format-elapsed-seconds (elapsed-seconds)
  (let* ((total-seconds (max 0 (or elapsed-seconds 0)))
         (hours (truncate total-seconds 3600))
         (remaining (mod total-seconds 3600))
         (minutes (truncate remaining 60))
         (seconds (mod remaining 60)))
    (cond
      ((> hours 0)
       (format nil "~Dh ~Dm ~Ds" hours minutes seconds))
      ((> minutes 0)
       (format nil "~Dm ~Ds" minutes seconds))
      (t
       (format nil "~Ds" seconds)))))

(defun plan-execution-elapsed-seconds (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (let* ((started-at (plan-execution-state-started-at state))
         (finished-at (plan-execution-state-finished-at state))
         (status (plan-execution-state-status state))
         (end-time (if (member status '(:completed :failed :aborted) :test #'eq)
                       finished-at
                       (get-universal-time))))
    (if (and (integerp started-at)
             (integerp end-time))
        (max 0 (- end-time started-at))
        0)))

(defun plan-execution-progress-line (&optional (state (current-plan-execution-state))
                                               &key
                                                 (prefix "Execution progress"))
  (check-type state plan-execution-state)
  (let* ((approved-indexes (or (plan-execution-state-approved-step-indexes state) '()))
         (total (length approved-indexes))
         (prefix-text (%safe-plan-execution-string prefix "Execution progress")))
    (unless (plusp total)
      (return-from plan-execution-progress-line
        (format nil "~A: step 0 of 0 (elapsed 0s)" prefix-text)))
    (let* ((current-index (plan-execution-state-current-step-index state))
           (current-position (and (integerp current-index)
                                  (position current-index approved-indexes :test #'=)))
           (completed (length (plan-execution-state-completed-step-indexes state)))
           (status (plan-execution-state-status state))
           (step-number
             (cond
               ((integerp current-position)
                (1+ current-position))
               ((eq status :completed)
                total)
               ((plusp completed)
                (min total (1+ completed)))
               (t
                1))))
      (format nil "~A: step ~D of ~D (elapsed ~A)"
              prefix-text
              step-number
              total
              (%plan-execution-format-elapsed-seconds
               (plan-execution-elapsed-seconds state))))))

(defun %append-plan-execution-progress-output (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (plan-execution-append-output
   (format nil "LIVE> ~A"
           (plan-execution-progress-line state :prefix "Progress"))
   :phase :execution
   :style :meta
   :state state)
  state)

;;; --- Start / pause / resume / abort + next-step ---------------------

(defun start-plan-execution (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (let ((transition (%dispatch-plan-execution-transition state :start)))
    (%apply-plan-execution-transition! state transition)
    state))

(defun pause-plan-execution (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (let ((transition (%dispatch-plan-execution-transition state :pause)))
    (%apply-plan-execution-transition! state transition)
    state))

(defun resume-plan-execution (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (let ((transition (%dispatch-plan-execution-transition state :resume)))
    (%apply-plan-execution-transition! state transition)
    state))

(defun abort-plan-execution (&key
                               (state (current-plan-execution-state))
                               reason)
  (check-type state plan-execution-state)
  (let ((transition (%dispatch-plan-execution-transition state :abort :reason reason)))
    (%apply-plan-execution-transition! state transition)
    state))

(defun plan-execution-next-step-index (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (first (plan-execution-state-pending-step-indexes state)))
