(in-package :amoebum)

(defun %make-plan-execution-output-effect (line &key
                                                  step-index
                                                  (phase :execution)
                                                  (severity :info)
                                                  (style :plain)
                                                  recovery-actions)
  (list :kind :output
        :line line
        :step-index step-index
        :phase phase
        :severity severity
        :style style
        :recovery-actions recovery-actions))

(defun %make-plan-execution-status-effect (step-index status)
  (list :kind :status-event
        :step-index step-index
        :status status))

(defun %make-plan-execution-progress-effect ()
  (list :kind :progress-output))

(defun %apply-plan-execution-state-updates! (state updates)
  (check-type state plan-execution-state)
  (loop for (key value) on (or updates '()) by #'cddr
        do (case key
             (:status
              (setf (plan-execution-state-status state) value))
             (:started-at
              (setf (plan-execution-state-started-at state) value))
             (:finished-at
              (setf (plan-execution-state-finished-at state) value))
             (:current-step-index
              (setf (plan-execution-state-current-step-index state) value))
             (:pending-step-indexes
              (setf (plan-execution-state-pending-step-indexes state)
                    (copy-list (or value '()))))
             (:completed-step-indexes
              (setf (plan-execution-state-completed-step-indexes state)
                    (copy-list (or value '()))))
             (:failure-reason
              (setf (plan-execution-state-failure-reason state) value))
             (:abort-reason
              (setf (plan-execution-state-abort-reason state) value))
             (otherwise nil)))
  state)

(defun %apply-plan-execution-step-updates! (state step-updates)
  (check-type state plan-execution-state)
  (dolist (update (or step-updates '()) state)
    (let* ((step-index (getf update :index))
           (step (and (integerp step-index)
                      (%find-plan-execution-step state step-index))))
      (when step
        (setf (plan-execution-step-status step) (getf update :status)
              (plan-execution-step-started-at step) (getf update :started-at)
              (plan-execution-step-finished-at step) (getf update :finished-at))))))

(defun %apply-plan-execution-effect! (state effect)
  (check-type state plan-execution-state)
  (case (getf effect :kind)
    (:output
     (plan-execution-append-output
      (getf effect :line)
      :step-index (getf effect :step-index)
      :phase (or (getf effect :phase) :execution)
      :severity (or (getf effect :severity) :info)
      :style (or (getf effect :style) :plain)
      :recovery-actions (getf effect :recovery-actions)
      :state state))
    (:status-event
     (let* ((step-index (getf effect :step-index))
            (step (and (integerp step-index)
                       (%find-plan-execution-step state step-index))))
       (when step
         (%publish-plan-step-status-event state step :status (getf effect :status)))))
    (:progress-output
     (%append-plan-execution-progress-output state))
    (:initialize
     (let* ((run-id (getf effect :run-id))
            (ps (getf effect :plan-state))
            (steps (getf effect :steps))
            (ordered-indexes (getf effect :ordered-indexes))
            (approved-step-indexes (getf effect :approved-step-indexes)))
       (setf (plan-execution-state-run-id state) run-id
             (plan-execution-state-created-at state) (get-universal-time)
             (plan-execution-state-started-at state) nil
             (plan-execution-state-finished-at state) nil
             (plan-execution-state-source-plan-exited-at state) (plan-mode-state-exited-at ps)
             (plan-execution-state-source-plan-exit-reason state)
             (plan-mode-state-last-exit-reason ps)
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
             (plan-execution-state-continuity-output state) '()
             (plan-execution-state-current-step-index state) nil
             (plan-execution-state-failure-reason state) nil
             (plan-execution-state-abort-reason state) nil
             (plan-execution-state-rollback-baseline-stash state) nil
             (plan-execution-state-rollback-baseline-directory state) nil
             (plan-execution-state-rollback-attempted-p state) nil
             (plan-execution-state-rollback-succeeded-p state) nil
             (plan-execution-state-rollback-notes state) nil)
       (%publish-plan-step-status-snapshot state)
       (prime-plan-execution-continuity state)))
    (:delegate-start
     (let ((transition (%dispatch-plan-execution-transition state :start)))
       (%apply-plan-execution-transition! state transition)))
    (otherwise nil))
  state)

(defun %apply-plan-execution-transition! (state transition)
  (check-type state plan-execution-state)
  (check-type transition plan-execution-transition)
  (%apply-plan-execution-state-updates! state
                                        (plan-execution-transition-state-updates transition))
  (%apply-plan-execution-step-updates! state
                                       (plan-execution-transition-step-updates transition))
  (dolist (effect (plan-execution-transition-effects transition) state)
    (%apply-plan-execution-effect! state effect)))
