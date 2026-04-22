;;;; amoebum/src/plan-execution-loop.lisp
;;;;
;;;; NXT-415: Restart-preserving execution loop extracted from
;;;; amoebum/src/plan-execution.lisp.
;;;;
;;;; Owns the agentic-loop entry points and supporting machinery:
;;;;   - User-approval coordination (lock + condvar + flags)
;;;;   - plan-execution-context lifecycle helpers
;;;;   - execute-next-approved-plan-step  (single-step primitive)
;;;;   - execute-approved-plan-steps      (driving loop with rollback)
;;;;
;;;; PUBLIC CONTRACT (UNCHANGED — load-bearing for the chat pipeline):
;;;;
;;;;   (execute-approved-plan-steps EXECUTOR
;;;;     &key state rollback-on-failure-p signal-failure-p
;;;;          interactive-p rollback-directory)
;;;;     => (values state results failure-condition rollback-succeeded-p)
;;;;
;;;; The function preserves restart semantics so that conditions raised
;;;; mid-step propagate to callers (chat pipeline, condition-to-llm-context)
;;;; with the same restart names available as before NXT-415.

(in-package :amoebum)

;;; --- User-approval coordination primitives -------------------------

(defvar *plan-step-approval-lock* (bt:make-lock "plan-step-approval-lock"))
(defvar *plan-step-approval-condvar* (bt:make-condition-variable :name "plan-step-approval-cv"))
(defvar *plan-step-awaiting-approval-p* nil
  "T when interactive plan execution is paused waiting for user approval of the next step.")
(defvar *plan-step-approved-p* nil
  "Set to T by the TUI when the user approves the next step.")

(defun approve-next-plan-step ()
  "Called by the TUI to approve the next step in interactive plan execution."
  (bt:with-lock-held (*plan-step-approval-lock*)
    (setf *plan-step-approved-p* t)
    (bt:condition-notify *plan-step-approval-condvar*)))

(defun plan-step-awaiting-approval-p ()
  "Returns T if interactive plan execution is paused for user approval."
  *plan-step-awaiting-approval-p*)

(defun %wait-for-plan-step-approval (state step-index)
  "Block until the user approves the next step."
  (check-type state plan-execution-state)
  (bt:with-lock-held (*plan-step-approval-lock*)
    (setf (plan-execution-state-awaiting-approval-step-index state) step-index
          *plan-step-awaiting-approval-p* t
          *plan-step-approved-p* nil)
    (loop until *plan-step-approved-p*
          do (bt:condition-wait *plan-step-approval-condvar*
                                *plan-step-approval-lock*))
    (setf (plan-execution-state-awaiting-approval-step-index state) nil
          *plan-step-awaiting-approval-p* nil
          *plan-step-approved-p* nil)))

;;; --- plan-execution-context lifecycle helpers ----------------------

(defun %plan-execution-context-state (context)
  (check-type context plan-execution-context)
  (or (plan-execution-context-state context)
      (error "Plan execution context is missing state.")))

(defun %plan-execution-context-executor (context)
  (check-type context plan-execution-context)
  (let ((executor (plan-execution-context-executor context)))
    (unless (functionp executor)
      (error "Plan execution executor must be a function."))
    executor))

(defun %prepare-plan-execution-context-rollback (context)
  (check-type context plan-execution-context)
  (let ((state (%plan-execution-context-state context)))
    (when (plan-execution-context-rollback-on-failure-p context)
      (if (%prepare-plan-execution-rollback-baseline
           state
           (plan-execution-context-rollback-directory context))
          (plan-execution-append-output
           "LIVE> Rollback baseline captured via git."
           :phase :system
           :style :meta
           :state state)
          (plan-execution-append-output
           (format nil "LIVE> Rollback baseline unavailable; proceeding without rollback (~A)."
                   (%safe-plan-execution-string
                    (plan-execution-state-rollback-notes state)
                    "no baseline"))
           :phase :system
           :severity :warning
           :style :warning
           :state state))))
  context)

(defun %plan-execution-context-await-step-approval (context first-step-p)
  (check-type context plan-execution-context)
  (unless (or (not (plan-execution-context-interactive-p context))
              first-step-p)
    (let* ((state (%plan-execution-context-state context))
           (next-idx (plan-execution-next-step-index state)))
      (when next-idx
        (plan-execution-append-output
         (format nil "LIVE> [step ~D] Waiting for approval... (press Enter in TUI)"
                 next-idx)
         :step-index next-idx
         :phase :execution
         :style :meta
         :state state)
        (%wait-for-plan-step-approval state next-idx)
        (plan-execution-append-output
         (format nil "LIVE> [step ~D] Approval received; resuming execution."
                 next-idx)
         :step-index next-idx
         :phase :execution
         :style :meta
         :state state)))))

(defun %record-plan-execution-result (context step result)
  (check-type context plan-execution-context)
  (when step
    (push (cons (plan-execution-step-index step) result)
          (plan-execution-context-execution-results context)))
  context)

(defun %finish-plan-execution-success (context result done-p)
  (check-type context plan-execution-context)
  (let ((state (%plan-execution-context-state context)))
    (when done-p
      (when (typep result 'error)
        (error result))
      (%drop-plan-execution-rollback-baseline state)
      (return-from %finish-plan-execution-success
        (values state
                (nreverse (plan-execution-context-execution-results context))
                nil
                nil))))
  nil)

;;; --- Single-step primitive ----------------------------------------

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
        (%apply-plan-execution-transition!
         state
         (%evaluate-plan-execution-transition state
                                             :step-running
                                             :step step
                                             :now (get-universal-time)))
        (handler-case
            (let ((result (funcall executor step)))
              (let ((transition
                      (%evaluate-plan-execution-transition state
                                                          :step-success
                                                          :step step
                                                          :result result
                                                          :now (get-universal-time))))
                (%apply-plan-execution-transition! state transition)
                (values state
                        step
                        result
                        (plan-execution-transition-done-p transition))))
          (error (condition)
            (%apply-plan-execution-transition!
             state
             (%evaluate-plan-execution-transition state
                                                 :step-failure
                                                 :step step
                                                 :condition condition
                                                 :now (get-universal-time)))
            (values state step condition t)))))))

;;; --- Driving loop + failure handler -------------------------------

(defun %execute-approved-plan-steps-loop (context)
  (check-type context plan-execution-context)
  (let ((executor (%plan-execution-context-executor context))
        (state (%plan-execution-context-state context)))
    (loop
      with first-step-p = t
      do (%plan-execution-context-await-step-approval context first-step-p)
         (setf first-step-p nil)
         (multiple-value-bind (_ step result done-p)
             (execute-next-approved-plan-step executor :state state)
           (declare (ignore _))
           (%record-plan-execution-result context step result)
           (multiple-value-bind (completed-state results failure rollback)
               (%finish-plan-execution-success context result done-p)
             (when completed-state
               (return (values completed-state results failure rollback))))))))

(defun %handle-plan-execution-failure (context condition)
  (check-type context plan-execution-context)
  (let* ((state (%plan-execution-context-state context))
         (rollback-succeeded-p nil))
    (setf (plan-execution-context-failure-condition context) condition
          (plan-execution-state-status state) :failed
          (plan-execution-state-failure-reason state) (princ-to-string condition)
          (plan-execution-state-finished-at state) (get-universal-time))
    (plan-execution-append-output
     (format nil "LIVE> Execution failed: ~A"
             (%safe-plan-execution-string condition "unknown error"))
     :phase :execution
     :severity :error
     :style :error
     :step-index (plan-execution-state-current-step-index state)
     :state state)
    (when (and (plan-execution-context-rollback-on-failure-p context)
               (or (plan-execution-state-rollback-baseline-stash state)
                   (plan-execution-state-rollback-baseline-directory state)))
      (plan-execution-append-output
       "LIVE> Failure detected; attempting git rollback."
       :phase :system
       :severity :warning
       :style :warning
       :state state)
      (setf rollback-succeeded-p (%rollback-plan-execution-via-git state))
      (plan-execution-append-output
       (if rollback-succeeded-p
           "LIVE> Rollback completed; git baseline restored."
           (format nil "LIVE> Rollback failed: ~A"
                   (%safe-plan-execution-string
                    (plan-execution-state-rollback-notes state)
                    "unknown rollback failure")))
       :phase :system
       :severity (if rollback-succeeded-p :info :error)
       :style (if rollback-succeeded-p :success :error)
       :state state))
    (if (plan-execution-context-signal-failure-p context)
        (error condition)
        (values state
                (nreverse (plan-execution-context-execution-results context))
                (plan-execution-context-failure-condition context)
                rollback-succeeded-p))))

(defun execute-approved-plan-steps (executor &key
                                             (state (current-plan-execution-state))
                                             (rollback-on-failure-p t)
                                             (signal-failure-p t)
                                             (interactive-p nil)
                                             rollback-directory)
  (check-type state plan-execution-state)
  (let ((context (make-plan-execution-context
                  :executor executor
                  :state state
                  :rollback-on-failure-p rollback-on-failure-p
                  :signal-failure-p signal-failure-p
                  :interactive-p interactive-p
                  :rollback-directory rollback-directory)))
    (%plan-execution-context-executor context)
    (%prepare-plan-execution-context-rollback context)
    (handler-case
        (%execute-approved-plan-steps-loop context)
      (error (condition)
        (%handle-plan-execution-failure context condition)))))
