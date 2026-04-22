;;;; amoebum/src/plan-execution-state-machine.lisp
;;;;
;;;; NXT-415: Declarative state machine extracted from
;;;; amoebum/src/plan-execution.lisp.
;;;;
;;;; Owns the entire (status, event) -> transition surface:
;;;;   - Step lifecycle transitions (running, success, failure)
;;;;   - Control transitions (reset, initialize, start, pause, resume, abort)
;;;;   - Declarative +plan-execution-transition-table+
;;;;   - %dispatch-plan-execution-transition / %plan-execution-valid-transition-p
;;;;   - Effect type registry + transition-event publication
;;;;   - %evaluate-plan-execution-transition (table-first dispatch with the
;;;;     legacy *plan-execution-transition-handlers* fallback)
;;;;
;;;; Restart preservation: the public lifecycle entry points
;;;; (start/pause/resume/abort/reset) and dispatch helpers are kept here
;;;; so that callers raising conditions mid-step can recover and resume
;;;; the existing plan-execution-state via the same restart names that
;;;; landed in NXT-363.

(in-package :amoebum)

;;; --- Step-lifecycle transitions ---------------------------------------

(defun %plan-execution-transition-step-running (state &key step now)
  (check-type state plan-execution-state)
  (check-type step plan-execution-step)
  (let* ((step-index (plan-execution-step-index step))
         (ts (get-universal-time))
         (decision-context
           (%build-plan-transition-decision-context state :step-running step))
         (context-plist (policy-decision-context-plist decision-context)))
    (make-plan-execution-transition
     :state-updates (list :current-step-index step-index)
     :step-updates (list (list :index step-index
                               :status :running
                               :started-at now
                               :finished-at nil))
     :effects (list (%make-plan-execution-status-effect step-index :running)
                    (%make-plan-execution-output-effect
                     (format nil "LIVE> [step ~D running] ~A"
                             step-index
                             (%safe-plan-execution-string
                              (plan-execution-step-description step)
                              "Executing approved step."))
                     :step-index step-index
                     :phase :execution
                     :style :meta)
                    (%make-plan-execution-progress-effect))
     :decision-context decision-context
     :structured-trace
     (list (make-policy-trace-entry
            :phase :input
            :source :plan-execution
            :data (list :step-index step-index
                        :event :step-running
                        :decision-context context-plist)
            :timestamp ts)
           (make-policy-trace-entry
            :phase :evaluate
            :source :step-transition
            :decision :running
            :data (list :step-index step-index
                        :decision-context context-plist)
            :timestamp ts)))))

(defun %plan-execution-transition-step-success (state &key step result now)
  (check-type state plan-execution-state)
  (check-type step plan-execution-step)
  (let* ((step-index (plan-execution-step-index step))
         (pending (copy-list (or (plan-execution-state-pending-step-indexes state) '())))
         (remaining (if (and pending (eql (first pending) step-index))
                        (rest pending)
                        (remove step-index pending :test #'= :count 1)))
         (completed (append (copy-list (or (plan-execution-state-completed-step-indexes state) '()))
                            (list step-index)))
         (done-p (null remaining))
         (state-updates (list :pending-step-indexes remaining
                              :completed-step-indexes completed
                              :current-step-index nil))
         (effects (list (%make-plan-execution-status-effect step-index :done)
                        (%make-plan-execution-output-effect
                         (format nil "LIVE> [step ~D done] ~A"
                                 step-index
                                 (%summarize-execution-result result))
                         :step-index step-index
                         :phase :execution
                         :style :success)))
         (ts (get-universal-time))
         (decision-context
           (%build-plan-transition-decision-context state :step-success step
                                                    :result result))
         (context-plist (policy-decision-context-plist decision-context)))
    (when done-p
      (setf state-updates (append state-updates
                                  (list :status :completed
                                        :finished-at now))
            effects (append effects
                            (list (%make-plan-execution-output-effect
                                   "LIVE> All approved steps completed."
                                   :phase :execution
                                   :style :success)))))
    (setf effects (append effects
                          (list (%make-plan-execution-progress-effect))))
    (make-plan-execution-transition
     :state-updates state-updates
     :step-updates (list (list :index step-index
                               :status :completed
                               :started-at (plan-execution-step-started-at step)
                               :finished-at now))
     :effects effects
     :done-p done-p
     :result result
     :decision-context decision-context
     :structured-trace
     (list (make-policy-trace-entry
            :phase :input
            :source :plan-execution
            :data (list :step-index step-index
                        :event :step-success
                        :decision-context context-plist)
            :timestamp ts)
           (make-policy-trace-entry
            :phase :evaluate
            :source :step-transition
            :decision :completed
            :data (list :step-index step-index
                        :done-p done-p
                        :decision-context context-plist)
            :timestamp ts)))))

(defun %plan-execution-transition-step-failure (state &key step condition now)
  (check-type state plan-execution-state)
  (check-type step plan-execution-step)
  (let* ((step-index (plan-execution-step-index step))
         (ts (get-universal-time))
         (decision-context
           (%build-plan-transition-decision-context state :step-failure step
                                                    :condition condition))
         (context-plist (policy-decision-context-plist decision-context)))
    (make-plan-execution-transition
     :state-updates (list :status :failed
                          :failure-reason condition
                          :current-step-index nil
                          :finished-at now)
     :step-updates (list (list :index step-index
                               :status :blocked
                               :started-at (or (plan-execution-step-started-at step) now)
                               :finished-at now))
     :effects (list (%make-plan-execution-status-effect step-index :blocked)
                    (%make-plan-execution-output-effect
                     (format nil
                             "LIVE> [step ~D failed] ~A. Choose next action: /execute (retry), /plan review, or /plan modify."
                             step-index
                             (%safe-plan-execution-string condition "step execution failed"))
                     :step-index step-index
                     :phase :execution
                     :severity :error
                     :style :error)
                    (%make-plan-execution-progress-effect))
     :done-p t
     :condition condition
     :decision-context decision-context
     :structured-trace
     (list (make-policy-trace-entry
            :phase :input
            :source :plan-execution
            :data (list :step-index step-index
                        :event :step-failure
                        :decision-context context-plist)
            :timestamp ts)
           (make-policy-trace-entry
            :phase :evaluate
            :source :step-transition
            :decision :failed
            :reason (when condition
                      (handler-case (princ-to-string condition)
                        (error () "step execution failed")))
            :data (list :step-index step-index
                        :decision-context context-plist)
            :timestamp ts)))))

;;; --- Control transitions (FP-Refine Phase 1, Target 1) ---------------

(defun %pe-transition-reset (state &key &allow-other-keys)
  "Pure transition: reset all slots to idle defaults."
  (declare (ignore state))
  (make-plan-execution-transition
   :state-updates (list :status :idle
                        :started-at nil
                        :finished-at nil
                        :current-step-index nil
                        :failure-reason nil
                        :abort-reason nil)))

(defun %pe-transition-initialize (state &key plan-state run-id &allow-other-keys)
  "Pure transition: initialize from idle to ready with plan steps."
  (declare (ignore state))
  (let* ((ps (or plan-state (current-plan-mode-state)))
         (steps (copy-list (or (plan-mode-state-steps ps) '())))
         (ordered-indexes (%plan-step-order steps))
         (approved-step-indexes (%approved-step-indexes-for-execution ps ordered-indexes)))
    (unless ordered-indexes
      (error "No plan steps are available for execution."))
    (unless approved-step-indexes
      (error "No approved plan steps are available for execution."))
    (make-plan-execution-transition
     :state-updates (list :status :ready)
     :effects (list (list :kind :initialize
                          :run-id (or run-id (%next-plan-execution-run-id))
                          :plan-state ps
                          :steps steps
                          :ordered-indexes ordered-indexes
                          :approved-step-indexes approved-step-indexes)))))

(defun %pe-transition-start (state &key &allow-other-keys)
  "Pure transition: move to running, set started-at if first start."
  (make-plan-execution-transition
   :state-updates (append (list :status :running
                                :finished-at nil)
                          (unless (plan-execution-state-started-at state)
                            (list :started-at (get-universal-time))))
   :effects (list (%make-plan-execution-output-effect
                   "LIVE> Execution run started."
                   :phase :execution
                   :style :meta)
                  (%make-plan-execution-progress-effect))))

(defun %pe-transition-pause (state &key &allow-other-keys)
  "Pure transition: move to paused."
  (declare (ignore state))
  (make-plan-execution-transition
   :state-updates (list :status :paused)
   :effects (list (%make-plan-execution-output-effect
                   "LIVE> Execution paused."
                   :phase :execution
                   :style :warning))))

(defun %pe-transition-resume (state &key &allow-other-keys)
  "Pure transition: resume delegates to start."
  (declare (ignore state))
  (make-plan-execution-transition
   :state-updates nil
   :effects (list (%make-plan-execution-output-effect
                   "LIVE> Execution resumed."
                   :phase :execution
                   :style :meta)
                  (list :kind :delegate-start))))

(defun %pe-transition-abort (state &key reason &allow-other-keys)
  "Pure transition: move to aborted."
  (declare (ignore state))
  (make-plan-execution-transition
   :state-updates (list :status :aborted
                        :abort-reason reason
                        :finished-at (get-universal-time))
   :effects (list (%make-plan-execution-output-effect
                   (format nil "LIVE> Execution aborted (~A)."
                           (%safe-plan-execution-string reason "unspecified"))
                   :phase :execution
                   :severity :warning
                   :style :warning))))

;;; --- Declarative transition table -------------------------------------

(defparameter +plan-execution-transition-table+
  '(;; Initialization
    ((:idle :initialize)       . %pe-transition-initialize)
    ;; Execution control
    ((:ready :start)           . %pe-transition-start)
    ((:paused :start)          . %pe-transition-start)
    ((:running :pause)         . %pe-transition-pause)
    ((:paused :resume)         . %pe-transition-resume)
    ;; Step lifecycle (existing handlers)
    ((:running :step-running)  . %plan-execution-transition-step-running)
    ((:running :step-success)  . %plan-execution-transition-step-success)
    ((:running :step-failure)  . %plan-execution-transition-step-failure)
    ;; Abort (from any non-terminal)
    ((:idle :abort)            . %pe-transition-abort)
    ((:ready :abort)           . %pe-transition-abort)
    ((:running :abort)         . %pe-transition-abort)
    ((:paused :abort)          . %pe-transition-abort)
    ;; Reset (from any)
    ((:idle :reset)            . %pe-transition-reset)
    ((:ready :reset)           . %pe-transition-reset)
    ((:running :reset)         . %pe-transition-reset)
    ((:paused :reset)          . %pe-transition-reset)
    ((:completed :reset)       . %pe-transition-reset)
    ((:failed :reset)          . %pe-transition-reset)
    ((:aborted :reset)         . %pe-transition-reset))
  "Declarative plan-execution state machine: ((from-status event) . handler-fn-name).")

(defun %dispatch-plan-execution-transition (state event &rest args)
  "Look up (status, event) in transition table, call handler, return transition."
  (check-type state plan-execution-state)
  (let* ((status (plan-execution-state-status state))
         (key (list status event))
         (entry (assoc key +plan-execution-transition-table+ :test #'equal)))
    (unless entry
      (error 'simple-error
             :format-control "Invalid plan-execution transition from ~S on event ~S."
             :format-arguments (list status event)))
    (apply (symbol-function (cdr entry)) state args)))

(defun %plan-execution-valid-transition-p (from-status event)
  "Return T if (FROM-STATUS EVENT) is a valid transition."
  (not (null (assoc (list from-status event) +plan-execution-transition-table+ :test #'equal))))

(defparameter *plan-execution-transition-handlers*
  (list (cons :step-running #'%plan-execution-transition-step-running)
        (cons :step-success #'%plan-execution-transition-step-success)
        (cons :step-failure #'%plan-execution-transition-step-failure)))

;;; --- NXT-137: Effect type registry ----------------------------------

(defparameter *plan-execution-effect-type-registry*
  (let ((table (make-hash-table :test #'eq)))
    (setf (gethash :output table) '(:description "Append output text" :side-effect-p t)
          (gethash :status table) '(:description "Update step status indicator" :side-effect-p t)
          (gethash :progress table) '(:description "Refresh progress display" :side-effect-p t))
    table)
  "Registry mapping plan-execution effect types to metadata.")

(defun plan-execution-effect-type-registered-p (effect-type)
  "Return T if EFFECT-TYPE is registered."
  (not (null (gethash effect-type *plan-execution-effect-type-registry*))))

(defun plan-execution-effect-type-metadata (effect-type)
  "Return metadata plist for EFFECT-TYPE or NIL."
  (gethash effect-type *plan-execution-effect-type-registry*))

(defun register-plan-execution-effect-type (effect-type &key description (side-effect-p t))
  "Register a new plan-execution effect type."
  (setf (gethash effect-type *plan-execution-effect-type-registry*)
        (list :description description :side-effect-p side-effect-p))
  effect-type)

;;; --- NXT-138: Transition event-bus publication ---------------------

(defun publish-plan-execution-transition-event (transition &key event-bus event-type)
  "Publish TRANSITION to EVENT-BUS as a structured event if both are available."
  (when (and event-bus
             (plan-execution-transition-p transition)
             event-type)
    (let ((payload (list :state-updates (plan-execution-transition-state-updates transition)
                         :step-updates (plan-execution-transition-step-updates transition)
                         :done-p (plan-execution-transition-done-p transition)
                         :decision-context
                         (and (plan-execution-transition-decision-context transition)
                              (policy-decision-context-plist
                               (plan-execution-transition-decision-context transition)))
                         :structured-trace (plan-execution-transition-structured-trace transition))))
      (handler-case
          (publish event-bus event-type :payload payload)
        (error () nil))))
  transition)

(defun %evaluate-plan-execution-transition (state event &rest args &key &allow-other-keys)
  "Evaluate a plan-execution transition. Dispatches through the transition table
for events registered there, falls back to *plan-execution-transition-handlers*."
  (check-type state plan-execution-state)
  (let* ((status (plan-execution-state-status state))
         (table-entry (assoc (list status event) +plan-execution-transition-table+ :test #'equal)))
    (if table-entry
        (apply (symbol-function (cdr table-entry)) state args)
        ;; Fallback for backward compat with any external handler registrations
        (let ((handler (cdr (assoc event *plan-execution-transition-handlers* :test #'eq))))
          (unless handler
            (error "Unknown plan execution transition event ~S." event))
          (apply handler state args)))))
