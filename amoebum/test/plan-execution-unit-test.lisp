;;;; amoebum/test/plan-execution-unit-test.lisp
;;;;
;;;; NXT-283: Dedicated unit tests for amoebum/src/plan-execution.lisp.
;;;;
;;;; Complements plan-execution-transition-table-test.lisp (which locks
;;;; down the declarative dispatch table) by exercising the step
;;;; lifecycle transitions, the effect shapes returned by pure
;;;; transition handlers, and the behavior of the dispatch helpers on
;;;; invalid / terminal states. Tests are read-only over plan-execution
;;;; internals: they never touch the default `*plan-execution-state*`,
;;;; construct fresh states per test, and do not call `restore-session`.

(in-package :amoebum/test)

(def-suite plan-execution-unit-suite :in amoebum-suite
  :description "Unit tests for amoebum/src/plan-execution.lisp (NXT-283).")

(in-suite plan-execution-unit-suite)

;;; --- Helpers -------------------------------------------------------------

(defun %pe-unit-state (&key (status :idle))
  "Fresh plan-execution-state at STATUS, isolated from the global."
  (amoebum::%make-plan-execution-state :status status))

(defun %pe-unit-step (index &key (description "unit test step")
                                 (status :pending))
  "Construct an execution step directly, approved for execution."
  (amoebum::make-plan-execution-step
   :index index
   :description description
   :approved-p t
   :status status))

(defun %pe-unit-state-with-steps (indexes)
  "Build a :running state whose pending and step list match INDEXES."
  (let* ((steps (mapcar (lambda (i) (%pe-unit-step i)) indexes))
         (state (amoebum::%make-plan-execution-state :status :running)))
    (setf (amoebum::plan-execution-state-steps state) steps
          (amoebum::plan-execution-state-ordered-step-indexes state)
          (copy-list indexes)
          (amoebum::plan-execution-state-approved-step-indexes state)
          (copy-list indexes)
          (amoebum::plan-execution-state-pending-step-indexes state)
          (copy-list indexes))
    state))

;;; --- Terminal status detection ------------------------------------------

(test pe-unit-terminal-status-detects-completed-failed-aborted
  "`%plan-execution-terminal-status-p` returns T only for terminal keywords."
  (is-true (amoebum::%plan-execution-terminal-status-p :completed))
  (is-true (amoebum::%plan-execution-terminal-status-p :failed))
  (is-true (amoebum::%plan-execution-terminal-status-p :aborted))
  (is-false (amoebum::%plan-execution-terminal-status-p :idle))
  (is-false (amoebum::%plan-execution-terminal-status-p :ready))
  (is-false (amoebum::%plan-execution-terminal-status-p :running))
  (is-false (amoebum::%plan-execution-terminal-status-p :paused)))

(test pe-unit-normalize-status-rejects-unknown-keyword
  "Unknown keyword values normalize to :idle."
  (is (eq :idle (amoebum::%normalize-plan-execution-status :bogus)))
  (is (eq :idle (amoebum::%normalize-plan-execution-status nil)))
  (is (eq :running (amoebum::%normalize-plan-execution-status :running)))
  (is (eq :completed (amoebum::%normalize-plan-execution-status "completed"))))

;;; --- step-running transition --------------------------------------------

(test pe-unit-step-running-transition-shape
  "`%plan-execution-transition-step-running` produces the expected effect bundle."
  (let* ((state (%pe-unit-state-with-steps '(1 2)))
         (step (amoebum::%find-plan-execution-step state 1))
         (now 9999)
         (transition (amoebum::%plan-execution-transition-step-running
                      state :step step :now now))
         (state-updates (amoebum::plan-execution-transition-state-updates transition))
         (step-updates (amoebum::plan-execution-transition-step-updates transition))
         (effects (amoebum::plan-execution-transition-effects transition)))
    (is (amoebum::plan-execution-transition-p transition))
    ;; Current step index is set to running step.
    (is (= 1 (getf state-updates :current-step-index)))
    ;; Exactly one step update whose status is :running and started-at == now.
    (is (= 1 (length step-updates)))
    (let ((update (first step-updates)))
      (is (= 1 (getf update :index)))
      (is (eq :running (getf update :status)))
      (is (= now (getf update :started-at)))
      (is (null (getf update :finished-at))))
    ;; Effects include a :status-event for step 1 and at least one :output.
    (is (find-if (lambda (e)
                   (and (eq :status-event (getf e :kind))
                        (eql 1 (getf e :step-index))
                        (eq :running (getf e :status))))
                 effects))
    (is (find-if (lambda (e) (eq :output (getf e :kind))) effects))
    (is (find-if (lambda (e) (eq :progress-output (getf e :kind))) effects))
    ;; step-running is not terminal.
    (is-false (amoebum::plan-execution-transition-done-p transition))))

;;; --- step-success transitions -------------------------------------------

(test pe-unit-step-success-single-step-marks-done
  "When the only pending step succeeds, transition is done-p and status becomes :completed."
  (let* ((state (%pe-unit-state-with-steps '(1)))
         (step (amoebum::%find-plan-execution-step state 1))
         (now 1234)
         (transition (amoebum::%plan-execution-transition-step-success
                      state :step step :result :ok :now now))
         (updates (amoebum::plan-execution-transition-state-updates transition)))
    (is-true (amoebum::plan-execution-transition-done-p transition))
    (is (eq :completed (getf updates :status)))
    (is (= now (getf updates :finished-at)))
    (is (null (getf updates :pending-step-indexes)))
    (is (equal '(1) (getf updates :completed-step-indexes)))
    (is (null (getf updates :current-step-index)))
    (is (eq :ok (amoebum::plan-execution-transition-result transition)))))

(test pe-unit-step-success-multi-step-advances-pending
  "When more steps remain, transition is NOT done-p and status is not changed."
  (let* ((state (%pe-unit-state-with-steps '(1 2 3)))
         (step (amoebum::%find-plan-execution-step state 1))
         (transition (amoebum::%plan-execution-transition-step-success
                      state :step step :result :ok :now 2000))
         (updates (amoebum::plan-execution-transition-state-updates transition)))
    (is-false (amoebum::plan-execution-transition-done-p transition))
    ;; Status should NOT be set in updates plist (remains :running).
    (is (null (getf updates :status)))
    (is (equal '(2 3) (getf updates :pending-step-indexes)))
    (is (equal '(1) (getf updates :completed-step-indexes)))
    (is (null (getf updates :current-step-index)))))

;;; --- step-failure transition --------------------------------------------

(test pe-unit-step-failure-marks-state-failed
  "step-failure marks the run as failed with a failure-reason and done-p t."
  (let* ((state (%pe-unit-state-with-steps '(1 2)))
         (step (amoebum::%find-plan-execution-step state 1))
         (condition (make-condition 'simple-error
                                    :format-control "boom"
                                    :format-arguments nil))
         (transition (amoebum::%plan-execution-transition-step-failure
                      state :step step :condition condition :now 4242))
         (updates (amoebum::plan-execution-transition-state-updates transition))
         (step-updates (amoebum::plan-execution-transition-step-updates transition))
         (effects (amoebum::plan-execution-transition-effects transition)))
    (is-true (amoebum::plan-execution-transition-done-p transition))
    (is (eq :failed (getf updates :status)))
    (is (eq condition (getf updates :failure-reason)))
    (is (= 4242 (getf updates :finished-at)))
    (is (null (getf updates :current-step-index)))
    ;; Failing step is marked :blocked in step-updates.
    (is (= 1 (length step-updates)))
    (is (eq :blocked (getf (first step-updates) :status)))
    ;; Error-severity output effect is present.
    (is (find-if (lambda (e)
                   (and (eq :output (getf e :kind))
                        (eq :error (getf e :severity))))
                 effects))))

;;; --- dispatch helpers ---------------------------------------------------

(test pe-unit-dispatch-rejects-unknown-event
  "Dispatching an event not present in the table signals an error."
  (let ((state (%pe-unit-state :status :ready)))
    (signals error
      (amoebum::%dispatch-plan-execution-transition state :no-such-event))))

(test pe-unit-valid-transition-p-rejects-unknown-event
  "`%plan-execution-valid-transition-p` returns NIL for unknown events."
  (is-false (amoebum::%plan-execution-valid-transition-p :ready :no-such-event))
  (is-false (amoebum::%plan-execution-valid-transition-p :running :initialize))
  (is-false (amoebum::%plan-execution-valid-transition-p :completed :pause)))

;;; --- apply helpers integrate state and step updates --------------------

(test pe-unit-apply-transition-writes-state-and-step
  "`%apply-plan-execution-transition!` mutates both state slots and step slots."
  (let* ((state (%pe-unit-state-with-steps '(5)))
         (step (amoebum::%find-plan-execution-step state 5))
         (transition (amoebum::%plan-execution-transition-step-running
                      state :step step :now 7777)))
    (amoebum::%apply-plan-execution-transition! state transition)
    ;; State's current-step-index now reflects the running step.
    (is (= 5 (amoebum::plan-execution-state-current-step-index state)))
    ;; Step's status moved to :running with started-at from the transition.
    (let ((refreshed (amoebum::%find-plan-execution-step state 5)))
      (is (eq :running (amoebum::plan-execution-step-status refreshed)))
      (is (= 7777 (amoebum::plan-execution-step-started-at refreshed))))
    ;; At least one continuity-output entry was appended by the effect.
    (is-true (plusp (length (amoebum::plan-execution-state-continuity-output state))))))

;;; --- abort effect content ----------------------------------------------

(test pe-unit-abort-effect-includes-reason-text
  "Abort transition emits a warning-styled output containing the reason."
  (let* ((state (%pe-unit-state :status :running))
         (transition (amoebum::%pe-transition-abort state :reason "canary"))
         (effects (amoebum::plan-execution-transition-effects transition))
         (output (find-if (lambda (e) (eq :output (getf e :kind))) effects)))
    (is-true output)
    (is (eq :warning (getf output :style)))
    (is-true (search "canary" (getf output :line)))))

;;; --- initialize transition error path ---------------------------------

(test pe-unit-initialize-without-steps-signals-error
  "`%pe-transition-initialize` errors when the plan-mode-state has no steps."
  (let ((state (%pe-unit-state :status :idle))
        (plan-state (amoebum::%make-plan-mode-state :steps '()
                                                     :approved-step-indexes '())))
    (signals error
      (amoebum::%pe-transition-initialize state :plan-state plan-state))))

;;; --- execute-next-approved-plan-step guard rails ----------------------

(test pe-unit-execute-next-rejects-terminal-state
  "`execute-next-approved-plan-step` refuses to run from a terminal status."
  (let ((state (%pe-unit-state :status :completed)))
    (signals error
      (amoebum::execute-next-approved-plan-step (lambda (step)
                                                  (declare (ignore step))
                                                  :noop)
                                                :state state))))

(test pe-unit-execute-next-rejects-idle-state
  "`execute-next-approved-plan-step` refuses to run from :idle (not initialized)."
  (let ((state (%pe-unit-state :status :idle)))
    (signals error
      (amoebum::execute-next-approved-plan-step (lambda (step)
                                                  (declare (ignore step))
                                                  :noop)
                                                :state state))))
