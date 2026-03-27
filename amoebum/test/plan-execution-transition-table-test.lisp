(in-package :amoebum/test)

;;; ============================================================
;;; Plan-execution transition table tests (FP-Refine Phase 1)
;;; ============================================================

(def-suite plan-execution-transition-table-suite :in amoebum-suite
  :description "Plan-execution declarative transition table tests.")

(in-suite plan-execution-transition-table-suite)

;;; --- Helper: make a minimal plan-execution-state at a given status ---

(defun %pe-test-state (&key (status :idle))
  (let ((state (amoebum::%make-plan-execution-state :status status)))
    state))

;;; --- Transition table structure ---

(test pe-transition-table-has-19-entries
  (is (= 19 (length amoebum::+plan-execution-transition-table+))))

;;; --- Valid transitions ---

(test pe-idle-initialize-valid
  (is (amoebum::%plan-execution-valid-transition-p :idle :initialize)))

(test pe-ready-start-valid
  (is (amoebum::%plan-execution-valid-transition-p :ready :start)))

(test pe-paused-start-valid
  (is (amoebum::%plan-execution-valid-transition-p :paused :start)))

(test pe-running-pause-valid
  (is (amoebum::%plan-execution-valid-transition-p :running :pause)))

(test pe-paused-resume-valid
  (is (amoebum::%plan-execution-valid-transition-p :paused :resume)))

(test pe-running-step-running-valid
  (is (amoebum::%plan-execution-valid-transition-p :running :step-running)))

(test pe-running-step-success-valid
  (is (amoebum::%plan-execution-valid-transition-p :running :step-success)))

(test pe-running-step-failure-valid
  (is (amoebum::%plan-execution-valid-transition-p :running :step-failure)))

;; Abort from non-terminal states
(test pe-idle-abort-valid
  (is (amoebum::%plan-execution-valid-transition-p :idle :abort)))

(test pe-ready-abort-valid
  (is (amoebum::%plan-execution-valid-transition-p :ready :abort)))

(test pe-running-abort-valid
  (is (amoebum::%plan-execution-valid-transition-p :running :abort)))

(test pe-paused-abort-valid
  (is (amoebum::%plan-execution-valid-transition-p :paused :abort)))

;; Reset from all states
(test pe-idle-reset-valid
  (is (amoebum::%plan-execution-valid-transition-p :idle :reset)))

(test pe-ready-reset-valid
  (is (amoebum::%plan-execution-valid-transition-p :ready :reset)))

(test pe-running-reset-valid
  (is (amoebum::%plan-execution-valid-transition-p :running :reset)))

(test pe-paused-reset-valid
  (is (amoebum::%plan-execution-valid-transition-p :paused :reset)))

(test pe-completed-reset-valid
  (is (amoebum::%plan-execution-valid-transition-p :completed :reset)))

(test pe-failed-reset-valid
  (is (amoebum::%plan-execution-valid-transition-p :failed :reset)))

(test pe-aborted-reset-valid
  (is (amoebum::%plan-execution-valid-transition-p :aborted :reset)))

;;; --- Invalid transitions ---

(test pe-completed-start-invalid
  (is (not (amoebum::%plan-execution-valid-transition-p :completed :start))))

(test pe-failed-start-invalid
  (is (not (amoebum::%plan-execution-valid-transition-p :failed :start))))

(test pe-aborted-start-invalid
  (is (not (amoebum::%plan-execution-valid-transition-p :aborted :start))))

(test pe-idle-start-invalid
  (is (not (amoebum::%plan-execution-valid-transition-p :idle :start))))

(test pe-idle-pause-invalid
  (is (not (amoebum::%plan-execution-valid-transition-p :idle :pause))))

(test pe-completed-abort-invalid
  (is (not (amoebum::%plan-execution-valid-transition-p :completed :abort))))

(test pe-failed-abort-invalid
  (is (not (amoebum::%plan-execution-valid-transition-p :failed :abort))))

(test pe-aborted-abort-invalid
  (is (not (amoebum::%plan-execution-valid-transition-p :aborted :abort))))

;;; --- Pure transition functions ---

(test pe-transition-reset-returns-idle
  (let* ((state (%pe-test-state :status :running))
         (transition (amoebum::%pe-transition-reset state)))
    (is (amoebum::plan-execution-transition-p transition))
    (is (eq :idle (getf (amoebum::plan-execution-transition-state-updates transition) :status)))
    (is (null (getf (amoebum::plan-execution-transition-state-updates transition) :started-at)))
    (is (null (getf (amoebum::plan-execution-transition-state-updates transition) :finished-at)))))

(test pe-transition-start-returns-running
  (let* ((state (%pe-test-state :status :ready))
         (transition (amoebum::%pe-transition-start state)))
    (is (eq :running (getf (amoebum::plan-execution-transition-state-updates transition) :status)))
    ;; started-at set because state has no previous started-at
    (is (integerp (getf (amoebum::plan-execution-transition-state-updates transition) :started-at)))))

(test pe-transition-start-preserves-started-at
  (let* ((state (%pe-test-state :status :paused)))
    (setf (amoebum::plan-execution-state-started-at state) 12345)
    (let ((transition (amoebum::%pe-transition-start state)))
      ;; started-at not in updates (preserved from previous)
      (let ((updates (amoebum::plan-execution-transition-state-updates transition)))
        (is (eq :running (getf updates :status)))
        ;; Should NOT override existing started-at
        (is (not (getf updates :started-at)))))))

(test pe-transition-pause-returns-paused
  (let* ((state (%pe-test-state :status :running))
         (transition (amoebum::%pe-transition-pause state)))
    (is (eq :paused (getf (amoebum::plan-execution-transition-state-updates transition) :status)))))

(test pe-transition-abort-returns-aborted
  (let* ((state (%pe-test-state :status :running))
         (transition (amoebum::%pe-transition-abort state :reason "user cancelled")))
    (let ((updates (amoebum::plan-execution-transition-state-updates transition)))
      (is (eq :aborted (getf updates :status)))
      (is (string= "user cancelled" (getf updates :abort-reason)))
      (is (integerp (getf updates :finished-at))))))

;;; --- Dispatch function ---

(test dispatch-ready-start
  (let* ((state (%pe-test-state :status :ready))
         (transition (amoebum::%dispatch-plan-execution-transition state :start)))
    (is (amoebum::plan-execution-transition-p transition))
    (is (eq :running (getf (amoebum::plan-execution-transition-state-updates transition) :status)))))

(test dispatch-invalid-signals-error
  (let ((state (%pe-test-state :status :completed)))
    (signals error
      (amoebum::%dispatch-plan-execution-transition state :start))))

(test dispatch-running-pause
  (let* ((state (%pe-test-state :status :running))
         (transition (amoebum::%dispatch-plan-execution-transition state :pause)))
    (is (eq :paused (getf (amoebum::plan-execution-transition-state-updates transition) :status)))))

(test dispatch-running-abort
  (let* ((state (%pe-test-state :status :running))
         (transition (amoebum::%dispatch-plan-execution-transition state :abort
                       :reason "emergency")))
    (is (eq :aborted (getf (amoebum::plan-execution-transition-state-updates transition) :status)))))

;;; --- Full lifecycle: reset → ready → running → paused → running → completed ---

(test pe-full-lifecycle-via-transitions
  (let ((state (%pe-test-state :status :idle)))
    ;; Reset from idle (no-op on status but valid)
    (amoebum::%apply-plan-execution-transition!
     state (amoebum::%dispatch-plan-execution-transition state :reset))
    (is (eq :idle (amoebum::plan-execution-state-status state)))
    ;; Manually set ready (initialize requires plan-mode-state)
    (setf (amoebum::plan-execution-state-status state) :ready)
    ;; Start
    (amoebum::%apply-plan-execution-transition!
     state (amoebum::%dispatch-plan-execution-transition state :start))
    (is (eq :running (amoebum::plan-execution-state-status state)))
    ;; Pause
    (amoebum::%apply-plan-execution-transition!
     state (amoebum::%dispatch-plan-execution-transition state :pause))
    (is (eq :paused (amoebum::plan-execution-state-status state)))
    ;; Start again (from paused)
    (amoebum::%apply-plan-execution-transition!
     state (amoebum::%dispatch-plan-execution-transition state :start))
    (is (eq :running (amoebum::plan-execution-state-status state)))
    ;; Abort
    (amoebum::%apply-plan-execution-transition!
     state (amoebum::%dispatch-plan-execution-transition state :abort :reason "test abort"))
    (is (eq :aborted (amoebum::plan-execution-state-status state)))
    (is (string= "test abort" (amoebum::plan-execution-state-abort-reason state)))
    ;; Reset from aborted
    (amoebum::%apply-plan-execution-transition!
     state (amoebum::%dispatch-plan-execution-transition state :reset))
    (is (eq :idle (amoebum::plan-execution-state-status state)))))

;;; --- Abort from various states ---

(test pe-abort-from-idle
  (let ((state (%pe-test-state :status :idle)))
    (amoebum::%apply-plan-execution-transition!
     state (amoebum::%dispatch-plan-execution-transition state :abort :reason "test"))
    (is (eq :aborted (amoebum::plan-execution-state-status state)))))

(test pe-abort-from-ready
  (let ((state (%pe-test-state :status :ready)))
    (amoebum::%apply-plan-execution-transition!
     state (amoebum::%dispatch-plan-execution-transition state :abort :reason "test"))
    (is (eq :aborted (amoebum::plan-execution-state-status state)))))

(test pe-abort-from-paused
  (let ((state (%pe-test-state :status :paused)))
    (amoebum::%apply-plan-execution-transition!
     state (amoebum::%dispatch-plan-execution-transition state :abort :reason "test"))
    (is (eq :aborted (amoebum::plan-execution-state-status state)))))

;;; --- Reset from all terminal states ---

(test pe-reset-from-completed
  (let ((state (%pe-test-state :status :completed)))
    (amoebum::%apply-plan-execution-transition!
     state (amoebum::%dispatch-plan-execution-transition state :reset))
    (is (eq :idle (amoebum::plan-execution-state-status state)))))

(test pe-reset-from-failed
  (let ((state (%pe-test-state :status :failed)))
    (amoebum::%apply-plan-execution-transition!
     state (amoebum::%dispatch-plan-execution-transition state :reset))
    (is (eq :idle (amoebum::plan-execution-state-status state)))))
