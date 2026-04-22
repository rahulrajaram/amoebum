(in-package :amoebum/test)

;;; ============================================================
;;; Token-stream transition table tests (FP-Refine Phase 1)
;;; ============================================================

(def-suite token-stream-transition-table-suite :in amoebum-suite
  :description "Token-stream declarative transition table tests.")

(in-suite token-stream-transition-table-suite)

;;; --- Transition table structure ---

(test transition-table-has-10-entries
  (is (= 10 (length amoebum::+token-stream-transitions+))))

;;; --- Valid transitions ---

(test idle-start-is-valid
  (is (amoebum::%token-stream-valid-transition-p :idle :start)))

(test running-complete-is-valid
  (is (amoebum::%token-stream-valid-transition-p :running :complete)))

(test running-cancelled-is-valid
  (is (amoebum::%token-stream-valid-transition-p :running :cancelled)))

(test running-failed-is-valid
  (is (amoebum::%token-stream-valid-transition-p :running :failed)))

(test running-timeout-is-valid
  (is (amoebum::%token-stream-valid-transition-p :running :timeout)))

(test running-force-reset-is-valid
  (is (amoebum::%token-stream-valid-transition-p :running :force-reset)))

(test completed-start-is-valid
  (is (amoebum::%token-stream-valid-transition-p :completed :start)))

(test cancelled-start-is-valid
  (is (amoebum::%token-stream-valid-transition-p :cancelled :start)))

(test failed-start-is-valid
  (is (amoebum::%token-stream-valid-transition-p :failed :start)))

(test error-start-is-valid
  (is (amoebum::%token-stream-valid-transition-p :error :start)))

;;; --- Invalid transitions ---

(test idle-complete-is-invalid
  (is (not (amoebum::%token-stream-valid-transition-p :idle :complete))))

(test completed-complete-is-invalid
  (is (not (amoebum::%token-stream-valid-transition-p :completed :complete))))

(test idle-cancelled-is-invalid
  (is (not (amoebum::%token-stream-valid-transition-p :idle :cancelled))))

(test idle-failed-is-invalid
  (is (not (amoebum::%token-stream-valid-transition-p :idle :failed))))

;;; --- Pure transition functions return plists ---

(test ts-transition-start-returns-running-plist
  (let* ((state (amoebum:make-token-stream-state))
         (updates (amoebum::%ts-transition-start state
                    :target-message-index 5
                    :budget-warning-threshold-percent 80
                    :budget-abort-threshold-percent 70)))
    (is (eq :running (getf updates :status)))
    (is (= 5 (getf updates :target-message-index)))
    (is (= 80 (getf updates :budget-warning-threshold-percent)))
    (is (= 70 (getf updates :budget-abort-threshold-percent)))
    (is (null (getf updates :cancel-requested-p)))
    (is (null (getf updates :aborted-p)))
    ;; Original state untouched
    (is (eq :idle (amoebum::token-stream-state-status state)))))

(test ts-transition-complete-returns-completed-plist
  (let* ((state (amoebum:make-token-stream-state))
         (updates (amoebum::%ts-transition-complete state)))
    (is (eq :completed (getf updates :status)))
    (is (integerp (getf updates :ended-ms)))))

(test ts-transition-cancelled-returns-cancelled-plist
  (let* ((state (amoebum:make-token-stream-state))
         (updates (amoebum::%ts-transition-cancelled state)))
    (is (eq :cancelled (getf updates :status)))))

(test ts-transition-failed-returns-failed-plist
  (let* ((state (amoebum:make-token-stream-state))
         (updates (amoebum::%ts-transition-failed state :error-message "oops")))
    (is (eq :failed (getf updates :status)))
    (is (string= "oops" (getf updates :error-message)))))

(test ts-transition-timeout-returns-error-plist
  (let* ((state (amoebum:make-token-stream-state))
         (updates (amoebum::%ts-transition-timeout state :elapsed 999)))
    (is (eq :error (getf updates :status)))
    (is (search "999" (getf updates :error-message)))))

(test ts-transition-force-reset-returns-idle-plist
  (let* ((state (amoebum:make-token-stream-state))
         (updates (amoebum::%ts-transition-force-reset state :elapsed 500)))
    (is (eq :idle (getf updates :status)))
    (is (null (getf updates :cancel-requested-p)))
    (is (null (getf updates :aborted-p)))))

;;; --- Apply updates mutates correctly ---

(test apply-updates-sets-status
  (let ((state (amoebum:make-token-stream-state)))
    (amoebum::%apply-token-stream-updates! state '(:status :running :started-ms 1000))
    (is (eq :running (amoebum::token-stream-state-status state)))
    (is (= 1000 (amoebum::token-stream-state-started-ms state)))))

(test apply-updates-sets-error-message
  (let ((state (amoebum:make-token-stream-state)))
    (amoebum::%apply-token-stream-updates! state
      '(:status :failed :error-message "connection lost"))
    (is (eq :failed (amoebum::token-stream-state-status state)))
    (is (string= "connection lost" (amoebum::token-stream-state-error-message state)))))

;;; --- Dispatch function ---

(test compute-transition-idle-start
  (let* ((state (amoebum:make-token-stream-state))
         (updates (amoebum::%compute-token-stream-transition state :start
                    :target-message-index 3
                    :budget-warning-threshold-percent 90
                    :budget-abort-threshold-percent 80)))
    (is (eq :running (getf updates :status)))))

(test pure-reducer-returns-result-with-new-state
  (let* ((state (amoebum:make-token-stream-state))
         (result (amoebum:token-stream-transition
                  state
                  '(:type :start
                    :target-message-index 3
                    :budget-warning-threshold-percent 90
                    :budget-abort-threshold-percent 80))))
    (is-true (amoebum.fp:ok-p result))
    (is (eq :idle (amoebum::token-stream-state-status state)))
    (is (eq :running
            (amoebum::token-stream-state-status (amoebum.fp:ok-value result))))))

(test compute-transition-invalid-signals-error
  (let ((state (amoebum:make-token-stream-state)))
    ;; :idle + :complete is not in the table
    (signals error
      (amoebum::%compute-token-stream-transition state :complete))))

;;; --- Full lifecycle through dispatch ---

(test full-lifecycle-idle-to-completed
  (let ((state (amoebum:make-token-stream-state)))
    ;; idle → running
    (amoebum::%apply-token-stream-updates! state
      (amoebum::%compute-token-stream-transition state :start
        :target-message-index nil
        :budget-warning-threshold-percent 90
        :budget-abort-threshold-percent 80))
    (is (eq :running (amoebum::token-stream-state-status state)))
    ;; running → completed
    (amoebum::%apply-token-stream-updates! state
      (amoebum::%compute-token-stream-transition state :complete))
    (is (eq :completed (amoebum::token-stream-state-status state)))
    ;; completed → running (restart)
    (amoebum::%apply-token-stream-updates! state
      (amoebum::%compute-token-stream-transition state :start
        :target-message-index nil
        :budget-warning-threshold-percent 90
        :budget-abort-threshold-percent 80))
    (is (eq :running (amoebum::token-stream-state-status state)))))

(test full-lifecycle-idle-to-failed
  (let ((state (amoebum:make-token-stream-state)))
    (amoebum::%apply-token-stream-updates! state
      (amoebum::%compute-token-stream-transition state :start
        :target-message-index nil
        :budget-warning-threshold-percent 90
        :budget-abort-threshold-percent 80))
    (amoebum::%apply-token-stream-updates! state
      (amoebum::%compute-token-stream-transition state :failed
        :error-message "network error"))
    (is (eq :failed (amoebum::token-stream-state-status state)))
    (is (string= "network error" (amoebum::token-stream-state-error-message state)))))
