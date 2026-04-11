(in-package :amoebum/test)

;;; ============================================================
;;; NXT-285: Unit tests for amoebum/src/swarm.lisp
;;;
;;; Complements swarm-execution-semantics-test.lisp (which covers
;;; NXT-017 signal/retry semantics and NXT-018 stalled-run
;;; detection) by exercising the unit-level concerns not otherwise
;;; tested:
;;;
;;;   - Worker assignment via *swarm-registry* (each spawn gets a
;;;     unique agent-id and is addressable through find-swarm-agent)
;;;   - Fan-out: a batch of N tasks distributes across N independent
;;;     swarm agents, each runs concurrently, and each runner
;;;     observes its own task
;;;   - Error propagation: a runner that signals an error drives the
;;;     swarm agent to :failed with the error message captured, and
;;;     the agent id is recoverable through the registry
;;;   - Lifecycle: initializing -> running -> completed transitions
;;;     are visible through swarm-agent-status
;;;
;;; These tests deliberately avoid touching swarm.lisp or the
;;; execution-semantics suite.
;;; ============================================================

(def-suite swarm-unit-suite :in amoebum-suite)
(in-suite swarm-unit-suite)

;;; --- Local fixture (mirrors the one in swarm-execution-semantics-test
;;; but lives here so each suite is independent) ---

(defun %swarm-unit-save ()
  (list :registry amoebum::*swarm-registry*
        :counter amoebum::*swarm-counter*))

(defun %swarm-unit-restore (saved)
  (setf amoebum::*swarm-registry* (getf saved :registry)
        amoebum::*swarm-counter* (getf saved :counter)))

(defmacro with-unit-swarm (&body body)
  (let ((saved (gensym "SAVED")))
    `(let ((,saved (%swarm-unit-save)))
       (setf amoebum::*swarm-registry* (make-hash-table :test #'equal)
             amoebum::*swarm-counter* 0)
       (unwind-protect
           (progn ,@body)
         (%swarm-unit-restore ,saved)))))

(defun %unit-wait-terminal (agent-id &key (timeout-ms 3000))
  "Wait up to TIMEOUT-MS for AGENT-ID to reach a terminal state.
Returns the final status keyword or NIL on timeout."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-ms
                               internal-time-units-per-second
                               1/1000)))))
    (loop
      (let ((agent (amoebum:find-swarm-agent agent-id)))
        (when agent
          (let ((status (amoebum:swarm-agent-status agent)))
            (when (member status '(:completed :failed :cancelled :timeout)
                          :test #'eq)
              (return status))))
        (when (> (get-internal-real-time) deadline)
          (return nil))
        (sleep 0.01)))))

;;; ============================================================
;;; Worker assignment: registry is the assignment table
;;; ============================================================

(test swarm-unit-spawn-assigns-unique-ids
  "Consecutive spawn-swarm-agent calls receive distinct agent ids
and each is stored in *swarm-registry* under its id."
  (with-unit-swarm
    (let* ((runner (lambda (_a) (declare (ignore _a)) "done"))
           (a (amoebum:spawn-swarm-agent "unit assign a" :runner runner))
           (b (amoebum:spawn-swarm-agent "unit assign b" :runner runner))
           (c (amoebum:spawn-swarm-agent "unit assign c" :runner runner)))
      (let ((ids (list (amoebum:swarm-agent-id a)
                       (amoebum:swarm-agent-id b)
                       (amoebum:swarm-agent-id c))))
        ;; All ids distinct
        (is (= 3 (length (remove-duplicates ids :test #'string=))))
        ;; All three findable via the registry
        (dolist (id ids)
          (is (not (null (amoebum:find-swarm-agent id)))))
        ;; Drain for isolation
        (dolist (id ids)
          (amoebum:collect-swarm-result id))))))

(test swarm-unit-spawn-honours-caller-supplied-id
  "spawn-swarm-agent :id keyword selects the registry slot for the
new agent, giving the caller explicit worker assignment control."
  (with-unit-swarm
    (let* ((runner (lambda (_a) (declare (ignore _a)) :ok))
           (explicit-id "unit-worker-42")
           (agent (amoebum:spawn-swarm-agent
                   "unit explicit assignment"
                   :id explicit-id
                   :runner runner)))
      (is (string= explicit-id (amoebum:swarm-agent-id agent)))
      (is (eq agent (amoebum:find-swarm-agent explicit-id)))
      (amoebum:collect-swarm-result explicit-id))))

;;; ============================================================
;;; Fan-out: N tasks distribute to N independent workers
;;; ============================================================

(test swarm-unit-fan-out-distributes-to-all-workers
  "Spawning N swarm agents in a batch produces N agents; each runner
receives exactly its own task and all N terminate successfully."
  (with-unit-swarm
    (let* ((n 5)
           (lock (bt:make-lock "swarm-unit-fanout-lock"))
           (seen '())
           (runner (lambda (runner-agent)
                     (let ((task (amoebum::agent-record-task runner-agent)))
                       (bt:with-lock-held (lock)
                         (push task seen))
                       task)))
           (tasks (loop for i from 0 below n
                        collect (format nil "fanout-task-~D" i)))
           (agents (mapcar (lambda (task)
                             (amoebum:spawn-swarm-agent task :runner runner))
                           tasks)))
      ;; Wait for all to finish
      (dolist (agent agents)
        (let ((status (%unit-wait-terminal (amoebum:swarm-agent-id agent))))
          (is (eq :completed status))))
      ;; Every task must have been observed by its runner exactly once
      (is (= n (length seen)))
      (dolist (task tasks)
        (is (not (null (find task seen :test #'string=)))))
      ;; list-swarm-agents sees the whole cohort
      (is (= n (length (amoebum:list-swarm-agents)))))))

(test swarm-unit-fan-out-workers-are-independent
  "Failure in one fanned-out worker does not affect siblings: the
surviving agents still reach :completed with their own results."
  (with-unit-swarm
    (let* ((failing-runner (lambda (_a)
                             (declare (ignore _a))
                             (error "planned fan-out failure")))
           (ok-runner (lambda (_a) (declare (ignore _a)) "sibling-ok"))
           (bad (amoebum:spawn-swarm-agent "fanout failing"
                                           :runner failing-runner))
           (good-1 (amoebum:spawn-swarm-agent "fanout good 1"
                                              :runner ok-runner))
           (good-2 (amoebum:spawn-swarm-agent "fanout good 2"
                                              :runner ok-runner))
           (bad-id (amoebum:swarm-agent-id bad))
           (good-1-id (amoebum:swarm-agent-id good-1))
           (good-2-id (amoebum:swarm-agent-id good-2)))
      (is (eq :failed (%unit-wait-terminal bad-id)))
      (is (eq :completed (%unit-wait-terminal good-1-id)))
      (is (eq :completed (%unit-wait-terminal good-2-id)))
      (is (equal "sibling-ok"
                 (amoebum:swarm-agent-result
                  (amoebum:find-swarm-agent good-1-id))))
      (is (equal "sibling-ok"
                 (amoebum:swarm-agent-result
                  (amoebum:find-swarm-agent good-2-id)))))))

;;; ============================================================
;;; Error propagation: runner signal -> :failed + id recoverable
;;; ============================================================

(test swarm-unit-runner-error-propagates-to-failed
  "A runner that signals an error drives the agent to :failed and
the error message is captured on the struct."
  (with-unit-swarm
    (let* ((agent (amoebum:spawn-swarm-agent
                   "unit error propagation"
                   :runner (lambda (_a)
                             (declare (ignore _a))
                             (error "boom-unit"))))
           (agent-id (amoebum:swarm-agent-id agent))
           (status (%unit-wait-terminal agent-id)))
      (is (eq :failed status))
      (let* ((fresh (amoebum:find-swarm-agent agent-id))
             (msg (amoebum:swarm-agent-error-message fresh)))
        (is (stringp msg))
        (is (not (null (search "boom-unit" msg))))
        ;; agent id is still the key in the registry, so failure is
        ;; traceable back to the specific worker
        (is (string= agent-id (amoebum:swarm-agent-id fresh)))))))

(test swarm-unit-error-isolates-by-agent-id
  "When one of several workers fails, the failing agent id is
recoverable from the registry without disturbing other workers."
  (with-unit-swarm
    (let* ((ok-runner (lambda (_a) (declare (ignore _a)) :ok))
           (bad-runner (lambda (_a)
                         (declare (ignore _a))
                         (error "isolated-failure")))
           (a (amoebum:spawn-swarm-agent "iso ok 1" :runner ok-runner))
           (b (amoebum:spawn-swarm-agent "iso bad"  :runner bad-runner))
           (c (amoebum:spawn-swarm-agent "iso ok 2" :runner ok-runner))
           (a-id (amoebum:swarm-agent-id a))
           (b-id (amoebum:swarm-agent-id b))
           (c-id (amoebum:swarm-agent-id c)))
      (%unit-wait-terminal a-id)
      (%unit-wait-terminal b-id)
      (%unit-wait-terminal c-id)
      (let ((failed (amoebum:list-swarm-agents :status :failed))
            (completed (amoebum:list-swarm-agents :status :completed)))
        (is (= 1 (length failed)))
        (is (string= b-id (amoebum:swarm-agent-id (first failed))))
        (is (= 2 (length completed)))
        (is (not (null (find a-id completed
                             :key #'amoebum:swarm-agent-id
                             :test #'string=))))
        (is (not (null (find c-id completed
                             :key #'amoebum:swarm-agent-id
                             :test #'string=))))))))

;;; ============================================================
;;; Lifecycle: spawn -> running -> completed
;;; ============================================================

(test swarm-unit-lifecycle-reaches-completed
  "A successful swarm agent reaches :completed and finished-at is
populated after collect-swarm-result returns."
  (with-unit-swarm
    (let* ((agent (amoebum:spawn-swarm-agent
                   "unit lifecycle completed"
                   :runner (lambda (_a) (declare (ignore _a)) "all-done")))
           (agent-id (amoebum:swarm-agent-id agent)))
      (multiple-value-bind (result status)
          (amoebum:collect-swarm-result agent-id)
        (is (equal "all-done" result))
        (is (eq :completed status)))
      (let ((fresh (amoebum:find-swarm-agent agent-id)))
        (is (not (null (amoebum:swarm-agent-finished-at fresh))))))))

(test swarm-unit-lifecycle-observable-running-state
  "Between spawn and terminal, a long-running swarm agent is
observable in :running (or :initializing) then transitions to
:completed once the runner returns."
  (with-unit-swarm
    (let* ((gate (bt:make-lock "unit-lifecycle-gate"))
           (proceed nil)
           (cv (bt:make-condition-variable))
           (agent (amoebum:spawn-swarm-agent
                   "unit lifecycle transition"
                   :runner (lambda (_a)
                             (declare (ignore _a))
                             (bt:with-lock-held (gate)
                               (loop until proceed do
                                 (bt:condition-wait cv gate)))
                             "released")))
           (agent-id (amoebum:swarm-agent-id agent)))
      ;; Wait briefly for the thread to start and transition to :running
      (loop repeat 50
            until (eq :running
                      (amoebum:swarm-agent-status
                       (amoebum:find-swarm-agent agent-id)))
            do (sleep 0.01))
      (let ((mid-status (amoebum:swarm-agent-status
                         (amoebum:find-swarm-agent agent-id))))
        (is (member mid-status '(:initializing :running) :test #'eq)))
      ;; Release the runner
      (bt:with-lock-held (gate)
        (setf proceed t)
        (bt:condition-notify cv))
      (is (eq :completed (%unit-wait-terminal agent-id))))))

(test swarm-unit-clear-swarm-registry-resets-state
  "clear-swarm-registry empties the worker table and resets the
monotonic id counter. Subsequent spawns start from a clean slate."
  (with-unit-swarm
    (let ((runner (lambda (_a) (declare (ignore _a)) "x")))
      (amoebum:collect-swarm-result
       (amoebum:swarm-agent-id
        (amoebum:spawn-swarm-agent "pre-clear a" :runner runner)))
      (amoebum:collect-swarm-result
       (amoebum:swarm-agent-id
        (amoebum:spawn-swarm-agent "pre-clear b" :runner runner)))
      (is (= 2 (length (amoebum:list-swarm-agents))))
      (amoebum:clear-swarm-registry)
      (is (null (amoebum:list-swarm-agents)))
      ;; After clearing, a new spawn is the only registry entry.
      (let ((fresh (amoebum:spawn-swarm-agent "post-clear" :runner runner)))
        (amoebum:collect-swarm-result (amoebum:swarm-agent-id fresh))
        (is (= 1 (length (amoebum:list-swarm-agents))))))))
