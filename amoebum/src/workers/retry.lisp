(in-package :amoebum)

;;; ============================================================
;;; I260: Worker Retry Logic and Supervision Policy
;;;
;;; Retry policies with configurable backoff strategies,
;;; failure classification (transient vs permanent), and
;;; parent-child supervision tree with restarts.
;;; ============================================================

;;; --- Backoff strategies ---

(defun %backoff-delay (strategy base-seconds attempt)
  "Compute delay in seconds for ATTEMPT using STRATEGY.
   STRATEGY is one of :none, :linear, :exponential."
  (ecase strategy
    (:none 0)
    (:linear (* base-seconds attempt))
    (:exponential (* base-seconds (expt 2 (1- attempt))))))

;;; --- Supervision policy ---

(defstruct (worker-supervision-policy
            (:constructor make-worker-supervision-policy
                (&key (max-retries 0)
                      (backoff-strategy :none)
                      (backoff-base-seconds 5)
                      (timeout-escalation-factor 1.5)
                      transient-exit-codes
                      transient-output-patterns
                      permanent-exit-codes)))
  "Policy governing retry and failure classification for workers."
  (max-retries 0 :type integer)
  (backoff-strategy :none :type keyword)  ; :none, :linear, :exponential
  (backoff-base-seconds 5 :type number)
  (timeout-escalation-factor 1.5 :type number)
  (transient-exit-codes '(75 111) :type list)  ; EX_TEMPFAIL, connection-refused
  (transient-output-patterns nil :type list)    ; regex strings
  (permanent-exit-codes '(126 127) :type list)) ; permission-denied, not-found

(defparameter *default-supervision-policy*
  (make-worker-supervision-policy)
  "Default supervision policy with no retries.")

;;; --- Failure classification ---

(defun classify-worker-failure (worker &optional (policy *default-supervision-policy*))
  "Classify a worker failure as :transient or :permanent.
   Returns (values classification reason)."
  (let ((exit-code (worker-record-exit-code worker))
        (output (or (worker-record-output-buffer worker) "")))
    (cond
      ;; Explicit permanent exit codes
      ((and exit-code
            (member exit-code (worker-supervision-policy-permanent-exit-codes policy)
                    :test #'=))
       (values :permanent (format nil "exit code ~D is permanently failing" exit-code)))
      ;; Explicit transient exit codes
      ((and exit-code
            (member exit-code (worker-supervision-policy-transient-exit-codes policy)
                    :test #'=))
       (values :transient (format nil "exit code ~D is transiently failing" exit-code)))
      ;; Transient output patterns
      ((some (lambda (pattern)
               (cl-ppcre:scan pattern output))
             (worker-supervision-policy-transient-output-patterns policy))
       (values :transient "output matches transient pattern"))
      ;; Timeout is transient by default
      ((eq (worker-record-status worker) :timeout)
       (values :transient "timeout is transient"))
      ;; Default: permanent
      (t
       (values :permanent "no transient classification matched")))))

;;; --- Retry execution ---

(defun %compute-retry-timeout (original-timeout policy attempt)
  "Escalate timeout for retry attempt."
  (let ((factor (worker-supervision-policy-timeout-escalation-factor policy)))
    (ceiling (* original-timeout (expt factor attempt)))))

(defun worker-retry-eligible-p (worker &optional (policy *default-supervision-policy*))
  "Return T if WORKER can be retried under POLICY."
  (and (member (worker-record-status worker) '(:failed :timeout) :test #'eq)
       (< (worker-record-retry-count worker)
          (worker-supervision-policy-max-retries policy))
       (eq :transient (classify-worker-failure worker policy))))

(defun schedule-worker-retry (worker &key
                                       (policy *default-supervision-policy*)
                                       (supervisor (ensure-worker-supervisor))
                                       (original-timeout 120))
  "Schedule a retry for WORKER. Returns new worker-record or NIL if not eligible."
  (unless (worker-retry-eligible-p worker policy)
    (return-from schedule-worker-retry nil))
  (let* ((attempt (1+ (worker-record-retry-count worker)))
         (delay (%backoff-delay
                 (worker-supervision-policy-backoff-strategy policy)
                 (worker-supervision-policy-backoff-base-seconds policy)
                 attempt))
         (new-timeout (%compute-retry-timeout original-timeout policy attempt)))
    ;; Emit retry event
    (%publish-worker-event +event-type-worker-retry+ worker
                           :severity :warning)
    ;; Wait for backoff delay
    (when (plusp delay)
      (sleep delay))
    ;; Spawn replacement worker with incremented retry count
    (let ((new-worker (supervisor-spawn supervisor
                                        (worker-record-type worker)
                                        (worker-record-command worker)
                                        :label (format nil "~A (retry ~D)"
                                                       (worker-record-label worker)
                                                       attempt)
                                        :timeout-seconds new-timeout
                                        :max-retries (worker-record-max-retries worker)
                                        :worktree (worker-record-worktree worker))))
      (%with-worker-lock
        (setf (worker-record-retry-count new-worker) attempt))
      new-worker)))

;;; --- Supervised spawn (spawn with automatic retry) ---

(defun spawn-worker-supervised (type command &key
                                               label
                                               (timeout-seconds 120)
                                               (policy *default-supervision-policy*)
                                               cwd
                                               worktree)
  "Spawn a worker with automatic retry on transient failure.
   Blocks until final success or max retries exhausted.
   Returns (values final-worker final-status)."
  (let* ((supervisor (ensure-worker-supervisor))
         (worker (supervisor-spawn supervisor type command
                                   :label (or label "supervised-task")
                                   :timeout-seconds timeout-seconds
                                   :max-retries (worker-supervision-policy-max-retries policy)
                                   :cwd cwd
                                   :worktree worktree)))
    (loop
      (multiple-value-bind (status _result)
          (await-worker (worker-record-id worker) :timeout-seconds (+ timeout-seconds 10))
        (declare (ignore _result))
        (cond
          ;; Success or cancellation - done
          ((member status '(:completed :cancelled nil) :test #'eq)
           (return (values worker status)))
          ;; Failure - try retry
          ((worker-retry-eligible-p worker policy)
           (let ((retry-worker (schedule-worker-retry worker
                                                      :policy policy
                                                      :supervisor supervisor
                                                      :original-timeout timeout-seconds)))
             (if retry-worker
                 (setf worker retry-worker)
                 (return (values worker status)))))
          ;; Permanent failure
          (t
           (return (values worker status))))))))

;;; --- Parent-child supervision ---

(define-condition child-worker-failed (error)
  ((child-worker :initarg :child-worker :reader child-worker-failed-worker)
   (classification :initarg :classification :reader child-worker-failed-classification)
   (parent-worker-id :initarg :parent-worker-id :reader child-worker-failed-parent-id))
  (:report (lambda (c stream)
             (format stream "Child worker ~A failed (~A)"
                     (worker-record-id (child-worker-failed-worker c))
                     (child-worker-failed-classification c)))))

(defun supervise-child-worker (parent-worker-id child-worker &key
                                                                (policy *default-supervision-policy*))
  "Supervise CHILD-WORKER on behalf of PARENT-WORKER-ID.
   On failure, signals CHILD-WORKER-FAILED with restarts:
   :retry-child, :skip-child, :abort-parent."
  (multiple-value-bind (status _result)
      (await-worker (worker-record-id child-worker))
    (declare (ignore _result))
    (when (member status '(:failed :timeout) :test #'eq)
      (multiple-value-bind (classification _reason)
          (classify-worker-failure child-worker policy)
        (declare (ignore _reason))
        (restart-case
            (error 'child-worker-failed
                   :child-worker child-worker
                   :classification classification
                   :parent-worker-id parent-worker-id)
          (retry-child ()
            :report "Retry the child worker"
            (let ((retry (schedule-worker-retry child-worker
                                                :policy policy)))
              (if retry
                  (supervise-child-worker parent-worker-id retry :policy policy)
                  (values child-worker :failed))))
          (skip-child ()
            :report "Skip the failed child and continue"
            (values child-worker :skipped))
          (abort-parent ()
            :report "Abort the parent worker"
            (worker-cancel parent-worker-id)
            (values child-worker :aborted))))))
  (values child-worker (worker-record-status child-worker)))
