(in-package :amoebum/test)

;;; ============================================================
;;; I258: Unified Worker-Supervisor Protocol — Smoke Tests
;;;
;;; Tests protocol completeness, record structure, event types,
;;; and registry mechanics. Shell execution tests use a
;;; permissive sandbox policy to avoid permission prompts.
;;; ============================================================

(def-suite worker-supervisor-suite :in amoebum-suite)
(in-suite worker-supervisor-suite)

(defun %wait-for-runtime-agent-terminal-status (agent-id &key (backend :auto) (timeout-ms 2500))
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-ms
                               internal-time-units-per-second
                               1/1000)))))
    (loop
      for status = (amoebum:runtime-agent-status agent-id :backend backend)
      when (member status '(:completed :failed :cancelled :timeout) :test #'eq)
        do (return status)
      when (> (get-internal-real-time) deadline)
        do (return nil)
      do (sleep 0.01))))

;;; --- Protocol completeness ---

(test worker-supervisor-protocol-generics-exist
  "All worker-supervisor protocol generics are defined."
  (is (fboundp 'amoebum:supervisor-spawn))
  (is (fboundp 'amoebum:supervisor-status))
  (is (fboundp 'amoebum:supervisor-result))
  (is (fboundp 'amoebum:supervisor-cancel))
  (is (fboundp 'amoebum:supervisor-list))
  (is (fboundp 'amoebum:supervisor-output)))

;;; --- Worker record ---

(test worker-record-structure
  "worker-record has all expected accessors."
  (is (fboundp 'amoebum.workers:worker-record-id))
  (is (fboundp 'amoebum.workers:worker-record-type))
  (is (fboundp 'amoebum.workers:worker-record-label))
  (is (fboundp 'amoebum.workers:worker-record-command))
  (is (fboundp 'amoebum.workers:worker-record-status))
  (is (fboundp 'amoebum.workers:worker-record-created-at))
  (is (fboundp 'amoebum.workers:worker-record-started-at))
  (is (fboundp 'amoebum.workers:worker-record-finished-at))
  (is (fboundp 'amoebum.workers:worker-record-result))
  (is (fboundp 'amoebum.workers:worker-record-output-buffer))
  (is (fboundp 'amoebum.workers:worker-record-exit-code))
  (is (fboundp 'amoebum.workers:worker-record-error-message))
  (is (fboundp 'amoebum.workers:worker-record-retry-count))
  (is (fboundp 'amoebum.workers:worker-record-max-retries))
  (is (fboundp 'amoebum.workers:worker-record-backend))
  (is (fboundp 'amoebum.workers:worker-record-inner-id)))

;;; --- Event types ---

(test worker-event-types-defined
  "Worker event type constants are defined."
  (is (keywordp amoebum:+event-type-worker-spawned+))
  (is (keywordp amoebum:+event-type-worker-started+))
  (is (keywordp amoebum:+event-type-worker-completed+))
  (is (keywordp amoebum:+event-type-worker-failed+))
  (is (keywordp amoebum:+event-type-worker-cancelled+))
  (is (keywordp amoebum:+event-type-worker-retry+)))

;;; --- In-process supervisor ---

(test ensure-worker-supervisor-returns-supervisor
  "ensure-worker-supervisor returns an in-process-supervisor."
  (let ((amoebum:*worker-supervisor* nil))
    (let ((sup (amoebum:ensure-worker-supervisor)))
      (is (typep sup 'amoebum:in-process-supervisor)))))

;;; --- Spawn shell worker (permission-gated; reaches terminal state) ---

(test spawn-shell-worker-reaches-terminal
  "Spawning a shell worker reaches a terminal status (completed or failed)."
  (let ((amoebum:*worker-supervisor* nil))
    (amoebum:clear-workers)
    (let* ((worker (amoebum:spawn-worker :shell "echo hello-worker"
                                         :label "test echo"
                                         :timeout-seconds 10
                                         :cwd "/tmp"))
           (worker-id (amoebum.workers:worker-record-id worker)))
      (is (stringp worker-id))
      (is (eq :shell (amoebum.workers:worker-record-type worker)))
      ;; Await terminal state — may complete or fail depending on sandbox
      (multiple-value-bind (status result)
          (amoebum.workers:await-worker worker-id :timeout-seconds 15)
        (is (member status '(:completed :failed) :test #'eq))
        (is (or (null result) (listp result)))))))

;;; --- Worker list ---

(test worker-list-includes-spawned
  "worker-list includes spawned workers."
  (let ((amoebum:*worker-supervisor* nil))
    (amoebum:clear-workers)
    (let ((worker (amoebum:spawn-worker :shell "echo test-list"
                                        :label "list test"
                                        :timeout-seconds 10
                                        :cwd "/tmp")))
      (amoebum.workers:await-worker (amoebum.workers:worker-record-id worker)
                            :timeout-seconds 15)
      (let ((workers (amoebum.workers:worker-list)))
        (is (>= (length workers) 1))
        (is (find (amoebum.workers:worker-record-id worker) workers
                  :key #'amoebum.workers:worker-record-id :test #'equal))))))

;;; --- Worker output ---

(test worker-output-returns-string-after-terminal
  "worker-output returns a string (possibly empty) after terminal status."
  (let ((amoebum:*worker-supervisor* nil))
    (amoebum:clear-workers)
    (let* ((worker (amoebum:spawn-worker :shell "echo output-test"
                                         :label "output test"
                                         :timeout-seconds 10
                                         :cwd "/tmp"))
           (wid (amoebum.workers:worker-record-id worker)))
      (amoebum.workers:await-worker wid :timeout-seconds 15)
      (let ((output (amoebum.workers:worker-output wid)))
        (is (or (null output) (stringp output)))))))

;;; --- Worker cancel ---

(test worker-cancel-terminal-returns-nil
  "Cancelling a terminal worker returns NIL."
  (let ((amoebum:*worker-supervisor* nil))
    (amoebum:clear-workers)
    (let* ((worker (amoebum:spawn-worker :shell "echo fast"
                                         :label "cancel test"
                                         :timeout-seconds 10
                                         :cwd "/tmp"))
           (wid (amoebum.workers:worker-record-id worker)))
      (amoebum.workers:await-worker wid :timeout-seconds 15)
      ;; Worker is already in terminal state, cancel should return NIL
      (is (null (amoebum:worker-cancel wid))))))

;;; --- Worker status ---

(test worker-status-returns-keyword
  "worker-status returns a keyword for known workers."
  (let ((amoebum:*worker-supervisor* nil))
    (amoebum:clear-workers)
    (let* ((worker (amoebum:spawn-worker :shell "echo status-test"
                                         :label "status test"
                                         :timeout-seconds 10
                                         :cwd "/tmp"))
           (wid (amoebum.workers:worker-record-id worker)))
      (amoebum.workers:await-worker wid :timeout-seconds 15)
      (is (keywordp (amoebum:worker-status wid))))))

(test spawn-agent-worker-uses-local-delegation-backend
  "Agent workers should use the same local delegation path as /spawn when configured local."
  (let ((old-supervisor amoebum:*worker-supervisor*)
        (old-worker-registry amoebum::*worker-registry*)
        (old-agent-registry amoebum::*agent-registry*)
        (old-swarm-registry amoebum::*swarm-registry*)
        (old-worker-seq amoebum::*next-worker-sequence*)
        (old-agent-seq amoebum::*next-agent-sequence*)
        (old-swarm-counter amoebum::*swarm-counter*)
        (old-config amoebum::*current-config*))
    (unwind-protect
        (progn
          (setf amoebum:*worker-supervisor* nil
                amoebum::*worker-registry* (make-hash-table :test #'equal)
                amoebum::*agent-registry* (make-hash-table :test #'equal)
                amoebum::*swarm-registry* (make-hash-table :test #'equal)
                amoebum::*next-worker-sequence* 0
                amoebum::*next-agent-sequence* 0
                amoebum::*swarm-counter* 0
                amoebum::*current-config*
                (let ((cfg (amoebum.config:load-config :project-root (uiop:getcwd))))
                  (setf (gethash :swarm-delegation-mode (amoebum.config:config-values cfg)) :local)
                  cfg))
          (let* ((worker (amoebum:spawn-worker :agent "worker local task"
                                               :label "worker local"
                                               :timeout-seconds 10))
                 (wid (amoebum.workers:worker-record-id worker)))
            (multiple-value-bind (status result)
                (amoebum.workers:await-worker wid :timeout-seconds 15)
              (declare (ignore result))
              (is (eq :completed status)))
            (is (eq :in-process (amoebum.workers:worker-record-backend worker)))
            (is (string= "task-0001" (amoebum.workers:worker-record-inner-id worker)))
            (is (= 1 (hash-table-count amoebum::*agent-registry*)))
            (is (= 0 (hash-table-count amoebum::*swarm-registry*)))))
      (setf amoebum:*worker-supervisor* old-supervisor
            amoebum::*worker-registry* old-worker-registry
            amoebum::*agent-registry* old-agent-registry
            amoebum::*swarm-registry* old-swarm-registry
            amoebum::*next-worker-sequence* old-worker-seq
            amoebum::*next-agent-sequence* old-agent-seq
            amoebum::*swarm-counter* old-swarm-counter
            amoebum::*current-config* old-config))))

(test spawn-agent-worker-uses-networked-delegation-backend
  "Agent workers should use the same SW4RM delegation path as /spawn when configured networked."
  (let ((old-supervisor amoebum:*worker-supervisor*)
        (old-worker-registry amoebum::*worker-registry*)
        (old-agent-registry amoebum::*agent-registry*)
        (old-swarm-registry amoebum::*swarm-registry*)
        (old-worker-seq amoebum::*next-worker-sequence*)
        (old-agent-seq amoebum::*next-agent-sequence*)
        (old-swarm-counter amoebum::*swarm-counter*)
        (old-config amoebum::*current-config*))
    (unwind-protect
        (progn
          (setf amoebum:*worker-supervisor* nil
                amoebum::*worker-registry* (make-hash-table :test #'equal)
                amoebum::*agent-registry* (make-hash-table :test #'equal)
                amoebum::*swarm-registry* (make-hash-table :test #'equal)
                amoebum::*next-worker-sequence* 0
                amoebum::*next-agent-sequence* 0
                amoebum::*swarm-counter* 0
                amoebum::*current-config*
                (let ((cfg (amoebum.config:load-config :project-root (uiop:getcwd))))
                  (setf (gethash :swarm-delegation-mode (amoebum.config:config-values cfg)) :networked)
                  cfg))
          (let* ((worker (amoebum:spawn-worker :agent "worker networked task"
                                               :label "worker networked"
                                               :timeout-seconds 10))
                 (wid (amoebum.workers:worker-record-id worker)))
            (multiple-value-bind (status result)
                (amoebum.workers:await-worker wid :timeout-seconds 15)
              (is (eq :completed status))
              (is (listp result))
              (is (eq :swarm (getf result :backend)))
              (is (eq :completed (getf result :status))))
            (is (eq :swarm (amoebum.workers:worker-record-backend worker)))
            (is (string= "swarm-1" (amoebum.workers:worker-record-inner-id worker)))
            (is (= 0 (hash-table-count amoebum::*agent-registry*)))
            (is (= 1 (hash-table-count amoebum::*swarm-registry*)))))
      (setf amoebum:*worker-supervisor* old-supervisor
            amoebum::*worker-registry* old-worker-registry
            amoebum::*agent-registry* old-agent-registry
            amoebum::*swarm-registry* old-swarm-registry
            amoebum::*next-worker-sequence* old-worker-seq
            amoebum::*next-agent-sequence* old-agent-seq
            amoebum::*swarm-counter* old-swarm-counter
            amoebum::*current-config* old-config)))) 

(test runtime-agent-api-unifies-local-and-swarm-lookups
  "The shared runtime agent API should resolve both local and SW4RM-backed agents."
  (let ((old-agent-registry amoebum::*agent-registry*)
        (old-swarm-registry amoebum::*swarm-registry*)
        (old-agent-seq amoebum::*next-agent-sequence*)
        (old-swarm-counter amoebum::*swarm-counter*))
    (unwind-protect
        (progn
          (setf amoebum::*agent-registry* (make-hash-table :test #'equal)
                amoebum::*swarm-registry* (make-hash-table :test #'equal)
                amoebum::*next-agent-sequence* 0
                amoebum::*swarm-counter* 0)
          (let* ((local-agent (amoebum:spawn-agent
                               "local runtime status task"
                               :runner (lambda (_agent)
                                         (declare (ignore _agent))
                                         "local runtime ok")))
                 (swarm-agent (amoebum:spawn-swarm-agent
                               "swarm runtime status task"
                               :runner (lambda (_agent)
                                         (declare (ignore _agent))
                                         "swarm runtime ok")))
                 (local-id (amoebum:agent-record-id local-agent))
                 (swarm-id (amoebum:swarm-agent-id swarm-agent)))
            (is (eq :completed
                    (%wait-for-runtime-agent-terminal-status local-id :backend :local)))
            (multiple-value-bind (swarm-result swarm-status)
                (amoebum:collect-swarm-result swarm-id)
              (is (string= "swarm runtime ok" swarm-result))
              (is (eq :completed swarm-status)))
            (is (eq :local (amoebum:runtime-agent-backend local-id)))
            (is (eq :local (amoebum:runtime-agent-backend local-agent)))
            (is (string= local-id (amoebum:runtime-agent-id local-id)))
            (is (string= "local runtime status task"
                         (amoebum:runtime-agent-task local-id)))
            (is (eq :completed (amoebum:runtime-agent-status local-id)))
            (is (string= "local runtime ok"
                         (amoebum:runtime-agent-result local-id)))
            (is-true (amoebum:runtime-agent-terminal-p local-id))
            (is (eq :swarm (amoebum:runtime-agent-backend swarm-id)))
            (is (eq :swarm (amoebum:runtime-agent-backend swarm-agent)))
            (is (string= swarm-id (amoebum:runtime-agent-id swarm-id)))
            (is (string= "swarm runtime status task"
                         (amoebum:runtime-agent-task swarm-id)))
            (is (eq :completed
                    (amoebum:runtime-agent-status swarm-id :backend :swarm)))
            (is (string= "swarm runtime ok"
                         (amoebum:runtime-agent-result swarm-id :backend :swarm)))
            (is-true (amoebum:runtime-agent-terminal-p swarm-id :backend :swarm))
            (is (null (amoebum:find-runtime-agent "missing-runtime-agent")))))
      (setf amoebum::*agent-registry* old-agent-registry
            amoebum::*swarm-registry* old-swarm-registry
            amoebum::*next-agent-sequence* old-agent-seq
            amoebum::*swarm-counter* old-swarm-counter))))

(test runtime-agent-api-surfaces-sw4rm-failure-paths
  "The shared runtime agent API should preserve SW4RM crash, timeout, and cancellation outcomes."
  (let ((old-swarm-registry amoebum::*swarm-registry*)
        (old-swarm-counter amoebum::*swarm-counter*))
    (unwind-protect
        (progn
          (setf amoebum::*swarm-registry* (make-hash-table :test #'equal)
                amoebum::*swarm-counter* 0)
          (let* ((failed-agent
                   (amoebum:spawn-swarm-agent
                    "swarm crash task"
                    :runner (lambda (_agent)
                              (declare (ignore _agent))
                              (error "sw4rm crash sentinel"))))
                 (timeout-agent
                   (amoebum:spawn-swarm-agent
                    "swarm timeout task"
                    :timeout-seconds 1
                    :runner (lambda (_agent)
                              (declare (ignore _agent))
                              (sleep 2)
                              "late swarm completion")))
                 (cancelled-agent
                   (amoebum:spawn-swarm-agent
                    "swarm cancel task"
                    :runner (lambda (runner-agent)
                              (loop repeat 200 do
                                (sleep 0.01)
                                (amoebum::agent-check-cancel runner-agent))
                              "never reached")))
                 (failed-id (amoebum:swarm-agent-id failed-agent))
                 (timeout-id (amoebum:swarm-agent-id timeout-agent))
                 (cancelled-id (amoebum:swarm-agent-id cancelled-agent)))
            (amoebum:kill-swarm-agent cancelled-id)

            (multiple-value-bind (failed-result failed-status)
                (amoebum:collect-swarm-result failed-id)
              (is (null failed-result))
              (is (eq :failed failed-status)))
            (is (eq :failed
                    (%wait-for-runtime-agent-terminal-status failed-id :backend :swarm)))
            (is (eq :failed (amoebum:runtime-agent-status failed-id :backend :swarm)))
            (is (null (amoebum:runtime-agent-result failed-id :backend :swarm)))
            (is (search "sw4rm crash sentinel"
                        (or (amoebum:runtime-agent-error-message failed-id :backend :swarm) "")
                        :test #'char-equal))
            (is-true (amoebum:runtime-agent-terminal-p failed-id :backend :swarm))

            (multiple-value-bind (timeout-result timeout-status)
                (amoebum:collect-swarm-result timeout-id)
              (is (null timeout-result))
              (is (eq :timeout timeout-status)))
            (is (eq :timeout
                    (%wait-for-runtime-agent-terminal-status timeout-id :backend :swarm)))
            (is (eq :timeout (amoebum:runtime-agent-status timeout-id :backend :swarm)))
            (is (null (amoebum:runtime-agent-result timeout-id :backend :swarm)))
            (is (search "timed out after 1 seconds"
                        (or (amoebum:runtime-agent-error-message timeout-id :backend :swarm) "")
                        :test #'char-equal))
            (is-true (amoebum:runtime-agent-terminal-p timeout-id :backend :swarm))

            (multiple-value-bind (cancelled-result cancelled-status)
                (amoebum:collect-swarm-result cancelled-id)
              (is (null cancelled-result))
              (is (eq :cancelled cancelled-status)))
            (is (eq :cancelled
                    (%wait-for-runtime-agent-terminal-status cancelled-id :backend :swarm)))
            (is (eq :cancelled (amoebum:runtime-agent-status cancelled-id :backend :swarm)))
            (is (null (amoebum:runtime-agent-result cancelled-id :backend :swarm)))
            (is (search "cancelled"
                        (string-downcase
                         (or (amoebum:runtime-agent-error-message
                              cancelled-id
                              :backend :swarm)
                             ""))
                        :test #'char-equal))
            (is-true (amoebum:runtime-agent-terminal-p cancelled-id :backend :swarm))))
      (setf amoebum::*swarm-registry* old-swarm-registry
            amoebum::*swarm-counter* old-swarm-counter))))

(test worker-status-nil-for-unknown
  "worker-status returns NIL for unknown worker-id."
  (let ((amoebum:*worker-supervisor* nil))
    (is (null (amoebum:worker-status "w-nonexistent-9999")))))

;;; --- Clear workers ---

(test clear-workers-empties-registry
  "clear-workers removes all workers."
  (let ((amoebum:*worker-supervisor* nil))
    (amoebum:clear-workers)
    (amoebum:spawn-worker :shell "echo cleanup"
                          :label "cleanup test"
                          :timeout-seconds 10
                          :cwd "/tmp")
    (sleep 1)
    (amoebum:clear-workers)
    (is (= 0 (length (amoebum.workers:worker-list))))))

;;; --- Active worker count ---

(test active-worker-count-zero-after-terminal
  "active-worker-count is 0 after all workers reach terminal status."
  (let ((amoebum:*worker-supervisor* nil))
    (amoebum:clear-workers)
    (let* ((worker (amoebum:spawn-worker :shell "echo done"
                                         :label "count test"
                                         :timeout-seconds 10
                                         :cwd "/tmp"))
           (wid (amoebum.workers:worker-record-id worker)))
      (amoebum.workers:await-worker wid :timeout-seconds 15)
      (is (= 0 (amoebum:active-worker-count))))))

;;; --- Await-workers (join) ---

(test await-workers-joins-multiple
  "await-workers waits for multiple workers to reach terminal status."
  (let ((amoebum:*worker-supervisor* nil))
    (amoebum:clear-workers)
    (let* ((w1 (amoebum:spawn-worker :shell "echo w1"
                                     :label "join test 1"
                                     :timeout-seconds 10
                                     :cwd "/tmp"))
           (w2 (amoebum:spawn-worker :shell "echo w2"
                                     :label "join test 2"
                                     :timeout-seconds 10
                                     :cwd "/tmp"))
           (ids (list (amoebum.workers:worker-record-id w1)
                      (amoebum.workers:worker-record-id w2)))
           (results (amoebum.workers:await-workers ids :timeout-seconds 15)))
      (is (= 2 (length results)))
      ;; All workers should be in terminal state
      (is (every (lambda (r) (member (second r) '(:completed :failed) :test #'eq))
                 results)))))

;;; --- Await-any-worker (race) ---

(test await-any-worker-returns-first
  "await-any-worker returns the first worker to reach terminal status."
  (let ((amoebum:*worker-supervisor* nil))
    (amoebum:clear-workers)
    (let* ((w1 (amoebum:spawn-worker :shell "echo fast"
                                     :label "race test"
                                     :timeout-seconds 10
                                     :cwd "/tmp"))
           (ids (list (amoebum.workers:worker-record-id w1))))
      (multiple-value-bind (wid status result)
          (amoebum:await-any-worker ids :timeout-seconds 15)
        (is (stringp wid))
        (is (member status '(:completed :failed) :test #'eq))
        (is (or (null result) (listp result)))))))

;;; --- Convenience API aliases ---

(test convenience-api-functions-exist
  "All convenience API functions are bound."
  (is (fboundp 'amoebum:spawn-worker))
  (is (fboundp 'amoebum:worker-status))
  (is (fboundp 'amoebum:worker-result))
  (is (fboundp 'amoebum:worker-cancel))
  (is (fboundp 'amoebum.workers:worker-list))
  (is (fboundp 'amoebum.workers:worker-output))
  (is (fboundp 'amoebum:active-worker-count))
  (is (fboundp 'amoebum:clear-workers))
  (is (fboundp 'amoebum.workers:await-worker))
  (is (fboundp 'amoebum.workers:await-workers))
  (is (fboundp 'amoebum:await-any-worker)))
