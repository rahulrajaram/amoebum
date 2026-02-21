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
  (is (fboundp 'amoebum:worker-record-id))
  (is (fboundp 'amoebum:worker-record-type))
  (is (fboundp 'amoebum:worker-record-label))
  (is (fboundp 'amoebum:worker-record-command))
  (is (fboundp 'amoebum:worker-record-status))
  (is (fboundp 'amoebum:worker-record-created-at))
  (is (fboundp 'amoebum:worker-record-started-at))
  (is (fboundp 'amoebum:worker-record-finished-at))
  (is (fboundp 'amoebum:worker-record-result))
  (is (fboundp 'amoebum:worker-record-output-buffer))
  (is (fboundp 'amoebum:worker-record-exit-code))
  (is (fboundp 'amoebum:worker-record-error-message))
  (is (fboundp 'amoebum:worker-record-retry-count))
  (is (fboundp 'amoebum:worker-record-max-retries))
  (is (fboundp 'amoebum:worker-record-backend))
  (is (fboundp 'amoebum:worker-record-inner-id)))

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
           (worker-id (amoebum:worker-record-id worker)))
      (is (stringp worker-id))
      (is (eq :shell (amoebum:worker-record-type worker)))
      ;; Await terminal state — may complete or fail depending on sandbox
      (multiple-value-bind (status result)
          (amoebum:await-worker worker-id :timeout-seconds 15)
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
      (amoebum:await-worker (amoebum:worker-record-id worker)
                            :timeout-seconds 15)
      (let ((workers (amoebum:worker-list)))
        (is (>= (length workers) 1))
        (is (find (amoebum:worker-record-id worker) workers
                  :key #'amoebum:worker-record-id :test #'equal))))))

;;; --- Worker output ---

(test worker-output-returns-string-after-terminal
  "worker-output returns a string (possibly empty) after terminal status."
  (let ((amoebum:*worker-supervisor* nil))
    (amoebum:clear-workers)
    (let* ((worker (amoebum:spawn-worker :shell "echo output-test"
                                         :label "output test"
                                         :timeout-seconds 10
                                         :cwd "/tmp"))
           (wid (amoebum:worker-record-id worker)))
      (amoebum:await-worker wid :timeout-seconds 15)
      (let ((output (amoebum:worker-output wid)))
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
           (wid (amoebum:worker-record-id worker)))
      (amoebum:await-worker wid :timeout-seconds 15)
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
           (wid (amoebum:worker-record-id worker)))
      (amoebum:await-worker wid :timeout-seconds 15)
      (is (keywordp (amoebum:worker-status wid))))))

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
    (is (= 0 (length (amoebum:worker-list))))))

;;; --- Active worker count ---

(test active-worker-count-zero-after-terminal
  "active-worker-count is 0 after all workers reach terminal status."
  (let ((amoebum:*worker-supervisor* nil))
    (amoebum:clear-workers)
    (let* ((worker (amoebum:spawn-worker :shell "echo done"
                                         :label "count test"
                                         :timeout-seconds 10
                                         :cwd "/tmp"))
           (wid (amoebum:worker-record-id worker)))
      (amoebum:await-worker wid :timeout-seconds 15)
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
           (ids (list (amoebum:worker-record-id w1)
                      (amoebum:worker-record-id w2)))
           (results (amoebum:await-workers ids :timeout-seconds 15)))
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
           (ids (list (amoebum:worker-record-id w1))))
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
  (is (fboundp 'amoebum:worker-list))
  (is (fboundp 'amoebum:worker-output))
  (is (fboundp 'amoebum:active-worker-count))
  (is (fboundp 'amoebum:clear-workers))
  (is (fboundp 'amoebum:await-worker))
  (is (fboundp 'amoebum:await-workers))
  (is (fboundp 'amoebum:await-any-worker)))
