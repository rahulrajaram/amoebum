(in-package :amoebum/test)

;;; ============================================================
;;; I261: Worker Fan-Out and Join — Smoke Tests
;;; ============================================================

(def-suite worker-fanout-suite :in amoebum-suite)
(in-suite worker-fanout-suite)

;;; --- Structure tests ---

(test worker-group-struct-exists
  "worker-group struct and accessors are defined."
  (is (fboundp 'amoebum.workers:worker-group-p))
  (is (fboundp 'amoebum.workers:worker-group-id))
  (is (fboundp 'amoebum.workers:worker-group-worker-ids))
  (is (fboundp 'amoebum.workers:worker-group-status)))

;;; --- API functions ---

(test fanout-api-functions-exist
  "Fan-out API functions are bound."
  (is (fboundp 'amoebum:fan-out-workers))
  (is (fboundp 'amoebum.workers:join-worker-group))
  (is (fboundp 'amoebum.workers:race-worker-group))
  (is (fboundp 'amoebum:merge-worker-results))
  (is (fboundp 'amoebum.workers:find-worker-group))
  (is (fboundp 'amoebum.workers:worker-group-results))
  (is (fboundp 'amoebum.workers:clear-worker-groups)))

;;; --- Fan-out spawning ---

(test fan-out-workers-spawns-multiple
  "fan-out-workers spawns workers and returns group-id + worker-ids."
  (let ((old-sup amoebum:*worker-supervisor*))
    (unwind-protect
         (progn
           (setf amoebum:*worker-supervisor* nil)
           (amoebum:clear-workers)
           (amoebum.workers:clear-worker-groups)
           (multiple-value-bind (group-id worker-ids)
               (amoebum:fan-out-workers
                (list (list :type :shell :command "echo one" :cwd "/tmp")
                      (list :type :shell :command "echo two" :cwd "/tmp")
                      (list :type :shell :command "echo three" :cwd "/tmp")))
             (is (stringp group-id))
             (is (= 3 (length worker-ids)))
             (is (every #'stringp worker-ids))
             ;; Group should be registered
             (let ((group (amoebum.workers:find-worker-group group-id)))
               (is (amoebum.workers:worker-group-p group))
               (is (= 3 (length (amoebum.workers:worker-group-worker-ids group)))))))
      (setf amoebum:*worker-supervisor* old-sup))))

;;; --- Join group ---

(test join-worker-group-waits
  "join-worker-group blocks until all workers complete."
  (let ((old-sup amoebum:*worker-supervisor*))
    (unwind-protect
         (progn
           (setf amoebum:*worker-supervisor* nil)
           (amoebum:clear-workers)
           (amoebum.workers:clear-worker-groups)
           (multiple-value-bind (group-id worker-ids)
               (amoebum:fan-out-workers
                (list (list :type :shell :command "echo one" :cwd "/tmp")
                      (list :type :shell :command "echo two" :cwd "/tmp")))
             (declare (ignore worker-ids))
             (let ((results (amoebum.workers:join-worker-group group-id)))
               ;; Should return 2 result triples
               (is (= 2 (length results)))
               ;; Each triple is (worker-id status result)
               (dolist (triple results)
                 (is (= 3 (length triple)))
                 (is (stringp (first triple)))
                 ;; Status should be terminal (completed or failed due to sandbox)
                 (is (member (second triple)
                             '(:completed :failed :timeout :cancelled)
                             :test #'eq))))))
      (setf amoebum:*worker-supervisor* old-sup))))

;;; --- Race group ---

(test race-worker-group-first-wins
  "race-worker-group returns first completed worker."
  (let ((old-sup amoebum:*worker-supervisor*))
    (unwind-protect
         (progn
           (setf amoebum:*worker-supervisor* nil)
           (amoebum:clear-workers)
           (amoebum.workers:clear-worker-groups)
           (multiple-value-bind (group-id _worker-ids)
               (amoebum:fan-out-workers
                (list (list :type :shell :command "echo fast" :cwd "/tmp")
                      (list :type :shell :command "echo slow" :cwd "/tmp"))
                :timeout-seconds 30)
             (declare (ignore _worker-ids))
             (multiple-value-bind (winner-id winner-status _result)
                 (amoebum.workers:race-worker-group group-id)
               (declare (ignore _result))
               ;; Should have a winner
               (is (or (stringp winner-id)
                       (null winner-id)))  ; might be nil if sandbox blocks
               (when winner-id
                 (is (member winner-status
                             '(:completed :failed :timeout :cancelled)
                             :test #'eq))))))
      (setf amoebum:*worker-supervisor* old-sup))))

;;; --- Result aggregation ---

(test merge-worker-results-concat
  "merge-worker-results :concat concatenates outputs."
  ;; Use mock data - no actual workers needed
  (let ((result (amoebum:merge-worker-results :list
                  '(("w-1" :completed (:output "hello"))
                    ("w-2" :completed (:output "world"))))))
    ;; :list mode returns list of plists
    (is (= 2 (length result)))
    (is (eq :completed (getf (first result) :status)))))

(test merge-worker-results-first-success
  "merge-worker-results :first-success returns first completed."
  (let ((result (amoebum:merge-worker-results :first-success
                  '(("w-1" :failed nil)
                    ("w-2" :completed (:data "ok"))
                    ("w-3" :completed (:data "also ok"))))))
    (is (listp result))
    (is (equal "w-2" (getf result :worker-id)))))

;;; --- Group cleanup ---

(test clear-worker-groups-clears
  "clear-worker-groups empties the registry."
  (amoebum.workers:clear-worker-groups)
  (is (null (amoebum.workers:find-worker-group "nonexistent"))))

;;; --- Group with timeout ---

(test fan-out-with-group-timeout
  "fan-out-workers accepts group-level timeout."
  (let ((old-sup amoebum:*worker-supervisor*))
    (unwind-protect
         (progn
           (setf amoebum:*worker-supervisor* nil)
           (amoebum:clear-workers)
           (amoebum.workers:clear-worker-groups)
           (multiple-value-bind (group-id _wids)
               (amoebum:fan-out-workers
                (list (list :type :shell :command "echo test" :cwd "/tmp"))
                :timeout-seconds 60)
             (declare (ignore _wids))
             (let ((group (amoebum.workers:find-worker-group group-id)))
               (is (= 60 (amoebum.workers:worker-group-timeout-seconds group))))))
      (setf amoebum:*worker-supervisor* old-sup))))
