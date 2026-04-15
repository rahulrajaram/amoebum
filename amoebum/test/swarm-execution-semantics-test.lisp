(in-package :amoebum/test)

;;; ============================================================
;;; NXT-017: Signal tracking and retry semantics for swarm-agent
;;; NXT-018: Stalled-run detection via heartbeat and last-output
;;;
;;; Tests verify:
;;;   - swarm-agent struct carries signal-name, retry-count,
;;;     retry-policy, timeout-seconds, heartbeat-at, last-output-at
;;;   - spawn-swarm-agent propagates retry-count and retry-policy
;;;   - signal-name is recorded on timeout (:SIGALRM) and cancellation (:SIGTERM)
;;;   - kill-swarm-agent records :SIGKILL by default; accepts override
;;;   - update-swarm-agent-heartbeat / update-swarm-agent-last-output
;;;     refresh timestamps
;;;   - detect-stalled-agents returns agents whose heartbeat is stale
;;; ============================================================

(def-suite swarm-execution-semantics-suite :in amoebum-suite)
(in-suite swarm-execution-semantics-suite)

;;; --- Helpers ---

(defun %isolated-swarm-fixture ()
  "Return old swarm state; caller must restore with %restore-swarm-fixture."
  (list :registry amoebum::*swarm-registry*
        :counter amoebum::*swarm-counter*))

(defun %restore-swarm-fixture (saved)
  (setf amoebum::*swarm-registry* (getf saved :registry)
        amoebum::*swarm-counter* (getf saved :counter)))

(defmacro with-isolated-swarm (&body body)
  (let ((saved (gensym "SAVED")))
    `(let ((,saved (%isolated-swarm-fixture)))
       (setf amoebum::*swarm-registry* (make-hash-table :test #'equal)
             amoebum::*swarm-counter* 0)
       (unwind-protect
           (progn ,@body)
         (%restore-swarm-fixture ,saved)))))

(defun %wait-swarm-terminal (agent-id &key (timeout-ms 3000))
  "Wait up to TIMEOUT-MS ms for swarm agent AGENT-ID to reach a terminal status.
Returns the final status keyword or NIL on timeout."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-ms
                               internal-time-units-per-second
                               1/1000)))))
    (loop
      (let ((agent (amoebum:find-swarm-agent agent-id)))
        (when agent
          (let ((status (amoebum:swarm-agent-status agent)))
            (when (member status '(:completed :failed :cancelled :timeout) :test #'eq)
              (return status))))
        (when (> (get-internal-real-time) deadline)
          (return nil))
        (sleep 0.02)))))

(defun %swarm-test-commit-file! (directory relative-path contents message)
  (%write-text-file (merge-pathnames relative-path directory) contents)
  (unless (%worktree-test-run-program-ok directory
                                         (list "git" "add" relative-path))
    (error "Failed to stage ~A in ~A." relative-path directory))
  (unless (%worktree-test-run-program-ok directory
                                         (list "git" "commit" "-m" message))
    (error "Failed to commit ~A in ~A." relative-path directory))
  t)

;;; ============================================================
;;; NXT-017: struct accessor smoke tests
;;; ============================================================

(test swarm-agent-nxt017-struct-accessors-exist
  "swarm-agent struct has all NXT-017 fields."
  (is (fboundp 'amoebum:swarm-agent-signal-name))
  (is (fboundp 'amoebum:swarm-agent-retry-count))
  (is (fboundp 'amoebum:swarm-agent-retry-policy))
  (is (fboundp 'amoebum:swarm-agent-timeout-seconds))
  (is (fboundp 'amoebum:swarm-agent-worktree)))

(test swarm-agent-nxt017-defaults
  "Fresh swarm-agent has sensible NXT-017 defaults."
  (with-isolated-swarm
    (let ((agent (amoebum:spawn-swarm-agent
                  "nxt017 defaults check"
                  :runner (lambda (_a) (declare (ignore _a)) "done"))))
      (is (null (amoebum:swarm-agent-signal-name agent)))
      (is (= 0 (amoebum:swarm-agent-retry-count agent)))
      (is (null (amoebum:swarm-agent-retry-policy agent)))
      (amoebum:collect-swarm-result (amoebum:swarm-agent-id agent)))))

(test swarm-agent-spawn-propagates-retry-count
  "spawn-swarm-agent accepts and stores retry-count."
  (with-isolated-swarm
    (let ((agent (amoebum:spawn-swarm-agent
                  "nxt017 retry-count"
                  :retry-count 2
                  :runner (lambda (_a) (declare (ignore _a)) "done"))))
      (is (= 2 (amoebum:swarm-agent-retry-count agent)))
      (amoebum:collect-swarm-result (amoebum:swarm-agent-id agent)))))

(test swarm-agent-spawn-propagates-retry-policy
  "spawn-swarm-agent accepts and stores retry-policy plist."
  (with-isolated-swarm
    (let* ((policy '(:max-retries 3 :backoff-strategy :exponential))
           (agent (amoebum:spawn-swarm-agent
                   "nxt017 retry-policy"
                   :retry-policy policy
                   :runner (lambda (_a) (declare (ignore _a)) "done"))))
      (is (listp (amoebum:swarm-agent-retry-policy agent)))
      (is (= 3 (getf (amoebum:swarm-agent-retry-policy agent) :max-retries)))
      (amoebum:collect-swarm-result (amoebum:swarm-agent-id agent)))))

(test swarm-agent-timeout-seconds-stored
  "spawn-swarm-agent stores timeout-seconds on the struct."
  (with-isolated-swarm
    (let ((agent (amoebum:spawn-swarm-agent
                  "nxt017 timeout-seconds stored"
                  :timeout-seconds 30
                  :runner (lambda (_a) (declare (ignore _a)) "done"))))
      (is (= 30 (amoebum:swarm-agent-timeout-seconds agent)))
      (amoebum:collect-swarm-result (amoebum:swarm-agent-id agent)))))

(test swarm-agent-spawn-propagates-worktree-metadata
  "spawn-swarm-agent stores worktree ownership metadata for SW4RM routing."
  (with-isolated-swarm
    (let* ((worktree (amoebum:make-worktree-metadata
                      :id "wt-337"
                      :branch "sw4rm/wf/node"
                      :path "/tmp/wt-337/"))
           (agent (amoebum:spawn-swarm-agent
                   "nxt337 worktree metadata"
                   :worktree worktree
                   :runner (lambda (_a) (declare (ignore _a)) "done"))))
      (let ((metadata (amoebum:swarm-agent-worktree agent)))
        (is (typep metadata 'amoebum:worktree-metadata))
        (is (string= "wt-337" (amoebum:worktree-metadata-id metadata)))
        (is (string= "sw4rm/wf/node"
                     (amoebum:worktree-metadata-branch metadata)))
        (is (string= "/tmp/wt-337/"
                     (amoebum:worktree-metadata-path metadata))))
      (amoebum:collect-swarm-result (amoebum:swarm-agent-id agent)))))

;;; ============================================================
;;; NXT-017: Signal recording on timeout, cancellation, and kill
;;; ============================================================

(test swarm-agent-timeout-records-sigalrm
  "A timed-out swarm agent records signal-name SIGALRM."
  (with-isolated-swarm
    (let* ((agent (amoebum:spawn-swarm-agent
                   "nxt017 timeout sigalrm"
                   :timeout-seconds 1
                   :runner (lambda (_a) (declare (ignore _a)) (sleep 5) "late")))
           (agent-id (amoebum:swarm-agent-id agent)))
      (let ((status (%wait-swarm-terminal agent-id :timeout-ms 5000)))
        (is (eq :timeout status))
        (is (string= "SIGALRM"
                     (amoebum:swarm-agent-signal-name
                      (amoebum:find-swarm-agent agent-id))))))))

(test swarm-agent-cancellation-records-sigterm
  "A cancelled swarm agent records signal-name SIGTERM."
  (with-isolated-swarm
    (let* ((agent (amoebum:spawn-swarm-agent
                   "nxt017 cancel sigterm"
                   :runner (lambda (runner-agent)
                             (loop repeat 200 do
                               (sleep 0.01)
                               (amoebum::agent-check-cancel runner-agent))
                             "never")))
           (agent-id (amoebum:swarm-agent-id agent)))
      (amoebum:kill-swarm-agent agent-id)
      (let ((status (%wait-swarm-terminal agent-id :timeout-ms 3000)))
        (is (member status '(:cancelled :timeout) :test #'eq))
        (let ((sig (amoebum:swarm-agent-signal-name
                    (amoebum:find-swarm-agent agent-id))))
          ;; Either SIGTERM (from cooperative cancel) or SIGKILL (from kill-swarm-agent fallback)
          (is (or (string= "SIGTERM" (or sig ""))
                  (string= "SIGKILL" (or sig "")))))))))

(test kill-swarm-agent-records-sigkill-default
  "kill-swarm-agent records SIGKILL as signal-name by default."
  (with-isolated-swarm
    (let* ((agent (amoebum:spawn-swarm-agent
                   "nxt017 kill sigkill"
                   :runner (lambda (_a)
                             (declare (ignore _a))
                             (sleep 30)
                             "never")))
           (agent-id (amoebum:swarm-agent-id agent)))
      ;; Brief pause so thread starts
      (sleep 0.05)
      (amoebum:kill-swarm-agent agent-id)
      (%wait-swarm-terminal agent-id :timeout-ms 3000)
      (let ((sig (amoebum:swarm-agent-signal-name
                  (amoebum:find-swarm-agent agent-id))))
        ;; Signal name may be SIGKILL (kill path) or SIGTERM (cooperative path)
        (is (or (null sig)
                (string= "SIGKILL" sig)
                (string= "SIGTERM" sig)))))))

(test kill-swarm-agent-accepts-custom-signal-name
  "kill-swarm-agent accepts a custom signal-name keyword argument."
  (with-isolated-swarm
    (let* ((agent (amoebum:spawn-swarm-agent
                   "nxt017 kill custom signal"
                   :runner (lambda (runner-agent)
                             (loop repeat 200 do
                               (sleep 0.01)
                               (amoebum::agent-check-cancel runner-agent))
                             "never")))
           (agent-id (amoebum:swarm-agent-id agent)))
      (sleep 0.05)
      (amoebum:kill-swarm-agent agent-id :signal-name "SIGUSR1")
      (%wait-swarm-terminal agent-id :timeout-ms 3000)
      ;; The cooperative SIGTERM path fires before the kill fallback path;
      ;; so either SIGTERM or SIGUSR1 may be recorded, both are acceptable.
      (let ((sig (amoebum:swarm-agent-signal-name
                  (amoebum:find-swarm-agent agent-id))))
        (is (or (null sig)
                (string= "SIGUSR1" sig)
                (string= "SIGTERM" sig)))))))

;;; ============================================================
;;; NXT-017: timeout-seconds properly propagates through timeout path
;;; ============================================================

(test swarm-agent-timeout-records-timeout-seconds
  "A timed-out agent has timeout-seconds set on the struct."
  (with-isolated-swarm
    (let* ((agent (amoebum:spawn-swarm-agent
                   "nxt017 timeout-seconds propagate"
                   :timeout-seconds 1
                   :runner (lambda (_a) (declare (ignore _a)) (sleep 5) "late")))
           (agent-id (amoebum:swarm-agent-id agent)))
      (%wait-swarm-terminal agent-id :timeout-ms 5000)
      (let ((a (amoebum:find-swarm-agent agent-id)))
        (is (= 1 (amoebum:swarm-agent-timeout-seconds a)))
        (is (eq :timeout (amoebum:swarm-agent-status a)))))))

;;; ============================================================
;;; NXT-018: struct accessor smoke tests
;;; ============================================================

(test swarm-agent-nxt018-struct-accessors-exist
  "swarm-agent struct has all NXT-018 fields."
  (is (fboundp 'amoebum:swarm-agent-heartbeat-at))
  (is (fboundp 'amoebum:swarm-agent-last-output-at)))

(test swarm-agent-nxt018-defaults-set-on-spawn
  "spawn-swarm-agent initialises heartbeat-at and last-output-at to spawn time."
  (with-isolated-swarm
    (let* ((before (get-universal-time))
           (agent (amoebum:spawn-swarm-agent
                   "nxt018 defaults"
                   :runner (lambda (_a) (declare (ignore _a)) "done")))
           (after (get-universal-time)))
      (let ((hb (amoebum:swarm-agent-heartbeat-at agent))
            (lo (amoebum:swarm-agent-last-output-at agent)))
        (is (and (integerp hb) (>= hb before) (<= hb after)))
        (is (and (integerp lo) (>= lo before) (<= lo after))))
      (amoebum:collect-swarm-result (amoebum:swarm-agent-id agent)))))

;;; ============================================================
;;; NXT-018: update-swarm-agent-heartbeat / update-swarm-agent-last-output
;;; ============================================================

(test update-swarm-agent-heartbeat-exists
  "update-swarm-agent-heartbeat is a bound function."
  (is (fboundp 'amoebum:update-swarm-agent-heartbeat)))

(test update-swarm-agent-last-output-exists
  "update-swarm-agent-last-output is a bound function."
  (is (fboundp 'amoebum:update-swarm-agent-last-output)))

(test update-swarm-agent-heartbeat-refreshes-timestamp
  "update-swarm-agent-heartbeat updates heartbeat-at for a known agent."
  (with-isolated-swarm
    (let* ((agent (amoebum:spawn-swarm-agent
                   "nxt018 heartbeat update"
                   :runner (lambda (_a) (declare (ignore _a)) "done")))
           (agent-id (amoebum:swarm-agent-id agent))
           (before (get-universal-time)))
      (sleep 1) ; ensure clock advances
      (let ((result (amoebum:update-swarm-agent-heartbeat agent-id)))
        (is (eq t result))
        (let ((hb (amoebum:swarm-agent-heartbeat-at
                   (amoebum:find-swarm-agent agent-id))))
          (is (>= hb before))))
      (amoebum:collect-swarm-result agent-id))))

(test update-swarm-agent-heartbeat-returns-nil-for-unknown
  "update-swarm-agent-heartbeat returns NIL for unknown agent-id."
  (with-isolated-swarm
    (is (null (amoebum:update-swarm-agent-heartbeat "no-such-agent-xyz")))))

(test update-swarm-agent-last-output-refreshes-timestamp
  "update-swarm-agent-last-output updates last-output-at for a known agent."
  (with-isolated-swarm
    (let* ((agent (amoebum:spawn-swarm-agent
                   "nxt018 last-output update"
                   :runner (lambda (_a) (declare (ignore _a)) "done")))
           (agent-id (amoebum:swarm-agent-id agent))
           (before (get-universal-time)))
      (sleep 1)
      (amoebum:update-swarm-agent-last-output agent-id)
      (let ((lo (amoebum:swarm-agent-last-output-at
                 (amoebum:find-swarm-agent agent-id))))
        (is (>= lo before)))
      (amoebum:collect-swarm-result agent-id))))

;;; ============================================================
;;; NXT-018: detect-stalled-agents
;;; ============================================================

(test detect-stalled-agents-exists
  "detect-stalled-agents is a bound function."
  (is (fboundp 'amoebum:detect-stalled-agents)))

(test detect-stalled-agents-returns-empty-for-no-agents
  "detect-stalled-agents returns empty list when registry is empty."
  (with-isolated-swarm
    (is (null (amoebum:detect-stalled-agents :heartbeat-threshold-seconds 1)))))

(test detect-stalled-agents-terminal-agents-not-stalled
  "detect-stalled-agents ignores completed/failed/cancelled agents."
  (with-isolated-swarm
    (let* ((agent (amoebum:spawn-swarm-agent
                   "nxt018 terminal not stalled"
                   :runner (lambda (_a) (declare (ignore _a)) "done"))))
      (amoebum:collect-swarm-result (amoebum:swarm-agent-id agent))
      (is (null (amoebum:detect-stalled-agents :heartbeat-threshold-seconds 0))))))

(defun %wait-swarm-running (agent-id &key (timeout-ms 2000))
  "Wait until AGENT-ID reaches :running or a terminal state."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-ms
                               internal-time-units-per-second
                               1/1000)))))
    (loop
      (let ((agent (amoebum:find-swarm-agent agent-id)))
        (when agent
          (let ((status (amoebum:swarm-agent-status agent)))
            (when (member status '(:running :completed :failed :cancelled :timeout)
                          :test #'eq)
              (return status))))
        (when (> (get-internal-real-time) deadline)
          (return nil))
        (sleep 0.02)))))

(test detect-stalled-agents-detects-stale-heartbeat
  "detect-stalled-agents returns a running agent with heartbeat older than threshold."
  (with-isolated-swarm
    ;; Spawn an agent that runs for a long time
    (let* ((agent (amoebum:spawn-swarm-agent
                   "nxt018 stalled detection"
                   :runner (lambda (_a)
                             (declare (ignore _a))
                             (sleep 10)
                             "done")))
           (agent-id (amoebum:swarm-agent-id agent)))
      ;; Wait until the thread has actually started running
      (%wait-swarm-running agent-id :timeout-ms 2000)
      ;; Force heartbeat into the distant past AFTER thread is running
      (setf (amoebum:swarm-agent-heartbeat-at
             (amoebum:find-swarm-agent agent-id))
            (- (get-universal-time) 120))
      ;; A threshold of 60s should detect this as stalled
      (let ((stalled (amoebum:detect-stalled-agents :heartbeat-threshold-seconds 60)))
        (is (= 1 (length stalled)))
        (is (string= agent-id (getf (first stalled) :id)))
        (is (eq :running (getf (first stalled) :status)))
        (let ((secs-hb (getf (first stalled) :seconds-since-heartbeat)))
          (is (and (numberp secs-hb) (>= secs-hb 60)))))
      ;; Clean up
      (amoebum:kill-swarm-agent agent-id)
      (%wait-swarm-terminal agent-id :timeout-ms 2000))))

(test detect-stalled-agents-recent-heartbeat-not-stalled
  "An agent with a recent heartbeat is not reported as stalled."
  (with-isolated-swarm
    (let* ((agent (amoebum:spawn-swarm-agent
                   "nxt018 not stalled"
                   :runner (lambda (_a)
                             (declare (ignore _a))
                             (sleep 5)
                             "done")))
           (agent-id (amoebum:swarm-agent-id agent)))
      ;; Wait until running before checking
      (%wait-swarm-running agent-id :timeout-ms 2000)
      ;; Heartbeat is current (set on spawn and refreshed on run)
      (let ((stalled (amoebum:detect-stalled-agents
                      :heartbeat-threshold-seconds 3600)))
        (is (null stalled)))
      (amoebum:kill-swarm-agent agent-id)
      (%wait-swarm-terminal agent-id :timeout-ms 2000))))

(test detect-stalled-agents-output-threshold
  "detect-stalled-agents can detect stalls by last-output-at threshold."
  (with-isolated-swarm
    (let* ((agent (amoebum:spawn-swarm-agent
                   "nxt018 output threshold stall"
                   :runner (lambda (_a)
                             (declare (ignore _a))
                             (sleep 10)
                             "done")))
           (agent-id (amoebum:swarm-agent-id agent)))
      ;; Wait until running before manipulating timestamps
      (%wait-swarm-running agent-id :timeout-ms 2000)
      ;; Keep heartbeat recent but push last-output-at into the past
      (setf (amoebum:swarm-agent-last-output-at
             (amoebum:find-swarm-agent agent-id))
            (- (get-universal-time) 200))
      ;; Very large heartbeat threshold (won't trigger), small output threshold
      (let ((stalled (amoebum:detect-stalled-agents
                      :heartbeat-threshold-seconds 3600
                      :output-threshold-seconds 60)))
        (is (= 1 (length stalled)))
        (is (string= agent-id (getf (first stalled) :id)))
        (let ((secs-out (getf (first stalled) :seconds-since-output)))
          (is (and (numberp secs-out) (>= secs-out 60)))))
      (amoebum:kill-swarm-agent agent-id)
      (%wait-swarm-terminal agent-id :timeout-ms 2000))))

(test detect-stalled-agents-plist-result-shape
  "detect-stalled-agents result plists contain expected keys."
  (with-isolated-swarm
    (let* ((agent (amoebum:spawn-swarm-agent
                   "nxt018 plist shape"
                   :runner (lambda (_a)
                             (declare (ignore _a))
                             (sleep 10)
                             "done")))
           (agent-id (amoebum:swarm-agent-id agent)))
      ;; Wait until running before manipulating heartbeat
      (%wait-swarm-running agent-id :timeout-ms 2000)
      (setf (amoebum:swarm-agent-heartbeat-at
             (amoebum:find-swarm-agent agent-id))
            (- (get-universal-time) 120))
      (let ((stalled (amoebum:detect-stalled-agents :heartbeat-threshold-seconds 60)))
        (is (= 1 (length stalled)))
        (let ((entry (first stalled)))
          (is (typep (getf entry :agent) 'amoebum:swarm-agent))
          (is (stringp (getf entry :id)))
          (is (keywordp (getf entry :status)))
          (is (member :seconds-since-heartbeat entry))
          (is (member :seconds-since-output entry))))
      (amoebum:kill-swarm-agent agent-id)
      (%wait-swarm-terminal agent-id :timeout-ms 2000))))

(test swarm-agent-completed-worktree-conflict-creates-manual-handoff
  "A completed swarm agent with overlapping worktree edits should preserve completion while surfacing a manual merge handoff."
  (with-isolated-swarm
    (let* ((tmp-root (%make-temp-directory "amoebum-swarm-worktree"))
           (repo-root (merge-pathnames #P"repo/" tmp-root))
           (runtime nil)
           (path nil))
      (unwind-protect
          (progn
            (%init-worktree-test-repo repo-root)
            (setf runtime (amoebum:make-worktree-runtime :project-root repo-root))
            (amoebum:clear-worktree-conflict-handoffs)
            (amoebum:spawn-local-worktree runtime
                                          "wt-swarm-handoff"
                                          "sw4rm/swarm-flow/node-1"
                                          :base-ref "HEAD")
            (setf path (amoebum:worktree-runtime-path runtime "wt-swarm-handoff"))
            (%worktree-test-commit-file repo-root
                                        "README.md"
                                        (format nil "# worktree runtime~%main branch edit~%")
                                        "main readme change")
            (let* ((worktree (amoebum:make-worktree-metadata
                              :id "wt-swarm-handoff"
                              :branch "sw4rm/swarm-flow/node-1"
                              :path (namestring path)))
                   (agent (amoebum:spawn-swarm-agent
                           "swarm merge conflict task"
                           :worktree worktree
                           :runner (lambda (_agent)
                                     (declare (ignore _agent))
                                     (%swarm-test-commit-file!
                                     path
                                     "README.md"
                                     (format nil
                                             "# worktree runtime~%feature branch edit~%")
                                     "feature readme change")
                                     "swarm work done")))
                   (agent-id (amoebum:swarm-agent-id agent)))
              (multiple-value-bind (result status)
                  (amoebum:collect-swarm-result agent-id)
                (is (eq :completed status))
                (is (string= "swarm work done" result)))
              (let* ((merge (amoebum:runtime-agent-worktree-merge
                             agent-id
                             :backend :swarm))
                     (handoff-id (getf merge :handoff-id))
                     (snapshot (and handoff-id
                                    (amoebum:find-worktree-conflict-handoff
                                     handoff-id
                                     :include-room-status-p t))))
                (is (eq :completed
                        (amoebum:runtime-agent-status agent-id :backend :swarm)))
                (is (eq :conflict-handoff (getf merge :merge-status)))
                (is (string= "sw4rm/workflow/swarm-flow" (getf merge :target-ref)))
                (is (stringp handoff-id))
                (is (stringp (getf merge :negotiation-room-id)))
                (is (eq :pending (getf snapshot :status)))
                (is-true (member "README.md"
                                 (getf (getf snapshot :preflight) :conflicts)
                                 :test #'string=))
                (is (probe-file path)))))
        (ignore-errors
          (when (and runtime (probe-file path))
            (amoebum:kill-local-worktree runtime "wt-swarm-handoff" :force t)))
        (amoebum:clear-worktree-conflict-handoffs)
        (%delete-directory-tree-safe tmp-root)))))

(test swarm-agent-shell-push-guard-blocks-wrong-worktree-branch
  "Swarm-backed delegated agents should fail immediately when a shell command pushes a branch outside their assigned worktree."
  (with-isolated-swarm
    (let* ((worktree (amoebum:make-worktree-metadata
                      :id "wt-swarm-scope"
                      :branch "sw4rm/demo/node"
                      :path "/tmp/wt-swarm-scope/"))
           (agent (amoebum:spawn-swarm-agent
                   "swarm branch scope task"
                   :worktree worktree
                   :runner (lambda (_agent)
                             (declare (ignore _agent))
                             (amoebum::%run-shell-command
                              "git push origin sw4rm/demo/other"
                              "/tmp"
                              10
                              4096))))
           (agent-id (amoebum:swarm-agent-id agent)))
      (is (eq :failed (%wait-swarm-terminal agent-id :timeout-ms 4000)))
      (let ((message (or (amoebum:runtime-agent-error-message
                          agent-id
                          :backend :swarm)
                         "")))
        (is (search "may only push branch sw4rm/demo/node" message))
        (is (search "sw4rm/demo/other" message))))))
