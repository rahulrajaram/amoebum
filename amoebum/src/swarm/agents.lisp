(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; SW4RM Integration Glue (I83-I93)
;;;
;;; Connects the SW4RM SDK to the amoebum application layer.
;;; - spawn-swarm-agent: create a sub-agent with its own state machine
;;; - collect-result: gather output from a completed agent
;;; - kill-agent: terminate a running agent
;;; ---------------------------------------------------------------------------

;;; --- Swarm Agent Record ---

(defstruct (swarm-agent
            (:constructor make-swarm-agent
                (&key id task (status :initializing)
                      (created-at (get-universal-time))
                      state-machine result thread error-message backing-agent
                      worktree
                      worktree-merge
                      signal-name
                      (retry-count 0)
                      retry-policy
                      timeout-seconds
                      heartbeat-at
                      last-output-at)))
  (id "" :type string)
  (task "" :type string)
  (status :initializing :type keyword)
  (created-at (get-universal-time) :type integer)
  (finished-at nil :type (or null integer))
  (state-machine nil)
  (result nil)
  (thread nil)
  (error-message nil :type (or null string))
  (backing-agent nil)
  (worktree nil)
  (worktree-merge nil)
  ;; NXT-017: Signal tracking and retry semantics
  (signal-name nil :type (or null string))
  (retry-count 0 :type integer)
  (retry-policy nil)
  (timeout-seconds nil :type (or null number))
  ;; NXT-018: Stalled-run detection
  (heartbeat-at nil :type (or null integer))
  (last-output-at nil :type (or null integer)))

(defvar *swarm-registry* (make-hash-table :test #'equal)
  "Hash-table of id -> swarm-agent.")

(defvar *swarm-counter* 0)

(defun %next-swarm-id ()
  (format nil "swarm-~A" (incf *swarm-counter*)))

(defun %swarm-transition-safe (agent target-state &key metadata)
  (let ((state-machine (swarm-agent-state-machine agent)))
    (when state-machine
      (handler-case
          (sw4rm-sdk::transition-to state-machine target-state :metadata metadata)
        (error ()
          nil)))))

(defun %swarm-status-metadata (status &key timeout-seconds error-message)
  (let ((metadata (list :agent-status status)))
    (when error-message
      (setf metadata (append metadata (list :error-message error-message))))
    (case status
      (:queued
       (append metadata (list :ack-stage sw4rm-sdk:+received+)))
      (:running
       (append metadata (list :ack-stage sw4rm-sdk:+read+)))
      (:cancelling
       (append metadata (list :ack-stage sw4rm-sdk:+rejected+
                              :error-code sw4rm-sdk:+forced-preemption+
                              :cancelled t)))
      (:completed
       (append metadata (list :ack-stage sw4rm-sdk:+fulfilled+)))
      (:failed
       (append metadata (list :ack-stage sw4rm-sdk:+failed+
                              :error-code sw4rm-sdk:+internal-error+)))
      (:cancelled
       (append metadata (list :ack-stage sw4rm-sdk:+rejected+
                              :error-code sw4rm-sdk:+forced-preemption+
                              :cancelled t)))
      (:timeout
       (append metadata (list :ack-stage sw4rm-sdk:+timed-out+
                              :error-code sw4rm-sdk:+tool-timeout+
                              :timed-out t
                              :timeout-seconds timeout-seconds)))
      (otherwise
       metadata))))

(defun %swarm-mark-running (agent)
  (let ((backing-agent (swarm-agent-backing-agent agent)))
    (when backing-agent
      (setf (agent-record-status backing-agent) :running
            (agent-record-started-ms backing-agent) (%agent-now-ms))))
  (setf (swarm-agent-status agent) :running)
  (%swarm-transition-safe agent :scheduled
                          :metadata (%swarm-status-metadata :queued))
  (%swarm-transition-safe agent :running
                          :metadata (%swarm-status-metadata :running))
  agent)

(defun %swarm-mark-cancelling (agent)
  (let ((backing-agent (swarm-agent-backing-agent agent)))
    (when backing-agent
      (setf (agent-record-cancel-requested-p backing-agent) t
            (agent-record-status backing-agent) :cancelling)))
  (%swarm-transition-safe agent :shutting-down
                          :metadata (%swarm-status-metadata :cancelling))
  agent)

(defun %swarm-mark-terminal (agent status result error-message
                             &key timeout-seconds stdout stderr signal-name)
  (let ((backing-agent (swarm-agent-backing-agent agent))
        (metadata (%swarm-status-metadata status
                                          :timeout-seconds timeout-seconds
                                          :error-message error-message))
        (finished-ms (%agent-now-ms))
        (worktree-merge (%maybe-finalize-runtime-worktree
                         (swarm-agent-worktree agent)
                         status
                         :agent-id (swarm-agent-id agent)
                         :backend :swarm
                         :task (swarm-agent-task agent)
                         :result result)))
    (when backing-agent
      (setf (agent-record-status backing-agent) status
            (agent-record-finished-ms backing-agent) finished-ms
            (agent-record-result backing-agent) result
            (agent-record-stdout backing-agent) stdout
            (agent-record-stderr backing-agent) stderr
            (agent-record-error-message backing-agent) error-message
            (agent-record-worktree-merge backing-agent) worktree-merge))
    (when (and backing-agent (eq status :cancelled))
      (setf (agent-record-cancel-requested-p backing-agent) t))
    (setf (swarm-agent-status agent) status
          (swarm-agent-result agent) result
          (swarm-agent-error-message agent) error-message
          (swarm-agent-worktree-merge agent) worktree-merge
          (swarm-agent-finished-at agent) (get-universal-time))
    (when signal-name
      (setf (swarm-agent-signal-name agent) signal-name))
    (when timeout-seconds
      (setf (swarm-agent-timeout-seconds agent) timeout-seconds))
    (case status
      (:completed
       (%swarm-transition-safe agent :completed :metadata metadata))
      (:failed
       (%swarm-transition-safe agent :failed :metadata metadata))
      (:cancelled
       (%swarm-transition-safe agent :shutting-down :metadata metadata)
       (%swarm-transition-safe agent :failed :metadata metadata))
      (:timeout
       (%swarm-transition-safe agent :shutting-down :metadata metadata)
       (%swarm-transition-safe agent :failed :metadata metadata)))
    agent))

(defun %swarm-runner-finished-status (runner-agent)
  (if (agent-record-cancel-requested-p runner-agent)
      :cancelled
      :completed))

;;; --- Lifecycle ---

(defun spawn-swarm-agent (task &key
                               (id nil)
                               (event-bus (current-event-bus))
                               runner
                               timeout-seconds
                               worktree
                               (retry-count 0)
                               retry-policy)
  "Spawn a new swarm sub-agent for TASK. Returns a swarm-agent.
NXT-017: Accepts RETRY-COUNT (number of prior retries) and RETRY-POLICY
\(plist with :max-retries, :backoff-strategy, :backoff-base-seconds).
NXT-018: Records heartbeat-at and last-output-at timestamps on the agent struct."
  (let* ((agent-id (or id (%next-swarm-id)))
         (spawn-time (get-universal-time))
         (worktree-runtime-factory *delegated-worktree-runtime-factory*)
         (worktree-metadata (coerce-worktree-metadata :worktree worktree))
         (runner-agent (%make-agent-record
                        :id agent-id
                        :type :swarm
                        :task task
                        :status :queued
                        :created-ms (%agent-now-ms)
                        :worktree worktree-metadata))
         (agent (make-swarm-agent :id agent-id
                                  :task task
                                  :status :initializing
                                  :backing-agent runner-agent
                                  :worktree worktree-metadata
                                  :retry-count retry-count
                                  :retry-policy retry-policy
                                  :timeout-seconds timeout-seconds
                                  :heartbeat-at spawn-time
                                  :last-output-at spawn-time))
         (agent-runner (or runner #'%default-agent-runner)))
    (setf (gethash agent-id *swarm-registry*) agent)
    (handler-case
        (let ((sm (make-instance 'sw4rm-sdk::agent-state-machine)))
          (setf (swarm-agent-state-machine agent) sm)
          (%swarm-transition-safe agent :runnable
                                  :metadata (%swarm-status-metadata :queued)))
      (error () nil))
    (publish event-bus
             (make-event :type +event-type-agent-spawn+
                         :source "swarm"
                         :payload (append (list :id agent-id
                                                :task task)
                                          (let ((metadata
                                                  (worktree-metadata-plist
                                                   worktree-metadata)))
                                            (when metadata
                                              (list :worktree metadata))))))
    (setf (swarm-agent-thread agent)
          (bt:make-thread
           (lambda ()
             (let ((*delegated-worktree-runtime-factory*
                     worktree-runtime-factory))
               (let ((stdout-stream (make-string-output-stream))
                     (stderr-stream (make-string-output-stream))
                     (result nil)
                     (status :completed)
                     (error-message nil)
                     (terminal-signal-name nil))
                 (setf (swarm-agent-heartbeat-at agent) (get-universal-time))
                 (%swarm-mark-running agent)
                 (handler-case
                     (with-delegated-agent-worktree-context
                         (:agent-id (swarm-agent-id agent)
                          :backend :swarm
                          :worktree (swarm-agent-worktree agent))
                       (let ((*standard-output* stdout-stream)
                             (*error-output* stderr-stream))
                         (setf result
                               #+sbcl
                               (if (and timeout-seconds
                                        (numberp timeout-seconds)
                                        (> timeout-seconds 0))
                                   (sb-ext:with-timeout timeout-seconds
                                     (funcall agent-runner runner-agent))
                                   (funcall agent-runner runner-agent))
                               #-sbcl
                               (funcall agent-runner runner-agent)
                               status (%swarm-runner-finished-status runner-agent))))
                   (agent-cancelled (condition)
                     (setf status :cancelled
                           terminal-signal-name "SIGTERM"
                           error-message (princ-to-string condition)))
                   #+sbcl
                   (sb-ext:timeout (_condition)
                     (setf status :timeout
                           terminal-signal-name "SIGALRM"
                           error-message (format nil
                                                 "Swarm agent ~A timed out after ~A seconds."
                                                 agent-id
                                                 timeout-seconds)))
                   (error (condition)
                     (if (agent-record-cancel-requested-p runner-agent)
                         (setf status :cancelled
                               terminal-signal-name "SIGTERM"
                               error-message (princ-to-string condition))
                         (setf status :failed
                               error-message (princ-to-string condition)))))
                 (let ((stdout-text (get-output-stream-string stdout-stream))
                       (stderr-text (get-output-stream-string stderr-stream)))
                   (when (or (plusp (length stdout-text))
                             (plusp (length stderr-text)))
                     (setf (swarm-agent-last-output-at agent) (get-universal-time)))
                   (%swarm-mark-terminal agent status result error-message
                                         :timeout-seconds timeout-seconds
                                         :signal-name terminal-signal-name
                                         :stdout stdout-text
                                         :stderr stderr-text))
                 (publish event-bus
                          (make-event :type (if (eq status :cancelled)
                                                +event-type-agent-cancelled+
                                                +event-type-agent-complete+)
                                      :source "swarm"
                                      :payload (append
                                                (list :id agent-id
                                                      :status status
                                                      :task task
                                                      :signal-name terminal-signal-name
                                                      :retry-count
                                                      (swarm-agent-retry-count agent)
                                                      :error-message error-message)
                                                (let ((metadata
                                                        (worktree-metadata-plist
                                                         (swarm-agent-worktree agent))))
                                                  (when metadata
                                                    (list :worktree metadata)))))))))
           :name (format nil "swarm-~A" agent-id)))
    agent))

(defun collect-swarm-result (agent-id &key (timeout-seconds 60))
  "Wait for a swarm agent to complete and return its result."
  (declare (ignore timeout-seconds))
  (let ((agent (gethash agent-id *swarm-registry*)))
    (unless agent
      (error "Swarm agent ~A not found." agent-id))
    (let ((thread (swarm-agent-thread agent)))
      (when (and thread (bt:thread-alive-p thread))
        (bt:join-thread thread)))
    (values (swarm-agent-result agent)
            (swarm-agent-status agent))))

(defun kill-swarm-agent (agent-id &key (event-bus (current-event-bus)) (signal-name "SIGKILL"))
  "Terminate a running swarm agent.
NXT-017: SIGNAL-NAME records which signal caused termination (default SIGKILL)."
  (let ((agent (gethash agent-id *swarm-registry*)))
    (unless agent
      (error "Swarm agent ~A not found." agent-id))
    (let ((thread (swarm-agent-thread agent)))
      (when (member (swarm-agent-status agent) '(:initializing :running) :test #'eq)
        (%swarm-mark-cancelling agent))
      (when (and thread (bt:thread-alive-p thread))
        (bt:join-thread thread)))
    (unless (member (swarm-agent-status agent) '(:completed :failed :cancelled :timeout) :test #'eq)
      (%swarm-mark-terminal agent :cancelled nil "Swarm agent cancelled."
                            :signal-name signal-name)
      (publish event-bus
               (make-event :type +event-type-agent-cancelled+
                           :source "swarm"
                           :payload (append (list :id agent-id
                                                  :status :cancelled
                                                  :signal-name signal-name)
                                             (let ((metadata (worktree-metadata-plist
                                                              (swarm-agent-worktree agent))))
                                               (when metadata
                                                 (list :worktree metadata)))))))
    agent))

;;; --- Registry Queries ---

(defun list-swarm-agents (&key (status nil))
  "List all swarm agents, optionally filtered by STATUS."
  (let ((result '()))
    (maphash (lambda (id agent)
               (declare (ignore id))
               (when (or (null status)
                         (eq status (swarm-agent-status agent)))
                 (push agent result)))
             *swarm-registry*)
    (sort result #'> :key #'swarm-agent-created-at)))

(defun find-swarm-agent (agent-id)
  "Find a swarm agent by ID."
  (gethash agent-id *swarm-registry*))

(defun clear-swarm-registry ()
  "Clear all swarm agents."
  (clrhash *swarm-registry*)
  (setf *swarm-counter* 0))

(defun swarm-status-summary ()
  "Return a summary string of the swarm state."
  (let ((agents (list-swarm-agents)))
    (with-output-to-string (out)
      (format out "Swarm: ~A agents~%" (length agents))
      (dolist (a agents)
        (format out "  ~A [~A] ~A~%"
                (swarm-agent-id a)
                (swarm-agent-status a)
                (if (> (length (swarm-agent-task a)) 50)
                    (subseq (swarm-agent-task a) 0 50)
                    (swarm-agent-task a)))))))

;;; ---------------------------------------------------------------------------
;;; NXT-018: Stalled-run detection via heartbeat and last-output timestamps
;;; ---------------------------------------------------------------------------

(defun update-swarm-agent-heartbeat (agent-id)
  "Refresh the heartbeat timestamp for AGENT-ID to the current time.
Called by long-running runners periodically to signal liveness.
Returns T if the agent was found and updated, NIL otherwise."
  (let ((agent (gethash agent-id *swarm-registry*)))
    (when agent
      (setf (swarm-agent-heartbeat-at agent) (get-universal-time))
      t)))

(defun update-swarm-agent-last-output (agent-id)
  "Refresh the last-output-at timestamp for AGENT-ID to the current time.
Call when new stdout/stderr output is produced during execution.
Returns T if the agent was found and updated, NIL otherwise."
  (let ((agent (gethash agent-id *swarm-registry*)))
    (when agent
      (setf (swarm-agent-last-output-at agent) (get-universal-time))
      t)))

(defun %swarm-agent-stalled-p (agent heartbeat-threshold-seconds output-threshold-seconds now)
  "Return T if AGENT appears stalled.
An agent is stalled when it is in a non-terminal status AND either:
  - its heartbeat-at is older than HEARTBEAT-THRESHOLD-SECONDS, or
  - its last-output-at is older than OUTPUT-THRESHOLD-SECONDS (when non-nil threshold)."
  (let ((status (swarm-agent-status agent)))
    (when (member status '(:initializing :running) :test #'eq)
      (let ((heartbeat (swarm-agent-heartbeat-at agent))
            (last-out (swarm-agent-last-output-at agent)))
        (or (and heartbeat-threshold-seconds
                 heartbeat
                 (> (- now heartbeat) heartbeat-threshold-seconds))
            (and output-threshold-seconds
                 last-out
                 (> (- now last-out) output-threshold-seconds)))))))

(defun detect-stalled-agents (&key
                                (heartbeat-threshold-seconds 60)
                                (output-threshold-seconds nil)
                                (status nil))
  "Return a list of swarm agents that appear stalled.
An agent is considered stalled if it is non-terminal and either:
  - HEARTBEAT-THRESHOLD-SECONDS have elapsed since its last heartbeat, or
  - OUTPUT-THRESHOLD-SECONDS have elapsed since its last output (if provided).

Optionally filter by STATUS (e.g. :running).

Each returned element is a plist:
  (:agent <swarm-agent> :id <string> :status <keyword>
   :seconds-since-heartbeat <number-or-nil>
   :seconds-since-output <number-or-nil>)"
  (let ((now (get-universal-time))
        (stalled '()))
    (maphash (lambda (_id agent)
               (declare (ignore _id))
               (when (or (null status)
                         (eq status (swarm-agent-status agent)))
                 (when (%swarm-agent-stalled-p agent
                                               heartbeat-threshold-seconds
                                               output-threshold-seconds
                                               now)
                   (let* ((heartbeat (swarm-agent-heartbeat-at agent))
                          (last-out (swarm-agent-last-output-at agent))
                          (secs-hb (and heartbeat (- now heartbeat)))
                          (secs-out (and last-out (- now last-out))))
                     (push (list :agent agent
                                 :id (swarm-agent-id agent)
                                 :status (swarm-agent-status agent)
                                 :seconds-since-heartbeat secs-hb
                                 :seconds-since-output secs-out)
                           stalled)))))
             *swarm-registry*)
    (sort stalled #'> :key (lambda (entry)
                              (or (getf entry :seconds-since-heartbeat) 0)))))
