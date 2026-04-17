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
  (signal-name nil :type (or null string))   ; e.g. "SIGTERM", "SIGKILL"
  (retry-count 0 :type integer)              ; number of times this agent has been retried
  (retry-policy nil)                         ; plist: (:max-retries N :backoff-strategy :none/:linear/:exponential)
  (timeout-seconds nil :type (or null number)) ; stored for record-keeping
  ;; NXT-018: Stalled-run detection
  (heartbeat-at nil :type (or null integer)) ; universal-time of last heartbeat
  (last-output-at nil :type (or null integer))) ; universal-time of last output activity

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

(defun %swarm-mark-terminal (agent status result error-message &key timeout-seconds stdout stderr signal-name)
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
  ;; NXT-017: record signal name and effective timeout-seconds on the struct
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

;;; ---------------------------------------------------------------------------
;;; Inter-user coordination and delegation (I253)
;;; ---------------------------------------------------------------------------

(defvar *user-session-registry* (sw4rm-sdk:make-local-registry)
  "SW4RM local registry of active user sessions.")

(defvar *user-session->agent-id* (make-hash-table :test #'equal)
  "Map session-id -> SW4RM agent-id.")

(defvar *user-session->user-id* (make-hash-table :test #'equal)
  "Map session-id -> user-id.")

(defvar *user-agent->session-id* (make-hash-table :test #'equal)
  "Map SW4RM agent-id -> session-id.")

(defvar *user-negotiation-room-participants* (make-hash-table :test #'equal)
  "Map negotiation room-id -> list of participant session ids.")

(defvar *user-handoff-client* nil
  "Shared local SW4RM handoff client for inter-user delegation.")

(defvar *user-negotiation-client* nil
  "Shared local SW4RM negotiation-room client for user code-review rooms.")

(defvar *user-handoff-sequence* 0
  "Monotonic handoff sequence counter for generated request IDs.")

(defvar *user-coordination-lock* (bt:make-lock "amoebum-user-coordination-lock")
  "Lock protecting user coordination registry state.")

(defun %coordination-trim-string (value)
  (if (stringp value)
      (string-trim '(#\Space #\Tab #\Newline #\Return) value)
      (string-trim '(#\Space #\Tab #\Newline #\Return)
                   (princ-to-string value))))

(defun %coordination-require-string (value field-name)
  (let ((trimmed (%coordination-trim-string value)))
    (when (zerop (length trimmed))
      (error "~A must be a non-empty string." field-name))
    trimmed))

(defun %coordination-normalize-token (value)
  (let* ((trimmed (%coordination-require-string value "coordination token"))
         (text (string-downcase trimmed)))
    (coerce (loop for ch across text
                  collect (if (or (alphanumericp ch)
                                  (char= ch #\-)
                                  (char= ch #\_))
                              ch
                              #\-))
            'string)))

(defparameter +provider-secret-key-aliases+
  '(("anthropic-provider" . "ANTHROPIC_API_KEY")
    ("anthropic" . "ANTHROPIC_API_KEY")
    ("openai-compatible-provider" . "OPENAI_API_KEY")
    ("openai-compat" . "OPENAI_API_KEY")
    ("openai" . "OPENAI_API_KEY")
    ("kimi-provider" . "MOONSHOT_API_KEY")
    ("kimi" . "MOONSHOT_API_KEY")
    ("moonshot" . "MOONSHOT_API_KEY")))

(defparameter +delegation-sensitive-context-keys+
  '("provider-secrets"
    "provider-secret"
    "provider-credentials"
    "provider-credential"
    "provider-api-key"
    "api-key"
    "api_key"
    "anthropic_api_key"
    "openai_api_key"
    "moonshot_api_key"))

(defconstant +handoff-context-default-max-bytes+ 65536
  "Default serialized size ceiling for coding-task handoff context packets.")

(defconstant +handoff-context-min-max-bytes+ 1024
  "Smallest supported serialized size ceiling for handoff context packets.")

(defun %provider-secret-key-id (raw-key)
  (let* ((text (%coordination-require-string raw-key "provider secret key"))
         (down (string-downcase text))
         (alias (cdr (assoc down +provider-secret-key-aliases+ :test #'string=))))
    (or alias (string-upcase down))))

(defun %provider-secret-pairs (provider-secrets)
  (cond
    ((null provider-secrets) nil)
    ((and (listp provider-secrets)
          (every (lambda (entry)
                   (and (consp entry)
                        (not (null (car entry)))))
                 provider-secrets))
     (mapcar (lambda (entry)
               (let ((tail (cdr entry)))
                 (cons (car entry)
                       (if (and (consp tail) (null (cdr tail)))
                           (car tail)
                           tail))))
             provider-secrets))
    ((and (listp provider-secrets)
          (evenp (length provider-secrets)))
     (loop for (key value) on provider-secrets by #'cddr
           collect (cons key value)))
    (t
     (error "provider-secrets must be an alist or plist, got ~S" provider-secrets))))

(defun %coordination-plist-like-p (value)
  (and (listp value)
       (evenp (length value))
       (loop for (key _value) on value by #'cddr
             always (or (keywordp key)
                        (symbolp key)
                        (stringp key)))))

(defun %delegation-sensitive-context-key-p (key)
  (let* ((text (%coordination-trim-string key))
         (down (string-downcase text)))
    (member down +delegation-sensitive-context-keys+ :test #'string=)))

(defun %sanitize-delegation-context (context)
  (cond
    ((null context) "")
    ((hash-table-p context)
     (let ((clean (make-hash-table :test (hash-table-test context))))
       (maphash (lambda (key value)
                  (unless (%delegation-sensitive-context-key-p key)
                    (setf (gethash key clean)
                          (%sanitize-delegation-context value))))
                context)
       clean))
    ((%coordination-plist-like-p context)
     (let ((clean '()))
       (loop for (key value) on context by #'cddr do
         (unless (%delegation-sensitive-context-key-p key)
           (setf clean
                 (append clean
                         (list key (%sanitize-delegation-context value))))))
       clean))
    ((and (listp context)
          (every #'consp context))
     (let ((clean '()))
       (dolist (entry context (nreverse clean))
         (unless (%delegation-sensitive-context-key-p (car entry))
           (push (cons (car entry)
                       (%sanitize-delegation-context (cdr entry)))
                 clean)))))
    ((listp context)
     (mapcar #'%sanitize-delegation-context context))
    (t context)))

(defun %take-list-prefix (items limit)
  "Return up to LIMIT elements from ITEMS."
  (if (and (integerp limit) (>= limit 0))
      (loop for item in items
            for index from 0
            while (< index limit)
            collect item)
      (copy-list items)))

(defun %safe-git-status-snapshot (&optional project-root)
  "Return a compact git status snapshot or NIL on failure."
  (handler-case
      (let* ((status (%git-status-data :project-root project-root))
             (tracking (copy-tree (or (getf status :tracking) '())))
             (staged (copy-list (or (getf status :staged) '())))
             (unstaged (copy-list (or (getf status :unstaged) '())))
             (untracked (copy-list (or (getf status :untracked) '()))))
        (list :project-root (getf status :project-root)
              :branch (getf status :branch)
              :tracking tracking
              :staged staged
              :unstaged unstaged
              :untracked untracked))
    (error ()
      nil)))

(defun %trim-git-status-snapshot (snapshot limit)
  "Return SNAPSHOT with file lists capped to LIMIT entries each."
  (if (null snapshot)
      nil
      (list :project-root (getf snapshot :project-root)
            :branch (getf snapshot :branch)
            :tracking (copy-tree (or (getf snapshot :tracking) '()))
            :staged (%take-list-prefix (or (getf snapshot :staged) '()) limit)
            :unstaged (%take-list-prefix (or (getf snapshot :unstaged) '()) limit)
            :untracked (%take-list-prefix (or (getf snapshot :untracked) '()) limit))))

(defun %take-last-list-items (items limit)
  "Return the trailing LIMIT elements from ITEMS."
  (let* ((values (copy-list (or items '())))
         (count (length values)))
    (cond
      ((or (null limit) (>= limit count))
       values)
      ((<= limit 0)
       '())
      (t
       (nthcdr (- count limit) values)))))

(defun %conversation-handoff-snapshot (conversation &key (entry-limit 12))
  "Return a bounded conversation snapshot suitable for delegation."
  (let ((snapshot (and (typep conversation 'conversation-state)
                       (%conversation->snapshot conversation))))
    (when snapshot
      (list :session-id (getf snapshot :session-id)
            :state (getf snapshot :state)
            :created-at (getf snapshot :created-at)
            :updated-at (getf snapshot :updated-at)
            :active-fork (getf snapshot :active-fork)
            :fork-branch-point (getf snapshot :fork-branch-point)
            :forks (copy-tree (or (getf snapshot :forks) '()))
            :entry-count (length (or (getf snapshot :entries) '()))
            :entries (%take-last-list-items (or (getf snapshot :entries) '())
                                            entry-limit)))))

(defun %trim-memory-snapshot-scope (entries limit)
  "Return up to LIMIT serialized memory entries."
  (%take-list-prefix (or entries '()) limit))

(defun %memory-handoff-snapshot (backend &key (entry-limit 8))
  "Return a bounded memory snapshot suitable for delegation."
  (let ((snapshot (and backend (%memory->snapshot backend))))
    (when snapshot
      (list :backend-kind (getf snapshot :backend-kind)
            :effective-count (length (or (getf snapshot :effective) '()))
            :global-count (length (or (getf snapshot :global) '()))
            :project-count (length (or (getf snapshot :project) '()))
            :session-count (length (or (getf snapshot :session) '()))
            :effective (%trim-memory-snapshot-scope (getf snapshot :effective)
                                                    entry-limit)
            :global (%trim-memory-snapshot-scope (getf snapshot :global)
                                                 entry-limit)
            :project (%trim-memory-snapshot-scope (getf snapshot :project)
                                                  entry-limit)
            :session (%trim-memory-snapshot-scope (getf snapshot :session)
                                                  entry-limit)))))

(defun %resolve-handoff-project-root (sanitized-context)
  "Resolve the project root hinted by SANITIZED-CONTEXT, if any."
  (let ((project-root (and (listp sanitized-context)
                           (getf sanitized-context :project-root))))
    (cond
      ((pathnamep project-root)
       (uiop:ensure-directory-pathname project-root))
      ((and (stringp project-root) (plusp (length project-root)))
       (uiop:ensure-directory-pathname (pathname project-root)))
      (t
       nil))))

(defun %handoff-context-max-bytes (budget)
  "Return the serialized size ceiling implied by BUDGET."
  (let* ((explicit (or (and (listp budget) (getf budget :context-max-bytes))
                       (and (listp budget) (getf budget :max-context-bytes))))
         (token-budget (and (listp budget)
                            (getf budget :token-budget-remaining))))
    (cond
      ((and (integerp explicit) (> explicit 0))
       (max +handoff-context-min-max-bytes+
            (min explicit +handoff-context-default-max-bytes+)))
      ((and (integerp token-budget) (> token-budget 0))
       (max +handoff-context-min-max-bytes+
            (min +handoff-context-default-max-bytes+
                 (* 4 token-budget))))
      (t
       +handoff-context-default-max-bytes+))))

(defun %handoff-context-budget-mode (max-bytes)
  "Pick a detail mode for MAX-BYTES."
  (cond
    ((<= max-bytes 4096) :compact)
    ((<= max-bytes 12288) :operator)
    (t :verbose)))

(defun %current-ide-context ()
  "Return the globally attached IDE context when available."
  (let ((symbol (find-symbol "*IDE-CONTEXT*" :amoebum)))
    (when (and symbol (boundp symbol))
      (symbol-value symbol))))

(defun %handoff-ide-packet (sanitized-context max-bytes)
  "Return a bounded IDE/file context packet."
  (let* ((ctx (or (and (listp sanitized-context)
                       (getf sanitized-context :ide-context))
                  (%current-ide-context)))
         (mode (%handoff-context-budget-mode max-bytes))
         (budget (max 1 (floor max-bytes 4))))
    (when (ide-context-p ctx)
      (ide-context-build-packet ctx :mode mode :budget budget))))

(defun %extract-handoff-context-extras (sanitized-context)
  "Return stable extra keys from SANITIZED-CONTEXT not promoted into the packet."
  (cond
    ((not (listp sanitized-context))
     sanitized-context)
    (t
     (let ((extras '()))
       (loop for (key value) on sanitized-context by #'cddr do
         (unless (member key '(:conversation :memory-backend :worktree :ide-context :project-root)
                         :test #'eq)
           (setf extras (append extras (list key value)))))
       extras))))

(defun %coding-task-context-packet (sanitized-context budget)
  "Build a stable structured packet for coding-task delegation."
  (let* ((max-bytes (%handoff-context-max-bytes budget))
         (mode (%handoff-context-budget-mode max-bytes))
         (conversation (and (listp sanitized-context)
                            (getf sanitized-context :conversation)))
         (memory-backend (and (listp sanitized-context)
                              (getf sanitized-context :memory-backend)))
         (worktree (or (and (listp sanitized-context)
                            (getf sanitized-context :worktree))
                       (current-delegated-agent-worktree)))
         (project-root (%resolve-handoff-project-root sanitized-context))
         (entry-limit (ecase mode
                        (:compact 4)
                        (:operator 8)
                        (:verbose 16)))
         (memory-limit (ecase mode
                         (:compact 3)
                         (:operator 6)
                         (:verbose 10)))
         (git-limit (ecase mode
                      (:compact 3)
                      (:operator 6)
                      (:verbose 12))))
    (list :schema-version 1
          :packet-kind "coding-task-context"
          :compression-mode mode
          :max-bytes max-bytes
          :generated-at (get-universal-time)
          :conversation (%conversation-handoff-snapshot conversation
                                                    :entry-limit entry-limit)
          :files (%handoff-ide-packet sanitized-context max-bytes)
          :git (%trim-git-status-snapshot (%safe-git-status-snapshot project-root)
                                          git-limit)
          :memory (%memory-handoff-snapshot memory-backend
                                            :entry-limit memory-limit)
          :worktree (worktree-metadata-plist worktree)
          :extras (%extract-handoff-context-extras sanitized-context))))

(defun %coding-task-context-packet-size (packet)
  "Return the serialized size of PACKET in bytes."
  (length (jonathan:to-json packet)))

(defun %fit-coding-task-context-packet (packet)
  "Shrink PACKET until it fits its declared :MAX-BYTES ceiling."
  (let* ((max-bytes (or (getf packet :max-bytes)
                        +handoff-context-default-max-bytes+))
         (fitted (copy-tree packet)))
    (labels ((size-fits-p ()
               (<= (%coding-task-context-packet-size fitted) max-bytes))
             (conversation-entries ()
               (and (getf fitted :conversation)
                    (getf (getf fitted :conversation) :entries)))
             (memory-scope (scope)
               (and (getf fitted :memory)
                    (getf (getf fitted :memory) scope)))
             (halve-list (items)
               (%take-last-list-items items (max 1 (floor (length items) 2)))))
      (loop repeat 12
            until (size-fits-p)
            do (cond
                 ((and (listp (getf fitted :extras))
                       (plusp (length (getf fitted :extras))))
                  (setf (getf fitted :extras) '()))
                 ((and (listp (conversation-entries))
                       (> (length (conversation-entries)) 1))
                  (setf (getf (getf fitted :conversation) :entries)
                        (halve-list (conversation-entries))))
                 ((and (listp (memory-scope :effective))
                       (> (length (memory-scope :effective)) 1))
                  (setf (getf (getf fitted :memory) :effective)
                        (%take-list-prefix (memory-scope :effective)
                                           (max 1 (floor (length (memory-scope :effective)) 2)))))
                 ((and (listp (memory-scope :project))
                       (> (length (memory-scope :project)) 1))
                  (setf (getf (getf fitted :memory) :project)
                        (%take-list-prefix (memory-scope :project)
                                           (max 1 (floor (length (memory-scope :project)) 2)))))
                 ((and (listp (memory-scope :global))
                       (> (length (memory-scope :global)) 1))
                  (setf (getf (getf fitted :memory) :global)
                        (%take-list-prefix (memory-scope :global)
                                           (max 1 (floor (length (memory-scope :global)) 2)))))
                 ((and (listp (memory-scope :session))
                       (> (length (memory-scope :session)) 1))
                  (setf (getf (getf fitted :memory) :session)
                        (%take-list-prefix (memory-scope :session)
                                           (max 1 (floor (length (memory-scope :session)) 2)))))
                 ((and (getf fitted :files)
                       (eq (getf (getf fitted :files) :mode) :verbose))
                  (setf (getf fitted :files)
                        (let ((files (copy-tree (getf fitted :files))))
                          (setf (getf files :mode) :operator
                                (getf files :selections)
                                (%take-list-prefix (or (getf files :selections) '()) 5))
                          files)))
                 ((and (getf fitted :files)
                       (eq (getf (getf fitted :files) :mode) :operator))
                  (setf (getf fitted :files)
                        (let ((files (copy-tree (getf fitted :files))))
                          (setf (getf files :mode) :compact
                                (getf files :selections) '()
                                (getf files :diagnostics)
                                (%take-list-prefix (or (getf files :diagnostics) '()) 3))
                          files)))
                 ((getf fitted :git)
                  (setf (getf fitted :git)
                        (%trim-git-status-snapshot (getf fitted :git) 1)))
                 (t
                  (return))))
      fitted)))

(defun %serialize-coding-task-context (sanitized-context budget)
  "Serialize SANITIZED-CONTEXT as a bounded structured handoff packet."
  (let* ((packet (%coding-task-context-packet sanitized-context budget))
         (fitted (%fit-coding-task-context-packet packet))
         (max-bytes (or (getf fitted :max-bytes)
                        +handoff-context-default-max-bytes+)))
    (sw4rm-sdk:serialize-handoff-context fitted :max-bytes max-bytes)))

(defun %deserialize-handoff-context-safely (snapshot)
  "Best-effort decode for a handoff context snapshot."
  (cond
    ((or (null snapshot)
         (and (stringp snapshot) (zerop (length snapshot))))
     nil)
    ((stringp snapshot)
     (ignore-errors (sw4rm-sdk:deserialize-handoff-context snapshot)))
    ((listp snapshot)
     snapshot)
    (t
     nil)))

(defun %annotate-handoff-payload-context (payload)
  "Attach parsed context metadata to PAYLOAD when present."
  (let* ((snapshot (and (listp payload) (getf payload :context-snapshot)))
         (packet (%deserialize-handoff-context-safely snapshot)))
    (append payload
            (when packet
              (list :context-packet packet))
            (when (stringp snapshot)
              (list :context-snapshot-size-bytes (length snapshot))))))

(defun %apply-agent-provider-secrets! (agent-id provider-secrets)
  (let ((registry *user-session-registry*))
    (unless (typep registry 'sw4rm-sdk:local-registry)
      (error "User session registry is not initialized."))
    (sw4rm-sdk:local-registry-clear-provider-secrets registry agent-id)
    (dolist (pair (%provider-secret-pairs provider-secrets))
      (let ((provider-key (%provider-secret-key-id (car pair)))
            (secret-value (%coordination-trim-string (cdr pair))))
        (when (plusp (length secret-value))
          (sw4rm-sdk:local-registry-set-provider-secret
           registry
           agent-id
           provider-key
           secret-value))))
    t))

(defun %user-agent-id (user-id session-id)
  (format nil "user/~A/session/~A"
          (%coordination-normalize-token user-id)
          (%coordination-normalize-token session-id)))

(defun %ensure-user-handoff-client ()
  (or *user-handoff-client*
      (setf *user-handoff-client*
            (make-instance 'sw4rm-sdk:handoff-client
                           :address "local://amoebum/user-handoff"))))

(defun %ensure-user-negotiation-client ()
  (or *user-negotiation-client*
      (setf *user-negotiation-client*
            (make-instance 'sw4rm-sdk:negotiation-room-client
                           :address "local://amoebum/user-negotiation"))))

(defun %registered-agent-id-for-session (session-id)
  (let ((resolved-session-id (%coordination-require-string session-id "session-id")))
    (bt:with-lock-held (*user-coordination-lock*)
      (or (gethash resolved-session-id *user-session->agent-id*)
          (error "No user peer registered for session-id ~S." resolved-session-id)))))

(defun %session-id-for-agent (agent-id)
  (bt:with-lock-held (*user-coordination-lock*)
    (gethash agent-id *user-agent->session-id*)))

(defun %next-user-handoff-id ()
  (bt:with-lock-held (*user-coordination-lock*)
    (incf *user-handoff-sequence*)
    (format nil "user-handoff-~D" *user-handoff-sequence*)))

(defun %publish-user-coordination-event (event-type payload &key (severity :info))
  (publish (current-event-bus)
           event-type
           :source :sw4rm-user-coordination
           :severity severity
           :payload payload)
  payload)

(defun clear-user-coordination-state ()
  "Clear user session registration, delegation, and negotiation state."
  (bt:with-lock-held (*user-coordination-lock*)
    (sw4rm-sdk:local-registry-clear *user-session-registry*)
    (clrhash *user-session->agent-id*)
    (clrhash *user-session->user-id*)
    (clrhash *user-agent->session-id*)
    (clrhash *user-negotiation-room-participants*)
    (setf *user-handoff-client* nil
          *user-negotiation-client* nil
          *user-handoff-sequence* 0)
    t))

(defun register-user-session-peer (session-id &key
                                                user-id
                                                (capabilities '("chat" "handoff" "code-review"))
                                                name
                                                description
                                                (provider-secrets nil provider-secrets-supplied-p)
                                                (if-exists :replace))
  "Register a user SESSION-ID as a SW4RM peer in the local registry."
  (let* ((resolved-session-id (%coordination-require-string session-id "session-id"))
         (resolved-user-id (%coordination-require-string (or user-id session-id) "user-id"))
         (agent-id (%user-agent-id resolved-user-id resolved-session-id))
         (agent-config (sw4rm-sdk:make-agent-config
                        :agent-id agent-id
                        :name (or name (format nil "user-~A" resolved-user-id))
                        :description (or description
                                         (format nil "Amoebum user session ~A"
                                                 resolved-session-id))
                        :capabilities (copy-list capabilities))))
    (bt:with-lock-held (*user-coordination-lock*)
      (let ((existing-agent-id (gethash resolved-session-id *user-session->agent-id*)))
        (when (and existing-agent-id
                   (not (string= existing-agent-id agent-id)))
          (sw4rm-sdk:local-registry-unregister *user-session-registry* existing-agent-id)
          (remhash existing-agent-id *user-agent->session-id*)))
      (sw4rm-sdk:local-registry-register *user-session-registry*
                                         agent-config
                                         :if-exists if-exists)
      (setf (gethash resolved-session-id *user-session->agent-id*) agent-id
            (gethash resolved-session-id *user-session->user-id*) resolved-user-id
            (gethash agent-id *user-agent->session-id*) resolved-session-id))
    (when provider-secrets-supplied-p
      (%apply-agent-provider-secrets! agent-id provider-secrets))
    agent-config))

(defun unregister-user-session-peer (session-id)
  "Unregister SESSION-ID from local user coordination."
  (let* ((resolved-session-id (%coordination-require-string session-id "session-id"))
         (agent-id nil))
    (bt:with-lock-held (*user-coordination-lock*)
      (setf agent-id (gethash resolved-session-id *user-session->agent-id*))
      (remhash resolved-session-id *user-session->agent-id*)
      (remhash resolved-session-id *user-session->user-id*)
      (when agent-id
        (remhash agent-id *user-agent->session-id*)))
    (if agent-id
        (sw4rm-sdk:local-registry-unregister *user-session-registry* agent-id)
        nil)))

(defun find-user-session-peer (session-id)
  "Return SW4RM agent-config for SESSION-ID or NIL."
  (let ((agent-id (bt:with-lock-held (*user-coordination-lock*)
                    (gethash (%coordination-require-string session-id "session-id")
                             *user-session->agent-id*))))
    (and agent-id
         (sw4rm-sdk:local-registry-get *user-session-registry* agent-id))))

(defun list-user-session-peers ()
  "Return registered user peers as plists."
  (bt:with-lock-held (*user-coordination-lock*)
    (let ((peers '()))
      (maphash (lambda (session-id agent-id)
                 (let ((config (sw4rm-sdk:local-registry-get
                                *user-session-registry*
                                agent-id)))
                   (push (list :session-id session-id
                               :user-id (gethash session-id *user-session->user-id*)
                               :agent-id agent-id
                               :capabilities (and config
                                                  (sw4rm-sdk:agent-config-capabilities config)))
                         peers)))
               *user-session->agent-id*)
      (sort peers #'string< :key (lambda (peer) (getf peer :session-id))))))

(defun handoff-between-users (from-session-id to-session-id reason
                                &key request-id
                                  context
                                  (capabilities-required '("code-review"))
                                  (priority 0)
                                  budget
                                  delegation-policy
                                  timeout-ms)
  "Initiate a SW4RM handoff request from one user session to another."
  (let* ((resolved-reason (%coordination-require-string reason "reason"))
         (from-agent-id (%registered-agent-id-for-session from-session-id))
         (to-agent-id (%registered-agent-id-for-session to-session-id))
         (sanitized-context (%sanitize-delegation-context context))
         (serialized-context (%serialize-coding-task-context sanitized-context budget))
         (context-packet (%deserialize-handoff-context-safely serialized-context))
         (request (list :request-id (or request-id (%next-user-handoff-id))
                        :from-agent from-agent-id
                        :to-agent to-agent-id
                        :reason resolved-reason
                        :context-snapshot (or serialized-context "")
                        :capabilities-required (copy-list capabilities-required)
                        :priority priority)))
    (when budget
      (setf (getf request :budget) (copy-list budget)))
    (when delegation-policy
      (setf (getf request :delegation-policy) (copy-list delegation-policy)))
    (when timeout-ms
      (setf (getf request :timeout-ms) timeout-ms))
    (let ((response (sw4rm-sdk:initiate-handoff (%ensure-user-handoff-client) request)))
      (let ((payload
              (append response
                      (list :from-session-id (%coordination-require-string from-session-id "from-session-id")
                            :to-session-id (%coordination-require-string to-session-id "to-session-id")
                            :agent-id from-agent-id
                            :from-agent-id from-agent-id
                            :to-agent-id to-agent-id
                            :reason resolved-reason
                            :context-snapshot (or serialized-context "")
                            :context-packet context-packet
                            :capabilities-required (copy-list capabilities-required)
                            :priority priority))))
        (%publish-user-coordination-event +event-type-user-handoff-requested+ payload)
        payload))))

(defun get-user-pending-handoffs (session-id)
  "Return pending handoff requests routed to SESSION-ID."
  (let* ((agent-id (%registered-agent-id-for-session session-id))
         (pending (sw4rm-sdk:get-pending-handoffs (%ensure-user-handoff-client) agent-id)))
    (mapcar (lambda (request)
              (let ((from-agent (getf request :from-agent))
                    (to-agent (getf request :to-agent)))
                (%annotate-handoff-payload-context
                 (append request
                         (list :from-session-id (%session-id-for-agent from-agent)
                               :to-session-id (%session-id-for-agent to-agent))))))
            pending)))

(defun user-handoff-status (handoff-id)
  "Return handoff status plist for HANDOFF-ID."
  (%annotate-handoff-payload-context
   (sw4rm-sdk:get-handoff-status (%ensure-user-handoff-client) handoff-id)))

(defun accept-user-handoff (handoff-id)
  "Accept HANDOFF-ID on the local coordination bus."
  (let* ((response (sw4rm-sdk:accept-handoff (%ensure-user-handoff-client) handoff-id))
         (status (or (ignore-errors (user-handoff-status handoff-id))
                     response))
         (payload (append status
                          (list :handoff-id handoff-id
                                :agent-id (or (getf status :to-agent)
                                              (getf status :to-agent-id)
                                              (getf response :to-agent)
                                              (getf response :to-agent-id))))))
    (%publish-user-coordination-event +event-type-user-handoff-accepted+ payload)
    payload))

(defun reject-user-handoff (handoff-id reason &key rejection-code retry-after-ms redirect-to-agent-id)
  "Reject HANDOFF-ID with optional protocol metadata."
  (let* ((resolved-code (or rejection-code sw4rm-sdk:+error-code-unspecified+))
         (response (sw4rm-sdk:reject-handoff-with-options (%ensure-user-handoff-client)
                                                          handoff-id
                                                          reason
                                                          :rejection-code resolved-code
                                                          :retry-after-ms retry-after-ms
                                                          :redirect-to-agent-id redirect-to-agent-id))
         (status (or (ignore-errors (user-handoff-status handoff-id))
                     response))
         (payload (append status
                          (list :handoff-id handoff-id
                                :reason reason
                                :rejection-code resolved-code
                                :retry-after-ms retry-after-ms
                                :redirect-to-agent-id redirect-to-agent-id
                                :agent-id (or (getf status :to-agent)
                                              (getf status :to-agent-id)
                                              (getf response :to-agent)
                                              (getf response :to-agent-id))))))
    (%publish-user-coordination-event +event-type-user-handoff-rejected+
                                      payload
                                      :severity :warning)
    payload))

(defun complete-user-handoff (handoff-id)
  "Mark HANDOFF-ID as complete."
  (let* ((response (sw4rm-sdk:complete-handoff (%ensure-user-handoff-client) handoff-id))
         (status (or (ignore-errors (user-handoff-status handoff-id))
                     response))
         (payload (append status
                          (list :handoff-id handoff-id
                                :agent-id (or (getf status :to-agent)
                                              (getf status :to-agent-id)
                                              (getf response :to-agent)
                                              (getf response :to-agent-id))))))
    (%publish-user-coordination-event +event-type-user-handoff-completed+ payload)
    payload))

(defun create-user-negotiation-room (room-id participant-session-ids
                                     &key description metadata)
  "Create a multi-user negotiation room for code review."
  (let* ((resolved-room-id (%coordination-require-string room-id "room-id"))
         (participants (remove-duplicates
                        (mapcar (lambda (session-id)
                                  (%coordination-require-string session-id "participant session-id"))
                                participant-session-ids)
                        :test #'string=)))
    (dolist (session-id participants)
      (%registered-agent-id-for-session session-id))
    (let ((response (sw4rm-sdk:create-room (%ensure-user-negotiation-client)
                                           resolved-room-id
                                           :description description
                                           :metadata metadata)))
      (bt:with-lock-held (*user-coordination-lock*)
        (setf (gethash resolved-room-id *user-negotiation-room-participants*)
              participants))
      (let ((payload (append response
                             (list :room-id resolved-room-id
                                   :participant-session-ids participants
                                   :description description
                                   :metadata metadata
                                   :agent-id (and participants
                                                  (%registered-agent-id-for-session
                                                   (first participants)))))))
        (%publish-user-coordination-event +event-type-user-negotiation-room-created+
                                          payload)
        payload))))

(defun submit-user-negotiation-artifact (room-id proposer-session-id artifact-id artifact
                                         &key requested-critic-session-ids
                                           (aggregation-strategy :confidence-weighted)
                                           timeout-ms
                                           metadata)
  "Submit ARTIFACT for review in ROOM-ID from PROPOSER-SESSION-ID."
  (let* ((resolved-room-id (%coordination-require-string room-id "room-id"))
         (resolved-artifact-id (%coordination-require-string artifact-id "artifact-id"))
         (proposer-agent-id (%registered-agent-id-for-session proposer-session-id))
         (requested-critic-agent-ids
           (mapcar #'%registered-agent-id-for-session requested-critic-session-ids))
         (proposal (list :artifact-id resolved-artifact-id
                         :negotiation-room-id resolved-room-id
                         :proposer-id proposer-agent-id
                         :artifact artifact
                         :metadata metadata
                         :requested-critics requested-critic-agent-ids
                         :aggregation-strategy aggregation-strategy)))
    (when timeout-ms
      (setf (getf proposal :timeout-ms) timeout-ms))
    (let ((response (sw4rm-sdk:submit-artifact (%ensure-user-negotiation-client) proposal)))
      (%publish-user-coordination-event
       +event-type-user-negotiation-artifact-submitted+
       (list :artifact-id resolved-artifact-id
             :room-id resolved-room-id
             :proposer-session-id proposer-session-id
             :requested-critic-session-ids (copy-list requested-critic-session-ids)
             :agent-id proposer-agent-id
             :metadata metadata
             :timeout-ms timeout-ms
             :response response))
      response)))

(defun add-user-negotiation-critique (room-id artifact-id critic-session-id passed
                                      &key score details)
  "Submit a critique vote in a user negotiation room."
  (let* ((resolved-room-id (%coordination-require-string room-id "room-id"))
         (resolved-artifact-id (%coordination-require-string artifact-id "artifact-id"))
         (critic-agent-id (%registered-agent-id-for-session critic-session-id)))
    (let ((response
            (sw4rm-sdk:add-critique
             (%ensure-user-negotiation-client)
             (list :artifact-id resolved-artifact-id
                   :negotiation-room-id resolved-room-id
                   :critic-id critic-agent-id
                   :passed (if passed t nil)
                   :score score
                   :details details))))
      (%publish-user-coordination-event
       +event-type-user-negotiation-critique-added+
       (list :artifact-id resolved-artifact-id
             :room-id resolved-room-id
             :critic-session-id critic-session-id
             :agent-id critic-agent-id
             :passed (if passed t nil)
             :score score
             :details details
             :response response))
      response)))

(defun get-user-negotiation-room-status (room-id)
  "Return room status enriched with session IDs."
  (let* ((resolved-room-id (%coordination-require-string room-id "room-id"))
         (status (sw4rm-sdk:get-room-status (%ensure-user-negotiation-client)
                                            resolved-room-id))
         (participants (bt:with-lock-held (*user-coordination-lock*)
                         (copy-list
                          (gethash resolved-room-id
                                   *user-negotiation-room-participants*))))
         (active-critic-sessions
           (remove nil
                   (mapcar #'%session-id-for-agent
                           (copy-list (getf status :active-critics))))))
    (append status
            (list :participant-session-ids participants
                  :active-critic-session-ids active-critic-sessions))))

(defun wait-for-user-negotiation-decision (artifact-id &key (timeout-s 30.0) (poll-interval-s 0.1))
  "Wait for a negotiation decision for ARTIFACT-ID."
  (let ((decision (sw4rm-sdk:wait-for-decision (%ensure-user-negotiation-client)
                                               artifact-id
                                               :timeout-s timeout-s
                                               :poll-interval-s poll-interval-s)))
    (%publish-user-coordination-event
     +event-type-user-negotiation-decision+
     (append decision
             (list :artifact-id artifact-id
                   :agent-id (or (getf decision :proposer-id)
                                 (getf decision :agent-id)))))
    decision))
