(in-package :amoebum)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %agent-event-type-keyword (name)
    (intern (string-upcase name) :keyword)))

(defparameter +event-type-agent-spawn+
  (%agent-event-type-keyword "agent:spawn"))

(defparameter +event-type-agent-complete+
  (%agent-event-type-keyword "agent:complete"))

(defparameter +event-type-agent-cancelled+
  (%agent-event-type-keyword "agent:cancelled"))

(define-condition agent-cancelled (condition)
  ((agent-id :initarg :agent-id
             :reader agent-cancelled-agent-id))
  (:report (lambda (condition stream)
             (format stream "Agent ~A was cancelled."
                     (agent-cancelled-agent-id condition)))))

(defstruct (agent-record
            (:constructor %make-agent-record
                (&key
                   id
                   (type :task)
                   task
                   parent-message-id
                   (status :queued)
                   (created-ms 0)
                   (started-ms 0)
                   (finished-ms 0)
                   (cancel-requested-p nil)
                   result
                   stdout
                   stderr
                   error-message
                   worktree
                   thread)))
  id
  (type :task)
  task
  parent-message-id
  (status :queued)
  (created-ms 0 :type integer)
  (started-ms 0 :type integer)
  (finished-ms 0 :type integer)
  (cancel-requested-p nil :type boolean)
  result
  stdout
  stderr
  error-message
  worktree
  thread)

(defparameter *agent-registry* (make-hash-table :test #'equal))

(defparameter *agent-registry-lock*
  (bordeaux-threads:make-lock "amoebum-agent-registry-lock"))

(defparameter *agent-completion-queue*
  (ptui.runtime.queue:make-event-queue))

(defparameter *next-agent-sequence* 0)

(defmacro %with-agent-registry-lock (() &body body)
  `(bordeaux-threads:with-lock-held (*agent-registry-lock*)
     ,@body))

(defun %agent-now-ms ()
  (ptui.util.time:monotonic-ms))

(defun %agent-trim-text (text)
  (if (stringp text)
      (string-trim '(#\Space #\Tab #\Newline #\Return) text)
      ""))

(defun %agent-blank-string-p (text)
  (zerop (length (%agent-trim-text text))))

(defun %normalize-agent-type (agent-type)
  (cond
    ((keywordp agent-type) agent-type)
    ((symbolp agent-type)
     (intern (string-upcase (symbol-name agent-type)) :keyword))
    ((stringp agent-type)
     (intern (string-upcase (%agent-trim-text agent-type)) :keyword))
    (t :task)))

(defun %next-agent-id (agent-type)
  (%with-agent-registry-lock ()
    (incf *next-agent-sequence*)
    (format nil "~(~A~)-~4,'0D"
            (or agent-type :task)
            *next-agent-sequence*)))

(defun %agent-payload (agent)
  (let ((payload
          (list :id (agent-record-id agent)
                :type (agent-record-type agent)
                :task (agent-record-task agent)
                :status (agent-record-status agent)
                :created-ms (agent-record-created-ms agent)
                :started-ms (agent-record-started-ms agent)
                :finished-ms (agent-record-finished-ms agent)
                :duration-ms (max 0 (- (agent-record-finished-ms agent)
                                       (agent-record-started-ms agent)))
                :cancel-requested-p (agent-record-cancel-requested-p agent)
                :result (agent-record-result agent)
                :stdout (agent-record-stdout agent)
                :stderr (agent-record-stderr agent)
                :error-message (agent-record-error-message agent)
                :parent-message-id (agent-record-parent-message-id agent))))
    (let ((worktree (worktree-metadata-plist (agent-record-worktree agent))))
      (if worktree
          (append payload (list :worktree worktree))
          payload))))

(defun %publish-agent-event (event-type payload &key (severity :info))
  (publish (current-event-bus)
           event-type
           :source :amoebum
           :severity severity
           :payload payload)
  payload)

(defun clear-agents ()
  (ptui.runtime.queue:queue-pop-all *agent-completion-queue*)
  (%with-agent-registry-lock ()
    (clrhash *agent-registry*)
    (setf *next-agent-sequence* 0))
  t)

(defun find-agent (agent-id)
  (%with-agent-registry-lock ()
    (gethash agent-id *agent-registry*)))

(defun list-agents (&key (include-completed-p t))
  (%with-agent-registry-lock ()
    (let ((agents '()))
      (maphash (lambda (_id agent)
                 (declare (ignore _id))
                 (when (or include-completed-p
                           (member (agent-record-status agent)
                                   '(:queued :running :cancelling)
                                   :test #'eq))
                   (push agent agents)))
               *agent-registry*)
      (sort agents #'< :key #'agent-record-created-ms))))

(defun active-agent-count ()
  (%with-agent-registry-lock ()
    (let ((count 0))
      (maphash (lambda (_id agent)
                 (declare (ignore _id))
                 (when (member (agent-record-status agent)
                               '(:queued :running :cancelling)
                               :test #'eq)
                   (incf count)))
               *agent-registry*)
      count)))

(defun agent-cancel-requested-p (agent-or-id)
  (let ((agent (if (typep agent-or-id 'agent-record)
                   agent-or-id
                   (find-agent agent-or-id))))
    (and agent
         (%with-agent-registry-lock ()
           (agent-record-cancel-requested-p agent)))))

(defun agent-check-cancel (agent-or-id)
  (when (agent-cancel-requested-p agent-or-id)
    (let ((agent (if (typep agent-or-id 'agent-record)
                     agent-or-id
                     (find-agent agent-or-id))))
      (error 'agent-cancelled
             :agent-id (and agent (agent-record-id agent)))))
  nil)

(defun cancel-agent (agent-id)
  (let ((agent (find-agent agent-id)))
    (unless agent
      (return-from cancel-agent nil))
    (%with-agent-registry-lock ()
      (setf (agent-record-cancel-requested-p agent) t)
      (when (member (agent-record-status agent) '(:queued :running) :test #'eq)
        (setf (agent-record-status agent) :cancelling)))
    (usdt-probe-agent-lifecycle :cancel-requested
                                (agent-record-id agent)
                                (agent-record-type agent)
                                (agent-record-status agent)
                                0
                                :parent-message-id (agent-record-parent-message-id agent))
    (%publish-agent-event +event-type-agent-cancelled+
                          (%agent-payload agent)
                          :severity :warning)
    t))

(defun %agent-output-string (agent)
  (with-output-to-string (out)
    (when (and (stringp (agent-record-stdout agent))
               (plusp (length (agent-record-stdout agent))))
      (write-string (agent-record-stdout agent) out))
    (when (and (stringp (agent-record-stderr agent))
               (plusp (length (agent-record-stderr agent))))
      (when (plusp (length (agent-record-stdout agent)))
        (write-char #\Newline out))
      (write-string (agent-record-stderr agent) out))))

(defun agent-output (agent-id)
  (let ((agent (find-agent agent-id)))
    (unless agent
      (return-from agent-output (values nil nil)))
    (values (%agent-output-string agent)
            (agent-record-status agent))))

(defun %swarm-agent-present-p (value)
  (and (fboundp 'swarm-agent-p)
       (ignore-errors (swarm-agent-p value))))

(defun %resolve-runtime-agent (agent-or-id backend)
  (cond
    ((or (agent-record-p agent-or-id)
         (%swarm-agent-present-p agent-or-id))
     agent-or-id)
    ((null agent-or-id)
     nil)
    (t
     (case backend
       (:local (find-agent agent-or-id))
       (:swarm (find-swarm-agent agent-or-id))
       (otherwise
        (or (find-agent agent-or-id)
            (find-swarm-agent agent-or-id)))))))

(defun find-runtime-agent (agent-id &key (backend :auto))
  (%resolve-runtime-agent agent-id backend))

(defun runtime-agent-backend (agent-or-id &key (backend :auto))
  (let ((agent (%resolve-runtime-agent agent-or-id backend)))
    (cond
      ((agent-record-p agent) :local)
      ((%swarm-agent-present-p agent) :swarm)
      (t nil))))

(defun runtime-agent-id (agent-or-id &key (backend :auto))
  (let ((agent (%resolve-runtime-agent agent-or-id backend)))
    (cond
      ((agent-record-p agent) (agent-record-id agent))
      ((%swarm-agent-present-p agent) (swarm-agent-id agent))
      (t nil))))

(defun runtime-agent-task (agent-or-id &key (backend :auto))
  (let ((agent (%resolve-runtime-agent agent-or-id backend)))
    (cond
      ((agent-record-p agent) (agent-record-task agent))
      ((%swarm-agent-present-p agent) (swarm-agent-task agent))
      (t nil))))

(defun runtime-agent-status (agent-or-id &key (backend :auto))
  (let ((agent (%resolve-runtime-agent agent-or-id backend)))
    (cond
      ((agent-record-p agent) (agent-record-status agent))
      ((%swarm-agent-present-p agent) (swarm-agent-status agent))
      (t nil))))

(defun runtime-agent-result (agent-or-id &key (backend :auto))
  (let ((agent (%resolve-runtime-agent agent-or-id backend)))
    (cond
      ((agent-record-p agent) (agent-record-result agent))
      ((%swarm-agent-present-p agent) (swarm-agent-result agent))
      (t nil))))

(defun runtime-agent-error-message (agent-or-id &key (backend :auto))
  (let ((agent (%resolve-runtime-agent agent-or-id backend)))
    (cond
      ((agent-record-p agent) (agent-record-error-message agent))
      ((%swarm-agent-present-p agent) (swarm-agent-error-message agent))
      (t nil))))

(defun runtime-agent-output (agent-or-id &key (backend :auto))
  (let ((agent (%resolve-runtime-agent agent-or-id backend)))
    (cond
      ((agent-record-p agent)
       (%agent-output-string agent))
      ((%swarm-agent-present-p agent)
       (let ((backing-agent (swarm-agent-backing-agent agent))
             (result (swarm-agent-result agent))
             (error-message (swarm-agent-error-message agent)))
         (if backing-agent
             (%agent-output-string backing-agent)
             (with-output-to-string (out)
               (when (and (stringp result)
                          (plusp (length result)))
                 (write-string result out))
               (when (and (stringp error-message)
                          (plusp (length error-message)))
                 (when (and (stringp result)
                            (plusp (length result)))
                   (write-char #\Newline out))
                 (write-string error-message out))))))
      (t nil))))

(defun runtime-agent-worktree (agent-or-id &key (backend :auto))
  (let ((agent (%resolve-runtime-agent agent-or-id backend)))
    (cond
      ((agent-record-p agent) (agent-record-worktree agent))
      ((%swarm-agent-present-p agent) (swarm-agent-worktree agent))
      (t nil))))

(defun runtime-agent-terminal-p (agent-or-id &key (backend :auto))
  (member (runtime-agent-status agent-or-id :backend backend)
          '(:completed :failed :cancelled :timeout)
          :test #'eq))

(defun %default-agent-runner (agent)
  (format t "Running ~A agent ~A~%"
          (string-downcase (symbol-name (agent-record-type agent)))
          (agent-record-id agent))
  (loop repeat 5 do
    (sleep 0.01)
    (agent-check-cancel agent))
  (let ((task (%agent-trim-text (agent-record-task agent))))
    (if (%agent-blank-string-p task)
        "Agent completed."
        (format nil "Agent completed task: ~A" task))))

(defun %agent-runner-finished-status (agent)
  (if (agent-record-cancel-requested-p agent)
      :cancelled
      :completed))

(defun %configured-swarm-delegation-mode (&optional (config (ignore-errors (current-config))))
  (or (and config (ignore-errors (config-value :swarm-delegation-mode config)))
      :local))

(defun %spawn-delegation-backend-label (backend)
  (ecase backend
    (:local "local")
    (:swarm "sw4rm")))

(defun %spawned-delegation-record-id (record backend)
  (ecase backend
    (:local (agent-record-id record))
    (:swarm (swarm-agent-id record))))

(defun %spawn-task-via-configured-backend (task-text &key config agent-type parent-message-id
                                                     persona system-prompt timeout-seconds
                                                     worktree)
  "Route TASK-TEXT through the configured local/SW4RM delegation backend.
Returns two values: the spawned record and the backend keyword (:local or :swarm).

Routing decision (NXT-008)
--------------------------
The routing mode is read from the :swarm-delegation-mode key in the active
config (see `%configured-swarm-delegation-mode`).  Default when absent: :local.

  :local (default)
    All sub-agents run in-process as bordeaux-threads threads inside the
    current SBCL image.  Preferred for single-user, single-machine use and
    during development/testing.  Zero network overhead; full access to shared
    in-process state (*agent-registry*, event bus, checkpoint store, etc.).

  :networked
    Delegates through the SW4RM SDK's local-mode router
    (`spawn-swarm-agent`).  Each task is handed off to a swarm state machine
    and may be executed by an independent worker process registered in the
    SW4RM local registry.  Use this when tasks need process isolation,
    parallel multi-session scheduling, or future networked-peer delegation.

The threshold for choosing :networked over :local is primarily operational:
switch when the orchestration overhead (state-machine bookkeeping, handoff
latency) is justified by the isolation or parallelism benefits.  For
single-session interactive use the :local path is always preferred because
it avoids unnecessary inter-process serialisation and keeps the agent
lifecycle fully inspectable within the same runtime."
  (let ((mode (%configured-swarm-delegation-mode config)))
    (case mode
      (:networked
       (values (spawn-swarm-agent task-text
                                  :timeout-seconds timeout-seconds
                                  :worktree worktree)
               :swarm))
      (otherwise
       (values (spawn-agent task-text
                            :agent-type (or agent-type :task)
                            :parent-message-id parent-message-id
                            :persona persona
                            :system-prompt system-prompt
                            :worktree worktree)
               :local)))))

(defun %agent-status->probe-phase (status)
  (case status
    (:completed :complete)
    (:cancelled :cancelled)
    (:failed :failed)
    (otherwise :complete)))

(defun %finish-agent! (agent status result stdout stderr error-message)
  (let ((now (%agent-now-ms)))
    (%with-agent-registry-lock ()
      (setf (agent-record-status agent) status
            (agent-record-finished-ms agent) now
            (agent-record-result agent) result
            (agent-record-stdout agent) stdout
            (agent-record-stderr agent) stderr
            (agent-record-error-message agent) error-message))
    (let ((elapsed-ms (if (plusp (agent-record-started-ms agent))
                          (max 0 (- now (agent-record-started-ms agent)))
                          0)))
      (usdt-probe-agent-lifecycle (%agent-status->probe-phase status)
                                  (agent-record-id agent)
                                  (agent-record-type agent)
                                  status
                                  elapsed-ms
                                  :parent-message-id
                                  (agent-record-parent-message-id agent)))
    (let ((payload (%agent-payload agent)))
      (ptui.runtime.queue:queue-push *agent-completion-queue* payload)
      (%publish-agent-event +event-type-agent-complete+
                            payload
                            :severity (if (eq status :failed) :error :info))
      payload)))

(defun %run-agent-worker (agent runner)
  (%with-agent-registry-lock ()
    (setf (agent-record-status agent) :running
          (agent-record-started-ms agent) (%agent-now-ms)))
  (usdt-probe-agent-lifecycle :start
                              (agent-record-id agent)
                              (agent-record-type agent)
                              :running
                              0
                              :parent-message-id (agent-record-parent-message-id agent))
  (let ((stdout-stream (make-string-output-stream))
        (stderr-stream (make-string-output-stream))
        (result nil)
        (status :completed)
        (error-message nil))
    (handler-case
        (let ((*standard-output* stdout-stream)
              (*error-output* stderr-stream))
          (setf result (funcall runner agent)
                status (%agent-runner-finished-status agent)))
      (agent-cancelled (condition)
        (setf status :cancelled
              error-message (princ-to-string condition)))
      (error (condition)
        (if (agent-record-cancel-requested-p agent)
            (setf status :cancelled
                  error-message (princ-to-string condition))
            (setf status :failed
                  error-message (princ-to-string condition)))))
    (%finish-agent! agent
                    status
                    result
                    (get-output-stream-string stdout-stream)
                    (get-output-stream-string stderr-stream)
                    error-message)))

(defun spawn-agent (task &key
                           (agent-type :task)
                           runner
                           parent-message-id
                           persona
                           system-prompt
                           worktree)
  "Spawn a background agent. PERSONA is a persona-definition struct.
   SYSTEM-PROMPT is an ad-hoc string override. Persona takes precedence."
  (let* ((normalized-type (%normalize-agent-type agent-type))
         (agent-id (%next-agent-id normalized-type))
         (worktree-metadata (coerce-worktree-metadata :worktree worktree))
         (agent (%make-agent-record
                 :id agent-id
                 :type normalized-type
                 :task task
                 :parent-message-id parent-message-id
                 :status :queued
                 :created-ms (%agent-now-ms)
                 :worktree worktree-metadata))
         (agent-runner (or runner #'%default-agent-runner))
         (thread
           (bordeaux-threads:make-thread
            (lambda ()
              (%run-agent-worker agent agent-runner))
            :name (format nil "amoebum-agent-~A" agent-id))))
    (declare (ignore persona system-prompt))
    (%with-agent-registry-lock ()
      (setf (gethash agent-id *agent-registry*) agent
            (agent-record-thread agent) thread))
    (usdt-probe-agent-lifecycle :spawn
                                (agent-record-id agent)
                                (agent-record-type agent)
                                (agent-record-status agent)
                                0
                                :parent-message-id (agent-record-parent-message-id agent))
    (%publish-agent-event +event-type-agent-spawn+ (%agent-payload agent) :severity :info)
    agent))

(defun drain-agent-completions ()
  (multiple-value-bind (events _count)
      (ptui.runtime.queue:queue-pop-all *agent-completion-queue*)
    (declare (ignore _count))
    events))
