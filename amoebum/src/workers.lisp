(in-package :amoebum)

;;; ============================================================
;;; I258: Unified Worker-Supervisor Protocol
;;;
;;; A single protocol for managing background work units,
;;; unifying shell background tasks, agents, and (soon) overwatch
;;; delegated processes under one interface.
;;; ============================================================

;;; --- Worker record ---

(defstruct (worker-record
            (:constructor %make-worker-record
                (&key id type label command status
                      created-at started-at finished-at
                      result output-buffer exit-code
                      error-message retry-count max-retries
                      backend inner-id)))
  (id nil :type (or null string))
  (type :shell :type keyword)             ; :shell, :agent, :process
  (label "" :type string)                 ; human-readable description
  (command nil)                           ; command string or task spec
  (status :pending :type keyword)         ; :pending :running :completed :failed :timeout :cancelled
  (created-at 0 :type integer)
  (started-at 0 :type integer)
  (finished-at 0 :type integer)
  (result nil)                            ; structured result plist
  (output-buffer nil :type (or null string))
  (exit-code nil :type (or null integer))
  (error-message nil :type (or null string))
  (retry-count 0 :type integer)
  (max-retries 0 :type integer)
  (backend :in-process :type keyword)     ; :in-process, :overwatch
  (inner-id nil :type (or null string)))  ; underlying shell-task-id or agent-id

;;; --- Worker event types ---

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %worker-event-keyword (name)
    (intern (string-upcase name) :keyword)))

(defparameter +event-type-worker-spawned+
  (%worker-event-keyword "worker:spawned"))

(defparameter +event-type-worker-started+
  (%worker-event-keyword "worker:started"))

(defparameter +event-type-worker-completed+
  (%worker-event-keyword "worker:completed"))

(defparameter +event-type-worker-failed+
  (%worker-event-keyword "worker:failed"))

(defparameter +event-type-worker-cancelled+
  (%worker-event-keyword "worker:cancelled"))

(defparameter +event-type-worker-retry+
  (%worker-event-keyword "worker:retry"))

;;; --- Worker registry ---

(defparameter *worker-registry* (make-hash-table :test #'equal))

#+sb-thread
(defparameter *worker-registry-lock*
  (sb-thread:make-mutex :name "amoebum-worker-registry-lock"))

(defparameter *next-worker-sequence* 0)

(defmacro %with-worker-lock (&body body)
  #+sb-thread
  `(sb-thread:with-mutex (*worker-registry-lock*) ,@body)
  #-sb-thread
  `(progn ,@body))

(defun %next-worker-id (worker-type)
  (%with-worker-lock
    (incf *next-worker-sequence*)
    (format nil "w-~(~A~)-~4,'0D"
            (or worker-type :shell)
            *next-worker-sequence*)))

(defun %worker-now ()
  (get-universal-time))

;;; --- Worker CLOS protocol ---

(defclass worker-supervisor () ())

(defgeneric supervisor-spawn (supervisor type command &key)
  (:documentation
   "Spawn a background worker. TYPE is :shell, :agent, or :process.
    COMMAND is a string (shell) or task description (agent).
    Accepts :persona and :system-prompt-override for agent workers.
    Returns a worker-record immediately."))

(defgeneric supervisor-status (supervisor worker-id)
  (:documentation
   "Return the current status keyword for WORKER-ID."))

(defgeneric supervisor-result (supervisor worker-id)
  (:documentation
   "Return the result plist for WORKER-ID. NIL if not yet finished."))

(defgeneric supervisor-cancel (supervisor worker-id)
  (:documentation
   "Request cancellation of WORKER-ID. Returns T if cancellation was requested."))

(defgeneric supervisor-list (supervisor &key include-finished)
  (:documentation
   "Return a list of worker-record snapshots."))

(defgeneric supervisor-output (supervisor worker-id)
  (:documentation
   "Return the current output buffer for WORKER-ID."))

;;; --- Registry helpers ---

(defun %store-worker (worker)
  (%with-worker-lock
    (setf (gethash (worker-record-id worker) *worker-registry*) worker))
  worker)

(defun %find-worker (worker-id)
  (%with-worker-lock
    (gethash worker-id *worker-registry*)))

(defun %worker-snapshot (worker)
  "Return a plist snapshot of a worker-record."
  (%with-worker-lock
    (list :id (worker-record-id worker)
          :type (worker-record-type worker)
          :label (worker-record-label worker)
          :status (worker-record-status worker)
          :backend (worker-record-backend worker)
          :created-at (worker-record-created-at worker)
          :started-at (worker-record-started-at worker)
          :finished-at (worker-record-finished-at worker)
          :exit-code (worker-record-exit-code worker)
          :error-message (worker-record-error-message worker)
          :retry-count (worker-record-retry-count worker)
          :inner-id (worker-record-inner-id worker))))

(defun %publish-worker-event (event-type worker &key (severity :info))
  (ignore-errors
    (publish (current-event-bus)
             event-type
             :source :worker-supervisor
             :severity severity
             :payload (%worker-snapshot worker)))
  nil)

(defun %worker-terminal-status-p (status)
  (member status '(:completed :failed :timeout :cancelled) :test #'eq))

;;; --- In-process backend ---

(defclass in-process-supervisor (worker-supervisor) ())

(defmethod supervisor-spawn ((supervisor in-process-supervisor) type command
                             &key label timeout-seconds max-output-chars
                                  cwd max-retries
                                  persona system-prompt-override)
  (let* ((worker-id (%next-worker-id type))
         (now (%worker-now))
         (worker (%make-worker-record
                  :id worker-id
                  :type type
                  :label (or label (format nil "~A: ~A"
                                           (string-downcase (symbol-name type))
                                           (if (stringp command)
                                               (subseq command 0 (min 60 (length command)))
                                               "task")))
                  :command command
                  :status :pending
                  :created-at now
                  :backend :in-process
                  :max-retries (or max-retries 0))))
    (%store-worker worker)
    (%publish-worker-event +event-type-worker-spawned+ worker)
    (ecase type
      (:shell
       (%spawn-shell-worker worker command
                            :cwd cwd
                            :timeout-seconds (or timeout-seconds 120)
                            :max-output-chars (or max-output-chars 8192)))
      (:agent
       (%spawn-agent-worker worker command
                            :persona persona
                            :system-prompt-override system-prompt-override))
      (:process
       (%spawn-shell-worker worker command
                            :cwd cwd
                            :timeout-seconds (or timeout-seconds 600)
                            :max-output-chars (or max-output-chars 65536))))
    worker))

(defun %spawn-shell-worker (worker command &key cwd timeout-seconds max-output-chars)
  #-sb-thread
  (error "Background shell workers require SBCL thread support.")
  #+sb-thread
  (let ((worker-id (worker-record-id worker)))
    (sb-thread:make-thread
     (lambda ()
       (%with-worker-lock
         (setf (worker-record-status worker) :running
               (worker-record-started-at worker) (%worker-now)))
       (%publish-worker-event +event-type-worker-started+ worker)
       (let ((result
               (handler-case
                   (%run-shell-command command
                                       (or cwd (%current-shell-directory))
                                       timeout-seconds
                                       max-output-chars)
                 (error (c)
                   (list :status :failed
                         :command (if (stringp command) command "")
                         :stdout ""
                         :stderr (princ-to-string c)
                         :exit-code 1
                         :timed-out nil)))))
         (let ((status (getf result :status))
               (exit-code (getf result :exit-code)))
           (%with-worker-lock
             (setf (worker-record-result worker) result
                   (worker-record-output-buffer worker)
                   (format nil "~A~@[~%~A~]"
                           (or (getf result :stdout) "")
                           (let ((err (getf result :stderr)))
                             (when (and (stringp err) (plusp (length err)))
                               err)))
                   (worker-record-exit-code worker) exit-code
                   (worker-record-finished-at worker) (%worker-now)
                   (worker-record-status worker)
                   (cond
                     ((eq status :timeout) :timeout)
                     ((and (integerp exit-code) (zerop exit-code)) :completed)
                     (t :failed))))
           (%publish-worker-event
            (if (eq (worker-record-status worker) :completed)
                +event-type-worker-completed+
                +event-type-worker-failed+)
            worker
            :severity (if (eq (worker-record-status worker) :completed)
                          :info :error)))))
     :name (format nil "amoebum-worker-~A" worker-id))))

(defun %spawn-agent-worker (worker command &key persona system-prompt-override)
  (let* ((agent (spawn-agent (if (stringp command)
                                 command
                                 (princ-to-string command))
                             :agent-type :task
                             :persona persona
                             :system-prompt (or system-prompt-override nil)))
         (agent-id (agent-record-id agent)))
    (%with-worker-lock
      (setf (worker-record-inner-id worker) agent-id
            (worker-record-status worker) :running
            (worker-record-started-at worker) (%worker-now)))
    (%publish-worker-event +event-type-worker-started+ worker)
    ;; Monitor agent completion in a thread
    #+sb-thread
    (sb-thread:make-thread
     (lambda ()
       (loop
         (let ((agent-rec (find-agent agent-id)))
           (unless agent-rec (return))
           (when (member (agent-record-status agent-rec)
                         '(:completed :failed :cancelled) :test #'eq)
             (%with-worker-lock
               (setf (worker-record-finished-at worker) (%worker-now)
                     (worker-record-status worker)
                     (case (agent-record-status agent-rec)
                       (:completed :completed)
                       (:cancelled :cancelled)
                       (otherwise :failed))
                     (worker-record-result worker)
                     (list :agent-id agent-id
                           :status (agent-record-status agent-rec)
                           :result (agent-record-result agent-rec))
                     (worker-record-output-buffer worker)
                     (%agent-output-string agent-rec)
                     (worker-record-error-message worker)
                     (agent-record-error-message agent-rec)))
             (%publish-worker-event
              (case (worker-record-status worker)
                (:completed +event-type-worker-completed+)
                (:cancelled +event-type-worker-cancelled+)
                (otherwise +event-type-worker-failed+))
              worker
              :severity (if (eq (worker-record-status worker) :completed)
                            :info :error))
             (return)))
         (sleep 0.5)))
     :name (format nil "amoebum-worker-monitor-~A" (worker-record-id worker)))))

(defmethod supervisor-status ((supervisor in-process-supervisor) worker-id)
  (let ((worker (%find-worker worker-id)))
    (if worker
        (worker-record-status worker)
        nil)))

(defmethod supervisor-result ((supervisor in-process-supervisor) worker-id)
  (let ((worker (%find-worker worker-id)))
    (when (and worker (%worker-terminal-status-p (worker-record-status worker)))
      (worker-record-result worker))))

(defmethod supervisor-cancel ((supervisor in-process-supervisor) worker-id)
  (let ((worker (%find-worker worker-id)))
    (unless worker (return-from supervisor-cancel nil))
    (when (%worker-terminal-status-p (worker-record-status worker))
      (return-from supervisor-cancel nil))
    ;; Delegate cancellation to underlying system
    (let ((inner-id (worker-record-inner-id worker)))
      (when (and inner-id (eq (worker-record-type worker) :agent))
        (cancel-agent inner-id)))
    (%with-worker-lock
      (setf (worker-record-status worker) :cancelled
            (worker-record-finished-at worker) (%worker-now)))
    (%publish-worker-event +event-type-worker-cancelled+ worker :severity :warning)
    t))

(defmethod supervisor-list ((supervisor in-process-supervisor) &key (include-finished t))
  (%with-worker-lock
    (let ((workers '()))
      (maphash (lambda (_id worker)
                 (declare (ignore _id))
                 (when (or include-finished
                           (not (%worker-terminal-status-p
                                 (worker-record-status worker))))
                   (push worker workers)))
               *worker-registry*)
      (sort workers #'< :key #'worker-record-created-at))))

(defmethod supervisor-output ((supervisor in-process-supervisor) worker-id)
  (let ((worker (%find-worker worker-id)))
    (when worker
      (worker-record-output-buffer worker))))

;;; --- Default supervisor ---

(defvar *worker-supervisor* nil
  "The active worker-supervisor instance.")

(defun ensure-worker-supervisor ()
  (or *worker-supervisor*
      (setf *worker-supervisor* (make-instance 'in-process-supervisor))))

(defun spawn-worker (type command &rest args &key label timeout-seconds
                                                   max-output-chars cwd max-retries
                                                   persona system-prompt-override)
  "Spawn a background worker through the active supervisor.
   TYPE: :shell, :agent, or :process.
   COMMAND: shell command string or agent task description.
   For :agent type, accepts :persona and :system-prompt-override.
   Returns a worker-record."
  (declare (ignore label timeout-seconds max-output-chars cwd max-retries
                   persona system-prompt-override))
  (apply #'supervisor-spawn (ensure-worker-supervisor) type command args))

(defun worker-status (worker-id)
  "Return status keyword for WORKER-ID, or NIL if not found."
  (supervisor-status (ensure-worker-supervisor) worker-id))

(defun worker-result (worker-id)
  "Return result plist for WORKER-ID, or NIL if not finished."
  (supervisor-result (ensure-worker-supervisor) worker-id))

(defun worker-cancel (worker-id)
  "Cancel WORKER-ID. Returns T if cancellation was requested."
  (supervisor-cancel (ensure-worker-supervisor) worker-id))

(defun worker-list (&key (include-finished t))
  "Return list of worker-records."
  (supervisor-list (ensure-worker-supervisor)
                   :include-finished include-finished))

(defun worker-output (worker-id)
  "Return output buffer string for WORKER-ID."
  (supervisor-output (ensure-worker-supervisor) worker-id))

(defun active-worker-count ()
  "Return count of non-terminal workers."
  (length (worker-list :include-finished nil)))

(defun clear-workers ()
  "Clear the worker registry."
  (%with-worker-lock
    (clrhash *worker-registry*)
    (setf *next-worker-sequence* 0))
  t)

;;; --- Await workers ---

(defun await-worker (worker-id &key (timeout-seconds 600) (poll-interval 0.5))
  "Block until WORKER-ID reaches a terminal status or timeout.
   Returns (values status result)."
  (let ((deadline (+ (get-universal-time) timeout-seconds)))
    (loop
      (let ((status (worker-status worker-id)))
        (when (or (null status) (%worker-terminal-status-p status))
          (return (values status (worker-result worker-id))))
        (when (>= (get-universal-time) deadline)
          (return (values :timeout nil)))
        (sleep poll-interval)))))

(defun await-workers (worker-ids &key (timeout-seconds 600) (poll-interval 0.5))
  "Block until all WORKER-IDS reach terminal status or timeout.
   Returns list of (worker-id status result) triples in input order."
  (let ((deadline (+ (get-universal-time) timeout-seconds)))
    (loop
      (let ((all-done t)
            (results '()))
        (dolist (wid worker-ids)
          (let ((status (worker-status wid)))
            (unless (or (null status) (%worker-terminal-status-p status))
              (setf all-done nil))
            (push (list wid status (worker-result wid)) results)))
        (when all-done
          (return (nreverse results)))
        (when (>= (get-universal-time) deadline)
          ;; Cancel remaining non-terminal workers
          (dolist (wid worker-ids)
            (let ((status (worker-status wid)))
              (when (and status (not (%worker-terminal-status-p status)))
                (worker-cancel wid))))
          (return (nreverse
                   (mapcar (lambda (wid)
                             (list wid (worker-status wid) (worker-result wid)))
                           worker-ids))))
        (sleep poll-interval)))))

(defun await-any-worker (worker-ids &key (timeout-seconds 600) (poll-interval 0.5))
  "Block until any WORKER-ID reaches terminal status. Returns first completed.
   Returns (values worker-id status result)."
  (let ((deadline (+ (get-universal-time) timeout-seconds)))
    (loop
      (dolist (wid worker-ids)
        (let ((status (worker-status wid)))
          (when (and status (%worker-terminal-status-p status))
            (return-from await-any-worker
              (values wid status (worker-result wid))))))
      (when (>= (get-universal-time) deadline)
        (return-from await-any-worker (values nil :timeout nil)))
      (sleep poll-interval))))
