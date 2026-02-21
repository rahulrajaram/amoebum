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
        :parent-message-id (agent-record-parent-message-id agent)))

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

(defun %finish-agent! (agent status result stdout stderr error-message)
  (let ((now (%agent-now-ms)))
    (%with-agent-registry-lock ()
      (setf (agent-record-status agent) status
            (agent-record-finished-ms agent) now
            (agent-record-result agent) result
            (agent-record-stdout agent) stdout
            (agent-record-stderr agent) stderr
            (agent-record-error-message agent) error-message))
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
                           system-prompt)
  "Spawn a background agent. PERSONA is a persona-definition struct.
   SYSTEM-PROMPT is an ad-hoc string override. Persona takes precedence."
  (let* ((normalized-type (%normalize-agent-type agent-type))
         (agent-id (%next-agent-id normalized-type))
         (agent (%make-agent-record
                 :id agent-id
                 :type normalized-type
                 :task task
                 :parent-message-id parent-message-id
                 :status :queued
                 :created-ms (%agent-now-ms)))
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
    (%publish-agent-event +event-type-agent-spawn+ (%agent-payload agent) :severity :info)
    agent))

(defun drain-agent-completions ()
  (multiple-value-bind (events _count)
      (ptui.runtime.queue:queue-pop-all *agent-completion-queue*)
    (declare (ignore _count))
    events))
