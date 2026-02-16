;;;; local-router.lisp
;;;; In-process router + fair scheduler for local SW4RM mode.

(in-package :sw4rm-sdk)

(define-condition queue-full-error (sw4rm-error)
  ((agent-id
    :initarg :agent-id
    :reader queue-full-error-agent-id)
   (queue-size
    :initarg :queue-size
    :reader queue-full-error-queue-size)
   (queue-capacity
    :initarg :queue-capacity
    :reader queue-full-error-queue-capacity))
  (:default-initargs :error-code +buffer-full+)
  (:report (lambda (condition stream)
             (format stream "Queue full for ~A (~D/~D)"
                     (queue-full-error-agent-id condition)
                     (queue-full-error-queue-size condition)
                     (queue-full-error-queue-capacity condition)))))

(defstruct dead-letter
  "Dead-letter entry for unroutable or rejected envelopes."
  (envelope nil :type t)
  (reason :unknown :type keyword)
  (timestamp (get-universal-time) :type integer))

(defclass local-router ()
  ((queues
    :initform (make-hash-table :test #'equal)
    :accessor router-queues)
   (route-order
    :initform nil
    :accessor router-route-order)
   (next-index
    :initform 0
    :accessor router-next-index)
   (queue-capacity
    :initarg :queue-capacity
    :initform 64
    :accessor router-queue-capacity
    :type (integer 1 *))
   (dead-letters
    :initform nil
    :accessor router-dead-letters)
   (lock
    :initform (bt:make-lock "sw4rm-local-router-lock")
    :accessor router-lock))
  (:documentation "In-memory local router with per-agent bounded queues."))

(defun make-local-router (&key (queue-capacity 64))
  "Construct a local router."
  (make-instance 'local-router :queue-capacity queue-capacity))

(defun %router-normalize-agent-id (agent-id)
  (string-trim '(#\Space #\Tab #\Newline #\Return) (princ-to-string agent-id)))

(defun %router-target-agent-id (envelope explicit-target)
  (or explicit-target
      (and (hash-table-p envelope)
           (or (gethash "target-agent-id" envelope)
               (gethash "target_agent_id" envelope)))
      (and (listp envelope)
           (or (getf envelope :target-agent-id)
               (getf envelope :target_agent_id)))))

(defun register-route (router agent-id)
  "Register AGENT-ID in ROUTER and initialize an empty queue."
  (check-type router local-router)
  (let ((normalized (%router-normalize-agent-id agent-id)))
    (bt:with-lock-held ((router-lock router))
      (unless (gethash normalized (router-queues router))
        (setf (gethash normalized (router-queues router)) nil)
        (setf (router-route-order router)
              (append (router-route-order router) (list normalized)))))
    normalized))

(defun unregister-route (router agent-id)
  "Unregister AGENT-ID and discard pending queued envelopes."
  (check-type router local-router)
  (let ((normalized (%router-normalize-agent-id agent-id)))
    (bt:with-lock-held ((router-lock router))
      (remhash normalized (router-queues router))
      (setf (router-route-order router)
            (remove normalized (router-route-order router) :test #'string=))
      (when (>= (router-next-index router) (length (router-route-order router)))
        (setf (router-next-index router) 0))))
  t)

(defun router-queue-size (router agent-id)
  "Return queue depth for AGENT-ID."
  (check-type router local-router)
  (let ((normalized (%router-normalize-agent-id agent-id)))
    (bt:with-lock-held ((router-lock router))
      (length (gethash normalized (router-queues router) nil)))))

(defun route-envelope (router envelope &key target-agent-id)
  "Route ENVELOPE into the target agent queue.

Signals QUEUE-FULL-ERROR when destination queue is at capacity."
  (check-type router local-router)
  (let* ((target (%router-target-agent-id envelope target-agent-id))
         (normalized (and target (%router-normalize-agent-id target))))
    (unless (and normalized (> (length normalized) 0))
      (bt:with-lock-held ((router-lock router))
        (push (make-dead-letter :envelope envelope :reason :no-target)
              (router-dead-letters router)))
      (error 'validation-error
             :message "target-agent-id is required for routing"
             :field "target-agent-id"
             :constraint "non-empty string"))
    (bt:with-lock-held ((router-lock router))
      (multiple-value-bind (queue present-p)
          (gethash normalized (router-queues router))
        (unless present-p
          (push (make-dead-letter :envelope envelope :reason :no-route)
                (router-dead-letters router))
          (error 'sw4rm-error
                 :message (format nil "No route for target agent ~A" normalized)
                 :error-code +no-route+))
        (when (>= (length queue) (router-queue-capacity router))
          (push (make-dead-letter :envelope envelope :reason :queue-full)
                (router-dead-letters router))
          (error 'queue-full-error
                 :message (format nil "Queue capacity reached for ~A" normalized)
                 :agent-id normalized
                 :queue-size (length queue)
                 :queue-capacity (router-queue-capacity router)))
        (setf (gethash normalized (router-queues router))
              (append queue (list envelope)))
        envelope))))

(defun dequeue-envelope (router agent-id)
  "Pop the oldest envelope for AGENT-ID, or NIL."
  (check-type router local-router)
  (let ((normalized (%router-normalize-agent-id agent-id)))
    (bt:with-lock-held ((router-lock router))
      (let ((queue (gethash normalized (router-queues router))))
        (when queue
          (let ((item (first queue)))
            (setf (gethash normalized (router-queues router)) (rest queue))
            item))))))

(defun schedule-next-envelope (router)
  "Round-robin dequeue across all registered routes.

Returns two values: target-agent-id and envelope."
  (check-type router local-router)
  (bt:with-lock-held ((router-lock router))
    (let* ((order (router-route-order router))
           (count (length order)))
      (when (zerop count)
        (return-from schedule-next-envelope (values nil nil)))
      (loop for offset from 0 below count
            for idx = (mod (+ (router-next-index router) offset) count)
            for agent-id = (nth idx order)
            for queue = (gethash agent-id (router-queues router))
            when queue
              do (let ((envelope (first queue)))
                   (setf (gethash agent-id (router-queues router)) (rest queue))
                   (setf (router-next-index router) (mod (1+ idx) count))
                   (return (values agent-id envelope)))
            finally (return (values nil nil))))))

(defun dead-letter-entries (router &key (limit nil))
  "Return newest-first dead-letter entries."
  (check-type router local-router)
  (bt:with-lock-held ((router-lock router))
    (if limit
        (subseq (router-dead-letters router)
                0
                (min limit (length (router-dead-letters router))))
        (copy-list (router-dead-letters router)))))

(defun clear-dead-letters (router)
  "Clear dead-letter queue."
  (check-type router local-router)
  (bt:with-lock-held ((router-lock router))
    (setf (router-dead-letters router) nil))
  t)

(defun router-snapshot (router)
  "Return queue depths and dead-letter count."
  (check-type router local-router)
  (bt:with-lock-held ((router-lock router))
    (let ((depths nil))
      (dolist (agent (router-route-order router))
        (push (cons agent (length (gethash agent (router-queues router))))
              depths))
      (list :queue-depths (nreverse depths)
            :dead-letter-count (length (router-dead-letters router))))))
