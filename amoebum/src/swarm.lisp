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
                      state-machine result thread error-message)))
  (id "" :type string)
  (task "" :type string)
  (status :initializing :type keyword)
  (created-at (get-universal-time) :type integer)
  (finished-at nil :type (or null integer))
  (state-machine nil)
  (result nil)
  (thread nil)
  (error-message nil :type (or null string)))

(defvar *swarm-registry* (make-hash-table :test #'equal)
  "Hash-table of id -> swarm-agent.")

(defvar *swarm-counter* 0)

(defun %next-swarm-id ()
  (format nil "swarm-~A" (incf *swarm-counter*)))

;;; --- Lifecycle ---

(defun spawn-swarm-agent (task &key (id nil) (event-bus (current-event-bus)))
  "Spawn a new swarm sub-agent for TASK. Returns a swarm-agent."
  (let* ((agent-id (or id (%next-swarm-id)))
         (agent (make-swarm-agent :id agent-id :task task :status :initializing)))
    (setf (gethash agent-id *swarm-registry*) agent)
    ;; Create a state machine for the agent
    (handler-case
        (let ((sm (make-instance 'sw4rm-sdk::agent-state-machine)))
          (setf (swarm-agent-state-machine agent) sm)
          ;; Transition to RUNNABLE
          (handler-case
              (sw4rm-sdk::transition-to sm :runnable)
            (error () nil)))
      (error () nil))
    ;; Publish spawn event
    (publish event-bus
             (make-event :type +event-type-agent-spawn+
                         :source "swarm"
                         :payload (list :id agent-id :task task)))
    ;; Run task in background thread
    (setf (swarm-agent-thread agent)
          (bt:make-thread
           (lambda ()
             (handler-case
                 (progn
                   (setf (swarm-agent-status agent) :running)
                   ;; The actual task execution would go here
                   ;; For now, mark as completed with a placeholder result
                   (setf (swarm-agent-result agent)
                         (format nil "Agent ~A completed task: ~A" agent-id task)
                         (swarm-agent-status agent) :completed
                         (swarm-agent-finished-at agent) (get-universal-time))
                   (publish event-bus
                            (make-event :type +event-type-agent-complete+
                                        :source "swarm"
                                        :payload (list :id agent-id))))
               (error (c)
                 (setf (swarm-agent-status agent) :failed
                       (swarm-agent-error-message agent) (princ-to-string c)
                       (swarm-agent-finished-at agent) (get-universal-time)))))
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

(defun kill-swarm-agent (agent-id &key (event-bus (current-event-bus)))
  "Terminate a running swarm agent."
  (let ((agent (gethash agent-id *swarm-registry*)))
    (unless agent
      (error "Swarm agent ~A not found." agent-id))
    (let ((thread (swarm-agent-thread agent)))
      (when (and thread (bt:thread-alive-p thread))
        (bt:destroy-thread thread)))
    (setf (swarm-agent-status agent) :cancelled
          (swarm-agent-finished-at agent) (get-universal-time))
    (publish event-bus
             (make-event :type +event-type-agent-cancelled+
                         :source "swarm"
                         :payload (list :id agent-id)))
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
