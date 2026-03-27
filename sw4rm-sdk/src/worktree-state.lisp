;;;; worktree-state.lisp
;;;; Worktree state machine implementation per SW4RM spec §16

(in-package :sw4rm-sdk)

;;; Condition Types

(define-condition worktree-state-error (error)
  ((from-state :initarg :from-state :reader from-state)
   (to-state :initarg :to-state :reader to-state)
   (reason :initarg :reason :reader reason :initform nil))
  (:report (lambda (condition stream)
             (format stream "Invalid worktree state transition from ~A to ~A~@[: ~A~]"
                     (from-state condition)
                     (to-state condition)
                     (reason condition))))
  (:documentation "Signaled when an invalid worktree state transition is attempted."))

;;; State Definitions

(deftype worktree-state ()
  "Valid worktree binding states per SW4RM spec §16."
  '(member :unbound :bound-home :switch-pending :bound-non-home :bind-failed))

;;; Transition Matrix

(defparameter *worktree-transition-matrix*
  '((:unbound . (:bound-home :bind-failed))
    (:bound-home . (:switch-pending :unbound))
    (:switch-pending . (:bound-non-home :bound-home))
    (:bound-non-home . (:bound-home :unbound))
    (:bind-failed . (:unbound :bound-home)))
  "Transition matrix for worktree binding states per spec §16.")

;;; Transition Validation Functions

(defun valid-worktree-transitions (from-state)
  "Return a list of valid target states from FROM-STATE.

   Args:
     from-state: Current worktree state (keyword)

   Returns:
     List of valid target states, or NIL if no transitions allowed."
  (check-type from-state worktree-state)
  (cdr (assoc from-state *worktree-transition-matrix*)))

(defun valid-worktree-transition-p (from-state to-state)
  "Return T if transitioning from FROM-STATE to TO-STATE is valid.

   Args:
     from-state: Current worktree state (keyword)
     to-state: Target worktree state (keyword)

   Returns:
     T if transition is valid, NIL otherwise."
  (check-type from-state worktree-state)
  (check-type to-state worktree-state)
  (member to-state (valid-worktree-transitions from-state)))

;;; Binding Information Structure

(defstruct binding-info
  "Information about a worktree binding."
  (worktree-id nil :type (or null string))
  (repo-id nil :type (or null string))
  (branch nil :type (or null string))
  (bound-at nil :type (or null integer))
  (expires-at nil :type (or null integer)))

;;; Worktree State Machine Class

(defclass worktree-state-machine ()
  ((state
    :initarg :state
    :accessor worktree-state
    :type worktree-state
    :initform :unbound
    :documentation "Current worktree binding state.")

   (home-binding
    :initarg :home-binding
    :accessor home-binding
    :type (or null binding-info)
    :initform nil
    :documentation "Information about the home worktree binding.")

   (current-binding
    :initarg :current-binding
    :accessor current-binding
    :type (or null binding-info)
    :initform nil
    :documentation "Information about the current worktree binding.")

   (pending-switch
    :initarg :pending-switch
    :accessor pending-switch
    :type (or null binding-info)
    :initform nil
    :documentation "Information about a pending switch request.")

   (switch-ttl
    :initarg :switch-ttl
    :accessor switch-ttl
    :type (integer 0 *)
    :initform 3600
    :documentation "Time-to-live for non-home bindings in seconds (default 1 hour).")

   (state-history
    :initarg :state-history
    :accessor state-history
    :type list
    :initform nil
    :documentation "History of state transitions, newest first.")

   (lock
    :accessor state-lock
    :initform (bt:make-lock "worktree-state-lock")
    :documentation "Lock for thread-safe operations."))
  (:documentation "Worktree state machine managing worktree binding lifecycle.
                   Implements the complete state transition matrix from spec §16."))

;;; State Transition History

(defstruct worktree-transition-entry
  "Records a worktree state transition event."
  (from-state nil :type (or null worktree-state))
  (to-state nil :type worktree-state)
  (timestamp (get-universal-time) :type integer)
  (metadata nil :type list))

(defstruct (worktree-transition-plan
             (:constructor make-worktree-transition-plan
                 (&key to-state
                       (slot-updates '())
                       metadata)))
  to-state
  (slot-updates '() :type list)
  metadata)

(defun make-worktree-state-machine (&key
                                      (state :unbound)
                                      home-binding
                                      current-binding
                                      pending-switch
                                      (switch-ttl 3600)
                                      (state-history '()))
  "Construct a worktree state machine with optional initial state."
  (make-instance 'worktree-state-machine
                 :state state
                 :home-binding home-binding
                 :current-binding current-binding
                 :pending-switch pending-switch
                 :switch-ttl switch-ttl
                 :state-history state-history))

(defun %copy-binding-info (binding)
  (when binding
    (make-binding-info
     :worktree-id (binding-info-worktree-id binding)
     :repo-id (binding-info-repo-id binding)
     :branch (binding-info-branch binding)
     :bound-at (binding-info-bound-at binding)
     :expires-at (binding-info-expires-at binding))))

(defun %apply-worktree-slot-updates! (wsm slot-updates)
  (loop for (key value) on (or slot-updates '()) by #'cddr
        do (case key
             (:home-binding
              (setf (home-binding wsm) value))
             (:current-binding
              (setf (current-binding wsm) value))
             (:pending-switch
              (setf (pending-switch wsm) value))
             (otherwise nil)))
  wsm)

(defun %apply-worktree-transition-plan! (wsm plan)
  (check-type wsm worktree-state-machine)
  (check-type plan worktree-transition-plan)
  (%transition-worktree-state wsm
                              (worktree-transition-plan-to-state plan)
                              :metadata (worktree-transition-plan-metadata plan))
  (%apply-worktree-slot-updates! wsm (worktree-transition-plan-slot-updates plan)))

(defun %plan-bind-home-transition (wsm &key worktree-id repo-id branch is-home)
  (declare (ignore wsm))
  (unless is-home
    (error "Non-home binding must go through switch approval process"))
  (let ((binding (make-binding-info
                  :worktree-id worktree-id
                  :repo-id repo-id
                  :branch branch
                  :bound-at (get-universal-time)
                  :expires-at nil)))
    (make-worktree-transition-plan
     :to-state :bound-home
     :slot-updates (list :home-binding binding
                         :current-binding binding
                         :pending-switch nil)
     :metadata (list :worktree-id worktree-id
                     :repo-id repo-id
                     :branch branch))))

(defun %plan-unbind-transition (wsm)
  (let ((old-binding (current-binding wsm)))
    (make-worktree-transition-plan
     :to-state :unbound
     :slot-updates (list :current-binding nil
                         :pending-switch nil)
     :metadata (list :old-worktree-id
                     (when old-binding
                       (binding-info-worktree-id old-binding))))))

(defun %plan-request-switch-transition (wsm &key worktree-id repo-id branch)
  (declare (ignore wsm))
  (let ((binding (make-binding-info
                  :worktree-id worktree-id
                  :repo-id repo-id
                  :branch branch
                  :bound-at nil
                  :expires-at nil)))
    (make-worktree-transition-plan
     :to-state :switch-pending
     :slot-updates (list :pending-switch binding)
     :metadata (list :worktree-id worktree-id
                     :repo-id repo-id
                     :branch branch))))

(defun %plan-approve-switch-transition (wsm &key now)
  (let ((pending (pending-switch wsm)))
    (unless pending
      (error 'worktree-state-error
             :from-state (worktree-state wsm)
             :to-state :bound-non-home
             :reason "No pending switch to approve"))
    (let* ((binding (%copy-binding-info pending))
           (bound-at (or now (get-universal-time)))
           (expires-at (+ bound-at (switch-ttl wsm))))
      (setf (binding-info-bound-at binding) bound-at
            (binding-info-expires-at binding) expires-at)
      (make-worktree-transition-plan
       :to-state :bound-non-home
       :slot-updates (list :current-binding binding
                           :pending-switch nil)
       :metadata (list :worktree-id (binding-info-worktree-id binding)
                       :expires-at expires-at)))))

(defun %plan-reject-switch-transition (wsm)
  (make-worktree-transition-plan
   :to-state :bound-home
   :slot-updates (list :current-binding (home-binding wsm)
                       :pending-switch nil)
   :metadata (list :reason "switch-rejected")))

(defun %plan-revert-home-transition (wsm)
  (make-worktree-transition-plan
   :to-state :bound-home
   :slot-updates (list :current-binding (home-binding wsm))
   :metadata (list :reason "reverted-to-home")))

(defparameter *worktree-transition-planners*
  (list (cons :bind-home #'%plan-bind-home-transition)
        (cons :unbind #'%plan-unbind-transition)
        (cons :request-switch #'%plan-request-switch-transition)
        (cons :approve-switch #'%plan-approve-switch-transition)
        (cons :reject-switch #'%plan-reject-switch-transition)
        (cons :revert-home #'%plan-revert-home-transition)))

(defun evaluate-worktree-transition (wsm event &rest args &key &allow-other-keys)
  "Return an explicit transition plan for the requested worktree EVENT."
  (let ((planner (cdr (assoc event *worktree-transition-planners* :test #'eq))))
    (unless planner
      (error "Unknown worktree transition event ~S." event))
    (apply planner wsm args)))

;;; Internal Transition Method

(defmethod %transition-worktree-state ((wsm worktree-state-machine) to-state &key metadata)
  "Internal method to perform state transition (assumes lock held).

   Args:
     wsm: Worktree state machine instance
     to-state: Target state
     metadata: Optional metadata plist

   Returns:
     The new state

   Signals:
     WORKTREE-STATE-ERROR: If transition is invalid"
  (let ((from-state (worktree-state wsm)))
    (unless (valid-worktree-transition-p from-state to-state)
      (error 'worktree-state-error
             :from-state from-state
             :to-state to-state
             :reason (format nil "No valid transition from ~A to ~A"
                            from-state to-state)))

    ;; Perform transition
    (setf (worktree-state wsm) to-state)

    ;; Record history
    (push (make-worktree-transition-entry
           :from-state from-state
           :to-state to-state
           :timestamp (get-universal-time)
           :metadata metadata)
          (state-history wsm))

    to-state))

;;; Public Methods

(defmethod bind-worktree ((wsm worktree-state-machine)
                         worktree-id repo-id branch
                         &key (is-home t))
  "Bind the agent to a worktree.

   Transitions from UNBOUND or BIND-FAILED to BOUND-HOME.

   Args:
     wsm: Worktree state machine instance
     worktree-id: Worktree identifier
     repo-id: Repository identifier
     branch: Git branch name
     is-home: If T, this is the home binding (default T)

   Returns:
     The new state

  Signals:
     WORKTREE-STATE-ERROR: If transition is invalid"
  (bt:with-lock-held ((state-lock wsm))
    (%apply-worktree-transition-plan!
     wsm
     (evaluate-worktree-transition wsm
                                   :bind-home
                                   :worktree-id worktree-id
                                   :repo-id repo-id
                                   :branch branch
                                   :is-home is-home))))

(defmethod unbind-worktree ((wsm worktree-state-machine))
  "Unbind the agent from the current worktree.

   Transitions to UNBOUND state.

   Args:
     wsm: Worktree state machine instance

   Returns:
     The new state

  Signals:
     WORKTREE-STATE-ERROR: If transition is invalid"
  (bt:with-lock-held ((state-lock wsm))
    (%apply-worktree-transition-plan! wsm
                                      (evaluate-worktree-transition wsm :unbind))))

(defmethod request-switch ((wsm worktree-state-machine)
                          worktree-id repo-id branch)
  "Request a switch to a non-home worktree.

   Transitions from BOUND-HOME to SWITCH-PENDING.

   Args:
     wsm: Worktree state machine instance
     worktree-id: Target worktree identifier
     repo-id: Target repository identifier
     branch: Target git branch name

   Returns:
     The new state

  Signals:
     WORKTREE-STATE-ERROR: If transition is invalid"
  (bt:with-lock-held ((state-lock wsm))
    (%apply-worktree-transition-plan!
     wsm
     (evaluate-worktree-transition wsm
                                   :request-switch
                                   :worktree-id worktree-id
                                   :repo-id repo-id
                                   :branch branch))))

(defmethod approve-switch-local ((wsm worktree-state-machine))
  "Approve a pending worktree switch.

   Transitions from SWITCH-PENDING to BOUND-NON-HOME.

   Args:
     wsm: Worktree state machine instance

   Returns:
     The new state

  Signals:
     WORKTREE-STATE-ERROR: If transition is invalid or no pending switch"
  (bt:with-lock-held ((state-lock wsm))
    (%apply-worktree-transition-plan!
     wsm
     (evaluate-worktree-transition wsm
                                   :approve-switch
                                   :now (get-universal-time)))))

(defmethod reject-switch-local ((wsm worktree-state-machine))
  "Reject a pending worktree switch.

   Transitions from SWITCH-PENDING back to BOUND-HOME.

   Args:
     wsm: Worktree state machine instance

   Returns:
     The new state

  Signals:
     WORKTREE-STATE-ERROR: If transition is invalid"
  (bt:with-lock-held ((state-lock wsm))
    (%apply-worktree-transition-plan! wsm
                                      (evaluate-worktree-transition wsm :reject-switch))))

(defmethod revert-to-home ((wsm worktree-state-machine))
  "Revert from non-home binding to home binding.

   Transitions from BOUND-NON-HOME to BOUND-HOME (TTL expiry or manual revoke).

   Args:
     wsm: Worktree state machine instance

   Returns:
     The new state

  Signals:
     WORKTREE-STATE-ERROR: If transition is invalid"
  (bt:with-lock-held ((state-lock wsm))
    (%apply-worktree-transition-plan! wsm
                                      (evaluate-worktree-transition wsm :revert-home))))

(defmethod check-ttl-expiry ((wsm worktree-state-machine))
  "Check if the current non-home binding has expired.

   If expired, automatically revert to home.

   Args:
     wsm: Worktree state machine instance

   Returns:
     T if binding was expired and reverted, NIL otherwise"
  (bt:with-lock-held ((state-lock wsm))
    (when (and (eq (worktree-state wsm) :bound-non-home)
               (current-binding wsm))
      (let ((expires-at (binding-info-expires-at (current-binding wsm))))
        (when (and expires-at (>= (get-universal-time) expires-at))
          (revert-to-home wsm)
          t)))))

(defmethod get-current-worktree ((wsm worktree-state-machine))
  "Get the current worktree binding information.

   Args:
     wsm: Worktree state machine instance

   Returns:
     binding-info struct or NIL if unbound"
  (current-binding wsm))

(defmethod get-home-worktree ((wsm worktree-state-machine))
  "Get the home worktree binding information.

   Args:
     wsm: Worktree state machine instance

   Returns:
     binding-info struct or NIL if not set"
  (home-binding wsm))

(defmethod get-pending-switch ((wsm worktree-state-machine))
  "Get the pending switch request information.

   Args:
     wsm: Worktree state machine instance

   Returns:
     binding-info struct or NIL if no pending switch"
  (pending-switch wsm))

(defmethod print-object ((wsm worktree-state-machine) stream)
  "Print worktree state machine in readable format."
  (print-unreadable-object (wsm stream :type t)
    (format stream "state=~A" (worktree-state wsm))
    (when (current-binding wsm)
      (format stream " worktree=~A"
              (binding-info-worktree-id (current-binding wsm))))))
