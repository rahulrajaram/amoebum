(in-package :amoebum)

(defparameter +event-type-git-commit+ (%event-type-keyword "git:commit"))
(defparameter +event-type-git-branch+ (%event-type-keyword "git:branch"))
(defparameter +event-type-agent-spawned+ (%event-type-keyword "agent:spawned"))
(defparameter +event-type-agent-completed+ (%event-type-keyword "agent:completed"))
(defparameter +event-type-agent-error+ (%event-type-keyword "agent:error"))

(defparameter +lifecycle-event-types+
  (list +event-type-git-commit+
        +event-type-git-branch+
        +event-type-agent-spawned+
        +event-type-agent-completed+
        +event-type-agent-error+))

(setf +core-event-types+
      (remove-duplicates (append +core-event-types+ +lifecycle-event-types+)
                         :test #'eq))

(defstruct (commit-event
            (:constructor make-commit-event
                (&key hash message author files-changed
                 (event-type +event-type-git-commit+))))
  hash
  message
  author
  files-changed
  (event-type +event-type-git-commit+ :type keyword))

(defstruct (branch-event
            (:constructor make-branch-event
                (&key old-branch new-branch action
                 (event-type +event-type-git-branch+))))
  old-branch
  new-branch
  action
  (event-type +event-type-git-branch+ :type keyword))

(defstruct (agent-spawned-event
            (:constructor make-agent-spawned-event
                (&key agent-id agent-type parent-id
                 (event-type +event-type-agent-spawned+))))
  agent-id
  agent-type
  parent-id
  (event-type +event-type-agent-spawned+ :type keyword))

(defstruct (agent-completed-event
            (:constructor make-agent-completed-event
                (&key agent-id result-status elapsed-ms
                 (event-type +event-type-agent-completed+))))
  agent-id
  result-status
  elapsed-ms
  (event-type +event-type-agent-completed+ :type keyword))

(defstruct (agent-error-event
            (:constructor make-agent-error-event
                (&key agent-id condition
                 (event-type +event-type-agent-error+))))
  agent-id
  condition
  (event-type +event-type-agent-error+ :type keyword))

(defun event-type-p (event-type)
  (handler-case
      (not (null (member (%normalize-event-type event-type)
                         +core-event-types+
                         :test #'eq)))
    (error ()
      nil)))
