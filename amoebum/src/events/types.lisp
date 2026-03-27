(in-package :amoebum)

(defparameter +event-type-git-commit+ (%event-type-keyword "git:commit"))
(defparameter +event-type-git-branch+ (%event-type-keyword "git:branch"))
(defparameter +event-type-agent-spawned+ (%event-type-keyword "agent:spawned"))
(defparameter +event-type-agent-completed+ (%event-type-keyword "agent:completed"))
(defparameter +event-type-agent-error+ (%event-type-keyword "agent:error"))
(defparameter +event-type-user-handoff-requested+ (%event-type-keyword "user:handoff-requested"))
(defparameter +event-type-user-handoff-accepted+ (%event-type-keyword "user:handoff-accepted"))
(defparameter +event-type-user-handoff-rejected+ (%event-type-keyword "user:handoff-rejected"))
(defparameter +event-type-user-handoff-completed+ (%event-type-keyword "user:handoff-completed"))
(defparameter +event-type-user-negotiation-room-created+ (%event-type-keyword "user:negotiation-room-created"))
(defparameter +event-type-user-negotiation-artifact-submitted+ (%event-type-keyword "user:negotiation-artifact-submitted"))
(defparameter +event-type-user-negotiation-critique-added+ (%event-type-keyword "user:negotiation-critique-added"))
(defparameter +event-type-user-negotiation-decision+ (%event-type-keyword "user:negotiation-decision"))

(defparameter +lifecycle-event-types+
  (list +event-type-git-commit+
        +event-type-git-branch+
        +event-type-agent-spawned+
        +event-type-agent-completed+
        +event-type-agent-error+
        +event-type-user-handoff-requested+
        +event-type-user-handoff-accepted+
        +event-type-user-handoff-rejected+
        +event-type-user-handoff-completed+
        +event-type-user-negotiation-room-created+
        +event-type-user-negotiation-artifact-submitted+
        +event-type-user-negotiation-critique-added+
        +event-type-user-negotiation-decision+))

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

;;; Phase 9 I217 — Typed event structs

(defparameter +event-type-tool-started-event+ (%event-type-keyword "tool-started"))
(defparameter +event-type-tool-completed-event+ (%event-type-keyword "tool-completed"))
(defparameter +event-type-tool-error-event+ (%event-type-keyword "tool-error"))
(defparameter +event-type-llm-request-event+ (%event-type-keyword "llm-request"))
(defparameter +event-type-llm-response-event+ (%event-type-keyword "llm-response"))
(defparameter +event-type-conversation-step-event+ (%event-type-keyword "conversation-step"))

(defparameter +typed-event-types+
  (list +event-type-tool-started-event+
        +event-type-tool-completed-event+
        +event-type-tool-error-event+
        +event-type-llm-request-event+
        +event-type-llm-response-event+
        +event-type-conversation-step-event+))

(setf +core-event-types+
      (remove-duplicates (append +core-event-types+ +typed-event-types+)
                         :test #'eq))

(defstruct (tool-started-event
            (:constructor make-tool-started-event
                (&key tool-name arguments timestamp
                 (event-type +event-type-tool-started-event+))))
  tool-name
  arguments
  timestamp
  (event-type +event-type-tool-started-event+ :type keyword))

(defstruct (tool-completed-event
            (:constructor make-tool-completed-event-type
                (&key tool-name result elapsed-ms timestamp
                 (event-type +event-type-tool-completed-event+))))
  tool-name
  result
  elapsed-ms
  timestamp
  (event-type +event-type-tool-completed-event+ :type keyword))

(defstruct (tool-error-event
            (:constructor make-tool-error-event-type
                (&key tool-name condition restarts timestamp
                 (event-type +event-type-tool-error-event+))))
  tool-name
  condition
  restarts
  timestamp
  (event-type +event-type-tool-error-event+ :type keyword))

(defstruct (llm-request-event
            (:constructor make-llm-request-event
                (&key provider model message-count estimated-tokens timestamp
                 (event-type +event-type-llm-request-event+))))
  provider
  model
  message-count
  estimated-tokens
  timestamp
  (event-type +event-type-llm-request-event+ :type keyword))

(defstruct (llm-response-event
            (:constructor make-llm-response-event
                (&key provider model usage latency-ms timestamp
                 (event-type +event-type-llm-response-event+))))
  provider
  model
  usage
  latency-ms
  timestamp
  (event-type +event-type-llm-response-event+ :type keyword))

(defstruct (conversation-step-event
            (:constructor make-conversation-step-event
                (&key step-number role content-length timestamp
                 (event-type +event-type-conversation-step-event+))))
  step-number
  role
  content-length
  timestamp
  (event-type +event-type-conversation-step-event+ :type keyword))

(defun event-type-p (event-type)
  (handler-case
      (not (null (member (%normalize-event-type event-type)
                         +core-event-types+
                         :test #'eq)))
    (error ()
      nil)))
