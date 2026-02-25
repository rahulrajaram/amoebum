(in-package :amoebum)

(defparameter +event-type-tool-started-event+ :tool-started)
(defparameter +event-type-tool-completed-event+ :tool-completed)
(defparameter +event-type-tool-error-event+ :tool-error)
(defparameter +event-type-llm-request-event+ :llm-request)
(defparameter +event-type-llm-response-event+ :llm-response)
(defparameter +event-type-conversation-step-event+ :conversation-step)

(defparameter +typed-event-types+
  (list +event-type-tool-started-event+
        +event-type-tool-completed-event+
        +event-type-tool-error-event+
        +event-type-llm-request-event+
        +event-type-llm-response-event+
        +event-type-conversation-step-event+))

(defun event-type-p (value)
  (and (keywordp value)
       (member value +typed-event-types+ :test #'eq)))

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
