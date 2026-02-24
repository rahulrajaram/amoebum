(in-package :amoebum)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %event-type-keyword (name)
    (intern (string-upcase name) :keyword)))

(defparameter +event-type-tool-invoked+ (%event-type-keyword "tool:invoked"))
(defparameter +event-type-tool-completed+ (%event-type-keyword "tool:completed"))
(defparameter +event-type-tool-error+ (%event-type-keyword "tool:error"))
(defparameter +event-type-tool-redefined+ (%event-type-keyword "tool:redefined"))
(defparameter +event-type-config-changed+ (%event-type-keyword "config:changed"))
(defparameter +event-type-permission-prompted+ (%event-type-keyword "permission:prompted"))
(defparameter +event-type-permission-blocked+ (%event-type-keyword "permission:blocked"))
(defparameter +event-type-memory-updated+ (%event-type-keyword "memory:updated"))
(defparameter +event-type-memory-backend-selected+
  (%event-type-keyword "memory:backend-selected"))
(defparameter +event-type-context-compressed+ (%event-type-keyword "context:compressed"))
(defparameter +event-type-mcp-tool-discovered+ (%event-type-keyword "mcp:tool-discovered"))
(defparameter +event-type-mcp-tool-invoked+ (%event-type-keyword "mcp:tool-invoked"))
(defparameter +event-type-keymap-overlay-enter+ (%event-type-keyword "keymap-overlay-enter"))
(defparameter +event-type-keymap-overlay-exit+ (%event-type-keyword "keymap-overlay-exit"))
(defparameter +event-type-extension-loaded+ (%event-type-keyword "extension:loaded"))
(defparameter +event-type-extension-error+ (%event-type-keyword "extension:error"))
(defparameter +event-type-stream-budget-warning+ (%event-type-keyword "stream:budget-warning"))
(defparameter +event-type-conversation-forked+ (%event-type-keyword "conversation:forked"))
(defparameter +event-type-tool-call-started+ (%event-type-keyword "tool-call:started"))
(defparameter +event-type-tool-call-argument-complete+
  (%event-type-keyword "tool-call:argument-complete"))
(defparameter +event-type-session-checkpointed+ (%event-type-keyword "session:checkpointed"))
(defparameter +event-type-session-restored+ (%event-type-keyword "session:restored"))

(defparameter +core-event-types+
  (list +event-type-tool-invoked+
        +event-type-tool-completed+
        +event-type-tool-error+
        +event-type-tool-redefined+
        +event-type-config-changed+
        +event-type-permission-prompted+
        +event-type-permission-blocked+
        +event-type-memory-updated+
        +event-type-memory-backend-selected+
        +event-type-context-compressed+
        +event-type-mcp-tool-discovered+
        +event-type-mcp-tool-invoked+
        +event-type-keymap-overlay-enter+
        +event-type-keymap-overlay-exit+
        +event-type-extension-loaded+
        +event-type-extension-error+
        +event-type-stream-budget-warning+
        +event-type-conversation-forked+
        +event-type-tool-call-started+
        +event-type-tool-call-argument-complete+
        +event-type-session-checkpointed+
        +event-type-session-restored+))

(defparameter *event-bus* nil)

(defstruct (event (:constructor %make-event
                     (&key (type :unknown)
                      (seq 0)
                      (ts-mono 0)
                      (timestamp 0)
                      (source :unknown)
                      (severity :info)
                      payload)))
  (type :unknown :type keyword)
  (seq 0 :type (unsigned-byte 64))
  (ts-mono 0 :type (unsigned-byte 64))
  (timestamp 0 :type (unsigned-byte 64))
  (source :unknown :type keyword)
  (severity :info :type keyword)
  payload)

(defstruct (tool-invoked-payload
            (:constructor make-tool-invoked-payload
                (&key tool-name args permission-mode request-id)))
  tool-name
  args
  permission-mode
  request-id)

(defstruct (tool-completed-payload
            (:constructor make-tool-completed-payload
                (&key tool-name args result elapsed-ms request-id)))
  tool-name
  args
  result
  elapsed-ms
  request-id)

(defstruct (tool-error-payload
            (:constructor make-tool-error-payload
                (&key tool-name args condition condition-reason-code
                       elapsed-ms request-id)))
  tool-name
  args
  condition
  (condition-reason-code nil :type (or null keyword))
  elapsed-ms
  request-id)

(defstruct (tool-redefined-payload
            (:constructor make-tool-redefined-payload
                (&key tool-name old-metadata new-metadata metadata-diff)))
  tool-name
  old-metadata
  new-metadata
  metadata-diff)

(defstruct (config-changed-payload
            (:constructor make-config-changed-payload
                (&key key old-value new-value)))
  key
  old-value
  new-value)

(defstruct (permission-prompted-payload
            (:constructor make-permission-prompted-payload
                (&key tool-name path command reason permission-mode reason-code)))
  tool-name
  path
  command
  reason
  permission-mode
  reason-code)

(defstruct (permission-blocked-payload
            (:constructor make-permission-blocked-payload
                (&key tool-name path command reason actionable-reason permission-mode reason-code)))
  tool-name
  path
  command
  reason
  actionable-reason
  permission-mode
  reason-code)

(defstruct (memory-updated-payload
            (:constructor make-memory-updated-payload
                (&key backend operation key value)))
  backend
  operation
  key
  value)

(defstruct (memory-backend-selected-payload
            (:constructor make-memory-backend-selected-payload
                (&key backend reason requested-backend)))
  backend
  reason
  requested-backend)

(defstruct (context-compressed-payload
            (:constructor make-context-compressed-payload
                (&key
                   (before-tokens 0)
                   (after-tokens 0)
                   (saved-tokens 0)
                   (summarized-messages 0)
                   (kept-messages 0)
                   (trigger :auto))))
  (before-tokens 0 :type integer)
  (after-tokens 0 :type integer)
  (saved-tokens 0 :type integer)
  (summarized-messages 0 :type integer)
  (kept-messages 0 :type integer)
  (trigger :auto :type keyword))

(defstruct (mcp-tool-discovered-payload
            (:constructor make-mcp-tool-discovered-payload
                (&key server-name tool-name namespaced-name description)))
  server-name
  tool-name
  namespaced-name
  description)

(defstruct (mcp-tool-invoked-payload
            (:constructor make-mcp-tool-invoked-payload
                (&key server-name tool-name namespaced-name args request-id)))
  server-name
  tool-name
  namespaced-name
  args
  request-id)

(defstruct (extension-loaded-payload
            (:constructor make-extension-loaded-payload
                (&key path scope)))
  path
  scope)

(defstruct (extension-error-payload
            (:constructor make-extension-error-payload
                (&key path scope condition)))
  path
  scope
  condition)

(defstruct (stream-budget-warning-payload
            (:constructor make-stream-budget-warning-payload
                (&key
                   (used-tokens 0)
                   (limit-tokens 0)
                   (usage-percent 0)
                   (threshold-percent 90))))
  (used-tokens 0 :type integer)
  (limit-tokens 0 :type integer)
  (usage-percent 0 :type integer)
  (threshold-percent 90 :type integer))

(defstruct (tool-call-started-payload
            (:constructor make-tool-call-started-payload
                (&key tool-name tool-call-id arguments index)))
  tool-name
  tool-call-id
  arguments
  index)

(defstruct (tool-call-argument-complete-payload
            (:constructor make-tool-call-argument-complete-payload
                (&key tool-name tool-call-id arguments index)))
  tool-name
  tool-call-id
  arguments
  index)

(defstruct (session-checkpointed-payload
            (:constructor make-session-checkpointed-payload
                (&key checkpoint-id
                 path
                 (trigger :manual)
                 (auto-p nil)
                 (message-count 0)
                 (extension-count 0)
                 (tool-count 0)
                 (memory-count 0))))
  checkpoint-id
  path
  (trigger :manual :type keyword)
  (auto-p nil :type boolean)
  (message-count 0 :type integer)
  (extension-count 0 :type integer)
  (tool-count 0 :type integer)
  (memory-count 0 :type integer))

(defstruct (session-restored-payload
            (:constructor make-session-restored-payload
                (&key checkpoint-id
                 path
                 (trigger :manual)
                 (message-count 0)
                 (extension-count 0)
                 (tool-count 0)
                 (memory-count 0))))
  checkpoint-id
  path
  (trigger :manual :type keyword)
  (message-count 0 :type integer)
  (extension-count 0 :type integer)
  (tool-count 0 :type integer)
  (memory-count 0 :type integer))

(defstruct (event-subscription
            (:constructor %make-event-subscription
                (&key id event-type handler filter (priority 100) created-at)))
  id
  event-type
  handler
  filter
  (priority 100 :type integer)
  (created-at 0 :type integer))

(defstruct (event-bus
            (:constructor %make-event-bus
                (&key (capacity 4096)
                 (next-seq 0)
                 (next-subscription-id 0)
                 (history (make-array 0 :adjustable t :fill-pointer 0))
                 (subscriptions (make-hash-table :test #'eql))
                 (subscription-index (make-hash-table :test #'eql)))))
  (capacity 4096 :type (integer 1 *))
  (next-seq 0 :type (unsigned-byte 64))
  (next-subscription-id 0 :type integer)
  history
  subscriptions
  subscription-index)

(defun %normalize-keyword (value name)
  (cond
    ((keywordp value) value)
    ((stringp value) (intern (string-upcase value) :keyword))
    ((symbolp value) (intern (string-upcase (symbol-name value)) :keyword))
    (t (error "~A must be a keyword-like value, got ~S." name value))))

(defun %normalize-event-type (event-type)
  (%normalize-keyword event-type "EVENT-TYPE"))

(defun %normalize-subscription-event-type (event-type)
  (cond
    ((and (stringp event-type) (string= event-type "*"))
     :*)
    ((and (symbolp event-type) (string= (symbol-name event-type) "*"))
     :*)
    (t
     (%normalize-event-type event-type))))

(defun %monotonic-milliseconds ()
  (truncate (* 1000
               (/ (coerce (get-internal-real-time) 'double-float)
                  (coerce internal-time-units-per-second 'double-float)))))

(defun make-event (&key (type :unknown)
                     (source :unknown)
                     (severity :info)
                     payload
                     (seq 0)
                     (ts-mono 0)
                     (timestamp 0))
  (%make-event :type (%normalize-event-type type)
               :seq seq
               :ts-mono ts-mono
               :timestamp timestamp
               :source (%normalize-keyword source "SOURCE")
               :severity (%normalize-keyword severity "SEVERITY")
               :payload payload))

(defun make-event-bus (&key (capacity 4096))
  (unless (and (integerp capacity) (> capacity 0))
    (error "EVENT BUS CAPACITY must be a positive integer, got ~S." capacity))
  (%make-event-bus :capacity capacity))

(defun current-event-bus ()
  (or *event-bus*
      (setf *event-bus* (make-event-bus))))

(defun event-history (bus)
  (unless (event-bus-p bus)
    (error "BUS must be an EVENT-BUS, got ~S." bus))
  (loop for entry across (event-bus-history bus)
        collect entry))

(defun %next-sequence (bus)
  (let ((next-seq (1+ (event-bus-next-seq bus))))
    (setf (event-bus-next-seq bus) next-seq)
    next-seq))

(defun %next-subscription-id (bus)
  (let ((next-id (1+ (event-bus-next-subscription-id bus))))
    (setf (event-bus-next-subscription-id bus) next-id)
    next-id))

(defun %record-event-history (bus event)
  (let ((history (event-bus-history bus)))
    (vector-push-extend event history)
    (when (> (length history) (event-bus-capacity bus))
      (replace history history :start1 0 :start2 1)
      (setf (fill-pointer history) (1- (fill-pointer history)))))
  event)

(defun %priority-ordered-subscriptions (subscriptions)
  (sort subscriptions
        (lambda (left right)
          (if (= (event-subscription-priority left)
                 (event-subscription-priority right))
              (< (event-subscription-created-at left)
                 (event-subscription-created-at right))
              (< (event-subscription-priority left)
                 (event-subscription-priority right))))))

(defun %collect-subscriptions-for-type (bus event-type)
  (let ((table (event-bus-subscriptions bus)))
    (%priority-ordered-subscriptions
     (append (copy-list (gethash event-type table))
             (copy-list (gethash :* table))))))

(defun %dispatch-event (bus event)
  (dolist (subscription (%collect-subscriptions-for-type bus (event-type event)))
    (let ((filter (event-subscription-filter subscription)))
      (when (or (null filter)
                (funcall filter event))
        (funcall (event-subscription-handler subscription) event))))
  event)

(defun %coerce-published-event (event source source-supplied-p
                              severity severity-supplied-p
                              payload payload-supplied-p)
  (if (event-p event)
      (let ((copy (copy-event event)))
        (setf (event-type copy) (%normalize-event-type (event-type copy)))
        (when source-supplied-p
          (setf (event-source copy) (%normalize-keyword source "SOURCE")))
        (when severity-supplied-p
          (setf (event-severity copy) (%normalize-keyword severity "SEVERITY")))
        (when payload-supplied-p
          (setf (event-payload copy) payload))
        copy)
      (make-event :type event
                  :source (if source-supplied-p source :amoebum)
                  :severity (if severity-supplied-p severity :info)
                  :payload (if payload-supplied-p payload nil))))

(defun publish (bus event &key (source nil source-supplied-p)
                            (severity nil severity-supplied-p)
                            (payload nil payload-supplied-p))
  (unless (event-bus-p bus)
    (error "BUS must be an EVENT-BUS, got ~S." bus))
  (let* ((prepared (%coerce-published-event event
                                            source source-supplied-p
                                            severity severity-supplied-p
                                            payload payload-supplied-p))
         (sequence-number (%next-sequence bus)))
    (setf (event-seq prepared) sequence-number
          (event-ts-mono prepared) (%monotonic-milliseconds)
          (event-timestamp prepared) (get-universal-time))
    (%record-event-history bus prepared)
    (%dispatch-event bus prepared)
    sequence-number))

(defun subscribe (bus event-type handler &key filter (priority 100))
  (unless (event-bus-p bus)
    (error "BUS must be an EVENT-BUS, got ~S." bus))
  (unless (functionp handler)
    (error "HANDLER must be a function, got ~S." handler))
  (when filter
    (unless (functionp filter)
      (error "FILTER must be a function or NIL, got ~S." filter)))
  (unless (integerp priority)
    (error "PRIORITY must be an integer, got ~S." priority))
  (let* ((normalized-type (%normalize-subscription-event-type event-type))
         (id (%next-subscription-id bus))
         (subscription (%make-event-subscription
                        :id id
                        :event-type normalized-type
                        :handler handler
                        :filter filter
                        :priority priority
                        :created-at id)))
    (push subscription (gethash normalized-type (event-bus-subscriptions bus)))
    (setf (gethash id (event-bus-subscription-index bus)) normalized-type)
    id))

(defun unsubscribe (bus subscription-id)
  (unless (event-bus-p bus)
    (error "BUS must be an EVENT-BUS, got ~S." bus))
  (let* ((index (event-bus-subscription-index bus))
         (event-type (gethash subscription-id index)))
    (if (null event-type)
        nil
        (let* ((table (event-bus-subscriptions bus))
               (subscriptions (gethash event-type table))
               (remaining (remove subscription-id
                                  subscriptions
                                  :key #'event-subscription-id
                                  :test #'eql)))
          (setf (gethash event-type table) remaining)
          (remhash subscription-id index)
          t))))

(defun make-tool-invoked-event (&key tool-name args permission-mode request-id)
  (make-event :type +event-type-tool-invoked+
              :source :amoebum
              :severity :debug
              :payload (make-tool-invoked-payload
                        :tool-name tool-name
                        :args args
                        :permission-mode permission-mode
                        :request-id request-id)))

(defun make-tool-completed-event (&key tool-name args result elapsed-ms request-id)
  (make-event :type +event-type-tool-completed+
              :source :amoebum
              :severity :info
              :payload (make-tool-completed-payload
                        :tool-name tool-name
                        :args args
                        :result result
                        :elapsed-ms elapsed-ms
                        :request-id request-id)))

(defun make-tool-error-event (&key tool-name args condition elapsed-ms request-id
                               condition-reason-code)
  (make-event :type +event-type-tool-error+
              :source :amoebum
              :severity :error
              :payload (make-tool-error-payload
                        :tool-name tool-name
                        :args args
                        :condition condition
                        :condition-reason-code condition-reason-code
                        :elapsed-ms elapsed-ms
                        :request-id request-id)))

(defun make-tool-redefined-event (&key tool-name old-metadata new-metadata metadata-diff)
  (make-event :type +event-type-tool-redefined+
              :source :amoebum
              :severity :info
              :payload (make-tool-redefined-payload
                        :tool-name tool-name
                        :old-metadata old-metadata
                        :new-metadata new-metadata
                        :metadata-diff metadata-diff)))

(defun make-config-changed-event (&key key old-value new-value)
  (make-event :type +event-type-config-changed+
              :source :amoebum
              :severity :info
              :payload (make-config-changed-payload
                        :key key
                        :old-value old-value
                        :new-value new-value)))

(defun make-permission-prompted-event (&key tool-name path command reason permission-mode
                                         reason-code)
  (make-event :type +event-type-permission-prompted+
              :source :amoebum
              :severity :warning
              :payload (make-permission-prompted-payload
                        :tool-name tool-name
                        :path path
                        :command command
                        :reason reason
                        :reason-code reason-code
                        :permission-mode permission-mode)))

(defun make-permission-blocked-event (&key tool-name path command reason actionable-reason
                                           permission-mode reason-code)
  (make-event :type +event-type-permission-blocked+
              :source :amoebum
              :severity :warning
              :payload (make-permission-blocked-payload
                        :tool-name tool-name
                        :path path
                        :command command
                        :reason reason
                        :actionable-reason actionable-reason
                        :reason-code reason-code
                        :permission-mode permission-mode)))

(defun make-memory-updated-event (&key backend operation key value)
  (make-event :type +event-type-memory-updated+
              :source :amoebum
              :severity :info
              :payload (make-memory-updated-payload
                        :backend backend
                        :operation operation
                        :key key
                        :value value)))

(defun make-memory-backend-selected-event (&key backend reason requested-backend)
  (make-event :type +event-type-memory-backend-selected+
              :source :amoebum
              :severity :info
              :payload (make-memory-backend-selected-payload
                        :backend backend
                        :reason reason
                        :requested-backend requested-backend)))

(defun make-context-compressed-event (&key before-tokens
                                           after-tokens
                                           saved-tokens
                                           summarized-messages
                                           kept-messages
                                           (trigger :auto))
  (make-event :type +event-type-context-compressed+
              :source :amoebum
              :severity :info
              :payload (make-context-compressed-payload
                        :before-tokens before-tokens
                        :after-tokens after-tokens
                        :saved-tokens saved-tokens
                        :summarized-messages summarized-messages
                        :kept-messages kept-messages
                        :trigger trigger)))

(defun make-mcp-tool-discovered-event (&key server-name
                                            tool-name
                                            namespaced-name
                                            description)
  (make-event :type +event-type-mcp-tool-discovered+
              :source :amoebum
              :severity :info
              :payload (make-mcp-tool-discovered-payload
                        :server-name server-name
                        :tool-name tool-name
                        :namespaced-name namespaced-name
                        :description description)))

(defun make-mcp-tool-invoked-event (&key server-name
                                         tool-name
                                         namespaced-name
                                         args
                                         request-id)
  (make-event :type +event-type-mcp-tool-invoked+
              :source :amoebum
              :severity :debug
              :payload (make-mcp-tool-invoked-payload
                        :server-name server-name
                        :tool-name tool-name
                        :namespaced-name namespaced-name
                        :args args
                        :request-id request-id)))

(defun make-extension-loaded-event (&key path scope)
  (make-event :type +event-type-extension-loaded+
              :source :amoebum
              :severity :info
              :payload (make-extension-loaded-payload
                        :path path
                        :scope scope)))

(defun make-extension-error-event (&key path scope condition)
  (make-event :type +event-type-extension-error+
              :source :amoebum
              :severity :error
              :payload (make-extension-error-payload
                        :path path
                        :scope scope
                        :condition condition)))

(defun make-stream-budget-warning-event (&key
                                           used-tokens
                                           limit-tokens
                                           usage-percent
                                           (threshold-percent 90))
  (make-event :type +event-type-stream-budget-warning+
              :source :amoebum
              :severity :warning
              :payload (make-stream-budget-warning-payload
                        :used-tokens used-tokens
                        :limit-tokens limit-tokens
                        :usage-percent usage-percent
                        :threshold-percent threshold-percent)))

(defun make-tool-call-started-event (&key
                                       tool-name
                                       tool-call-id
                                       arguments
                                       index)
  (make-event :type +event-type-tool-call-started+
              :source :amoebum
              :severity :info
              :payload (make-tool-call-started-payload
                        :tool-name tool-name
                        :tool-call-id tool-call-id
                        :arguments arguments
                        :index index)))

(defun make-tool-call-argument-complete-event (&key
                                                 tool-name
                                                 tool-call-id
                                                 arguments
                                                 index)
  (make-event :type +event-type-tool-call-argument-complete+
              :source :amoebum
              :severity :info
              :payload (make-tool-call-argument-complete-payload
                        :tool-name tool-name
                        :tool-call-id tool-call-id
                        :arguments arguments
                        :index index)))

(defun make-session-checkpointed-event (&key
                                          checkpoint-id
                                          path
                                          (trigger :manual)
                                          (auto-p nil)
                                          (message-count 0)
                                          (extension-count 0)
                                          (tool-count 0)
                                          (memory-count 0))
  (make-event :type +event-type-session-checkpointed+
              :source :amoebum
              :severity :info
              :payload (make-session-checkpointed-payload
                        :checkpoint-id checkpoint-id
                        :path path
                        :trigger trigger
                        :auto-p auto-p
                        :message-count message-count
                        :extension-count extension-count
                        :tool-count tool-count
                        :memory-count memory-count)))

(defun make-session-restored-event (&key
                                      checkpoint-id
                                      path
                                      (trigger :manual)
                                      (message-count 0)
                                      (extension-count 0)
                                      (tool-count 0)
                                      (memory-count 0))
  (make-event :type +event-type-session-restored+
              :source :amoebum
              :severity :info
              :payload (make-session-restored-payload
                        :checkpoint-id checkpoint-id
                        :path path
                        :trigger trigger
                        :message-count message-count
                        :extension-count extension-count
                        :tool-count tool-count
                        :memory-count memory-count)))
