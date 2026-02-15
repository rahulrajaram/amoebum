(in-package :amoebum)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %event-type-keyword (name)
    (intern (string-upcase name) :keyword)))

(defparameter +event-type-tool-invoked+ (%event-type-keyword "tool:invoked"))
(defparameter +event-type-tool-completed+ (%event-type-keyword "tool:completed"))
(defparameter +event-type-tool-error+ (%event-type-keyword "tool:error"))
(defparameter +event-type-config-changed+ (%event-type-keyword "config:changed"))
(defparameter +event-type-permission-prompted+ (%event-type-keyword "permission:prompted"))
(defparameter +event-type-memory-updated+ (%event-type-keyword "memory:updated"))

(defparameter +core-event-types+
  (list +event-type-tool-invoked+
        +event-type-tool-completed+
        +event-type-tool-error+
        +event-type-config-changed+
        +event-type-permission-prompted+
        +event-type-memory-updated+))

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
                (&key tool-name args condition elapsed-ms request-id)))
  tool-name
  args
  condition
  elapsed-ms
  request-id)

(defstruct (config-changed-payload
            (:constructor make-config-changed-payload
                (&key key old-value new-value)))
  key
  old-value
  new-value)

(defstruct (permission-prompted-payload
            (:constructor make-permission-prompted-payload
                (&key tool-name path command reason permission-mode)))
  tool-name
  path
  command
  reason
  permission-mode)

(defstruct (memory-updated-payload
            (:constructor make-memory-updated-payload
                (&key backend operation key value)))
  backend
  operation
  key
  value)

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

(defun make-tool-error-event (&key tool-name args condition elapsed-ms request-id)
  (make-event :type +event-type-tool-error+
              :source :amoebum
              :severity :error
              :payload (make-tool-error-payload
                        :tool-name tool-name
                        :args args
                        :condition condition
                        :elapsed-ms elapsed-ms
                        :request-id request-id)))

(defun make-config-changed-event (&key key old-value new-value)
  (make-event :type +event-type-config-changed+
              :source :amoebum
              :severity :info
              :payload (make-config-changed-payload
                        :key key
                        :old-value old-value
                        :new-value new-value)))

(defun make-permission-prompted-event (&key tool-name path command reason permission-mode)
  (make-event :type +event-type-permission-prompted+
              :source :amoebum
              :severity :warning
              :payload (make-permission-prompted-payload
                        :tool-name tool-name
                        :path path
                        :command command
                        :reason reason
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
