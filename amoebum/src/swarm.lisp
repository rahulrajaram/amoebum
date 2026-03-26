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
                      state-machine result thread error-message backing-agent)))
  (id "" :type string)
  (task "" :type string)
  (status :initializing :type keyword)
  (created-at (get-universal-time) :type integer)
  (finished-at nil :type (or null integer))
  (state-machine nil)
  (result nil)
  (thread nil)
  (error-message nil :type (or null string))
  (backing-agent nil))

(defvar *swarm-registry* (make-hash-table :test #'equal)
  "Hash-table of id -> swarm-agent.")

(defvar *swarm-counter* 0)

(defun %next-swarm-id ()
  (format nil "swarm-~A" (incf *swarm-counter*)))

(defun %swarm-transition-safe (agent target-state &key metadata)
  (let ((state-machine (swarm-agent-state-machine agent)))
    (when state-machine
      (handler-case
          (sw4rm-sdk::transition-to state-machine target-state :metadata metadata)
        (error ()
          nil)))))

(defun %swarm-status-metadata (status &key timeout-seconds error-message)
  (let ((metadata (list :agent-status status)))
    (when error-message
      (setf metadata (append metadata (list :error-message error-message))))
    (case status
      (:queued
       (append metadata (list :ack-stage sw4rm-sdk:+received+)))
      (:running
       (append metadata (list :ack-stage sw4rm-sdk:+read+)))
      (:cancelling
       (append metadata (list :ack-stage sw4rm-sdk:+rejected+
                              :error-code sw4rm-sdk:+forced-preemption+
                              :cancelled t)))
      (:completed
       (append metadata (list :ack-stage sw4rm-sdk:+fulfilled+)))
      (:failed
       (append metadata (list :ack-stage sw4rm-sdk:+failed+
                              :error-code sw4rm-sdk:+internal-error+)))
      (:cancelled
       (append metadata (list :ack-stage sw4rm-sdk:+rejected+
                              :error-code sw4rm-sdk:+forced-preemption+
                              :cancelled t)))
      (:timeout
       (append metadata (list :ack-stage sw4rm-sdk:+timed-out+
                              :error-code sw4rm-sdk:+tool-timeout+
                              :timed-out t
                              :timeout-seconds timeout-seconds)))
      (otherwise
       metadata))))

(defun %swarm-mark-running (agent)
  (let ((backing-agent (swarm-agent-backing-agent agent)))
    (when backing-agent
      (setf (agent-record-status backing-agent) :running
            (agent-record-started-ms backing-agent) (%agent-now-ms))))
  (setf (swarm-agent-status agent) :running)
  (%swarm-transition-safe agent :scheduled
                          :metadata (%swarm-status-metadata :queued))
  (%swarm-transition-safe agent :running
                          :metadata (%swarm-status-metadata :running))
  agent)

(defun %swarm-mark-cancelling (agent)
  (let ((backing-agent (swarm-agent-backing-agent agent)))
    (when backing-agent
      (setf (agent-record-cancel-requested-p backing-agent) t
            (agent-record-status backing-agent) :cancelling)))
  (%swarm-transition-safe agent :shutting-down
                          :metadata (%swarm-status-metadata :cancelling))
  agent)

(defun %swarm-mark-terminal (agent status result error-message &key timeout-seconds stdout stderr)
  (let ((backing-agent (swarm-agent-backing-agent agent))
        (metadata (%swarm-status-metadata status
                                          :timeout-seconds timeout-seconds
                                          :error-message error-message))
        (finished-ms (%agent-now-ms)))
    (when backing-agent
      (setf (agent-record-status backing-agent) status
            (agent-record-finished-ms backing-agent) finished-ms
            (agent-record-result backing-agent) result
            (agent-record-stdout backing-agent) stdout
            (agent-record-stderr backing-agent) stderr
            (agent-record-error-message backing-agent) error-message))
    (when (and backing-agent (eq status :cancelled))
      (setf (agent-record-cancel-requested-p backing-agent) t))
  (setf (swarm-agent-status agent) status
        (swarm-agent-result agent) result
        (swarm-agent-error-message agent) error-message
        (swarm-agent-finished-at agent) (get-universal-time))
    (case status
      (:completed
       (%swarm-transition-safe agent :completed :metadata metadata))
      (:failed
       (%swarm-transition-safe agent :failed :metadata metadata))
      (:cancelled
       (%swarm-transition-safe agent :shutting-down :metadata metadata)
       (%swarm-transition-safe agent :failed :metadata metadata))
      (:timeout
       (%swarm-transition-safe agent :shutting-down :metadata metadata)
       (%swarm-transition-safe agent :failed :metadata metadata)))
    agent))

(defun %swarm-runner-finished-status (runner-agent)
  (if (agent-record-cancel-requested-p runner-agent)
      :cancelled
      :completed))

;;; --- Lifecycle ---

(defun spawn-swarm-agent (task &key
                               (id nil)
                               (event-bus (current-event-bus))
                               runner
                               timeout-seconds)
  "Spawn a new swarm sub-agent for TASK. Returns a swarm-agent."
  (let* ((agent-id (or id (%next-swarm-id)))
         (runner-agent (%make-agent-record
                        :id agent-id
                        :type :swarm
                        :task task
                        :status :queued
                        :created-ms (%agent-now-ms)))
         (agent (make-swarm-agent :id agent-id
                                  :task task
                                  :status :initializing
                                  :backing-agent runner-agent))
         (agent-runner (or runner #'%default-agent-runner)))
    (setf (gethash agent-id *swarm-registry*) agent)
    ;; Create a state machine for the agent
    (handler-case
        (let ((sm (make-instance 'sw4rm-sdk::agent-state-machine)))
          (setf (swarm-agent-state-machine agent) sm)
          ;; Transition to RUNNABLE
          (%swarm-transition-safe agent :runnable
                                  :metadata (%swarm-status-metadata :queued)))
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
             (let ((stdout-stream (make-string-output-stream))
                   (stderr-stream (make-string-output-stream))
                   (result nil)
                   (status :completed)
                   (error-message nil))
               (%swarm-mark-running agent)
               (handler-case
                   (let ((*standard-output* stdout-stream)
                         (*error-output* stderr-stream))
                     (setf result
                           #+sbcl
                           (if (and timeout-seconds
                                    (numberp timeout-seconds)
                                    (> timeout-seconds 0))
                               (sb-ext:with-timeout timeout-seconds
                                 (funcall agent-runner runner-agent))
                               (funcall agent-runner runner-agent))
                           #-sbcl
                           (funcall agent-runner runner-agent)
                           status (%swarm-runner-finished-status runner-agent)))
                 (agent-cancelled (condition)
                   (setf status :cancelled
                         error-message (princ-to-string condition)))
                 #+sbcl
                 (sb-ext:timeout (_condition)
                   (setf status :timeout
                         error-message (format nil "Swarm agent ~A timed out after ~A seconds."
                                               agent-id
                                               timeout-seconds)))
                 (error (condition)
                   (if (agent-record-cancel-requested-p runner-agent)
                       (setf status :cancelled
                             error-message (princ-to-string condition))
                       (setf status :failed
                             error-message (princ-to-string condition)))))
               (%swarm-mark-terminal agent status result error-message
                                     :timeout-seconds timeout-seconds
                                     :stdout (get-output-stream-string stdout-stream)
                                     :stderr (get-output-stream-string stderr-stream))
               (publish event-bus
                        (make-event :type (if (eq status :cancelled)
                                              +event-type-agent-cancelled+
                                              +event-type-agent-complete+)
                                    :source "swarm"
                                    :payload (list :id agent-id
                                                   :status status
                                                   :task task
                                                   :error-message error-message)))))
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
      (when (member (swarm-agent-status agent) '(:initializing :running) :test #'eq)
        (%swarm-mark-cancelling agent))
      (when (and thread (bt:thread-alive-p thread))
        (bt:join-thread thread)))
    (unless (member (swarm-agent-status agent) '(:completed :failed :cancelled :timeout) :test #'eq)
      (%swarm-mark-terminal agent :cancelled nil "Swarm agent cancelled.")
      (publish event-bus
               (make-event :type +event-type-agent-cancelled+
                           :source "swarm"
                           :payload (list :id agent-id :status :cancelled))))
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

;;; ---------------------------------------------------------------------------
;;; Inter-user coordination and delegation (I253)
;;; ---------------------------------------------------------------------------

(defvar *user-session-registry* (sw4rm-sdk:make-local-registry)
  "SW4RM local registry of active user sessions.")

(defvar *user-session->agent-id* (make-hash-table :test #'equal)
  "Map session-id -> SW4RM agent-id.")

(defvar *user-session->user-id* (make-hash-table :test #'equal)
  "Map session-id -> user-id.")

(defvar *user-agent->session-id* (make-hash-table :test #'equal)
  "Map SW4RM agent-id -> session-id.")

(defvar *user-negotiation-room-participants* (make-hash-table :test #'equal)
  "Map negotiation room-id -> list of participant session ids.")

(defvar *user-handoff-client* nil
  "Shared local SW4RM handoff client for inter-user delegation.")

(defvar *user-negotiation-client* nil
  "Shared local SW4RM negotiation-room client for user code-review rooms.")

(defvar *user-handoff-sequence* 0
  "Monotonic handoff sequence counter for generated request IDs.")

(defvar *user-coordination-lock* (bt:make-lock "amoebum-user-coordination-lock")
  "Lock protecting user coordination registry state.")

(defun %coordination-trim-string (value)
  (if (stringp value)
      (string-trim '(#\Space #\Tab #\Newline #\Return) value)
      (string-trim '(#\Space #\Tab #\Newline #\Return)
                   (princ-to-string value))))

(defun %coordination-require-string (value field-name)
  (let ((trimmed (%coordination-trim-string value)))
    (when (zerop (length trimmed))
      (error "~A must be a non-empty string." field-name))
    trimmed))

(defun %coordination-normalize-token (value)
  (let* ((trimmed (%coordination-require-string value "coordination token"))
         (text (string-downcase trimmed)))
    (coerce (loop for ch across text
                  collect (if (or (alphanumericp ch)
                                  (char= ch #\-)
                                  (char= ch #\_))
                              ch
                              #\-))
            'string)))

(defparameter +provider-secret-key-aliases+
  '(("anthropic-provider" . "ANTHROPIC_API_KEY")
    ("anthropic" . "ANTHROPIC_API_KEY")
    ("openai-compatible-provider" . "OPENAI_API_KEY")
    ("openai-compat" . "OPENAI_API_KEY")
    ("openai" . "OPENAI_API_KEY")
    ("kimi-provider" . "MOONSHOT_API_KEY")
    ("kimi" . "MOONSHOT_API_KEY")
    ("moonshot" . "MOONSHOT_API_KEY")))

(defparameter +delegation-sensitive-context-keys+
  '("provider-secrets"
    "provider-secret"
    "provider-credentials"
    "provider-credential"
    "provider-api-key"
    "api-key"
    "api_key"
    "anthropic_api_key"
    "openai_api_key"
    "moonshot_api_key"))

(defun %provider-secret-key-id (raw-key)
  (let* ((text (%coordination-require-string raw-key "provider secret key"))
         (down (string-downcase text))
         (alias (cdr (assoc down +provider-secret-key-aliases+ :test #'string=))))
    (or alias (string-upcase down))))

(defun %provider-secret-pairs (provider-secrets)
  (cond
    ((null provider-secrets) nil)
    ((and (listp provider-secrets)
          (every (lambda (entry)
                   (and (consp entry)
                        (not (null (car entry)))))
                 provider-secrets))
     (mapcar (lambda (entry)
               (let ((tail (cdr entry)))
                 (cons (car entry)
                       (if (and (consp tail) (null (cdr tail)))
                           (car tail)
                           tail))))
             provider-secrets))
    ((and (listp provider-secrets)
          (evenp (length provider-secrets)))
     (loop for (key value) on provider-secrets by #'cddr
           collect (cons key value)))
    (t
     (error "provider-secrets must be an alist or plist, got ~S" provider-secrets))))

(defun %coordination-plist-like-p (value)
  (and (listp value)
       (evenp (length value))
       (loop for (key _value) on value by #'cddr
             always (or (keywordp key)
                        (symbolp key)
                        (stringp key)))))

(defun %delegation-sensitive-context-key-p (key)
  (let* ((text (%coordination-trim-string key))
         (down (string-downcase text)))
    (member down +delegation-sensitive-context-keys+ :test #'string=)))

(defun %sanitize-delegation-context (context)
  (cond
    ((null context) "")
    ((hash-table-p context)
     (let ((clean (make-hash-table :test (hash-table-test context))))
       (maphash (lambda (key value)
                  (unless (%delegation-sensitive-context-key-p key)
                    (setf (gethash key clean)
                          (%sanitize-delegation-context value))))
                context)
       clean))
    ((%coordination-plist-like-p context)
     (let ((clean '()))
       (loop for (key value) on context by #'cddr do
         (unless (%delegation-sensitive-context-key-p key)
           (setf clean
                 (append clean
                         (list key (%sanitize-delegation-context value))))))
       clean))
    ((and (listp context)
          (every #'consp context))
     (let ((clean '()))
       (dolist (entry context (nreverse clean))
         (unless (%delegation-sensitive-context-key-p (car entry))
           (push (cons (car entry)
                       (%sanitize-delegation-context (cdr entry)))
                 clean)))))
    ((listp context)
     (mapcar #'%sanitize-delegation-context context))
    (t context)))

(defun %apply-agent-provider-secrets! (agent-id provider-secrets)
  (let ((registry *user-session-registry*))
    (unless (typep registry 'sw4rm-sdk:local-registry)
      (error "User session registry is not initialized."))
    (sw4rm-sdk:local-registry-clear-provider-secrets registry agent-id)
    (dolist (pair (%provider-secret-pairs provider-secrets))
      (let ((provider-key (%provider-secret-key-id (car pair)))
            (secret-value (%coordination-trim-string (cdr pair))))
        (when (plusp (length secret-value))
          (sw4rm-sdk:local-registry-set-provider-secret
           registry
           agent-id
           provider-key
           secret-value))))
    t))

(defun %user-agent-id (user-id session-id)
  (format nil "user/~A/session/~A"
          (%coordination-normalize-token user-id)
          (%coordination-normalize-token session-id)))

(defun %ensure-user-handoff-client ()
  (or *user-handoff-client*
      (setf *user-handoff-client*
            (make-instance 'sw4rm-sdk:handoff-client
                           :address "local://amoebum/user-handoff"))))

(defun %ensure-user-negotiation-client ()
  (or *user-negotiation-client*
      (setf *user-negotiation-client*
            (make-instance 'sw4rm-sdk:negotiation-room-client
                           :address "local://amoebum/user-negotiation"))))

(defun %registered-agent-id-for-session (session-id)
  (let ((resolved-session-id (%coordination-require-string session-id "session-id")))
    (bt:with-lock-held (*user-coordination-lock*)
      (or (gethash resolved-session-id *user-session->agent-id*)
          (error "No user peer registered for session-id ~S." resolved-session-id)))))

(defun %session-id-for-agent (agent-id)
  (bt:with-lock-held (*user-coordination-lock*)
    (gethash agent-id *user-agent->session-id*)))

(defun %next-user-handoff-id ()
  (bt:with-lock-held (*user-coordination-lock*)
    (incf *user-handoff-sequence*)
    (format nil "user-handoff-~D" *user-handoff-sequence*)))

(defun %publish-user-coordination-event (event-type payload &key (severity :info))
  (publish (current-event-bus)
           event-type
           :source :sw4rm-user-coordination
           :severity severity
           :payload payload)
  payload)

(defun clear-user-coordination-state ()
  "Clear user session registration, delegation, and negotiation state."
  (bt:with-lock-held (*user-coordination-lock*)
    (sw4rm-sdk:local-registry-clear *user-session-registry*)
    (clrhash *user-session->agent-id*)
    (clrhash *user-session->user-id*)
    (clrhash *user-agent->session-id*)
    (clrhash *user-negotiation-room-participants*)
    (setf *user-handoff-client* nil
          *user-negotiation-client* nil
          *user-handoff-sequence* 0)
    t))

(defun register-user-session-peer (session-id &key
                                                user-id
                                                (capabilities '("chat" "handoff" "code-review"))
                                                name
                                                description
                                                (provider-secrets nil provider-secrets-supplied-p)
                                                (if-exists :replace))
  "Register a user SESSION-ID as a SW4RM peer in the local registry."
  (let* ((resolved-session-id (%coordination-require-string session-id "session-id"))
         (resolved-user-id (%coordination-require-string (or user-id session-id) "user-id"))
         (agent-id (%user-agent-id resolved-user-id resolved-session-id))
         (agent-config (sw4rm-sdk:make-agent-config
                        :agent-id agent-id
                        :name (or name (format nil "user-~A" resolved-user-id))
                        :description (or description
                                         (format nil "Amoebum user session ~A"
                                                 resolved-session-id))
                        :capabilities (copy-list capabilities))))
    (bt:with-lock-held (*user-coordination-lock*)
      (let ((existing-agent-id (gethash resolved-session-id *user-session->agent-id*)))
        (when (and existing-agent-id
                   (not (string= existing-agent-id agent-id)))
          (sw4rm-sdk:local-registry-unregister *user-session-registry* existing-agent-id)
          (remhash existing-agent-id *user-agent->session-id*)))
      (sw4rm-sdk:local-registry-register *user-session-registry*
                                         agent-config
                                         :if-exists if-exists)
      (setf (gethash resolved-session-id *user-session->agent-id*) agent-id
            (gethash resolved-session-id *user-session->user-id*) resolved-user-id
            (gethash agent-id *user-agent->session-id*) resolved-session-id))
    (when provider-secrets-supplied-p
      (%apply-agent-provider-secrets! agent-id provider-secrets))
    agent-config))

(defun unregister-user-session-peer (session-id)
  "Unregister SESSION-ID from local user coordination."
  (let* ((resolved-session-id (%coordination-require-string session-id "session-id"))
         (agent-id nil))
    (bt:with-lock-held (*user-coordination-lock*)
      (setf agent-id (gethash resolved-session-id *user-session->agent-id*))
      (remhash resolved-session-id *user-session->agent-id*)
      (remhash resolved-session-id *user-session->user-id*)
      (when agent-id
        (remhash agent-id *user-agent->session-id*)))
    (if agent-id
        (sw4rm-sdk:local-registry-unregister *user-session-registry* agent-id)
        nil)))

(defun find-user-session-peer (session-id)
  "Return SW4RM agent-config for SESSION-ID or NIL."
  (let ((agent-id (bt:with-lock-held (*user-coordination-lock*)
                    (gethash (%coordination-require-string session-id "session-id")
                             *user-session->agent-id*))))
    (and agent-id
         (sw4rm-sdk:local-registry-get *user-session-registry* agent-id))))

(defun list-user-session-peers ()
  "Return registered user peers as plists."
  (bt:with-lock-held (*user-coordination-lock*)
    (let ((peers '()))
      (maphash (lambda (session-id agent-id)
                 (let ((config (sw4rm-sdk:local-registry-get
                                *user-session-registry*
                                agent-id)))
                   (push (list :session-id session-id
                               :user-id (gethash session-id *user-session->user-id*)
                               :agent-id agent-id
                               :capabilities (and config
                                                  (sw4rm-sdk:agent-config-capabilities config)))
                         peers)))
               *user-session->agent-id*)
      (sort peers #'string< :key (lambda (peer) (getf peer :session-id))))))

(defun handoff-between-users (from-session-id to-session-id reason
                                &key request-id
                                  context
                                  (capabilities-required '("code-review"))
                                  (priority 0)
                                  budget
                                  delegation-policy
                                  timeout-ms)
  "Initiate a SW4RM handoff request from one user session to another."
  (let* ((resolved-reason (%coordination-require-string reason "reason"))
         (from-agent-id (%registered-agent-id-for-session from-session-id))
         (to-agent-id (%registered-agent-id-for-session to-session-id))
         (sanitized-context (%sanitize-delegation-context context))
         (request (list :request-id (or request-id (%next-user-handoff-id))
                        :from-agent from-agent-id
                        :to-agent to-agent-id
                        :reason resolved-reason
                        :context-snapshot (or sanitized-context "")
                        :capabilities-required (copy-list capabilities-required)
                        :priority priority)))
    (when budget
      (setf (getf request :budget) (copy-list budget)))
    (when delegation-policy
      (setf (getf request :delegation-policy) (copy-list delegation-policy)))
    (when timeout-ms
      (setf (getf request :timeout-ms) timeout-ms))
    (let ((response (sw4rm-sdk:initiate-handoff (%ensure-user-handoff-client) request)))
      (let ((payload
              (append response
                      (list :from-session-id (%coordination-require-string from-session-id "from-session-id")
                            :to-session-id (%coordination-require-string to-session-id "to-session-id")
                            :agent-id from-agent-id
                            :from-agent-id from-agent-id
                            :to-agent-id to-agent-id
                            :reason resolved-reason
                            :context-snapshot (or sanitized-context "")
                            :capabilities-required (copy-list capabilities-required)
                            :priority priority))))
        (%publish-user-coordination-event +event-type-user-handoff-requested+ payload)
        payload))))

(defun get-user-pending-handoffs (session-id)
  "Return pending handoff requests routed to SESSION-ID."
  (let* ((agent-id (%registered-agent-id-for-session session-id))
         (pending (sw4rm-sdk:get-pending-handoffs (%ensure-user-handoff-client) agent-id)))
    (mapcar (lambda (request)
              (let ((from-agent (getf request :from-agent))
                    (to-agent (getf request :to-agent)))
                (append request
                        (list :from-session-id (%session-id-for-agent from-agent)
                              :to-session-id (%session-id-for-agent to-agent)))))
            pending)))

(defun user-handoff-status (handoff-id)
  "Return handoff status plist for HANDOFF-ID."
  (sw4rm-sdk:get-handoff-status (%ensure-user-handoff-client) handoff-id))

(defun accept-user-handoff (handoff-id)
  "Accept HANDOFF-ID on the local coordination bus."
  (let* ((response (sw4rm-sdk:accept-handoff (%ensure-user-handoff-client) handoff-id))
         (status (or (ignore-errors (user-handoff-status handoff-id))
                     response))
         (payload (append status
                          (list :handoff-id handoff-id
                                :agent-id (or (getf status :to-agent)
                                              (getf status :to-agent-id)
                                              (getf response :to-agent)
                                              (getf response :to-agent-id))))))
    (%publish-user-coordination-event +event-type-user-handoff-accepted+ payload)
    payload))

(defun reject-user-handoff (handoff-id reason &key rejection-code retry-after-ms redirect-to-agent-id)
  "Reject HANDOFF-ID with optional protocol metadata."
  (let* ((resolved-code (or rejection-code sw4rm-sdk:+error-code-unspecified+))
         (response (sw4rm-sdk:reject-handoff-with-options (%ensure-user-handoff-client)
                                                          handoff-id
                                                          reason
                                                          :rejection-code resolved-code
                                                          :retry-after-ms retry-after-ms
                                                          :redirect-to-agent-id redirect-to-agent-id))
         (status (or (ignore-errors (user-handoff-status handoff-id))
                     response))
         (payload (append status
                          (list :handoff-id handoff-id
                                :reason reason
                                :rejection-code resolved-code
                                :retry-after-ms retry-after-ms
                                :redirect-to-agent-id redirect-to-agent-id
                                :agent-id (or (getf status :to-agent)
                                              (getf status :to-agent-id)
                                              (getf response :to-agent)
                                              (getf response :to-agent-id))))))
    (%publish-user-coordination-event +event-type-user-handoff-rejected+
                                      payload
                                      :severity :warning)
    payload))

(defun complete-user-handoff (handoff-id)
  "Mark HANDOFF-ID as complete."
  (let* ((response (sw4rm-sdk:complete-handoff (%ensure-user-handoff-client) handoff-id))
         (status (or (ignore-errors (user-handoff-status handoff-id))
                     response))
         (payload (append status
                          (list :handoff-id handoff-id
                                :agent-id (or (getf status :to-agent)
                                              (getf status :to-agent-id)
                                              (getf response :to-agent)
                                              (getf response :to-agent-id))))))
    (%publish-user-coordination-event +event-type-user-handoff-completed+ payload)
    payload))

(defun create-user-negotiation-room (room-id participant-session-ids
                                     &key description metadata)
  "Create a multi-user negotiation room for code review."
  (let* ((resolved-room-id (%coordination-require-string room-id "room-id"))
         (participants (remove-duplicates
                        (mapcar (lambda (session-id)
                                  (%coordination-require-string session-id "participant session-id"))
                                participant-session-ids)
                        :test #'string=)))
    (dolist (session-id participants)
      (%registered-agent-id-for-session session-id))
    (let ((response (sw4rm-sdk:create-room (%ensure-user-negotiation-client)
                                           resolved-room-id
                                           :description description
                                           :metadata metadata)))
      (bt:with-lock-held (*user-coordination-lock*)
        (setf (gethash resolved-room-id *user-negotiation-room-participants*)
              participants))
      (let ((payload (append response
                             (list :room-id resolved-room-id
                                   :participant-session-ids participants
                                   :description description
                                   :metadata metadata
                                   :agent-id (and participants
                                                  (%registered-agent-id-for-session
                                                   (first participants)))))))
        (%publish-user-coordination-event +event-type-user-negotiation-room-created+
                                          payload)
        payload))))

(defun submit-user-negotiation-artifact (room-id proposer-session-id artifact-id artifact
                                         &key requested-critic-session-ids
                                           (aggregation-strategy :confidence-weighted)
                                           timeout-ms
                                           metadata)
  "Submit ARTIFACT for review in ROOM-ID from PROPOSER-SESSION-ID."
  (let* ((resolved-room-id (%coordination-require-string room-id "room-id"))
         (resolved-artifact-id (%coordination-require-string artifact-id "artifact-id"))
         (proposer-agent-id (%registered-agent-id-for-session proposer-session-id))
         (requested-critic-agent-ids
           (mapcar #'%registered-agent-id-for-session requested-critic-session-ids))
         (proposal (list :artifact-id resolved-artifact-id
                         :negotiation-room-id resolved-room-id
                         :proposer-id proposer-agent-id
                         :artifact artifact
                         :metadata metadata
                         :requested-critics requested-critic-agent-ids
                         :aggregation-strategy aggregation-strategy)))
    (when timeout-ms
      (setf (getf proposal :timeout-ms) timeout-ms))
    (let ((response (sw4rm-sdk:submit-artifact (%ensure-user-negotiation-client) proposal)))
      (%publish-user-coordination-event
       +event-type-user-negotiation-artifact-submitted+
       (list :artifact-id resolved-artifact-id
             :room-id resolved-room-id
             :proposer-session-id proposer-session-id
             :requested-critic-session-ids (copy-list requested-critic-session-ids)
             :agent-id proposer-agent-id
             :metadata metadata
             :timeout-ms timeout-ms
             :response response))
      response)))

(defun add-user-negotiation-critique (room-id artifact-id critic-session-id passed
                                      &key score details)
  "Submit a critique vote in a user negotiation room."
  (let* ((resolved-room-id (%coordination-require-string room-id "room-id"))
         (resolved-artifact-id (%coordination-require-string artifact-id "artifact-id"))
         (critic-agent-id (%registered-agent-id-for-session critic-session-id)))
    (let ((response
            (sw4rm-sdk:add-critique
             (%ensure-user-negotiation-client)
             (list :artifact-id resolved-artifact-id
                   :negotiation-room-id resolved-room-id
                   :critic-id critic-agent-id
                   :passed (if passed t nil)
                   :score score
                   :details details))))
      (%publish-user-coordination-event
       +event-type-user-negotiation-critique-added+
       (list :artifact-id resolved-artifact-id
             :room-id resolved-room-id
             :critic-session-id critic-session-id
             :agent-id critic-agent-id
             :passed (if passed t nil)
             :score score
             :details details
             :response response))
      response)))

(defun get-user-negotiation-room-status (room-id)
  "Return room status enriched with session IDs."
  (let* ((resolved-room-id (%coordination-require-string room-id "room-id"))
         (status (sw4rm-sdk:get-room-status (%ensure-user-negotiation-client)
                                            resolved-room-id))
         (participants (bt:with-lock-held (*user-coordination-lock*)
                         (copy-list
                          (gethash resolved-room-id
                                   *user-negotiation-room-participants*))))
         (active-critic-sessions
           (remove nil
                   (mapcar #'%session-id-for-agent
                           (copy-list (getf status :active-critics))))))
    (append status
            (list :participant-session-ids participants
                  :active-critic-session-ids active-critic-sessions))))

(defun wait-for-user-negotiation-decision (artifact-id &key (timeout-s 30.0) (poll-interval-s 0.1))
  "Wait for a negotiation decision for ARTIFACT-ID."
  (let ((decision (sw4rm-sdk:wait-for-decision (%ensure-user-negotiation-client)
                                               artifact-id
                                               :timeout-s timeout-s
                                               :poll-interval-s poll-interval-s)))
    (%publish-user-coordination-event
     +event-type-user-negotiation-decision+
     (append decision
             (list :artifact-id artifact-id
                   :agent-id (or (getf decision :proposer-id)
                                 (getf decision :agent-id)))))
    decision))
