(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; Inter-user coordination and handoff state (I253, NXT-379)
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

(defvar *user-handoff-details* (make-hash-table :test #'equal)
  "Map handoff-id -> Amoebum-native result/provenance details.")

(defvar *user-coordination-lock* (bt:make-lock "amoebum-user-coordination-lock")
  "Lock protecting user coordination registry state.")

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

(defun %coordination-copy-value (value)
  (cond
    ((hash-table-p value)
     (let ((copy (make-hash-table :test (hash-table-test value))))
       (maphash (lambda (key inner-value)
                  (setf (gethash key copy)
                        (%coordination-copy-value inner-value)))
                value)
       copy))
    ((consp value)
     (copy-tree value))
    ((vectorp value)
     (copy-seq value))
    (t
     value)))

(defun %coordination-plist-put (plist key value)
  (let ((result (copy-list (or plist '()))))
    (loop for cell on result by #'cddr
          when (eq (car cell) key) do
            (setf (cadr cell) value)
            (return-from %coordination-plist-put result))
    (append result (list key value))))

(defun %coordination-plist-merge (&rest plists)
  (let ((result '()))
    (dolist (plist plists result)
      (loop for (key value) on plist by #'cddr do
        (setf result (%coordination-plist-put result key value))))))

(defun %copy-user-handoff-details (details)
  (if (listp details)
      (%coordination-copy-value details)
      '()))

(defun %user-handoff-details (handoff-id)
  (bt:with-lock-held (*user-coordination-lock*)
    (%copy-user-handoff-details
     (gethash (%coordination-require-string handoff-id "handoff-id")
              *user-handoff-details*))))

(defun %store-user-handoff-details! (handoff-id details)
  (let ((resolved-id (%coordination-require-string handoff-id "handoff-id")))
    (bt:with-lock-held (*user-coordination-lock*)
      (setf (gethash resolved-id *user-handoff-details*)
            (%copy-user-handoff-details details))))
  details)

(defun %update-user-handoff-details! (handoff-id updater)
  (let ((resolved-id (%coordination-require-string handoff-id "handoff-id")))
    (bt:with-lock-held (*user-coordination-lock*)
      (let* ((existing (%copy-user-handoff-details
                        (gethash resolved-id *user-handoff-details*)))
             (updated (funcall updater existing)))
        (setf (gethash resolved-id *user-handoff-details*)
              (%copy-user-handoff-details updated))
        (%copy-user-handoff-details updated)))))

(defun %handoff-context-summary (packet)
  (when (listp packet)
    (let* ((conversation (getf packet :conversation))
           (files (getf packet :files))
           (git (getf packet :git))
           (memory (getf packet :memory))
           (worktree (getf packet :worktree))
           (summary '()))
      (when (getf packet :packet-kind)
        (setf summary (append summary (list :packet-kind (getf packet :packet-kind)))))
      (when (getf packet :budget-mode)
        (setf summary (append summary (list :budget-mode (getf packet :budget-mode)))))
      (when (and (listp conversation) (getf conversation :entry-count))
        (setf summary (append summary (list :conversation-entry-count
                                            (getf conversation :entry-count)))))
      (when (and (listp files) (getf files :active-file))
        (setf summary (append summary (list :active-file (getf files :active-file)))))
      (when (and (listp git) (getf git :branch))
        (setf summary (append summary (list :branch (getf git :branch)))))
      (when (and (listp memory)
                 (or (getf memory :project-count)
                     (getf memory :session-count)))
        (setf summary (append summary
                              (list :project-memory-count (or (getf memory :project-count) 0)
                                    :session-memory-count (or (getf memory :session-count) 0)))))
      (when (and (listp worktree) (getf worktree :id))
        (setf summary (append summary (list :worktree-id (getf worktree :id)))))
      summary)))

(defun %handoff-summary-text (status handoff-id payload)
  (let* ((result (getf payload :result-payload))
         (partial-result (getf payload :partial-result))
         (summary (or (getf payload :summary)
                      (and (listp result) (getf result :summary))
                      (and (listp partial-result) (getf partial-result :summary))
                      (and (stringp result) result)
                      (and (stringp partial-result) partial-result)
                      (getf payload :reason))))
    (or summary
        (case status
          (:completed (format nil "Delegated handoff ~A completed." handoff-id))
          (:rejected (format nil "Delegated handoff ~A was rejected." handoff-id))
          (:accepted (format nil "Delegated handoff ~A accepted." handoff-id))
          (otherwise (format nil "Delegated handoff ~A updated." handoff-id))))))

(defun %handoff-conversation-merge (handoff-id payload)
  (let* ((status (or (getf payload :status) :pending))
         (summary (%handoff-summary-text status handoff-id payload))
         (diagnostics (getf payload :diagnostics))
         (message (list :role "assistant"
                        :name (format nil "handoff/~A" handoff-id)
                        :content summary
                        :partial (eq status :rejected))))
    (list :schema-version 1
          :handoff-id handoff-id
          :status status
          :summary summary
          :result-payload (%coordination-copy-value (getf payload :result-payload))
          :partial-result (%coordination-copy-value (getf payload :partial-result))
          :diagnostics (%coordination-copy-value diagnostics)
          :artifacts (%coordination-copy-value (getf payload :artifacts))
          :budget-spent (%coordination-copy-value (getf payload :budget-spent))
          :delegation-trace (%coordination-copy-value (getf payload :delegation-trace))
          :delegation-provenance (%coordination-copy-value
                                  (getf payload :delegation-provenance))
          :messages (list message))))

(defun %compose-user-handoff-payload (handoff-id status &rest extra-plists)
  (let* ((details (%user-handoff-details handoff-id))
         (merged (apply #'%coordination-plist-merge
                        (append (list status details)
                                extra-plists)))
         (with-merge (%coordination-plist-put merged
                                              :conversation-merge
                                              (%handoff-conversation-merge
                                               handoff-id
                                               merged))))
    (%annotate-handoff-payload-context with-merge)))

(defun clear-user-coordination-state ()
  "Clear user session registration, delegation, and negotiation state."
  (bt:with-lock-held (*user-coordination-lock*)
    (sw4rm-sdk:local-registry-clear *user-session-registry*)
    (clrhash *user-session->agent-id*)
    (clrhash *user-session->user-id*)
    (clrhash *user-agent->session-id*)
    (clrhash *user-handoff-details*)
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
         (serialized-context (%serialize-coding-task-context sanitized-context budget))
         (context-packet (%deserialize-handoff-context-safely serialized-context))
         (request (list :request-id (or request-id (%next-user-handoff-id))
                        :from-agent from-agent-id
                        :to-agent to-agent-id
                        :reason resolved-reason
                        :context-snapshot (or serialized-context "")
                        :capabilities-required (copy-list capabilities-required)
                        :priority priority)))
    (when budget
      (setf (getf request :budget) (copy-list budget)))
    (when delegation-policy
      (setf (getf request :delegation-policy) (copy-list delegation-policy)))
    (when timeout-ms
      (setf (getf request :timeout-ms) timeout-ms))
    (let ((response (sw4rm-sdk:initiate-handoff (%ensure-user-handoff-client) request)))
      (let* ((handoff-id (or (getf response :handoff-id)
                             (getf response :request-id)
                             (getf request :request-id)))
             (trace-entry (list :event :requested
                                :from-session-id (%coordination-require-string
                                                  from-session-id
                                                  "from-session-id")
                                :to-session-id (%coordination-require-string
                                                to-session-id
                                                "to-session-id")
                                :from-agent-id from-agent-id
                                :to-agent-id to-agent-id
                                :reason resolved-reason
                                :capabilities-required (copy-list capabilities-required)
                                :priority priority
                                :requested-at (getf (getf response :metadata) :created-at)
                                :context (%handoff-context-summary context-packet))))
        (%store-user-handoff-details!
         handoff-id
         (list :handoff-id handoff-id
               :from-session-id (%coordination-require-string
                                 from-session-id
                                 "from-session-id")
               :to-session-id (%coordination-require-string
                               to-session-id
                               "to-session-id")
               :agent-id from-agent-id
               :from-agent-id from-agent-id
               :to-agent-id to-agent-id
               :reason resolved-reason
               :context-snapshot (or serialized-context "")
               :context-packet context-packet
               :context-snapshot-size-bytes (length (or serialized-context ""))
               :capabilities-required (copy-list capabilities-required)
               :priority priority
               :delegation-trace (list trace-entry)
               :delegation-provenance
               (list :schema-version 1
                     :request-id handoff-id
                     :context (%handoff-context-summary context-packet)
                     :trace (list trace-entry))))
        (let ((payload (%compose-user-handoff-payload
                        handoff-id
                        response)))
          (%publish-user-coordination-event +event-type-user-handoff-requested+ payload)
          payload)))))

(defun get-user-pending-handoffs (session-id)
  "Return pending handoff requests routed to SESSION-ID."
  (let* ((agent-id (%registered-agent-id-for-session session-id))
         (pending (sw4rm-sdk:get-pending-handoffs (%ensure-user-handoff-client) agent-id)))
    (mapcar (lambda (request)
              (let ((from-agent (getf request :from-agent))
                    (to-agent (getf request :to-agent))
                    (handoff-id (or (getf request :handoff-id)
                                    (getf request :request-id))))
                (%compose-user-handoff-payload
                 handoff-id
                 request
                 (list :from-session-id (%session-id-for-agent from-agent)
                       :to-session-id (%session-id-for-agent to-agent)))))
            pending)))

(defun user-handoff-status (handoff-id)
  "Return handoff status plist for HANDOFF-ID."
  (%compose-user-handoff-payload
   handoff-id
   (sw4rm-sdk:get-handoff-status (%ensure-user-handoff-client) handoff-id)))

(defun accept-user-handoff (handoff-id)
  "Accept HANDOFF-ID on the local coordination bus."
  (let* ((response (sw4rm-sdk:accept-handoff (%ensure-user-handoff-client) handoff-id))
         (status (or (ignore-errors (user-handoff-status handoff-id))
                     response)))
    (%update-user-handoff-details!
     handoff-id
     (lambda (details)
       (let* ((accepted-at (getf (getf response :metadata) :accepted-at))
              (event (list :event :accepted
                           :accepted-at accepted-at
                           :accepting-agent (or (getf status :accepting-agent)
                                                (getf response :accepting-agent)))))
         (%coordination-plist-merge
          details
          (list :delegation-trace
                (append (copy-list (or (getf details :delegation-trace) '()))
                        (list event))
                :delegation-provenance
                (list :schema-version 1
                      :request-id handoff-id
                      :context (%handoff-context-summary (getf details :context-packet))
                      :trace (append (copy-list (or (getf details :delegation-trace) '()))
                                     (list event))))))))
    (let ((payload (%compose-user-handoff-payload
                    handoff-id
                    status
                    (list :handoff-id handoff-id
                          :agent-id (or (getf status :to-agent)
                                        (getf status :to-agent-id)
                                        (getf response :to-agent)
                                        (getf response :to-agent-id))))))
      (%publish-user-coordination-event +event-type-user-handoff-accepted+ payload)
      payload)))

(defun reject-user-handoff (handoff-id reason
                             &key rejection-code
                               retry-after-ms
                               redirect-to-agent-id
                               result-payload
                               partial-result
                               diagnostics
                               provenance
                               budget-spent
                               artifacts
                               summary)
  "Reject HANDOFF-ID with optional protocol metadata."
  (let* ((resolved-code (or rejection-code sw4rm-sdk:+error-code-unspecified+))
         (response (sw4rm-sdk:reject-handoff-with-options (%ensure-user-handoff-client)
                                                          handoff-id
                                                          reason
                                                          :rejection-code resolved-code
                                                          :retry-after-ms retry-after-ms
                                                          :redirect-to-agent-id redirect-to-agent-id))
         (status (or (ignore-errors (user-handoff-status handoff-id))
                     response)))
    (%update-user-handoff-details!
     handoff-id
     (lambda (details)
       (let* ((rejected-at (getf (getf response :metadata) :rejected-at))
              (event (list :event :rejected
                           :rejected-at rejected-at
                           :reason reason
                           :rejection-code resolved-code
                           :partial-result-present-p (not (null partial-result))
                           :diagnostic-count (length (or diagnostics '()))))
              (trace (append (copy-list (or (getf details :delegation-trace) '()))
                             (list event)))
              (provenance-record (list :schema-version 1
                                       :request-id handoff-id
                                       :context (%handoff-context-summary
                                                 (getf details :context-packet))
                                       :trace trace
                                       :outcome (%coordination-copy-value provenance))))
         (%coordination-plist-merge
          details
          (list :summary summary
                :reason reason
                :result-payload (%coordination-copy-value result-payload)
                :partial-result (%coordination-copy-value partial-result)
                :diagnostics (%coordination-copy-value diagnostics)
                :budget-spent (%coordination-copy-value budget-spent)
                :artifacts (%coordination-copy-value artifacts)
                :delegation-trace trace
                :delegation-provenance provenance-record)))))
    (let ((payload (%compose-user-handoff-payload
                    handoff-id
                    status
                    (list :handoff-id handoff-id
                          :summary summary
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
      payload)))

(defun complete-user-handoff (handoff-id
                              &key result-payload
                                partial-result
                                diagnostics
                                provenance
                                budget-spent
                                artifacts
                                summary)
  "Mark HANDOFF-ID as complete."
  (let* ((response (sw4rm-sdk:complete-handoff (%ensure-user-handoff-client) handoff-id))
         (status (or (ignore-errors (user-handoff-status handoff-id))
                     response)))
    (%update-user-handoff-details!
     handoff-id
     (lambda (details)
       (let* ((completed-at (getf (getf response :metadata) :completed-at))
              (event (list :event :completed
                           :completed-at completed-at
                           :partial-result-present-p (not (null partial-result))
                           :diagnostic-count (length (or diagnostics '()))))
              (trace (append (copy-list (or (getf details :delegation-trace) '()))
                             (list event)))
              (provenance-record (list :schema-version 1
                                       :request-id handoff-id
                                       :context (%handoff-context-summary
                                                 (getf details :context-packet))
                                       :trace trace
                                       :outcome (%coordination-copy-value provenance))))
         (%coordination-plist-merge
          details
          (list :summary summary
                :result-payload (%coordination-copy-value result-payload)
                :partial-result (%coordination-copy-value partial-result)
                :diagnostics (%coordination-copy-value diagnostics)
                :budget-spent (%coordination-copy-value budget-spent)
                :artifacts (%coordination-copy-value artifacts)
                :delegation-trace trace
                :delegation-provenance provenance-record)))))
    (let ((payload (%compose-user-handoff-payload
                    handoff-id
                    status
                    (list :handoff-id handoff-id
                          :summary summary
                          :agent-id (or (getf status :to-agent)
                                        (getf status :to-agent-id)
                                        (getf response :to-agent)
                                        (getf response :to-agent-id))))))
      (%publish-user-coordination-event +event-type-user-handoff-completed+ payload)
      payload)))
