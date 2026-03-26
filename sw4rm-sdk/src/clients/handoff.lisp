;;;; handoff.lisp - Handoff Service client for SW4RM
;;
;;;; Local in-memory SW4-004/SW4-005-compatible handoff behavior.

(in-package :sw4rm-sdk)

(defconstant +default-max-retries-on-overloaded+ 2
  "Default SW4-004 retry count for OVERLOADED.")

(defconstant +default-initial-backoff-ms+ 250
  "Default SW4-004 initial retry backoff in milliseconds.")

(defconstant +default-backoff-multiplier+ 2.0d0
  "Default SW4-004 retry backoff multiplier.")

(defconstant +default-max-backoff-ms+ 2000
  "Default SW4-004 max retry backoff in milliseconds.")

(defconstant +default-allow-spillover-routing+ nil
  "Default SW4-005 spillover routing policy.")

(defconstant +default-max-redirects+ 0
  "Default SW4-005 max redirect hops.")

(defconstant +retry-after-jitter-ratio+ 0.2d0
  "Jitter ratio used with retry-after hints for overloaded retries.")

(defconstant +default-effective-max-redirects+ 2
  "Effective default redirect follow bound when max-redirects is unset/zero.")

(defconstant +min-cancel-grace-period-ms+ 5000
  "Minimum cancellation grace period in milliseconds (SW4-004 §5).")

(defclass handoff-client (base-client)
  ((handoffs
    :initform (make-hash-table :test 'equal)
    :accessor handoff-client-handoffs
    :documentation "Map: handoff-id -> (:request plist :response plist).")
   (pending-by-agent
    :initform (make-hash-table :test 'equal)
    :accessor handoff-client-pending-by-agent
    :documentation "Map: to-agent -> list of pending handoff IDs.")
   (child-delegations
    :initform (make-hash-table :test 'equal)
    :accessor handoff-client-child-delegations
    :documentation "Map: parent-correlation-id -> list of child correlation IDs.")
   (cancellation-flags
    :initform (make-hash-table :test 'equal)
    :accessor handoff-client-cancellation-flags
    :documentation "Map: correlation-id -> cancellation flag plist."))
  (:documentation "In-memory handoff client with SW4-004/SW4-005 extensions."))

(define-condition handoff-rejected (sw4rm-error)
  ((handoff-id
    :initarg :handoff-id
    :reader handoff-rejected-handoff-id)
   (response
    :initarg :response
    :reader handoff-rejected-response)
   (rejection-code
    :initarg :rejection-code
    :reader handoff-rejected-rejection-code)
   (rejection-reason
    :initarg :rejection-reason
    :reader handoff-rejected-rejection-reason))
  (:default-initargs :error-code +no-route+)
  (:report (lambda (condition stream)
             (format stream "Handoff ~A rejected (~A): ~A"
                     (handoff-rejected-handoff-id condition)
                     (handoff-rejected-rejection-code condition)
                     (handoff-rejected-rejection-reason condition)))))

(defstruct (handoff-request
             (:constructor make-handoff-request
                 (&key request-id
                       from-agent
                       to-agent
                       reason
                       budget
                       delegation-policy
                       (context-snapshot "")
                       (capabilities-required '())
                       (priority 0)
                       timeout-ms)))
  "Normalized caller-side handoff envelope for delegation negotiation."
  request-id
  from-agent
  to-agent
  reason
  budget
  delegation-policy
  (context-snapshot "" :type string)
  (capabilities-required '() :type list)
  (priority 0)
  timeout-ms)

(defstruct (delegation-runtime
             (:constructor %make-delegation-runtime
                 (&key request
                       request-id
                       budget
                       policy
                       redirect-bound
                       max-retries-on-overloaded
                       visited-agents
                       now-fn
                       sleep-fn
                       rand-fn
                       (retry-index 0)
                       (redirect-hops 0))))
  request
  request-id
  budget
  policy
  redirect-bound
  max-retries-on-overloaded
  visited-agents
  now-fn
  sleep-fn
  rand-fn
  (retry-index 0)
  (redirect-hops 0))

(defun %required-string (plist key)
  "Read KEY from PLIST and require a non-empty string value."
  (let ((value (getf plist key)))
    (unless (and (stringp value) (> (length value) 0))
      (error 'validation-error
             :message (format nil "~A is required and must be a non-empty string" key)
             :field (string-downcase (symbol-name key))
             :constraint "non-empty string"))
    value))

(defun %now-ms ()
  "Current wall time in milliseconds."
  (current-time-ms))

(defun %copy-plist (plist)
  "Return a shallow copy of PLIST."
  (copy-list plist))

(defparameter +handoff-sensitive-request-keys+
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

(defun %handoff-sensitive-key-p (key)
  (let* ((text (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                              (princ-to-string key)))))
    (member text +handoff-sensitive-request-keys+ :test #'string=)))

(defun %handoff-plist-like-p (value)
  (and (listp value)
       (evenp (length value))
       (loop for (key _value) on value by #'cddr
             always (or (keywordp key)
                        (symbolp key)
                        (stringp key)))))

(defun %sanitize-handoff-value (value)
  (cond
    ((hash-table-p value)
     (let ((clean (make-hash-table :test (hash-table-test value))))
       (maphash (lambda (key inner-value)
                  (unless (%handoff-sensitive-key-p key)
                    (setf (gethash key clean)
                          (%sanitize-handoff-value inner-value))))
                value)
       clean))
    ((%handoff-plist-like-p value)
     (let ((clean '()))
       (loop for (key inner-value) on value by #'cddr do
         (unless (%handoff-sensitive-key-p key)
           (setf clean
                 (append clean
                         (list key (%sanitize-handoff-value inner-value))))))
       clean))
    ((and (listp value)
          (every #'consp value))
     (let ((clean '()))
       (dolist (entry value (nreverse clean))
         (unless (%handoff-sensitive-key-p (car entry))
           (let ((tail (cdr entry)))
             (push (cons (car entry)
                         (%sanitize-handoff-value
                 (if (and (consp tail) (null (cdr tail)))
                              (car tail)
                              tail)))
                   clean))))))
    ((listp value)
     (mapcar #'%sanitize-handoff-value value))
    (t value)))

(defun %sanitize-handoff-request (request)
  "Drop sensitive provider credential fields from REQUEST recursively."
  (if (not (%handoff-plist-like-p request))
      request
      (let ((clean '()))
        (loop for (key value) on request by #'cddr do
          (unless (%handoff-sensitive-key-p key)
            (setf clean
                  (append clean
                          (list key (%sanitize-handoff-value value))))))
        clean)))

(defun serialize-handoff-context (context &key (max-bytes 65536))
  "Serialize handoff CONTEXT to JSON, truncating if MAX-BYTES is exceeded."
  (let* ((as-json (if (stringp context)
                      context
                      (jonathan:to-json context)))
         (length-bytes (length as-json)))
    (if (> length-bytes max-bytes)
        (subseq as-json 0 max-bytes)
        as-json)))

(defun deserialize-handoff-context (serialized-context)
  "Parse a JSON handoff context payload."
  (cond
    ((or (null serialized-context) (string= serialized-context "")) nil)
    ((stringp serialized-context)
     (jonathan:parse serialized-context :as :plist))
    (t serialized-context)))

(defun %normalize-positive-integer (value fallback)
  "Return VALUE when it is a positive integer, otherwise FALLBACK."
  (if (and (integerp value) (> value 0))
      value
      fallback))

(defun %normalize-positive-number (value fallback)
  "Return VALUE when it is a positive real, otherwise FALLBACK."
  (if (and (realp value) (> value 0))
      value
      fallback))

(defun handoff-default-delegation-policy ()
  "Build default SW4-004/SW4-005 delegation policy plist."
  (list :max-retries-on-overloaded +default-max-retries-on-overloaded+
        :initial-backoff-ms +default-initial-backoff-ms+
        :backoff-multiplier +default-backoff-multiplier+
        :max-backoff-ms +default-max-backoff-ms+
        :allow-spillover-routing +default-allow-spillover-routing+
        :max-redirects +default-max-redirects+))

(defun %normalize-delegation-policy (policy)
  "Normalize optional delegation policy values with protocol defaults."
  (let ((defaults (handoff-default-delegation-policy)))
    (list :max-retries-on-overloaded
          (if (and (integerp (getf policy :max-retries-on-overloaded))
                   (>= (getf policy :max-retries-on-overloaded) 0))
              (getf policy :max-retries-on-overloaded)
              (getf defaults :max-retries-on-overloaded))
          :initial-backoff-ms
          (%normalize-positive-integer
           (getf policy :initial-backoff-ms)
           (getf defaults :initial-backoff-ms))
          :backoff-multiplier
          (%normalize-positive-number
           (getf policy :backoff-multiplier)
           (getf defaults :backoff-multiplier))
          :max-backoff-ms
          (%normalize-positive-integer
           (getf policy :max-backoff-ms)
           (getf defaults :max-backoff-ms))
          :allow-spillover-routing
          (if (null (getf policy :allow-spillover-routing))
              (getf defaults :allow-spillover-routing)
              (not (null (getf policy :allow-spillover-routing))))
          :max-redirects
          (if (and (integerp (getf policy :max-redirects))
                   (>= (getf policy :max-redirects) 0))
              (getf policy :max-redirects)
              (getf defaults :max-redirects)))))

(defun %default-sleep-seconds (seconds)
  "Sleep for SECONDS."
  (sleep seconds))

(defun %default-rand-uniform (low high)
  "Sample uniformly in [LOW, HIGH), clamped when HIGH <= LOW."
  (let ((low-float (coerce low 'double-float))
        (high-float (coerce high 'double-float)))
    (if (<= high-float low-float)
        low-float
        (+ low-float (* (random 1.0d0) (- high-float low-float))))))

(defun %deadline-exhausted-response (request-id)
  "Build an ACK_TIMEOUT handoff response for exhausted delegation budget."
  (list :request-id request-id
        :handoff-id request-id
        :accepted nil
        :status :rejected
        :rejection-reason "Delegation deadline exhausted before handoff acceptance"
        :rejection-code +ack-timeout+
        :retry-after-ms nil
        :redirect-to-agent-id nil))

(defun %invalid-redirect-response (request-id reason)
  "Build a validation-error handoff response for invalid redirect metadata."
  (list :request-id request-id
        :handoff-id request-id
        :accepted nil
        :status :rejected
        :rejection-reason reason
        :rejection-code +validation-error+
        :retry-after-ms nil
        :redirect-to-agent-id nil))

(defun %handle-rejection-restart (response handoff-id)
  "Expose a restartable rejection hook.

Returns two values:
  action    - :RETURN or :RETRY
  next-agent-id - non-empty string when action is :RETRY."
  (let ((action :return)
        (next-agent-id nil))
    (restart-case
        (signal 'handoff-rejected
                :message (or (getf response :rejection-reason) "handoff rejected")
                :handoff-id handoff-id
                :response response
                :rejection-code (getf response :rejection-code)
                :rejection-reason (or (getf response :rejection-reason) "unknown"))
      (try-next-agent (agent-id)
        :report "Retry delegation against another agent."
        (setf action :retry)
        (setf next-agent-id
              (and (stringp agent-id)
                   (string-trim '(#\Space #\Tab #\Newline #\Return) agent-id))))
      (return-rejection ()
        :report "Return the original rejection response."
        (setf action :return)))
    (values action next-agent-id)))

(defun %effective-max-redirects (policy)
  "Return configured max redirects, or effective default when not positive."
  (let ((configured (getf policy :max-redirects)))
    (if (and (integerp configured) (> configured 0))
        configured
        +default-effective-max-redirects+)))

(defun %budget-exhausted-p (budget now-ms)
  "Return T when BUDGET cannot fund additional handoff attempts."
  (let ((wall-time (getf budget :wall-time-remaining-ms))
        (deadline (getf budget :deadline-epoch-ms)))
    (or (and (integerp wall-time) (<= wall-time 0))
        (> now-ms deadline))))

(defun %consume-wall-time (budget elapsed-ms)
  "Deduct ELAPSED-MS from BUDGET wall-time in place when present."
  (let ((remaining (getf budget :wall-time-remaining-ms)))
    (when (integerp remaining)
      (setf (getf budget :wall-time-remaining-ms)
            (max 0 (- remaining (max elapsed-ms 0)))))))

(defun %next-retry-wait-ms (response retry-index policy rand-uniform-fn)
  "Compute overloaded retry wait with retry-after+jitter or exponential backoff."
  (let ((retry-after-ms (getf response :retry-after-ms)))
    (if (and (integerp retry-after-ms) (> retry-after-ms 0))
        (let* ((retry-after (coerce retry-after-ms 'double-float))
               (jitter (funcall rand-uniform-fn
                                0.0d0
                                (* retry-after +retry-after-jitter-ratio+))))
          (max 0 (floor (+ retry-after (max jitter 0.0d0)))))
        (let* ((initial-backoff-ms
                 (coerce (getf policy :initial-backoff-ms) 'double-float))
               (backoff-multiplier
                 (coerce (getf policy :backoff-multiplier) 'double-float))
               (max-backoff-ms
                 (coerce (getf policy :max-backoff-ms) 'double-float))
               (exponential (* initial-backoff-ms (expt backoff-multiplier retry-index)))
               (bounded (max 0.0d0 (min exponential max-backoff-ms))))
          (max 0 (floor (max 0.0d0 (funcall rand-uniform-fn 0.0d0 bounded))))))))

(defun %coerce-handoff-request (request)
  "Return REQUEST as a handoff-request struct."
  (cond
    ((handoff-request-p request) request)
    ((listp request)
     (make-handoff-request
      :request-id (or (getf request :request-id) (getf request :handoff-id))
      :from-agent (getf request :from-agent)
      :to-agent (getf request :to-agent)
      :reason (getf request :reason)
      :budget (getf request :budget)
      :delegation-policy (getf request :delegation-policy)
      :context-snapshot
      (or (getf request :context-snapshot)
          (getf request :context)
          "")
      :capabilities-required (copy-list (or (getf request :capabilities-required) '()))
      :priority (or (getf request :priority) 0)
      :timeout-ms (getf request :timeout-ms)))
    (t
     (error 'validation-error
            :message "handoff-request must be a plist or handoff-request struct"
            :field "handoff-request"
            :constraint "plist-or-struct"))))

(defun %validate-delegation-request (request)
  "Validate REQUEST before caller-side delegation begins."
  (%required-string (list :value (handoff-request-from-agent request)) :value)
  (%required-string (list :value (handoff-request-to-agent request)) :value)
  (%required-string (list :value (handoff-request-reason request)) :value)
  (unless (listp (handoff-request-budget request))
    (error 'validation-error
           :message "budget plist is required for cross-swarm delegation"
           :field "budget"
           :constraint "plist"))
  (let ((deadline (getf (handoff-request-budget request) :deadline-epoch-ms)))
    (unless (and (integerp deadline) (> deadline 0))
      (error 'validation-error
             :message "budget.deadline-epoch-ms is required for cross-swarm delegation"
             :field "budget.deadline-epoch-ms"
             :constraint "positive integer")))
  (let ((timeout-ms (handoff-request-timeout-ms request)))
    (when (and timeout-ms (or (not (integerp timeout-ms)) (< timeout-ms 0)))
      (error 'validation-error
             :message "timeout-ms must be >= 0"
             :field "timeout-ms"
             :constraint "non-negative integer"))))

(defun %prepare-handoff-envelope (request)
  "Build the plist envelope sent to SEND-HANDOFF-FN."
  (let* ((normalized-request (%coerce-handoff-request request))
         (_ (%validate-delegation-request normalized-request))
         (policy (%normalize-delegation-policy
                  (handoff-request-delegation-policy normalized-request)))
         (request-budget (%copy-plist (handoff-request-budget normalized-request)))
         (request-id (or (handoff-request-request-id normalized-request)
                         (generate-uuid)))
         (envelope
           (list :request-id request-id
                 :handoff-id request-id
                 :from-agent (handoff-request-from-agent normalized-request)
                 :to-agent (handoff-request-to-agent normalized-request)
                 :reason (handoff-request-reason normalized-request)
                 :context-snapshot (handoff-request-context-snapshot normalized-request)
                 :capabilities-required
                 (copy-list (handoff-request-capabilities-required normalized-request))
                 :priority (handoff-request-priority normalized-request)
                 :budget request-budget
                 :delegation-policy policy)))
    (when (handoff-request-timeout-ms normalized-request)
      (setf (getf envelope :timeout-ms)
            (handoff-request-timeout-ms normalized-request)))
    (values envelope request-id policy request-budget)))

(defun %make-delegation-state (request now-ms-fn sleep-seconds-fn rand-uniform-fn)
  "Create mutable runtime state for delegation negotiation."
  (multiple-value-bind (envelope request-id policy budget)
      (%prepare-handoff-envelope request)
    (%make-delegation-runtime
     :request envelope
     :request-id request-id
     :budget budget
     :policy policy
     :redirect-bound (%effective-max-redirects policy)
     :max-retries-on-overloaded (getf policy :max-retries-on-overloaded)
     :visited-agents (list (getf envelope :to-agent))
     :now-fn (or now-ms-fn #'%now-ms)
     :sleep-fn (or sleep-seconds-fn #'%default-sleep-seconds)
     :rand-fn (or rand-uniform-fn #'%default-rand-uniform))))

(defun %delegation-budget-exhausted-p (runtime now-ms)
  "Return T when RUNTIME cannot fund another attempt."
  (%budget-exhausted-p (delegation-runtime-budget runtime) now-ms))

(defun %perform-handoff-attempt (runtime send-handoff-fn start-ms)
  "Execute one handoff attempt and account for elapsed wall time."
  (let* ((response (funcall send-handoff-fn
                            (%copy-plist (delegation-runtime-request runtime))))
         (end-ms (funcall (delegation-runtime-now-fn runtime)))
         (elapsed-ms (max (- end-ms start-ms) 0)))
    (%consume-wall-time (delegation-runtime-budget runtime) elapsed-ms)
    (values response end-ms)))

(defun %retry-wait-allowed-p (runtime wait-ms end-ms)
  "Return T when WAIT-MS fits inside the remaining delegation budget."
  (let ((remaining-deadline-ms
          (- (getf (delegation-runtime-budget runtime) :deadline-epoch-ms) end-ms))
        (remaining-wall-time-ms
          (getf (delegation-runtime-budget runtime) :wall-time-remaining-ms)))
    (and (> wait-ms 0)
         (or (not (integerp remaining-wall-time-ms))
             (<= wait-ms remaining-wall-time-ms))
         (<= wait-ms remaining-deadline-ms))))

(defun %sleep-for-retry (runtime wait-ms)
  "Sleep for WAIT-MS and deduct the observed wall-time cost."
  (let ((before-sleep-ms (funcall (delegation-runtime-now-fn runtime))))
    (funcall (delegation-runtime-sleep-fn runtime) (/ wait-ms 1000.0d0))
    (let ((after-sleep-ms (funcall (delegation-runtime-now-fn runtime))))
      (%consume-wall-time (delegation-runtime-budget runtime)
                          (max (- after-sleep-ms before-sleep-ms) 0)))))

(defun %handle-overloaded-response (runtime response end-ms)
  "Advance overloaded retry negotiation.

Returns two values:
  action   - :retry or :return
  payload  - response plist when action is :return; otherwise NIL."
  (if (>= (delegation-runtime-retry-index runtime)
          (delegation-runtime-max-retries-on-overloaded runtime))
      (values :return response)
      (let ((wait-ms (%next-retry-wait-ms
                      response
                      (delegation-runtime-retry-index runtime)
                      (delegation-runtime-policy runtime)
                      (delegation-runtime-rand-fn runtime))))
        (incf (delegation-runtime-retry-index runtime))
        (if (%retry-wait-allowed-p runtime wait-ms end-ms)
            (progn
              (%sleep-for-retry runtime wait-ms)
              (values :retry nil))
            (values :return response)))))

(defun %redirect-target-from-response (response)
  "Return the normalized redirect target from RESPONSE."
  (let ((raw-target (getf response :redirect-to-agent-id)))
    (and (stringp raw-target)
         (string-trim '(#\Space #\Tab #\Newline #\Return) raw-target))))

(defun %apply-next-target (runtime next-target)
  "Retarget RUNTIME to NEXT-TARGET and record it in the visited set."
  (setf (getf (delegation-runtime-request runtime) :to-agent) next-target)
  (push next-target (delegation-runtime-visited-agents runtime)))

(defun %handle-rejection-response (runtime response)
  "Process non-redirect rejection responses.

Returns two values:
  action   - :retry or :return
  payload  - response plist when action is :return; otherwise NIL."
  (multiple-value-bind (action next-target)
      (%handle-rejection-restart response (delegation-runtime-request-id runtime))
    (if (and (eq action :retry)
             (stringp next-target)
             (> (length next-target) 0)
             (not (member next-target
                          (delegation-runtime-visited-agents runtime)
                          :test #'string=)))
        (progn
          (%apply-next-target runtime next-target)
          (values :retry nil))
        (values :return response))))

(defun %handle-redirect-response (runtime response)
  "Process SW4-005 redirect responses.

Returns two values:
  action   - :retry or :return
  payload  - response plist when action is :return; otherwise NIL."
  (if (not (getf (delegation-runtime-policy runtime) :allow-spillover-routing))
      (values :return response)
      (let ((target-agent (%redirect-target-from-response response)))
        (cond
          ((or (null target-agent) (= (length target-agent) 0))
           (values :return
                   (%invalid-redirect-response
                    (delegation-runtime-request-id runtime)
                    "Redirect response missing non-empty redirect_to_agent_id")))
          ((member target-agent
                   (delegation-runtime-visited-agents runtime)
                   :test #'string=)
           (values :return
                   (%invalid-redirect-response
                    (delegation-runtime-request-id runtime)
                    (format nil "Redirect loop detected for agent '~A'" target-agent))))
          ((>= (delegation-runtime-redirect-hops runtime)
               (delegation-runtime-redirect-bound runtime))
           (values :return response))
          (t
           (%apply-next-target runtime target-agent)
           (incf (delegation-runtime-redirect-hops runtime))
           (values :retry nil))))))

(defun %negotiate-handoff-response (runtime response end-ms)
  "Route RESPONSE through accepted/overloaded/rejection/redirect phases."
  (cond
    ((getf response :accepted)
     (values :return response))
    ((%delegation-budget-exhausted-p runtime end-ms)
     (values :return
             (%deadline-exhausted-response
              (delegation-runtime-request-id runtime))))
    ((eql (getf response :rejection-code) +overloaded+)
     (%handle-overloaded-response runtime response end-ms))
    ((eql (getf response :rejection-code) +redirect+)
     (%handle-redirect-response runtime response))
    (t
     (%handle-rejection-response runtime response))))

(defun delegate-to-swarm
    (send-handoff-fn handoff-request
     &key now-ms-fn sleep-seconds-fn rand-uniform-fn)
  "Execute caller-side SW4-005 delegation redirect/retry semantics.

SEND-HANDOFF-FN receives a normalized handoff request plist and must return a
handoff response plist. HANDOFF-REQUEST may be a plist or handoff-request
struct."
  (let ((runtime (%make-delegation-state
                  handoff-request
                  now-ms-fn
                  sleep-seconds-fn
                  rand-uniform-fn)))
    (loop
      for start-ms = (funcall (delegation-runtime-now-fn runtime))
      do
         (when (%delegation-budget-exhausted-p runtime start-ms)
           (return (%deadline-exhausted-response
                    (delegation-runtime-request-id runtime))))
         (multiple-value-bind (response end-ms)
             (%perform-handoff-attempt runtime send-handoff-fn start-ms)
           (multiple-value-bind (action payload)
               (%negotiate-handoff-response runtime response end-ms)
             (when (eq action :return)
               (return payload)))))))

(defun %normalize-handoff-request (request)
  "Validate and normalize a handoff REQUEST plist.

Returns two values: normalized-request and handoff-id."
  (let* ((request-plist
           (if (handoff-request-p request)
               (multiple-value-bind (envelope _request-id _policy _budget)
                   (%prepare-handoff-envelope request)
                 (declare (ignore _request-id _policy _budget))
                 envelope)
               request)))
    (%required-string request-plist :from-agent)
    (%required-string request-plist :to-agent)
    (%required-string request-plist :reason)
    (let* ((normalized (%copy-plist (%sanitize-handoff-request request-plist)))
         (budget (getf normalized :budget))
         (policy (getf normalized :delegation-policy))
         (handoff-id (or (getf normalized :request-id)
                         (getf normalized :handoff-id)
                         (generate-uuid))))
    (when budget
      (let ((deadline (getf budget :deadline-epoch-ms)))
        (unless (and (integerp deadline) (> deadline 0))
          (error 'validation-error
                 :message "budget.deadline-epoch-ms is required for cross-swarm delegation"
                 :field "budget.deadline-epoch-ms"
                 :constraint "positive integer"))))

    (when (or budget policy)
      (setf (getf normalized :delegation-policy)
            (%normalize-delegation-policy policy)))

    (unless (getf normalized :created-at)
      (setf (getf normalized :created-at) (%now-ms)))

    (setf (getf normalized :request-id) handoff-id)
    (setf (getf normalized :handoff-id) handoff-id)

      (values normalized handoff-id))))

(defun %get-handoff-entry-or-signal (client handoff-id)
  "Fetch a handoff entry or signal RPC-ERROR if it does not exist."
  (or (gethash handoff-id (handoff-client-handoffs client))
      (error 'rpc-error
             :message (format nil "Handoff ~A not found" handoff-id)
             :status-code "NOT_FOUND"
             :details "handoff id does not exist")))

(defun %remove-pending-id (client agent-id handoff-id)
  "Remove HANDOFF-ID from AGENT-ID pending list."
  (let* ((pending-map (handoff-client-pending-by-agent client))
         (pending (gethash agent-id pending-map)))
    (when pending
      (setf (gethash agent-id pending-map)
            (remove handoff-id pending :test #'string=)))))

(defun %ensure-pending-status (response handoff-id)
  "Require RESPONSE status to be :PENDING for response mutation methods."
  (unless (eq (getf response :status) :pending)
    (error 'rpc-error
           :message (format nil "Handoff ~A is not in PENDING status" handoff-id)
           :status-code "FAILED_PRECONDITION"
           :details (format nil "current status: ~A" (getf response :status)))))

(defgeneric initiate-handoff (client request)
  (:documentation "Create a handoff request in local in-memory storage."))

(defmethod initiate-handoff ((client handoff-client) request)
  (ensure-connected client)
  (multiple-value-bind (normalized-request handoff-id)
      (%normalize-handoff-request request)
    (let ((handoffs (handoff-client-handoffs client))
          (pending-map (handoff-client-pending-by-agent client)))
      (when (gethash handoff-id handoffs)
        (error 'validation-error
               :message (format nil "Handoff request with ID '~A' already exists" handoff-id)
               :field "request-id"
               :constraint "must be unique"))

      (let ((response (list :accepted t
                            :handoff-id handoff-id
                            :request-id handoff-id
                            :status :pending
                            :accepting-agent nil
                            :rejection-reason nil
                            :rejection-code nil
                            :retry-after-ms nil
                            :redirect-to-agent-id nil
                            :metadata (list :created-at (%now-ms)))))
        (setf (gethash handoff-id handoffs)
              (list :request normalized-request
                    :response response))

        (let ((to-agent (getf normalized-request :to-agent)))
          (setf (gethash to-agent pending-map)
                (append (gethash to-agent pending-map) (list handoff-id))))

        (%copy-plist response)))))

(defgeneric accept-handoff (client handoff-id)
  (:documentation "Accept a pending handoff request."))

(defmethod accept-handoff ((client handoff-client) handoff-id)
  (ensure-connected client)
  (let* ((entry (%get-handoff-entry-or-signal client handoff-id))
         (request (getf entry :request))
         (response (getf entry :response))
         (to-agent (getf request :to-agent)))
    (%ensure-pending-status response handoff-id)
    (setf (getf response :accepted) t)
    (setf (getf response :status) :accepted)
    (setf (getf response :accepting-agent) to-agent)
    (setf (getf response :metadata)
          (append (getf response :metadata)
                  (list :accepted-at (%now-ms))))
    (%remove-pending-id client to-agent handoff-id)
    (%copy-plist response)))

(defgeneric reject-handoff (client handoff-id reason)
  (:documentation "Reject a pending handoff request with default metadata."))

(defgeneric reject-handoff-with-options
    (client handoff-id reason &key rejection-code retry-after-ms redirect-to-agent-id)
  (:documentation "Reject a pending handoff request with SW4-004/SW4-005 metadata."))

(defmethod reject-handoff ((client handoff-client) handoff-id reason)
  (reject-handoff-with-options client handoff-id reason))

(defmethod reject-handoff-with-options
    ((client handoff-client) handoff-id reason
     &key
       (rejection-code +error-code-unspecified+)
       retry-after-ms
       redirect-to-agent-id)
  (ensure-connected client)
  (let* ((entry (%get-handoff-entry-or-signal client handoff-id))
         (request (getf entry :request))
         (response (getf entry :response))
         (to-agent (getf request :to-agent)))
    (%ensure-pending-status response handoff-id)
    (setf (getf response :accepted) nil)
    (setf (getf response :status) :rejected)
    (setf (getf response :rejection-reason) reason)
    (setf (getf response :rejection-code) rejection-code)
    (setf (getf response :retry-after-ms)
          (and (integerp retry-after-ms) (> retry-after-ms 0) retry-after-ms))
    (setf (getf response :redirect-to-agent-id)
          (and (stringp redirect-to-agent-id)
               (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                           redirect-to-agent-id)))
                 (and (> (length trimmed) 0) trimmed))))
    (setf (getf response :metadata)
          (append (getf response :metadata)
                  (list :rejected-at (%now-ms))))
    (%remove-pending-id client to-agent handoff-id)
    (%copy-plist response)))

(defgeneric complete-handoff (client handoff-id)
  (:documentation "Mark an accepted handoff as completed."))

(defmethod complete-handoff ((client handoff-client) handoff-id)
  (ensure-connected client)
  (let* ((entry (%get-handoff-entry-or-signal client handoff-id))
         (response (getf entry :response)))
    (unless (eq (getf response :status) :accepted)
      (error 'rpc-error
             :message (format nil "Handoff ~A is not in ACCEPTED status" handoff-id)
             :status-code "FAILED_PRECONDITION"
             :details (format nil "current status: ~A" (getf response :status))))

    (setf (getf response :status) :completed)
    (setf (getf response :metadata)
          (append (getf response :metadata)
                  (list :completed-at (%now-ms))))
    (%copy-plist response)))

(defgeneric get-pending-handoffs (client agent-id)
  (:documentation "Return pending handoff requests for AGENT-ID."))

(defmethod get-pending-handoffs ((client handoff-client) agent-id)
  (ensure-connected client)
  (let* ((pending-ids (copy-list (gethash agent-id (handoff-client-pending-by-agent client))))
         (handoffs (handoff-client-handoffs client))
         (results '()))
    (dolist (handoff-id pending-ids (nreverse results))
      (let ((entry (gethash handoff-id handoffs)))
        (when (and entry
                   (eq (getf (getf entry :response) :status) :pending))
          (push (%copy-plist (getf entry :request)) results))))))

(defgeneric get-handoff-status (client handoff-id)
  (:documentation "Return handoff response plist for HANDOFF-ID, or NIL."))

(defmethod get-handoff-status ((client handoff-client) handoff-id)
  (ensure-connected client)
  (let ((entry (gethash handoff-id (handoff-client-handoffs client))))
    (when entry
      (%copy-plist (getf entry :response)))))

(defgeneric register-child-delegation (client parent-correlation-id child-correlation-id)
  (:documentation "Link parent/child delegation IDs for cancellation cascade."))

(defmethod register-child-delegation
    ((client handoff-client) parent-correlation-id child-correlation-id)
  (ensure-connected client)
  (%required-string (list :value parent-correlation-id) :value)
  (%required-string (list :value child-correlation-id) :value)
  (let* ((child-map (handoff-client-child-delegations client))
         (children (gethash parent-correlation-id child-map)))
    (unless (member child-correlation-id children :test #'string=)
      (setf (gethash parent-correlation-id child-map)
            (append children (list child-correlation-id)))))
  t)

(defgeneric cancel-delegation (client cancel-request)
  (:documentation "Set cancellation flags for a correlation and known children."))

(defmethod cancel-delegation ((client handoff-client) cancel-request)
  (ensure-connected client)
  (let* ((correlation-id (%required-string cancel-request :correlation-id))
         (requested-grace (or (getf cancel-request :grace-period-ms) 0))
         (grace-period-ms (max +min-cancel-grace-period-ms+
                               (if (and (integerp requested-grace)
                                        (> requested-grace 0))
                                   requested-grace
                                   0)))
         (reason (or (getf cancel-request :reason) ""))
         (cancel-time-ms (%now-ms))
         (flags (handoff-client-cancellation-flags client))
         (children (gethash correlation-id (handoff-client-child-delegations client)))
         (flag (list :cancelled t
                     :reason reason
                     :grace-period-ms grace-period-ms
                     :cancel-time-ms cancel-time-ms)))
    (setf (gethash correlation-id flags) (%copy-plist flag))
    (dolist (child-correlation-id children)
      (setf (gethash child-correlation-id flags) (%copy-plist flag)))
    (list :acknowledged t
          :correlation-id correlation-id
          :grace-period-ms grace-period-ms
          :message "Cancellation recorded")))

(defgeneric cancelled-delegation-p (client correlation-id)
  (:documentation "Return T when CORRELATION-ID has an active cancellation flag."))

(defmethod cancelled-delegation-p ((client handoff-client) correlation-id)
  (ensure-connected client)
  (let ((entry (gethash correlation-id (handoff-client-cancellation-flags client))))
    (and entry (not (null (getf entry :cancelled))))))

(defgeneric cancellation-grace-expired-p (client correlation-id &optional now-ms)
  (:documentation "Return T when cancellation grace period has elapsed."))

(defmethod cancellation-grace-expired-p
    ((client handoff-client) correlation-id &optional now-ms)
  (ensure-connected client)
  (let ((entry (gethash correlation-id (handoff-client-cancellation-flags client))))
    (if (and entry (getf entry :cancelled))
        (>= (- (or now-ms (%now-ms))
               (getf entry :cancel-time-ms))
            (getf entry :grace-period-ms))
        nil)))

(defgeneric forced-preemption-error-code (client correlation-id &optional now-ms)
  (:documentation "Return FORCED_PREEMPTION once cancellation grace expires."))

(defmethod forced-preemption-error-code
    ((client handoff-client) correlation-id &optional now-ms)
  (if (cancellation-grace-expired-p client correlation-id now-ms)
      +forced-preemption+
      +error-code-unspecified+))

(defgeneric collect-forced-preemptions (client correlation-ids &optional now-ms)
  (:documentation "Collect correlation IDs that have exceeded cancellation grace."))

(defmethod collect-forced-preemptions
    ((client handoff-client) correlation-ids &optional now-ms)
  (ensure-connected client)
  (let ((forced '())
        (check-now (or now-ms (%now-ms))))
    (dolist (correlation-id correlation-ids (nreverse forced))
      (when (cancellation-grace-expired-p client correlation-id check-now)
        (push correlation-id forced)))))
