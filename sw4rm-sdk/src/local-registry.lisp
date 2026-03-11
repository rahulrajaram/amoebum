;;;; local-registry.lisp - In-process agent registry for SW4RM local mode

(in-package #:sw4rm-sdk)

;;; ---------------------------------------------------------------------------
;;; Conditions
;;; ---------------------------------------------------------------------------

(define-condition duplicate-agent-registration (sw4rm-error)
  ((agent-id
    :initarg :agent-id
    :reader duplicate-agent-registration-agent-id
    :type (or null string)
    :documentation "Agent identifier that triggered the duplicate registration.")
   (existing-config
    :initarg :existing-config
    :reader duplicate-agent-registration-existing-config
    :type (or null agent-config)
    :documentation "Existing agent configuration currently registered under AGENT-ID.")
   (attempted-config
    :initarg :attempted-config
    :reader duplicate-agent-registration-attempted-config
    :type (or null agent-config)
    :documentation "New agent configuration that attempted duplicate registration."))
  (:default-initargs :error-code +duplicate-detected+)
  (:documentation "Signaled when an attempt is made to register an already
registered agent-id.")
  (:report (lambda (condition stream)
             (format stream "Duplicate agent registration for id ~S"
                     (duplicate-agent-registration-agent-id condition)))))

(define-condition provider-secret-access-denied (sw4rm-error)
  ((requester-agent-id
    :initarg :requester-agent-id
    :reader provider-secret-access-denied-requester-agent-id
    :type (or null string)
    :documentation "Requester attempting to read a provider secret.")
   (target-agent-id
    :initarg :target-agent-id
    :reader provider-secret-access-denied-target-agent-id
    :type (or null string)
    :documentation "Agent that owns the provider secret.")
   (provider-key
    :initarg :provider-key
    :reader provider-secret-access-denied-provider-key
    :type (or null string)
    :documentation "Provider secret key requested."))
  (:default-initargs :error-code +permission-denied+)
  (:documentation "Signaled when one agent tries to read another agent's provider secret.")
  (:report (lambda (condition stream)
             (format stream
                     "Provider secret access denied for requester ~S on target ~S (key ~S)."
                     (provider-secret-access-denied-requester-agent-id condition)
                     (provider-secret-access-denied-target-agent-id condition)
                     (provider-secret-access-denied-provider-key condition)))))

;;; ---------------------------------------------------------------------------
;;; Registry entry and container
;;; ---------------------------------------------------------------------------

(defstruct local-registry-entry
  "Internal registry row for one agent."
  (agent-id nil :type string :read-only t)
  (config nil :type (or null agent-config) :read-only t)
  (capabilities nil :type list :read-only t)
  (registered-at (get-universal-time) :type integer :read-only t)
  (last-seen-at (get-universal-time) :type integer)
  (metadata nil :type list :read-only t))

(defstruct local-registry
  "Thread-safe local registry container.

The registry stores agent metadata keyed by AGENT-ID and maintains a
capability index for efficient find-by-capability lookups."
  (entries (make-hash-table :test #'equal) :read-only t)
  (capability-index (make-hash-table :test #'equal) :read-only t)
  (provider-secrets (make-hash-table :test #'equal) :read-only t)
  (lock (bt:make-lock "local-registry-lock") :read-only t))

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defparameter +if-exists-strategy+
  '(:error :replace :ignore))

(defun %normalize-agent-id (agent-id)
  "Normalize AGENT-ID for registry lookups."
  (string-trim '(#\Space #\Tab #\Newline #\Return)
              (princ-to-string agent-id)))

(defun %normalize-capability (value)
  "Normalize one capability value into lowercase string key."
  (let ((raw (cond
               ((stringp value) value)
               ((symbolp value) (symbol-name value))
               ((keywordp value) (symbol-name value))
               (t (princ-to-string value)))))
    (string-downcase
     (string-trim '(#\Space #\Tab #\Newline #\Return) raw))))

(defun %normalize-capabilities (capabilities)
  "Normalize and de-duplicate a list of capabilities."
  (let ((normalized nil))
    (dolist (cap capabilities)
      (let ((norm (%normalize-capability cap)))
        (when (and (stringp norm)
                   (not (string= norm "")))
          (pushnew norm normalized :test #'string=))))
    (sort (nreverse normalized) #'string<)))

(defun %normalize-provider-secret-key (provider-key)
  "Normalize provider secret key identifiers."
  (let* ((raw (cond
                ((stringp provider-key) provider-key)
                ((symbolp provider-key) (symbol-name provider-key))
                (t (princ-to-string provider-key))))
         (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) raw)))
    (if (zerop (length trimmed))
        ""
        (string-upcase trimmed))))

(defun %entry-config (entry)
  (local-registry-entry-config entry))

(defun %entry (registry agent-id)
  (gethash agent-id (local-registry-entries registry)))

(defun %read-agent-id (config)
  (let ((agent-id (agent-config-agent-id config)))
    (unless (and (stringp agent-id)
                 (not (string= agent-id "")))
      (error "agent-config must have a non-empty agent-id"))
    agent-id))

(defun %capability-bucket (registry capability)
  (let* ((capability-key (%normalize-capability capability))
         (capability-table (local-registry-capability-index registry)))
    (or (gethash capability-key capability-table)
        (setf (gethash capability-key capability-table)
              (make-hash-table :test #'equal)))))

(defun %index-entry (registry entry)
  (dolist (capability (local-registry-entry-capabilities entry))
    (setf (gethash (local-registry-entry-agent-id entry)
                   (%capability-bucket registry capability))
          t)))

(defun %unindex-entry (registry entry)
  (dolist (capability (local-registry-entry-capabilities entry))
    (let* ((capability-key (%normalize-capability capability))
           (capability-table (local-registry-capability-index registry))
           (bucket (gethash capability-key capability-table)))
      (when bucket
        (remhash (local-registry-entry-agent-id entry) bucket)
        (when (zerop (hash-table-count bucket))
          (remhash capability-key capability-table))))))

(defun %record (registry entry)
  (setf (gethash (local-registry-entry-agent-id entry)
                 (local-registry-entries registry))
        entry)
  (%index-entry registry entry))

(defun %provider-secret-table (registry agent-id &key (create-p nil))
  (let ((all-secrets (local-registry-provider-secrets registry))
        (normalized-agent-id (%normalize-agent-id agent-id)))
    (or (gethash normalized-agent-id all-secrets)
        (when create-p
          (setf (gethash normalized-agent-id all-secrets)
                (make-hash-table :test #'equal))))))

(defun %assert-provider-secret-access (requester-agent-id target-agent-id provider-key)
  (unless (string= requester-agent-id target-agent-id)
    (error 'provider-secret-access-denied
           :requester-agent-id requester-agent-id
           :target-agent-id target-agent-id
           :provider-key provider-key)))

;;; ---------------------------------------------------------------------------
;;; Public API
;;; ---------------------------------------------------------------------------

(defun local-registry-register (registry agent-config &key (if-exists :error))
  "Register AGENT-CONFIG in REGISTRY.

IF-EXISTS behavior:
  :ERROR   -> signal DUPLICATE-AGENT-REGISTRATION
  :REPLACE -> replace existing row
  :IGNORE  -> retain existing config and return it"
  (check-type registry local-registry)
  (check-type agent-config agent-config)
  (unless (member if-exists +if-exists-strategy+ :test #'eq)
    (error "Invalid if-exists strategy: ~S" if-exists))

  (let* ((agent-id (%normalize-agent-id (%read-agent-id agent-config)))
         (entry (make-local-registry-entry
                 :agent-id agent-id
                 :config agent-config
                 :capabilities (%normalize-capabilities
                                (agent-config-capabilities agent-config))
                 :registered-at (get-universal-time)
                 :last-seen-at (get-universal-time)
                 :metadata nil)))
    (bt:with-lock-held ((local-registry-lock registry))
      (let ((existing (%entry registry agent-id)))
        (when existing
          (ecase if-exists
            (:error
             (error 'duplicate-agent-registration
                    :agent-id agent-id
                    :existing-config (%entry-config existing)
                    :attempted-config agent-config))
            (:ignore
             (return-from local-registry-register
               (%entry-config existing)))
            (:replace
             (%unindex-entry registry existing))))
      (%record registry entry)))
  (local-registry-entry-config entry)))

(defun local-registry-unregister (registry agent-id)
  "Remove AGENT-ID from REGISTRY. Returns T if removed.
Returns NIL when no entry exists."
  (check-type registry local-registry)
  (let ((normalized (if (stringp agent-id)
                      (%normalize-agent-id agent-id)
                      agent-id)))
    (bt:with-lock-held ((local-registry-lock registry))
      (let ((entry (%entry registry normalized)))
        (when entry
          (%unindex-entry registry entry)
          (remhash normalized (local-registry-entries registry))
          (remhash normalized (local-registry-provider-secrets registry))
          t)))))

(defun local-registry-get (registry agent-id)
  "Return registered config for AGENT-ID, or NIL if not present."
  (check-type registry local-registry)
  (let ((normalized (if (stringp agent-id)
                     (%normalize-agent-id agent-id)
                     agent-id)))
    (bt:with-lock-held ((local-registry-lock registry))
      (let ((entry (%entry registry normalized)))
        (and entry (local-registry-entry-config entry))))))

(defun local-registry-get-entry (registry agent-id)
  "Return registry entry object for AGENT-ID, or NIL if not present."
  (check-type registry local-registry)
  (let ((normalized (if (stringp agent-id)
                     (%normalize-agent-id agent-id)
                     agent-id)))
    (bt:with-lock-held ((local-registry-lock registry))
      (%entry registry normalized))))

(defun local-registry-list (registry)
  "Return list of all registered agent configs."
  (check-type registry local-registry)
  (bt:with-lock-held ((local-registry-lock registry))
    (loop for entry being the hash-values of (local-registry-entries registry)
          collect (local-registry-entry-config entry))))

(defun local-registry-size (registry)
  "Return number of active registry entries."
  (check-type registry local-registry)
  (bt:with-lock-held ((local-registry-lock registry))
    (hash-table-count (local-registry-entries registry))))

(defun local-registry-clear (registry)
  "Clear all registry contents."
  (check-type registry local-registry)
  (bt:with-lock-held ((local-registry-lock registry))
    (clrhash (local-registry-entries registry))
    (clrhash (local-registry-capability-index registry))
    (clrhash (local-registry-provider-secrets registry))
    t))

(defun find-agents-by-capability (registry capability)
  "Return all agent configs having CAPABILITY."
  (check-type registry local-registry)
  (let ((capability-key (%normalize-capability capability)))
    (bt:with-lock-held ((local-registry-lock registry))
      (let ((bucket (gethash capability-key
                            (local-registry-capability-index registry))))
        (when (null bucket)
          (return-from find-agents-by-capability nil))
        (loop for agent-id being the hash-keys of bucket
              for entry = (%entry registry agent-id)
              when entry
                collect (local-registry-entry-config entry))))))

(defun local-registry-touch (registry agent-id)
  "Update LAST-SEEN timestamp for AGENT-ID.
Returns T when updated, NIL when missing."
  (check-type registry local-registry)
  (let ((normalized (if (stringp agent-id)
                     (%normalize-agent-id agent-id)
                     agent-id)))
    (bt:with-lock-held ((local-registry-lock registry))
      (let ((entry (%entry registry normalized)))
        (when entry
          (setf (local-registry-entry-last-seen-at entry)
                (get-universal-time))
          t)))))

(defun local-registry-set-provider-secret (registry agent-id provider-key secret-value)
  "Bind a provider credential for AGENT-ID.

This stores secret material in a private per-agent namespace attached to REGISTRY."
  (check-type registry local-registry)
  (let ((normalized-agent-id (%normalize-agent-id agent-id))
        (normalized-provider-key (%normalize-provider-secret-key provider-key)))
    (unless (and (stringp secret-value)
                 (plusp (length secret-value)))
      (error "provider secret value must be a non-empty string"))
    (when (zerop (length normalized-provider-key))
      (error "provider key must be a non-empty string or symbol"))
    (bt:with-lock-held ((local-registry-lock registry))
      (unless (%entry registry normalized-agent-id)
        (error "No registered agent for provider secret binding: ~S"
               normalized-agent-id))
      (let ((secrets (%provider-secret-table registry normalized-agent-id :create-p t)))
        (setf (gethash normalized-provider-key secrets) secret-value)
        t))))

(defun local-registry-resolve-provider-secret
    (registry requester-agent-id target-agent-id provider-key
     &key (signal-if-missing nil))
  "Resolve TARGET-AGENT-ID's provider secret for PROVIDER-KEY.

Only self-access is allowed: REQUESTER-AGENT-ID must equal TARGET-AGENT-ID."
  (check-type registry local-registry)
  (let* ((requester (%normalize-agent-id requester-agent-id))
         (target (%normalize-agent-id target-agent-id))
         (normalized-provider-key (%normalize-provider-secret-key provider-key)))
    (when (zerop (length normalized-provider-key))
      (error "provider key must be a non-empty string or symbol"))
    (bt:with-lock-held ((local-registry-lock registry))
      (%assert-provider-secret-access requester target normalized-provider-key)
      (let* ((secrets (%provider-secret-table registry target))
             (value (and secrets (gethash normalized-provider-key secrets))))
        (cond
          (value value)
          (signal-if-missing
           (error 'secret-not-found
                  :secret-key (format nil "~A/~A" target normalized-provider-key)))
          (t nil))))))

(defun local-registry-list-provider-secret-keys
    (registry requester-agent-id target-agent-id)
  "List provider key identifiers available for TARGET-AGENT-ID.

Only self-access is allowed."
  (check-type registry local-registry)
  (let ((requester (%normalize-agent-id requester-agent-id))
        (target (%normalize-agent-id target-agent-id)))
    (bt:with-lock-held ((local-registry-lock registry))
      (%assert-provider-secret-access requester target "<all>")
      (let ((secrets (%provider-secret-table registry target)))
        (if (null secrets)
            nil
            (loop for key being the hash-keys of secrets
                  collect key))))))

(defun local-registry-clear-provider-secrets (registry &optional agent-id)
  "Clear provider secrets for AGENT-ID, or for all agents when AGENT-ID is NIL."
  (check-type registry local-registry)
  (bt:with-lock-held ((local-registry-lock registry))
    (if agent-id
        (remhash (%normalize-agent-id agent-id)
                 (local-registry-provider-secrets registry))
        (clrhash (local-registry-provider-secrets registry)))
    t))
