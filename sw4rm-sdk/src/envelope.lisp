;;;; envelope.lisp - Envelope construction and manipulation

(in-package #:sw4rm-sdk)

;;;; Three-ID Envelope Model (spec §11.3)
;;;
;;; The SW4RM protocol uses three distinct identifiers to track messages:
;;;
;;; 1. message_id (Required, UUIDv4):
;;;    - Unique per delivery attempt
;;;    - New UUID generated on each retry
;;;    - Used for acknowledgement targeting
;;;
;;; 2. correlation_id (Required, UUIDv4):
;;;    - Groups related operations in workflow/session
;;;    - Stable across entire flow
;;;    - Set to workflow_id or negotiation_id for correlation
;;;
;;; 3. idempotency_token (Optional):
;;;    - Enables exactly-once semantics
;;;    - Stable across retries of same logical operation
;;;    - Format: {producer_id}:{operation_type}:{deterministic_hash}

;;;; UUID Generation

(defun generate-uuid ()
  "Generate a new UUIDv4 string.

Returns:
  String representation of UUID (e.g., '550e8400-e29b-41d4-a716-446655440000')"
  (string-downcase (princ-to-string (uuid:make-v4-uuid))))

;;;; Hybrid Logical Clock (HLC) Stub
;;;
;;; Implementations MAY enable HLC timestamping per spec §4.
;;; This is a placeholder using Unix milliseconds.

(defun generate-hlc-timestamp ()
  "Generate HLC timestamp in canonical format HLC:<wall>:<logical>:<node>.

Per spec §4, HLC format combines physical time (Unix microseconds),
logical counter (always 0 in this stub), and node identifier (hostname).
Full HLC implementations should increment the logical counter on
concurrent events within the same microsecond.

Returns:
  String in format HLC:<unix_microseconds>:0:<hostname>."
  (format nil "HLC:~D:0:~A"
          (truncate (* (get-universal-time) 1000000))
          (machine-instance)))

;;;; Deterministic Hash Computation
;;;
;;; For idempotency token generation per spec §11.2.

(defun %envelope-key->string (key)
  (cond
    ((stringp key) (string-downcase key))
    ((keywordp key) (string-downcase (substitute #\_ #\- (string key))))
    ((symbolp key) (string-downcase (substitute #\_ #\- (string key))))
    (t (string-downcase (princ-to-string key)))))

(defun %plist-or-hash->plist (params-plist)
  (cond
    ((hash-table-p params-plist)
     (let ((result nil))
       (maphash (lambda (key value)
                  (push value result)
                  (push key result))
                params-plist)
       (nreverse result)))
    (t (copy-list params-plist))))

(defun compute-deterministic-hash (params-plist)
  "Compute deterministic SHA256 hash from canonical operation parameters.

Args:
  PARAMS-PLIST: Property list of parameters that uniquely identify the operation.
                Should include all fields that distinguish this operation from others.

Returns:
  Hexadecimal string (first 16 characters of SHA256 hash).

Example:
  (compute-deterministic-hash '(:tool \"git-commit\" :repo \"myrepo\" :files (\"a.lisp\")))
  => \"a3b2c1d4e5f6a7b8\""
  ;; Canonicalize: sort keys and create stable representation
  (let* ((normalized-params (%plist-or-hash->plist params-plist))
         (sorted-params (sort (copy-list normalized-params)
                              #'string<
                              :key #'%envelope-key->string))
         (canonical-string (format nil "~{~A~^:~}" sorted-params))
         (digest (ironclad:digest-sequence
                  :sha256
                  (ironclad:ascii-string-to-byte-array canonical-string))))
    (subseq (ironclad:byte-array-to-hex-string digest) 0 16)))

(defun make-idempotency-token (producer-id operation-type &optional deterministic-hash)
  "Create idempotency token in format: {producer_id}:{operation_type}:{deterministic_hash}.

Per spec §11.2, idempotency tokens SHOULD follow this format for consistency.

Args:
  PRODUCER-ID: ID of the agent/service producing the message
  OPERATION-TYPE: Type of operation (e.g., 'tool-call', 'task-submit')
  DETERMINISTIC-HASH: Hash computed from canonical operation parameters

Returns:
  Formatted idempotency token string.

Example:
  (make-idempotency-token \"agent-1\" \"git-commit\" \"abc123\")
  => \"agent-1:git-commit:abc123\""
  (let ((token-hash (or deterministic-hash
                        (compute-deterministic-hash (list :producer-id producer-id
                                                          :operation-type operation-type)))))
    (format nil "~A:~A:~A" producer-id operation-type token-hash)))

;;;; Sequence Tracking
;;;
;;; Thread-safe sequence number generator for message ordering.

(defclass sequence-tracker ()
  ((current-sequence
    :initform 0
    :accessor sequence-tracker-current
    :documentation "Current sequence number.")
   (lock
    :initform (bordeaux-threads:make-lock "sequence-tracker-lock")
    :reader sequence-tracker-lock
    :documentation "Lock for thread-safe increment."))
  (:documentation "Thread-safe sequence number generator.

Maintains monotonic sequence numbers for message ordering.
Thread-safe for use across multiple agent threads."))

(defun make-sequence-tracker (&key (start 1))
  "Create a new sequence tracker starting at START (default 1).

Args:
  START: Initial sequence number (default 1)

Returns:
  New SEQUENCE-TRACKER instance."
  (let ((tracker (make-instance 'sequence-tracker)))
    (setf (sequence-tracker-current tracker) (1- start))
    tracker))

(defun next-sequence (tracker)
  "Get next sequence number from TRACKER in thread-safe manner.

Args:
  TRACKER: SEQUENCE-TRACKER instance

Returns:
  Next sequence number (integer)."
  (bordeaux-threads:with-lock-held ((sequence-tracker-lock tracker))
    (incf (sequence-tracker-current tracker))))

;;;; Envelope Construction
;;;
;;; Build message envelopes with Three-ID model support.

(defun make-envelope (&key
                      producer-id
                      source-agent-id
                      target-agent-id
                      message-type
                      (content-type "application/json")
                      (payload #())
                      correlation-id
                      idempotency-token
                      sequence-number
                      (retry-count 0)
                      repo-id
                      worktree-id
                      ttl-ms
                      (state +sent+)
                      effective-policy-id
                      audit-proof
                      audit-policy-id)
  "Build a message envelope with Three-ID model support (spec §11.3)."
  (let ((effective-producer-id (or producer-id source-agent-id)))
    (unless effective-producer-id
      (error 'validation-error
             :message "source/producer identifier is required"
             :field "source-agent-id"
             :constraint "must not be nil"))
    (unless message-type
      (error 'validation-error
             :message "message-type is required"
             :field "message-type"
             :constraint "must not be nil"))
    (let ((envelope (make-hash-table :test 'equal)))
      (flet ((setf-envelope (key value)
               (setf (gethash (%envelope-key->string key) envelope) value)))
        (setf-envelope :message-id (generate-uuid))
        (setf-envelope :idempotency-token (or idempotency-token ""))
        (setf-envelope :producer-id effective-producer-id)
        (setf-envelope :source-agent-id (or source-agent-id ""))
        (setf-envelope :target-agent-id (or target-agent-id ""))
        ;; Keep snake-case aliases for historical compatibility.
        (setf-envelope :source_agent_id (or source-agent-id ""))
        (setf-envelope :target_agent_id (or target-agent-id ""))
        (setf-envelope :correlation-id (or correlation-id (generate-uuid)))
        (setf-envelope :sequence-number (or sequence-number 1))
        (setf-envelope :retry-count retry-count)
        (setf-envelope :message-type message-type)
        (setf-envelope :content-type content-type)
        (setf-envelope :content-length (length payload))
        (setf-envelope :repo-id (or repo-id ""))
        (setf-envelope :worktree-id (or worktree-id ""))
        (setf-envelope :hlc-timestamp (generate-hlc-timestamp))
        (setf-envelope :ttl-ms (or ttl-ms 0))
        (setf-envelope :state state)
        (setf-envelope :effective-policy-id (or effective-policy-id ""))
        (setf-envelope :payload payload)
        (setf-envelope :audit-proof (or audit-proof #()))
        (setf-envelope :audit-policy-id (or audit-policy-id "")))
      envelope)))

;;;; Envelope State Management
;;;
;;; Functions for envelope lifecycle management per spec §11.

(defun update-envelope-state (envelope new-state)
  "Update the state of an envelope.

Args:
  ENVELOPE: Envelope property list
  NEW-STATE: New state value (from constants, e.g., +FULFILLED-ENVELOPE+)

Returns:
  Updated envelope (same object, modified in place).

Example:
  (let ((env (make-envelope :producer-id \"agent-1\" :message-type +DATA+)))
    (update-envelope-state env +received-envelope+)
    (getf env :state)) => 2"
  (if (hash-table-p envelope)
      (setf (gethash "state" envelope) new-state)
      (setf (getf envelope :state) new-state))
  envelope)

(defun %envelope-get (envelope key)
  "Read a field from either plist or hash-table envelopes."
  (if (hash-table-p envelope)
      (gethash (%envelope-key->string key) envelope)
      (getf envelope key)))

(defun terminal-state-p (state)
  "Check if an envelope state is terminal (no further transitions expected).

Per spec §11, terminal states are:
- FULFILLED (+fulfilled-envelope+)
- REJECTED (+rejected-envelope+)
- FAILED (+failed-envelope+)
- TIMED_OUT (+timed-out-envelope+)

Args:
  STATE: Envelope state value (integer from constants)

Returns:
  T if state is terminal, NIL otherwise.

Example:
  (terminal-state-p +fulfilled-envelope+) => T
  (terminal-state-p +received-envelope+) => NIL"
  (member state (list +fulfilled-envelope+
                      +rejected-envelope+
                      +failed-envelope+
                      +timed-out-envelope+)))

;;;; Envelope Validation
;;;
;;; Validate envelope structure and constraints.

(defun validate-envelope (envelope)
  "Validate envelope structure and required fields.

Args:
  ENVELOPE: Envelope property list

Signals:
  VALIDATION-ERROR if validation fails

Returns:
  T if valid, signals error otherwise."
  (macrolet ((check-field (field constraint &optional (error-msg nil))
               `(unless ,constraint
                  (error 'validation-error
                         :message (or ,error-msg
                                      (format nil "~A validation failed" ',field))
                          :field (string-downcase (symbol-name ',field))
                          :constraint (format nil "~A" ',constraint)))))
    (check-field message-id (%envelope-get envelope :message-id))
    (check-field producer-id (%envelope-get envelope :producer-id))
    (check-field correlation-id (%envelope-get envelope :correlation-id))
    (check-field message-type (%envelope-get envelope :message-type))
    (check-field sequence-number
                 (and (integerp (%envelope-get envelope :sequence-number))
                      (> (%envelope-get envelope :sequence-number) 0))
                 "sequence-number must be positive integer")
    (check-field retry-count
                 (and (integerp (%envelope-get envelope :retry-count))
                      (>= (%envelope-get envelope :retry-count) 0))
                 "retry-count must be non-negative integer"))
  t)

;;;; Message Log Ring Buffer
;;;
;;; Captures envelope traffic for local diagnostics and assertions.

(defstruct message-log-entry
  "One logged envelope snapshot."
  (message-id "" :type string)
  (correlation-id "" :type string)
  (source-agent-id "" :type string)
  (target-agent-id "" :type string)
  (message-type +message-type-unspecified+ :type integer)
  (state +sent+ :type integer)
  (timestamp (get-universal-time) :type integer)
  (envelope nil :type t))

(defclass message-log ()
  ((entries
    :initform nil
    :accessor message-log-entries
    :documentation "Newest-first list of MESSAGE-LOG-ENTRY values.")
   (capacity
    :initarg :capacity
    :initform 512
    :accessor message-log-capacity
    :type (integer 1 *))
   (lock
    :initform (bt:make-lock "sw4rm-message-log-lock")
    :accessor message-log-lock))
  (:documentation "In-memory ring buffer for envelope logging."))

(defun make-message-log (&key (capacity 512))
  "Construct a message-log with CAPACITY entries."
  (make-instance 'message-log :capacity capacity))

(defun message-log-size (log)
  "Return number of entries currently stored in LOG."
  (check-type log message-log)
  (bt:with-lock-held ((message-log-lock log))
    (length (message-log-entries log))))

(defun clear-message-log (log)
  "Remove all entries from LOG."
  (check-type log message-log)
  (bt:with-lock-held ((message-log-lock log))
    (setf (message-log-entries log) nil))
  t)

(defun %copy-envelope (envelope)
  (cond
    ((hash-table-p envelope)
     (alexandria:copy-hash-table envelope))
    ((listp envelope)
     (copy-list envelope))
    (t envelope)))

(defun log-envelope (log envelope)
  "Append ENVELOPE to LOG and return the resulting MESSAGE-LOG-ENTRY."
  (check-type log message-log)
  (let* ((entry (make-message-log-entry
                 :message-id (or (%envelope-get envelope :message-id) "")
                 :correlation-id (or (%envelope-get envelope :correlation-id) "")
                 :source-agent-id (or (%envelope-get envelope :source-agent-id)
                                      (%envelope-get envelope :producer-id)
                                      "")
                 :target-agent-id (or (%envelope-get envelope :target-agent-id) "")
                 :message-type (or (%envelope-get envelope :message-type)
                                   +message-type-unspecified+)
                 :state (or (%envelope-get envelope :state) +sent+)
                 :timestamp (get-universal-time)
                 :envelope (%copy-envelope envelope))))
    (bt:with-lock-held ((message-log-lock log))
      (push entry (message-log-entries log))
      (when (> (length (message-log-entries log)) (message-log-capacity log))
        (setf (message-log-entries log)
              (subseq (message-log-entries log) 0 (message-log-capacity log)))))
    entry))

(defun query-message-log (log &key message-id correlation-id source-agent-id target-agent-id
                              state message-type (limit nil))
  "Query LOG entries with optional filters, returning newest-first results."
  (check-type log message-log)
  (bt:with-lock-held ((message-log-lock log))
    (let ((results
            (remove-if-not
             (lambda (entry)
               (and (or (null message-id)
                        (string= message-id (message-log-entry-message-id entry)))
                    (or (null correlation-id)
                        (string= correlation-id (message-log-entry-correlation-id entry)))
                    (or (null source-agent-id)
                        (string= source-agent-id (message-log-entry-source-agent-id entry)))
                    (or (null target-agent-id)
                        (string= target-agent-id (message-log-entry-target-agent-id entry)))
                    (or (null state)
                        (eql state (message-log-entry-state entry)))
                    (or (null message-type)
                        (eql message-type (message-log-entry-message-type entry)))))
             (message-log-entries log))))
      (if limit
          (subseq results 0 (min limit (length results)))
          results))))
