;;;; activity-buffer.lisp
;;;; Activity buffer implementation per SW4RM spec §10

(in-package :sw4rm-sdk)

;;; buffer-full-error condition defined in errors.lisp

;;; Activity Entry Structure

(defstruct activity-entry
  "A single activity entry in the buffer.
   Fields conform to spec §10 requirements."
  (agent-id nil :type (or null string))
  (activity-id nil :type (or null string))
  (parent-activity-id nil :type (or null string))
  (parent-agent-id nil :type (or null string))
  (correlation-id nil :type (or null string))
  (sequence-id 0 :type integer)
  (activity-type :task :type t)
  (task-id nil :type (or null string))
  (repo-id nil :type (or null string))
  (worktree-id nil :type (or null string))
  (branch nil :type (or null string))
  (timestamp (get-universal-time) :type integer)
  (description nil :type (or null string)))

(defun %normalize-optional-id (value field-name)
  "Normalize optional VALUE into a trimmed non-empty string or NIL."
  (when value
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                (if (stringp value)
                                    value
                                    (princ-to-string value)))))
      (when (zerop (length trimmed))
        (error "~A must be non-empty when provided." field-name))
      trimmed)))

(defun validate-activity-entry (entry)
  "Validate an activity entry conforms to spec §10 requirements.

   Args:
     entry: Activity entry to validate

   Returns:
     T if valid

   Signals:
     ERROR: If validation fails"
  (check-type entry activity-entry)

  ;; Description must be <= 200 words per spec §10
  (when (activity-entry-description entry)
    (let* ((desc (activity-entry-description entry))
           (word-count (length (remove-if #'(lambda (s) (string= s ""))
                                         (split-sequence:split-sequence #\Space desc)))))
      (when (> word-count 200)
        (error "Activity description exceeds 200 word limit (got ~D words)" word-count))))

  (when (and (activity-entry-agent-id entry)
             (string= (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   (activity-entry-agent-id entry))
                      ""))
    (error "agent-id must be non-empty when provided"))

  (%normalize-optional-id (activity-entry-activity-id entry) "activity-id")
  (%normalize-optional-id (activity-entry-parent-activity-id entry) "parent-activity-id")
  (%normalize-optional-id (activity-entry-parent-agent-id entry) "parent-agent-id")
  (%normalize-optional-id (activity-entry-correlation-id entry) "correlation-id")

  t)

;;; Activity Buffer Class

(defclass activity-buffer ()
  ((entries
    :initarg :entries
    :accessor entries
    :type hash-table
    :initform (make-hash-table :test 'equal)
    :documentation "Hash table mapping activity keys to activity-entry structs.")

  (max-items
    :initarg :max-items
    :initarg :capacity
    :accessor max-items
    :type (integer 1 *)
    :initform 1000
   :documentation "Maximum number of activities to store.")

   (update-hook
    :initarg :update-hook
    :accessor update-hook
    :initform nil
    :documentation "Optional callback for activity upsert/remove events.")

   (timeline-events
    :initarg :timeline-events
    :accessor timeline-events
    :type list
    :initform '()
    :documentation "Append-only activity event history (newest first).")

   (sequence-counter
    :initarg :sequence-counter
    :accessor sequence-counter
    :type integer
    :initform 0
    :documentation "Monotonic sequence counter for deterministic timeline ordering.")

   (lock
    :accessor buffer-lock
    :initform (bt:make-lock "activity-buffer-lock")
    :documentation "Lock for thread-safe operations."))
  (:documentation "Activity buffer for tracking agent activities.
                   Implements spec §10 requirements including:
                   - Task/repo/worktree/branch tracking
                   - Timestamp recording
                   - Description (max 200 words)
                   - Capacity enforcement (reject when full)
                   - Reconciliation with task states"))

(defmethod initialize-instance :after ((buf activity-buffer) &key)
  "Validate buffer configuration after initialization."
  (with-slots (max-items sequence-counter) buf
    (when (< max-items 1)
      (error "max-items must be at least 1, got ~D" max-items))
    (when (< sequence-counter 0)
      (error "sequence-counter must be >= 0, got ~D" sequence-counter))))

(defun make-activity-key (task-id repo-id worktree-id)
  "Generate a unique key for an activity entry.

   Args:
     task-id: Task identifier
     repo-id: Repository identifier
     worktree-id: Worktree identifier

   Returns:
     String key"
  (format nil "~A/~A/~A"
          (or task-id "")
          (or repo-id "")
          (or worktree-id "")))

(defmethod current-size ((buf activity-buffer))
  "Get the current number of entries in the buffer.

   Args:
     buf: Activity buffer instance

   Returns:
     Number of entries"
  (hash-table-count (entries buf)))

(defmethod is-full-p ((buf activity-buffer))
  "Check if the buffer is at capacity.

   Args:
     buf: Activity buffer instance

   Returns:
     T if buffer is full, NIL otherwise"
  (>= (current-size buf) (max-items buf)))

(defmethod upsert-activity ((buf activity-buffer)
                           &key agent-id (activity-type :task)
                             activity-id parent-activity-id parent-agent-id correlation-id
                             task-id repo-id worktree-id branch timestamp description)
  "Insert or update an activity entry in the buffer.

   Per spec §10.1, signals BUFFER-FULL-ERROR if buffer is at capacity
   and this is a new entry (not an update).

   Args:
     buf: Activity buffer instance
     activity-id: Stable event identifier (auto-generated when omitted)
     parent-activity-id: Parent event identifier for timeline reconstruction
     parent-agent-id: Parent agent identifier for nested delegation attribution
     correlation-id: Correlation identifier for grouped reconstruction
     task-id: Task identifier
     repo-id: Repository identifier
     worktree-id: Worktree identifier
     branch: Git branch name
     timestamp: Optional integer timestamp override
     description: Activity description (max 200 words)

   Returns:
     The activity entry

   Signals:
     BUFFER-FULL-ERROR: If buffer is full and entry is new
     ERROR: If description exceeds 200 words"
  (bt:with-lock-held ((buffer-lock buf))
    (let* ((key (make-activity-key task-id repo-id worktree-id))
           (existing (gethash key (entries buf)))
           (sequence-id (incf (sequence-counter buf)))
           (normalized-activity-id (%normalize-optional-id activity-id "activity-id"))
           (normalized-parent-activity-id (%normalize-optional-id parent-activity-id "parent-activity-id"))
           (normalized-parent-agent-id (%normalize-optional-id parent-agent-id "parent-agent-id"))
           (normalized-correlation-id (%normalize-optional-id correlation-id "correlation-id"))
           (resolved-activity-id (or normalized-activity-id
                                     (format nil "activity-~8,'0D" sequence-id)))
           (resolved-correlation-id (or normalized-correlation-id
                                        (and existing (activity-entry-correlation-id existing))
                                        resolved-activity-id))
           (entry (make-activity-entry
                   :agent-id agent-id
                   :activity-id resolved-activity-id
                   :parent-activity-id (or normalized-parent-activity-id
                                           (and existing
                                                (activity-entry-parent-activity-id existing)))
                   :parent-agent-id (or normalized-parent-agent-id
                                        (and existing
                                             (activity-entry-parent-agent-id existing)))
                   :correlation-id resolved-correlation-id
                   :sequence-id sequence-id
                   :activity-type activity-type
                   :task-id task-id
                   :repo-id repo-id
                   :worktree-id worktree-id
                   :branch branch
                   :timestamp (or timestamp (get-universal-time))
                   :description description)))

      ;; Validate entry
      (validate-activity-entry entry)

      ;; Check capacity if new entry
      (when (and (not existing) (is-full-p buf))
        (error 'buffer-full-error
               :message (format nil "Activity buffer at capacity (~D/~D)"
                               (current-size buf) (max-items buf))
               :current-size (current-size buf)
               :max-size (max-items buf)))

      ;; Store entry
      (setf (gethash key (entries buf)) entry)
      (push entry (timeline-events buf))
      (when (update-hook buf)
        (funcall (update-hook buf) :upsert entry))
      entry)))

(defun buffer-count (buf)
  "Compatibility alias for CURRENT-SIZE."
  (current-size buf))

(defun add-entry (buf entry)
  "Compatibility alias that accepts an activity-entry and forwards to upsert."
  (check-type entry activity-entry)
  (upsert-activity buf
                   :agent-id (activity-entry-agent-id entry)
                   :activity-id (activity-entry-activity-id entry)
                   :parent-activity-id (activity-entry-parent-activity-id entry)
                   :parent-agent-id (activity-entry-parent-agent-id entry)
                   :correlation-id (activity-entry-correlation-id entry)
                   :activity-type (activity-entry-activity-type entry)
                   :task-id (activity-entry-task-id entry)
                   :repo-id (activity-entry-repo-id entry)
                   :worktree-id (activity-entry-worktree-id entry)
                   :branch (activity-entry-branch entry)
                   :timestamp (activity-entry-timestamp entry)
                   :description (activity-entry-description entry)))

(defmethod remove-activity ((buf activity-buffer)
                           &key task-id repo-id worktree-id)
  "Remove an activity entry from the buffer.

   Args:
     buf: Activity buffer instance
     task-id: Task identifier
     repo-id: Repository identifier
     worktree-id: Worktree identifier

   Returns:
     T if entry was found and removed, NIL otherwise"
  (bt:with-lock-held ((buffer-lock buf))
    (let ((key (make-activity-key task-id repo-id worktree-id)))
      (let ((existing (gethash key (entries buf))))
        (prog1 (remhash key (entries buf))
          (when (and existing (update-hook buf))
            (funcall (update-hook buf) :remove existing)))))))

(defmethod get-activity ((buf activity-buffer)
                        &key task-id repo-id worktree-id)
  "Get an activity entry from the buffer.

   Args:
     buf: Activity buffer instance
     task-id: Task identifier
     repo-id: Repository identifier
     worktree-id: Worktree identifier

   Returns:
     Activity entry or NIL if not found"
  (let ((key (make-activity-key task-id repo-id worktree-id)))
    (gethash key (entries buf))))

(defmethod list-buffer-activities ((buf activity-buffer)
                                   &key task-id repo-id agent-id activity-type
                                     correlation-id parent-agent-id)
  "List all activities matching the given filters.

   Args:
     buf: Activity buffer instance
     task-id: Optional task ID filter
     repo-id: Optional repository ID filter

   Returns:
     List of activity entries"
  (let ((results nil))
    (maphash (lambda (k v)
               (declare (ignore k))
               (when (and (or (null task-id)
                            (equal task-id (activity-entry-task-id v)))
                         (or (null repo-id)
                            (equal repo-id (activity-entry-repo-id v)))
                         (or (null agent-id)
                             (equal agent-id (activity-entry-agent-id v)))
                         (or (null activity-type)
                             (equal activity-type (activity-entry-activity-type v)))
                         (or (null correlation-id)
                             (equal correlation-id (activity-entry-correlation-id v)))
                         (or (null parent-agent-id)
                             (equal parent-agent-id (activity-entry-parent-agent-id v))))
                 (push v results)))
             (entries buf))
    results))

(defun %activity-order< (left right)
  "Deterministic ordering for timeline reconstruction."
  (let ((left-seq (activity-entry-sequence-id left))
        (right-seq (activity-entry-sequence-id right)))
    (cond
      ((/= left-seq right-seq)
       (< left-seq right-seq))
      ((/= (activity-entry-timestamp left) (activity-entry-timestamp right))
       (< (activity-entry-timestamp left) (activity-entry-timestamp right)))
      (t
       (string< (or (activity-entry-activity-id left) "")
                (or (activity-entry-activity-id right) ""))))))

(defmethod list-activity-events ((buf activity-buffer)
                                 &key correlation-id agent-id parent-agent-id)
  "Return buffered activity events in deterministic chronological order."
  (let* ((history (bt:with-lock-held ((buffer-lock buf))
                    (copy-list (timeline-events buf))))
         (ordered (sort (nreverse history) #'%activity-order<)))
    (remove-if-not
     (lambda (entry)
       (and (or (null correlation-id)
                (equal correlation-id (activity-entry-correlation-id entry)))
            (or (null agent-id)
                (equal agent-id (activity-entry-agent-id entry)))
            (or (null parent-agent-id)
                (equal parent-agent-id (activity-entry-parent-agent-id entry)))))
     ordered)))

(defun %collect-descendant-activity-ids (events root-activity-id)
  "Collect ROOT-ACTIVITY-ID and all descendants linked by parent-activity-id."
  (let ((children (make-hash-table :test #'equal))
        (allowed (make-hash-table :test #'equal))
        (queue '()))
    (dolist (entry events)
      (let ((parent-id (activity-entry-parent-activity-id entry))
            (activity-id (activity-entry-activity-id entry)))
        (when (and parent-id activity-id)
          (setf (gethash parent-id children)
                (cons activity-id (gethash parent-id children))))))
    (push root-activity-id queue)
    (setf (gethash root-activity-id allowed) t)
    (loop while queue do
      (let ((current (pop queue)))
        (dolist (child-id (gethash current children))
          (unless (gethash child-id allowed)
            (setf (gethash child-id allowed) t)
            (push child-id queue)))))
    allowed))

(defun %collect-descendant-agent-ids (events root-agent-id)
  "Collect ROOT-AGENT-ID plus all descendants linked by parent-agent-id."
  (let ((allowed (make-hash-table :test #'equal)))
    (setf (gethash root-agent-id allowed) t)
    (dolist (entry events)
      (let ((parent-agent-id (activity-entry-parent-agent-id entry))
            (agent-id (activity-entry-agent-id entry)))
        (when (and parent-agent-id
                   agent-id
                   (gethash parent-agent-id allowed))
          (setf (gethash agent-id allowed) t))))
    allowed))

(defun %entry->timeline-row (entry depth)
  "Convert activity ENTRY to a serializable timeline row."
  (list :activity-id (activity-entry-activity-id entry)
        :parent-activity-id (activity-entry-parent-activity-id entry)
        :correlation-id (activity-entry-correlation-id entry)
        :sequence-id (activity-entry-sequence-id entry)
        :depth depth
        :agent-id (activity-entry-agent-id entry)
        :parent-agent-id (activity-entry-parent-agent-id entry)
        :activity-type (activity-entry-activity-type entry)
        :task-id (activity-entry-task-id entry)
        :repo-id (activity-entry-repo-id entry)
        :worktree-id (activity-entry-worktree-id entry)
        :branch (activity-entry-branch entry)
        :timestamp (activity-entry-timestamp entry)
        :description (activity-entry-description entry)))

(defmethod reconstruct-activity-timeline ((buf activity-buffer)
                                          &key correlation-id
                                            root-activity-id
                                            root-agent-id)
  "Reconstruct a merged parent/child activity timeline with deterministic order.

Returns list of plists containing ordering metadata and attribution."
  (let* ((events (list-activity-events buf :correlation-id correlation-id))
         (scoped-by-root-activity
           (if root-activity-id
               (let ((allowed (%collect-descendant-activity-ids events root-activity-id)))
                 (remove-if-not (lambda (entry)
                                  (and (activity-entry-activity-id entry)
                                       (gethash (activity-entry-activity-id entry) allowed)))
                                events))
               events))
         (scoped-events
           (if root-agent-id
               (let ((allowed (%collect-descendant-agent-ids scoped-by-root-activity root-agent-id)))
                 (remove-if-not (lambda (entry)
                                  (let ((agent-id (activity-entry-agent-id entry)))
                                    (and agent-id (gethash agent-id allowed))))
                                scoped-by-root-activity))
               scoped-by-root-activity))
         (activity-depths (make-hash-table :test #'equal))
         (agent-depths (make-hash-table :test #'equal))
         (rows '()))
    (dolist (entry scoped-events (nreverse rows))
      (let* ((parent-activity-id (activity-entry-parent-activity-id entry))
             (parent-agent-id (activity-entry-parent-agent-id entry))
             (depth (cond
                      ((and parent-activity-id
                            (gethash parent-activity-id activity-depths))
                       (1+ (gethash parent-activity-id activity-depths)))
                      ((and parent-agent-id
                            (gethash parent-agent-id agent-depths))
                       (1+ (gethash parent-agent-id agent-depths)))
                      (t 0)))
             (activity-id (activity-entry-activity-id entry))
             (agent-id (activity-entry-agent-id entry)))
        (when activity-id
          (setf (gethash activity-id activity-depths) depth))
        (when agent-id
          (setf (gethash agent-id agent-depths)
                (max depth (or (gethash agent-id agent-depths) 0))))
        (push (%entry->timeline-row entry depth) rows)))))

(defmethod recent-activities ((buf activity-buffer) &key (limit 10))
  "Get the most recent activities.

   Args:
     buf: Activity buffer instance
     limit: Maximum number of entries to return (default 10)

   Returns:
     List of activity entries, sorted newest first"
  (let ((all-entries nil))
    (maphash (lambda (k v)
               (declare (ignore k))
               (push v all-entries))
             (entries buf))
    (subseq (sort all-entries #'> :key #'activity-entry-timestamp)
            0
            (min limit (length all-entries)))))

(defmethod reconcile ((buf activity-buffer) task-states)
  "Purge entries for completed, failed, or unknown tasks.

   Per spec §10, the buffer should be reconciled against actual task states
   to remove stale entries.

   Args:
     buf: Activity buffer instance
     task-states: Hash table mapping task-id to state keyword
                 (:completed, :failed, etc.)

   Returns:
     Number of entries removed"
  (bt:with-lock-held ((buffer-lock buf))
    (let ((removed-count 0))
      (maphash (lambda (key entry)
                 (let* ((task-id (activity-entry-task-id entry))
                        (state (gethash task-id task-states)))
                   ;; Remove if task is completed, failed, or unknown
                   (when (or (null state)
                           (member state '(:completed :failed)))
                     (remhash key (entries buf))
                     (incf removed-count))))
               (entries buf))
      removed-count)))

(defmethod clear-all ((buf activity-buffer))
  "Remove all entries from the buffer.

   Args:
     buf: Activity buffer instance

   Returns:
     Number of entries removed"
  (bt:with-lock-held ((buffer-lock buf))
    (let ((count (hash-table-count (entries buf))))
      (clrhash (entries buf))
      (setf (timeline-events buf) '()
            (sequence-counter buf) 0)
      count)))

(defmethod print-object ((buf activity-buffer) stream)
  "Print activity buffer in readable format."
  (print-unreadable-object (buf stream :type t)
    (format stream "~D/~D entries"
            (current-size buf)
            (max-items buf))))
