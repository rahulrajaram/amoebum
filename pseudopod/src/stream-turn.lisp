(in-package :pseudopod)

(defstruct (stream-turn-snapshot
            (:constructor %make-stream-turn-snapshot)
            (:copier nil))
  (role "assistant" :type string)
  (content-fragments '() :type list)
  (reasoning-fragments '() :type list)
  (tool-call-partials (make-hash-table :test #'eql))
  usage
  (parse-error-count 0 :type fixnum)
  stream-id
  (status :streaming)
  error-message
  (saw-finalized-answer-p nil :type boolean)
  (saw-tool-signal-p nil :type boolean)
  (saw-retry-p nil :type boolean)
  (saw-explicit-error-p nil :type boolean))

(defun make-stream-turn-snapshot (&key (role "assistant")
                                       (status :streaming)
                                       stream-id
                                       usage)
  (%make-stream-turn-snapshot
   :role (if (%non-empty-string-p role) role "assistant")
   :status status
   :stream-id stream-id
   :usage usage))

(defun reset-stream-turn-snapshot! (snapshot &key (role "assistant"))
  (check-type snapshot stream-turn-snapshot)
  (setf (stream-turn-snapshot-role snapshot)
        (if (%non-empty-string-p role) role "assistant")
        (stream-turn-snapshot-content-fragments snapshot) '()
        (stream-turn-snapshot-reasoning-fragments snapshot) '()
        (stream-turn-snapshot-usage snapshot) nil
        (stream-turn-snapshot-parse-error-count snapshot) 0
        (stream-turn-snapshot-stream-id snapshot) nil
        (stream-turn-snapshot-status snapshot) :streaming
        (stream-turn-snapshot-error-message snapshot) nil
        (stream-turn-snapshot-saw-finalized-answer-p snapshot) nil
        (stream-turn-snapshot-saw-tool-signal-p snapshot) nil
        (stream-turn-snapshot-saw-retry-p snapshot) nil
        (stream-turn-snapshot-saw-explicit-error-p snapshot) nil)
  (clrhash (stream-turn-snapshot-tool-call-partials snapshot))
  snapshot)

(defun %stream-turn-blank-string-p (value)
  (or (null value)
      (not (stringp value))
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  value)))))

(defun normalize-stream-turn-event (event)
  "Coerce provider/UI stream EVENT shapes into the shared reducer vocabulary."
  (let ((kind (and (listp event)
                   (or (getf event :type)
                       (getf event :kind)))))
    (case kind
      ((:text-delta :chunk)
       (list :type :text-delta
             :text (getf event :text)))
      (:answer-finalized
       (list :type :answer-finalized))
      (:reasoning
       (list :type :reasoning-delta
             :text (getf event :text)))
      (:tool-call-delta
       (list :type :tool-call-delta
             :index (getf event :index)
             :tool-call (getf event :tool-call)
             :tool-call-id (getf event :tool-call-id)
             :name (or (getf event :name)
                       (getf event :tool-name))
             :arguments (getf event :arguments)
             :arguments-complete-p (getf event :arguments-complete-p)))
      ((:tool-call-started :tool-call-argument-complete :tool-call-result)
       (list :type kind
             :index (getf event :index)
             :tool-call (getf event :tool-call)
             :tool-call-id (getf event :tool-call-id)
             :name (or (getf event :name)
                       (getf event :tool-name))
             :arguments (getf event :arguments)
             :result (getf event :result)
             :error-message (or (getf event :error-message)
                                (getf event :execution-error))))
      (:complete
       (list :type :done
             :status :completed))
      (:cancelled
       (list :type :done
             :status :cancelled
             :error-message (or (getf event :error-message)
                                (getf event :message))))
      (:failed
       (list :type :done
             :status :failed
             :error-message (or (getf event :error-message)
                                (getf event :message))))
      (:stream-progress
       (list :type :done
             :status (getf event :status)
             :stream-id (getf event :stream-id)
             :usage (getf event :usage)
             :parse-error-count (getf event :parse-error-count)
             :error-message (or (getf event :error-message)
                                (getf event :message))))
      ((:retry :retry-requested)
       (list :type :retry-requested
             :text (getf event :text)))
      ((:assistant-final :assistant-message :assistant-delta)
       (list :type kind
             :text (getf event :text)
             :partialp (getf event :partialp)))
      (otherwise
       event))))

(defun %stream-turn-merge-string (current chunk)
  (cond
    ((%stream-turn-blank-string-p chunk) current)
    ((%stream-turn-blank-string-p current) chunk)
    (t (concatenate 'string current chunk))))

(defun %stream-turn-parse-index (value)
  (cond
    ((integerp value) value)
    ((and (stringp value)
          (plusp (length value))
          (every #'digit-char-p value))
     (parse-integer value))
    (t nil)))

(defun %copy-hash-table-shallow (table)
  (let ((copy (make-hash-table :test #'equal)))
    (when (hash-table-p table)
      (maphash (lambda (key value)
                 (setf (gethash key copy) value))
               table))
    copy))

(defun %token-count-integer (value)
  (cond
    ((integerp value) value)
    ((and (stringp value)
          (plusp (length value))
          (every #'digit-char-p value))
     (parse-integer value))
    (t nil)))

(defun %stream-turn-join-fragments (fragments)
  (if (null fragments)
      ""
      (with-output-to-string (stream)
        (dolist (fragment (nreverse (copy-list fragments)))
          (write-string fragment stream)))))

(defun %stream-turn-ensure-tool-call-partial (partials index)
  (or (gethash index partials)
      (let ((entry (make-hash-table :test #'equal))
            (function-body (make-hash-table :test #'equal))
            (extras (make-hash-table :test #'equal)))
        (setf (gethash "type" entry) "function"
              (gethash "function" entry) function-body
              (gethash "stream_index" extras) index
              (gethash "extras" entry) extras
              (gethash index partials) entry)
        entry)))

(defun %stream-turn-event->tool-call-hash (event)
  (cond
    ((tool-call-p (getf event :tool-call))
     (let* ((hash (tool-call-to-hash (getf event :tool-call)))
            (index (getf event :index)))
       (when (integerp index)
         (setf (gethash "index" hash) index))
       hash))
    ((or (getf event :name)
         (getf event :tool-call-id)
         (getf event :arguments))
     (let* ((hash (make-hash-table :test #'equal))
            (function-body (make-hash-table :test #'equal))
            (index (getf event :index))
            (tool-call-id (getf event :tool-call-id))
            (name (getf event :name))
            (arguments (getf event :arguments)))
       (when (integerp index)
         (setf (gethash "index" hash) index))
       (when (%non-empty-string-p tool-call-id)
         (setf (gethash "id" hash) tool-call-id))
       (setf (gethash "type" hash) "function"
             (gethash "function" hash) function-body)
       (when (%non-empty-string-p name)
         (setf (gethash "name" function-body) name))
       (when (%non-empty-string-p arguments)
         (setf (gethash "arguments" function-body) arguments))
       hash))
    (t nil)))

(defun %stream-tool-call-name (entry)
  (let ((function-body (and (hash-table-p entry)
                            (gethash "function" entry))))
    (and (hash-table-p function-body)
         (%non-empty-string-p (gethash "name" function-body))
         (gethash "name" function-body))))

(defun %stream-tool-call-arguments (entry)
  (let ((function-body (and (hash-table-p entry)
                            (gethash "function" entry))))
    (and (hash-table-p function-body)
         (%non-empty-string-p (gethash "arguments" function-body))
         (gethash "arguments" function-body))))

(defun %stream-tool-call-arguments-complete-p (arguments)
  (let ((trimmed (and (stringp arguments)
                      (string-trim '(#\Space #\Tab #\Newline #\Return) arguments))))
    (when (%non-empty-string-p trimmed)
      (handler-case
          (hash-table-p (jonathan:parse trimmed :as :hash-table))
        (error ()
          nil)))))

(defun %make-stream-text-delta-chunk (text)
  (list :type :text-delta
        :text (or text "")))

(defun %make-stream-tool-call-delta-chunk (index entry)
  (let* ((name (%stream-tool-call-name entry))
         (arguments (%stream-tool-call-arguments entry))
         (tool-call (hash-to-tool-call entry)))
    (list :type :tool-call-delta
          :index index
          :name name
          :arguments arguments
          :arguments-complete-p (%stream-tool-call-arguments-complete-p arguments)
          :tool-call tool-call)))

(defun %make-stream-usage-delta-chunk (usage)
  (let* ((usage-copy (%copy-hash-table-shallow usage))
         (prompt-tokens (or (%token-count-integer (gethash "prompt_tokens" usage-copy))
                            (%token-count-integer (gethash "input_tokens" usage-copy))))
         (completion-tokens (or (%token-count-integer (gethash "completion_tokens" usage-copy))
                                (%token-count-integer (gethash "output_tokens" usage-copy))))
         (total-tokens (or (%token-count-integer (gethash "total_tokens" usage-copy))
                           (and prompt-tokens completion-tokens
                                (+ prompt-tokens completion-tokens)))))
    (list :type :usage-delta
          :usage usage-copy
          :prompt-tokens prompt-tokens
          :completion-tokens completion-tokens
          :total-tokens total-tokens)))

(defun %stream-turn-merge-tool-call-delta! (snapshot event)
  (let* ((raw-tool-call (%stream-turn-event->tool-call-hash event))
         (partials (stream-turn-snapshot-tool-call-partials snapshot)))
    (when (hash-table-p raw-tool-call)
      (let* ((index (%stream-turn-parse-index (gethash "index" raw-tool-call)))
             (entry (and index
                         (%stream-turn-ensure-tool-call-partial partials index))))
        (when entry
          (let ((id (gethash "id" raw-tool-call))
                (type (gethash "type" raw-tool-call))
                (name (gethash "name" raw-tool-call))
                (arguments (gethash "arguments" raw-tool-call))
                (function-delta (and (hash-table-p (gethash "function" raw-tool-call))
                                     (gethash "function" raw-tool-call)))
                (function-body (or (and (hash-table-p (gethash "function" entry))
                                        (gethash "function" entry))
                                   (let ((fresh (make-hash-table :test #'equal)))
                                     (setf (gethash "function" entry) fresh)
                                     fresh))))
            (when (%non-empty-string-p id)
              (setf (gethash "id" entry)
                    (%stream-turn-merge-string (gethash "id" entry) id)))
            (when (%non-empty-string-p type)
              (setf (gethash "type" entry) type))
            (when (%non-empty-string-p name)
              (setf (gethash "name" function-body)
                    (%stream-turn-merge-string (gethash "name" function-body) name)))
            (when (%non-empty-string-p arguments)
              (setf (gethash "arguments" function-body)
                    (%stream-turn-merge-string (gethash "arguments" function-body)
                                               arguments)))
            (when function-delta
              (let ((delta-name (gethash "name" function-delta))
                    (delta-arguments (gethash "arguments" function-delta)))
                (when (%non-empty-string-p delta-name)
                  (setf (gethash "name" function-body)
                        (%stream-turn-merge-string (gethash "name" function-body)
                                                   delta-name)))
                (when (%non-empty-string-p delta-arguments)
                  (setf (gethash "arguments" function-body)
                        (%stream-turn-merge-string (gethash "arguments" function-body)
                                                   delta-arguments))))))))))
  snapshot)

(defun %stream-turn-finalize-tool-call-partials (partials)
  (let (indexes)
    (maphash (lambda (index entry)
               (when (and (integerp index) (hash-table-p entry))
                 (push index indexes)))
             partials)
    (loop for index in (sort indexes #'<)
          for raw-tool-call = (gethash index partials)
          when (hash-table-p raw-tool-call)
            collect (hash-to-tool-call raw-tool-call))))

(defun stream-turn-snapshot-content (snapshot)
  (check-type snapshot stream-turn-snapshot)
  (%stream-turn-join-fragments
   (stream-turn-snapshot-content-fragments snapshot)))

(defun stream-turn-snapshot-reasoning-content (snapshot)
  (check-type snapshot stream-turn-snapshot)
  (%stream-turn-join-fragments
   (stream-turn-snapshot-reasoning-fragments snapshot)))

(defun stream-turn-snapshot-tool-calls (snapshot)
  (check-type snapshot stream-turn-snapshot)
  (%stream-turn-finalize-tool-call-partials
   (stream-turn-snapshot-tool-call-partials snapshot)))

(defun stream-turn-snapshot-terminal-outcome (snapshot)
  (check-type snapshot stream-turn-snapshot)
  (let ((status (stream-turn-snapshot-status snapshot)))
    (cond
      ((or (stream-turn-snapshot-saw-explicit-error-p snapshot)
           (member status '(:failed :cancelled :error) :test #'eq))
       :explicit-error)
      ((stream-turn-snapshot-saw-retry-p snapshot)
       :retry)
      ((or (stream-turn-snapshot-saw-tool-signal-p snapshot)
           (not (null (stream-turn-snapshot-tool-calls snapshot))))
       :tool-continuation)
      ((and (eq status :completed)
            (stream-turn-snapshot-saw-finalized-answer-p snapshot))
       :answer)
      (t
       :silent-completion))))

(defun maybe-finalize-stream-turn-answer! (snapshot)
  (check-type snapshot stream-turn-snapshot)
  (when (and (not (stream-turn-snapshot-saw-finalized-answer-p snapshot))
             (not (stream-turn-snapshot-saw-tool-signal-p snapshot))
             (not (stream-turn-snapshot-saw-retry-p snapshot))
             (not (stream-turn-snapshot-saw-explicit-error-p snapshot))
             (%non-empty-string-p (stream-turn-snapshot-content snapshot)))
    (stream-turn-apply-event! snapshot '(:type :answer-finalized)))
  snapshot)

(defun finalize-stream-turn-snapshot! (snapshot &key (status :completed))
  "Finalize SNAPSHOT: seal answer if applicable, then apply :done with STATUS.
Returns the snapshot."
  (check-type snapshot stream-turn-snapshot)
  (maybe-finalize-stream-turn-answer! snapshot)
  (stream-turn-apply-event! snapshot (list :type :done :status status))
  snapshot)

(defun stream-turn-snapshot-values (snapshot &key include-reasoning-p)
  "Extract (values role content tool-calls usage [reasoning]) from SNAPSHOT.
Returns 4 values normally, 5 when INCLUDE-REASONING-P is true."
  (check-type snapshot stream-turn-snapshot)
  (let ((role (stream-turn-snapshot-role snapshot))
        (content (stream-turn-snapshot-content snapshot))
        (tool-calls (stream-turn-snapshot-tool-calls snapshot))
        (usage (stream-turn-snapshot-usage snapshot)))
    (if include-reasoning-p
        (values role content tool-calls usage
                (stream-turn-snapshot-reasoning-content snapshot))
        (values role content tool-calls usage))))

(defun stream-turn-snapshot-to-alist (snapshot)
  "Serialize SNAPSHOT to an alist for checkpoint/restore."
  (check-type snapshot stream-turn-snapshot)
  (list (cons :role (stream-turn-snapshot-role snapshot))
        (cons :content (stream-turn-snapshot-content snapshot))
        (cons :reasoning-content (stream-turn-snapshot-reasoning-content snapshot))
        (cons :status (stream-turn-snapshot-status snapshot))
        (cons :stream-id (stream-turn-snapshot-stream-id snapshot))
        (cons :error-message (stream-turn-snapshot-error-message snapshot))
        (cons :parse-error-count (stream-turn-snapshot-parse-error-count snapshot))
        (cons :saw-finalized-answer-p (stream-turn-snapshot-saw-finalized-answer-p snapshot))
        (cons :saw-tool-signal-p (stream-turn-snapshot-saw-tool-signal-p snapshot))
        (cons :saw-retry-p (stream-turn-snapshot-saw-retry-p snapshot))
        (cons :saw-explicit-error-p (stream-turn-snapshot-saw-explicit-error-p snapshot))))

(defun stream-turn-snapshot-from-alist (alist)
  "Restore a snapshot from ALIST. Returns a fresh snapshot."
  (let ((snapshot (make-stream-turn-snapshot
                   :role (or (cdr (assoc :role alist)) "assistant")
                   :status (or (cdr (assoc :status alist)) :streaming)
                   :stream-id (cdr (assoc :stream-id alist)))))
    (let ((content (cdr (assoc :content alist))))
      (when (%non-empty-string-p content)
        (push content (stream-turn-snapshot-content-fragments snapshot))))
    (let ((reasoning (cdr (assoc :reasoning-content alist))))
      (when (%non-empty-string-p reasoning)
        (push reasoning (stream-turn-snapshot-reasoning-fragments snapshot))))
    (setf (stream-turn-snapshot-error-message snapshot)
          (cdr (assoc :error-message alist))
          (stream-turn-snapshot-parse-error-count snapshot)
          (or (cdr (assoc :parse-error-count alist)) 0)
          (stream-turn-snapshot-saw-finalized-answer-p snapshot)
          (not (null (cdr (assoc :saw-finalized-answer-p alist))))
          (stream-turn-snapshot-saw-tool-signal-p snapshot)
          (not (null (cdr (assoc :saw-tool-signal-p alist))))
          (stream-turn-snapshot-saw-retry-p snapshot)
          (not (null (cdr (assoc :saw-retry-p alist))))
          (stream-turn-snapshot-saw-explicit-error-p snapshot)
          (not (null (cdr (assoc :saw-explicit-error-p alist)))))
    snapshot))

(defstruct (stream-turn-snapshot-delta
            (:constructor make-stream-turn-snapshot-delta
                (&key content-appended reasoning-appended
                      tool-call-updated status-changed
                      usage-changed)))
  (content-appended nil :type (or null string))
  (reasoning-appended nil :type (or null string))
  (tool-call-updated nil)
  (status-changed nil)
  (usage-changed nil))

(defun compute-stream-turn-snapshot-delta (before-content before-reasoning before-status
                                           after-snapshot)
  "Compute incremental delta between before-state and current AFTER-SNAPSHOT."
  (check-type after-snapshot stream-turn-snapshot)
  (let* ((after-content (stream-turn-snapshot-content after-snapshot))
         (after-reasoning (stream-turn-snapshot-reasoning-content after-snapshot))
         (after-status (stream-turn-snapshot-status after-snapshot))
         (content-appended
           (when (and (stringp after-content)
                      (> (length after-content) (length (or before-content ""))))
             (subseq after-content (length (or before-content "")))))
         (reasoning-appended
           (when (and (stringp after-reasoning)
                      (> (length after-reasoning) (length (or before-reasoning ""))))
             (subseq after-reasoning (length (or before-reasoning ""))))))
    (make-stream-turn-snapshot-delta
     :content-appended content-appended
     :reasoning-appended reasoning-appended
     :status-changed (unless (eq before-status after-status) after-status))))

(defun stream-turn-apply-event! (snapshot event)
  (check-type snapshot stream-turn-snapshot)
  (let* ((event (normalize-stream-turn-event event))
         (type (or (getf event :type)
                   (getf event :kind)))
         (text (getf event :text))
         (status (getf event :status))
         (error-message (or (getf event :error-message)
                            (getf event :message))))
    (case type
      (:role
       (let ((role (getf event :role)))
         (when (%non-empty-string-p role)
           (setf (stream-turn-snapshot-role snapshot) role))))
      ((:reasoning :reasoning-delta)
       (when (%non-empty-string-p text)
         (push text (stream-turn-snapshot-reasoning-fragments snapshot))))
      ((:text-delta :assistant-delta :chunk)
       (when (%non-empty-string-p text)
         (push text (stream-turn-snapshot-content-fragments snapshot))))
      ((:assistant-final :assistant-message)
       (when (%non-empty-string-p text)
         (push text (stream-turn-snapshot-content-fragments snapshot)))
       (unless (getf event :partialp)
         (setf (stream-turn-snapshot-saw-finalized-answer-p snapshot) t)))
      (:answer-finalized
       (setf (stream-turn-snapshot-saw-finalized-answer-p snapshot) t))
      (:tool-call-delta
       (setf (stream-turn-snapshot-saw-tool-signal-p snapshot) t)
       (%stream-turn-merge-tool-call-delta! snapshot event))
      ((:tool-call-started :tool-call-argument-complete :tool-call-result
        :tool-started :tool-completed :tool-error :tool)
       (setf (stream-turn-snapshot-saw-tool-signal-p snapshot) t)
       (%stream-turn-merge-tool-call-delta! snapshot event))
      (:usage-delta
       (when (hash-table-p (getf event :usage))
         (setf (stream-turn-snapshot-usage snapshot) (getf event :usage))))
      (:parse-error
       (incf (stream-turn-snapshot-parse-error-count snapshot)))
      ((:retry :retry-requested)
       (setf (stream-turn-snapshot-saw-retry-p snapshot) t))
      ((:failed :error :explicit-error :cancelled)
       (setf (stream-turn-snapshot-saw-explicit-error-p snapshot) t
             (stream-turn-snapshot-status snapshot)
             (case type
               (:cancelled :cancelled)
               (otherwise :failed)))
       (when (%non-empty-string-p error-message)
         (setf (stream-turn-snapshot-error-message snapshot) error-message)))
      ((:done :complete :stream-progress)
       (when (or status (eq type :complete))
         (setf (stream-turn-snapshot-status snapshot)
               (or status :completed)))
       (when (getf event :stream-id)
         (setf (stream-turn-snapshot-stream-id snapshot)
               (getf event :stream-id)))
       (when (integerp (getf event :parse-error-count))
         (setf (stream-turn-snapshot-parse-error-count snapshot)
               (getf event :parse-error-count)))
       (when (hash-table-p (getf event :usage))
         (setf (stream-turn-snapshot-usage snapshot)
               (getf event :usage)))
       (when (%non-empty-string-p error-message)
         (setf (stream-turn-snapshot-error-message snapshot) error-message))
       (when (member (stream-turn-snapshot-status snapshot)
                     '(:failed :cancelled :error)
                     :test #'eq)
         (setf (stream-turn-snapshot-saw-explicit-error-p snapshot) t)))
      (otherwise nil)))
  snapshot)
