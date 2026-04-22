(in-package :amoebum)

;;;; Memory transfer between the file-store and Haake.
;;;;
;;;; Decomposed from memory.lisp by NXT-388. Owns the
;;;; `memory-import-to-haake` and `memory-export-from-haake` flows that
;;;; bridge the file backend's on-disk MEMORY.md sources and the
;;;; Haake CLI memory store. Tracks already-imported entries via the
;;;; `.amoebum/memory/haake-import-state-v1.sexp` sidecar so re-runs
;;;; are idempotent, and writes failure rows to
;;;; `.amoebum/memory/haake-import-failures.log` for postmortem.

(defun %utc-timestamp-string (&optional (timestamp (get-universal-time)))
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time timestamp 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour minute second)))

(defun %fnv1a-64-hash (text)
  (let ((hash #xcbf29ce484222325)
        (prime #x100000001B3)
        (modulus #x10000000000000000))
    (loop for char across (or text "") do
      (setf hash (logxor hash (char-code char)))
      (setf hash (mod (* hash prime) modulus)))
    hash))

(defun %memory-entry-source-hash (source-path entry)
  (let ((payload (format nil "~A|~A|~A|~A"
                         source-path
                         (%entry-scope-signature (memory-entry-scope entry))
                         (or (memory-entry-key entry) "")
                         (or (memory-entry-value entry) ""))))
    (format nil "~16,'0X" (%fnv1a-64-hash payload))))

(defun %memory-import-state-path (&optional backend)
  (merge-pathnames #P".amoebum/memory/haake-import-state-v1.sexp"
                   (%memory-project-root backend)))

(defun %memory-import-failure-log-path (&optional backend)
  (merge-pathnames #P".amoebum/memory/haake-import-failures.log"
                   (%memory-project-root backend)))

(defun %default-memory-import-state ()
  (list :version 1 :updated-at nil :imports '()))

(defun %load-memory-import-state (&optional backend)
  (let ((path (%memory-import-state-path backend)))
    (if (probe-file path)
        (handler-case
            (with-open-file (stream path :direction :input)
              (let ((state (read stream nil nil)))
                (if (and (listp state)
                         (integerp (getf state :version))
                         (listp (getf state :imports)))
                    state
                    (%default-memory-import-state))))
          (error ()
            (%default-memory-import-state)))
        (%default-memory-import-state))))

(defun %write-memory-import-state (state &optional backend)
  (let ((path (%memory-import-state-path backend)))
    (ensure-directories-exist path)
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (with-standard-io-syntax
        (write state :stream stream :pretty t)))
    path))

(defun %append-memory-import-failure (backend source-path source-hash condition)
  (let ((path (%memory-import-failure-log-path backend)))
    (ensure-directories-exist path)
    (with-open-file (stream path
                            :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)
      (format stream "~A | ~A | ~A | ~A~%"
              (%utc-timestamp-string)
              source-path
              source-hash
              condition))
    path))

(defun %import-state-known-hashes (state)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry (or (getf state :imports) '()))
      (let ((hash (and (listp entry) (getf entry :source-hash))))
        (when (and (stringp hash) (plusp (length hash)))
          (setf (gethash hash table) t))))
    table))

(defun %collect-memory-import-sources (backend)
  (let ((source-backend (%make-source-backend backend)))
    (%file-memory-source-specs source-backend :scope :effective)))

(defun %collect-memory-import-candidates (backend)
  (loop for source in (%collect-memory-import-sources backend)
        append (loop for entry in (copy-list (getf source :entries))
                     collect (list :entry entry
                                   :scope (getf source :scope)
                                   :source-path (getf source :source-path)))))

(defun %parse-imported-id (stdout source-hash key)
  (or (loop for line in (uiop:split-string (or stdout "") :separator '(#\Newline))
            for trimmed = (%trim-text line)
            when (and (plusp (length trimmed))
                      (%string-prefix-p-ci "id" trimmed))
              do (let* ((separator (or (position #\: trimmed)
                                       (position #\Tab trimmed)
                                       (position #\Space trimmed)))
                        (raw (if separator
                                 (%trim-text (subseq trimmed (1+ separator)))
                                 "")))
                   (when (plusp (length raw))
                     (return raw))))
      (and (plusp (length (%trim-text key)))
           (%trim-text key))
      source-hash))

(defun %haake-backend-for-transfer (backend)
  (cond
    ((and (fboundp 'haake-cli-memory-backend-p)
          (funcall (symbol-function 'haake-cli-memory-backend-p) backend))
     (values backend nil))
    (t
     (let ((cfg (current-config)))
       (cond
         ((not (%haake-cli-available-from-config-p cfg))
          (values nil :haake-cli-unavailable))
         ((not (%haake-cli-status-ok-from-config-p cfg))
          (values nil :haake-status-unavailable))
         ((not (%haake-cli-compatible-from-config-p cfg))
          (values nil :haake-cli-incompatible))
         (t
          (let ((candidate (%make-haake-backend-from-config cfg)))
            (if candidate
                (values candidate nil)
                (values nil :haake-backend-instantiation-failed)))))))))

(defun %haake-metadata-arguments (source-path source-hash import-batch-id imported-at)
  (list "--metadata" (format nil "source_path=~A" source-path)
        "--metadata" (format nil "source_hash=~A" source-hash)
        "--metadata" (format nil "import_batch_id=~A" import-batch-id)
        "--metadata" (format nil "imported_at=~A" imported-at)))

(defun memory-import-to-haake (&key (backend (current-memory-backend)))
  (multiple-value-bind (haake-backend missing-reason)
      (%haake-backend-for-transfer backend)
    (unless haake-backend
      (return-from memory-import-to-haake
        (list :status :error
              :reason missing-reason
              :message "Haake backend is not available for import.")))
    (let* ((state (%load-memory-import-state backend))
           (known-hashes (%import-state-known-hashes state))
           (import-batch-id (format nil "batch-~D" (get-universal-time)))
           (imported-at (%utc-timestamp-string))
           (new-import-records '())
           (imported-count 0)
           (skipped-count 0)
           (failed-count 0)
           (failure-log-path nil))
      (dolist (candidate (%collect-memory-import-candidates backend))
        (let* ((entry (getf candidate :entry))
               (scope (getf candidate :scope))
               (source-path (getf candidate :source-path))
               (source-hash (%memory-entry-source-hash source-path entry)))
          (if (gethash source-hash known-hashes)
              (incf skipped-count)
              (handler-case
                  (let* ((result (%haake-cli-run
                                  haake-backend
                                  (append (list "memory"
                                                "insert"
                                                (%haake-scope-path haake-backend scope)
                                                (memory-entry-value entry)
                                                "-t"
                                                "semantic"
                                                "--key"
                                                (memory-entry-key entry)
                                                "--agent"
                                                (haake-cli-memory-backend-agent haake-backend))
                                          (%haake-metadata-arguments source-path
                                                                     source-hash
                                                                     import-batch-id
                                                                     imported-at))))
                         (imported-id (%parse-imported-id (getf result :stdout)
                                                          source-hash
                                                          (memory-entry-key entry))))
                    (push (list :source-path source-path
                                :source-hash source-hash
                                :scope scope
                                :key (memory-entry-key entry)
                                :value (memory-entry-value entry)
                                :imported-id imported-id
                                :import-batch-id import-batch-id
                                :imported-at imported-at)
                          new-import-records)
                    (setf (gethash source-hash known-hashes) t)
                    (incf imported-count))
                (error (condition)
                  (incf failed-count)
                  (setf failure-log-path
                        (%append-memory-import-failure backend
                                                       source-path
                                                       source-hash
                                                       condition)))))))
      (when (or (plusp imported-count) (plusp failed-count))
        (%write-memory-import-state
         (list :version 1
               :updated-at (%utc-timestamp-string)
               :imports (append (or (getf state :imports) '())
                                (nreverse new-import-records)))
         backend))
      (list :status (if (plusp failed-count) :partial :ok)
            :import-batch-id import-batch-id
            :imported imported-count
            :skipped skipped-count
            :failed failed-count
            :state-path (namestring (%memory-import-state-path backend))
            :failure-log-path (and failure-log-path (namestring failure-log-path))))))

(defun %state-topic-scope-name (scope)
  (when (and (consp scope) (eq (first scope) :topic))
    (let ((topic (second scope)))
      (when topic
        (%trim-text (princ-to-string topic))))))

(defun %import-state-topic-names (state)
  (sort (remove-duplicates
         (amoebum.fp:filter-map
          (lambda (entry)
            (let* ((scope (and (listp entry) (getf entry :scope)))
                   (topic (%state-topic-scope-name scope)))
              (when (and topic (plusp (length topic)))
                topic)))
          (or (getf state :imports) '()))
         :test #'string-equal)
        #'string< :key #'string-downcase))

(defun %write-export-section (stream title entries)
  (format stream "## ~A~%" title)
  (if entries
      (dolist (entry (%sort-memory-entries entries))
        (write-line (%memory-entry-line entry) stream))
      (write-line "(none)" stream))
  (write-line "" stream))

(defun memory-export-from-haake (&key (backend (current-memory-backend)))
  (multiple-value-bind (haake-backend missing-reason)
      (%haake-backend-for-transfer backend)
    (unless haake-backend
      (return-from memory-export-from-haake
        (list :status :error
              :reason missing-reason
              :message "Haake backend is not available for export.")))
    (let* ((state (%load-memory-import-state backend))
           (topic-names (%import-state-topic-names state))
           (global-entries (memory-list haake-backend :scope :global))
           (project-entries (memory-list haake-backend :scope :project))
           (topic-entry-count 0)
           (output-path (merge-pathnames #P".amoebum/memory/haake-export-MEMORY.md"
                                         (%memory-project-root backend))))
      (ensure-directories-exist output-path)
      (with-open-file (stream output-path
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (write-line "# Amoebum Memory" stream)
        (format stream "# Exported from Haake at ~A~%~%" (%utc-timestamp-string))
        (%write-export-section stream "Global" global-entries)
        (%write-export-section stream "Project" project-entries)
        (dolist (topic topic-names)
          (let ((entries (memory-list haake-backend :scope (list :topic topic))))
            (incf topic-entry-count (length entries))
            (%write-export-section stream
                                   (format nil "Topic: ~A" topic)
                                   entries))))
      (list :status :ok
            :output-path (namestring output-path)
            :global-count (length global-entries)
            :project-count (length project-entries)
            :topic-count topic-entry-count
            :topic-scope-count (length topic-names)))))
