(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; Read Orchestration (I105, I148)
;;;
;;; Wires amoebum's read-file tool to pseudopod's read primitives with
;;; argument validation and user-facing errors.  Provides the orchestration
;;; layer that validates, normalises and dispatches read requests through
;;; the CLOS execute-tool pipeline.
;;; ---------------------------------------------------------------------------

;;; --- Constants and configuration -------------------------------------------

(defparameter *read-orchestration-max-line-limit* 100000
  "Maximum number of lines a single read request may return.")

(defparameter *read-orchestration-max-file-size-bytes* (* 50 1024 1024)
  "Maximum file size in bytes (50 MiB) for text reads.")

(defparameter *read-orchestration-supported-extensions*
  '("pdf" "png" "jpg" "jpeg" "gif" "svg" "ipynb" "csv" "tsv")
  "File extensions that receive specialised read handling.")

(defparameter +event-type-read-orchestration-cache+
  (intern "READ-ORCHESTRATION:CACHE" :keyword))

(defparameter *read-orchestration-cache* (make-hash-table :test #'equal)
  "In-memory cache of read orchestration payloads.")

(defparameter *read-orchestration-cache-index* (make-hash-table :test #'equal)
  "Map canonical path key to cache keys for targeted invalidation.")

(defparameter *read-orchestration-cache-signatures* (make-hash-table :test #'equal)
  "Map canonical path key to latest known file signature.")

(defparameter *read-orchestration-cache-lock*
  (bordeaux-threads:make-lock "amoebum-read-orchestration-cache"))

(defparameter *read-orchestration-cache-max-entries* 256
  "Maximum entries retained in the read orchestration cache.")

(defparameter *read-orchestration-cache-hits* 0)
(defparameter *read-orchestration-cache-misses* 0)
(defparameter *read-orchestration-cache-turn-id* nil)

(defun %clear-read-orchestration-cache-unsafe ()
  (clrhash *read-orchestration-cache*)
  (clrhash *read-orchestration-cache-index*)
  (clrhash *read-orchestration-cache-signatures*))

(defun clear-read-orchestration-cache (&key (reason :manual))
  "Clear read-orchestration cache entries and reset turn tracking."
  (declare (ignore reason))
  (bordeaux-threads:with-lock-held (*read-orchestration-cache-lock*)
    (%clear-read-orchestration-cache-unsafe)
    (setf *read-orchestration-cache-turn-id* nil))
  t)

(defun read-orchestration-cache-metrics ()
  "Return cache hit/miss/accounting metrics for read orchestration."
  (bordeaux-threads:with-lock-held (*read-orchestration-cache-lock*)
    (list :hits *read-orchestration-cache-hits*
          :misses *read-orchestration-cache-misses*
          :entries (hash-table-count *read-orchestration-cache*)
          :turn-id *read-orchestration-cache-turn-id*)))

(defun %read-orchestration-begin-turn (turn-id)
  (when turn-id
    (bordeaux-threads:with-lock-held (*read-orchestration-cache-lock*)
      (unless (equal turn-id *read-orchestration-cache-turn-id*)
        (%clear-read-orchestration-cache-unsafe)
        (setf *read-orchestration-cache-turn-id* turn-id)))))

(defun %read-orchestration-content-hash (path)
  (handler-case
      (with-open-file (stream path
                              :direction :input
                              :element-type '(unsigned-byte 8))
        (let* ((size (file-length stream))
               (octets (make-array size :element-type '(unsigned-byte 8))))
          (read-sequence octets stream)
          (sxhash octets)))
    (error ()
      nil)))

(defun %read-orchestration-file-signature (path)
  (let ((resolved (or (ignore-errors (truename path))
                      (probe-file path)
                      path)))
    (list :mtime (ignore-errors (file-write-date resolved))
          :size (%file-size-bytes resolved)
          :hash (%read-orchestration-content-hash resolved))))

(defun %read-orchestration-cache-path-key (path)
  (coerce-path-string (or (ignore-errors (truename path))
                  (probe-file path)
                  path)))

(defun %read-orchestration-cache-key (path-key signature offset limit pages)
  (list path-key signature (or offset 0) limit pages))

(defun %read-orchestration-invalidate-stale-unsafe (path-key signature)
  (let ((known (gethash path-key *read-orchestration-cache-signatures*)))
    (unless (equal known signature)
      (dolist (cache-key (gethash path-key *read-orchestration-cache-index*))
        (remhash cache-key *read-orchestration-cache*))
      (setf (gethash path-key *read-orchestration-cache-index*) '()
            (gethash path-key *read-orchestration-cache-signatures*) signature))))

(defun %read-orchestration-cache-lookup (path offset limit pages)
  (let* ((path-key (%read-orchestration-cache-path-key path))
         (signature (%read-orchestration-file-signature path))
         (cache-key (%read-orchestration-cache-key path-key signature offset limit pages)))
    (bordeaux-threads:with-lock-held (*read-orchestration-cache-lock*)
      (%read-orchestration-invalidate-stale-unsafe path-key signature)
      (let ((cached (gethash cache-key *read-orchestration-cache*)))
        (if cached
            (progn
              (incf *read-orchestration-cache-hits*)
              (values cached :hit))
            (progn
              (incf *read-orchestration-cache-misses*)
              (values nil :miss)))))))

(defun %read-orchestration-cache-store (path offset limit pages content)
  (let* ((path-key (%read-orchestration-cache-path-key path))
         (signature (%read-orchestration-file-signature path))
         (cache-key (%read-orchestration-cache-key path-key signature offset limit pages)))
    (bordeaux-threads:with-lock-held (*read-orchestration-cache-lock*)
      (%read-orchestration-invalidate-stale-unsafe path-key signature)
      (setf (gethash cache-key *read-orchestration-cache*) content)
      (pushnew cache-key
               (gethash path-key *read-orchestration-cache-index*)
               :test #'equal)
      (when (> (hash-table-count *read-orchestration-cache*)
               *read-orchestration-cache-max-entries*)
        (%clear-read-orchestration-cache-unsafe))
      content)))

(defun %normalize-speculative-paths (path candidate-paths)
  (let ((seen (make-hash-table :test #'equal))
        (ordered '()))
    (dolist (candidate (if candidate-paths
                           (cons path candidate-paths)
                           (list path)))
      (let ((normalized (%validate-read-path candidate)))
        (unless (gethash normalized seen)
          (setf (gethash normalized seen) t)
          (push normalized ordered))))
    (nreverse ordered)))

(defun %attempt-orchestrated-read (path-string offset limit pages)
  (handler-case
      (multiple-value-bind (_path validated-offset validated-limit validated-pages)
          (validate-read-arguments path-string offset limit pages)
        (declare (ignore _path))
        (let ((pathname-value (pathname path-string)))
          (multiple-value-bind (cached cache-status)
              (%read-orchestration-cache-lookup pathname-value
                                                validated-offset
                                                validated-limit
                                                validated-pages)
            (let ((content (or cached
                               (%read-orchestration-cache-store
                                pathname-value
                                validated-offset
                                validated-limit
                                validated-pages
                                (%read-file-content pathname-value
                                                    validated-offset
                                                    validated-limit
                                                    validated-pages)))))
              (list :ok t
                    :path (coerce-path-string pathname-value)
                    :cache-status cache-status
                    :content content)))))
    (error (condition)
      (list :ok nil
            :path path-string
            :error condition))))

(defun %run-speculative-read-attempts (paths offset limit pages)
  (if (= (length paths) 1)
      (list (%attempt-orchestrated-read (first paths) offset limit pages))
      (let* ((results (make-array (length paths) :initial-element nil))
             (threads
               (loop for path in paths
                     for index from 0
                     collect
                     (let ((path-copy path)
                           (index-copy index))
                       (bordeaux-threads:make-thread
                        (lambda ()
                          (setf (aref results index-copy)
                                (%attempt-orchestrated-read path-copy offset limit pages)))
                        :name (format nil "read-orchestration-speculative-~D" index-copy))))))
        (dolist (thread threads)
          (bordeaux-threads:join-thread thread))
        (loop for index from 0 below (length paths)
              collect (aref results index)))))

(defun %publish-read-orchestration-cache-event (event-bus selected-path attempts turn-id)
  (when (and event-bus (event-bus-p event-bus))
    (let* ((hit-count (count :hit attempts :test #'eq :key (lambda (attempt)
                                                             (getf attempt :cache-status))))
           (miss-count (count :miss attempts :test #'eq :key (lambda (attempt)
                                                               (getf attempt :cache-status))))
           (metrics (read-orchestration-cache-metrics)))
      (publish event-bus
               (make-event :type +event-type-read-orchestration-cache+
                           :source :amoebum
                           :severity :debug
                           :payload (list :selected-path selected-path
                                          :candidate-count (length attempts)
                                          :speculative-p (> (length attempts) 1)
                                          :turn-id turn-id
                                          :cache-hit-count hit-count
                                          :cache-miss-count miss-count
                                          :cache-hits-total (getf metrics :hits 0)
                                          :cache-misses-total (getf metrics :misses 0)
                                          :cache-entries (getf metrics :entries 0)))))))

;;; --- Argument validation helpers -------------------------------------------

(define-condition read-orchestration-error (tool-argument-error)
  ()
  (:documentation "Signalled when a read-file orchestration check fails.")
  (:report (lambda (condition stream)
             (let ((reason (or (tool-error-reason condition)
                               (amoebum-error-message condition)
                               "invalid read request")))
               (format stream "Read orchestration error for tool ~S: ~A"
                       (tool-error-tool-name condition) reason)))))

(defun %read-orch-signal (argument-name message &key reason)
  "Signal a READ-ORCHESTRATION-ERROR with the given details."
  (error 'read-orchestration-error
         :tool-name "read-file"
         :argument-name argument-name
         :message message
         :reason (or reason message)))

(defun %validate-read-path (path)
  "Validate that PATH is a non-empty absolute pathname string."
  (when (null path)
    (%read-orch-signal "path" "File path is required."
                       :reason "missing required argument"))
  (let ((path-string (typecase path
                       (pathname (namestring path))
                       (string path)
                       (t (princ-to-string path)))))
    (when (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                       path-string)))
      (%read-orch-signal "path" "File path must not be empty."
                         :reason "empty path"))
    (unless (char= (char path-string 0) #\/)
      (%read-orch-signal "path"
                         (format nil "File path must be absolute, got ~S." path-string)
                         :reason "relative path"))
    path-string))

(defun %validate-read-offset (offset)
  "Validate OFFSET is nil or a non-negative integer."
  (when offset
    (unless (integerp offset)
      (%read-orch-signal "offset"
                         (format nil "Offset must be an integer, got ~S." offset)
                         :reason "invalid offset type"))
    (when (< offset 0)
      (%read-orch-signal "offset"
                         (format nil "Offset must be non-negative, got ~D." offset)
                         :reason "negative offset")))
  offset)

(defun %validate-read-limit (limit)
  "Validate LIMIT is nil or a positive integer within bounds."
  (when limit
    (unless (integerp limit)
      (%read-orch-signal "limit"
                         (format nil "Limit must be an integer, got ~S." limit)
                         :reason "invalid limit type"))
    (when (< limit 0)
      (%read-orch-signal "limit"
                         (format nil "Limit must be non-negative, got ~D." limit)
                         :reason "negative limit"))
    (when (> limit *read-orchestration-max-line-limit*)
      (%read-orch-signal "limit"
                         (format nil "Limit ~D exceeds maximum ~D."
                                 limit *read-orchestration-max-line-limit*)
                         :reason "limit exceeds maximum")))
  limit)

(defun %validate-read-pages (pages path-string)
  "Validate PAGES argument: only allowed for PDF files."
  (when pages
    (let ((extension (let ((type (pathname-type (pathname path-string))))
                       (and type (string-downcase (princ-to-string type))))))
      (unless (string= (or extension "") "pdf")
        (%read-orch-signal "pages"
                           (format nil "Pages argument is only supported for PDF files, got ~S."
                                   path-string)
                           :reason "pages not supported for file type"))
      (unless (stringp pages)
        (%read-orch-signal "pages"
                           (format nil "Pages must be a string, got ~S." pages)
                           :reason "invalid pages type"))
      (let ((trimmed (string-trim '(#\Space #\Tab) pages)))
        (when (zerop (length trimmed))
          (%read-orch-signal "pages"
                             "Pages string must not be empty."
                             :reason "empty pages string")))))
  pages)

(defun %validate-file-exists (path-string)
  "Check that the file at PATH-STRING exists and is accessible."
  (unless (probe-file path-string)
    (%read-orch-signal "path"
                       (format nil "File not found: ~A" path-string)
                       :reason "file not found")))

(defun %validate-file-size (path-string)
  "Warn if file exceeds the configured maximum for text reads."
  (let ((extension (let ((type (pathname-type (pathname path-string))))
                     (and type (string-downcase (princ-to-string type))))))
    ;; Skip size check for binary-handled types (images, PDF)
    (unless (member extension '("png" "jpg" "jpeg" "gif" "svg" "pdf") :test #'string=)
      (let ((size (%file-size-bytes path-string)))
        (when (and size (> size *read-orchestration-max-file-size-bytes*))
          (%read-orch-signal "path"
                             (format nil "File ~A is ~,1F MiB, exceeding ~,1F MiB limit. Use offset/limit to read a portion."
                                     path-string
                                     (/ size 1024.0 1024.0)
                                     (/ *read-orchestration-max-file-size-bytes* 1024.0 1024.0))
                             :reason "file too large"))))))

;;; --- Orchestration entry point ---------------------------------------------

(defun validate-read-arguments (path offset limit pages)
  "Validate all read-file arguments, signalling READ-ORCHESTRATION-ERROR
on failure.  Returns normalised (path-string offset limit pages) values."
  (let ((path-string (%validate-read-path path)))
    (%validate-read-offset offset)
    (%validate-read-limit limit)
    (%validate-read-pages pages path-string)
    (%validate-file-exists path-string)
    (%validate-file-size path-string)
    (values path-string offset limit pages)))

(defun orchestrate-read (path &key offset limit pages candidate-paths turn-id event-bus)
  "Orchestrate a read-file request: validate arguments, check permissions,
invoke the read primitive, record state, and return content.

This is the primary entry point for programmatic read dispatch that
bypasses the LLM tool-call path but still applies full validation."
  (%read-orchestration-begin-turn turn-id)
  (let* ((candidate-path-list (%normalize-speculative-paths path candidate-paths))
         (attempts (%run-speculative-read-attempts candidate-path-list offset limit pages))
         (selected (find-if (lambda (attempt)
                              (getf attempt :ok))
                            attempts)))
    (unless selected
      (let ((first-error (getf (first attempts) :error)))
        (if first-error
            (error first-error)
            (%read-orch-signal "path"
                               (format nil "Unable to read any candidate path (~{~A~^, ~})."
                                       candidate-path-list)
                               :reason "all speculative reads failed"))))
    (%record-file-read-state (pathname (getf selected :path)))
    (%publish-read-orchestration-cache-event (or event-bus (ignore-errors (current-event-bus)))
                                             (getf selected :path)
                                             attempts
                                             turn-id)
    (getf selected :content)))

(defun orchestrate-read-via-pipeline (path &key offset limit pages
                                            (context nil)
                                            (event-bus nil))
  "Orchestrate a read through the full execute-tool CLOS pipeline.
Creates a tool-call and dispatches it through execute-tool for full
hook/event/permission coverage.

CONTEXT is an optional AMOEBUM-CONTEXT.  If nil, a default full-auto
context is created."
  ;; Pre-validate before pipeline dispatch so the user gets clean errors
  (validate-read-arguments path offset limit pages)
  (let* ((path-string (typecase path
                        (pathname (namestring path))
                        (t path)))
         (args-json
          (with-output-to-string (stream)
            (write-char #\{ stream)
            (format stream "\"path\":\"~A\"" path-string)
            (when offset
              (format stream ",\"offset\":~D" offset))
            (when limit
              (format stream ",\"limit\":~D" limit))
            (when pages
              (format stream ",\"pages\":\"~A\"" pages))
            (write-char #\} stream)))
         (ctx (or context
                  (make-amoebum-context
                   :permission-mode :full-auto
                   :event-bus (or event-bus (current-event-bus))
                   :initialize-notifications-p nil))))
    (let ((tool-call (pseudopod:make-tool-call
                      :id (format nil "read-orch-~D" (get-universal-time))
                      :name "read-file"
                      :arguments args-json)))
      (execute-tool tool-call ctx))))

;;; --- User-facing error formatting ------------------------------------------

(defun format-read-error-for-user (condition)
  "Format a read-related condition into a user-facing error string."
  (typecase condition
    (read-orchestration-error
     (format nil "Error: ~A" (or (amoebum-error-message condition)
                                 (tool-error-reason condition)
                                 "Invalid read request.")))
    (tool-permission-denied
     (format nil "Permission denied: Cannot read ~A"
             (or (tool-error-reason condition) "the requested file.")))
    (tool-timeout
     (format nil "Read timed out after ~A seconds."
             (or (tool-timeout-seconds condition) "unknown")))
    (tool-error
     (format nil "Read failed: ~A"
             (or (tool-error-reason condition)
                 (amoebum-error-message condition)
                 "Unknown error.")))
    (t
     (format nil "Read failed: ~A" condition))))
