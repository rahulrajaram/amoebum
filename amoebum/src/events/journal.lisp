(in-package :amoebum)

;;; ============================================================
;;; I263: Event Journal with Segment Rotation
;;;
;;; Crash-safe JSONL event journal with configurable segment
;;; rotation (by size or time). Events are appended to the
;;; active segment; when a threshold is met, the segment is
;;; closed and a new one opened.
;;; ============================================================

;;; --- Configuration ---

(defparameter *journal-directory* nil
  "Override directory for event journal segments. NIL uses default.")

(defparameter *journal-max-segment-bytes* (* 10 1024 1024) ; 10 MB
  "Maximum bytes per journal segment before rotation.")

(defparameter *journal-max-segment-seconds* 3600
  "Maximum seconds a segment stays active before rotation.")

(defparameter *journal-max-segments* 50
  "Maximum number of journal segments to retain. Oldest are pruned.")

(defparameter *journal-flush-interval-events* 10
  "Flush the journal stream every N events.")

;;; --- Journal segment record ---

(defstruct (journal-segment
            (:constructor %make-journal-segment
                (&key path created-at closed-at event-count byte-count)))
  (path nil :type (or null pathname))
  (created-at 0 :type integer)
  (closed-at 0 :type integer)
  (event-count 0 :type integer)
  (byte-count 0 :type integer))

;;; --- Journal state ---

(defstruct (event-journal
            (:constructor %make-event-journal
                (&key directory max-segment-bytes max-segment-seconds
                      max-segments flush-interval)))
  (directory nil :type (or null pathname))
  (max-segment-bytes (* 10 1024 1024) :type integer)
  (max-segment-seconds 3600 :type integer)
  (max-segments 50 :type integer)
  (flush-interval 10 :type integer)
  (active-stream nil)
  (active-segment nil :type (or null journal-segment))
  (segments nil :type list)   ; list of closed journal-segment records
  (total-events 0 :type integer)
  (subscription-id nil)
  (running-p nil :type boolean))

#+sb-thread
(defparameter *journal-lock*
  (sb-thread:make-mutex :name "amoebum-event-journal-lock"))

(defmacro %with-journal-lock (&body body)
  #+sb-thread
  `(sb-thread:with-mutex (*journal-lock*) ,@body)
  #-sb-thread
  `(progn ,@body))

;;; --- Directory resolution ---

(defun %journal-default-directory ()
  (let* ((home (user-homedir-pathname))
         (data-dir (merge-pathnames ".amoebum/journal/" home)))
    (ensure-directories-exist data-dir)
    data-dir))

(defun %journal-directory (journal)
  (or (event-journal-directory journal)
      *journal-directory*
      (%journal-default-directory)))

;;; --- Segment naming ---

(defun %journal-segment-name ()
  "Generate a segment filename based on current time."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (format nil "journal-~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0D.jsonl"
            year month day hour min sec)))

;;; --- Minimal JSON serialization for events ---

(defun %journal-escape-string (s)
  "Escape a string for JSON output."
  (with-output-to-string (out)
    (loop for c across s do
      (case c
        (#\" (write-string "\\\"" out))
        (#\\ (write-string "\\\\" out))
        (#\Newline (write-string "\\n" out))
        (#\Return (write-string "\\r" out))
        (#\Tab (write-string "\\t" out))
        (otherwise (write-char c out))))))

(defun %journal-event-type-string (event-type)
  "Return a stable package-neutral event type label for JSONL."
  (substitute #\- #\: (symbol-name event-type)))

(defun %journal-serialize-event (event)
  "Serialize an event to a single JSON line."
  (format nil "{\"seq\":~D,\"ts\":~D,\"type\":\"~A\",\"source\":\"~A\",\"severity\":\"~A\",\"payload\":\"~A\"}"
          (event-seq event)
          (event-timestamp event)
          (%journal-escape-string (%journal-event-type-string (event-type event)))
          (%journal-escape-string (princ-to-string (event-source event)))
          (%journal-escape-string (symbol-name (event-severity event)))
          (%journal-escape-string
           (let ((*print-length* 100)
                 (*print-level* 5))
             (princ-to-string (event-payload event))))))

;;; --- Segment lifecycle ---

(defun %journal-open-segment (journal)
  "Open a new active journal segment."
  (let* ((dir (%journal-directory journal))
         (_ (ensure-directories-exist dir))
         (name (%journal-segment-name))
         (path (merge-pathnames name dir))
         (stream (open path :direction :output
                            :if-exists :append
                            :if-does-not-exist :create
                            :element-type 'character
                            :external-format :utf-8))
         (segment (%make-journal-segment
                   :path path
                   :created-at (get-universal-time))))
    (%with-journal-lock
      (setf (event-journal-active-stream journal) stream
            (event-journal-active-segment journal) segment))
    segment))

(defun %journal-close-segment (journal)
  "Close the active segment and add it to the segments list."
  (%with-journal-lock
    (let ((stream (event-journal-active-stream journal))
          (segment (event-journal-active-segment journal)))
      (when stream
        (ignore-errors (force-output stream))
        (ignore-errors (close stream)))
      (when segment
        (setf (journal-segment-closed-at segment) (get-universal-time))
        (push segment (event-journal-segments journal)))
      (setf (event-journal-active-stream journal) nil
            (event-journal-active-segment journal) nil))))

(defun %journal-rotation-needed-p (journal)
  "Check if the active segment needs rotation."
  (let ((segment (event-journal-active-segment journal)))
    (when segment
      (or (>= (journal-segment-byte-count segment)
              (event-journal-max-segment-bytes journal))
          (>= (- (get-universal-time) (journal-segment-created-at segment))
              (event-journal-max-segment-seconds journal))))))

(defun %journal-rotate-if-needed (journal)
  "Rotate the active segment if thresholds are exceeded."
  (when (%journal-rotation-needed-p journal)
    (%journal-close-segment journal)
    (%journal-open-segment journal)
    (%journal-prune-old-segments journal)))

(defun %journal-prune-old-segments (journal)
  "Remove oldest segments beyond max-segments limit."
  (let ((max (event-journal-max-segments journal)))
    (%with-journal-lock
      (let ((segments (event-journal-segments journal)))
        (when (> (length segments) max)
          (let ((to-remove (nthcdr max segments)))
            (dolist (seg to-remove)
              (let ((path (journal-segment-path seg)))
                (when (and path (probe-file path))
                  (ignore-errors (delete-file path)))))
            (setf (event-journal-segments journal)
                  (subseq segments 0 max))))))))

;;; --- Event writing ---

(defun %journal-write-event (journal event)
  "Write a single event to the active journal segment."
  (let ((line (handler-case
                  (%journal-serialize-event event)
                (error () nil))))
    (when line
      (%journal-rotate-if-needed journal)
      (let ((stream (event-journal-active-stream journal)))
        (when stream
          (let ((bytes (length line)))
            (write-string line stream)
            (write-char #\Newline stream)
            (%with-journal-lock
              (let ((seg (event-journal-active-segment journal)))
                (when seg
                  (incf (journal-segment-event-count seg))
                  (incf (journal-segment-byte-count seg) (+ bytes 1))))
              (incf (event-journal-total-events journal)))
            ;; Periodic flush
            (when (zerop (mod (event-journal-total-events journal)
                              (event-journal-flush-interval journal)))
              (ignore-errors (force-output stream)))))))))

;;; --- Journal public API ---

(defvar *event-journal* nil
  "The active event journal instance.")

(defun make-event-journal-instance (&key directory max-segment-bytes
                                         max-segment-seconds max-segments
                                         flush-interval)
  "Create a new event journal."
  (%make-event-journal
   :directory (and directory (pathname directory))
   :max-segment-bytes (or max-segment-bytes *journal-max-segment-bytes*)
   :max-segment-seconds (or max-segment-seconds *journal-max-segment-seconds*)
   :max-segments (or max-segments *journal-max-segments*)
   :flush-interval (or flush-interval *journal-flush-interval-events*)))

(defun start-event-journal (&key journal (event-bus (current-event-bus)))
  "Start the event journal, subscribing to the event bus."
  (let ((j (or journal
               (setf *event-journal*
                     (make-event-journal-instance)))))
    (when (event-journal-running-p j)
      (return-from start-event-journal j))
    ;; Open first segment
    (%journal-open-segment j)
    ;; Subscribe to all events (wildcard :*)
    (let ((sub-id (subscribe event-bus :*
                             (lambda (event)
                               (%journal-write-event j event)))))
      (%with-journal-lock
        (setf (event-journal-subscription-id j) sub-id
              (event-journal-running-p j) t)))
    j))

(defun stop-event-journal (&optional (journal *event-journal*))
  "Stop the event journal and close the active segment."
  (when (and journal (event-journal-running-p journal))
    ;; Unsubscribe from event bus
    (let ((sub-id (event-journal-subscription-id journal)))
      (when sub-id
        (ignore-errors
          (unsubscribe (current-event-bus) sub-id))))
    ;; Close active segment
    (%journal-close-segment journal)
    (%with-journal-lock
      (setf (event-journal-running-p journal) nil
            (event-journal-subscription-id journal) nil)))
  journal)

(defun journal-statistics (&optional (journal *event-journal*))
  "Return a plist of journal statistics."
  (if journal
      (%with-journal-lock
        (list :running-p (event-journal-running-p journal)
              :total-events (event-journal-total-events journal)
              :closed-segments (length (event-journal-segments journal))
              :active-segment-events
              (let ((seg (event-journal-active-segment journal)))
                (if seg (journal-segment-event-count seg) 0))
              :active-segment-bytes
              (let ((seg (event-journal-active-segment journal)))
                (if seg (journal-segment-byte-count seg) 0))
              :directory (namestring (%journal-directory journal))))
      (list :running-p nil :total-events 0)))

(defun journal-segment-paths (&optional (journal *event-journal*))
  "Return list of all segment file paths (closed + active)."
  (when journal
    (%with-journal-lock
      (let ((paths (mapcar #'journal-segment-path
                           (event-journal-segments journal))))
        (let ((active (event-journal-active-segment journal)))
          (when active
            (push (journal-segment-path active) paths)))
        (remove nil paths)))))

(defun reset-event-journal ()
  "Stop and discard the current journal."
  (when *event-journal*
    (stop-event-journal *event-journal*))
  (setf *event-journal* nil))
