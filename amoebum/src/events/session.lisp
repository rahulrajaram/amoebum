(in-package :amoebum)

;;; ============================================================
;;; I265: Session Recording and Playback
;;;
;;; Session metadata tracking, listing, replay, and export.
;;; Sessions span from journal start to stop, with metadata
;;; written on clean shutdown.
;;; ============================================================

;;; --- Session metadata ---

(defstruct (session-metadata
            (:constructor %make-session-metadata
                (&key id start-time end-time event-count
                      tool-call-count tokens-used model project-path
                      journal-segments)))
  (id "" :type string)
  (start-time 0 :type integer)
  (end-time 0 :type integer)
  (event-count 0 :type integer)
  (tool-call-count 0 :type integer)
  (tokens-used 0 :type integer)
  (model "" :type string)
  (project-path "" :type string)
  (journal-segments nil :type list))  ; list of segment file paths

(defparameter *session-directory* nil
  "Override directory for session metadata. NIL uses default.")

(defparameter *current-session-id* nil
  "The ID of the current session.")

;;; --- Directory resolution ---

(defun %session-default-directory ()
  (let ((dir (merge-pathnames ".amoebum/sessions/"
                              (user-homedir-pathname))))
    (ensure-directories-exist dir)
    dir))

(defun %session-directory ()
  (or *session-directory* (%session-default-directory)))

;;; --- Session ID generation ---

(defun %generate-session-id ()
  "Generate a session ID based on timestamp."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (format nil "session-~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0D"
            year month day hour min sec)))

;;; --- Metadata file I/O ---

(defun %session-metadata-path (session-id)
  (merge-pathnames (format nil "~A.meta.sexp" session-id)
                   (%session-directory)))

(defun %write-session-metadata (metadata)
  "Write session metadata to disk."
  (let ((path (%session-metadata-path (session-metadata-id metadata))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
      (let ((*print-readably* t)
            (*print-pretty* t))
        (write
         (list :id (session-metadata-id metadata)
               :start-time (session-metadata-start-time metadata)
               :end-time (session-metadata-end-time metadata)
               :event-count (session-metadata-event-count metadata)
               :tool-call-count (session-metadata-tool-call-count metadata)
               :tokens-used (session-metadata-tokens-used metadata)
               :model (session-metadata-model metadata)
               :project-path (session-metadata-project-path metadata)
               :journal-segments (mapcar #'namestring
                                         (session-metadata-journal-segments metadata)))
         :stream out)))
    path))

(defun %read-session-metadata (path)
  "Read session metadata from a .meta.sexp file."
  (handler-case
      (with-open-file (in path :direction :input :if-does-not-exist nil)
        (when in
          (let ((data (read in nil nil)))
            (when (listp data)
              (%make-session-metadata
               :id (or (getf data :id) "")
               :start-time (or (getf data :start-time) 0)
               :end-time (or (getf data :end-time) 0)
               :event-count (or (getf data :event-count) 0)
               :tool-call-count (or (getf data :tool-call-count) 0)
               :tokens-used (or (getf data :tokens-used) 0)
               :model (or (getf data :model) "")
               :project-path (or (getf data :project-path) "")
               :journal-segments (mapcar #'pathname
                                         (or (getf data :journal-segments) nil)))))))
    (error () nil)))

;;; --- Session lifecycle ---

(defun start-session (&key model project-path)
  "Start recording a new session. Returns session ID."
  (let* ((id (%generate-session-id))
         (metadata (%make-session-metadata
                    :id id
                    :start-time (get-universal-time)
                    :model (or model "")
                    :project-path (or project-path ""))))
    (setf *current-session-id* id)
    (%write-session-metadata metadata)
    id))

(defun stop-session (&key (journal *event-journal*))
  "Stop the current session, writing final metadata."
  (when *current-session-id*
    (let* ((path (%session-metadata-path *current-session-id*))
           (existing (%read-session-metadata path))
           (stats (when journal (journal-statistics journal)))
           (segments (when journal (journal-segment-paths journal)))
           (metadata (%make-session-metadata
                      :id *current-session-id*
                      :start-time (if existing
                                      (session-metadata-start-time existing)
                                      0)
                      :end-time (get-universal-time)
                      :event-count (if stats (getf stats :total-events) 0)
                      :tool-call-count (if existing
                                           (session-metadata-tool-call-count existing)
                                           0)
                      :tokens-used (if existing
                                       (session-metadata-tokens-used existing)
                                       0)
                      :model (if existing
                                 (session-metadata-model existing)
                                 "")
                      :project-path (if existing
                                        (session-metadata-project-path existing)
                                        "")
                      :journal-segments (or segments nil))))
      (%write-session-metadata metadata)
      (setf *current-session-id* nil)
      metadata)))

;;; --- Session listing ---

(defun list-sessions (&key date-from date-to)
  "List recorded sessions, optionally filtered by date range.
   DATE-FROM and DATE-TO are universal-time integers.
   Returns list of session-metadata structs sorted newest first."
  (let ((dir (%session-directory))
        (results '()))
    (when (probe-file dir)
      (dolist (path (directory (merge-pathnames "*.meta.sexp" dir)))
        (let ((meta (%read-session-metadata path)))
          (when meta
            (when (or (null date-from)
                      (>= (session-metadata-start-time meta) date-from))
              (when (or (null date-to)
                        (<= (session-metadata-start-time meta) date-to))
                (push meta results)))))))
    (sort results #'> :key #'session-metadata-start-time)))

;;; --- Session replay ---

(defun replay-session (session-id &key (speed-factor 0.0)
                                       event-types severities
                                       (target-bus (current-event-bus)))
  "Replay all events from a recorded session."
  (let* ((path (%session-metadata-path session-id))
         (meta (%read-session-metadata path)))
    (if (and meta (session-metadata-journal-segments meta))
        (replay-journal (session-metadata-journal-segments meta)
                        :speed-factor speed-factor
                        :event-types event-types
                        :severities severities
                        :target-bus target-bus)
        0)))

;;; --- Session export ---

(defun export-session (session-id &key (output-directory nil))
  "Export a session as a standalone JSONL bundle with metadata.
   Returns the path to the exported bundle directory."
  (let* ((meta-path (%session-metadata-path session-id))
         (meta (%read-session-metadata meta-path))
         (out-dir (or output-directory
                      (merge-pathnames (format nil "~A-export/" session-id)
                                       (%session-directory)))))
    (unless meta
      (return-from export-session nil))
    (ensure-directories-exist out-dir)
    ;; Copy metadata
    (let ((meta-dest (merge-pathnames "metadata.sexp" out-dir)))
      (uiop:copy-file meta-path meta-dest))
    ;; Copy journal segments
    (let ((seg-count 0))
      (dolist (seg-path (session-metadata-journal-segments meta))
        (when (and seg-path (probe-file seg-path))
          (let* ((name (file-namestring seg-path))
                 (dest (merge-pathnames name out-dir)))
            (uiop:copy-file seg-path dest)
            (incf seg-count))))
      ;; Write manifest
      (let ((manifest-path (merge-pathnames "manifest.sexp" out-dir)))
        (with-open-file (out manifest-path :direction :output
                                           :if-exists :supersede
                                           :if-does-not-exist :create)
          (write (list :session-id session-id
                       :exported-at (get-universal-time)
                       :segment-count seg-count
                       :event-count (session-metadata-event-count meta))
                 :stream out))))
    out-dir))
