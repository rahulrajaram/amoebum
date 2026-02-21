(in-package :amoebum/test)

;;; ============================================================
;;; I265: Session Recording and Playback — Smoke Tests
;;; ============================================================

(def-suite session-recording-suite :in amoebum-suite)
(in-suite session-recording-suite)

;;; --- Structure tests ---

(test session-metadata-struct-exists
  "session-metadata struct and accessors are defined."
  (is (fboundp 'amoebum:session-metadata-p))
  (is (fboundp 'amoebum:session-metadata-id))
  (is (fboundp 'amoebum:session-metadata-start-time))
  (is (fboundp 'amoebum:session-metadata-end-time))
  (is (fboundp 'amoebum:session-metadata-event-count))
  (is (fboundp 'amoebum:session-metadata-model))
  (is (fboundp 'amoebum:session-metadata-project-path))
  (is (fboundp 'amoebum:session-metadata-journal-segments)))

;;; --- API functions ---

(test session-api-functions-exist
  "Session API functions are bound."
  (is (fboundp 'amoebum:start-session))
  (is (fboundp 'amoebum:stop-session))
  (is (fboundp 'amoebum:list-sessions))
  (is (fboundp 'amoebum:replay-session))
  (is (fboundp 'amoebum:export-session)))

;;; --- Session lifecycle ---

(test start-session-returns-id
  "start-session returns a session ID string."
  (let ((old-dir amoebum:*session-directory*)
        (old-id amoebum:*current-session-id*)
        (tmp-dir (merge-pathnames
                  (format nil "amoebum-session-test-~D/" (get-universal-time))
                  #P"/tmp/")))
    (unwind-protect
         (progn
           (ensure-directories-exist tmp-dir)
           (setf amoebum:*session-directory* tmp-dir)
           (let ((id (amoebum:start-session :model "test-model"
                                            :project-path "/test/proj")))
             (is (stringp id))
             (is (search "session-" id))
             (is (equal id amoebum:*current-session-id*))
             ;; Metadata file should exist
             (let ((meta-files (directory (merge-pathnames "*.meta.sexp" tmp-dir))))
               (is (= 1 (length meta-files))))))
      (setf amoebum:*session-directory* old-dir
            amoebum:*current-session-id* old-id)
      (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t)))))

(test stop-session-writes-metadata
  "stop-session writes end-time to metadata."
  (let ((old-dir amoebum:*session-directory*)
        (old-id amoebum:*current-session-id*)
        (old-journal amoebum:*event-journal*)
        (tmp-dir (merge-pathnames
                  (format nil "amoebum-session-stop-~D/" (get-universal-time))
                  #P"/tmp/")))
    (unwind-protect
         (progn
           (ensure-directories-exist tmp-dir)
           (setf amoebum:*session-directory* tmp-dir
                 amoebum:*event-journal* nil)
           (amoebum:start-session :model "stop-test")
           (let ((meta (amoebum:stop-session)))
             (is (amoebum:session-metadata-p meta))
             (is (plusp (amoebum:session-metadata-end-time meta)))
             (is (null amoebum:*current-session-id*))))
      (setf amoebum:*session-directory* old-dir
            amoebum:*current-session-id* old-id
            amoebum:*event-journal* old-journal)
      (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t)))))

;;; --- Session listing ---

(test list-sessions-finds-sessions
  "list-sessions finds recorded sessions."
  (let ((old-dir amoebum:*session-directory*)
        (old-id amoebum:*current-session-id*)
        (old-journal amoebum:*event-journal*)
        (tmp-dir (merge-pathnames
                  (format nil "amoebum-session-list-~D/" (get-universal-time))
                  #P"/tmp/")))
    (unwind-protect
         (progn
           (ensure-directories-exist tmp-dir)
           (setf amoebum:*session-directory* tmp-dir
                 amoebum:*event-journal* nil)
           ;; Create two sessions
           (amoebum:start-session :model "list-test-1")
           (amoebum:stop-session)
           (sleep 1)  ; ensure different timestamps
           (amoebum:start-session :model "list-test-2")
           (amoebum:stop-session)
           (let ((sessions (amoebum:list-sessions)))
             (is (= 2 (length sessions)))
             ;; Newest first
             (is (>= (amoebum:session-metadata-start-time (first sessions))
                      (amoebum:session-metadata-start-time (second sessions))))))
      (setf amoebum:*session-directory* old-dir
            amoebum:*current-session-id* old-id
            amoebum:*event-journal* old-journal)
      (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t)))))

;;; --- Session replay ---

(test replay-session-nonexistent
  "replay-session returns 0 for nonexistent session."
  (let ((old-dir amoebum:*session-directory*)
        (tmp-dir (merge-pathnames "amoebum-replay-none/" #P"/tmp/")))
    (unwind-protect
         (progn
           (ensure-directories-exist tmp-dir)
           (setf amoebum:*session-directory* tmp-dir)
           (is (= 0 (amoebum:replay-session "no-such-session"))))
      (setf amoebum:*session-directory* old-dir)
      (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t)))))

;;; --- Session export ---

(test export-session-nonexistent
  "export-session returns nil for nonexistent session."
  (let ((old-dir amoebum:*session-directory*)
        (tmp-dir (merge-pathnames "amoebum-export-none/" #P"/tmp/")))
    (unwind-protect
         (progn
           (ensure-directories-exist tmp-dir)
           (setf amoebum:*session-directory* tmp-dir)
           (is (null (amoebum:export-session "no-such-session"))))
      (setf amoebum:*session-directory* old-dir)
      (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t)))))
