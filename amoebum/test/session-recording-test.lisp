(in-package :amoebum/test)

;;; ============================================================
;;; I265: Session Recording and Playback — Smoke Tests
;;; ============================================================

(def-suite session-recording-suite :in amoebum-suite)
(in-suite session-recording-suite)

;;; --- Structure tests ---

(test session-metadata-struct-exists
  "session-metadata struct and accessors are defined."
  (is (fboundp 'amoebum.sessions:session-metadata-p))
  (is (fboundp 'amoebum.sessions:session-metadata-id))
  (is (fboundp 'amoebum.sessions:session-metadata-start-time))
  (is (fboundp 'amoebum.sessions:session-metadata-end-time))
  (is (fboundp 'amoebum.sessions:session-metadata-event-count))
  (is (fboundp 'amoebum.sessions:session-metadata-model))
  (is (fboundp 'amoebum.sessions:session-metadata-project-path))
  (is (fboundp 'amoebum.sessions:session-metadata-journal-segments)))

;;; --- API functions ---

(test session-api-functions-exist
  "Session API functions are bound."
  (is (fboundp 'amoebum.sessions:start-session))
  (is (fboundp 'amoebum.sessions:stop-session))
  (is (fboundp 'amoebum.sessions:list-sessions))
  (is (fboundp 'amoebum.sessions:replay-session))
  (is (fboundp 'amoebum.sessions:export-session)))

;;; --- Session lifecycle ---

(test start-session-returns-id
  "start-session returns a session ID string."
  (let ((old-dir amoebum.sessions:*session-directory*)
        (old-id amoebum.sessions:*current-session-id*)
        (tmp-dir (merge-pathnames
                  (format nil "amoebum-session-test-~D/" (get-universal-time))
                  #P"/tmp/")))
    (unwind-protect
         (progn
           (ensure-directories-exist tmp-dir)
           (setf amoebum.sessions:*session-directory* tmp-dir)
           (let ((id (amoebum.sessions:start-session :model "test-model"
                                            :project-path "/test/proj")))
             (is (stringp id))
             (is (search "session-" id))
             (is (equal id amoebum.sessions:*current-session-id*))
             ;; Metadata file should exist
             (let ((meta-files (directory (merge-pathnames "*.meta.sexp" tmp-dir))))
               (is (= 1 (length meta-files))))))
      (setf amoebum.sessions:*session-directory* old-dir
            amoebum.sessions:*current-session-id* old-id)
      (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t)))))

(test stop-session-writes-metadata
  "stop-session writes end-time to metadata."
  (let ((old-dir amoebum.sessions:*session-directory*)
        (old-id amoebum.sessions:*current-session-id*)
        (old-journal amoebum:*event-journal*)
        (tmp-dir (merge-pathnames
                  (format nil "amoebum-session-stop-~D/" (get-universal-time))
                  #P"/tmp/")))
    (unwind-protect
         (progn
           (ensure-directories-exist tmp-dir)
           (setf amoebum.sessions:*session-directory* tmp-dir
                 amoebum:*event-journal* nil)
           (amoebum.sessions:start-session :model "stop-test")
           (let ((meta (amoebum.sessions:stop-session)))
             (is (amoebum.sessions:session-metadata-p meta))
             (is (plusp (amoebum.sessions:session-metadata-end-time meta)))
             (is (null amoebum.sessions:*current-session-id*))))
      (setf amoebum.sessions:*session-directory* old-dir
            amoebum.sessions:*current-session-id* old-id
            amoebum:*event-journal* old-journal)
      (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t)))))

;;; --- Session listing ---

(test list-sessions-finds-sessions
  "list-sessions finds recorded sessions."
  (let ((old-dir amoebum.sessions:*session-directory*)
        (old-id amoebum.sessions:*current-session-id*)
        (old-journal amoebum:*event-journal*)
        (tmp-dir (merge-pathnames
                  (format nil "amoebum-session-list-~D/" (get-universal-time))
                  #P"/tmp/")))
    (unwind-protect
         (progn
           (ensure-directories-exist tmp-dir)
           (setf amoebum.sessions:*session-directory* tmp-dir
                 amoebum:*event-journal* nil)
           ;; Create two sessions
           (amoebum.sessions:start-session :model "list-test-1")
           (amoebum.sessions:stop-session)
           (sleep 1)  ; ensure different timestamps
           (amoebum.sessions:start-session :model "list-test-2")
           (amoebum.sessions:stop-session)
           (let ((sessions (amoebum.sessions:list-sessions)))
             (is (= 2 (length sessions)))
             ;; Newest first
             (is (>= (amoebum.sessions:session-metadata-start-time (first sessions))
                      (amoebum.sessions:session-metadata-start-time (second sessions))))))
      (setf amoebum.sessions:*session-directory* old-dir
            amoebum.sessions:*current-session-id* old-id
            amoebum:*event-journal* old-journal)
      (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t)))))

;;; --- Session replay ---

(test replay-session-nonexistent
  "replay-session returns 0 for nonexistent session."
  (let ((old-dir amoebum.sessions:*session-directory*)
        (tmp-dir (merge-pathnames "amoebum-replay-none/" #P"/tmp/")))
    (unwind-protect
         (progn
           (ensure-directories-exist tmp-dir)
           (setf amoebum.sessions:*session-directory* tmp-dir)
           (is (= 0 (amoebum.sessions:replay-session "no-such-session"))))
      (setf amoebum.sessions:*session-directory* old-dir)
      (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t)))))

;;; --- Session export ---

(test export-session-nonexistent
  "export-session returns nil for nonexistent session."
  (let ((old-dir amoebum.sessions:*session-directory*)
        (tmp-dir (merge-pathnames "amoebum-export-none/" #P"/tmp/")))
    (unwind-protect
         (progn
           (ensure-directories-exist tmp-dir)
           (setf amoebum.sessions:*session-directory* tmp-dir)
           (is (null (amoebum.sessions:export-session "no-such-session"))))
      (setf amoebum.sessions:*session-directory* old-dir)
      (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t)))))
