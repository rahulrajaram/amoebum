(in-package :amoebum/test)

;;; NXT-597: Generic file-watcher primitive
;;; (`amoebum/src/fp/file-watcher.lisp`).
;;;
;;; Guards the caller-driven polling watcher used by the YAML-theme
;;; reload (NXT-587) and the upcoming hot-patch reload (NXT-576).
;;; The primitive owns mtime polling, debounce, and event publishing;
;;; adopters layer side effects on top.

(def-suite file-watcher-suite
  :description
  "NXT-597: generic file-watcher primitive — empty paths, mtime detect,
debounce, status lifecycle, and on-error :log resilience."
  :in amoebum-suite)

(in-suite file-watcher-suite)

(defun %nxt-597-tmp-dir ()
  "Tmp dir under the ASDF project root (the permission layer refuses
reads outside `<repo>/amoebum/`, see MEMORY.md 2026-04-08)."
  (let ((dir (merge-pathnames ".tmp-nxt-597/"
                              (asdf:system-source-directory :amoebum))))
    (ensure-directories-exist dir)
    dir))

(defun %nxt-597-write-fixture (&key (content "v1"))
  "Write a unique fixture file under the tmp dir and return its
pathname."
  (let* ((path (merge-pathnames
                (format nil "fixture-~A-~A.txt"
                        (get-universal-time)
                        (random 100000))
                (%nxt-597-tmp-dir))))
    (with-open-file (s path :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string content s))
    path))

(defun %nxt-597-event-types-on-bus (bus)
  (mapcar #'amoebum::event-type (amoebum::event-history bus)))

(test watcher-no-op-when-paths-empty
  "make-watcher with no paths must poll cleanly and never publish."
  (let* ((bus (amoebum::make-event-bus))
         (watcher (amoebum::make-watcher
                   :id "empty"
                   :paths '()
                   :event-type amoebum::+event-type-yaml-theme-file-changed+
                   :event-bus bus)))
    (amoebum::start-watcher watcher)
    (is (null (amoebum::poll-watcher-once watcher)))
    (is (zerop (length (%nxt-597-event-types-on-bus bus))))))

(test watcher-detects-mtime-change-and-publishes
  "When a watched file's mtime advances past last-mtimes, poll publishes
a +EVENT-TYPE-YAML-THEME-FILE-CHANGED+ event whose payload's path
matches the watched namestring and kind is :modified."
  (let* ((bus (amoebum::make-event-bus))
         (path (%nxt-597-write-fixture :content "v1"))
         (watcher (amoebum::make-watcher
                   :id "mtime-test"
                   :paths (list path)
                   :event-type amoebum::+event-type-yaml-theme-file-changed+
                   :event-bus bus
                   :debounce-ms 0)))
    (unwind-protect
         (progn
           (amoebum::start-watcher watcher)
           ;; Force mtime to advance — explicit setf gives us a deterministic
           ;; bump even on coarse-grained filesystems.
           (let ((old-mtime (file-write-date path)))
             (setf (gethash (namestring (pathname path))
                            (amoebum::file-watcher-last-mtimes watcher))
                   (- old-mtime 1)))
           (let ((published (amoebum::poll-watcher-once watcher)))
             (is-true published))
           (let* ((events (amoebum::event-history bus))
                  (evt (find amoebum::+event-type-yaml-theme-file-changed+
                             events
                             :key #'amoebum::event-type
                             :test #'eq)))
             (is (not (null evt)))
             (when evt
               (let ((payload (amoebum::event-payload evt)))
                 (is (typep payload 'amoebum::file-changed-payload))
                 (is (equal (namestring path)
                            (amoebum::file-changed-path payload)))
                 (is (integerp (amoebum::file-changed-mtime payload)))
                 (is (eq :modified
                         (amoebum::file-changed-kind payload)))))))
      (ignore-errors (delete-file path)))))

(test watcher-debounce-suppresses-rapid-changes
  "Two mtime advances within debounce-ms produce a single event."
  (let* ((bus (amoebum::make-event-bus))
         (path (%nxt-597-write-fixture :content "v1"))
         (watcher (amoebum::make-watcher
                   :id "debounce-test"
                   :paths (list path)
                   :event-type amoebum::+event-type-yaml-theme-file-changed+
                   :event-bus bus
                   :debounce-ms 60000)))
    (unwind-protect
         (progn
           (amoebum::start-watcher watcher)
           (let ((path-key (namestring (pathname path)))
                 (current (file-write-date path)))
             ;; First synthetic change.
             (setf (gethash path-key
                            (amoebum::file-watcher-last-mtimes watcher))
                   (- current 2))
             (amoebum::poll-watcher-once watcher)
             ;; Second synthetic change immediately after.
             (setf (gethash path-key
                            (amoebum::file-watcher-last-mtimes watcher))
                   (- current 1))
             (amoebum::poll-watcher-once watcher))
           (let ((event-count
                   (count amoebum::+event-type-yaml-theme-file-changed+
                          (%nxt-597-event-types-on-bus bus)
                          :test #'eq)))
             (is (= 1 event-count))))
      (ignore-errors (delete-file path)))))

(test watcher-status-lifecycle
  "start-watcher -> :running, stop-watcher -> :stopped."
  (let ((watcher (amoebum::make-watcher
                  :id "lifecycle"
                  :paths '()
                  :event-type amoebum::+event-type-yaml-theme-file-changed+
                  :event-bus nil)))
    (is (eq :stopped (amoebum::watcher-status watcher)))
    (amoebum::start-watcher watcher)
    (is (eq :running (amoebum::watcher-status watcher)))
    (amoebum::stop-watcher watcher)
    (is (eq :stopped (amoebum::watcher-status watcher)))))

(test watcher-on-error-log-tolerates-missing-path
  "With on-error :log, polling a non-existent path must not raise; the
watcher's status must remain :running."
  (let* ((missing (merge-pathnames
                   (format nil "definitely-not-there-~A.txt"
                           (random 100000))
                   (%nxt-597-tmp-dir)))
         (bus (amoebum::make-event-bus))
         (watcher (amoebum::make-watcher
                   :id "missing-path"
                   :paths (list missing)
                   :event-type amoebum::+event-type-yaml-theme-file-changed+
                   :event-bus bus
                   :on-error :log)))
    (amoebum::start-watcher watcher)
    (finishes (amoebum::poll-watcher-once watcher))
    (is (eq :running (amoebum::watcher-status watcher)))))
