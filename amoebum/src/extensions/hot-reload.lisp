(in-package :amoebum)

;;;; ============================================================
;;;; NXT-387: Extension hot-reload watch-thread runtime.
;;;;
;;;; Owns the polling watch loop and the start/stop lifecycle for the
;;;; background hot-reload thread. Reads the loader's discovery state
;;;; (*extension-watch-snapshot*, *extension-last-discovered*) and
;;;; calls back into the loader's reload entry point on change. The
;;;; loader holds the registry; this module holds the watch thread.
;;;;
;;;; Public API (preserved across the loader -> hot-reload split):
;;;;   - start-extension-hot-reload
;;;;   - stop-extension-hot-reload
;;;;   - check-extension-hot-reload
;;;;   - %rebuild-extension-watch-snapshot (loader-internal helper
;;;;     surfaced here because it owns *extension-watch-snapshot*)
;;;;
;;;; Thread shutdown:
;;;;   stop-extension-hot-reload clears the running flag BEFORE
;;;;   detaching the thread reference, so any in-flight loop iteration
;;;;   exits before we attempt to join. We refuse to self-join when
;;;;   stop is invoked from inside the watch thread (defensive — the
;;;;   loop body never calls stop itself, but reload errors might).
;;;; ============================================================

(defun %rebuild-extension-watch-snapshot (&optional (paths *extension-last-discovered*))
  "Recompute *EXTENSION-WATCH-SNAPSHOT* from PATHS plus any manifest /
entry-point references in the current load report. The snapshot maps
each watched path to its last known file-write date so the polling
loop can detect changes without re-reading file contents."
  (clrhash *extension-watch-snapshot*)
  (dolist (entry *extension-load-report*)
    (let ((manifest-path (extension-load-record-manifest-path entry))
          (entry-point (extension-load-record-entry-point entry)))
      (when (and (stringp manifest-path) (plusp (length (%extension-trim manifest-path))))
        (push manifest-path paths))
      (when (and (stringp entry-point)
                 (plusp (length (%extension-trim entry-point)))
                 (or (search ".lisp" entry-point :test #'char-equal)
                     (search "/" entry-point :test #'char=)
                     (search "\\" entry-point :test #'char=)))
        (push entry-point paths))))
  (dolist (path-text paths)
    (when (plusp (length (%extension-trim path-text)))
      (setf (gethash path-text *extension-watch-snapshot*)
            (%safe-file-write-date path-text))))
  *extension-watch-snapshot*)

(defun check-extension-hot-reload (&key project-root global-directory project-directory
                                        (reload-on-change t)
                                        (start-hot-reload *extension-hot-reload-enabled-p*))
  "Poll discovered extension files against *EXTENSION-WATCH-SNAPSHOT*.

Returns true when at least one manifest or entry-point file's
write-date has changed (or when an extension has appeared / vanished
since the previous snapshot). When RELOAD-ON-CHANGE is true and a
change is detected, calls RELOAD-USER-EXTENSIONS — the START-HOT-RELOAD
flag is forwarded so callers can suppress the watch thread (used by
tests that drive change detection manually)."
  (let* ((candidates (%collect-extension-candidates :project-root project-root
                                                    :global-directory global-directory
                                                    :project-directory project-directory))
         (current-paths (mapcar (lambda (entry)
                                  (%canonical-extension-path (cdr entry)))
                                candidates))
         (changed-p nil))
    (dolist (path-text current-paths)
      (let ((current (%safe-file-write-date path-text))
            (previous (gethash path-text *extension-watch-snapshot* :__missing__)))
        (when (or (eq previous :__missing__)
                  (not (eql previous current)))
          (setf changed-p t))))
    (maphash (lambda (path-text _value)
               (declare (ignore _value))
               (unless (member path-text current-paths :test #'string-equal)
                 (setf changed-p t)))
             *extension-watch-snapshot*)
    (when changed-p
      (if reload-on-change
          (progn
            (reload-user-extensions :project-root project-root
                                    :global-directory global-directory
                                    :project-directory project-directory
                                    :start-hot-reload start-hot-reload)
            t)
          (progn
            (%rebuild-extension-watch-snapshot current-paths)
            t)))))

(defun start-extension-hot-reload (&key project-root global-directory project-directory)
  "Launch (or return the existing) background hot-reload watch thread.

The thread polls CHECK-EXTENSION-HOT-RELOAD at the configured
interval. RELOAD-ON-CHANGE is true so detected changes trigger a
reload; START-HOT-RELOAD is suppressed inside the loop body so a
reload from within the watcher never recursively spawns another
thread."
  (when (and *extension-hot-reload-thread*
             (bordeaux-threads:thread-alive-p *extension-hot-reload-thread*))
    (return-from start-extension-hot-reload *extension-hot-reload-thread*))
  (setf *extension-hot-reload-running-p* t)
  (setf *extension-hot-reload-thread*
        (bordeaux-threads:make-thread
         (lambda ()
           (loop while *extension-hot-reload-running-p* do
             (ignore-errors
               (check-extension-hot-reload :project-root project-root
                                           :global-directory global-directory
                                           :project-directory project-directory
                                           :reload-on-change t
                                           :start-hot-reload nil))
             (sleep (max 0.1d0 *extension-hot-reload-interval-seconds*))))
         :name "amoebum-extension-hot-reload"))
  *extension-hot-reload-thread*)

(defun stop-extension-hot-reload ()
  "Halt the hot-reload watch thread.

Order of operations matters for race-freedom: clear the running flag
FIRST so any concurrent loop iteration sees the shutdown signal on its
next check, then drop the thread reference, then join. We never
self-join — if STOP is somehow called from inside the watch thread
itself we let it unwind naturally."
  (let ((thread *extension-hot-reload-thread*))
    (setf *extension-hot-reload-running-p* nil
          *extension-hot-reload-thread* nil)
    (when (and thread
               (bordeaux-threads:thread-alive-p thread)
               (not (eq thread (bordeaux-threads:current-thread))))
      (ignore-errors
        (bordeaux-threads:join-thread thread)))
    t))
