(in-package :amoebum)

;;;; amoebum/src/ui/hot-patch-watcher.lisp
;;;;
;;;; NXT-576: Hot-patch flow for live source reload.
;;;;
;;;; Watches `.lisp` files under `amoebum/src/` and (when present)
;;;; `~/.amoebum/extensions/`. On detected mtime change, the changed
;;;; file is `(load ...)`ed and a TUI toast is appended via
;;;; `chat-ui-add-message` confirming the redefinition will take
;;;; effect on the next call.
;;;;
;;;; Design — adopt the NXT-597 file-watcher primitive:
;;;;   - The primitive owns mtime polling, debounce, and event publish.
;;;;   - This module subscribes by walking the event-bus history after
;;;;     each `poll-watcher-once`, filtering for events whose sequence
;;;;     number exceeds the watermark stored in
;;;;     `*hot-patch-last-processed-seq*`. This keeps the primitive's
;;;;     API clean (publish-only) and lets future adopters reuse the
;;;;     same event-history walker pattern.
;;;;
;;;; Path-list cap: amoebum/src has ~80 .lisp files; the primitive's
;;;; per-frame poll does one syscall per path. We hard-cap at
;;;; +HOT-PATCH-MAX-PATHS+ (default 200) so a runaway
;;;; ~/.amoebum/extensions/ tree cannot make the per-frame walk
;;;; pathological. The cap is applied silently (with a runtime-log
;;;; warning) — a user editing source files inside amoebum/src must
;;;; not be blocked from hot-patching just because their extensions
;;;; directory is over-populated.

(defparameter +hot-patch-max-paths+ 1024
  "Upper bound on the number of files the hot-patch watcher will
poll per frame. Polling cost is proportional to this count.

The amoebum/src tree currently contains ~250 .lisp files, well below
this ceiling. The cap exists to bound the cost when
~/.amoebum/extensions/ balloons (a user could conceivably dump a
large library tree there). 1024 file-write-date syscalls per frame
is still well under any reasonable per-frame budget.")

(defparameter *hot-patch-watcher* nil
  "Lazy file-watcher singleton for amoebum/src + ~/.amoebum/extensions/.")

(defparameter *hot-patch-last-processed-seq* 0
  "High-water mark for event-bus sequence numbers already processed by
`hot-patch-poll-once`. Events with `event-seq` <= this value are
skipped.")

(defparameter *hot-patch-extensions-directory-override* nil
  "Test-only override: when non-NIL, used in place of
`~/.amoebum/extensions/` when computing watch paths.")

(defparameter *hot-patch-amoebum-src-directory-override* nil
  "Test-only override: when non-NIL, used in place of the amoebum/src
directory when computing watch paths.")

(defun %hot-patch-amoebum-src-directory ()
  "Return the amoebum/src/ directory pathname (with override hook)."
  (or *hot-patch-amoebum-src-directory-override*
      (let ((root (ignore-errors
                   (asdf:system-source-directory :amoebum))))
        (and root
             (uiop:ensure-directory-pathname
              (merge-pathnames #P"src/" root))))))

(defun %hot-patch-extensions-directory ()
  "Return the ~/.amoebum/extensions/ directory pathname (with override
hook). NIL is returned if neither the override nor the directory
exists, so callers can skip enumeration cleanly."
  (let ((dir (or *hot-patch-extensions-directory-override*
                 (uiop:ensure-directory-pathname
                  (merge-pathnames #P".amoebum/extensions/"
                                   (user-homedir-pathname))))))
    (and dir (probe-file dir) dir)))

(defun %hot-patch-collect-lisp-files (directory)
  "Collect .lisp files recursively under DIRECTORY. Returns a list of
namestrings. Tolerates missing/unreadable subdirectories silently."
  (let ((acc '()))
    (labels ((walk (dir)
               (let* ((safe (uiop:ensure-directory-pathname dir))
                      (files (ignore-errors
                              (uiop:directory-files safe "*.lisp")))
                      (subs (ignore-errors
                             (uiop:subdirectories safe))))
                 (dolist (file files)
                   (push (namestring file) acc))
                 (dolist (sub subs)
                   (walk sub)))))
      (when (and directory (probe-file directory))
        (walk directory)))
    acc))

(defun %hot-patch-watch-paths ()
  "Compute the set of paths to watch.
Returns a list of namestring paths. Capped at
+HOT-PATCH-MAX-PATHS+; if exceeded, emits a runtime-log warning."
  (let* ((src-dir (%hot-patch-amoebum-src-directory))
         (ext-dir (%hot-patch-extensions-directory))
         (paths (append (%hot-patch-collect-lisp-files src-dir)
                        (%hot-patch-collect-lisp-files ext-dir)))
         (deduped (remove-duplicates paths :test #'equal))
         (count (length deduped)))
    (cond
      ((<= count +hot-patch-max-paths+)
       deduped)
      (t
       (ignore-errors
        (log-runtime-event
         :level :warn
         :kind "hot-patch-paths-capped"
         :source :hot-patch-watcher
         :message (format nil
                          "Hot-patch watcher path-list (~D) exceeds cap (~D); truncating."
                          count +hot-patch-max-paths+)
         :details (list :total count :cap +hot-patch-max-paths+)))
       (subseq deduped 0 +hot-patch-max-paths+)))))

(defun %hot-patch-ensure-watcher (event-bus)
  "Lazily construct the hot-patch watcher singleton. Rebuilds it if
EVENT-BUS changes (typical in tests that swap *event-bus*)."
  (when (or (null *hot-patch-watcher*)
            (not (eq (file-watcher-event-bus *hot-patch-watcher*)
                     event-bus)))
    (setf *hot-patch-watcher*
          (start-watcher
           (make-watcher :id "hot-patch"
                         :paths (%hot-patch-watch-paths)
                         :event-type +event-type-extension-reloaded+
                         :event-bus event-bus
                         :debounce-ms 500
                         :on-error :log)))
    ;; Realign the seq watermark to the bus's current high-water mark
    ;; so a fresh watcher does not retroactively process historical
    ;; events on the bus from prior subsystems.
    (setf *hot-patch-last-processed-seq*
          (event-bus-next-seq event-bus)))
  *hot-patch-watcher*)

(defun %hot-patch-handle-changed-file (path)
  "Reload a changed .lisp file. Returns (values t nil) on success or
(values nil error-message) on failure. Never raises."
  (handler-case
      (progn
        (load path)
        (values t nil))
    (error (e)
      (values nil (princ-to-string e)))))

(defun %hot-patch-relative-path (path)
  "Best-effort short label for PATH relative to the amoebum/src tree
or the home extensions tree. Falls back to the bare file namestring,
then to the full namestring."
  (let* ((src-dir (%hot-patch-amoebum-src-directory))
         (ext-dir (%hot-patch-extensions-directory))
         (path-string (namestring path)))
    (cond
      ((and src-dir
            (let ((root (namestring src-dir)))
              (and (>= (length path-string) (length root))
                   (string= path-string root :end1 (length root)))))
       (concatenate 'string "amoebum/src/"
                    (subseq path-string
                            (length (namestring src-dir)))))
      ((and ext-dir
            (let ((root (namestring ext-dir)))
              (and (>= (length path-string) (length root))
                   (string= path-string root :end1 (length root)))))
       (concatenate 'string "~/.amoebum/extensions/"
                    (subseq path-string
                            (length (namestring ext-dir)))))
      (t (or (file-namestring (pathname path)) path-string)))))

(defun %hot-patch-emit-success-toast (chat-state path)
  (chat-ui-add-message
   chat-state
   "system"
   (format nil "Hot-patched: ~A (reloaded; takes effect on next call)"
           (%hot-patch-relative-path path))))

(defun %hot-patch-emit-failure-toast (chat-state path message)
  (chat-ui-add-message
   chat-state
   "system"
   (format nil "Hot-patch FAILED: ~A -- ~A"
           (%hot-patch-relative-path path)
           message)))

(defun %hot-patch-process-event (chat-state event)
  "Process a single +EVENT-TYPE-EXTENSION-RELOADED+ EVENT: extract its
path payload, reload the file, and emit a toast."
  (let* ((payload (event-payload event))
         (path (and (typep payload 'file-changed-payload)
                    (file-changed-path payload))))
    (when (and path (plusp (length path)))
      (multiple-value-bind (ok message)
          (%hot-patch-handle-changed-file path)
        (cond
          (ok
           (%hot-patch-emit-success-toast chat-state path)
           (ignore-errors
            (log-runtime-event
             :level :info
             :kind "hot-patch-reloaded"
             :source :hot-patch-watcher
             :message (format nil "Reloaded ~A" path)
             :details (list :path path))))
          (t
           (%hot-patch-emit-failure-toast chat-state path message)
           (ignore-errors
            (log-runtime-event
             :level :warning
             :kind "hot-patch-reload-failed"
             :source :hot-patch-watcher
             :message (format nil "Failed to reload ~A: ~A"
                              path message)
             :details (list :path path :error message)))))))))

(defun %hot-patch-drain-events (chat-state event-bus)
  "Walk EVENT-BUS history once, processing every
+EVENT-TYPE-EXTENSION-RELOADED+ event whose `event-seq` exceeds
*HOT-PATCH-LAST-PROCESSED-SEQ*. Updates the watermark. Returns the
count of files reloaded."
  (let ((processed 0)
        (max-seq *hot-patch-last-processed-seq*))
    (dolist (event (event-history event-bus))
      (when (and (eq (event-type event) +event-type-extension-reloaded+)
                 (> (event-seq event) *hot-patch-last-processed-seq*))
        (%hot-patch-process-event chat-state event)
        (incf processed)
        (when (> (event-seq event) max-seq)
          (setf max-seq (event-seq event)))))
    (setf *hot-patch-last-processed-seq* max-seq)
    processed))

(defun hot-patch-poll-once (chat-state &key (event-bus *event-bus*))
  "Per-frame poll. Drives the underlying file-watcher primitive once,
then walks newly-published events on EVENT-BUS to reload changed
files and emit toasts.

Returns the number of files reloaded this call (0 when no changes
were detected). Safe to call from a chat-panel :effects block — at
most one `file-write-date` syscall per watched path per call."
  (when (and chat-state event-bus)
    (let ((watcher (%hot-patch-ensure-watcher event-bus)))
      (poll-watcher-once watcher)
      (%hot-patch-drain-events chat-state event-bus))))
