(in-package :amoebum)

;;;; amoebum/src/fp/file-watcher.lisp
;;;;
;;;; NXT-597: Generic polling file-watcher primitive.
;;;;
;;;; Caller-driven (no background thread). Each `poll-watcher-once` tick
;;;; performs at most one `file-write-date` syscall per watched path,
;;;; compares with the last observed mtime, and on detected change
;;;; publishes a typed event on the watcher's event-bus carrying a
;;;; `file-changed-payload`. A debounce window suppresses redundant
;;;; events from rapid back-to-back saves.
;;;;
;;;; Designed for adoption by the YAML-theme watcher (NXT-587) and the
;;;; hot-patch watcher (NXT-576). The primitive owns ONLY detection +
;;;; publish — adopters layer their own on-change side effects (reload,
;;;; toast, runtime-log) on top, either via the bus subscriber pattern
;;;; or by inspecting the boolean return of `poll-watcher-once`.
;;;;
;;;; Interface and event payload shape are defined by the NXT-593
;;;; decision memo (`.agent/file-watcher-primitive-decision-2026-04-29.md`).

(defstruct (file-changed-payload (:conc-name file-changed-))
  "Frozen payload shape published on the event bus when a watcher
detects a file mtime change."
  (path           ""        :type string  :read-only t)
  (mtime          0         :type integer :read-only t)
  (kind           :modified :type (member :modified :created :deleted)
                            :read-only t)
  (watcher-id     ""        :type string  :read-only t))

(defstruct file-watcher
  "Caller-driven polling file watcher.

Slots:
  ID            -- string, identifies the watcher in payloads.
  PATHS         -- list of pathname designators (strings or pathnames).
  EVENT-TYPE    -- event-type to publish on detected change.
  EVENT-BUS     -- event-bus instance, or NIL to disable publishing.
  DEBOUNCE-MS   -- minimum interval (ms) between successive publishes
                   for the same path. 0 disables debouncing.
  ON-ERROR      -- one of :log, :raise, :ignore. Controls behavior when
                   `file-write-date` raises (deleted file, permissions).
  LAST-MTIMES   -- hash-table from path namestring to last observed
                   universal-time integer. Initialized lazily.
  LAST-PUBLISH  -- hash-table from path namestring to monotonic-ms of
                   the last published event. Used for debounce.
  STATUS        -- :stopped, :running, or (:failed-with REASON)."
  (id                   ""               :type string)
  (paths                nil              :type list)
  (event-type           nil)
  (event-bus            nil)
  (debounce-ms          250              :type integer)
  (on-error             :log             :type keyword)
  (last-mtimes          (make-hash-table :test 'equal) :type hash-table)
  (last-publish         (make-hash-table :test 'equal) :type hash-table)
  (status               :stopped))

(defun %file-watcher-path-key (path)
  "Stable string key for a pathname designator."
  (namestring (pathname path)))

(defun %file-watcher-current-mtime (path)
  "Return the current `file-write-date` for PATH, or NIL on any error.
The two-value return mirrors `gethash`: (mtime found-p)."
  (handler-case
      (let ((mtime (file-write-date path)))
        (if mtime
            (values mtime t)
            (values nil nil)))
    (error ()
      (values nil nil))))

(defun %file-watcher-handle-error (watcher path condition)
  "Apply WATCHER's ON-ERROR policy to a CONDITION raised while polling PATH."
  (ecase (file-watcher-on-error watcher)
    (:ignore nil)
    (:log
     (ignore-errors
      (log-runtime-event
       :level :debug
       :kind "file-watcher-poll-error"
       :source :file-watcher
       :message (format nil "file-watcher poll error on ~A: ~A"
                        path condition)
       :details (list :path (%file-watcher-path-key path)
                      :watcher-id (file-watcher-id watcher)))))
    (:raise
     (error condition))))

(defun make-watcher (&key (paths nil) event-type event-bus
                          (debounce-ms 250) (on-error :log) (id ""))
  "Construct a `file-watcher`. Initial mtimes are captured eagerly so the
first `poll-watcher-once` does not fire spurious events for files that
already exist."
  (let ((watcher (make-file-watcher
                  :id id
                  :paths paths
                  :event-type event-type
                  :event-bus event-bus
                  :debounce-ms debounce-ms
                  :on-error on-error)))
    (dolist (path paths)
      (multiple-value-bind (mtime foundp)
          (%file-watcher-current-mtime path)
        (when foundp
          (setf (gethash (%file-watcher-path-key path)
                         (file-watcher-last-mtimes watcher))
                mtime))))
    watcher))

(defun start-watcher (watcher)
  "Mark WATCHER as :running. Returns WATCHER."
  (setf (file-watcher-status watcher) :running)
  watcher)

(defun stop-watcher (watcher)
  "Mark WATCHER as :stopped. Returns WATCHER."
  (setf (file-watcher-status watcher) :stopped)
  watcher)

(defun watcher-status (watcher)
  "Return the watcher's status: :running, :stopped, or
`(:failed-with REASON)`."
  (file-watcher-status watcher))

(defun %file-watcher-debounce-elapsed-p (watcher path-key now-ms)
  "T if enough time has passed since the last publish for PATH-KEY."
  (let ((debounce (file-watcher-debounce-ms watcher))
        (last (gethash path-key (file-watcher-last-publish watcher))))
    (or (zerop debounce)
        (null last)
        (>= (- now-ms last) debounce))))

(declaim (inline %file-watcher-monotonic-ms))
(defun %file-watcher-monotonic-ms ()
  "Monotonic millisecond clock — alias for `monotonic-ms` so callers
can tolerate future clock-source changes here."
  (monotonic-ms))

(defun %file-watcher-publish-change (watcher path-key new-mtime kind)
  "Publish a change event on WATCHER's bus and update bookkeeping."
  (when (and (file-watcher-event-bus watcher)
             (file-watcher-event-type watcher))
    (publish (file-watcher-event-bus watcher)
             (file-watcher-event-type watcher)
             :source :file-watcher
             :severity :info
             :payload (make-file-changed-payload
                       :path path-key
                       :mtime (or new-mtime 0)
                       :kind kind
                       :watcher-id (file-watcher-id watcher))))
  (setf (gethash path-key (file-watcher-last-publish watcher))
        (%file-watcher-monotonic-ms))
  (when new-mtime
    (setf (gethash path-key (file-watcher-last-mtimes watcher))
          new-mtime)))

(defun %file-watcher-poll-one-path (watcher path)
  "Poll a single PATH. Returns T if a change event was published."
  (handler-case
      (multiple-value-bind (current-mtime foundp)
          (%file-watcher-current-mtime path)
        (let* ((path-key (%file-watcher-path-key path))
               (last (gethash path-key (file-watcher-last-mtimes watcher)))
               (now-ms (%file-watcher-monotonic-ms)))
          (cond
            ;; File missing — only emit a :deleted event once (when we
            ;; previously had an mtime).
            ((not foundp)
             (when (and last
                        (%file-watcher-debounce-elapsed-p watcher path-key now-ms))
               (%file-watcher-publish-change watcher path-key 0 :deleted)
               (remhash path-key (file-watcher-last-mtimes watcher))
               t))
            ;; File appeared for the first time.
            ((null last)
             (when (%file-watcher-debounce-elapsed-p watcher path-key now-ms)
               (%file-watcher-publish-change watcher path-key
                                             current-mtime :created)
               t))
            ;; mtime advanced.
            ((> current-mtime last)
             (when (%file-watcher-debounce-elapsed-p watcher path-key now-ms)
               (%file-watcher-publish-change watcher path-key
                                             current-mtime :modified)
               t))
            (t nil))))
    (error (c)
      (%file-watcher-handle-error watcher path c)
      nil)))

(defun poll-watcher-once (watcher)
  "Poll all of WATCHER's PATHS exactly once. Returns T if at least one
change event was published; NIL otherwise. Safe to call from a per-frame
idle hook — at most one `file-write-date` syscall per watched path.

Polls regardless of status — callers that want start/stop gating should
inspect `watcher-status` themselves before calling. (This keeps the
primitive simple and lets adopters wire status into their own caller
loops.)"
  (let ((any-published nil))
    (dolist (path (file-watcher-paths watcher))
      (when (%file-watcher-poll-one-path watcher path)
        (setf any-published t)))
    any-published))
