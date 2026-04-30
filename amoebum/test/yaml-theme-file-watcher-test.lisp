(in-package :amoebum/test)

;;; NXT-587: YAML source file watcher.
;;;
;;; Guards `%yaml-theme-poll-and-publish-if-changed` (in
;;; `amoebum/src/ui/yaml-theme-layout.lisp`) — the per-frame mtime
;;; poll wired into chat-panel's :effects block. On detected change,
;;; the watcher must:
;;;
;;;   1. Publish a +EVENT-TYPE-YAML-THEME-FILE-CHANGED+ event on the
;;;      bound *event-bus* with the source path in its payload.
;;;   2. Delegate the actual reload to %chat-handle-yaml-reload-key!
;;;      with :trigger :watcher (which is already responsible for
;;;      reload + toast + runtime-log).
;;;
;;; Test isolation: every yaml-theme global the watcher touches plus
;;; *event-bus* is saved and restored via unwind-protect, per the
;;; non-negotiable rule in `amoebum/test/CLAUDE.md`.

(def-suite yaml-theme-file-watcher-suite
  :description
  "NXT-587: YAML file watcher — mtime poll, event publish, reload delegation."
  :in amoebum-suite)

(in-suite yaml-theme-file-watcher-suite)

(defmacro %nxt-587-with-clean-yaml-globals (&body body)
  "Save and restore every yaml-theme global plus *event-bus* the watcher
might touch. Mirrors %nxt-586-with-clean-yaml-globals but adds bus + the
loaded-p flag (the watcher branches on yaml-theme-needs-reload-p which
reads *yaml-theme-loaded-p*)."
  (let ((saved-source (gensym "SAVED-SOURCE-"))
        (saved-mtime (gensym "SAVED-MTIME-"))
        (saved-loaded-p (gensym "SAVED-LOADED-P-"))
        (saved-layout (gensym "SAVED-LAYOUT-"))
        (saved-behavior (gensym "SAVED-BEHAVIOR-"))
        (saved-bus (gensym "SAVED-BUS-")))
    `(let ((,saved-source amoebum::*yaml-theme-source-path*)
           (,saved-mtime amoebum::*yaml-theme-last-modified*)
           (,saved-loaded-p amoebum::*yaml-theme-loaded-p*)
           (,saved-layout amoebum::*yaml-layout-loaded*)
           (,saved-behavior amoebum::*yaml-behavior-loaded*)
           (,saved-bus amoebum::*event-bus*))
       (unwind-protect
            (progn ,@body)
         (setf amoebum::*yaml-theme-source-path* ,saved-source
               amoebum::*yaml-theme-last-modified* ,saved-mtime
               amoebum::*yaml-theme-loaded-p* ,saved-loaded-p
               amoebum::*yaml-layout-loaded* ,saved-layout
               amoebum::*yaml-behavior-loaded* ,saved-behavior
               amoebum::*event-bus* ,saved-bus)))))

(defun %nxt-587-write-temp-yaml-file ()
  "Write a tiny YAML file under amoebum/.tmp-nxt-587/ and return its pathname.
The file lives inside the ASDF project root so the permission layer
allows reads (see `.amoebum/MEMORY.md` 2026-04-08 entry on
plan-mode-smoke-test)."
  (let* ((dir (merge-pathnames ".tmp-nxt-587/"
                               (asdf:system-source-directory :amoebum)))
         (path (merge-pathnames
                (format nil "watcher-fixture-~A.yaml" (get-universal-time))
                dir)))
    (ensure-directories-exist dir)
    (with-open-file (stream path :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
      (write-string "palette:" stream)
      (terpri stream))
    path))

(defun %nxt-587-event-types-on-bus (bus)
  "Return the list of event-type keywords currently on BUS's history."
  (mapcar #'amoebum::event-type (amoebum::event-history bus)))

(test watcher-no-op-without-source-path
  "%yaml-theme-poll-and-publish-if-changed returns NIL and does not error
when *yaml-theme-source-path* is nil (no YAML loaded yet)."
  (%nxt-587-with-clean-yaml-globals
    (setf amoebum::*yaml-theme-source-path* nil
          amoebum::*yaml-theme-last-modified* nil
          amoebum::*yaml-theme-loaded-p* nil
          amoebum::*event-bus* (amoebum::make-event-bus))
    (let ((chat-state (amoebum::make-chat-ui-state)))
      (is (null (amoebum::%yaml-theme-poll-and-publish-if-changed
                 chat-state
                 :event-bus amoebum::*event-bus*)))
      (is (null (find amoebum::+event-type-yaml-theme-file-changed+
                      (%nxt-587-event-types-on-bus amoebum::*event-bus*)
                      :test #'eq))))))

(test watcher-detects-mtime-change-and-publishes-event
  "When the YAML file's mtime exceeds *yaml-theme-last-modified*, the watcher
returns T and publishes +EVENT-TYPE-YAML-THEME-FILE-CHANGED+ on the bus."
  (%nxt-587-with-clean-yaml-globals
    (let* ((path (%nxt-587-write-temp-yaml-file))
           (bus (amoebum::make-event-bus))
           (chat-state (amoebum::make-chat-ui-state)))
      (unwind-protect
           (progn
             (setf amoebum::*event-bus* bus
                   amoebum::*yaml-theme-source-path* path
                   amoebum::*yaml-theme-loaded-p* t
                   ;; Force needs-reload-p to return T by claiming we last
                   ;; loaded the file one second before its actual mtime.
                   amoebum::*yaml-theme-last-modified*
                   (max 0 (- (file-write-date path) 1)))
             (let ((returned (amoebum::%yaml-theme-poll-and-publish-if-changed
                              chat-state :event-bus bus)))
               (is-true returned))
             (let* ((events (amoebum::event-history bus))
                    (yaml-event
                     (find amoebum::+event-type-yaml-theme-file-changed+
                           events
                           :key #'amoebum::event-type
                           :test #'eq)))
               (is (not (null yaml-event)))
               (when yaml-event
                 ;; NXT-597: payload is now a frozen `file-changed-payload`
                 ;; struct produced by the generic file-watcher primitive
                 ;; rather than an ad-hoc plist.
                 (let ((payload (amoebum::event-payload yaml-event)))
                   (is (typep payload 'amoebum::file-changed-payload))
                   (is (equal (namestring path)
                              (amoebum::file-changed-path payload)))))))
        (ignore-errors (delete-file path))))))

(test watcher-no-event-when-mtime-unchanged
  "When *yaml-theme-last-modified* matches the file's actual mtime, the
watcher returns NIL and publishes no yaml-theme-file-changed event."
  (%nxt-587-with-clean-yaml-globals
    (let* ((path (%nxt-587-write-temp-yaml-file))
           (bus (amoebum::make-event-bus))
           (chat-state (amoebum::make-chat-ui-state)))
      (unwind-protect
           (progn
             (setf amoebum::*event-bus* bus
                   amoebum::*yaml-theme-source-path* path
                   amoebum::*yaml-theme-loaded-p* t
                   ;; mtime == last-modified -> needs-reload-p returns NIL.
                   amoebum::*yaml-theme-last-modified* (file-write-date path))
             (is (null (amoebum::%yaml-theme-poll-and-publish-if-changed
                        chat-state :event-bus bus)))
             (is (null (find amoebum::+event-type-yaml-theme-file-changed+
                             (%nxt-587-event-types-on-bus bus)
                             :test #'eq))))
        (ignore-errors (delete-file path))))))

(test reload-helper-accepts-watcher-trigger
  "%chat-handle-yaml-reload-key! accepts :trigger :watcher without erroring
even when no source path is set (no-op path). The trigger keyword is the
key API contract NXT-587 added to the helper."
  (%nxt-587-with-clean-yaml-globals
    (setf amoebum::*yaml-theme-source-path* nil
          amoebum::*yaml-theme-last-modified* nil
          amoebum::*yaml-theme-loaded-p* nil)
    (let ((chat-state (amoebum::make-chat-ui-state)))
      (finishes
       (amoebum::%chat-handle-yaml-reload-key! chat-state :trigger :watcher))
      ;; The watcher path must NOT add a "no change" toast — only the
      ;; operator path does.
      (is (zerop (length (amoebum::chat-ui-state-messages chat-state)))))))
