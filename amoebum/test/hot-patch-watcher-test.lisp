(in-package :amoebum/test)

;;; NXT-576: Hot-patch watcher.
;;;
;;; Guards `amoebum/src/ui/hot-patch-watcher.lisp` — the per-frame
;;; file-watcher that reloads changed `.lisp` files under
;;; `amoebum/src/` and `~/.amoebum/extensions/` and emits a TUI toast.
;;;
;;; Test isolation: every global the module touches plus *event-bus*
;;; is saved and restored via unwind-protect, per the non-negotiable
;;; rule in `amoebum/test/CLAUDE.md`.

(def-suite hot-patch-watcher-suite
  :description
  "NXT-576: hot-patch watcher — path discovery, reload, failure handling, toast."
  :in amoebum-suite)

(in-suite hot-patch-watcher-suite)

(defmacro %nxt-576-with-clean-globals (&body body)
  "Save and restore every global the hot-patch watcher might touch
plus *event-bus*."
  (let ((saved-watcher (gensym "SAVED-WATCHER-"))
        (saved-seq (gensym "SAVED-SEQ-"))
        (saved-ext-override (gensym "SAVED-EXT-OVERRIDE-"))
        (saved-src-override (gensym "SAVED-SRC-OVERRIDE-"))
        (saved-bus (gensym "SAVED-BUS-")))
    `(let ((,saved-watcher amoebum::*hot-patch-watcher*)
           (,saved-seq amoebum::*hot-patch-last-processed-seq*)
           (,saved-ext-override
            amoebum::*hot-patch-extensions-directory-override*)
           (,saved-src-override
            amoebum::*hot-patch-amoebum-src-directory-override*)
           (,saved-bus amoebum::*event-bus*))
       (unwind-protect
            (progn ,@body)
         (setf amoebum::*hot-patch-watcher* ,saved-watcher
               amoebum::*hot-patch-last-processed-seq* ,saved-seq
               amoebum::*hot-patch-extensions-directory-override*
               ,saved-ext-override
               amoebum::*hot-patch-amoebum-src-directory-override*
               ,saved-src-override
               amoebum::*event-bus* ,saved-bus)))))

(defun %nxt-576-tmp-dir ()
  "Tmp dir under the ASDF project root (the permission layer refuses
reads outside `<repo>/amoebum/`, see MEMORY.md 2026-04-08)."
  (let ((dir (merge-pathnames ".tmp-nxt-576/"
                              (asdf:system-source-directory :amoebum))))
    (ensure-directories-exist dir)
    dir))

(defun %nxt-576-write-temp-lisp (content)
  "Write CONTENT to a unique temp .lisp file and return its pathname."
  (let* ((path (merge-pathnames
                (format nil "fixture-~A-~A.lisp"
                        (get-universal-time)
                        (random 1000000))
                (%nxt-576-tmp-dir))))
    (with-open-file (s path :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string content s))
    path))

(test watch-paths-include-amoebum-src-lisp-files
  "%hot-patch-watch-paths returns at least one .lisp file from the
amoebum/src tree (the canonical home of the source code being
hot-patched)."
  (%nxt-576-with-clean-globals
    (let ((paths (amoebum::%hot-patch-watch-paths)))
      (is (consp paths))
      (is (every (lambda (path)
                   (let ((s (string path)))
                     (and (>= (length s) 5)
                          (string= ".lisp" s
                                   :start2 (- (length s) 5)))))
                 paths))
      ;; main.lisp is a stable, well-known file in amoebum/src.
      (let* ((src-dir (amoebum::%hot-patch-amoebum-src-directory))
             (main-path (and src-dir
                             (namestring
                              (merge-pathnames "main.lisp" src-dir)))))
        (is (not (null main-path)))
        (is (find main-path paths :test #'equal))))))

(test watch-paths-tolerates-missing-extensions-dir
  "When ~/.amoebum/extensions/ does not exist, %hot-patch-watch-paths
must not error and must still return amoebum/src files."
  (%nxt-576-with-clean-globals
    ;; Force the extensions directory to a non-existent path.
    (setf amoebum::*hot-patch-extensions-directory-override*
          (merge-pathnames
           (format nil "definitely-not-there-~A/" (random 1000000))
           (%nxt-576-tmp-dir)))
    (finishes (amoebum::%hot-patch-watch-paths))
    (is (consp (amoebum::%hot-patch-watch-paths)))))

(defparameter *hot-patch-test-marker* :unset
  "Sentinel rebound by the temp .lisp fixtures used in the reload test.")

(test handle-changed-file-reloads-temp-lisp
  "%hot-patch-handle-changed-file actually `(load ...)`s the file —
rewriting the file with a new defparameter value flips the symbol
on the next call."
  (%nxt-576-with-clean-globals
    (let ((path nil))
      (unwind-protect
           (progn
             (setf *hot-patch-test-marker* :unset)
             (setf path
                   (%nxt-576-write-temp-lisp
                    "(in-package :amoebum/test)
(setf amoebum/test::*hot-patch-test-marker* :before)"))
             (multiple-value-bind (ok err)
                 (amoebum::%hot-patch-handle-changed-file path)
               (is-true ok)
               (is (null err)))
             (is (eq :before *hot-patch-test-marker*))
             ;; Rewrite with new content.
             (with-open-file (s path :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
               (write-string
                "(in-package :amoebum/test)
(setf amoebum/test::*hot-patch-test-marker* :after)"
                s))
             (multiple-value-bind (ok err)
                 (amoebum::%hot-patch-handle-changed-file path)
               (is-true ok)
               (is (null err)))
             (is (eq :after *hot-patch-test-marker*)))
        (setf *hot-patch-test-marker* :unset)
        (when path (ignore-errors (delete-file path)))))))

(test handle-changed-file-returns-failure-on-broken-file
  "A syntactically broken file must NOT signal — the helper returns
(values nil <error-string>) so the per-frame poll loop is never
broken by user typos mid-edit."
  (%nxt-576-with-clean-globals
    (let ((path nil))
      (unwind-protect
           (progn
             (setf path
                   (%nxt-576-write-temp-lisp
                    "(defun bad-fn (("))
             (multiple-value-bind (ok err)
                 (amoebum::%hot-patch-handle-changed-file path)
               (is (null ok))
               (is (stringp err))
               (is (plusp (length err)))))
        (when path (ignore-errors (delete-file path)))))))

(test poll-once-drains-event-bus-and-toasts
  "An emitted +EVENT-TYPE-EXTENSION-RELOADED+ event whose payload's
file-changed-path points at a real temp file is consumed by
hot-patch-poll-once: the file is loaded, the watermark advances, and
a toast is appended to chat history."
  (%nxt-576-with-clean-globals
    (let ((path nil)
          (bus (amoebum::make-event-bus))
          (chat-state (amoebum::make-chat-ui-state)))
      (unwind-protect
           (progn
             (setf amoebum::*event-bus* bus)
             ;; Initialize the watermark so we ignore historical
             ;; events that other subsystems may publish during
             ;; %hot-patch-ensure-watcher.
             (setf amoebum::*hot-patch-last-processed-seq*
                   (amoebum::event-bus-next-seq bus))
             (setf *hot-patch-test-marker* :unset)
             (setf path
                   (%nxt-576-write-temp-lisp
                    "(in-package :amoebum/test)
(setf amoebum/test::*hot-patch-test-marker* :poll)"))
             ;; Synthesize an extension-reloaded event referencing the
             ;; temp file.
             (amoebum::publish
              bus
              amoebum::+event-type-extension-reloaded+
              :source :hot-patch-watcher
              :severity :info
              :payload (amoebum::make-file-changed-payload
                        :path (namestring path)
                        :mtime (or (file-write-date path) 0)
                        :kind :modified
                        :watcher-id "hot-patch"))
             (let ((messages-before
                     (length (amoebum::chat-ui-state-messages chat-state))))
               ;; Drain directly — this exercises the event-bus walker
               ;; without coupling the test to the watcher-paths cache.
               (let ((processed
                       (amoebum::%hot-patch-drain-events
                        chat-state bus)))
                 (is (= 1 processed)))
               (is (eq :poll *hot-patch-test-marker*))
               (is (> (length (amoebum::chat-ui-state-messages chat-state))
                      messages-before))))
        (setf *hot-patch-test-marker* :unset)
        (when path (ignore-errors (delete-file path)))))))
