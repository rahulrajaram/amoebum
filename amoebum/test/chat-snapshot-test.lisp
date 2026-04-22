(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Chat UI Snapshot Tests (I140)
;;; ---------------------------------------------------------------------------

(def-suite chat-snapshot-suite :in amoebum-suite
  :description "Chat UI snapshot coverage for message area, tool preview, status, and empty states.")

(in-suite chat-snapshot-suite)

(defparameter +chat-snapshot-dir*
  (asdf:system-relative-pathname "amoebum" "test/snapshots/"))

(defun chat-snapshot-path (name)
  (merge-pathnames (pathname (format nil "~A.snap" name)) +chat-snapshot-dir*))

(defun %assert-chat-snapshot (buffer name)
  (multiple-value-bind (pass diff)
      (ptui.test-support.harness:assert-snapshot
       buffer
       (chat-snapshot-path name)
       :test-name (string-capitalize (princ-to-string name)))
    (is (eq t pass))
    (is (null diff))))

;;; ---------------------------------------------------------------------------
;;; Byte-identical snapshot ratchet (NXT-399)
;;;
;;; The %assert-chat-snapshot helper above goes through
;;; ptui.test-support.harness:assert-snapshot which uses a line-by-line
;;; comparison (snapshot-diff) and silently *creates* the golden file on
;;; first run. That is too lenient for an explicit invariant ratchet:
;;; deleting the golden would re-create it without warning, and trailing
;;; blank-row drift could go unnoticed.
;;;
;;; %assert-chat-snapshot-byte-identical asserts that the on-disk golden
;;; file matches the live render BYTE-FOR-BYTE (string equal on the full
;;; UTF-8 contents) and does NOT silently create a missing baseline. Any
;;; intentional update must be made explicit by setting the environment
;;; variable AMOEBUM_UPDATE_SNAPSHOTS=1 before re-running the test, in
;;; which case the golden file is rewritten and a clear "snapshot updated"
;;; message is emitted on *standard-output*.
;;;
;;; Wired into the ui focused profile (CHAT-SNAPSHOT-SUITE) and the
;;; regression canary so NXT-384 / NXT-385 (and any future chat-render
;;; motion) cannot silently drift the rendered output.
;;; ---------------------------------------------------------------------------

(defun %amoebum-update-snapshots-p ()
  (let ((env (uiop:getenv "AMOEBUM_UPDATE_SNAPSHOTS")))
    (and env (string= env "1"))))

(defun %read-golden-bytes (path)
  "Read the golden file at PATH as a UTF-8 string. Returns NIL when the
file does not exist."
  (let ((resolved (probe-file path)))
    (when resolved
      (uiop:read-file-string resolved :external-format :utf-8))))

(defun %write-golden-bytes (path content)
  "Write CONTENT to PATH as UTF-8 with LF line endings, no trailing
modifications."
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string content stream))
  path)

(defun %first-line-difference (expected actual)
  "Return a short human-readable description of the first line where
EXPECTED and ACTUAL strings diverge, or NIL if equal. Does NOT strip
trailing blank lines — every byte counts."
  (let ((expected-lines (uiop:split-string expected :separator '(#\Newline)))
        (actual-lines (uiop:split-string actual :separator '(#\Newline))))
    (let ((max-lines (max (length expected-lines) (length actual-lines))))
      (dotimes (i max-lines)
        (let ((exp-line (if (< i (length expected-lines))
                            (nth i expected-lines)
                            "<missing>"))
              (act-line (if (< i (length actual-lines))
                            (nth i actual-lines)
                            "<missing>")))
          (unless (string= exp-line act-line)
            (return-from %first-line-difference
              (format nil "Line ~D differs:~%  expected: ~S~%  actual:   ~S"
                      (1+ i) exp-line act-line))))))
    nil))

(defun %assert-chat-snapshot-byte-identical-update (path name live)
  (%write-golden-bytes path live)
  (format t "~&AMOEBUM_SNAPSHOT_UPDATED ~A chars=~D path=~A~%"
          name (length live) (namestring path))
  ;; Treat update mode as a passing assertion so the explicit refresh
  ;; round-trip does not break the suite.
  (is-true t
           "Snapshot ~A updated via AMOEBUM_UPDATE_SNAPSHOTS=1." name))

(defun %assert-chat-snapshot-byte-identical-compare (path name golden live)
  (let ((diff (%first-line-difference golden live)))
    (is (equal golden live)
        "AMOEBUM_SNAPSHOT_RATCHET_DRIFT ~A: byte-identical drift detected against ~A. ~
         golden=~D chars, live=~D chars. Re-run with AMOEBUM_UPDATE_SNAPSHOTS=1 to ~
         update the baseline if the change is intentional, then `git add` the snapshot. ~
         First diverging line:~%~A"
        name (namestring path) (length golden) (length live)
        (or diff "<no line-level diff; trailing-byte drift only>"))))

(defun %assert-chat-snapshot-byte-identical (buffer name)
  "Strict byte-identical ratchet against the checked-in golden file.

If AMOEBUM_UPDATE_SNAPSHOTS=1 is set in the environment, overwrites the
golden file with the live render and emits a clear notice on
*standard-output*. Otherwise asserts EQUAL on the full string contents,
or fails loudly with a clear remediation message when the golden file is
missing."
  (let* ((path (chat-snapshot-path name))
         (live (ptui.test-support.snapshot:buffer-to-snapshot buffer)))
    (cond
      ((%amoebum-update-snapshots-p)
       (%assert-chat-snapshot-byte-identical-update path name live))
      (t
       (let ((golden (%read-golden-bytes path)))
         (cond
           ((null golden)
            (is-true nil
                     "AMOEBUM_SNAPSHOT_RATCHET_MISSING ~A: no checked-in baseline at ~A. ~
                      Run with AMOEBUM_UPDATE_SNAPSHOTS=1 if creating a new ratchet is intentional, ~
                      then `git add` the resulting file."
                     name (namestring path)))
           (t
            (%assert-chat-snapshot-byte-identical-compare path name golden live)))))))
  (values))

(defun %snapshot-status-bar-state (&key
                                   (branch-name "main")
                                   (model-name "gpt-4o-mini")
                                   (context-window-limit 12000)
                                   stream-summary)
  (let* ((event-bus (amoebum:make-event-bus :capacity 16))
         (state (amoebum.ui:make-status-bar-state
                 :branch-name branch-name
                 :model-name model-name
                 :permission-mode :full-auto
                 :context-window-limit context-window-limit
                 :event-bus event-bus)))
    (when stream-summary
      (amoebum:publish-status-bar-stream-summary stream-summary
                                                :event-bus event-bus))
    state))

(defun %snapshot-chat-state (&key
                              messages
                              status-bar-state)
  (let ((*default-pathname-defaults*
          (pathname "/home/rahul/Documents/amoebum/"))
        (amoebum::*current-config* nil))
    (ignore-errors (amoebum::drain-voice-transcriptions))
    (let ((state (amoebum.ui:make-chat-ui-state
                  :status-bar-state (or status-bar-state
                                        (%snapshot-status-bar-state)))))
      (dolist (message messages)
        (amoebum:chat-ui-add-message state
                                     (first message)
                                     (second message)))
      state)))

(defun %render-chat-ui (state &key (cols 84) (rows 20))
  (amoebum:render-chat-ui-buffer
   state
   (ptui.core.types:make-size cols rows)))

(defun %status-line-buffer (status-state &key (cols 84))
  (let ((buffer (ptui.render.buffer:make-buffer cols 1)))
    (ptui.render.buffer:buffer-draw-text
     buffer
     0
     0
     (amoebum.ui:status-bar-line status-state))
    buffer))

(defun %chat-snapshot-message-area-buffer ()
  "Construct the canonical chat-snapshot-message-area render buffer.
This is the single source of truth for both the legacy line-by-line
assertion and the byte-identical ratchet (NXT-399) so they are
guaranteed to exercise the same render path."
  (let ((state (%snapshot-chat-state
                :messages '(("system" "System: respond concisely and cite sources only.")
                            ("user" "Can you summarize the module layout for the chat UI?")
                            ("assistant" "The chat UI uses a box container, status bar, message history, and prompt input."))
                :status-bar-state (%snapshot-status-bar-state :branch-name "feat/chat-snapshot"))))
    (%render-chat-ui state :cols 84 :rows 20)))

(test chat-snapshot-message-area
  (let ((buffer (%chat-snapshot-message-area-buffer)))
    ;; NXT-399: enforce byte-identical drift detection against the
    ;; checked-in baseline FIRST so chat-render motion (NXT-384/NXT-385
    ;; et al) cannot silently change the rendered output. We run this
    ;; before the legacy %assert-chat-snapshot because that helper
    ;; silently re-creates a missing golden on first run, which would
    ;; mask a deletion of the baseline. Update via
    ;; AMOEBUM_UPDATE_SNAPSHOTS=1 when intentional.
    (%assert-chat-snapshot-byte-identical buffer "message-area")
    (%assert-chat-snapshot buffer "message-area")))

(test chat-snapshot-message-area-byte-identical-ratchet
  ;; NXT-399: dedicated, named ratchet test. Asserts that the live
  ;; render of the canonical message-area scenario is BYTE-IDENTICAL to
  ;; the checked-in golden file at amoebum/test/snapshots/message-area.snap.
  ;; Any drift fails this test with a clear remediation message
  ;; pointing at AMOEBUM_UPDATE_SNAPSHOTS=1. This is the explicit
  ;; invariant ratchet covering the chat UI message-area render path.
  (%assert-chat-snapshot-byte-identical
   (%chat-snapshot-message-area-buffer)
   "message-area"))

(test chat-snapshot-tool-call-preview
  (let* ((state (%snapshot-chat-state
                 :messages '(("assistant" "I will check symbols in the current tree."))
                 :status-bar-state (%snapshot-status-bar-state :branch-name "feat/chat-snapshot")))
         (tool-calls (amoebum.ui:chat-ui-state-stream-tool-calls state)))
    (setf (gethash :preview tool-calls)
          (list :key :preview
                :tool-name "search_symbols"
                :arguments "{\"query\":\"chat\", \"limit\": 3}"))
    (%assert-chat-snapshot (%render-chat-ui state :cols 84 :rows 20)
                           "tool-call-preview")))

(test chat-snapshot-status-bar
  (let* ((status-state (%snapshot-status-bar-state
                       :branch-name "feat/chat-snapshot"
                       :model-name "gpt-4o"
                       :stream-summary (list :status :running
                                            :tokens 512
                                            :tokens-per-second 6.25d0
                                            :activep t)))
         (buffer (%status-line-buffer status-state :cols 84)))
    (%assert-chat-snapshot buffer "status-bar")))

(test chat-snapshot-empty-state
  (let* ((state (%snapshot-chat-state
                 :status-bar-state (%snapshot-status-bar-state :branch-name "feat/chat-snapshot")))
         (buffer (%render-chat-ui state :cols 84 :rows 20)))
    (%assert-chat-snapshot buffer "empty-state")))

(test chat-snapshot-exit-warning-row
  (let* ((state (%snapshot-chat-state
                 :status-bar-state (%snapshot-status-bar-state :branch-name "feat/chat-snapshot")))
         (ignore
           (setf (amoebum::chat-ui-state-ctrl-c-quit-armed-at-ms state)
                 (ptui.util.time:monotonic-ms))))
    (%assert-chat-snapshot (%render-chat-ui state :cols 84 :rows 20)
                           "exit-warning-row")))
