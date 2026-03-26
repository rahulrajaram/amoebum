(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Output-Style Appearance Tests  (NXT-091)
;;;
;;; Snapshot tests for the three status-bar output-style presets:
;;;   :compact   -- minimal, short labels
;;;   :operator  -- balanced, default style
;;;   :verbose   -- detailed, full tool results
;;;
;;; The status-bar line is wide enough to include the :output-mode segment
;;; (which carries the style label) so snapshots for each preset differ.
;;; Formatter regression tests verify that switching styles changes the line.
;;; ---------------------------------------------------------------------------

(def-suite output-style-appearance-suite :in amoebum-suite
  :description "TUI snapshot tests for compact, operator, and verbose output-style presets.")

(in-suite output-style-appearance-suite)

;;; ---------------------------------------------------------------------------
;;; Snapshot path helpers
;;; ---------------------------------------------------------------------------

(defun %output-style-snapshot-path (name)
  (merge-pathnames (pathname (format nil "~A.snap" name))
                   +chat-snapshot-dir*))

(defun %assert-output-style-snapshot (buffer name)
  (multiple-value-bind (pass diff)
      (ptui.test-support.harness:assert-snapshot
       buffer
       (%output-style-snapshot-path name)
       :test-name (string-capitalize (princ-to-string name)))
    (is (eq t pass))
    (is (null diff))))

;;; ---------------------------------------------------------------------------
;;; Shared test helpers
;;; ---------------------------------------------------------------------------

;;; Width chosen so that the :output-mode segment ("mode arch/compact" etc.)
;;; is always visible and distinguishes the three style snapshots.
(defparameter +output-style-snapshot-cols+ 160)

(defun %output-style-status-bar-state (&key (style :operator)
                                            (branch-name "feature/nxt-091")
                                            (model-name "moonshot-v1-128k")
                                            (context-used 8192)
                                            (context-max 128000))
  "Build a status-bar-state wired to STYLE with a fixed set of field values
so that every snapshot is deterministic."
  (let* ((event-bus (amoebum:make-event-bus :capacity 16))
         (state (amoebum.ui:make-status-bar-state
                 :branch-name branch-name
                 :model-name model-name
                 :permission-mode :supervised
                 :focus-mode :arch
                 :output-style style
                 :context-window-limit context-max
                 :event-bus event-bus)))
    (setf (amoebum::status-bar-state-context-used-tokens state) context-used
          (amoebum::status-bar-state-context-max-tokens  state) context-max)
    state))

(defun %output-style-status-line-buffer (style &key (cols +output-style-snapshot-cols+))
  "Render the status-bar line for STYLE into a 1-row buffer of width COLS.
COLS defaults to +output-style-snapshot-cols+ (160) so the :output-mode
segment, which encodes the style name, is fully visible."
  (let* ((state (%output-style-status-bar-state :style style))
         (buffer (ptui.render.buffer:make-buffer cols 1)))
    (ptui.render.buffer:buffer-draw-text
     buffer
     0
     0
     (amoebum.ui:status-bar-line state :width cols))
    buffer))

(defun %output-style-full-ui-buffer (style &key (cols 84) (rows 20))
  "Render the complete chat UI with the given output-style applied to the
 status-bar, using a small deterministic conversation."
  (with-safe-chat-env
    (let* ((status-state (%output-style-status-bar-state :style style))
           (state (amoebum.ui:make-chat-ui-state
                   :status-bar-state status-state)))
      ;; Detach tree browser so filesystem layout does not affect snapshots.
      (setf (amoebum::chat-ui-state-tree-browser-state state)
            (amoebum::make-empty-tree-browser-state :label "files"))
      (setf (amoebum::tree-browser-state-active-p
             (amoebum::chat-ui-state-tree-browser-state state))
            nil)
      (amoebum:chat-ui-add-message state "user"      "Describe the architecture.")
      (amoebum:chat-ui-add-message state "assistant" "The system uses four ASDF subsystems.")
      (%safe-render-chat-ui state :cols cols :rows rows))))

;;; ---------------------------------------------------------------------------
;;; Status-line buffer snapshots — one per style
;;;
;;; These use a 160-column buffer so the trailing "mode arch/<style>" segment
;;; is always included, making each snapshot unique.
;;; ---------------------------------------------------------------------------

(test output-style-appearance-compact-status-line
  "Status bar rendered with :compact output-style (160-col buffer)."
  (let ((buffer (%output-style-status-line-buffer :compact)))
    (%assert-output-style-snapshot buffer "output-style-compact-status-line")))

(test output-style-appearance-operator-status-line
  "Status bar rendered with :operator output-style (160-col buffer)."
  (let ((buffer (%output-style-status-line-buffer :operator)))
    (%assert-output-style-snapshot buffer "output-style-operator-status-line")))

(test output-style-appearance-verbose-status-line
  "Status bar rendered with :verbose output-style (160-col buffer)."
  (let ((buffer (%output-style-status-line-buffer :verbose)))
    (%assert-output-style-snapshot buffer "output-style-verbose-status-line")))

;;; ---------------------------------------------------------------------------
;;; Full chat-UI buffer snapshots — one per style (84x20)
;;; ---------------------------------------------------------------------------

(test output-style-appearance-compact-full-ui
  "Full chat UI with :compact output-style applied."
  (let ((buffer (%output-style-full-ui-buffer :compact)))
    (%assert-output-style-snapshot buffer "output-style-compact-full-ui")))

(test output-style-appearance-operator-full-ui
  "Full chat UI with :operator output-style applied."
  (let ((buffer (%output-style-full-ui-buffer :operator)))
    (%assert-output-style-snapshot buffer "output-style-operator-full-ui")))

(test output-style-appearance-verbose-full-ui
  "Full chat UI with :verbose output-style applied."
  (let ((buffer (%output-style-full-ui-buffer :verbose)))
    (%assert-output-style-snapshot buffer "output-style-verbose-full-ui")))

;;; ---------------------------------------------------------------------------
;;; Formatter regression: switching styles changes the rendered line
;;;
;;; Uses status-bar-line without a :width cap so the full line is compared,
;;; guaranteeing the :output-mode segment is always present.
;;; ---------------------------------------------------------------------------

(test output-style-appearance-switching-changes-render
  "Switching output-style via status-bar-set-output-style! produces a
different full status-bar line each time."
  (let ((state (%output-style-status-bar-state :style :operator)))
    ;; Capture the full (untruncated) line for each style.
    (let ((line-operator (amoebum.ui:status-bar-line state)))
      (amoebum.ui:status-bar-set-output-style! state :compact)
      (let ((line-compact (amoebum.ui:status-bar-line state)))
        (amoebum.ui:status-bar-set-output-style! state :verbose)
        (let ((line-verbose (amoebum.ui:status-bar-line state)))
          (is (stringp line-operator))
          (is (stringp line-compact))
          (is (stringp line-verbose))
          (is (not (string= line-operator line-compact))
              "operator and compact lines must differ")
          (is (not (string= line-operator line-verbose))
              "operator and verbose lines must differ")
          (is (not (string= line-compact line-verbose))
              "compact and verbose lines must differ"))))))

(test output-style-appearance-output-mode-segment-reflects-style
  "The :output-mode status-bar segment text changes when output-style changes."
  (let ((state (%output-style-status-bar-state :style :compact)))
    (let ((segments-compact (amoebum.ui:status-bar-segments state)))
      (amoebum.ui:status-bar-set-output-style! state :verbose)
      (let ((segments-verbose (amoebum.ui:status-bar-segments state)))
        (is (not (equal segments-compact segments-verbose))
            "segment list must change when output-style is switched")))))
