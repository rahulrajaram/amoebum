(in-package :amoebum)

;;;; ---------------------------------------------------------------------------
;;;; Built-in keymaps.
;;;;
;;;; Canonical keymaps shipped with amoebum: `chat-mode`, `command-mode`,
;;;; `diff-mode`. Each is a placeholder that application code replaces with
;;;; real handlers; they exist here so that the keymap registry is never
;;;; empty and so the default activation step has something to push.
;;;;
;;;; `activate-default-keymaps` is the composable public entry point used by
;;;; tests and boot sequences. The trailing eval-when only kicks in during
;;;; initial image load, when `*keymap-stack*` has not yet been populated.
;;;;
;;;; Behavior is preserved verbatim from the original
;;;; `amoebum/src/macros/defkeys.lisp`; only file boundaries change.
;;;; ---------------------------------------------------------------------------

(defkeys chat-mode
  "Chat mode default keymap."
  ("RET" state :description "Submit or newline handler placeholder.")
  ("C-c" state :description "Quit placeholder.")
  ("TAB" state :description "Completion placeholder."))

(defkeys command-mode
  "Command mode keymap."
  ("ESC" state :description "Exit command mode placeholder.")
  (":" state :description "Enter command prefix placeholder.")
  ("RET" state :description "Execute command placeholder."))

(defkeys diff-mode
  "Diff mode keymap."
  ("up" state :description "Move diff selection up.")
  ("down" state :description "Move diff selection down.")
  ("q" state :description "Exit diff view placeholder."))

(defun activate-default-keymaps ()
  (reset-keymap-stack)
  (push-keymap 'chat-mode)
  *keymap-stack*)

(eval-when (:load-toplevel :execute)
  (when (null *keymap-stack*)
    (activate-default-keymaps)))
