(in-package :amoebum)

;;; NXT-575: REPL panel state.
;;;
;;; Pure-CL state struct used by panels/repl-panel.lisp. The submit helper is
;;; the only side-effecting entry point: it routes the current input through
;;; `sandboxed-eval` (defined in src/self-modify.lisp) so the same denylist
;;; (DELETE-FILE, OPEN, RUN-PROGRAM, ...) that gates /self-modify and
;;; /deftool also gates the chat REPL. No render-layer dependencies.

(defstruct (repl-state (:constructor %make-repl-state))
  (active-p nil :type boolean)
  (input-text "" :type string)
  (cursor-position 0 :type fixnum)
  ;; History is most-recent-first; each entry is a plist
  ;; (:input STR :output STR-OR-NIL :error STR-OR-NIL).
  (history '() :type list)
  (history-cursor 0 :type fixnum)
  (scroll-offset 0 :type fixnum)
  (max-history-entries 200 :type fixnum))

(defun make-repl-state ()
  "Construct a fresh REPL panel state (inactive, empty)."
  (%make-repl-state))

(defun repl-state-toggle! (state)
  "Flip the REPL panel active flag and return STATE."
  (setf (repl-state-active-p state) (not (repl-state-active-p state)))
  state)

(defun %repl-format-result (value)
  "Render an evaluated value as a printable string."
  (handler-case (princ-to-string value)
    (error (c) (format nil "<unprintable result: ~A>" c))))

(defun %repl-cap-history! (state)
  (let ((cap (repl-state-max-history-entries state)))
    (when (and (plusp cap)
               (> (length (repl-state-history state)) cap))
      (setf (repl-state-history state)
            (subseq (repl-state-history state) 0 cap)))))

(defun repl-state-submit-input! (state)
  "Evaluate the current input via `sandboxed-eval`, push a history entry,
clear the input field, and return the rendered output string (or error
message). Returns NIL when the input is blank."
  (let* ((text (repl-state-input-text state))
         (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
    (when (zerop (length trimmed))
      (return-from repl-state-submit-input! nil))
    (multiple-value-bind (value errorp error-msg)
        (sandboxed-eval trimmed)
      (let* ((output (and (not errorp) (%repl-format-result value)))
             (error-text (and errorp (or error-msg "evaluation error"))))
        (push (list :input text :output output :error error-text)
              (repl-state-history state))
        (%repl-cap-history! state)
        (setf (repl-state-input-text state) ""
              (repl-state-cursor-position state) 0
              (repl-state-history-cursor state) 0
              (repl-state-scroll-offset state) 0)
        (or output error-text)))))

(defun repl-state-append-text! (state text)
  "Append TEXT at the current cursor position."
  (when (and (stringp text) (plusp (length text)))
    (let* ((current (repl-state-input-text state))
           (pos (min (max 0 (repl-state-cursor-position state))
                     (length current))))
      (setf (repl-state-input-text state)
            (concatenate 'string
                         (subseq current 0 pos)
                         text
                         (subseq current pos))
            (repl-state-cursor-position state) (+ pos (length text)))))
  state)

(defun repl-state-backspace! (state)
  "Delete the grapheme to the left of the cursor."
  (let* ((current (repl-state-input-text state))
         (pos (min (max 0 (repl-state-cursor-position state))
                   (length current))))
    (when (plusp pos)
      (setf (repl-state-input-text state)
            (concatenate 'string
                         (subseq current 0 (1- pos))
                         (subseq current pos))
            (repl-state-cursor-position state) (1- pos))))
  state)

(defun repl-state-clear-input! (state)
  (setf (repl-state-input-text state) ""
        (repl-state-cursor-position state) 0)
  state)
