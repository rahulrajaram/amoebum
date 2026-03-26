(in-package :pseudopod)

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Pseudopod utility functions
;;;; ─────────────────────────────────────────────────────────────────────────

;;; ── ANSI Escape Code Sanitization ─────────────────────────────────────────

(defun sanitize-ansi-escapes (text)
  "Remove ANSI escape codes from TEXT.
Handles standard CSI sequences (ESC [ ... m) and OSC sequences (ESC ] ... BEL).
This prevents 'invalid character \\x1b' errors when sending tool output to LLM APIs.
See: https://en.wikipedia.org/wiki/ANSI_escape_code"
  (if (stringp text)
      ;; Remove CSI sequences: ESC [ followed by any number of parameter bytes (0x30-0x3F),
      ;; intermediate bytes (0x20-0x2F), and ending with a final byte (0x40-0x7E)
      ;; Common case: ESC [ (0-9;)* m  (SGR - Select Graphic Rendition)
      ;; Also handle OSC sequences: ESC ] ... BEL (or ESC \\)
      (let* ((result text)
             ;; Pattern 1: CSI sequences ESC [ ... letter
             ;; Matches things like \x1b[32m (green), \x1b[0m (reset), etc.
             (result (cl-ppcre:regex-replace-all "\\x1b\\[[0-9;]*[A-Za-z]" result ""))
             ;; Pattern 2: OSC sequences ESC ] ... BEL
             (result (cl-ppcre:regex-replace-all "\\x1b\\][^\\x07]*\\x07" result ""))
             ;; Pattern 3: OSC sequences ESC ] ... ESC \\ (ST - String Terminator)
             (result (cl-ppcre:regex-replace-all "\\x1b\\][^\\x1b]*\\x1b\\\\" result ""))
             ;; Pattern 4: Simple escape sequences (like ESC c - reset)
             (result (cl-ppcre:regex-replace-all "\\x1b[^\\x5b\\x5d]" result "")))
        result)
      text))

(defun sanitize-string-for-llm (value)
  "Convert VALUE to a string and sanitize it for safe LLM consumption.
Removes ANSI escape codes and ensures valid Unicode."
  (let ((string (typecase value
                  (string value)
                  (null "")
                  (t (princ-to-string value)))))
    (sanitize-ansi-escapes string)))
