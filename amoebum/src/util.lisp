(in-package :amoebum)

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Shared utility functions — single canonical implementations
;;;; ─────────────────────────────────────────────────────────────────────────

;;; ── Name normalization ──────────────────────────────────────────────────

(defun normalize-name (value)
  "Lowercase, trim, type-coerced name normalization.
Accepts strings, symbols, characters, and other printable values.
Returns a lowercase, whitespace-trimmed string."
  (string-downcase
   (string-trim '(#\Space #\Tab #\Newline #\Return)
                (typecase value
                  (string value)
                  (symbol (symbol-name value))
                  (character (string value))
                  (t (princ-to-string value))))))

;;; ── Path coercion ───────────────────────────────────────────────────────

(defun coerce-path-string (path)
  "Coerce PATH to its string representation.
Handles pathnames, strings, symbols, and arbitrary printable values."
  (typecase path
    (pathname (namestring path))
    (string path)
    (symbol (symbol-name path))
    (t (princ-to-string path))))

;;; ── Monotonic time ──────────────────────────────────────────────────────

(defun monotonic-ms ()
  "Return monotonic milliseconds from the process clock."
  (truncate (* 1000
               (/ (coerce (get-internal-real-time) 'double-float)
                  (coerce internal-time-units-per-second 'double-float)))))

(defun monotonic-seconds ()
  "Return monotonic seconds as a double-float."
  (/ (coerce (get-internal-real-time) 'double-float)
     internal-time-units-per-second))

;;; ── Registry DSL ─────────────────────────────────────────────────────────

(defmacro defregistry (name &key (test 'equal) (key-fn 'normalize-name))
  "Define a named registry with hash table, register, find, remove, and list.
Generates:
  *NAME-registry*        — the hash table
  register-NAME-entry    — (register-NAME-entry key value)
  find-NAME-entry        — (find-NAME-entry key) → value or nil
  remove-NAME-entry      — (remove-NAME-entry key)
  list-NAME-entries      — (list-NAME-entries) → sorted alist
  clear-NAME-registry    — (clear-NAME-registry)"
  (let* ((prefix (string-upcase (string name)))
         (registry-var (intern (format nil "*~A-REGISTRY*" prefix)))
         (register-fn (intern (format nil "REGISTER-~A-ENTRY" prefix)))
         (find-fn (intern (format nil "FIND-~A-ENTRY" prefix)))
         (remove-fn (intern (format nil "REMOVE-~A-ENTRY" prefix)))
         (list-fn (intern (format nil "LIST-~A-ENTRIES" prefix)))
         (clear-fn (intern (format nil "CLEAR-~A-REGISTRY" prefix))))
    `(progn
       (defparameter ,registry-var (make-hash-table :test #',test))
       (defun ,register-fn (key value)
         (setf (gethash (,key-fn key) ,registry-var) value))
       (defun ,find-fn (key)
         (values (gethash (,key-fn key) ,registry-var)))
       (defun ,remove-fn (key)
         (remhash (,key-fn key) ,registry-var))
       (defun ,list-fn ()
         (let ((entries '()))
           (maphash (lambda (k v) (push (cons k v) entries)) ,registry-var)
           entries))
       (defun ,clear-fn ()
         (clrhash ,registry-var)))))

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
