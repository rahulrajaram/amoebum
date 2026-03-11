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
