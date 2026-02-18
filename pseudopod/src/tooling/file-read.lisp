(in-package :pseudopod)

;;; ---------------------------------------------------------------------------
;;; I103: Basic file read primitive
;;;
;;; Provides a baseline file-read primitive for full-file reads with
;;; deterministic text decoding (UTF-8 with fallback to Latin-1).
;;; ---------------------------------------------------------------------------

;;; ---- Condition types ----

(define-condition pseudopod-file-error (pseudopod-error)
  ((path :initarg :path
         :initform nil
         :reader pseudopod-file-error-path))
  (:report (lambda (condition stream)
             (format stream "File error~@[ for ~A~]: ~A"
                     (pseudopod-file-error-path condition)
                     (or (pseudopod-error-message condition)
                         "unknown file error")))))

(define-condition pseudopod-file-not-found (pseudopod-file-error)
  ()
  (:report (lambda (condition stream)
             (format stream "File not found: ~A"
                     (or (pseudopod-file-error-path condition) "<unknown>")))))

;;; ---- File read result struct ----

(defstruct (file-read-result (:constructor %make-file-read-result))
  "Result of reading a file from disk."
  (content "" :type string)
  (path nil)
  (size 0 :type integer)
  (modification-time nil)
  (encoding :utf-8 :type keyword))

;;; ---- Internal helpers ----

(defun %resolve-file-path (path)
  "Resolve PATH to an absolute namestring. Accepts strings and pathnames."
  (let ((pathname (etypecase path
                    (pathname path)
                    (string (parse-namestring path)))))
    (namestring (merge-pathnames pathname))))

(defun %file-modification-time (path)
  "Return the file-write-date for PATH as a universal time, or NIL on failure."
  (handler-case (file-write-date path)
    (error () nil)))

(defun %read-file-bytes (path)
  "Read the entire file at PATH into an octet vector."
  (with-open-file (stream path
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (let* ((size (file-length stream))
           (buffer (make-array size :element-type '(unsigned-byte 8))))
      (read-sequence buffer stream)
      buffer)))

(defun %decode-utf-8 (octets)
  "Attempt to decode OCTETS as UTF-8. Returns (VALUES string :UTF-8) on success,
or NIL on failure."
  (handler-case
      (values (babel:octets-to-string octets :encoding :utf-8) :utf-8)
    (error () nil)))

(defun %decode-latin-1 (octets)
  "Decode OCTETS as Latin-1 (ISO-8859-1). Always succeeds since every byte
maps to a valid code point. Returns (VALUES string :LATIN-1)."
  (values (babel:octets-to-string octets :encoding :latin-1) :latin-1))

(defun %decode-file-bytes (octets)
  "Deterministic text decoding: try UTF-8 first, fall back to Latin-1.
Returns (VALUES decoded-string encoding-keyword)."
  (multiple-value-bind (text encoding) (%decode-utf-8 octets)
    (if text
        (values text encoding)
        (%decode-latin-1 octets))))

;;; ---- Public API ----

(defun read-file (path)
  "Read the file at PATH and return a FILE-READ-RESULT.

Reads the entire file, decodes its contents using deterministic text
decoding (UTF-8 with Latin-1 fallback), and returns a result struct
containing the content, metadata (file size, modification time), and
the encoding used.

Signals PSEUDOPOD-FILE-NOT-FOUND (with USE-VALUE restart) if the file
does not exist."
  (let ((resolved (%resolve-file-path path)))
    (restart-case
        (unless (probe-file resolved)
          (error 'pseudopod-file-not-found
                 :path resolved
                 :message (format nil "File not found: ~A" resolved)))
      (use-value (alternative-path)
        :report "Supply an alternative file path."
        :interactive (lambda ()
                       (format *query-io* "~&Alternative path: ")
                       (force-output *query-io*)
                       (list (read-line *query-io*)))
        (return-from read-file (read-file alternative-path))))
    (let* ((truename (truename resolved))
           (octets (%read-file-bytes truename))
           (size (length octets))
           (mtime (%file-modification-time truename)))
      (multiple-value-bind (content encoding) (%decode-file-bytes octets)
        (%make-file-read-result
         :content content
         :path (namestring truename)
         :size size
         :modification-time mtime
         :encoding encoding)))))
