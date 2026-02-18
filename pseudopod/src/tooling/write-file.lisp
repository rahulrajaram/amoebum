(in-package :pseudopod)

;;; ---- I106: Whole-file write primitive ----
;;;
;;; Atomic write: content -> temp file (same directory) -> rename to target.
;;; This ensures the target path is never left in a partially-written state.

(define-condition pseudopod-write-error (pseudopod-error)
  ((path :initarg :path
         :initform nil
         :reader pseudopod-write-error-path))
  (:report (lambda (condition stream)
             (format stream "Write failed for ~A: ~A"
                     (or (pseudopod-write-error-path condition) "<unknown>")
                     (or (pseudopod-error-message condition) "unknown error")))))

(defstruct (write-result (:constructor %make-write-result))
  "Metadata returned after a successful whole-file write."
  (path       nil :type (or null pathname string))
  (bytes      0   :type integer)
  (created-p  nil :type boolean)
  (encoding   :utf-8 :type keyword))

(defun write-file (path content &key (encoding :utf-8) (if-exists :supersede))
  "Write CONTENT to PATH atomically.

CONTENT is a string.  ENCODING defaults to :UTF-8.
IF-EXISTS may be :SUPERSEDE (default, overwrites) or :ERROR (signals if file exists).

The write is atomic: content is first written to a temporary file in the same
directory, then renamed to the target path.  This guarantees that readers never
see a partially-written file.

Returns a WRITE-RESULT struct with metadata about the operation."
  (let* ((target (etypecase path
                   (pathname path)
                   (string (parse-namestring path))))
         (target (merge-pathnames target))
         (dir (uiop:pathname-directory-pathname target))
         (existed-p (uiop:file-exists-p target))
         (temp-path nil))
    ;; Validate encoding
    (unless (member encoding '(:utf-8 :ascii :latin-1 :iso-8859-1) :test #'eq)
      (error 'pseudopod-write-error
             :path target
             :message (format nil "Unsupported encoding: ~A" encoding)))
    ;; Check if-exists semantics
    (when (and existed-p (eq if-exists :error))
      (error 'pseudopod-write-error
             :path target
             :message (format nil "File already exists: ~A" target)))
    ;; Ensure target directory exists
    (ensure-directories-exist dir)
    ;; Write to temp file in the same directory, then rename
    (unwind-protect
        (progn
          (setf temp-path
                (merge-pathnames
                 (format nil ".pseudopod-write-~A-~A.tmp"
                         (get-universal-time) (random 1000000))
                 dir))
          (with-open-file (stream temp-path
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create
                                  :external-format encoding
                                  :element-type 'character)
            (write-string content stream)
            (finish-output stream))
          ;; Atomic rename
          (rename-file temp-path target)
          ;; temp-path was successfully renamed; clear so unwind-protect
          ;; cleanup does not try to delete a now-nonexistent file.
          (setf temp-path nil))
      ;; Cleanup: remove temp file if rename did not happen
      (when (and temp-path (uiop:file-exists-p temp-path))
        (handler-case (cl:delete-file temp-path)
          (error () nil))))
    ;; Return metadata
    (let ((stat-size (or (ignore-errors
                          (with-open-file (s target :direction :input)
                            (file-length s)))
                         0)))
      (%make-write-result
       :path target
       :bytes stat-size
       :created-p (not existed-p)
       :encoding encoding))))
