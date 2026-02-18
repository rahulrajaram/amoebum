(in-package :pseudopod)

;;; ---------------------------------------------------------------------------
;;; I108: String-replace edit primitive
;;;
;;; Provides a deterministic string-replace operation for file editing.
;;; Performs exact string matching (not regex) with clear behavior for
;;; single-match, no-match, and multi-match cases.
;;; ---------------------------------------------------------------------------

;;; ---- Condition types ----

(define-condition pseudopod-edit-error (pseudopod-error)
  ((path :initarg :path
         :initform nil
         :reader pseudopod-edit-error-path))
  (:report (lambda (condition stream)
             (format stream "Edit error~@[ for ~A~]: ~A"
                     (pseudopod-edit-error-path condition)
                     (or (pseudopod-error-message condition)
                         "unknown edit error")))))

(define-condition pseudopod-edit-no-match (pseudopod-edit-error)
  ()
  (:report (lambda (condition stream)
             (format stream "No match found~@[ in ~A~]: ~A"
                     (pseudopod-edit-error-path condition)
                     (or (pseudopod-error-message condition)
                         "old-string not found in file")))))

(define-condition pseudopod-edit-ambiguous (pseudopod-edit-error)
  ((match-count :initarg :match-count
                :initform 0
                :reader pseudopod-edit-ambiguous-match-count))
  (:report (lambda (condition stream)
             (format stream "Ambiguous match~@[ in ~A~]: ~D occurrences found. ~A"
                     (pseudopod-edit-error-path condition)
                     (pseudopod-edit-ambiguous-match-count condition)
                     (or (pseudopod-error-message condition)
                         "Use :replace-all t to replace all occurrences.")))))

;;; ---- Result struct ----

(defstruct (string-replace-result (:constructor %make-string-replace-result))
  "Metadata returned after a string-replace edit operation."
  (path         nil :type (or null pathname string))
  (match-count  0   :type integer)
  (replaced     0   :type integer)
  (old-string   ""  :type string)
  (new-string   ""  :type string))

;;; ---- Internal helpers ----

(defun %count-occurrences (haystack needle)
  "Count non-overlapping occurrences of NEEDLE in HAYSTACK.
Returns the count as an integer."
  (let ((needle-len (length needle))
        (count 0)
        (start 0))
    (when (zerop needle-len)
      (return-from %count-occurrences 0))
    (loop
      (let ((pos (search needle haystack :start2 start)))
        (unless pos (return count))
        (incf count)
        (setf start (+ pos needle-len))))))

(defun %replace-first (haystack old-string new-string)
  "Replace the first occurrence of OLD-STRING in HAYSTACK with NEW-STRING.
Returns the new string."
  (let ((pos (search old-string haystack)))
    (if pos
        (concatenate 'string
                     (subseq haystack 0 pos)
                     new-string
                     (subseq haystack (+ pos (length old-string))))
        haystack)))

(defun %replace-all-occurrences (haystack old-string new-string)
  "Replace all non-overlapping occurrences of OLD-STRING in HAYSTACK with NEW-STRING.
Returns the new string."
  (let ((old-len (length old-string)))
    (when (zerop old-len)
      (return-from %replace-all-occurrences haystack))
    (with-output-to-string (out)
      (let ((start 0))
        (loop
          (let ((pos (search old-string haystack :start2 start)))
            (unless pos
              (write-string (subseq haystack start) out)
              (return))
            (write-string (subseq haystack start pos) out)
            (write-string new-string out)
            (setf start (+ pos old-len))))))))

;;; ---- Public API ----

(defun string-replace-in-file (path old-string new-string
                                &key (replace-all nil) (encoding :utf-8))
  "Replace occurrences of OLD-STRING with NEW-STRING in the file at PATH.

Performs exact string matching (not regex).  Behavior:

  - Single match: replaces and writes back atomically.
  - No match: signals PSEUDOPOD-EDIT-NO-MATCH.
  - Multiple matches without :REPLACE-ALL: signals PSEUDOPOD-EDIT-AMBIGUOUS.
  - Multiple matches with :REPLACE-ALL T: replaces all and writes back atomically.

The write is atomic (temp file + rename), consistent with WRITE-FILE.

Returns a STRING-REPLACE-RESULT struct with match count and replacement details."
  (let* ((resolved (etypecase path
                     (pathname path)
                     (string (parse-namestring path))))
         (resolved (merge-pathnames resolved)))
    ;; Ensure file exists
    (unless (probe-file resolved)
      (error 'pseudopod-edit-no-match
             :path resolved
             :message (format nil "File not found: ~A" resolved)))
    ;; Validate old-string is non-empty
    (when (zerop (length old-string))
      (error 'pseudopod-edit-error
             :path resolved
             :message "old-string must not be empty"))
    ;; Read current content
    (let* ((content (uiop:read-file-string resolved))
           (match-count (%count-occurrences content old-string)))
      ;; Handle cases
      (cond
        ;; No match
        ((zerop match-count)
         (error 'pseudopod-edit-no-match
                :path resolved
                :message (format nil "old-string not found in ~A" resolved)))
        ;; Multiple matches without replace-all
        ((and (> match-count 1) (not replace-all))
         (error 'pseudopod-edit-ambiguous
                :path resolved
                :match-count match-count
                :message (format nil "~D occurrences found; use :replace-all t to replace all"
                                 match-count)))
        ;; Single match or replace-all
        (t
         (let ((new-content (if (and replace-all (> match-count 1))
                                (%replace-all-occurrences content old-string new-string)
                                (%replace-first content old-string new-string))))
           ;; Atomic write via write-file
           (write-file resolved new-content :encoding encoding)
           ;; Return result
           (%make-string-replace-result
            :path resolved
            :match-count match-count
            :replaced (if replace-all match-count 1)
            :old-string old-string
            :new-string new-string)))))))
