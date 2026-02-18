(in-package :pseudopod/test)

;;; ---------------------------------------------------------------------------
;;; I103: File Read Primitive Tests
;;; ---------------------------------------------------------------------------

(def-suite file-read-suite :in pseudopod-suite
  :description "File read primitive tests (I103).")

(in-suite file-read-suite)

;;; ---- Helpers ----

(defun make-temp-file (content &key (prefix "pseudopod-i103-test-"))
  "Create a temporary file with CONTENT and return its path as a namestring."
  (let ((path (merge-pathnames
               (format nil "~A~D.txt" prefix (random 1000000))
               (uiop:temporary-directory))))
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type 'character
                            :external-format :utf-8)
      (write-string content stream))
    (namestring path)))

(defun make-temp-binary-file (octets &key (prefix "pseudopod-i103-bin-"))
  "Create a temporary binary file with OCTETS and return its path."
  (let ((path (merge-pathnames
               (format nil "~A~D.bin" prefix (random 1000000))
               (uiop:temporary-directory))))
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
      (write-sequence octets stream))
    (namestring path)))

(defun cleanup-temp-file (path)
  "Delete a temporary file, ignoring errors."
  (when (and path (probe-file path))
    (handler-case (delete-file path)
      (error () nil))))

;;; ---- Tests: Successful file read ----

(test file-read-success-basic
  "Read a simple UTF-8 text file and verify content."
  (let ((path nil))
    (unwind-protect
        (progn
          (setf path (make-temp-file "Hello, pseudopod!"))
          (let ((result (pseudopod:read-file path)))
            (is-true (pseudopod:file-read-result-p result))
            (is (string= "Hello, pseudopod!"
                         (pseudopod:file-read-result-content result)))))
      (cleanup-temp-file path))))

(test file-read-empty-file
  "Read an empty file and verify empty content with zero size."
  (let ((path nil))
    (unwind-protect
        (progn
          (setf path (make-temp-file ""))
          (let ((result (pseudopod:read-file path)))
            (is-true (pseudopod:file-read-result-p result))
            (is (string= "" (pseudopod:file-read-result-content result)))
            (is (= 0 (pseudopod:file-read-result-size result)))))
      (cleanup-temp-file path))))

(test file-read-multiline
  "Read a multiline file and verify content integrity."
  (let ((path nil)
        (expected (format nil "Line 1~%Line 2~%Line 3~%")))
    (unwind-protect
        (progn
          (setf path (make-temp-file expected))
          (let ((result (pseudopod:read-file path)))
            (is (string= expected
                         (pseudopod:file-read-result-content result)))))
      (cleanup-temp-file path))))

(test file-read-unicode-content
  "Read a file with Unicode content (UTF-8)."
  (let ((path nil)
        (expected "Caf\u00e9 \u2014 \u00fcber \u2603 snowman"))
    (unwind-protect
        (progn
          (setf path (make-temp-file expected))
          (let ((result (pseudopod:read-file path)))
            (is (string= expected
                         (pseudopod:file-read-result-content result)))
            (is (eq :utf-8 (pseudopod:file-read-result-encoding result)))))
      (cleanup-temp-file path))))

;;; ---- Tests: Metadata correctness ----

(test file-read-metadata-size
  "Verify file size metadata matches actual byte count."
  (let ((path nil)
        (content "12345"))
    (unwind-protect
        (progn
          (setf path (make-temp-file content))
          (let ((result (pseudopod:read-file path)))
            ;; "12345" in UTF-8 is exactly 5 bytes
            (is (= 5 (pseudopod:file-read-result-size result)))))
      (cleanup-temp-file path))))

(test file-read-metadata-path
  "Verify the result path is an absolute namestring."
  (let ((path nil))
    (unwind-protect
        (progn
          (setf path (make-temp-file "path check"))
          (let* ((result (pseudopod:read-file path))
                 (result-path (pseudopod:file-read-result-path result)))
            (is-true (stringp result-path))
            (is-true (uiop:absolute-pathname-p (pathname result-path)))))
      (cleanup-temp-file path))))

(test file-read-metadata-modification-time
  "Verify modification time is a positive universal time."
  (let ((path nil))
    (unwind-protect
        (progn
          (setf path (make-temp-file "mtime check"))
          (let* ((result (pseudopod:read-file path))
                 (mtime (pseudopod:file-read-result-modification-time result)))
            (is-true (integerp mtime))
            (is-true (plusp mtime))))
      (cleanup-temp-file path))))

(test file-read-metadata-encoding
  "Verify encoding is reported as :UTF-8 for valid UTF-8 files."
  (let ((path nil))
    (unwind-protect
        (progn
          (setf path (make-temp-file "encoding check"))
          (let ((result (pseudopod:read-file path)))
            (is (eq :utf-8 (pseudopod:file-read-result-encoding result)))))
      (cleanup-temp-file path))))

;;; ---- Tests: Missing file ----

(test file-read-missing-file-signals-condition
  "Reading a non-existent file signals PSEUDOPOD-FILE-NOT-FOUND."
  (let ((bad-path "/tmp/pseudopod-i103-definitely-does-not-exist.txt"))
    (signals pseudopod:pseudopod-file-not-found
      (pseudopod:read-file bad-path))))

(test file-read-missing-file-is-file-error
  "PSEUDOPOD-FILE-NOT-FOUND is a subtype of PSEUDOPOD-FILE-ERROR."
  (let ((bad-path "/tmp/pseudopod-i103-definitely-does-not-exist-2.txt"))
    (signals pseudopod:pseudopod-file-error
      (pseudopod:read-file bad-path))))

(test file-read-missing-file-is-pseudopod-error
  "PSEUDOPOD-FILE-NOT-FOUND is a subtype of PSEUDOPOD-ERROR."
  (let ((bad-path "/tmp/pseudopod-i103-definitely-does-not-exist-3.txt"))
    (signals pseudopod:pseudopod-error
      (pseudopod:read-file bad-path))))

(test file-read-missing-file-use-value-restart
  "USE-VALUE restart on missing file redirects to alternative path."
  (let ((good-path nil)
        (bad-path "/tmp/pseudopod-i103-nonexistent-restart-test.txt"))
    (unwind-protect
        (progn
          (setf good-path (make-temp-file "recovered content"))
          (let ((result (handler-bind
                            ((pseudopod:pseudopod-file-not-found
                               (lambda (c)
                                 (declare (ignore c))
                                 (invoke-restart 'use-value good-path))))
                          (pseudopod:read-file bad-path))))
            (is-true (pseudopod:file-read-result-p result))
            (is (string= "recovered content"
                         (pseudopod:file-read-result-content result)))))
      (cleanup-temp-file good-path))))

;;; ---- Tests: Latin-1 fallback ----

(test file-read-latin1-fallback
  "File with invalid UTF-8 bytes falls back to Latin-1 decoding."
  (let ((path nil)
        ;; Bytes 0xC0 0xC1 are invalid UTF-8 lead bytes
        (octets (make-array 5 :element-type '(unsigned-byte 8)
                              :initial-contents '(72 101 #xC0 #xC1 10))))
    (unwind-protect
        (progn
          (setf path (make-temp-binary-file octets))
          (let ((result (pseudopod:read-file path)))
            (is-true (pseudopod:file-read-result-p result))
            (is (eq :latin-1 (pseudopod:file-read-result-encoding result)))
            (is (= 5 (pseudopod:file-read-result-size result)))
            ;; Content should be decoded as Latin-1
            (is (= 5 (length (pseudopod:file-read-result-content result))))))
      (cleanup-temp-file path))))
