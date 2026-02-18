(in-package :pseudopod/test)

;;; ---- I106 Tests: Whole-file write primitive ----

(defun make-test-path (name)
  "Create a temporary file path for testing."
  (merge-pathnames
   (format nil "pseudopod-write-test-~A-~A.txt" name (random 1000000))
   (uiop:temporary-directory)))

(defmacro with-test-file ((var name) &body body)
  "Execute BODY with VAR bound to a temp path, cleaning up afterwards."
  `(let ((,var (make-test-path ,name)))
     (unwind-protect
         (progn ,@body)
       (when (uiop:file-exists-p ,var)
         (handler-case (cl:delete-file ,var)
           (error () nil))))))

(test write-file-create-new
  "Write to a new file creates it with correct content and metadata."
  (with-test-file (path "create")
    (let ((result (pseudopod:write-file path "hello world")))
      (is-true (pseudopod:write-result-p result))
      (is-true (pseudopod:write-result-created-p result))
      (is (eq :utf-8 (pseudopod:write-result-encoding result)))
      (is (> (pseudopod:write-result-bytes result) 0))
      (is (uiop:file-exists-p (pseudopod:write-result-path result)))
      ;; Verify content
      (is (string= "hello world"
                    (uiop:read-file-string
                     (pseudopod:write-result-path result)))))))

(test write-file-overwrite-existing
  "Write to an existing file overwrites content and reports not-created."
  (with-test-file (path "overwrite")
    ;; Create initial file
    (pseudopod:write-file path "original content")
    (is (string= "original content" (uiop:read-file-string path)))
    ;; Overwrite
    (let ((result (pseudopod:write-file path "new content")))
      (is-true (pseudopod:write-result-p result))
      (is-false (pseudopod:write-result-created-p result))
      (is (string= "new content" (uiop:read-file-string path))))))

(test write-file-if-exists-error
  "Write with :if-exists :error signals when file already exists."
  (with-test-file (path "exists-error")
    (pseudopod:write-file path "initial")
    (signals pseudopod:pseudopod-write-error
      (pseudopod:write-file path "should fail" :if-exists :error))))

(test write-file-empty-content
  "Write empty string produces an empty file."
  (with-test-file (path "empty")
    (let ((result (pseudopod:write-file path "")))
      (is-true (pseudopod:write-result-p result))
      (is-true (pseudopod:write-result-created-p result))
      (is (string= "" (uiop:read-file-string path))))))

(test write-file-unicode-content
  "Write UTF-8 content including multibyte characters."
  (with-test-file (path "unicode")
    (let* ((text (format nil "Hello ~A world ~A" #\U+2603 #\U+1F600))
           (result (pseudopod:write-file path text)))
      (is-true (pseudopod:write-result-p result))
      (is (string= text (uiop:read-file-string path))))))

(test write-file-atomic-no-partial
  "After a successful write, the file contains complete content (not partial)."
  (with-test-file (path "atomic")
    (let ((long-content (make-string 10000 :initial-element #\X)))
      (pseudopod:write-file path long-content)
      ;; The file must contain the full content
      (let ((read-back (uiop:read-file-string path)))
        (is (= (length long-content) (length read-back)))
        (is (string= long-content read-back))))))

(test write-file-creates-parent-directories
  "Write to a path whose parent directories do not exist creates them."
  (let* ((base-dir (merge-pathnames
                    (format nil "pseudopod-write-test-~A/" (random 1000000))
                    (uiop:temporary-directory)))
         (nested-path (merge-pathnames "sub/dir/file.txt" base-dir)))
    (unwind-protect
        (progn
          (let ((result (pseudopod:write-file nested-path "nested content")))
            (is-true (pseudopod:write-result-p result))
            (is-true (pseudopod:write-result-created-p result))
            (is (string= "nested content" (uiop:read-file-string nested-path)))))
      ;; Cleanup
      (when (uiop:directory-exists-p base-dir)
        (uiop:delete-directory-tree base-dir :validate t)))))

(test write-file-unsupported-encoding-signals-error
  "Write with an unsupported encoding signals pseudopod-write-error."
  (with-test-file (path "bad-encoding")
    (signals pseudopod:pseudopod-write-error
      (pseudopod:write-file path "content" :encoding :ebcdic))))

(test write-file-no-temp-file-left-on-success
  "After a successful write, no .tmp files remain in the directory."
  (with-test-file (path "no-tmp")
    (pseudopod:write-file path "clean write")
    (let* ((dir (uiop:pathname-directory-pathname path))
           (tmp-files (directory (merge-pathnames ".pseudopod-write-*.tmp" dir))))
      (is (= 0 (length tmp-files))))))
