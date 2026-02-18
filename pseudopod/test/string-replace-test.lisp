(in-package :pseudopod/test)

;;; ---- I108 Tests: String-replace edit primitive ----

(defun make-edit-test-path (name)
  "Create a temporary file path for string-replace testing."
  (merge-pathnames
   (format nil "pseudopod-edit-test-~A-~A.txt" name (random 1000000))
   (uiop:temporary-directory)))

(defmacro with-edit-test-file ((var name &optional initial-content) &body body)
  "Execute BODY with VAR bound to a temp path containing INITIAL-CONTENT.
Cleans up afterwards."
  `(let ((,var (make-edit-test-path ,name)))
     (unwind-protect
         (progn
           ,@(when initial-content
               `((with-open-file (stream ,var
                                         :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create
                                         :external-format :utf-8)
                   (write-string ,initial-content stream))))
           ,@body)
       (when (uiop:file-exists-p ,var)
         (handler-case (cl:delete-file ,var)
           (error () nil))))))

;;; ---- Single match replace ----

(test string-replace-single-match
  "Replace a single occurrence of old-string with new-string."
  (with-edit-test-file (path "single" "Hello world, goodbye world.")
    (let ((result (pseudopod:string-replace-in-file
                   path "goodbye" "farewell")))
      (is-true (pseudopod:string-replace-result-p result))
      (is (= 1 (pseudopod:string-replace-result-match-count result)))
      (is (= 1 (pseudopod:string-replace-result-replaced result)))
      (is (string= "goodbye" (pseudopod:string-replace-result-old-string result)))
      (is (string= "farewell" (pseudopod:string-replace-result-new-string result)))
      ;; Verify file content
      (is (string= "Hello world, farewell world."
                    (uiop:read-file-string path))))))

(test string-replace-single-match-multiline
  "Replace works correctly with multiline content."
  (with-edit-test-file (path "multiline"
                        (format nil "line one~%target line~%line three"))
    (let ((result (pseudopod:string-replace-in-file
                   path "target line" "replaced line")))
      (is (= 1 (pseudopod:string-replace-result-match-count result)))
      (is (string= (format nil "line one~%replaced line~%line three")
                    (uiop:read-file-string path))))))

;;; ---- No match error ----

(test string-replace-no-match-signals-error
  "Signals pseudopod-edit-no-match when old-string is not found."
  (with-edit-test-file (path "nomatch" "Hello world.")
    (signals pseudopod:pseudopod-edit-no-match
      (pseudopod:string-replace-in-file path "NONEXISTENT" "replacement"))))

(test string-replace-file-not-found-signals-error
  "Signals pseudopod-edit-no-match when the file does not exist."
  (let ((bogus-path (make-edit-test-path "nonexistent-file")))
    (signals pseudopod:pseudopod-edit-no-match
      (pseudopod:string-replace-in-file bogus-path "old" "new"))))

(test string-replace-empty-old-string-signals-error
  "Signals pseudopod-edit-error when old-string is empty."
  (with-edit-test-file (path "empty-old" "some content")
    (signals pseudopod:pseudopod-edit-error
      (pseudopod:string-replace-in-file path "" "new"))))

;;; ---- Multi-match ambiguity error ----

(test string-replace-multi-match-signals-ambiguous
  "Signals pseudopod-edit-ambiguous when multiple matches found without replace-all."
  (with-edit-test-file (path "multi" "foo bar foo baz foo")
    (handler-case
        (progn
          (pseudopod:string-replace-in-file path "foo" "qux")
          (fail "Expected pseudopod-edit-ambiguous to be signaled"))
      (pseudopod:pseudopod-edit-ambiguous (c)
        (is (= 3 (pseudopod:pseudopod-edit-ambiguous-match-count c))))
      (error (c)
        (fail "Expected pseudopod-edit-ambiguous, got ~A" (type-of c))))
    ;; File should be unchanged
    (is (string= "foo bar foo baz foo" (uiop:read-file-string path)))))

;;; ---- Replace-all mode ----

(test string-replace-all-mode
  "Replace all occurrences when :replace-all t is given."
  (with-edit-test-file (path "replall" "foo bar foo baz foo")
    (let ((result (pseudopod:string-replace-in-file
                   path "foo" "qux" :replace-all t)))
      (is-true (pseudopod:string-replace-result-p result))
      (is (= 3 (pseudopod:string-replace-result-match-count result)))
      (is (= 3 (pseudopod:string-replace-result-replaced result)))
      (is (string= "qux bar qux baz qux"
                    (uiop:read-file-string path))))))

(test string-replace-all-single-match-ok
  "Replace-all with a single match works without error."
  (with-edit-test-file (path "replall-single" "one unique string here")
    (let ((result (pseudopod:string-replace-in-file
                   path "unique" "special" :replace-all t)))
      (is (= 1 (pseudopod:string-replace-result-match-count result)))
      (is (= 1 (pseudopod:string-replace-result-replaced result)))
      (is (string= "one special string here"
                    (uiop:read-file-string path))))))

;;; ---- Atomic write behavior ----

(test string-replace-atomic-write
  "After a successful replace, no temp files remain and content is complete."
  (with-edit-test-file (path "atomic" (make-string 10000 :initial-element #\X))
    (let* ((dir (uiop:pathname-directory-pathname path))
           (old-string (make-string 100 :initial-element #\X))
           (new-string (make-string 100 :initial-element #\Y)))
      ;; Perform replace
      (pseudopod:string-replace-in-file path old-string new-string)
      ;; No temp files should remain
      (let ((tmp-files (directory (merge-pathnames ".pseudopod-write-*.tmp" dir))))
        (is (= 0 (length tmp-files))))
      ;; Content should be well-formed (starts with Y's then X's)
      (let ((content (uiop:read-file-string path)))
        (is (= 10000 (length content)))
        (is (char= #\Y (char content 0)))))))

;;; ---- Edge cases ----

(test string-replace-new-string-empty
  "Replace with empty new-string effectively deletes the match."
  (with-edit-test-file (path "delete" "Hello beautiful world")
    (pseudopod:string-replace-in-file path "beautiful " "")
    (is (string= "Hello world" (uiop:read-file-string path)))))

(test string-replace-result-path-set
  "Result struct has the correct path set."
  (with-edit-test-file (path "path-check" "test content here")
    (let ((result (pseudopod:string-replace-in-file path "content" "data")))
      (is-true (pseudopod:string-replace-result-path result)))))
