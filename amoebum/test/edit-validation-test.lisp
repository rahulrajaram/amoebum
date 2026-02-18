(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Edit Validation Tests (I109)
;;; ---------------------------------------------------------------------------

(def-suite edit-validation-suite :in amoebum-suite
  :description "Edit validation integration tests (I109).")

(in-suite edit-validation-suite)

;;; --- Test helpers ----------------------------------------------------------

(defun %make-edit-test-dir ()
  "Create a temporary directory for edit validation tests."
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames
               (make-pathname
                :directory `(:relative
                             ,(format nil "amoebum-i109-~D-~D"
                                      (get-universal-time)
                                      (random 1000000))))
               (uiop:ensure-directory-pathname (uiop:temporary-directory))))))
    (ensure-directories-exist (merge-pathnames #P".keep" dir))
    dir))

(defun %write-edit-test-file (dir filename content)
  "Write a test file and return its absolute path."
  (let ((path (merge-pathnames filename dir)))
    (ensure-directories-exist path)
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (write-string content stream))
    path))

;;; --- Precondition: valid edit passes ---------------------------------------

(test edit-val-valid-edit-passes-preconditions
  "A valid edit with existing file and non-empty old-string passes validation."
  (let* ((dir (%make-edit-test-dir))
         (content "line one\nline two\nline three\n")
         (path (%write-edit-test-file dir "valid.txt" content)))
    (unwind-protect
        (let ((result (amoebum:validate-edit-preconditions
                       (namestring path) "line two" "line TWO")))
          (is (listp result))
          (is-true (getf result :valid-p)
                   "Expected valid edit to pass preconditions.")
          (is (stringp (getf result :path)))
          (is (null (getf result :warnings))
              "Expected no warnings for a clean valid edit."))
      (%delete-directory-tree-safe dir))))

;;; --- Precondition: missing file fails --------------------------------------

(test edit-val-missing-file-fails
  "Editing a non-existent file signals edit-validation-error."
  (signals amoebum:edit-validation-error
    (amoebum:validate-edit-preconditions
     "/tmp/amoebum-i109-nonexistent-file.txt" "old" "new")))

;;; --- Precondition: nil path fails ------------------------------------------

(test edit-val-nil-path-fails
  "Nil path signals edit-validation-error."
  (signals amoebum:edit-validation-error
    (amoebum:validate-edit-preconditions nil "old" "new")))

;;; --- Precondition: empty old-string fails ----------------------------------

(test edit-val-empty-old-string-fails
  "Empty old-string signals edit-validation-error."
  (let* ((dir (%make-edit-test-dir))
         (path (%write-edit-test-file dir "empty-old.txt" "content")))
    (unwind-protect
        (signals amoebum:edit-validation-error
          (amoebum:validate-edit-preconditions (namestring path) "" "new"))
      (%delete-directory-tree-safe dir))))

;;; --- Precondition: nil old-string fails ------------------------------------

(test edit-val-nil-old-string-fails
  "Nil old-string signals edit-validation-error."
  (let* ((dir (%make-edit-test-dir))
         (path (%write-edit-test-file dir "nil-old.txt" "content")))
    (unwind-protect
        (signals amoebum:edit-validation-error
          (amoebum:validate-edit-preconditions (namestring path) nil "new"))
      (%delete-directory-tree-safe dir))))

;;; --- Content hash mismatch fails -------------------------------------------

(test edit-val-content-hash-mismatch-warns
  "Content hash mismatch after file change produces a warning."
  (let* ((dir (%make-edit-test-dir))
         (original-content "original content here")
         (path (%write-edit-test-file dir "hash-test.txt" original-content))
         (path-string (namestring path))
         (original-hashes amoebum:*edit-validation-content-hashes*))
    (unwind-protect
        (let ((amoebum:*edit-validation-content-hashes*
                (make-hash-table :test #'equal)))
          ;; Record the hash for the original content
          (amoebum::%edit-validation-record-content-hash path-string original-content)
          ;; Now modify the file behind the scenes
          (with-open-file (stream path
                                  :direction :output
                                  :if-exists :supersede
                                  :external-format :utf-8)
            (write-string "modified content here" stream))
          ;; Validation should detect the mismatch
          (let ((result (amoebum:validate-edit-preconditions
                         path-string "modified" "replacement")))
            (is (listp result))
            (is-true (getf result :valid-p)
                     "Hash mismatch is advisory, should still be valid.")
            (is (not (getf result :content-hash-ok-p))
                "Expected content-hash-ok-p to be nil on mismatch.")
            (is-true (getf result :warnings)
                     "Expected at least one warning about hash mismatch.")))
      (setf amoebum:*edit-validation-content-hashes* original-hashes)
      (%delete-directory-tree-safe dir))))

;;; --- Content hash match succeeds -------------------------------------------

(test edit-val-content-hash-match-succeeds
  "Content hash match after read produces no warning."
  (let* ((dir (%make-edit-test-dir))
         (content "stable content")
         (path (%write-edit-test-file dir "stable.txt" content))
         (path-string (namestring path))
         (original-hashes amoebum:*edit-validation-content-hashes*))
    (unwind-protect
        (let ((amoebum:*edit-validation-content-hashes*
                (make-hash-table :test #'equal)))
          ;; Record hash
          (amoebum::%edit-validation-record-content-hash path-string content)
          ;; Validate without modifying file
          (let ((result (amoebum:validate-edit-preconditions
                         path-string "stable" "new-stable")))
            (is-true (getf result :content-hash-ok-p)
                     "Expected hash to match when file unchanged.")
            (is (null (getf result :warnings))
                "Expected no warnings when hash matches.")))
      (setf amoebum:*edit-validation-content-hashes* original-hashes)
      (%delete-directory-tree-safe dir))))

;;; --- Post-edit hook runs ---------------------------------------------------

(test edit-val-post-edit-hook-runs
  "Registered post-edit hooks are called during postcondition validation."
  (let* ((dir (%make-edit-test-dir))
         (path (%write-edit-test-file dir "hooked.txt" "content"))
         (path-string (namestring path))
         (hook-called nil)
         (hook-path-received nil)
         (original-hooks amoebum:*edit-validation-post-hooks*))
    (unwind-protect
        (progn
          (setf amoebum:*edit-validation-post-hooks* '())
          (amoebum:register-post-edit-hook
           "test-hook"
           (lambda (p c)
             (declare (ignore c))
             (setf hook-called t
                   hook-path-received p)
             (values t "hook passed")))
          (let ((result (amoebum:validate-edit-postconditions
                         path-string "new content")))
            (is-true hook-called "Expected post-edit hook to be called.")
            (is (string= hook-path-received path-string)
                "Expected hook to receive the correct path.")
            (is-true (getf result :valid-p)
                     "Expected postconditions to pass when hook passes.")
            (is-true (getf result :hook-results)
                     "Expected hook-results to be populated.")))
      (setf amoebum:*edit-validation-post-hooks* original-hooks)
      (%delete-directory-tree-safe dir))))

;;; --- Post-edit hook failure reports ----------------------------------------

(test edit-val-post-edit-hook-failure-reports
  "A failing post-edit hook produces warnings in postcondition result."
  (let* ((dir (%make-edit-test-dir))
         (path (%write-edit-test-file dir "fail-hook.txt" "content"))
         (path-string (namestring path))
         (original-hooks amoebum:*edit-validation-post-hooks*))
    (unwind-protect
        (progn
          (setf amoebum:*edit-validation-post-hooks* '())
          (amoebum:register-post-edit-hook
           "failing-hook"
           (lambda (p c)
             (declare (ignore p c))
             (values nil "syntax error detected")))
          (let ((result (amoebum:validate-edit-postconditions
                         path-string "new content")))
            (is (not (getf result :valid-p))
                "Expected postconditions to fail when hook fails.")
            (is-true (getf result :warnings)
                     "Expected warnings from failing hook.")
            (is (search "syntax error detected"
                        (first (getf result :warnings)))
                "Expected warning to contain hook failure message.")))
      (setf amoebum:*edit-validation-post-hooks* original-hooks)
      (%delete-directory-tree-safe dir))))

;;; --- Post-edit hook error handling -----------------------------------------

(test edit-val-post-edit-hook-error-handled
  "A hook that signals an error is caught and reported as failure."
  (let ((original-hooks amoebum:*edit-validation-post-hooks*))
    (unwind-protect
        (progn
          (setf amoebum:*edit-validation-post-hooks* '())
          (amoebum:register-post-edit-hook
           "error-hook"
           (lambda (p c)
             (declare (ignore p c))
             (error "deliberate test error")))
          (let ((result (amoebum:validate-edit-postconditions "/tmp/test" "content")))
            (is (not (getf result :valid-p))
                "Expected postconditions to fail when hook errors.")
            (is-true (getf result :warnings)
                     "Expected warnings from erroring hook.")))
      (setf amoebum:*edit-validation-post-hooks* original-hooks))))

;;; --- Hook registration / unregistration ------------------------------------

(test edit-val-hook-registration-lifecycle
  "Post-edit hooks can be registered, listed, and unregistered."
  (let ((original-hooks amoebum:*edit-validation-post-hooks*))
    (unwind-protect
        (progn
          (amoebum:clear-post-edit-hooks)
          (is (null amoebum:*edit-validation-post-hooks*)
              "Expected empty hooks after clear.")
          (amoebum:register-post-edit-hook "hook-a" (lambda (p c) (declare (ignore p c)) (values t "ok")))
          (amoebum:register-post-edit-hook "hook-b" (lambda (p c) (declare (ignore p c)) (values t "ok")))
          (is (= 2 (length amoebum:*edit-validation-post-hooks*))
              "Expected two registered hooks.")
          (amoebum:unregister-post-edit-hook "hook-a")
          (is (= 1 (length amoebum:*edit-validation-post-hooks*))
              "Expected one hook after unregister.")
          (amoebum:clear-post-edit-hooks)
          (is (null amoebum:*edit-validation-post-hooks*)
              "Expected empty hooks after final clear."))
      (setf amoebum:*edit-validation-post-hooks* original-hooks))))

;;; --- Condition type hierarchy ----------------------------------------------

(test edit-val-condition-type-hierarchy
  "edit-validation-error is a subtype of tool-argument-error."
  (let ((err (make-condition 'amoebum:edit-validation-error
                              :tool-name "edit-file"
                              :argument-name "path"
                              :message "test")))
    (is (typep err 'amoebum:tool-argument-error))
    (is (typep err 'amoebum:tool-error))
    (is (typep err 'amoebum:edit-validation-error))))

;;; --- Content hash computation consistency ----------------------------------

(test edit-val-content-hash-deterministic
  "Content hash computation is deterministic for the same input."
  (let* ((content "hello world test content")
         (hash1 (amoebum::%edit-validation-content-hash content))
         (hash2 (amoebum::%edit-validation-content-hash content)))
    (is-true (amoebum::%edit-validation-hash-equal-p hash1 hash2)
             "Expected identical content to produce equal hashes.")))

(test edit-val-content-hash-differs-on-change
  "Content hash differs when content changes."
  (let* ((content1 "original content")
         (content2 "modified content")
         (hash1 (amoebum::%edit-validation-content-hash content1))
         (hash2 (amoebum::%edit-validation-content-hash content2)))
    (is (not (amoebum::%edit-validation-hash-equal-p hash1 hash2))
        "Expected different content to produce different hashes.")))

;;; --- Install/uninstall hooks -----------------------------------------------

(test edit-val-install-uninstall-hooks
  "Edit validation hooks can be installed and uninstalled without error."
  (let ((original-hooks amoebum:*hook-registry*))
    (unwind-protect
        (progn
          (finishes (amoebum:install-edit-validation-hooks))
          (finishes (amoebum:uninstall-edit-validation-hooks)))
      (setf amoebum:*hook-registry* original-hooks))))

;;; --- Empty path string fails -----------------------------------------------

(test edit-val-empty-path-string-fails
  "Empty path string signals edit-validation-error."
  (signals amoebum:edit-validation-error
    (amoebum:validate-edit-preconditions "" "old" "new")))

;;; --- Whitespace-only path fails --------------------------------------------

(test edit-val-whitespace-path-fails
  "Whitespace-only path signals edit-validation-error."
  (signals amoebum:edit-validation-error
    (amoebum:validate-edit-preconditions "   " "old" "new")))
