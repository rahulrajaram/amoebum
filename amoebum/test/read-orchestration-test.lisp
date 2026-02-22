(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Read Orchestration Tests (I105)
;;; ---------------------------------------------------------------------------

(def-suite read-orchestration-suite :in amoebum-suite
  :description "Read orchestration integration tests (I105).")

(in-suite read-orchestration-suite)

;;; --- Test helpers ----------------------------------------------------------

(defun %make-read-test-dir ()
  "Create a temporary directory for read orchestration tests."
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames
               (make-pathname
                :directory `(:relative
                             ,(format nil "amoebum-i105-~D-~D"
                                      (get-universal-time)
                                      (random 1000000))))
               (uiop:ensure-directory-pathname (uiop:temporary-directory))))))
    (ensure-directories-exist (merge-pathnames #P".keep" dir))
    dir))

(defun %write-read-test-file (dir filename content)
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

;;; --- Argument validation tests ---------------------------------------------

(test read-orch-validates-nil-path
  "Nil path signals read-orchestration-error."
  (signals amoebum:read-orchestration-error
    (amoebum:validate-read-arguments nil nil nil nil)))

(test read-orch-validates-empty-path
  "Empty-string path signals read-orchestration-error."
  (signals amoebum:read-orchestration-error
    (amoebum:validate-read-arguments "" nil nil nil)))

(test read-orch-validates-relative-path
  "Relative path signals read-orchestration-error."
  (signals amoebum:read-orchestration-error
    (amoebum:validate-read-arguments "relative/file.txt" nil nil nil)))

(test read-orch-validates-nonexistent-file
  "Non-existent file signals read-orchestration-error."
  (signals amoebum:read-orchestration-error
    (amoebum:validate-read-arguments "/tmp/amoebum-i105-nonexistent-file.txt"
                                      nil nil nil)))

(test read-orch-validates-negative-offset
  "Negative offset signals read-orchestration-error."
  (let* ((dir (%make-read-test-dir))
         (path (%write-read-test-file dir "test.txt" "hello")))
    (unwind-protect
        (signals amoebum:read-orchestration-error
          (amoebum:validate-read-arguments (namestring path) -1 nil nil))
      (%delete-directory-tree-safe dir))))

(test read-orch-validates-negative-limit
  "Negative limit signals read-orchestration-error."
  (let* ((dir (%make-read-test-dir))
         (path (%write-read-test-file dir "test.txt" "hello")))
    (unwind-protect
        (signals amoebum:read-orchestration-error
          (amoebum:validate-read-arguments (namestring path) nil -5 nil))
      (%delete-directory-tree-safe dir))))

(test read-orch-validates-excessive-limit
  "Limit exceeding maximum signals read-orchestration-error."
  (let* ((dir (%make-read-test-dir))
         (path (%write-read-test-file dir "test.txt" "hello"))
         (amoebum:*read-orchestration-max-line-limit* 100))
    (unwind-protect
        (signals amoebum:read-orchestration-error
          (amoebum:validate-read-arguments (namestring path) nil 200 nil))
      (%delete-directory-tree-safe dir))))

(test read-orch-validates-pages-on-non-pdf
  "Pages argument on non-PDF file signals read-orchestration-error."
  (let* ((dir (%make-read-test-dir))
         (path (%write-read-test-file dir "test.txt" "hello")))
    (unwind-protect
        (signals amoebum:read-orchestration-error
          (amoebum:validate-read-arguments (namestring path) nil nil "1-5"))
      (%delete-directory-tree-safe dir))))

(test read-orch-valid-arguments-succeed
  "Valid arguments pass validation without error."
  (let* ((dir (%make-read-test-dir))
         (path (%write-read-test-file dir "test.txt" "hello")))
    (unwind-protect
        (multiple-value-bind (path-string offset limit pages)
            (amoebum:validate-read-arguments (namestring path) 0 10 nil)
          (is (stringp path-string))
          (is (= 0 offset))
          (is (= 10 limit))
          (is (null pages)))
      (%delete-directory-tree-safe dir))))

(test read-orch-accepts-pathname-object
  "Pathname objects are accepted and normalised to string."
  (let* ((dir (%make-read-test-dir))
         (path (%write-read-test-file dir "test.txt" "hello")))
    (unwind-protect
        (multiple-value-bind (path-string offset limit pages)
            (amoebum:validate-read-arguments path nil nil nil)
          (declare (ignore offset limit pages))
          (is (stringp path-string))
          (is (search "test.txt" path-string)))
      (%delete-directory-tree-safe dir))))

;;; --- Full orchestrate-read tests -------------------------------------------

(test read-orch-full-file-read
  "orchestrate-read returns complete file content."
  (let* ((dir (%make-read-test-dir))
         (content (format nil "line one~%line two~%line three~%"))
         (path (%write-read-test-file dir "full.txt" content))
         (original-rules amoebum:*permission-rules*))
    (unwind-protect
        (progn
          (setf amoebum:*permission-rules* nil)
          (amoebum:add-permission-rule :effect :allow
                                          :path (namestring dir)
                                          :tool :read-file)
          (let ((result (amoebum:orchestrate-read (namestring path))))
            (is (stringp result))
            (is (search "line one" result))
            (is (search "line two" result))
            (is (search "line three" result))))
      (setf amoebum:*permission-rules* original-rules)
      (%delete-directory-tree-safe dir))))

(test read-orch-structured-read-with-offset-limit
  "orchestrate-read with offset and limit returns bounded content."
  (let* ((dir (%make-read-test-dir))
         (content (format nil "~{line ~D~%~}" '(1 2 3 4 5 6 7 8 9 10)))
         (path (%write-read-test-file dir "ranged.txt" content))
         (original-rules amoebum:*permission-rules*))
    (unwind-protect
        (progn
          (setf amoebum:*permission-rules* nil)
          (amoebum:add-permission-rule :effect :allow
                                          :path (namestring dir)
                                          :tool :read-file)
          (let ((result (amoebum:orchestrate-read (namestring path)
                                                   :offset 2
                                                   :limit 3)))
            (is (stringp result))
            ;; With offset=2 and limit=3 we should get lines 3, 4, 5
            (is (search "line 3" result))
            (is (search "line 5" result))
            ;; Should not contain line 1 or line 8
            (is (not (search "line 1" result)))))
      (setf amoebum:*permission-rules* original-rules)
      (%delete-directory-tree-safe dir))))

(test read-orch-full-file-no-offset-no-limit
  "orchestrate-read with no offset/limit returns all lines."
  (let* ((dir (%make-read-test-dir))
         (content (format nil "alpha~%bravo~%charlie~%"))
         (path (%write-read-test-file dir "all.txt" content))
         (original-rules amoebum:*permission-rules*))
    (unwind-protect
        (progn
          (setf amoebum:*permission-rules* nil)
          (amoebum:add-permission-rule :effect :allow
                                          :path (namestring dir)
                                          :tool :read-file)
          (let ((result (amoebum:orchestrate-read (namestring path))))
            (is (search "alpha" result))
            (is (search "bravo" result))
            (is (search "charlie" result))))
      (setf amoebum:*permission-rules* original-rules)
      (%delete-directory-tree-safe dir))))

;;; --- Pipeline integration tests --------------------------------------------

(test read-orch-pipeline-integration
  "orchestrate-read-via-pipeline dispatches through execute-tool pipeline."
  (let* ((dir (%make-read-test-dir))
         (content "pipeline test content")
         (path (%write-read-test-file dir "pipe.txt" content))
         (original-toolset amoebum:*toolset*)
         (original-metadata amoebum:*tool-metadata*)
         (original-history amoebum:*tool-history*)
         (original-rules amoebum:*permission-rules*)
         (original-bus amoebum:*event-bus*)
         (bus (amoebum:make-event-bus :capacity 64))
         (invoked-events 0)
         (completed-events 0))
    (unwind-protect
        (progn
          (setf amoebum:*event-bus* bus
                amoebum:*permission-rules* nil)
          (amoebum:add-permission-rule :effect :allow
                                          :path (namestring dir)
                                          :tool :read-file)
          (amoebum:subscribe bus
                             amoebum:+event-type-tool-invoked+
                             (lambda (_event)
                               (declare (ignore _event))
                               (incf invoked-events)))
          (amoebum:subscribe bus
                             amoebum:+event-type-tool-completed+
                             (lambda (_event)
                               (declare (ignore _event))
                               (incf completed-events)))
          (let ((result (amoebum:orchestrate-read-via-pipeline
                         (namestring path)
                         :event-bus bus)))
            (is (stringp result))
            (is (search "pipeline test content" result))
            (is (= 1 invoked-events)
                "Expected one tool:invoked event from pipeline read.")
            (is (= 1 completed-events)
                "Expected one tool:completed event from pipeline read.")))
      (setf amoebum:*toolset* original-toolset
            amoebum:*tool-metadata* original-metadata
            amoebum:*tool-history* original-history
            amoebum:*event-bus* original-bus
            amoebum:*permission-rules* original-rules)
      (%delete-directory-tree-safe dir))))

;;; --- Error formatting tests ------------------------------------------------

(test read-orch-format-error-messages
  "format-read-error-for-user produces user-friendly strings."
  (let ((orch-err (make-condition 'amoebum:read-orchestration-error
                                   :tool-name "read-file"
                                   :argument-name "path"
                                   :message "File not found: /tmp/nope.txt"
                                   :reason "file not found"))
        (perm-err (make-condition 'amoebum:tool-permission-denied
                                   :tool-name "read-file"
                                   :reason "policy blocked")))
    (let ((orch-msg (amoebum:format-read-error-for-user orch-err))
          (perm-msg (amoebum:format-read-error-for-user perm-err)))
      (is (stringp orch-msg))
      (is (search "Error:" orch-msg))
      (is (search "File not found" orch-msg))
      (is (stringp perm-msg))
      (is (search "Permission denied" perm-msg)))))

(test read-orch-condition-type-hierarchy
  "read-orchestration-error is a subtype of tool-argument-error."
  (let ((err (make-condition 'amoebum:read-orchestration-error
                              :tool-name "read-file"
                              :argument-name "path"
                              :message "test")))
    (is (typep err 'amoebum:tool-argument-error))
    (is (typep err 'amoebum:tool-error))
    (is (typep err 'amoebum:read-orchestration-error))))

;;; --- Edge case tests -------------------------------------------------------

(test read-orch-whitespace-only-path
  "Whitespace-only path signals error."
  (signals amoebum:read-orchestration-error
    (amoebum:validate-read-arguments "   " nil nil nil)))

(test read-orch-zero-limit-is-valid
  "Zero limit is a valid argument (returns empty content)."
  (let* ((dir (%make-read-test-dir))
         (path (%write-read-test-file dir "zero.txt" "content")))
    (unwind-protect
        (finishes
          (amoebum:validate-read-arguments (namestring path) nil 0 nil))
      (%delete-directory-tree-safe dir))))

(test read-orch-zero-offset-is-valid
  "Zero offset is a valid argument."
  (let* ((dir (%make-read-test-dir))
         (path (%write-read-test-file dir "zero-off.txt" "content")))
    (unwind-protect
        (finishes
          (amoebum:validate-read-arguments (namestring path) 0 nil nil))
      (%delete-directory-tree-safe dir))))

(test read-orch-string-offset-signals-error
  "Non-integer offset signals error."
  (let* ((dir (%make-read-test-dir))
         (path (%write-read-test-file dir "bad-off.txt" "content")))
    (unwind-protect
        (signals amoebum:read-orchestration-error
          (amoebum:validate-read-arguments (namestring path) "five" nil nil))
      (%delete-directory-tree-safe dir))))

(test read-orch-string-limit-signals-error
  "Non-integer limit signals error."
  (let* ((dir (%make-read-test-dir))
         (path (%write-read-test-file dir "bad-lim.txt" "content")))
    (unwind-protect
        (signals amoebum:read-orchestration-error
          (amoebum:validate-read-arguments (namestring path) nil "ten" nil))
      (%delete-directory-tree-safe dir))))

;;; --- I148 speculative reads + cache tests ---------------------------------

(test read-orch-cache-hit-miss-accounting
  "Read orchestration emits hit/miss accounting and invalidates on turn/file changes."
  (let* ((dir (%make-read-test-dir))
         (path (%write-read-test-file dir "cache.txt" "first"))
         (bus (amoebum:make-event-bus :capacity 64))
         (original-rules amoebum:*permission-rules*))
    (unwind-protect
        (progn
          (setf amoebum:*permission-rules* nil)
          (amoebum:add-permission-rule :effect :allow
                                       :path (namestring dir)
                                       :tool :read-file)
          (amoebum:clear-read-orchestration-cache)
          ;; first read: miss
          (let ((first (amoebum:orchestrate-read (namestring path)
                                                 :turn-id "turn-1"
                                                 :event-bus bus)))
            (is (search "first" first)))
          ;; second read same turn: hit
          (let ((second (amoebum:orchestrate-read (namestring path)
                                                  :turn-id "turn-1"
                                                  :event-bus bus)))
            (is (search "first" second)))
          ;; file update should invalidate old signature cache and miss.
          (%write-read-test-file dir "cache.txt" "second")
          (let ((third (amoebum:orchestrate-read (namestring path)
                                                 :turn-id "turn-1"
                                                 :event-bus bus)))
            (is (search "second" third)))
          ;; turn change should clear per-turn cache and produce miss.
          (let ((fourth (amoebum:orchestrate-read (namestring path)
                                                  :turn-id "turn-2"
                                                  :event-bus bus)))
            (is (search "second" fourth)))
          (let* ((events (remove-if-not
                          (lambda (event)
                            (eq (amoebum:event-type event)
                                amoebum:+event-type-read-orchestration-cache+))
                          (amoebum:event-history bus)))
                 (last-event (car (last events)))
                 (payload (and last-event (amoebum:event-payload last-event)))
                 (metrics (amoebum:read-orchestration-cache-metrics)))
            (is (= 4 (length events)))
            (is (equal "turn-2" (getf payload :turn-id)))
            (is (= 1 (getf payload :cache-miss-count)))
            (is (>= (getf metrics :hits 0) 1))
            (is (>= (getf metrics :misses 0) 3))))
      (setf amoebum:*permission-rules* original-rules)
      (amoebum:clear-read-orchestration-cache)
      (%delete-directory-tree-safe dir))))

(test read-orch-speculative-reads-deterministic
  "Speculative reads choose the first successful candidate in provided order."
  (let* ((dir (%make-read-test-dir))
         (primary (merge-pathnames "missing.txt" dir))
         (secondary (%write-read-test-file dir "secondary.txt" "secondary-choice"))
         (tertiary (%write-read-test-file dir "tertiary.txt" "tertiary-choice"))
         (bus (amoebum:make-event-bus :capacity 64))
         (original-rules amoebum:*permission-rules*))
    (unwind-protect
        (progn
          (setf amoebum:*permission-rules* nil)
          (amoebum:add-permission-rule :effect :allow
                                       :path (namestring dir)
                                       :tool :read-file)
          (amoebum:clear-read-orchestration-cache)
          (loop repeat 3 do
                (let ((result (amoebum:orchestrate-read (namestring primary)
                                                        :candidate-paths (list (namestring primary)
                                                                               (namestring secondary)
                                                                               (namestring tertiary))
                                                        :turn-id "spec-turn"
                                                        :event-bus bus)))
                  (is (search "secondary-choice" result))
                  (is (not (search "tertiary-choice" result)))))
          (let* ((events (remove-if-not
                          (lambda (event)
                            (eq (amoebum:event-type event)
                                amoebum:+event-type-read-orchestration-cache+))
                          (amoebum:event-history bus)))
                 (last-event (car (last events)))
                 (payload (and last-event (amoebum:event-payload last-event))))
            (is (>= (length events) 1))
            (is (getf payload :speculative-p))
            (is (= 3 (getf payload :candidate-count)))
            (is (search "secondary.txt" (or (getf payload :selected-path) "")))))
      (setf amoebum:*permission-rules* original-rules)
      (amoebum:clear-read-orchestration-cache)
      (%delete-directory-tree-safe dir))))
