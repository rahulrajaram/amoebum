(in-package :amoebum/test)

;;; ============================================================
;;; I264: Event Replay and Audit Query — Smoke Tests
;;; ============================================================

(def-suite event-replay-suite :in amoebum-suite)
(in-suite event-replay-suite)

;;; --- API functions ---

(test replay-api-functions-exist
  "Replay and audit API functions are bound."
  (is (fboundp 'amoebum:replay-journal))
  (is (fboundp 'amoebum:audit-query))
  (is (fboundp 'amoebum:audit-current-journal))
  (is (fboundp 'amoebum:replay-current-journal)))

;;; --- Write test journal, then query it ---

(defun %make-test-journal-file ()
  "Create a temporary JSONL journal file for testing. Returns path."
  (let* ((dir (merge-pathnames
               (format nil "amoebum-replay-test-~D/" (get-universal-time))
               #P"/tmp/"))
         (_ (ensure-directories-exist dir))
         (path (merge-pathnames "test-journal.jsonl" dir)))
    (declare (ignore _))
    (with-open-file (out path :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
      ;; Write 5 test events
      (format out "{\"seq\":1,\"ts\":1000,\"type\":\"TOOL:INVOKED\",\"source\":\"AMOEBUM\",\"severity\":\"INFO\",\"payload\":\"test event one\"}~%")
      (format out "{\"seq\":2,\"ts\":1001,\"type\":\"TOOL:COMPLETED\",\"source\":\"AMOEBUM\",\"severity\":\"INFO\",\"payload\":\"test event two\"}~%")
      (format out "{\"seq\":3,\"ts\":1002,\"type\":\"PERMISSION:BLOCKED\",\"source\":\"AMOEBUM\",\"severity\":\"WARNING\",\"payload\":\"blocked access\"}~%")
      (format out "{\"seq\":4,\"ts\":1003,\"type\":\"TOOL:ERROR\",\"source\":\"AMOEBUM\",\"severity\":\"ERROR\",\"payload\":\"something broke\"}~%")
      (format out "{\"seq\":5,\"ts\":1004,\"type\":\"TOOL:INVOKED\",\"source\":\"AMOEBUM\",\"severity\":\"INFO\",\"payload\":\"another invocation\"}~%"))
    (values path dir)))

;;; --- Audit query tests ---

(test audit-query-returns-all-events
  "audit-query with no filters returns all events."
  (multiple-value-bind (path dir) (%make-test-journal-file)
    (unwind-protect
         (let ((results (amoebum:audit-query (list path))))
           (is (= 5 (length results)))
           ;; Each result is a plist
           (is (listp (first results)))
           (is (= 1 (getf (first results) :seq)))
           (is (= 5 (getf (fifth results) :seq))))
      ;; Cleanup
      (ignore-errors (delete-file path))
      (ignore-errors (uiop:delete-empty-directory dir)))))

(test audit-query-filters-by-type
  "audit-query filters by event type."
  (multiple-value-bind (path dir) (%make-test-journal-file)
    (unwind-protect
         (let ((results (amoebum:audit-query (list path)
                                             :event-types '(:|TOOL:INVOKED|))))
           (is (= 2 (length results)))
           (is (every (lambda (r) (eq :|TOOL:INVOKED| (getf r :type)))
                      results)))
      (ignore-errors (delete-file path))
      (ignore-errors (uiop:delete-empty-directory dir)))))

(test audit-query-filters-by-severity
  "audit-query filters by severity."
  (multiple-value-bind (path dir) (%make-test-journal-file)
    (unwind-protect
         (let ((results (amoebum:audit-query (list path)
                                             :severities '(:error))))
           (is (= 1 (length results)))
           (is (eq :error (getf (first results) :severity))))
      (ignore-errors (delete-file path))
      (ignore-errors (uiop:delete-empty-directory dir)))))

(test audit-query-filters-by-text
  "audit-query text search finds matching payloads."
  (multiple-value-bind (path dir) (%make-test-journal-file)
    (unwind-protect
         (let ((results (amoebum:audit-query (list path)
                                             :text-search "broke")))
           (is (= 1 (length results)))
           (is (search "broke" (getf (first results) :payload))))
      (ignore-errors (delete-file path))
      (ignore-errors (uiop:delete-empty-directory dir)))))

(test audit-query-max-results
  "audit-query limits results."
  (multiple-value-bind (path dir) (%make-test-journal-file)
    (unwind-protect
         (let ((results (amoebum:audit-query (list path) :max-results 2)))
           (is (= 2 (length results))))
      (ignore-errors (delete-file path))
      (ignore-errors (uiop:delete-empty-directory dir)))))

;;; --- Output formats ---

(test audit-query-json-format
  "audit-query :json format returns a string."
  (multiple-value-bind (path dir) (%make-test-journal-file)
    (unwind-protect
         (let ((result (amoebum:audit-query (list path)
                                            :output-format :json
                                            :max-results 2)))
           (is (stringp result))
           (is (char= #\[ (char result 0)))
           (is (char= #\] (char result (1- (length result))))))
      (ignore-errors (delete-file path))
      (ignore-errors (uiop:delete-empty-directory dir)))))

(test audit-query-table-format
  "audit-query :table format returns a formatted string."
  (multiple-value-bind (path dir) (%make-test-journal-file)
    (unwind-protect
         (let ((result (amoebum:audit-query (list path)
                                            :output-format :table
                                            :max-results 2)))
           (is (stringp result))
           ;; Should contain header
           (is (search "SEQ" result))
           (is (search "TYPE" result)))
      (ignore-errors (delete-file path))
      (ignore-errors (uiop:delete-empty-directory dir)))))

;;; --- Replay tests ---

(test replay-journal-instant
  "replay-journal with speed-factor 0 replays instantly."
  (multiple-value-bind (path dir) (%make-test-journal-file)
    (unwind-protect
         (let* ((replay-bus (amoebum:make-event-bus))
                (count (amoebum:replay-journal (list path)
                                               :speed-factor 0.0
                                               :target-bus replay-bus)))
           (is (= 5 count))
           ;; Events should be in the bus history
           (let ((history (amoebum:event-history replay-bus)))
             (is (= 5 (length history)))))
      (ignore-errors (delete-file path))
      (ignore-errors (uiop:delete-empty-directory dir)))))

(test replay-journal-with-filter
  "replay-journal respects event type filter."
  (multiple-value-bind (path dir) (%make-test-journal-file)
    (unwind-protect
         (let* ((replay-bus (amoebum:make-event-bus))
                (count (amoebum:replay-journal (list path)
                                               :event-types '(:|TOOL:ERROR|)
                                               :speed-factor 0.0
                                               :target-bus replay-bus)))
           (is (= 1 count)))
      (ignore-errors (delete-file path))
      (ignore-errors (uiop:delete-empty-directory dir)))))

(test replay-journal-with-seq-range
  "replay-journal respects from-seq and to-seq."
  (multiple-value-bind (path dir) (%make-test-journal-file)
    (unwind-protect
         (let* ((replay-bus (amoebum:make-event-bus))
                (count (amoebum:replay-journal (list path)
                                               :from-seq 2
                                               :to-seq 4
                                               :speed-factor 0.0
                                               :target-bus replay-bus)))
           (is (= 3 count)))  ; seqs 2, 3, 4
      (ignore-errors (delete-file path))
      (ignore-errors (uiop:delete-empty-directory dir)))))

;;; --- Empty/nil cases ---

(test audit-current-journal-nil
  "audit-current-journal returns nil when no journal active."
  (let ((amoebum:*event-journal* nil))
    (is (null (amoebum:audit-current-journal)))))

(test replay-current-journal-nil
  "replay-current-journal returns 0 when no journal active."
  (let ((amoebum:*event-journal* nil))
    (is (= 0 (amoebum:replay-current-journal)))))
