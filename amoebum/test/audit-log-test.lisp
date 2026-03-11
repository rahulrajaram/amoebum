(in-package :amoebum/test)

(def-suite audit-log-suite
  :description "Structured JSONL audit log backend tests (I224)."
  :in amoebum-suite)

(in-suite audit-log-suite)

(defun %audit-log-read-lines (path)
  (with-open-file (stream path :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun %audit-log-make-event (&key
                                (event-type amoebum:+event-type-tool-completed+)
                                (request-id "session-a"))
  (amoebum:make-event
   :type event-type
   :source :amoebum
   :severity :info
   :payload (amoebum:make-tool-completed-payload
             :tool-name "audit-log-test"
             :args '(:path "README.md")
             :result "ok"
             :elapsed-ms 12
             :request-id request-id)))

(test audit-log-writes-jsonl-lines
  (let* ((tmp-root (%make-temp-directory "amoebum-i224-audit-write"))
         (log-path (merge-pathnames #P"audit/events.jsonl" tmp-root))
         (backend (amoebum:make-log-backend :path log-path)))
    (unwind-protect
        (progn
          (is-true (amoebum:audit-log-write-event backend (%audit-log-make-event)))
          (let* ((lines (%audit-log-read-lines log-path))
                 (decoded (jonathan:parse (first lines) :as :hash-table)))
            (is (= 1 (length lines)))
            (is (string= "tool:completed" (gethash "event-type" decoded)))
            (is (string= "session-a" (gethash "session-id" decoded)))
            (is (hash-table-p (gethash "data" decoded)))))
      (%delete-directory-tree-safe tmp-root))))

(test audit-log-rotates-at-size-threshold
  (let* ((tmp-root (%make-temp-directory "amoebum-i224-audit-rotate"))
         (log-path (merge-pathnames #P"audit/events.jsonl" tmp-root))
         (backend (amoebum:make-log-backend :path log-path)))
    (unwind-protect
        (let ((amoebum::+audit-log-max-bytes+ 256))
          (dotimes (index 8)
            (amoebum:audit-log-write-event
             backend
             (%audit-log-make-event :request-id (format nil "session-~D" index))))
          (let ((rotated
                  (directory
                   (merge-pathnames #P"audit/events-*.jsonl" tmp-root))))
            (is-true (probe-file log-path))
            (is-true (plusp (length rotated)))))
      (%delete-directory-tree-safe tmp-root))))

(test audit-log-query-filters
  (let* ((tmp-root (%make-temp-directory "amoebum-i224-audit-query"))
         (log-path (merge-pathnames #P"audit/events.jsonl" tmp-root))
         (backend (amoebum:make-log-backend :path log-path))
         (t1 4000000000)
         (t2 4000000100)
         (t3 4000000200))
    (unwind-protect
        (progn
          (amoebum:audit-log-write-event
           backend
           (%audit-log-make-event
            :event-type amoebum:+event-type-tool-completed+
            :request-id "session-a")
           :timestamp t1)
          (amoebum:audit-log-write-event
           backend
           (%audit-log-make-event
            :event-type amoebum:+event-type-tool-error+
            :request-id "session-b")
           :timestamp t2)
          (amoebum:audit-log-write-event
           backend
           (%audit-log-make-event
            :event-type amoebum:+event-type-tool-error+
            :request-id "session-c")
           :timestamp t3)
          (is (= 2 (length (amoebum:audit-log-query
                            :path log-path
                            :event-type "tool:error"))))
          (is (= 1 (length (amoebum:audit-log-query
                            :path log-path
                            :session-id "session-b"))))
          (is (= 1 (length (amoebum:audit-log-query
                            :path log-path
                            :start-time t2
                            :end-time t2)))))
      (%delete-directory-tree-safe tmp-root))))

(test audit-log-thread-safe-writes
  (let* ((tmp-root (%make-temp-directory "amoebum-i224-audit-thread"))
         (log-path (merge-pathnames #P"audit/events.jsonl" tmp-root))
         (backend (amoebum:make-log-backend :path log-path))
         (thread-count 6)
         (writes-per-thread 25))
    (unwind-protect
        (let ((threads '()))
          (dotimes (thread-index thread-count)
            (push (bordeaux-threads:make-thread
                   (lambda ()
                     (dotimes (i writes-per-thread)
                       (declare (ignore i))
                       (amoebum:audit-log-write-event
                        backend
                        (%audit-log-make-event
                         :request-id (format nil "thread-~D" thread-index)))))
                   :name (format nil "audit-log-test-~D" thread-index))
                  threads))
          (dolist (thread threads)
            (bordeaux-threads:join-thread thread))
          (let ((lines (%audit-log-read-lines log-path)))
            (is (= (* thread-count writes-per-thread) (length lines)))
            (is (every (lambda (line)
                         (hash-table-p (jonathan:parse line :as :hash-table)))
                       lines))))
      (%delete-directory-tree-safe tmp-root))))

(test audit-log-smoke-sentinel
  (is-true t)
  (format t "AUDIT_LOG_SMOKE_OK~%"))
