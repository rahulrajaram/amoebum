(in-package :amoebum/test)

;;; ============================================================
;;; I263: Event Journal — Smoke Tests
;;; ============================================================

(def-suite event-journal-suite :in amoebum-suite)
(in-suite event-journal-suite)

;;; --- Structure completeness ---

(test event-journal-structures-exist
  "Journal structures and accessors are defined."
  (is (fboundp 'amoebum:journal-segment-p))
  (is (fboundp 'amoebum:journal-segment-path))
  (is (fboundp 'amoebum:journal-segment-created-at))
  (is (fboundp 'amoebum:journal-segment-closed-at))
  (is (fboundp 'amoebum:journal-segment-event-count))
  (is (fboundp 'amoebum:journal-segment-byte-count))
  (is (fboundp 'amoebum:event-journal-p))
  (is (fboundp 'amoebum:event-journal-directory))
  (is (fboundp 'amoebum:event-journal-running-p))
  (is (fboundp 'amoebum:event-journal-total-events))
  (is (fboundp 'amoebum:event-journal-segments)))

;;; --- API functions ---

(test event-journal-api-functions-exist
  "Journal API functions are bound."
  (is (fboundp 'amoebum:make-event-journal-instance))
  (is (fboundp 'amoebum:start-event-journal))
  (is (fboundp 'amoebum:stop-event-journal))
  (is (fboundp 'amoebum:journal-statistics))
  (is (fboundp 'amoebum:journal-segment-paths))
  (is (fboundp 'amoebum:reset-event-journal)))

;;; --- Journal creation ---

(test make-event-journal-with-defaults
  "make-event-journal-instance creates a journal with default config."
  (let ((j (amoebum:make-event-journal-instance)))
    (is (amoebum:event-journal-p j))
    (is (not (amoebum:event-journal-running-p j)))
    (is (= 0 (amoebum:event-journal-total-events j)))))

(test make-event-journal-with-custom-config
  "make-event-journal-instance accepts custom configuration."
  (let ((j (amoebum:make-event-journal-instance
            :max-segment-bytes 1024
            :max-segment-seconds 60
            :max-segments 5
            :flush-interval 3)))
    (is (amoebum:event-journal-p j))))

;;; --- Journal start/stop lifecycle ---

(test journal-start-stop-lifecycle
  "Starting and stopping a journal works correctly."
  (let* ((dir (merge-pathnames
               (format nil "amoebum-journal-test-~D/" (get-universal-time))
               #P"/tmp/"))
         (bus (amoebum:make-event-bus))
         (amoebum:*event-bus* bus)
         (j (amoebum:make-event-journal-instance
             :directory dir
             :max-segment-bytes 4096)))
    (unwind-protect
         (progn
           ;; Start
           (amoebum:start-event-journal :journal j :event-bus bus)
           (is (amoebum:event-journal-running-p j))
           ;; Publish some events
           (dotimes (i 5)
             (amoebum:publish bus :test-event
                              :source :journal-test
                              :severity :info
                              :payload (format nil "event-~D" i)))
           (sleep 0.1)
           ;; Check statistics
           (let ((stats (amoebum:journal-statistics j)))
             (is (getf stats :running-p))
             (is (>= (getf stats :total-events) 5)))
           ;; Check segment paths
           (let ((paths (amoebum:journal-segment-paths j)))
             (is (>= (length paths) 1)))
           ;; Stop
           (amoebum:stop-event-journal j)
           (is (not (amoebum:event-journal-running-p j))))
      ;; Cleanup
      (ignore-errors (amoebum:stop-event-journal j))
      (ignore-errors
        (dolist (path (directory (merge-pathnames "*.jsonl" dir)))
          (delete-file path))
        (uiop:delete-empty-directory dir)))))

;;; --- Journal writes JSONL ---

(test journal-writes-jsonl-format
  "Journal writes events in JSONL format."
  (let* ((dir (merge-pathnames
               (format nil "amoebum-journal-jsonl-~D/" (get-universal-time))
               #P"/tmp/"))
         (bus (amoebum:make-event-bus))
         (amoebum:*event-bus* bus)
         (j (amoebum:make-event-journal-instance
             :directory dir)))
    (unwind-protect
         (progn
           (amoebum:start-event-journal :journal j :event-bus bus)
           ;; Publish an event
           (amoebum:publish bus :test-jsonl
                            :source :jsonl-test
                            :severity :info
                            :payload "hello-journal")
           (sleep 0.1)
           ;; Force flush
           (amoebum:stop-event-journal j)
           ;; Read the segment file
           (let ((paths (amoebum:journal-segment-paths j)))
             (when (and paths (first paths) (probe-file (first paths)))
               (let ((content (uiop:read-file-string (first paths))))
                 ;; Should contain JSON with our event
                 (is (search "TEST-JSONL" (string-upcase content)))
                 ;; Each line should be a JSON object
                 (let ((lines (remove-if
                               (lambda (s) (zerop (length (string-trim '(#\Space) s))))
                               (uiop:split-string content :separator '(#\Newline)))))
                   (is (>= (length lines) 1))
                   ;; Each line should start with { and end with }
                   (dolist (line lines)
                     (is (char= #\{ (char line 0)))
                     (is (char= #\} (char line (1- (length line)))))))))))
      ;; Cleanup
      (ignore-errors (amoebum:stop-event-journal j))
      (ignore-errors
        (dolist (path (directory (merge-pathnames "*.jsonl" dir)))
          (delete-file path))
        (uiop:delete-empty-directory dir)))))

;;; --- Statistics when no journal ---

(test journal-statistics-nil-journal
  "journal-statistics returns sane defaults when no journal exists."
  (let ((stats (amoebum:journal-statistics nil)))
    (is (listp stats))
    (is (not (getf stats :running-p)))
    (is (= 0 (getf stats :total-events)))))

;;; --- Reset journal ---

(test reset-event-journal-clears-global
  "reset-event-journal clears the global journal."
  (let ((amoebum:*event-journal* nil))
    (amoebum:reset-event-journal)
    (is (null amoebum:*event-journal*))))
