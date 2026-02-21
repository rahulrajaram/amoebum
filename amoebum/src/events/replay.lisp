(in-package :amoebum)

;;; ============================================================
;;; I264: Event Replay Engine and Audit Query
;;;
;;; Replay journal segments into a target event bus with
;;; timing control, and query journals by type/time/severity/
;;; text with configurable output formats.
;;; ============================================================

;;; --- Journal line parsing ---

(defun %replay-parse-json-string (json key)
  "Extract a string value for KEY from a simple JSON object string.
   Only handles flat JSON objects with string/number values."
  (let* ((pattern (format nil "\"~A\"\\s*:\\s*" key))
         (match-start (cl-ppcre:scan pattern json)))
    (when match-start
      (let ((value-start (cl-ppcre:scan "[^:\\s]" json :start
                                        (+ match-start (length key) 3))))
        (when value-start
          (cond
            ;; Quoted string value
            ((char= (char json value-start) #\")
             (let ((end (position #\" json :start (1+ value-start))))
               (when end
                 (subseq json (1+ value-start) end))))
            ;; Numeric value
            (t
             (let ((end (or (position #\, json :start value-start)
                            (position #\} json :start value-start)
                            (length json))))
               (string-trim '(#\Space #\Tab) (subseq json value-start end))))))))))

(defun %replay-parse-journal-line (line)
  "Parse a JSONL line into a plist with :seq :ts :type :source :severity :payload."
  (handler-case
      (let ((seq-str (%replay-parse-json-string line "seq"))
            (ts-str (%replay-parse-json-string line "ts"))
            (type-str (%replay-parse-json-string line "type"))
            (source-str (%replay-parse-json-string line "source"))
            (severity-str (%replay-parse-json-string line "severity"))
            (payload-str (%replay-parse-json-string line "payload")))
        (list :seq (if seq-str (parse-integer seq-str :junk-allowed t) 0)
              :ts (if ts-str (parse-integer ts-str :junk-allowed t) 0)
              :type (if type-str (intern (string-upcase type-str) :keyword) :unknown)
              :source (if source-str (intern (string-upcase source-str) :keyword) :unknown)
              :severity (if severity-str (intern (string-upcase severity-str) :keyword) :info)
              :payload (or payload-str "")))
    (error () nil)))

;;; --- Journal segment reader ---

(defun %replay-read-segment (path)
  "Read all events from a journal segment file. Returns list of parsed plists."
  (when (and path (probe-file path))
    (let ((events '()))
      (with-open-file (in path :direction :input
                                :external-format :utf-8
                                :if-does-not-exist nil)
        (when in
          (loop for line = (read-line in nil nil)
                while line
                for trimmed = (string-trim '(#\Space #\Tab #\Return) line)
                when (and (plusp (length trimmed))
                          (char= #\{ (char trimmed 0)))
                  do (let ((parsed (%replay-parse-journal-line trimmed)))
                       (when parsed
                         (push parsed events))))))
      (nreverse events))))

;;; --- Filter predicates for replay/query ---

(defun %replay-make-filter (event-types time-from time-to severities text-search)
  "Build a predicate function for filtering parsed event plists."
  (lambda (evt)
    (and
     ;; Event type filter
     (or (null event-types)
         (member (getf evt :type) event-types :test #'eq))
     ;; Time range filter
     (or (null time-from)
         (>= (or (getf evt :ts) 0) time-from))
     (or (null time-to)
         (<= (or (getf evt :ts) 0) time-to))
     ;; Severity filter
     (or (null severities)
         (member (getf evt :severity) severities :test #'eq))
     ;; Text search in payload
     (or (null text-search)
         (search (string-upcase text-search)
                 (string-upcase (or (getf evt :payload) "")))))))

;;; --- Replay engine ---

(defun replay-journal (segment-paths &key
                                       from-seq to-seq
                                       event-types severities
                                       time-from time-to
                                       text-search
                                       (speed-factor 1.0)
                                       (target-bus (current-event-bus)))
  "Replay events from journal SEGMENT-PATHS into TARGET-BUS.
   Timing is scaled by SPEED-FACTOR (1.0 = real-time, 0.0 = instant).
   Returns count of replayed events."
  (let ((filter (%replay-make-filter event-types time-from time-to
                                     severities text-search))
        (all-events '())
        (count 0))
    ;; Collect all events from segments
    (dolist (path (if (listp segment-paths) segment-paths (list segment-paths)))
      (let ((events (%replay-read-segment path)))
        (setf all-events (nconc all-events events))))
    ;; Apply sequence range filter
    (when from-seq
      (setf all-events (remove-if (lambda (e)
                                    (< (or (getf e :seq) 0) from-seq))
                                  all-events)))
    (when to-seq
      (setf all-events (remove-if (lambda (e)
                                    (> (or (getf e :seq) 0) to-seq))
                                  all-events)))
    ;; Sort by sequence number
    (setf all-events (sort all-events #'< :key (lambda (e) (or (getf e :seq) 0))))
    ;; Replay with timing
    (let ((prev-ts nil))
      (dolist (evt all-events)
        (when (funcall filter evt)
          ;; Compute inter-event delay
          (when (and prev-ts (plusp speed-factor))
            (let* ((ts (or (getf evt :ts) 0))
                   (delta (- ts prev-ts)))
              (when (plusp delta)
                (sleep (/ delta speed-factor)))))
          (setf prev-ts (or (getf evt :ts) 0))
          ;; Publish to target bus
          (publish target-bus
                   (getf evt :type)
                   :source (getf evt :source)
                   :severity (getf evt :severity)
                   :payload (getf evt :payload))
          (incf count))))
    count))

;;; --- Audit query ---

(defun audit-query (segment-paths &key
                                    event-types severities
                                    time-from time-to
                                    text-search
                                    (max-results 100)
                                    (output-format :plist))
  "Query journal segments and return matching events.
   OUTPUT-FORMAT is :plist, :json, or :table.
   Returns list of matching events (limited by MAX-RESULTS)."
  (let ((filter (%replay-make-filter event-types time-from time-to
                                     severities text-search))
        (all-events '()))
    ;; Collect from all segments
    (dolist (path (if (listp segment-paths) segment-paths (list segment-paths)))
      (let ((events (%replay-read-segment path)))
        (setf all-events (nconc all-events events))))
    ;; Sort by sequence
    (setf all-events (sort all-events #'< :key (lambda (e) (or (getf e :seq) 0))))
    ;; Filter
    (let* ((filtered (remove-if-not filter all-events))
           (limited (if (> (length filtered) max-results)
                        (subseq filtered 0 max-results)
                        filtered)))
      ;; Format output
      (ecase output-format
        (:plist limited)
        (:json (%audit-format-json limited))
        (:table (%audit-format-table limited))))))

(defun %audit-format-json (events)
  "Format events as a JSON string (list of objects)."
  (with-output-to-string (out)
    (write-char #\[ out)
    (loop for (evt . rest) on events
          do (format out "{\"seq\":~D,\"ts\":~D,\"type\":\"~A\",\"source\":\"~A\",\"severity\":\"~A\",\"payload\":\"~A\"}"
                     (or (getf evt :seq) 0)
                     (or (getf evt :ts) 0)
                     (getf evt :type)
                     (getf evt :source)
                     (getf evt :severity)
                     (%journal-escape-string (or (getf evt :payload) "")))
          when rest do (write-char #\, out))
    (write-char #\] out)))

(defun %audit-format-table (events)
  "Format events as a printable table string."
  (with-output-to-string (out)
    (format out "~&~6A ~12A ~20A ~10A ~10A ~A~%"
            "SEQ" "TIMESTAMP" "TYPE" "SOURCE" "SEVERITY" "PAYLOAD")
    (format out "~A~%" (make-string 80 :initial-element #\-))
    (dolist (evt events)
      (format out "~6D ~12D ~20A ~10A ~10A ~A~%"
              (or (getf evt :seq) 0)
              (or (getf evt :ts) 0)
              (getf evt :type)
              (getf evt :source)
              (getf evt :severity)
              (let ((p (or (getf evt :payload) "")))
                (if (> (length p) 40)
                    (concatenate 'string (subseq p 0 37) "...")
                    p))))))

;;; --- Convenience: query active journal ---

(defun audit-current-journal (&key event-types severities time-from time-to
                                   text-search (max-results 100)
                                   (output-format :plist))
  "Query the currently active event journal."
  (let ((paths (journal-segment-paths *event-journal*)))
    (if paths
        (audit-query paths
                     :event-types event-types
                     :severities severities
                     :time-from time-from
                     :time-to time-to
                     :text-search text-search
                     :max-results max-results
                     :output-format output-format)
        nil)))

(defun replay-current-journal (&key from-seq to-seq event-types severities
                                    time-from time-to text-search
                                    (speed-factor 0.0)
                                    (target-bus (current-event-bus)))
  "Replay events from the currently active event journal."
  (let ((paths (journal-segment-paths *event-journal*)))
    (if paths
        (replay-journal paths
                        :from-seq from-seq :to-seq to-seq
                        :event-types event-types :severities severities
                        :time-from time-from :time-to time-to
                        :text-search text-search
                        :speed-factor speed-factor
                        :target-bus target-bus)
        0)))
