(in-package :amoebum)

(defparameter +audit-log-max-bytes+ (* 10 1024 1024))

(defun %audit-log-normalize-event-type (event-type)
  (string-downcase
   (etypecase event-type
     (keyword (symbol-name event-type))
     (symbol (symbol-name event-type))
     (string event-type))))

(defun %audit-log-universal->iso8601 (timestamp)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time timestamp 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour minute second)))

(defun %audit-log-iso8601->universal (text)
  (when (and (stringp text) (>= (length text) 20))
    (handler-case
        (let ((year (parse-integer text :start 0 :end 4))
              (month (parse-integer text :start 5 :end 7))
              (day (parse-integer text :start 8 :end 10))
              (hour (parse-integer text :start 11 :end 13))
              (minute (parse-integer text :start 14 :end 16))
              (second (parse-integer text :start 17 :end 19)))
          (encode-universal-time second minute hour day month year 0))
      (error ()
        nil))))

(defun %audit-log-event-session-id (event)
  (let ((payload (event-payload event)))
    (or (typecase payload
          (tool-invoked-payload (tool-invoked-payload-request-id payload))
          (tool-completed-payload (tool-completed-payload-request-id payload))
          (tool-error-payload (tool-error-payload-request-id payload))
          (plan-step-status-payload (plan-step-status-payload-run-id payload))
          (t nil))
        (and (keywordp (event-source event))
             (not (eq (event-source event) :unknown))
             (string-downcase (symbol-name (event-source event))))
        nil)))

(defun %audit-log-event-data (event)
  (let ((data (make-hash-table :test #'equal)))
    (setf (gethash "source" data)
          (if (keywordp (event-source event))
              (string-downcase (symbol-name (event-source event)))
              (princ-to-string (event-source event))))
    (setf (gethash "severity" data)
          (if (keywordp (event-severity event))
              (string-downcase (symbol-name (event-severity event)))
              (princ-to-string (event-severity event))))
    (setf (gethash "seq" data) (event-seq event))
    (setf (gethash "payload" data)
          (let ((payload (event-payload event)))
            (if payload
                (prin1-to-string payload)
                nil)))
    data))

(defun %audit-log-json-object (event &optional timestamp-override)
  (let ((object (make-hash-table :test #'equal))
        (timestamp (or timestamp-override
                       (event-timestamp event)
                       (get-universal-time))))
    (setf (gethash "timestamp" object)
          (%audit-log-universal->iso8601 timestamp))
    (setf (gethash "event-type" object)
          (%audit-log-normalize-event-type (event-type event)))
    (setf (gethash "session-id" object)
          (%audit-log-event-session-id event))
    (setf (gethash "data" object) (%audit-log-event-data event))
    object))

(defun %audit-log-json-line (event &optional timestamp-override)
  (jonathan:to-json (%audit-log-json-object event timestamp-override)))

(defun %audit-log-file-size-bytes (path)
  (if (probe-file path)
      (with-open-file (stream path
                              :direction :input
                              :element-type '(unsigned-byte 8))
        (file-length stream))
      0))

(defun %audit-log-rotation-path (path)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (let* ((base-name (format nil "events-~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0D"
                              year month day hour minute second))
           (candidate (make-pathname :name base-name :type "jsonl" :defaults path))
           (index 1))
      (loop while (probe-file candidate) do
        (setf candidate
              (make-pathname
               :name (format nil "~A-~D" base-name index)
               :type "jsonl"
               :defaults path))
        (incf index))
      candidate)))

(defun %audit-log-rotate-if-needed (path next-line)
  (let* ((current-size (%audit-log-file-size-bytes path))
         (next-size (+ current-size (length next-line) 1)))
    (when (and (probe-file path)
               (> next-size +audit-log-max-bytes+))
      (rename-file path (%audit-log-rotation-path path)))))

(defun audit-log-write-event (backend event &key timestamp)
  (let* ((path (or (log-backend-path backend)
                   (%default-log-path)))
         (line (%audit-log-json-line event timestamp)))
    (bordeaux-threads:with-lock-held ((log-backend-lock backend))
      (ensure-directories-exist path)
      (%audit-log-rotate-if-needed path line)
      (with-open-file (stream path
                              :direction :output
                              :if-exists :append
                              :if-does-not-exist :create)
        (write-line line stream)
        (finish-output stream)))
    t))

(defun %audit-log-event-matches-p (event &key event-type session-id start-time end-time)
  (let* ((event-type-value (gethash "event-type" event))
         (session-id-value (gethash "session-id" event))
         (timestamp-value (gethash "timestamp" event))
         (timestamp-universal (%audit-log-iso8601->universal timestamp-value)))
    (and (or (null event-type)
             (string-equal event-type-value
                           (%audit-log-normalize-event-type event-type)))
         (or (null session-id)
             (string= (or session-id-value "")
                      (princ-to-string session-id)))
         (or (null start-time)
             (and timestamp-universal
                  (>= timestamp-universal start-time)))
         (or (null end-time)
             (and timestamp-universal
                  (<= timestamp-universal end-time))))))

(defun audit-log-query (&key
                          (path (%default-log-path))
                          event-type
                          session-id
                          start-time
                          end-time)
  (let ((results '()))
    (when (probe-file path)
      (with-open-file (stream path :direction :input :if-does-not-exist nil)
        (loop for line = (read-line stream nil nil)
              while line do
                (when (> (length line) 0)
                  (handler-case
                      (let ((event (jonathan:parse line :as :hash-table)))
                        (when (%audit-log-event-matches-p
                               event
                               :event-type event-type
                               :session-id session-id
                               :start-time start-time
                               :end-time end-time)
                          (push event results)))
                    (error ()
                      nil))))))
    (nreverse results)))

(defmethod notify-send ((backend log-backend) (notification notification))
  (let ((source-event (notification-source-event notification))
        (timestamp (notification-timestamp notification)))
    (handler-case
        (if source-event
            (progn
              (audit-log-write-event backend source-event :timestamp timestamp)
              (values t nil))
            (values nil "notification missing source event"))
      (error (condition)
        (ptui.util.log:log-warn "audit log backend failed for ~A: ~A"
                                (log-backend-path backend)
                                condition)
        (values nil (princ-to-string condition))))))
