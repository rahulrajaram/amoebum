(in-package :amoebum)

(defvar *runtime-log-lock* (bt:make-lock "amoebum-runtime-log-lock"))
(defparameter +runtime-log-backtrace-frames+ 40)

(defun %runtime-log-trimmed-env (name)
  (let ((value (uiop:getenv name)))
    (let ((trimmed (and value
                        (string-trim '(#\Space #\Tab #\Newline #\Return)
                                     value))))
      (and trimmed
           (plusp (length trimmed))
           trimmed))))

(defun %runtime-log-iso8601-now ()
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour minute second)))

(defun %runtime-log-json-object (&rest pairs)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr do
          (setf (gethash key table) value))
    table))

(defun %runtime-log-json-encodable (value)
  (cond
    ((or (null value)
         (stringp value)
         (numberp value)
         (eq value t))
     value)
    ((pathnamep value)
     (namestring value))
    ((keywordp value)
     (string-downcase (symbol-name value)))
    ((symbolp value)
     (symbol-name value))
    ((hash-table-p value)
     (let ((table (make-hash-table :test #'equal)))
       (maphash (lambda (key item)
                  (setf (gethash (string-downcase
                                  (if (or (symbolp key) (keywordp key))
                                      (symbol-name key)
                                      (princ-to-string key)))
                                 table)
                        (%runtime-log-json-encodable item)))
                value)
       table))
    ((and (listp value) (evenp (length value)))
     (let ((table (make-hash-table :test #'equal)))
       (loop for (key item) on value by #'cddr do
         (setf (gethash (string-downcase
                         (if (or (symbolp key) (keywordp key))
                             (symbol-name key)
                             (princ-to-string key)))
                        table)
               (%runtime-log-json-encodable item)))
       table))
    ((listp value)
     (coerce (mapcar #'%runtime-log-json-encodable value) 'vector))
    ((vectorp value)
     (coerce (loop for item across value
                   collect (%runtime-log-json-encodable item))
             'vector))
    (t
     (princ-to-string value))))

(defun %runtime-log-path-from-text (text)
  (if text
      (pathname text)
      nil))

(defun default-runtime-log-path (&optional (home (user-homedir-pathname)))
  (merge-pathnames #P".amoebum/runtime/runtime.log"
                   home))

(defun default-crash-log-path (&optional (home (user-homedir-pathname)))
  (merge-pathnames #P".amoebum/runtime/crash.log"
                   home))

(defun runtime-log-path ()
  (or (%runtime-log-path-from-text
       (%runtime-log-trimmed-env "AMOEBUM_RUNTIME_LOG_FILE"))
      (default-runtime-log-path)))

(defun crash-log-path ()
  (or (%runtime-log-path-from-text
       (%runtime-log-trimmed-env "AMOEBUM_CRASH_LOG_FILE"))
      (merge-pathnames #P"crash.log"
                       (uiop:pathname-directory-pathname
                        (runtime-log-path)))
      (default-crash-log-path)))

(defun %runtime-log-backtrace-string ()
  (with-output-to-string (stream)
    #+sbcl
    (handler-case
        (sb-debug:print-backtrace :stream stream
                                  :count +runtime-log-backtrace-frames+
                                  :print-thread t
                                  :emergency-best-effort t)
      (error (condition)
        (format stream "backtrace unavailable: ~A" condition)))
    #-sbcl
    (write-string "backtrace unavailable on this Lisp implementation" stream)))

(defun %write-runtime-log-entry (path entry)
  (handler-case
      (bt:with-lock-held (*runtime-log-lock*)
        (ensure-directories-exist path)
        (with-open-file (stream path
                                :direction :output
                                :if-exists :append
                                :if-does-not-exist :create
                                :external-format :utf-8)
          (write-line (jonathan:to-json entry) stream)
          (finish-output stream))
        t)
    (error ()
      nil)))

(defun log-runtime-event (&key (level :info)
                               (kind "runtime-event")
                               (source :runtime)
                               message
                               details
                               (path (runtime-log-path)))
  (let ((entry (%runtime-log-json-object
                "timestamp" (%runtime-log-iso8601-now)
                "level" (%runtime-log-json-encodable level)
                "kind" (%runtime-log-json-encodable kind)
                "source" (%runtime-log-json-encodable source)
                "message" (or message "")
                "details" (%runtime-log-json-encodable details))))
    (%write-runtime-log-entry path entry)))

(defun log-runtime-condition (condition &key (level :error)
                                          (kind "runtime-condition")
                                          (source :runtime)
                                          message
                                          details
                                          (path (crash-log-path))
                                          (include-backtrace-p t))
  (let* ((condition-type (string-downcase
                          (symbol-name (class-name (class-of condition)))))
         (summary-details (append (or details '())
                                  (list :condition-type condition-type
                                        :crash-log-path path)))
         (entry (%runtime-log-json-object
                 "timestamp" (%runtime-log-iso8601-now)
                 "level" (%runtime-log-json-encodable level)
                 "kind" (%runtime-log-json-encodable kind)
                 "source" (%runtime-log-json-encodable source)
                 "message" (or message (princ-to-string condition))
                 "condition_type" condition-type
                 "condition" (princ-to-string condition)
                 "details" (%runtime-log-json-encodable details)
                 "backtrace" (and include-backtrace-p
                                  (%runtime-log-backtrace-string)))))
    (ignore-errors
      (log-runtime-event :level level
                         :kind kind
                         :source source
                         :message (or message (princ-to-string condition))
                         :details summary-details))
    (%write-runtime-log-entry path entry)))
