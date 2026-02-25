(in-package :amoebum)

(defstruct (webhook-config
            (:constructor make-webhook-config
                (&key
                   (url "")
                   (method :post)
                   (headers '())
                   secret)))
  (url "" :type string)
  (method :post :type keyword)
  (headers '() :type list)
  secret)

(defclass webhook-backend (notification-backend)
  ((webhook-config
    :initarg :webhook-config
    :accessor webhook-backend-config
    :type webhook-config)))

(defparameter *webhook-fetch-function* #'pseudopod:fetch-url)
(defparameter *webhook-sleep-function* #'sleep)
(defparameter *webhook-http-request-function* nil)

(defun %webhook-parse-curl-output (payload)
  (let* ((text (or payload ""))
         (marker "AMOEBUM_WEBHOOK_META:")
         (position (search marker text :from-end t :test #'char=)))
    (unless position
      (error "Unable to parse webhook curl metadata marker from output."))
    (let* ((body (subseq text 0 position))
           (metadata (subseq text (+ position (length marker))))
           (parts (cl-ppcre:split "\\t" metadata))
           (status (handler-case
                       (parse-integer (or (first parts) "0"))
                     (error () 0)))
           (effective-url (string-trim '(#\Space #\Tab #\Newline #\Return)
                                       (or (second parts) "")))
           (content-type (string-trim '(#\Space #\Tab #\Newline #\Return)
                                      (or (third parts) ""))))
      (values body status effective-url content-type))))

(defun %webhook-shell-http-request (url &key timeout-seconds user-agent max-body-bytes
                                       follow-redirects method headers content)
  (declare (ignore max-body-bytes))
  (let* ((timeout (if (and (integerp timeout-seconds) (> timeout-seconds 0))
                      timeout-seconds
                      10))
         (agent (if (and (stringp user-agent)
                         (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                     user-agent))))
                    user-agent
                    "amoebum-webhook/0.1"))
         (resolved-method (string-upcase
                           (string-trim '(#\Space #\Tab #\Newline #\Return)
                                        (or method "POST"))))
         (command
           (append
            (list "curl" "-sS")
            (when follow-redirects
              (list "-L"))
            (list "--request" resolved-method
                  "--max-time" (write-to-string timeout)
                  "--connect-timeout" (write-to-string (min timeout 10))
                  "--user-agent" agent)
            (loop for (header-name . header-value) in headers
                  append (list "-H" (format nil "~A: ~A" header-name header-value)))
            (list "--data-binary" (or content "")
                  "--write-out"
                  (format nil "AMOEBUM_WEBHOOK_META:%{http_code}~C%{url_effective}~C%{content_type}"
                          #\Tab
                          #\Tab)
                  url))))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program command
                          :ignore-error-status t
                          :output :string
                          :error-output :string)
      (unless (zerop (or exit-code 0))
        (error 'pseudopod:pseudopod-fetch-error
               :url url
               :message (if (string= (or stderr "") "")
                            (or stdout "")
                            (or stderr ""))))
      (multiple-value-bind (body status effective-url content-type)
          (%webhook-parse-curl-output stdout)
        (list :status status
              :body body
              :effective-url (if (string= (or effective-url "") "") url effective-url)
              :content-type content-type)))))

(defun %coerce-webhook-method (value)
  (cond
    ((keywordp value) value)
    ((stringp value)
     (intern (string-upcase value) :keyword))
    ((symbolp value)
     (intern (string-upcase (symbol-name value)) :keyword))
    (t :post)))

(defun %webhook-header-value (entry)
  (cond
    ((stringp entry) entry)
    ((symbolp entry) (string-downcase (symbol-name entry)))
    (t (princ-to-string entry))))

(defun %normalize-webhook-headers (headers)
  (let ((raw-list
          (cond
            ((null headers) '())
            ((listp headers) headers)
            ((vectorp headers) (coerce headers 'list))
            (t '()))))
    (loop for entry in raw-list
          for key = (and (consp entry) (car entry))
          for value = (and (consp entry) (cdr entry))
          when (and key value)
            collect (cons (%webhook-header-value key)
                          (%webhook-header-value value)))))

(defun %octets-to-lower-hex (octets)
  (with-output-to-string (stream)
    (loop for byte across octets do
      (format stream "~2,'0x" byte))))

(defun %webhook-signature-hex (secret payload-json)
  (let* ((key (babel:string-to-octets (or secret "") :encoding :utf-8))
         (payload (babel:string-to-octets (or payload-json "") :encoding :utf-8))
         (hmac (ironclad:make-hmac key :sha256)))
    (ironclad:update-hmac hmac payload)
    (%octets-to-lower-hex (ironclad:hmac-digest hmac))))

(defun %webhook-entry-value (entry key)
  (cond
    ((hash-table-p entry)
     (or (gethash key entry)
         (and (keywordp key)
              (gethash (string-downcase (symbol-name key)) entry))
         (and (keywordp key)
              (gethash (symbol-name key) entry))))
    ((and (listp entry)
          (keywordp key))
     (or (getf entry key)
         (cdr (assoc key entry :test #'eq))
         (cdr (assoc (string-downcase (symbol-name key)) entry :test #'equal))
         (cdr (assoc (symbol-name key) entry :test #'equal))))
    (t nil)))

(defun %coerce-webhook-config (entry)
  (cond
    ((webhook-config-p entry) entry)
    (t
     (let* ((url (or (%webhook-entry-value entry :url) ""))
            (method (%coerce-webhook-method (%webhook-entry-value entry :method)))
            (headers (%normalize-webhook-headers (%webhook-entry-value entry :headers)))
            (secret (%webhook-entry-value entry :secret)))
       (make-webhook-config
        :url (if (stringp url) url (princ-to-string url))
        :method method
        :headers headers
        :secret (if (null secret) nil (princ-to-string secret)))))))

(defun %notification-json-payload (notification)
  (let* ((source-event (notification-source-event notification))
         (payload (make-hash-table :test #'equal)))
    (setf (gethash "title" payload) (notification-title notification)
          (gethash "body" payload) (notification-body notification)
          (gethash "severity" payload) (string-downcase (symbol-name (notification-severity notification)))
          (gethash "category" payload) (string-downcase (symbol-name (notification-category notification)))
          (gethash "urgency" payload) (string-downcase (symbol-name (notification-urgency notification)))
          (gethash "timestamp" payload) (notification-timestamp notification))
    (when source-event
      (let ((event-hash (make-hash-table :test #'equal)))
        (setf (gethash "type" event-hash)
              (string-downcase (symbol-name (event-type source-event)))
              (gethash "seq" event-hash) (event-seq source-event)
              (gethash "timestamp" event-hash) (event-timestamp source-event)
              (gethash "payload" event-hash) (prin1-to-string (event-payload source-event)))
        (setf (gethash "event" payload) event-hash)))
    (jonathan:to-json payload)))

(defun %webhook-request-headers (config payload-json)
  (let ((headers
          (append
           '(("Content-Type" . "application/json"))
           (%normalize-webhook-headers (webhook-config-headers config)))))
    (if (and (stringp (webhook-config-secret config))
             (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                         (webhook-config-secret config)))))
        (append headers
                (list (cons "X-Amoebum-Signature-256"
                            (format nil "sha256=~A"
                                    (%webhook-signature-hex (webhook-config-secret config)
                                                            payload-json)))))
        headers)))

(defun send-webhook-notification (config notification)
  (let* ((resolved-config (if (webhook-config-p config)
                              config
                              (%coerce-webhook-config config)))
         (url (string-trim '(#\Space #\Tab #\Newline #\Return)
                           (or (webhook-config-url resolved-config) "")))
         (method (string-downcase (symbol-name (%coerce-webhook-method
                                                (webhook-config-method resolved-config)))))
         (payload-json (%notification-json-payload notification))
         (headers (%webhook-request-headers resolved-config payload-json))
         (backoff-seconds '(1 2 4)))
    (if (zerop (length url))
        (values nil :missing-url)
        (loop for retry-index from 0 do
          (handler-case
              (progn
                (funcall *webhook-fetch-function*
                         url
                         :timeout-seconds 10
                         :cache-ttl-seconds 0
                         :follow-redirects t
                         :http-get-fn
                         (lambda (request-url &key timeout-seconds user-agent max-body-bytes follow-redirects)
                           (funcall (or *webhook-http-request-function*
                                        #'%webhook-shell-http-request)
                                    request-url
                                    :timeout-seconds timeout-seconds
                                    :user-agent user-agent
                                    :max-body-bytes max-body-bytes
                                    :follow-redirects follow-redirects
                                    :method method
                                    :headers headers
                                    :content payload-json)))
                (return (values t nil)))
            (pseudopod:pseudopod-fetch-http-error (condition)
              (let ((status (pseudopod:pseudopod-fetch-error-status-code condition)))
                (if (and (integerp status)
                         (<= 500 status 599)
                         (< retry-index 3))
                    (funcall *webhook-sleep-function* (nth retry-index backoff-seconds))
                    (return (values nil (princ-to-string condition))))))
            (error (condition)
              (return (values nil (princ-to-string condition)))))))))

(defmethod notify-available-p ((backend webhook-backend))
  (let ((config (webhook-backend-config backend)))
    (and (webhook-config-p config)
         (stringp (webhook-config-url config))
         (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                     (webhook-config-url config)))))))

(defmethod notify-send ((backend webhook-backend) (notification notification))
  (if (not (notify-available-p backend))
      (values nil :backend-unavailable)
      (send-webhook-notification (webhook-backend-config backend)
                                 notification)))

(defun make-webhook-backend (&key config webhook-config (enabled-p t))
  (let* ((cfg (or config (current-config)))
         (resolved (%coerce-webhook-config webhook-config)))
    (make-instance 'webhook-backend
                   :name :webhook
                   :enabled-p enabled-p
                   :config cfg
                   :webhook-config resolved)))

(defun make-webhook-backends (&key config (enabled-p t))
  (let* ((cfg (or config (current-config)))
         (raw (config-value :notification-webhooks cfg))
         (entries
           (cond
             ((null raw) '())
             ((listp raw) raw)
             ((vectorp raw) (coerce raw 'list))
             (t (list raw)))))
    (loop for entry in entries
          for resolved = (%coerce-webhook-config entry)
          when (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                           (webhook-config-url resolved))))
            collect (make-webhook-backend :config cfg
                                          :enabled-p enabled-p
                                          :webhook-config resolved))))
