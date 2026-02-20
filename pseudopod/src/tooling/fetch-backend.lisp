(in-package :pseudopod)

;;; ---------------------------------------------------------------------------
;;; I123 foundation: Fetch backend adapter used by amoebum web-fetch orchestration.
;;;
;;; Stable transport layer for single-URL HTTP fetches with structured errors
;;; and normalized FETCH-RESPONSE output.
;;; ---------------------------------------------------------------------------

(defparameter *default-fetch-timeout-seconds* 20
  "Default fetch timeout in seconds.")

(defparameter *default-fetch-user-agent* "pseudopod-fetch/0.1"
  "Default User-Agent for fetch requests.")

(define-condition pseudopod-fetch-error (pseudopod-error)
  ((status-code :initarg :status-code
                :initform nil
                :reader pseudopod-fetch-error-status-code)
   (url :initarg :url
        :initform nil
        :reader pseudopod-fetch-error-url))
  (:report (lambda (condition stream)
             (format stream "Fetch backend error~@[ status=~A~]~@[ url=~A~]: ~A"
                     (pseudopod-fetch-error-status-code condition)
                     (pseudopod-fetch-error-url condition)
                     (or (pseudopod-error-message condition)
                         "unknown fetch backend error")))))

(defstruct (fetch-response (:constructor %make-fetch-response))
  "Stable fetch response schema returned by FETCH-BACKEND."
  (url "" :type string)
  (effective-url "" :type string)
  (status 0 :type integer)
  (body "" :type string)
  (content-type "" :type string)
  (fetched-at 0 :type integer))

(defun %fetch-trim (value)
  (string-trim '(#\Space #\Tab #\Newline #\Return) (or value "")))

(defun %fetch-empty-string-p (value)
  (zerop (length (%fetch-trim value))))

(defun %fetch-safe-parse-integer (value &optional (default 0))
  (handler-case
      (parse-integer (%fetch-trim value))
    (error () default)))

(defun %fetch-curl-meta-marker ()
  "PSEUDOPOD_FETCH_META:")

(defun %fetch-split-curl-output (text)
  (let* ((payload (or text ""))
         (marker (%fetch-curl-meta-marker))
         (position (search marker payload :from-end t :test #'char=)))
    (unless position
      (error "Unable to parse curl metadata marker from output."))
    (let* ((body (subseq payload 0 position))
           (metadata (subseq payload (+ position (length marker))))
           (parts (cl-ppcre:split "\\t" metadata))
           (status-text (or (first parts) "0"))
           (effective-url (%fetch-trim (or (second parts) "")))
           (content-type (%fetch-trim (or (third parts) ""))))
      (values body
              (%fetch-safe-parse-integer status-text 0)
              effective-url
              content-type))))

(defun %default-fetch-http-get (url &key timeout-seconds user-agent)
  (let* ((timeout (max 1 (or timeout-seconds *default-fetch-timeout-seconds*)))
         (agent (if (%fetch-empty-string-p user-agent)
                    *default-fetch-user-agent*
                    user-agent))
         (command (list "curl"
                        "-L"
                        "-sS"
                        "--max-time" (write-to-string timeout)
                        "--connect-timeout" (write-to-string (min timeout 10))
                        "--user-agent" agent
                        "--write-out"
                        (format nil "~A%{http_code}~C%{url_effective}~C%{content_type}"
                                (%fetch-curl-meta-marker)
                                #\Tab
                                #\Tab)
                        url)))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program command
                          :ignore-error-status t
                          :output :string
                          :error-output :string)
      (unless (zerop (or exit-code 0))
        (error 'pseudopod-fetch-error
               :url url
               :status-code nil
               :message (format nil "Fetch HTTP GET failed (~{~A~^ ~}) [exit=~A]: ~A"
                                command
                                (or exit-code 0)
                                (%fetch-trim
                                 (if (%fetch-empty-string-p stderr) stdout stderr)))))
      (multiple-value-bind (body status effective-url content-type)
          (%fetch-split-curl-output stdout)
        (list :status status
              :body body
              :effective-url (if (%fetch-empty-string-p effective-url) url effective-url)
              :content-type content-type)))))

(defun fetch-backend (url &key
                            (timeout-seconds *default-fetch-timeout-seconds*)
                            (user-agent *default-fetch-user-agent*)
                            http-get-fn)
  "Fetch URL and return a normalized FETCH-RESPONSE struct."
  (check-type url string)
  (let* ((normalized-url (%fetch-trim url))
         (runner (or http-get-fn #'%default-fetch-http-get)))
    (when (%fetch-empty-string-p normalized-url)
      (error 'pseudopod-fetch-error
             :url normalized-url
             :status-code nil
             :message "Fetch URL must not be empty."))
    (let* ((response
             (handler-case
                 (funcall runner
                          normalized-url
                          :timeout-seconds timeout-seconds
                          :user-agent user-agent)
               (pseudopod-fetch-error (condition)
                 (error condition))
               (error (condition)
                 (error 'pseudopod-fetch-error
                        :url normalized-url
                        :status-code nil
                        :cause condition
                        :message (princ-to-string condition)))))
           (status (getf response :status))
           (body (or (getf response :body) ""))
           (effective-url (or (getf response :effective-url) normalized-url))
           (content-type (or (getf response :content-type) "")))
      (unless (and (integerp status) (<= 200 status 299))
        (error 'pseudopod-fetch-error
               :url normalized-url
               :status-code status
               :message (format nil "Fetch backend returned HTTP status ~A." status)))
      (%make-fetch-response
       :url normalized-url
       :effective-url effective-url
       :status status
       :body body
       :content-type content-type
       :fetched-at (get-universal-time)))))
