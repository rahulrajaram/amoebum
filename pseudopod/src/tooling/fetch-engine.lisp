(in-package :pseudopod)

;;; ---------------------------------------------------------------------------
;;; I123: Fetch engine
;;;
;;; Remote content retrieval primitive with configurable timeout, redirect
;;; tracking, bounded body truncation, lightweight TTL cache, and structured
;;; error handling for timeout and HTTP status failures.
;;; ---------------------------------------------------------------------------

(defparameter *default-fetch-timeout-seconds* 20
  "Default HTTP timeout in seconds for FETCH-URL.")

(defparameter *default-fetch-cache-ttl-seconds* 900
  "Default fetch cache TTL in seconds.")

(defparameter *default-fetch-max-body-bytes* 262144
  "Default maximum response body size retained in memory.")

(defparameter *default-fetch-user-agent* "pseudopod-fetch/0.1"
  "Default User-Agent for FETCH-URL requests.")

(defparameter *fetch-cache* (make-hash-table :test #'equal)
  "In-memory cache for fetch results keyed by URL and truncation settings.")

(define-condition pseudopod-fetch-error (pseudopod-error)
  ((url :initarg :url
        :initform nil
        :reader pseudopod-fetch-error-url)
   (status-code :initarg :status-code
                :initform nil
                :reader pseudopod-fetch-error-status-code)
   (effective-url :initarg :effective-url
                  :initform nil
                  :reader pseudopod-fetch-error-effective-url))
  (:report (lambda (condition stream)
             (format stream "Fetch error~@[ status=~A~]~@[ url=~A~]: ~A"
                     (pseudopod-fetch-error-status-code condition)
                     (or (pseudopod-fetch-error-effective-url condition)
                         (pseudopod-fetch-error-url condition))
                     (or (pseudopod-error-message condition)
                         "unknown fetch error")))))

(define-condition pseudopod-fetch-timeout (pseudopod-fetch-error)
  ((timeout-seconds :initarg :timeout-seconds
                    :initform nil
                    :reader pseudopod-fetch-timeout-seconds))
  (:report (lambda (condition stream)
             (format stream "Fetch timed out after ~A seconds for ~A"
                     (or (pseudopod-fetch-timeout-seconds condition) "?")
                     (or (pseudopod-fetch-error-url condition) "<unknown>")))))

(define-condition pseudopod-fetch-http-error (pseudopod-fetch-error)
  ((body :initarg :body
         :initform nil
         :reader pseudopod-fetch-http-error-body)
   (content-type :initarg :content-type
                 :initform nil
                 :reader pseudopod-fetch-http-error-content-type))
  (:report (lambda (condition stream)
             (format stream "Fetch HTTP status ~A for ~A"
                     (or (pseudopod-fetch-error-status-code condition) "?")
                     (or (pseudopod-fetch-error-effective-url condition)
                         (pseudopod-fetch-error-url condition)
                         "<unknown>")))))

(defstruct (fetch-result (:constructor %make-fetch-result))
  "Structured response produced by FETCH-URL."
  (url "" :type string)
  (effective-url "" :type string)
  (status-code 0 :type integer)
  (content-type "" :type string)
  (body "" :type string)
  (body-bytes 0 :type integer)
  (truncated-p nil :type (member t nil))
  (redirected-p nil :type (member t nil))
  (host-changed-p nil :type (member t nil))
  (cached-p nil :type (member t nil))
  (fetched-at 0 :type integer))

(defstruct (fetch-options
             (:constructor make-fetch-options
                 (&key
                   (timeout-seconds *default-fetch-timeout-seconds*)
                   (cache-ttl-seconds *default-fetch-cache-ttl-seconds*)
                   (max-body-bytes *default-fetch-max-body-bytes*)
                   (user-agent *default-fetch-user-agent*)
                   (follow-redirects t)
                   http-get-fn)))
  "Configuration bundle for FETCH-URL."
  (timeout-seconds *default-fetch-timeout-seconds* :type integer)
  (cache-ttl-seconds *default-fetch-cache-ttl-seconds* :type integer)
  (max-body-bytes *default-fetch-max-body-bytes* :type integer)
  (user-agent *default-fetch-user-agent* :type string)
  (follow-redirects t :type (member t nil))
  (http-get-fn nil :type t))

(defun %fetch-trim (value)
  (string-trim '(#\Space #\Tab #\Newline #\Return) (or value "")))

(defun %fetch-empty-string-p (value)
  (zerop (length (%fetch-trim value))))

(defun %fetch-safe-parse-integer (value &optional (default 0))
  (handler-case
      (parse-integer (%fetch-trim value))
    (error () default)))

(defun %fetch-url-domain (url)
  (when (stringp url)
    (cl-ppcre:register-groups-bind (host)
        ("^[A-Za-z][A-Za-z0-9+.-]*://([^/?#:]+)" url)
      (let* ((port-position (position #\: host))
             (without-port (if port-position
                               (subseq host 0 port-position)
                               host)))
        (string-downcase without-port)))))

(defun %fetch-host-changed-p (requested-url effective-url)
  (let ((requested-host (%fetch-url-domain requested-url))
        (effective-host (%fetch-url-domain effective-url)))
    (and requested-host
         effective-host
         (not (string= requested-host effective-host)))))

(defun %fetch-curl-meta-marker ()
  "PSEUDOPOD_FETCH_META:")

(defun %fetch-split-curl-output (payload)
  (let* ((text (or payload ""))
         (marker (%fetch-curl-meta-marker))
         (position (search marker text :from-end t :test #'char=)))
    (unless position
      (error "Unable to parse curl metadata marker from output."))
    (let* ((body (subseq text 0 position))
           (metadata (subseq text (+ position (length marker))))
           (parts (cl-ppcre:split "\\t" metadata))
           (status (%fetch-safe-parse-integer (or (first parts) "0") 0))
           (effective-url (%fetch-trim (or (second parts) "")))
           (content-type (%fetch-trim (or (third parts) ""))))
      (values body status effective-url content-type))))

(defun %fetch-default-http-get (url &key timeout-seconds user-agent max-body-bytes follow-redirects)
  (declare (ignore max-body-bytes))
  (let* ((timeout (max 1 (or timeout-seconds *default-fetch-timeout-seconds*)))
         (agent (if (%fetch-empty-string-p user-agent)
                    *default-fetch-user-agent*
                    user-agent))
         (command
           (append (list "curl"
                         "-sS")
                   (when follow-redirects (list "-L"))
                   (list "--max-time" (write-to-string timeout)
                         "--connect-timeout" (write-to-string (min timeout 10))
                         "--user-agent" agent
                         "--write-out"
                         (format nil "~A%{http_code}~C%{url_effective}~C%{content_type}"
                                 (%fetch-curl-meta-marker)
                                 #\Tab
                                 #\Tab)
                         url))))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program command
                          :ignore-error-status t
                          :output :string
                          :error-output :string)
      (unless (zerop (or exit-code 0))
        (if (= exit-code 28)
            (error 'pseudopod-fetch-timeout
                   :url url
                   :timeout-seconds timeout
                   :message (format nil "Fetch timed out after ~A seconds." timeout))
            (error 'pseudopod-fetch-error
                   :url url
                   :message (%fetch-trim (if (%fetch-empty-string-p stderr)
                                             stdout
                                             stderr)))))
      (multiple-value-bind (body status effective-url content-type)
          (%fetch-split-curl-output stdout)
        (list :status status
              :body body
              :effective-url (if (%fetch-empty-string-p effective-url) url effective-url)
              :content-type content-type)))))

(defun %fetch-string-byte-length (text)
  (handler-case
      (babel:string-size-in-octets (or text "") :encoding :utf-8)
    (error ()
      (length (or text "")))))

(defun %fetch-truncate-body (body max-body-bytes)
  (let* ((payload (or body ""))
         (limit (max 1 max-body-bytes)))
    (if (<= (%fetch-string-byte-length payload) limit)
        (values payload nil)
        ;; UTF-8 byte-aware truncation via binary search on string boundary.
        (let ((low 0)
              (high (length payload))
              (best 0))
          (loop while (<= low high) do
                (let* ((mid (floor (+ low high) 2))
                       (candidate (subseq payload 0 mid))
                       (candidate-bytes (%fetch-string-byte-length candidate)))
                  (if (<= candidate-bytes limit)
                      (setf best mid
                            low (1+ mid))
                      (setf high (1- mid)))))
          (values (subseq payload 0 best) t)))))

(defun %fetch-cache-key (url max-body-bytes follow-redirects user-agent)
  (list url max-body-bytes follow-redirects (%fetch-trim user-agent)))

(defun %fetch-copy-result (result &key (cached-p (fetch-result-cached-p result)))
  (%make-fetch-result :url (fetch-result-url result)
                      :effective-url (fetch-result-effective-url result)
                      :status-code (fetch-result-status-code result)
                      :content-type (fetch-result-content-type result)
                      :body (fetch-result-body result)
                      :body-bytes (fetch-result-body-bytes result)
                      :truncated-p (fetch-result-truncated-p result)
                      :redirected-p (fetch-result-redirected-p result)
                      :host-changed-p (fetch-result-host-changed-p result)
                      :cached-p cached-p
                      :fetched-at (fetch-result-fetched-at result)))

(defun %fetch-cache-get (cache-key now)
  (let ((entry (gethash cache-key *fetch-cache*)))
    (when entry
      (let ((expires-at (or (first entry) 0))
            (result (second entry)))
        (if (and (plusp expires-at) (>= now expires-at))
            (progn
              (remhash cache-key *fetch-cache*)
              nil)
            (%fetch-copy-result result :cached-p t))))))

(defun %fetch-cache-put (cache-key result now ttl-seconds)
  (when (and (integerp ttl-seconds) (> ttl-seconds 0))
    (setf (gethash cache-key *fetch-cache*)
          (list (+ now ttl-seconds)
                (%fetch-copy-result result :cached-p nil)))))

(defun %fetch-resolve-options (options)
  (let ((resolved (or options (make-fetch-options))))
    (check-type resolved fetch-options)
    (make-fetch-options
     :timeout-seconds (if (and (integerp (fetch-options-timeout-seconds resolved))
                               (> (fetch-options-timeout-seconds resolved) 0))
                          (fetch-options-timeout-seconds resolved)
                          *default-fetch-timeout-seconds*)
     :cache-ttl-seconds (if (and (integerp (fetch-options-cache-ttl-seconds resolved))
                                 (> (fetch-options-cache-ttl-seconds resolved) 0))
                            (fetch-options-cache-ttl-seconds resolved)
                            0)
     :max-body-bytes (if (and (integerp (fetch-options-max-body-bytes resolved))
                              (> (fetch-options-max-body-bytes resolved) 0))
                         (fetch-options-max-body-bytes resolved)
                         *default-fetch-max-body-bytes*)
     :user-agent (fetch-options-user-agent resolved)
     :follow-redirects (fetch-options-follow-redirects resolved)
     :http-get-fn (or (fetch-options-http-get-fn resolved)
                      #'%fetch-default-http-get))))

(defun %fetch-runner-response (url options)
  (handler-case
      (funcall (fetch-options-http-get-fn options)
               url
               :timeout-seconds (fetch-options-timeout-seconds options)
               :max-body-bytes (fetch-options-max-body-bytes options)
               :user-agent (fetch-options-user-agent options)
               :follow-redirects (fetch-options-follow-redirects options))
    (pseudopod-fetch-error (condition)
      (error condition))
    (error (condition)
      (error 'pseudopod-fetch-error
             :url url
             :cause condition
             :message (princ-to-string condition)))))

(defun %process-fetch-response (url response options now)
  (let* ((status (getf response :status 0))
         (effective-url (or (getf response :effective-url) url))
         (content-type (or (getf response :content-type) ""))
         (raw-body (or (getf response :body) "")))
    (unless (and (integerp status) (<= 100 status 599))
      (error 'pseudopod-fetch-error
             :url url
             :effective-url effective-url
             :message (format nil "Invalid HTTP status value: ~S" status)))
    (unless (<= 200 status 299)
      (error 'pseudopod-fetch-http-error
             :url url
             :effective-url effective-url
             :status-code status
             :body raw-body
             :content-type content-type
             :message (format nil "Fetch returned HTTP status ~A." status)))
    (multiple-value-bind (body truncated-p)
        (%fetch-truncate-body raw-body (fetch-options-max-body-bytes options))
      (%make-fetch-result
       :url url
       :effective-url effective-url
       :status-code status
       :content-type content-type
       :body body
       :body-bytes (%fetch-string-byte-length body)
       :truncated-p truncated-p
       :redirected-p (not (string= url effective-url))
       :host-changed-p (%fetch-host-changed-p url effective-url)
       :cached-p nil
       :fetched-at now))))

(defun %fetch-url-cache-key (url options)
  (%fetch-cache-key url
                    (fetch-options-max-body-bytes options)
                    (fetch-options-follow-redirects options)
                    (fetch-options-user-agent options)))

(defun %fetch-url-cache-hit (url options now)
  (and (> (fetch-options-cache-ttl-seconds options) 0)
       (%fetch-cache-get (%fetch-url-cache-key url options) now)))

(defun %fetch-store-result (url options result now)
  (%fetch-cache-put (%fetch-url-cache-key url options)
                    result
                    now
                    (fetch-options-cache-ttl-seconds options))
  result)

(defun fetch-url (url &optional options)
  "Fetch URL and return a typed FETCH-RESULT.

Signals:
  - PSEUDOPOD-FETCH-TIMEOUT on timeout.
  - PSEUDOPOD-FETCH-HTTP-ERROR for non-2xx HTTP responses.
  - PSEUDOPOD-FETCH-ERROR for other failures."
  (check-type url string)
  (let* ((normalized-url (%fetch-trim url))
         (resolved-options (%fetch-resolve-options options)))
    (when (%fetch-empty-string-p normalized-url)
      (error 'pseudopod-fetch-error
             :url normalized-url
             :message "Fetch URL must not be empty."))
    (let ((now (get-universal-time)))
      (or (%fetch-url-cache-hit normalized-url resolved-options now)
          (%fetch-store-result
           normalized-url
           resolved-options
           (%process-fetch-response normalized-url
                                    (%fetch-runner-response normalized-url resolved-options)
                                    resolved-options
                                    now)
           now)))))
