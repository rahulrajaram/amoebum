(in-package :amoebum)

(defparameter *web-search-default-limit* 5)
(defparameter *web-search-max-limit* 10)
(defparameter *web-search-rate-limit-seconds* 1.0d0)
(defparameter *web-search-rate-limit-lock*
  (bordeaux-threads:make-lock "amoebum-web-search-rate-limit"))
(defparameter *web-search-last-request-at* 0.0d0)
(defparameter *web-search-http-get-runner* nil)
(defparameter *web-search-default-timeout-seconds* 20)
(defparameter *web-search-default-duckduckgo-url* "https://duckduckgo.com/html/")
(defparameter *web-search-default-user-agent* "amoebum-web-search/0.1")

(defparameter *web-fetch-http-get-runner* nil)
(defparameter *web-fetch-default-timeout-seconds* 20)
(defparameter *web-fetch-default-max-markdown-bytes* 10240)
(defparameter *web-fetch-default-user-agent* "amoebum-web-fetch/0.1")

(defun %web-trim (value)
  (string-trim '(#\Space #\Tab #\Newline #\Return) (or value "")))

(defun %web-empty-string-p (value)
  (zerop (length (%web-trim value))))

(defun %web-normalize-space (value)
  (let ((collapsed (cl-ppcre:regex-replace-all "\\s+" (or value "") " ")))
    (%web-trim collapsed)))

;; Monotonic time delegated to monotonic-seconds in util.lisp

(defun %web-acquire-rate-limit-slot ()
  (bordeaux-threads:with-lock-held (*web-search-rate-limit-lock*)
    (let* ((now (monotonic-seconds))
           (elapsed (- now *web-search-last-request-at*))
           (remaining (- *web-search-rate-limit-seconds* elapsed))
           (slept 0.0d0))
      (when (> remaining 0.0d0)
        (sleep remaining)
        (setf slept remaining
              now (monotonic-seconds)))
      (setf *web-search-last-request-at* now)
      slept)))

(defun %web-safe-parse-integer (value &optional (default 0))
  (handler-case
      (parse-integer (%web-trim value))
    (error () default)))

(defun %web-curl-meta-marker ()
  "AMOEBUM_META:")

(defun %web-split-curl-output (text)
  (let* ((payload (or text ""))
         (marker (%web-curl-meta-marker))
         (position (search marker payload :from-end t :test #'char=)))
    (unless position
      (error "Unable to parse curl metadata marker from output."))
    (let* ((body (subseq payload 0 position))
           (metadata (subseq payload (+ position (length marker))))
           (parts (cl-ppcre:split "\\t" metadata))
           (status-text (or (first parts) "0"))
           (effective-url (or (second parts) ""))
           (content-type (or (third parts) "")))
      (values body status-text (%web-trim effective-url) (%web-trim content-type)))))

(defun %web-http-query-argument (key value)
  (format nil "~A=~A" key (or value "")))

(defun %web-default-http-get-runner (url &key params timeout-seconds user-agent)
  (let* ((timeout (max 1 (or timeout-seconds *web-search-default-timeout-seconds*)))
         (agent (if (%web-empty-string-p user-agent)
                    *web-search-default-user-agent*
                    user-agent))
         (query-arguments
           (mapcan (lambda (entry)
                     (list "--data-urlencode"
                           (%web-http-query-argument (car entry) (cdr entry))))
                   params))
         (command
           (append (list "curl"
                         "-L"
                         "-sS"
                         "--max-time" (write-to-string timeout)
                         "--connect-timeout" (write-to-string (min timeout 10))
                         "--user-agent" agent
                         "--get")
                   query-arguments
                   (list "--write-out"
                         (format nil "~A%{http_code}~C%{url_effective}~C%{content_type}"
                                 (%web-curl-meta-marker)
                                 #\Tab
                                 #\Tab)
                         url))))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program command
                          :ignore-error-status t
                          :output :string
                          :error-output :string)
      (unless (zerop (or exit-code 0))
        (error "HTTP GET failed (~{~A~^ ~}): ~A"
               command
               (%web-trim (if (%web-empty-string-p stderr) stdout stderr))))
      (multiple-value-bind (body status-text effective-url content-type)
          (%web-split-curl-output stdout)
        (list :status (%web-safe-parse-integer status-text 0)
              :body body
              :url (if (%web-empty-string-p effective-url) url effective-url)
              :effective-url (if (%web-empty-string-p effective-url) url effective-url)
              :content-type content-type)))))

(defun %web-http-get (url &key params timeout-seconds user-agent (respect-rate-limit t))
  (when respect-rate-limit
    (%web-acquire-rate-limit-slot))
  (let* ((runner (or *web-search-http-get-runner*
                     #'%web-default-http-get-runner))
         (response (funcall runner
                            url
                            :params params
                            :timeout-seconds timeout-seconds
                            :user-agent user-agent))
         (status (getf response :status)))
    (unless (and (integerp status) (<= 200 status 299))
      (error "HTTP GET returned status ~S for ~A." status url))
    response))

(defun %web-string-suffix-p (suffix value)
  (let ((suffix-length (length suffix))
        (value-length (length value)))
    (and (<= suffix-length value-length)
         (string= suffix value
                  :start1 0
                  :end1 suffix-length
                  :start2 (- value-length suffix-length)
                  :end2 value-length))))

(defun %web-string-prefix-p (prefix value)
  (let ((prefix-length (length prefix))
        (value-length (length value)))
    (and (<= prefix-length value-length)
         (string= prefix value
                  :start1 0
                  :end1 prefix-length
                  :start2 0
                  :end2 prefix-length))))

(defun %web-normalize-domain (domain)
  (let ((trimmed (string-downcase (%web-trim domain))))
    (cond
      ((zerop (length trimmed)) nil)
      ((char= (char trimmed 0) #\.) (subseq trimmed 1))
      (t trimmed))))

(defun %web-sequence->list (value)
  (cond
    ((null value) nil)
    ((stringp value) (list value))
    ((listp value) value)
    ((vectorp value) (coerce value 'list))
    (t nil)))

(defun %web-url-domain (url)
  (when (stringp url)
    (cl-ppcre:register-groups-bind (host)
        ("^[A-Za-z][A-Za-z0-9+.-]*://([^/?#:]+)" url)
      (let* ((port-position (position #\: host))
             (without-port (if port-position
                               (subseq host 0 port-position)
                               host)))
        (string-downcase without-port)))))

(defun %web-strip-html-tags (value)
  (cl-ppcre:regex-replace-all "<[^>]+>" (or value "") " "))

(defun %web-decode-basic-entities (value)
  (let ((decoded (or value "")))
    (setf decoded (cl-ppcre:regex-replace-all "&amp;" decoded "&"))
    (setf decoded (cl-ppcre:regex-replace-all "&lt;" decoded "<"))
    (setf decoded (cl-ppcre:regex-replace-all "&gt;" decoded ">"))
    (setf decoded (cl-ppcre:regex-replace-all "&quot;" decoded "\""))
    (setf decoded (cl-ppcre:regex-replace-all "&#39;" decoded "'"))
    (setf decoded (cl-ppcre:regex-replace-all "&#x27;" decoded "'"))
    (setf decoded (cl-ppcre:regex-replace-all "&nbsp;" decoded " "))
    decoded))

(defun %web-normalize-html-text (value)
  (%web-normalize-space (%web-decode-basic-entities (%web-strip-html-tags value))))

(defun %web-resolve-positive-integer (candidate fallback)
  (if (and (integerp candidate) (> candidate 0))
      candidate
      fallback))
