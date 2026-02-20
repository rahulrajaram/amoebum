(in-package :pseudopod)

;;; ---------------------------------------------------------------------------
;;; I121: Search backend adapter
;;;
;;; Stable backend adapter for web-search providers used by amoebum orchestration.
;;; Normalizes backend payloads into SEARCH-RESPONSE / SEARCH-HIT structs.
;;; ---------------------------------------------------------------------------

(defparameter *default-search-timeout-seconds* 20
  "Default backend timeout in seconds.")

(defparameter *default-search-rate-limit-seconds* 1.0d0
  "Minimum interval between backend requests in seconds.")

(defparameter *default-search-user-agent* "pseudopod-search/0.1"
  "Default User-Agent for backend requests.")

(defparameter *default-duckduckgo-search-url* "https://duckduckgo.com/html/"
  "Default DuckDuckGo HTML backend endpoint.")

(defparameter *search-last-request-at* 0.0d0
  "Monotonic timestamp of the most recent backend request.")

(define-condition pseudopod-search-error (pseudopod-error)
  ((backend :initarg :backend
            :initform nil
            :reader pseudopod-search-error-backend)
   (status-code :initarg :status-code
                :initform nil
                :reader pseudopod-search-error-status-code)
   (url :initarg :url
        :initform nil
        :reader pseudopod-search-error-url))
  (:report (lambda (condition stream)
             (format stream "Search backend error~@[ (~(~A~))~]~@[ status=~A~]~@[ url=~A~]: ~A"
                     (pseudopod-search-error-backend condition)
                     (pseudopod-search-error-status-code condition)
                     (pseudopod-search-error-url condition)
                     (or (pseudopod-error-message condition)
                         "unknown search backend error")))))

(define-condition pseudopod-search-parse-error (pseudopod-search-error)
  ((payload :initarg :payload
            :initform nil
            :reader pseudopod-search-parse-error-payload))
  (:report (lambda (condition stream)
             (format stream "Search parse error~@[ (~(~A~))~]: ~A"
                     (pseudopod-search-error-backend condition)
                     (or (pseudopod-error-message condition)
                         "unable to parse backend response")))))

(defstruct (search-hit (:constructor %make-search-hit))
  "Normalized web-search hit."
  (title "" :type string)
  (url "" :type string)
  (snippet "" :type string)
  (source-domain nil :type (or null string))
  (rank 0 :type integer))

(defstruct (search-response (:constructor %make-search-response))
  "Stable backend response schema returned by SEARCH-BACKEND."
  (backend :unknown :type keyword)
  (query "" :type string)
  (results '() :type list)
  (result-count 0 :type integer)
  (fetched-at 0 :type integer))

(defun %search-trim (value)
  (string-trim '(#\Space #\Tab #\Newline #\Return) (or value "")))

(defun %search-empty-string-p (value)
  (zerop (length (%search-trim value))))

(defun %search-normalize-space (value)
  (%search-trim (cl-ppcre:regex-replace-all "\\s+" (or value "") " ")))

(defun %search-sequence->list (value)
  (cond
    ((null value) nil)
    ((listp value) value)
    ((vectorp value) (loop for item across value collect item))
    (t nil)))

(defun %search-safe-parse-integer (value &optional (default 0))
  (handler-case
      (parse-integer (%search-trim value))
    (error () default)))

(defun %search-monotonic-seconds ()
  (/ (coerce (get-internal-real-time) 'double-float)
     internal-time-units-per-second))

(defun %search-apply-rate-limit (min-interval-seconds)
  (let* ((interval (max 0.0d0 (coerce (or min-interval-seconds 0.0d0) 'double-float)))
         (now (%search-monotonic-seconds))
         (elapsed (- now *search-last-request-at*))
         (remaining (- interval elapsed)))
    (when (> remaining 0.0d0)
      (sleep remaining)
      (setf now (%search-monotonic-seconds)))
    (setf *search-last-request-at* now)))

(defun %search-curl-meta-marker ()
  "PSEUDOPOD_SEARCH_META:")

(defun %search-http-query-argument (key value)
  (format nil "~A=~A" key (or value "")))

(defun %search-split-curl-output (text)
  (let* ((payload (or text ""))
         (marker (%search-curl-meta-marker))
         (position (search marker payload :from-end t :test #'char=)))
    (unless position
      (error "Unable to parse curl metadata marker from output."))
    (let* ((body (subseq payload 0 position))
           (metadata (subseq payload (+ position (length marker))))
           (parts (cl-ppcre:split "\\t" metadata))
           (status-text (or (first parts) "0"))
           (effective-url (%search-trim (or (second parts) "")))
           (content-type (%search-trim (or (third parts) ""))))
      (values body (%search-safe-parse-integer status-text 0) effective-url content-type))))

(defun %default-search-http-get (url &key params timeout-seconds user-agent)
  (let* ((timeout (max 1 (or timeout-seconds *default-search-timeout-seconds*)))
         (agent (if (%search-empty-string-p user-agent)
                    *default-search-user-agent*
                    user-agent))
         (query-arguments
           (mapcan (lambda (entry)
                     (list "--data-urlencode"
                           (%search-http-query-argument (car entry) (cdr entry))))
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
                                 (%search-curl-meta-marker)
                                 #\Tab
                                 #\Tab)
                         url))))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program command
                          :ignore-error-status t
                          :output :string
                          :error-output :string)
      (unless (zerop (or exit-code 0))
        (error "Search HTTP GET failed (~{~A~^ ~}): ~A"
               command
               (%search-trim (if (%search-empty-string-p stderr) stdout stderr))))
      (multiple-value-bind (body status effective-url content-type)
          (%search-split-curl-output stdout)
        (list :status status
              :body body
              :effective-url (if (%search-empty-string-p effective-url) url effective-url)
              :content-type content-type)))))

(defun %search-decode-basic-entities (value)
  (let ((decoded (or value "")))
    (setf decoded (cl-ppcre:regex-replace-all "&amp;" decoded "&"))
    (setf decoded (cl-ppcre:regex-replace-all "&lt;" decoded "<"))
    (setf decoded (cl-ppcre:regex-replace-all "&gt;" decoded ">"))
    (setf decoded (cl-ppcre:regex-replace-all "&quot;" decoded "\""))
    (setf decoded (cl-ppcre:regex-replace-all "&#39;" decoded "'"))
    (setf decoded (cl-ppcre:regex-replace-all "&#x27;" decoded "'"))
    (setf decoded (cl-ppcre:regex-replace-all "&nbsp;" decoded " "))
    decoded))

(defun %search-strip-html-tags (value)
  (cl-ppcre:regex-replace-all "<[^>]+>" (or value "") " "))

(defun %search-normalize-html-text (value)
  (%search-normalize-space
   (%search-strip-html-tags
    (%search-decode-basic-entities value))))

(defun %search-url-domain (url)
  (when (stringp url)
    (cl-ppcre:register-groups-bind (host)
        ("^[A-Za-z][A-Za-z0-9+.-]*://([^/?#:]+)" url)
      (let* ((port-position (position #\: host))
             (without-port (if port-position
                               (subseq host 0 port-position)
                               host)))
        (string-downcase without-port)))))

(defun %search-percent-decode (text)
  (let ((payload (or text "")))
    (with-output-to-string (stream)
      (loop for index from 0 below (length payload) do
            (let ((character (char payload index)))
              (cond
                ((and (char= character #\%)
                      (<= (+ index 2) (1- (length payload))))
                 (let* ((hex (subseq payload (1+ index) (+ index 3)))
                        (value (handler-case
                                   (parse-integer hex :radix 16)
                                 (error () nil))))
                   (if value
                       (progn
                         (write-char (code-char value) stream)
                         (incf index 2))
                       (write-char character stream))))
                ((char= character #\+)
                 (write-char #\Space stream))
                (t
                 (write-char character stream))))))))

(defun %search-query-parameter (url parameter-name)
  (let* ((payload (or url ""))
         (question (position #\? payload :from-end t)))
    (when question
      (let ((query (subseq payload (1+ question))))
        (dolist (piece (cl-ppcre:split "&" query))
          (let ((separator (position #\= piece)))
            (when separator
              (let ((name (subseq piece 0 separator))
                    (value (subseq piece (1+ separator))))
                (when (string= name parameter-name)
                  (return-from %search-query-parameter (%search-percent-decode value)))))))))))

(defun %search-normalize-result-url (url)
  (let* ((normalized (%search-normalize-html-text url))
         (prefixed (cond
                     ((uiop:string-prefix-p "//" normalized)
                      (format nil "https:~A" normalized))
                     ((uiop:string-prefix-p "/" normalized)
                      (format nil "https://duckduckgo.com~A" normalized))
                     (t normalized)))
         (redirect-target
           (and (search "duckduckgo.com/l/?" prefixed :test #'char-equal)
                (%search-query-parameter prefixed "uddg"))))
    (if (%search-empty-string-p redirect-target)
        prefixed
        redirect-target)))

(defun %search-candidate-title (title url)
  (let ((normalized (%search-normalize-html-text title)))
    (if (%search-empty-string-p normalized)
        url
        normalized)))

(defun %search-normalize-result (title url snippet)
  (when (and (stringp url) (not (%search-empty-string-p url)))
    (let* ((normalized-url (%search-normalize-result-url url))
           (domain (%search-url-domain normalized-url))
           (normalized-snippet (%search-normalize-html-text snippet)))
      (list :title (%search-candidate-title title normalized-url)
            :url normalized-url
            :snippet normalized-snippet
            :source-domain domain))))

(defun %search-parse-json (payload)
  (let* ((jonathan-package (or (find-package :jonathan)
                               (error "Missing package JONATHAN for JSON parsing.")))
         (parse-symbol (or (find-symbol "PARSE" jonathan-package)
                           (error "Missing JONATHAN:PARSE function."))))
    (funcall (symbol-function parse-symbol) payload :as :hash-table)))

(defun %search-limit-results (results limit)
  (if (and limit (> (length results) limit))
      (subseq results 0 limit)
      results))

(defun %search-parse-searxng-results (payload limit)
  (let* ((document (%search-parse-json payload))
         (entries (%search-sequence->list (and (hash-table-p document)
                                               (gethash "results" document))))
         (results '()))
    (dolist (entry entries)
      (when (hash-table-p entry)
        (let* ((url (gethash "url" entry))
               (title (or (gethash "title" entry) url))
               (snippet (or (gethash "content" entry)
                            (gethash "snippet" entry)
                            ""))
               (normalized (%search-normalize-result title url snippet)))
          (when normalized
            (push normalized results)))))
    (%search-limit-results (nreverse results) limit)))

(defun %search-parse-duckduckgo-results (payload limit)
  (let ((links '())
        (snippets '()))
    (cl-ppcre:do-register-groups (href title)
        ("(?is)<a[^>]*class=['\"][^'\"]*result__a[^'\"]*['\"][^>]*href=['\"]([^'\"]+)['\"][^>]*>(.*?)</a>"
         payload)
      (let ((normalized (%search-normalize-result title href "")))
        (when normalized
          (push normalized links))))
    (cl-ppcre:do-register-groups (snippet)
        ("(?is)<[^>]*class=['\"][^'\"]*result__snippet[^'\"]*['\"][^>]*>(.*?)</[^>]+>"
         payload)
      (push (%search-normalize-html-text snippet) snippets))
    (let ((ordered-links (nreverse links))
          (ordered-snippets (nreverse snippets))
          (results '()))
      (loop for result in ordered-links
            for index from 0 do
            (let ((snippet (or (nth index ordered-snippets) "")))
              (push (list :title (getf result :title)
                          :url (getf result :url)
                          :snippet snippet
                          :source-domain (getf result :source-domain))
                    results)))
      (%search-limit-results (nreverse results) limit))))

(defun %search-build-hit (result rank)
  (%make-search-hit
   :title (or (getf result :title) "")
   :url (or (getf result :url) "")
   :snippet (or (getf result :snippet) "")
   :source-domain (getf result :source-domain)
   :rank rank))

(defun %search-backend-request-shape (backend query searxng-url duckduckgo-url)
  (case backend
    (:searxng
     (unless (and (stringp searxng-url)
                  (not (%search-empty-string-p searxng-url)))
       (error 'pseudopod-search-error
              :backend :searxng
              :message "SearXNG backend requires a non-empty URL."))
     (values (%search-trim searxng-url)
             (list (cons "q" query)
                   (cons "format" "json"))
             #'%search-parse-searxng-results))
    (:duckduckgo
     (let ((resolved-url (if (and (stringp duckduckgo-url)
                                  (not (%search-empty-string-p duckduckgo-url)))
                             (%search-trim duckduckgo-url)
                             *default-duckduckgo-search-url*)))
       (values resolved-url
               (list (cons "q" query))
               #'%search-parse-duckduckgo-results)))
    (otherwise
     (error 'pseudopod-search-error
            :backend backend
            :message (format nil "Unsupported search backend ~S." backend)))))

(defun search-backend (backend query
                       &key
                         (limit 5)
                         searxng-url
                         duckduckgo-url
                         (timeout-seconds *default-search-timeout-seconds*)
                         (user-agent *default-search-user-agent*)
                         (min-interval-seconds *default-search-rate-limit-seconds*)
                         http-get-fn)
  "Execute BACKEND search for QUERY and return a normalized SEARCH-RESPONSE.

BACKEND must be one of :SEARXNG or :DUCKDUCKGO.  The response schema is stable
across backends and always uses SEARCH-HIT structs under SEARCH-RESPONSE-RESULTS."
  (check-type backend keyword)
  (check-type query string)
  (let* ((normalized-query (%search-trim query))
         (normalized-limit (if (and (integerp limit) (> limit 0))
                               (min limit 50)
                               5))
         (runner (or http-get-fn #'%default-search-http-get)))
    (when (%search-empty-string-p normalized-query)
      (error 'pseudopod-search-error
             :backend backend
             :message "Search query must not be empty."))
    (multiple-value-bind (url params parser)
        (%search-backend-request-shape backend
                                       normalized-query
                                       searxng-url
                                       duckduckgo-url)
      (%search-apply-rate-limit min-interval-seconds)
      (let* ((response
               (handler-case
                   (funcall runner
                            url
                            :params params
                            :timeout-seconds timeout-seconds
                            :user-agent user-agent)
                 (pseudopod-search-error (condition)
                   (error condition))
                 (error (condition)
                   (error 'pseudopod-search-error
                          :backend backend
                          :url url
                          :cause condition
                          :message (princ-to-string condition)))))
             (status (getf response :status))
             (body (or (getf response :body) "")))
        (unless (and (integerp status) (<= 200 status 299))
          (error 'pseudopod-search-error
                 :backend backend
                 :url url
                 :status-code status
                 :message (format nil "Search backend returned HTTP status ~A." status)))
        (let* ((parsed-results
                 (handler-case
                     (funcall parser body normalized-limit)
                   (pseudopod-search-error (condition)
                     (error condition))
                   (error (condition)
                     (error 'pseudopod-search-parse-error
                            :backend backend
                            :url url
                            :payload body
                            :cause condition
                            :message (princ-to-string condition)))))
               (hits
                 (loop for result in parsed-results
                       for rank from 1
                       collect (%search-build-hit result rank))))
          (%make-search-response
           :backend backend
           :query normalized-query
           :results hits
           :result-count (length hits)
           :fetched-at (get-universal-time)))))))
