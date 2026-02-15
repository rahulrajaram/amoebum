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
(defparameter *web-fetch-default-timeout-seconds* 20)
(defparameter *web-fetch-default-cache-ttl-seconds* 900)
(defparameter *web-fetch-default-max-markdown-bytes* 10240)
(defparameter *web-fetch-default-user-agent* "amoebum-web-fetch/0.1")
(defparameter *web-fetch-cache* (make-hash-table :test #'equal))
(defparameter *web-fetch-cache-lock*
  (bordeaux-threads:make-lock "amoebum-web-fetch-cache"))

(defun %web-trim (value)
  (string-trim '(#\Space #\Tab #\Newline #\Return) (or value "")))

(defun %web-empty-string-p (value)
  (zerop (length (%web-trim value))))

(defun %web-normalize-space (value)
  (let ((collapsed (cl-ppcre:regex-replace-all "\\s+" (or value "") " ")))
    (%web-trim collapsed)))

(defun %web-monotonic-seconds ()
  (/ (coerce (get-internal-real-time) 'double-float)
     internal-time-units-per-second))

(defun %web-acquire-rate-limit-slot ()
  (bordeaux-threads:with-lock-held (*web-search-rate-limit-lock*)
    (let* ((now (%web-monotonic-seconds))
           (elapsed (- now *web-search-last-request-at*))
           (remaining (- *web-search-rate-limit-seconds* elapsed))
           (slept 0.0d0))
      (when (> remaining 0.0d0)
        (sleep remaining)
        (setf slept remaining
              now (%web-monotonic-seconds)))
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

(defun %web-normalize-domain-list (value)
  (let ((entries
          (cond
            ((null value) nil)
            ((stringp value) (cl-ppcre:split "[,\\s]+" value))
            (t (%web-sequence->list value)))))
    (remove-duplicates
     (remove nil
             (mapcar #'%web-normalize-domain entries))
     :test #'string=)))

(defun %web-domain-matches-rule-p (domain rule)
  (or (string= domain rule)
      (%web-string-suffix-p (format nil ".~A" rule) domain)))

(defun %web-domain-allowed-p (domain allow-domains block-domains)
  (let ((blocked (and domain
                      (some (lambda (rule)
                              (%web-domain-matches-rule-p domain rule))
                            block-domains)))
        (allowed (or (null allow-domains)
                     (and domain
                          (some (lambda (rule)
                                  (%web-domain-matches-rule-p domain rule))
                                allow-domains)))))
    (and (not blocked) allowed)))

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

(defun %web-percent-decode (text)
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

(defun %web-query-parameter (url parameter-name)
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
                  (return-from %web-query-parameter (%web-percent-decode value)))))))))))

(defun %web-normalize-result-url (url)
  (let* ((normalized (%web-normalize-html-text url))
         (prefixed (cond
                     ((uiop:string-prefix-p "//" normalized)
                      (format nil "https:~A" normalized))
                     ((uiop:string-prefix-p "/" normalized)
                      (format nil "https://duckduckgo.com~A" normalized))
                     (t normalized)))
         (redirect-target
           (and (search "duckduckgo.com/l/?" prefixed :test #'char-equal)
                (%web-query-parameter prefixed "uddg"))))
    (if (%web-empty-string-p redirect-target)
        prefixed
        redirect-target)))

(defun %web-json-parse-hash-table (payload)
  (let* ((jonathan-package (or (find-package :jonathan)
                               (error "Missing package JONATHAN for JSON parsing.")))
         (parse-symbol (or (find-symbol "PARSE" jonathan-package)
                           (error "Missing JONATHAN:PARSE function."))))
    (funcall (symbol-function parse-symbol) payload :as :hash-table)))

(defun %web-candidate-title (title url)
  (let ((normalized (%web-normalize-html-text title)))
    (if (%web-empty-string-p normalized)
        url
        normalized)))

(defun %web-normalize-result (title url snippet)
  (when (and (stringp url) (not (%web-empty-string-p url)))
    (let* ((normalized-url (%web-normalize-result-url url))
           (domain (%web-url-domain normalized-url))
           (normalized-snippet (%web-normalize-html-text snippet)))
      (list :title (%web-candidate-title title normalized-url)
            :url normalized-url
            :snippet normalized-snippet
            :source-domain domain))))

(defun %web-limit-results (results limit)
  (if (and limit (> (length results) limit))
      (subseq results 0 limit)
      results))

(defun %web-parse-searxng-results (payload limit)
  (let* ((document (%web-json-parse-hash-table payload))
         (entries (%web-sequence->list (and (hash-table-p document)
                                            (gethash "results" document))))
         (results '()))
    (dolist (entry entries)
      (when (hash-table-p entry)
        (let* ((url (gethash "url" entry))
               (title (or (gethash "title" entry) url))
               (snippet (or (gethash "content" entry)
                            (gethash "snippet" entry)
                            ""))
               (normalized (%web-normalize-result title url snippet)))
          (when normalized
            (push normalized results)))))
    (%web-limit-results (nreverse results) limit)))

(defun %web-parse-duckduckgo-results (payload limit)
  (let ((links '())
        (snippets '()))
    (cl-ppcre:do-register-groups (href title)
        ("(?is)<a[^>]*class=['\"][^'\"]*result__a[^'\"]*['\"][^>]*href=['\"]([^'\"]+)['\"][^>]*>(.*?)</a>"
         payload)
      (let ((normalized (%web-normalize-result title href "")))
        (when normalized
          (push normalized links))))
    (cl-ppcre:do-register-groups (snippet)
        ("(?is)<[^>]*class=['\"][^'\"]*result__snippet[^'\"]*['\"][^>]*>(.*?)</[^>]+>" payload)
      (push (%web-normalize-html-text snippet) snippets))
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
      (%web-limit-results (nreverse results) limit))))

(defun %web-filter-results (results allow-domains block-domains)
  (remove-if-not
   (lambda (result)
     (let ((domain (or (getf result :source-domain)
                       (%web-url-domain (getf result :url)))))
       (%web-domain-allowed-p domain allow-domains block-domains)))
   results))

(defun %web-markdown-lines-for-result (index result)
  (let ((title (getf result :title))
        (url (getf result :url))
        (snippet (getf result :snippet))
        (source-domain (or (getf result :source-domain) "unknown")))
    (with-output-to-string (stream)
      (format stream "~D. [~A](~A)~%" index title url)
      (unless (%web-empty-string-p snippet)
        (format stream "   ~A~%" snippet))
      (format stream "   Source: ~A~%" source-domain))))

(defun %web-results->markdown (query backend-name results)
  (with-output-to-string (stream)
    (format stream "Web search results for `~A`~%" query)
    (format stream "Backend: ~A~2%" backend-name)
    (if results
        (loop for result in results
              for index from 1 do
              (write-string (%web-markdown-lines-for-result index result) stream))
        (format stream "No results found.~%"))))

(defun %web-effective-searxng-url (override)
  (let ((configured (or override (config-value :web-search-searxng-url))))
    (unless (%web-empty-string-p configured)
      (%web-trim configured))))

(defun %web-effective-duckduckgo-url ()
  (let ((configured (config-value :web-search-duckduckgo-url)))
    (if (%web-empty-string-p configured)
        *web-search-default-duckduckgo-url*
        (%web-trim configured))))

(defun %web-effective-search-user-agent ()
  (let ((configured (config-value :web-search-user-agent)))
    (if (%web-empty-string-p configured)
        *web-search-default-user-agent*
        (%web-trim configured))))

(defun %web-effective-domain-list (explicit-value config-key)
  (if explicit-value
      (%web-normalize-domain-list explicit-value)
      (%web-normalize-domain-list (config-value config-key))))

(defun %web-effective-fetch-user-agent (override)
  (let ((configured (or override (config-value :web-fetch-user-agent))))
    (if (%web-empty-string-p configured)
        *web-fetch-default-user-agent*
        (%web-trim configured))))

(defun %web-resolve-positive-integer (candidate fallback)
  (if (and (integerp candidate) (> candidate 0))
      candidate
      fallback))

(defun %web-resolve-fetch-timeout (override)
  (%web-resolve-positive-integer
   override
   (%web-resolve-positive-integer (config-value :web-fetch-timeout-seconds)
                                  *web-fetch-default-timeout-seconds*)))

(defun %web-resolve-fetch-max-markdown-bytes (override)
  (%web-resolve-positive-integer
   override
   (%web-resolve-positive-integer (config-value :web-fetch-max-markdown-bytes)
                                  *web-fetch-default-max-markdown-bytes*)))

(defun %web-resolve-fetch-cache-ttl-seconds (override)
  (%web-resolve-positive-integer
   override
   (%web-resolve-positive-integer (config-value :web-fetch-cache-ttl-seconds)
                                  *web-fetch-default-cache-ttl-seconds*)))

(defun %web-fetch-cache-key (url max-markdown-bytes)
  (list (%web-trim url) max-markdown-bytes))

(defun %web-copy-plist (plist)
  (loop for (key value) on plist by #'cddr append (list key value)))

(defun %web-fetch-cache-get (cache-key now)
  (bordeaux-threads:with-lock-held (*web-fetch-cache-lock*)
    (let ((entry (gethash cache-key *web-fetch-cache*)))
      (when entry
        (let ((expires-at (or (getf entry :expires-at) 0.0d0)))
          (if (and (> expires-at 0.0d0) (> now expires-at))
              (progn
                (remhash cache-key *web-fetch-cache*)
                nil)
              (%web-copy-plist (getf entry :result))))))))

(defun %web-fetch-cache-put (cache-key result now ttl-seconds)
  (when (and (integerp ttl-seconds) (> ttl-seconds 0))
    (bordeaux-threads:with-lock-held (*web-fetch-cache-lock*)
      (setf (gethash cache-key *web-fetch-cache*)
            (list :expires-at (+ now ttl-seconds)
                  :result (%web-copy-plist result))))))

(defun %web-strip-html-comments (value)
  (cl-ppcre:regex-replace-all "(?is)<!--.*?-->" (or value "") " "))

(defun %web-strip-noise-html-blocks (value)
  (let ((cleaned (%web-strip-html-comments value)))
    (dolist (pattern '("(?is)<script\\b[^>]*>.*?</script>"
                       "(?is)<style\\b[^>]*>.*?</style>"
                       "(?is)<noscript\\b[^>]*>.*?</noscript>"
                       "(?is)<svg\\b[^>]*>.*?</svg>"
                       "(?is)<form\\b[^>]*>.*?</form>"
                       "(?is)<nav\\b[^>]*>.*?</nav>"
                       "(?is)<header\\b[^>]*>.*?</header>"
                       "(?is)<footer\\b[^>]*>.*?</footer>"
                       "(?is)<aside\\b[^>]*>.*?</aside>"
                       "(?is)<([a-z0-9]+)\\b[^>]*(?:id|class)\\s*=\\s*['\"][^'\"]*(?:nav|menu|sidebar|footer|header|cookie|popup|ads?|advert|promo|banner|subscribe|breadcrumb)[^'\"]*['\"][^>]*>.*?</\\1>"))
      (setf cleaned (cl-ppcre:regex-replace-all pattern cleaned " ")))
    cleaned))

(defun %web-extract-tag-content (html tag-name)
  (let ((pattern (format nil "(?is)<~A\\b[^>]*>(.*?)</~A>" tag-name tag-name)))
    (cl-ppcre:register-groups-bind (content)
        (pattern (or html ""))
      content)))

(defun %web-largest-readable-block (html)
  (let ((best-content nil)
        (best-score 0))
    (cl-ppcre:do-register-groups (content)
        ("(?is)<(?:article|main|section|div)\\b[^>]*>(.*?)</(?:article|main|section|div)>"
         (or html ""))
      (let ((score (length (%web-normalize-html-text content))))
        (when (> score best-score)
          (setf best-score score
                best-content content))))
    best-content))

(defun %web-content-substantial-p (value &optional (minimum-length 120))
  (> (length (%web-normalize-html-text value)) minimum-length))

(defun %web-extract-main-html (html)
  (let* ((cleaned (%web-strip-noise-html-blocks html))
         (article (%web-extract-tag-content cleaned "article"))
         (main (%web-extract-tag-content cleaned "main"))
         (largest (%web-largest-readable-block cleaned))
         (body (%web-extract-tag-content cleaned "body")))
    (cond
      ((%web-content-substantial-p article) article)
      ((%web-content-substantial-p main) main)
      ((%web-content-substantial-p largest) largest)
      ((%web-content-substantial-p body 20) body)
      (t cleaned))))

(defun %web-extract-title (html)
  (or (let ((title (%web-extract-tag-content html "title")))
        (unless (%web-empty-string-p title)
          (%web-normalize-html-text title)))
      (let ((heading (%web-extract-tag-content html "h1")))
        (unless (%web-empty-string-p heading)
          (%web-normalize-html-text heading)))
      "Fetched Web Content"))

(defun %web-html->markdown-body (html)
  (let ((text (%web-strip-noise-html-blocks html)))
    (dolist (replacement '(("(?is)<br\\s*/?>" . "\n")
                           ("(?is)</p\\s*>" . "\n\n")
                           ("(?is)</div\\s*>" . "\n")
                           ("(?is)<li\\b[^>]*>" . "\n- ")
                           ("(?is)</li\\s*>" . "\n")
                           ("(?is)<h1\\b[^>]*>" . "\n\n# ")
                           ("(?is)</h1\\s*>" . "\n\n")
                           ("(?is)<h2\\b[^>]*>" . "\n\n## ")
                           ("(?is)</h2\\s*>" . "\n\n")
                           ("(?is)<h3\\b[^>]*>" . "\n\n### ")
                           ("(?is)</h3\\s*>" . "\n\n")
                           ("(?is)<h[4-6]\\b[^>]*>" . "\n\n#### ")
                           ("(?is)</h[4-6]\\s*>" . "\n\n")
                           ("(?is)<(section|article|main|ul|ol)\\b[^>]*>" . "\n\n")
                           ("(?is)</(section|article|main|ul|ol)\\s*>" . "\n\n")))
      (setf text (cl-ppcre:regex-replace-all (car replacement) text (cdr replacement))))
    (setf text (%web-decode-basic-entities text))
    (setf text (%web-strip-html-tags text))
    (setf text (cl-ppcre:regex-replace-all "[\\t\\r\\f\\v ]+" text " "))
    (setf text (cl-ppcre:regex-replace-all " ?\\n ?" text "\n"))
    (setf text (cl-ppcre:regex-replace-all "\\n{3,}" text "\n\n"))
    (%web-trim text)))

(defun %web-document->markdown (requested-url effective-url html)
  (let* ((main-html (%web-extract-main-html html))
         (title (%web-extract-title html))
         (body (%web-html->markdown-body main-html)))
    (with-output-to-string (stream)
      (format stream "# ~A~2%" title)
      (format stream "Source: ~A~2%" (or effective-url requested-url))
      (if (%web-empty-string-p body)
          (write-string "No readable content extracted." stream)
          (write-string body stream)))))

(defun %web-truncate-markdown (markdown max-bytes)
  (let* ((limit (max 1 max-bytes))
         (text (or markdown "")))
    (if (<= (length text) limit)
        (values text nil)
        (let* ((suffix (format nil "~2%...[truncated to ~D bytes]..." limit))
               (body-limit (max 0 (- limit (length suffix))))
               (prefix (subseq text 0 body-limit)))
          (values (concatenate 'string prefix suffix) t)))))

(defun %web-host-changed-p (requested-url effective-url)
  (let ((requested-host (%web-url-domain requested-url))
        (effective-host (%web-url-domain effective-url)))
    (and requested-host
         effective-host
         (not (string= requested-host effective-host)))))

(defun %web-url-looks-like-login-p (url)
  (and (stringp url)
       (cl-ppcre:scan "(?i)(?:/login|/signin|/sign-in|/auth|/oauth|/session|/account/login)"
                      url)))

(defun %web-body-looks-like-login-p (html)
  (and (stringp html)
       (or (cl-ppcre:scan "(?is)<input[^>]*type=['\"]password['\"]" html)
           (cl-ppcre:scan "(?is)(?:sign\\s*in|log\\s*in|forgot\\s+password|two-factor|continue with)"
                          html))))

(defun %web-authentication-warning (requested-url effective-url html status)
  (let ((host-changed (%web-host-changed-p requested-url effective-url))
        (login-url (%web-url-looks-like-login-p effective-url))
        (login-body (%web-body-looks-like-login-p html)))
    (cond
      ((member status '(401 403) :test #'=)
       (format nil "Potential authentication wall (HTTP ~D)." status))
      ((and host-changed login-url)
       (format nil "Potential authentication wall: redirected from ~A to login URL ~A."
               requested-url
               effective-url))
      ((and login-url login-body)
       (format nil "Potential authentication wall detected at ~A." effective-url))
      (login-body
       "Potential authentication wall: page resembles a login form.")
      (t nil))))

(defun %web-search-searxng (query limit searxng-url user-agent)
  (let* ((response (%web-http-get searxng-url
                                  :params (list (cons "q" query)
                                                (cons "format" "json"))
                                  :timeout-seconds *web-search-default-timeout-seconds*
                                  :user-agent user-agent))
         (body (getf response :body)))
    (%web-parse-searxng-results body limit)))

(defun %web-search-duckduckgo (query limit user-agent)
  (let* ((response (%web-http-get (%web-effective-duckduckgo-url)
                                  :params (list (cons "q" query))
                                  :timeout-seconds *web-search-default-timeout-seconds*
                                  :user-agent user-agent))
         (body (getf response :body)))
    (%web-parse-duckduckgo-results body limit)))

(defun %web-search-backend-order (backend searxng-url)
  (case backend
    (:searxng (list :searxng))
    (:duckduckgo (list :duckduckgo))
    (:auto (if searxng-url
               (list :searxng :duckduckgo)
               (list :duckduckgo)))
    (otherwise (error "Unsupported backend ~S." backend))))

(defun %web-search-results (query backend searxng-url limit user-agent)
  (let ((errors '()))
    (dolist (candidate (%web-search-backend-order backend searxng-url))
      (handler-case
          (let ((results
                  (case candidate
                    (:searxng
                     (unless searxng-url
                       (error "SearXNG backend requested but no URL configured."))
                     (%web-search-searxng query limit searxng-url user-agent))
                    (:duckduckgo
                     (%web-search-duckduckgo query limit user-agent))
                    (otherwise
                     (error "Unsupported backend ~S." candidate)))))
            (return-from %web-search-results
              (values candidate results (nreverse errors))))
        (error (condition)
          (push (format nil "~(~A~): ~A" candidate condition) errors))))
    (error "Web search failed: ~{~A~^ | ~}" (nreverse errors))))

(deftool web-search ((query string :description "Search query" :required t)
                     (limit integer :description "Maximum number of results" :default 5)
                     (backend (member :auto :searxng :duckduckgo)
                              :description "Search backend preference"
                              :default :auto)
                     (searxng-url (or null string)
                                  :description "Optional SearXNG URL override"
                                  :default nil)
                     (allow-domains (or null string)
                                    :description "Comma-separated domain allow list override"
                                    :default nil)
                     (block-domains (or null string)
                                    :description "Comma-separated domain block list override"
                                    :default nil))
  "Search the web and return markdown-formatted results with source attribution."
  (:permission :auto)
  (:dangerous nil)
  (:category :web)
  (:timeout 60)
  (let* ((normalized-query (%web-trim query))
         (normalized-limit (if (and (integerp limit) (> limit 0))
                               (min limit *web-search-max-limit*)
                               *web-search-default-limit*))
         (resolved-searxng-url (%web-effective-searxng-url searxng-url))
         (allow-list (%web-effective-domain-list allow-domains :web-search-allow-domains))
         (block-list (%web-effective-domain-list block-domains :web-search-block-domains))
         (user-agent (%web-effective-search-user-agent)))
    (when (%web-empty-string-p normalized-query)
      (error "QUERY must not be empty."))
    (multiple-value-bind (effective-backend raw-results fallback-errors)
        (%web-search-results normalized-query
                             backend
                             resolved-searxng-url
                             normalized-limit
                             user-agent)
      (let* ((filtered-results (%web-filter-results raw-results allow-list block-list))
             (limited-results (%web-limit-results filtered-results normalized-limit))
             (markdown (%web-results->markdown normalized-query
                                               (string-downcase (symbol-name effective-backend))
                                               limited-results)))
        (list :query normalized-query
              :backend effective-backend
              :count (length limited-results)
              :allow-domains allow-list
              :block-domains block-list
              :fallback-errors fallback-errors
              :results limited-results
              :markdown markdown)))))

(deftool web-fetch ((url string :description "URL to fetch and convert to markdown" :required t)
                    (timeout-seconds (or null integer)
                                     :description "HTTP timeout in seconds"
                                     :default nil)
                    (max-markdown-bytes integer
                                        :description "Maximum markdown payload size after conversion"
                                        :default 10240)
                    (cache-ttl-seconds (or null integer)
                                       :description "Fetch cache TTL in seconds"
                                       :default nil)
                    (user-agent (or null string)
                                :description "Optional User-Agent override"
                                :default nil))
  "Fetch a web page, extract readable content, and return markdown."
  (:permission :auto)
  (:dangerous nil)
  (:category :web)
  (:timeout 60)
  (let* ((normalized-url (%web-trim url))
         (resolved-timeout (%web-resolve-fetch-timeout timeout-seconds))
         (resolved-max-markdown-bytes (%web-resolve-fetch-max-markdown-bytes max-markdown-bytes))
         (resolved-cache-ttl (%web-resolve-fetch-cache-ttl-seconds cache-ttl-seconds))
         (resolved-user-agent (%web-effective-fetch-user-agent user-agent)))
    (when (%web-empty-string-p normalized-url)
      (error "URL must not be empty."))
    (let* ((cache-key (%web-fetch-cache-key normalized-url resolved-max-markdown-bytes))
           (now (%web-monotonic-seconds))
           (cached (%web-fetch-cache-get cache-key now)))
      (if cached
          (append (list :cached t) cached)
          (let* ((response (%web-http-get normalized-url
                                          :timeout-seconds resolved-timeout
                                          :user-agent resolved-user-agent
                                          :respect-rate-limit nil))
                 (status (or (getf response :status) 0))
                 (body (or (getf response :body) ""))
                 (effective-url (or (getf response :effective-url)
                                    (getf response :url)
                                    normalized-url))
                 (content-type (or (getf response :content-type) ""))
                 (base-markdown (%web-document->markdown normalized-url effective-url body))
                 (auth-warning (%web-authentication-warning normalized-url effective-url body status))
                 (host-changed (%web-host-changed-p normalized-url effective-url)))
            (multiple-value-bind (markdown truncated-p)
                (%web-truncate-markdown base-markdown resolved-max-markdown-bytes)
              (let ((result
                      (list :cached nil
                            :url normalized-url
                            :effective-url effective-url
                            :status status
                            :content-type content-type
                            :host-changed host-changed
                            :authentication-warning auth-warning
                            :truncated-p truncated-p
                            :max-markdown-bytes resolved-max-markdown-bytes
                            :cache-ttl-seconds resolved-cache-ttl
                            :markdown markdown)))
                (%web-fetch-cache-put cache-key result now resolved-cache-ttl)
                result)))))))
