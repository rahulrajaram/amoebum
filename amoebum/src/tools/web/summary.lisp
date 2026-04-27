(in-package :amoebum)

(defparameter *web-fetch-summary-max-snippets* 3)
(defparameter *web-fetch-summary-snippet-max-chars* 220)

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

(defun %web-truncate-text-with-ellipsis (value max-chars)
  (let* ((limit (max 1 max-chars))
         (payload (or value "")))
    (if (<= (length payload) limit)
        payload
        (let* ((ellipsis "...")
               (prefix-limit (max 0 (- limit (length ellipsis)))))
          (concatenate 'string
                       (subseq payload 0 prefix-limit)
                       ellipsis)))))

(defun %web-markdown-first-line-matching (markdown predicate)
  (loop for raw-line in (cl-ppcre:split "\\n+" (or markdown ""))
        for line = (%web-trim raw-line)
        when (and (> (length line) 0)
                  (funcall predicate line))
          do (return line)
        finally (return nil)))

(defun %web-markdown-summary-snippets (markdown &key (limit *web-fetch-summary-max-snippets*) (max-chars *web-fetch-summary-snippet-max-chars*))
  (let ((snippets '()))
    (dolist (raw-line (cl-ppcre:split "\\n+" (or markdown "")))
      (let ((line (%web-normalize-space raw-line)))
        (when (and (> (length line) 0)
                   (not (%web-string-prefix-p "#" line))
                   (not (cl-ppcre:scan "(?i)^source:" line))
                   (>= (length line) 40))
          (let ((snippet (%web-truncate-text-with-ellipsis line max-chars)))
            (unless (member snippet snippets :test #'string=)
              (push snippet snippets)
              (when (>= (length snippets) (max 1 limit))
                (return)))))))
    (nreverse snippets)))

(defun %web-summarize-oversized-markdown (markdown max-bytes)
  (let* ((limit (max 1 max-bytes))
         (text (or markdown ""))
         (snippet-limit (max 60
                             (min *web-fetch-summary-snippet-max-chars*
                                  (floor limit 3))))
         (snippet-count (max 1
                             (min *web-fetch-summary-max-snippets*
                                  (floor limit 120))))
         (title-line (or (%web-markdown-first-line-matching
                          text
                          (lambda (line) (%web-string-prefix-p "#" line)))
                         "# Fetched Web Content"))
         (source-line (%web-markdown-first-line-matching
                       text
                       (lambda (line) (cl-ppcre:scan "(?i)^source:" line))))
         (snippets (%web-markdown-summary-snippets text
                                                   :limit snippet-count
                                                   :max-chars snippet-limit)))
    (with-output-to-string (stream)
      (format stream "~A~2%" title-line)
      (when source-line
        (format stream "~A~2%" source-line))
      (format stream "Summary generated for oversized page (~D bytes).~2%"
              (length text))
      (if snippets
          (loop for snippet in snippets
                for index from 1 do
                  (format stream "~D. ~A~%" index snippet))
          (format stream "1. No readable summary snippets were extracted.~%"))
      (format stream "~%...[summarized to fit ~D bytes]..." limit))))

(defun %web-bound-markdown (markdown max-bytes)
  (let* ((limit (max 1 max-bytes))
         (text (or markdown "")))
    (if (<= (length text) limit)
        (values text nil nil)
        (multiple-value-bind (summary summary-truncated-p)
            (%web-truncate-markdown
             (%web-summarize-oversized-markdown text limit)
             limit)
          (declare (ignore summary-truncated-p))
          (values summary t t)))))

(defun %web-host-changed-p (requested-url effective-url)
  (let ((requested-host (%web-url-domain requested-url))
        (effective-host (%web-url-domain effective-url)))
    (and requested-host
         effective-host
         (not (string= requested-host effective-host)))))

(defun %web-redirect-host-diagnostic (requested-url effective-url)
  (let ((requested-host (%web-url-domain requested-url))
        (effective-host (%web-url-domain effective-url)))
    (when (and requested-host
               effective-host
               (not (string= requested-host effective-host)))
      (format nil "Redirect host changed: ~A -> ~A"
              requested-host
              effective-host))))

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
