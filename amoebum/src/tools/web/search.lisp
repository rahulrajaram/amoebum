(in-package :amoebum)

(defparameter *web-search-current-date-provider* #'get-universal-time)

(defparameter *web-search-temporal-keywords*
  '("latest" "recent" "newest" "current" "today" "now" "new" "updated"
    "last week" "last month" "this year" "this month"))

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

(defun %web-query-has-iso-date-p (query)
  (cl-ppcre:scan "\\d{4}-\\d{2}-\\d{2}" query))

(defun %web-query-temporal-p (query)
  (let ((lower (string-downcase query)))
    (some (lambda (keyword)
            (search keyword lower :test #'char=))
          *web-search-temporal-keywords*)))

(defun %web-shape-search-query (query allow-domains block-domains)
  (let ((parts (list query))
        (date-context nil))
    (dolist (domain allow-domains)
      (push (format nil "site:~A" domain) parts))
    (dolist (domain block-domains)
      (push (format nil "-site:~A" domain) parts))
    (when (and (%web-query-temporal-p query)
               (not (%web-query-has-iso-date-p query)))
      (let ((now (funcall *web-search-current-date-provider*)))
        (multiple-value-bind (sec min hour day month year)
            (decode-universal-time now 0)
          (declare (ignore sec min hour))
          (let ((date-str (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)))
            (setf date-context date-str)
            (push (format nil "as of ~A" date-str) parts)))))
    (values (format nil "~{~A~^ ~}" (nreverse parts))
            date-context)))

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
  (let ((configured (or override (cfg :web-search-searxng-url))))
    (unless (%web-empty-string-p configured)
      (%web-trim configured))))

(defun %web-effective-duckduckgo-url ()
  (let ((configured (cfg :web-search-duckduckgo-url)))
    (if (%web-empty-string-p configured)
        *web-search-default-duckduckgo-url*
        (%web-trim configured))))

(defun %web-effective-search-user-agent ()
  (let ((configured (cfg :web-search-user-agent)))
    (if (%web-empty-string-p configured)
        *web-search-default-user-agent*
        (%web-trim configured))))

(defun %web-effective-domain-list (explicit-value config-key)
  (if explicit-value
      (%web-normalize-domain-list explicit-value)
      (%web-normalize-domain-list (cfg config-key))))

(defun %web-pseudopod-hit->result (hit)
  (list :title (pseudopod:search-hit-title hit)
        :url (pseudopod:search-hit-url hit)
        :snippet (pseudopod:search-hit-snippet hit)
        :source-domain (pseudopod:search-hit-source-domain hit)))

(defun %web-search-searxng (query limit searxng-url user-agent)
  (let* ((response
           (pseudopod:search-backend
            :searxng
            query
            (pseudopod:make-search-options
             :limit limit
             :searxng-url searxng-url
             :timeout-seconds *web-search-default-timeout-seconds*
             :user-agent user-agent
             :min-interval-seconds *web-search-rate-limit-seconds*)))
         (hits (pseudopod:search-response-results response)))
    (mapcar #'%web-pseudopod-hit->result hits)))

(defun %web-search-duckduckgo (query limit user-agent)
  (let* ((response
           (pseudopod:search-backend
            :duckduckgo
            query
            (pseudopod:make-search-options
             :limit limit
             :duckduckgo-url (%web-effective-duckduckgo-url)
             :timeout-seconds *web-search-default-timeout-seconds*
             :user-agent user-agent
             :min-interval-seconds *web-search-rate-limit-seconds*)))
         (hits (pseudopod:search-response-results response)))
    (mapcar #'%web-pseudopod-hit->result hits)))

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
