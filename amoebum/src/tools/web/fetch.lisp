(in-package :amoebum)

(defun %web-effective-fetch-user-agent (override)
  (let ((configured (or override (cfg :web-fetch-user-agent))))
    (if (%web-empty-string-p configured)
        *web-fetch-default-user-agent*
        (%web-trim configured))))

(defun %web-resolve-fetch-timeout (override)
  (%web-resolve-positive-integer
   override
   (%web-resolve-positive-integer (cfg :web-fetch-timeout-seconds)
                                  *web-fetch-default-timeout-seconds*)))

(defun %web-resolve-fetch-max-markdown-bytes (override)
  (%web-resolve-positive-integer
   override
   (%web-resolve-positive-integer (cfg :web-fetch-max-markdown-bytes)
                                  *web-fetch-default-max-markdown-bytes*)))

(defun %web-fetch-response->plist (response)
  (list :engine :pseudopod
        :url (pseudopod:fetch-response-url response)
        :effective-url (pseudopod:fetch-response-effective-url response)
        :status (pseudopod:fetch-response-status response)
        :body (pseudopod:fetch-response-body response)
        :content-type (pseudopod:fetch-response-content-type response)
        :fetched-at (pseudopod:fetch-response-fetched-at response)))

(defun %web-fetch-via-pseudopod (url timeout-seconds user-agent)
  (%web-fetch-response->plist
   (pseudopod:fetch-backend
    url
    :timeout-seconds timeout-seconds
    :user-agent user-agent
    :http-get-fn *web-fetch-http-get-runner*)))

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
           (now (monotonic-seconds))
           (cached (%web-fetch-cache-get cache-key now)))
      (if cached
          (append (list :cached t) cached)
          (let* ((response (%web-fetch-via-pseudopod normalized-url
                                                     resolved-timeout
                                                     resolved-user-agent))
                 (status (or (getf response :status) 0))
                 (body (or (getf response :body) ""))
                 (effective-url (or (getf response :effective-url)
                                    (getf response :url)
                                    normalized-url))
                 (content-type (or (getf response :content-type) ""))
                 (fetch-engine (or (getf response :engine) :unknown))
                 (fetched-at (or (getf response :fetched-at) 0))
                 (base-markdown (%web-document->markdown normalized-url effective-url body))
                 (auth-warning (%web-authentication-warning normalized-url effective-url body status))
                 (host-changed (%web-host-changed-p normalized-url effective-url))
                 (redirect-host-diagnostic
                   (%web-redirect-host-diagnostic normalized-url effective-url)))
            (multiple-value-bind (markdown truncated-p summarized-p)
                (%web-bound-markdown base-markdown resolved-max-markdown-bytes)
              (let ((result
                      (list :cached nil
                            :url normalized-url
                            :effective-url effective-url
                            :status status
                            :content-type content-type
                            :fetch-engine fetch-engine
                            :fetched-at fetched-at
                            :host-changed host-changed
                            :redirect-host-diagnostic redirect-host-diagnostic
                            :authentication-warning auth-warning
                            :truncated-p truncated-p
                            :summarized-p summarized-p
                            :max-markdown-bytes resolved-max-markdown-bytes
                            :cache-ttl-seconds resolved-cache-ttl
                            :markdown markdown)))
                (%web-fetch-cache-put cache-key result now resolved-cache-ttl)
                result)))))))
