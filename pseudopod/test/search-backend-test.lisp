(in-package :pseudopod/test)

;;; ---------------------------------------------------------------------------
;;; I121: Search backend adapter contract tests
;;; ---------------------------------------------------------------------------

(def-suite search-backend-suite :in pseudopod-suite
  :description "Search backend adapter tests (I121).")

(in-suite search-backend-suite)

(defun %search-test-now-seconds ()
  (/ (coerce (get-internal-real-time) 'double-float)
     internal-time-units-per-second))

(test search-backend-searxng-stable-schema
  (let ((captured-url nil)
        (captured-params nil)
        (captured-user-agent nil)
        (captured-timeout nil))
    (let ((response
            (pseudopod:search-backend
             :searxng
             "amoebum adapter"
             :limit 3
             :searxng-url "https://search.example/query"
             :min-interval-seconds 0
             :timeout-seconds 17
             :user-agent "pseudopod-test-agent/1"
             :http-get-fn
             (lambda (url &key params timeout-seconds user-agent)
               (setf captured-url url
                     captured-params params
                     captured-timeout timeout-seconds
                     captured-user-agent user-agent)
               (list :status 200
                     :body
                     "{\"results\":[{\"title\":\"Alpha\",\"url\":\"https://docs.example/a\",\"content\":\"First result\"},{\"url\":\"https://docs.example/b\",\"snippet\":\"Second result\"}]}"
                     :effective-url url
                     :content-type "application/json")))))
      (is-true (pseudopod:search-response-p response))
      (is (eq :searxng (pseudopod:search-response-backend response)))
      (is (string= "amoebum adapter" (pseudopod:search-response-query response)))
      (is (= 2 (pseudopod:search-response-result-count response)))
      (is (integerp (pseudopod:search-response-fetched-at response)))
      (let* ((hits (pseudopod:search-response-results response))
             (first-hit (first hits))
             (second-hit (second hits)))
        (is (= 2 (length hits)))
        (is-true (pseudopod:search-hit-p first-hit))
        (is (string= "Alpha" (pseudopod:search-hit-title first-hit)))
        (is (string= "https://docs.example/a" (pseudopod:search-hit-url first-hit)))
        (is (string= "First result" (pseudopod:search-hit-snippet first-hit)))
        (is (string= "docs.example" (or (pseudopod:search-hit-source-domain first-hit) "")))
        (is (= 1 (pseudopod:search-hit-rank first-hit)))
        (is (string= "https://docs.example/b" (pseudopod:search-hit-title second-hit)))
        (is (string= "Second result" (pseudopod:search-hit-snippet second-hit)))
        (is (= 2 (pseudopod:search-hit-rank second-hit))))
      (is (string= "https://search.example/query" captured-url))
      (is (string= "amoebum adapter" (cdr (assoc "q" captured-params :test #'string=))))
      (is (string= "json" (cdr (assoc "format" captured-params :test #'string=))))
      (is (= 17 captured-timeout))
      (is (string= "pseudopod-test-agent/1" captured-user-agent)))))

(test search-backend-duckduckgo-stable-schema
  (let ((response
          (pseudopod:search-backend
           :duckduckgo
           "lisp parser"
           :limit 2
           :duckduckgo-url "https://duck.example/html/"
           :min-interval-seconds 0
           :http-get-fn
           (lambda (url &key params timeout-seconds user-agent)
             (declare (ignore params timeout-seconds user-agent))
             (list :status 200
                   :body
                   "<html><body><a class='result__a' href='https://duckduckgo.com/l/?uddg=https%3A%2F%2Fexample.org%2Fguide'>Guide &amp; Notes</a><div class='result__snippet'>Read &lt;b&gt;this&lt;/b&gt; first.</div></body></html>"
                   :effective-url url
                   :content-type "text/html")))))
    (is-true (pseudopod:search-response-p response))
    (is (eq :duckduckgo (pseudopod:search-response-backend response)))
    (is (= 1 (pseudopod:search-response-result-count response)))
    (let ((hit (first (pseudopod:search-response-results response))))
      (is-true (pseudopod:search-hit-p hit))
      (is (string= "Guide & Notes" (pseudopod:search-hit-title hit)))
      (is (string= "https://example.org/guide" (pseudopod:search-hit-url hit)))
      (is (string= "Read this first." (pseudopod:search-hit-snippet hit)))
      (is (string= "example.org" (or (pseudopod:search-hit-source-domain hit) ""))))))

(test search-backend-rate-limit-enforced
  (let* ((original-last pseudopod::*search-last-request-at*)
         (timestamps '())
         (runner
           (lambda (url &key params timeout-seconds user-agent)
             (declare (ignore url params timeout-seconds user-agent))
             (push (%search-test-now-seconds) timestamps)
             (list :status 200
                   :body "{\"results\":[{\"title\":\"ok\",\"url\":\"https://example.org\",\"content\":\"ok\"}]}"
                   :effective-url "https://search.example/query"
                   :content-type "application/json"))))
    (unwind-protect
        (progn
          (setf pseudopod::*search-last-request-at* 0.0d0)
          (pseudopod:search-backend :searxng
                                    "first"
                                    :searxng-url "https://search.example/query"
                                    :min-interval-seconds 0.2d0
                                    :http-get-fn runner)
          (pseudopod:search-backend :searxng
                                    "second"
                                    :searxng-url "https://search.example/query"
                                    :min-interval-seconds 0.2d0
                                    :http-get-fn runner)
          (is (= 2 (length timestamps)))
          (let* ((ordered (nreverse timestamps))
                 (delta (- (second ordered) (first ordered))))
            (is-true (>= delta 0.18d0)
                     "Expected >=0.18s between backend calls, observed ~,3F sec."
                     delta)))
      (setf pseudopod::*search-last-request-at* original-last))))

(test search-backend-http-status-error
  (signals pseudopod:pseudopod-search-error
    (pseudopod:search-backend
     :searxng
     "broken backend"
     :searxng-url "https://search.example/query"
     :min-interval-seconds 0
     :http-get-fn
     (lambda (url &key params timeout-seconds user-agent)
       (declare (ignore url params timeout-seconds user-agent))
       (list :status 503 :body "unavailable" :effective-url "" :content-type "text/plain")))))

(test search-backend-parse-error
  (signals pseudopod:pseudopod-search-parse-error
    (pseudopod:search-backend
     :searxng
     "invalid payload"
     :searxng-url "https://search.example/query"
     :min-interval-seconds 0
     :http-get-fn
     (lambda (url &key params timeout-seconds user-agent)
       (declare (ignore url params timeout-seconds user-agent))
       (list :status 200 :body "{not-json" :effective-url "" :content-type "application/json")))))
