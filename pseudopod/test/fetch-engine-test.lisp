(in-package :pseudopod/test)

;;; ---------------------------------------------------------------------------
;;; I123: Fetch Engine Tests
;;; ---------------------------------------------------------------------------

(def-suite fetch-engine-suite :in pseudopod-suite
  :description "Fetch engine success/timeout/http-error coverage.")

(in-suite fetch-engine-suite)

(test fetch-engine-success
  (let ((captured-url nil)
        (captured-timeout nil)
        (captured-max-body-bytes nil)
        (captured-user-agent nil)
        (captured-follow-redirects nil))
    (let ((result
            (pseudopod:fetch-url
             "https://docs.example/article"
             :timeout-seconds 17
             :max-body-bytes 4096
             :cache-ttl-seconds 0
             :user-agent "pseudopod-test-agent/1.0"
             :follow-redirects t
             :http-get-fn
             (lambda (url &key timeout-seconds max-body-bytes user-agent follow-redirects)
               (setf captured-url url
                     captured-timeout timeout-seconds
                     captured-max-body-bytes max-body-bytes
                     captured-user-agent user-agent
                     captured-follow-redirects follow-redirects)
               (list :status 200
                     :effective-url "https://docs.example/article"
                     :content-type "text/html; charset=utf-8"
                     :body "<html><body>Hello fetch engine.</body></html>")))))
      (is-true (pseudopod:fetch-result-p result))
      (is (string= "https://docs.example/article" (pseudopod:fetch-result-url result)))
      (is (= 200 (pseudopod:fetch-result-status-code result)))
      (is (string= "text/html; charset=utf-8" (pseudopod:fetch-result-content-type result)))
      (is (search "Hello fetch engine." (pseudopod:fetch-result-body result)))
      (is (null (pseudopod:fetch-result-truncated-p result)))
      (is (null (pseudopod:fetch-result-cached-p result)))
      (is (integerp (pseudopod:fetch-result-fetched-at result)))
      (is (string= "https://docs.example/article" captured-url))
      (is (= 17 captured-timeout))
      (is (= 4096 captured-max-body-bytes))
      (is (string= "pseudopod-test-agent/1.0" captured-user-agent))
      (is (eq t captured-follow-redirects)))))

(test fetch-engine-timeout-signals-timeout-condition
  (signals pseudopod:pseudopod-fetch-timeout
    (pseudopod:fetch-url
     "https://docs.example/slow"
     :cache-ttl-seconds 0
     :http-get-fn
     (lambda (url &key timeout-seconds max-body-bytes user-agent follow-redirects)
       (declare (ignore max-body-bytes user-agent follow-redirects))
       (error 'pseudopod:pseudopod-fetch-timeout
              :url url
              :timeout-seconds timeout-seconds
              :message "simulated timeout")))))

(test fetch-engine-http-status-signals-http-error
  (let ((condition
          (handler-case
              (progn
                (pseudopod:fetch-url
                 "https://docs.example/failure"
                 :cache-ttl-seconds 0
                 :http-get-fn
                 (lambda (url &key timeout-seconds max-body-bytes user-agent follow-redirects)
                   (declare (ignore url timeout-seconds max-body-bytes user-agent follow-redirects))
                   (list :status 503
                         :effective-url "https://docs.example/failure"
                         :content-type "text/plain"
                         :body "temporarily unavailable")))
                nil)
            (pseudopod:pseudopod-fetch-http-error (caught) caught))))
    (is-true (typep condition 'pseudopod:pseudopod-fetch-http-error))
    (is (= 503 (pseudopod:pseudopod-fetch-error-status-code condition)))
    (is (string= "text/plain" (pseudopod:pseudopod-fetch-http-error-content-type condition)))
    (is (string= "temporarily unavailable" (pseudopod:pseudopod-fetch-http-error-body condition)))))

(test fetch-engine-redirect-host-change-detected
  (let ((result
          (pseudopod:fetch-url
           "https://docs.example/protected"
           :cache-ttl-seconds 0
           :http-get-fn
           (lambda (url &key timeout-seconds max-body-bytes user-agent follow-redirects)
             (declare (ignore url timeout-seconds max-body-bytes user-agent follow-redirects))
             ;; Simulate a followed redirect chain finishing on a different host.
             (list :status 200
                   :effective-url "https://auth.example.net/session/login"
                   :content-type "text/html; charset=utf-8"
                   :body "<html><body>sign in</body></html>")))))
    (is-true (pseudopod:fetch-result-p result))
    (is (pseudopod:fetch-result-redirected-p result))
    (is (pseudopod:fetch-result-host-changed-p result))
    (is (null (pseudopod:fetch-result-cached-p result)))
    (is (string= "https://auth.example.net/session/login"
                 (pseudopod:fetch-result-effective-url result)))))

(test fetch-engine-cache-hit-and-expiry
  (clrhash pseudopod::*fetch-cache*)
  (let* ((call-count 0)
         (url "https://docs.example/cacheable")
         (max-body-bytes 4096)
         (user-agent "pseudopod-cache-test/1.0")
         (runner (lambda (request-url &key timeout-seconds max-body-bytes user-agent follow-redirects)
                   (declare (ignore timeout-seconds max-body-bytes user-agent follow-redirects))
                   (incf call-count)
                   (list :status 200
                         :effective-url request-url
                         :content-type "text/plain"
                         :body (format nil "payload-~D" call-count)))))
    (unwind-protect
        (progn
          (let ((first
                  (pseudopod:fetch-url
                   url
                   :cache-ttl-seconds 30
                   :max-body-bytes max-body-bytes
                   :user-agent user-agent
                   :http-get-fn runner)))
            (is (null (pseudopod:fetch-result-cached-p first)))
            (is (= 1 call-count)))
          (let ((second
                  (pseudopod:fetch-url
                   url
                   :cache-ttl-seconds 30
                   :max-body-bytes max-body-bytes
                   :user-agent user-agent
                   :http-get-fn runner)))
            (is (pseudopod:fetch-result-cached-p second))
            (is (= 1 call-count)))
          (let* ((cache-key (pseudopod::%fetch-cache-key url
                                                         max-body-bytes
                                                         t
                                                         user-agent))
                 (entry (gethash cache-key pseudopod::*fetch-cache*)))
            (is-true entry)
            ;; Force expiry deterministically without sleeping.
            (setf (first entry) (1- (get-universal-time))
                  (gethash cache-key pseudopod::*fetch-cache*) entry))
          (let ((third
                  (pseudopod:fetch-url
                   url
                   :cache-ttl-seconds 30
                   :max-body-bytes max-body-bytes
                   :user-agent user-agent
                   :http-get-fn runner)))
            (is (null (pseudopod:fetch-result-cached-p third)))
            (is (= 2 call-count))))
      (clrhash pseudopod::*fetch-cache*))))
