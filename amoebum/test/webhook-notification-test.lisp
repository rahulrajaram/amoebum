(in-package :amoebum/test)

(def-suite webhook-notification-suite
  :description "Webhook notification backend tests (I223)."
  :in amoebum-suite)

(in-suite webhook-notification-suite)

(test webhook-post-sends-json-payload-and-content-type
  (let ((old-http amoebum.notifications:*webhook-http-request-function*)
        (captured-method nil)
        (captured-headers nil)
        (captured-content nil))
    (unwind-protect
        (progn
          (setf amoebum.notifications:*webhook-http-request-function*
                (lambda (_url &key method headers content &allow-other-keys)
                  (setf captured-method method
                        captured-headers headers
                        captured-content content)
                  (list :status 200
                        :body "{\"ok\":true}"
                        :effective-url "https://example.test/webhook"
                        :content-type "application/json")))
          (multiple-value-bind (ok detail)
              (amoebum.notifications:send-webhook-notification
               (amoebum.notifications:make-webhook-config
                :url "https://example.test/webhook")
               (amoebum.notifications:make-notification
                :title "Task Complete"
                :body "Tool completed."
                :severity :info
                :category :task-complete
                :timestamp 100))
            (is-true ok)
            (is (null detail))))
      (setf amoebum.notifications:*webhook-http-request-function* old-http))
    (is (string= "post" (or captured-method "")))
    (is-true (assoc "Content-Type" captured-headers :test #'string=))
    (is-true (search "\"title\":\"Task Complete\"" (or captured-content "") :test #'char=))
    (is-true (search "\"category\":\"task-complete\"" (or captured-content "") :test #'char=))))

(test webhook-signing-adds-hmac-header
  (let ((old-http amoebum.notifications:*webhook-http-request-function*)
        (captured-headers nil)
        (captured-content nil))
    (unwind-protect
        (progn
          (setf amoebum.notifications:*webhook-http-request-function*
                (lambda (_url &key headers content &allow-other-keys)
                  (setf captured-headers headers
                        captured-content content)
                  (list :status 200
                        :body "{\"ok\":true}"
                        :effective-url "https://example.test/webhook"
                        :content-type "application/json")))
          (multiple-value-bind (ok detail)
              (amoebum.notifications:send-webhook-notification
               (amoebum.notifications:make-webhook-config
                :url "https://example.test/webhook"
                :secret "s3cr3t")
               (amoebum.notifications:make-notification
                :title "Task Error"
                :body "Tool failed."
                :severity :error
                :category :error
                :timestamp 200))
            (is-true ok)
            (is (null detail))))
      (setf amoebum.notifications:*webhook-http-request-function* old-http))
    (let* ((header (assoc "X-Amoebum-Signature-256" captured-headers :test #'string=))
           (expected (format nil "sha256=~A"
                             (amoebum::%webhook-signature-hex "s3cr3t"
                                                              captured-content))))
      (is-true header)
      (is (string= expected (cdr header))))))

(test webhook-retries-on-5xx-with-exponential-backoff
  (let ((old-http amoebum.notifications:*webhook-http-request-function*)
        (old-sleep amoebum.notifications:*webhook-sleep-function*)
        (attempts 0)
        (sleep-calls '()))
    (unwind-protect
        (progn
          (setf amoebum.notifications:*webhook-http-request-function*
                (lambda (_url &key &allow-other-keys)
                  (incf attempts)
                  (if (< attempts 3)
                      (list :status 503
                            :body "{\"error\":\"temporary\"}"
                            :effective-url "https://example.test/webhook"
                            :content-type "application/json")
                      (list :status 200
                            :body "{\"ok\":true}"
                            :effective-url "https://example.test/webhook"
                            :content-type "application/json"))))
          (setf amoebum.notifications:*webhook-sleep-function*
                (lambda (seconds)
                  (push seconds sleep-calls)
                  nil))
          (multiple-value-bind (ok detail)
              (amoebum.notifications:send-webhook-notification
               (amoebum.notifications:make-webhook-config
                :url "https://example.test/webhook")
               (amoebum.notifications:make-notification
                :title "Retry"
                :body "Retry on transient failure."
                :severity :warning
                :category :general
                :timestamp 300))
            (is-true ok)
            (is (null detail))))
      (setf amoebum.notifications:*webhook-http-request-function* old-http
            amoebum.notifications:*webhook-sleep-function* old-sleep))
    (is (= 3 attempts))
    (is (equal '(1 2) (nreverse sleep-calls)))))

(test webhook-notification-smoke-sentinel
  (is-true t)
  (format t "WEBHOOK_NOTIFICATION_SMOKE_OK~%"))
