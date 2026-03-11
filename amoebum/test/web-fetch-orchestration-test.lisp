(in-package :amoebum/test)

(def-suite web-fetch-orchestration-suite
  :in amoebum-suite
  :description "Auth warning and oversized fetch summarization tests.")

(in-suite web-fetch-orchestration-suite)

(test authentication-warning-status-heuristic
  "HTTP auth status codes should always trigger an authentication warning."
  (let ((warning (amoebum::%web-authentication-warning
                  "https://example.test/private"
                  "https://example.test/private"
                  "<html><body>denied</body></html>"
                  401)))
    (is (stringp warning))
    (is-true (search "HTTP 401" warning :test #'char-equal))))

(test authentication-warning-login-form-heuristic
  "Login-form pages should trigger an authentication warning even with 200 status."
  (let ((warning (amoebum::%web-authentication-warning
                  "https://example.test/docs"
                  "https://example.test/login"
                  "<html><body><form><input type='password' name='pw'/></form></body></html>"
                  200)))
    (is (stringp warning))
    (is-true (search "authentication wall" warning :test #'char-equal))))

(test oversized-markdown-uses-summary-path
  "Oversized markdown should be reduced via summary path and remain bounded."
  (let* ((body (with-output-to-string (stream)
                 (dotimes (index 120)
                   (format stream
                           "Paragraph ~D contains detailed operational context about a long incident timeline and remediation decisions.~2%"
                           index))))
         (markdown (format nil "# Very Long Page~2%Source: https://example.test/long~2%~A"
                           body)))
    (multiple-value-bind (bounded truncated-p summarized-p)
        (amoebum::%web-bound-markdown markdown 700)
      (is-true truncated-p)
      (is-true summarized-p)
      (is (<= (length bounded) 700))
      (is-true (search "Summary generated for oversized page"
                       bounded
                       :test #'char-equal))
      (is-true (search "1." bounded :test #'char-equal)))))
