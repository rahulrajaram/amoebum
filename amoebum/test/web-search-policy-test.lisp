(in-package :amoebum/test)

(def-suite web-search-policy-suite
  :in amoebum-suite
  :description "I357 web search domain policy and date-aware query shaping tests.")

(in-suite web-search-policy-suite)

(test web-filter-results-honors-allowlist
  (let* ((results (list (list :title "Allowed"
                              :url "https://allowed.example/page"
                              :source-domain "allowed.example")
                        (list :title "Other"
                              :url "https://other.example/page"
                              :source-domain "other.example")))
         (filtered (amoebum::%web-filter-results results
                                                 '("allowed.example")
                                                 nil)))
    (is (= 1 (length filtered)))
    (is (string= "allowed.example"
                 (getf (first filtered) :source-domain)))))

(test web-filter-results-honors-blocklist
  (let* ((results (list (list :title "Allowed"
                              :url "https://allowed.example/page"
                              :source-domain "allowed.example")
                        (list :title "Blocked"
                              :url "https://blocked.example/page"
                              :source-domain "blocked.example")))
         (filtered (amoebum::%web-filter-results results
                                                 nil
                                                 '("blocked.example"))))
    (is (= 1 (length filtered)))
    (is (string= "allowed.example"
                 (getf (first filtered) :source-domain)))))

(test web-shape-search-query-injects-domain-policy-and-date-context
  (let ((amoebum::*web-search-current-date-provider*
          (lambda ()
            (encode-universal-time 0 0 12 7 3 2026 0))))
    (multiple-value-bind (effective-query date-context)
        (amoebum::%web-shape-search-query "latest amoebum release"
                                          '("allowed.example")
                                          '("blocked.example"))
      (is (string= "2026-03-07" date-context))
      (is (search "site:allowed.example" effective-query :test #'char-equal))
      (is (search "-site:blocked.example" effective-query :test #'char-equal))
      (is (search "as of 2026-03-07" effective-query :test #'char-equal)))))

(test web-shape-search-query-skips-date-context-for-stable-query
  (let ((amoebum::*web-search-current-date-provider*
          (lambda ()
            (encode-universal-time 0 0 12 7 3 2026 0))))
    (multiple-value-bind (effective-query date-context)
        (amoebum::%web-shape-search-query "amoebum architecture"
                                          nil
                                          nil)
      (is (null date-context))
      (is (string= "amoebum architecture" effective-query)))))

(test web-shape-search-query-skips-date-context-when-query-has-iso-date
  (let ((amoebum::*web-search-current-date-provider*
          (lambda ()
            (encode-universal-time 0 0 12 7 3 2026 0))))
    (multiple-value-bind (effective-query date-context)
        (amoebum::%web-shape-search-query "latest amoebum release 2026-01-15"
                                          nil
                                          nil)
      (is (null date-context))
      (is (search "2026-01-15" effective-query :test #'char-equal))
      (is-false (search "as of" effective-query :test #'char-equal)))))
