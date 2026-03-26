(in-package :amoebum/test)

;;; ============================================================
;;; NXT-110: Adapter Integration Tests
;;;
;;; Happy-path integration tests for Cultivar resolve-preview-expand
;;; and Yore search-fetch pipelines.  Tests exercise the full
;;; make-adapter -> operation -> result verification sequence, using
;;; only the stub implementations (no live services required).
;;; ============================================================

(def-suite adapter-integration-suite
  :description "NXT-110: Integration tests for Cultivar and Yore adapter happy paths."
  :in amoebum-suite)

(in-suite adapter-integration-suite)

;;; ------------------------------------------------------------------
;;; NXT-110.1 — Cultivar happy-path: resolve -> preview -> expand
;;; ------------------------------------------------------------------

(test cultivar-happy-path-resolve-preview-expand
  "Full Cultivar pipeline: make-cultivar-adapter -> cultivar-resolve ->
cultivar-preview -> cultivar-expand all return valid stub plists."
  (let* ((adapter (amoebum:make-cultivar-adapter :enabled-p t))
         ;; Step 1: resolve
         (resolve-result (amoebum:cultivar-resolve adapter "find lisp symbols"))
         ;; Step 2: preview — use a stub context-id that would come from resolve
         (preview-result (amoebum:cultivar-preview adapter "ctx-happy-001"))
         ;; Step 3: expand — full content for the same context-id
         (expand-result  (amoebum:cultivar-expand  adapter "ctx-happy-001")))
    ;; Resolve must return a plist with :RESULTS
    (is (listp resolve-result)
        "cultivar-resolve should return a list plist")
    (is (member :results resolve-result)
        "cultivar-resolve result must contain :RESULTS key")
    ;; Preview must return a plist with :PREVIEW
    (is (listp preview-result)
        "cultivar-preview should return a list plist")
    (is (member :preview preview-result)
        "cultivar-preview result must contain :PREVIEW key")
    ;; Expand must return a plist with :CONTENT
    (is (listp expand-result)
        "cultivar-expand should return a list plist")
    (is (member :content expand-result)
        "cultivar-expand result must contain :CONTENT key")))

(test cultivar-happy-path-constructor-to-resolve
  "make-cultivar-adapter correctly seeds the adapter so cultivar-resolve
returns a plist without signalling a condition."
  (let* ((adapter (amoebum:make-cultivar-adapter
                   :endpoint "http://cultivar.test:8080"
                   :enabled-p t))
         (result (handler-case
                     (amoebum:cultivar-resolve adapter "test query")
                   (error (c)
                     (list :error (princ-to-string c))))))
    (is (listp result) "Result should be a list, not an error")
    (is (not (getf result :error)) "cultivar-resolve must not signal an error")
    (is (member :results result) "Result must have :RESULTS key")))

(test cultivar-happy-path-preview-returns-plist
  "cultivar-preview with an enabled adapter returns a plist — no condition."
  (let* ((adapter (amoebum:make-cultivar-adapter :enabled-p t))
         (result (handler-case
                     (amoebum:cultivar-preview adapter "ctx-preview-test")
                   (error (c)
                     (list :error (princ-to-string c))))))
    (is (listp result))
    (is (not (getf result :error)))
    (is (member :preview result))))

(test cultivar-happy-path-expand-returns-plist
  "cultivar-expand with an enabled adapter returns a plist — no condition."
  (let* ((adapter (amoebum:make-cultivar-adapter :enabled-p t))
         (result (handler-case
                     (amoebum:cultivar-expand adapter "ctx-expand-test")
                   (error (c)
                     (list :error (princ-to-string c))))))
    (is (listp result))
    (is (not (getf result :error)))
    (is (member :content result))))

;;; ------------------------------------------------------------------
;;; NXT-110.2 — Cultivar disabled adapter returns empty results
;;; ------------------------------------------------------------------

(test cultivar-disabled-adapter-resolve-returns-empty-results
  "cultivar-resolve on a disabled adapter always returns (:RESULTS NIL)."
  (let* ((adapter (amoebum:make-cultivar-adapter))
         (result  (amoebum:cultivar-resolve adapter "any query")))
    (is (listp result))
    (is (null (getf result :results))
        "Disabled adapter must return NIL results")))

(test cultivar-disabled-adapter-preview-returns-nil-preview
  "cultivar-preview on a disabled adapter always returns (:PREVIEW NIL)."
  (let* ((adapter (amoebum:make-cultivar-adapter))
         (result  (amoebum:cultivar-preview adapter "ctx-disabled")))
    (is (listp result))
    (is (null (getf result :preview))
        "Disabled adapter must return NIL preview")))

(test cultivar-disabled-adapter-expand-returns-nil-content
  "cultivar-expand on a disabled adapter always returns (:CONTENT NIL)."
  (let* ((adapter (amoebum:make-cultivar-adapter))
         (result  (amoebum:cultivar-expand adapter "ctx-disabled")))
    (is (listp result))
    (is (null (getf result :content))
        "Disabled adapter must return NIL content")))

;;; ------------------------------------------------------------------
;;; NXT-110.3 — Cultivar nil adapter is handled gracefully
;;; ------------------------------------------------------------------

(test cultivar-nil-adapter-resolve-does-not-signal
  "cultivar-resolve with NIL adapter returns (:RESULTS NIL) without signalling."
  (let ((result (handler-case
                    (amoebum:cultivar-resolve nil "any query")
                  (error (c)
                    (list :error (princ-to-string c))))))
    (is (listp result))
    (is (not (getf result :error)) "NIL adapter must not cause an error")
    (is (null (getf result :results)))))

(test cultivar-nil-adapter-preview-does-not-signal
  "cultivar-preview with NIL adapter returns (:PREVIEW NIL) without signalling."
  (let ((result (handler-case
                    (amoebum:cultivar-preview nil "ctx-nil")
                  (error (c)
                    (list :error (princ-to-string c))))))
    (is (listp result))
    (is (not (getf result :error)))
    (is (null (getf result :preview)))))

(test cultivar-nil-adapter-expand-does-not-signal
  "cultivar-expand with NIL adapter returns (:CONTENT NIL) without signalling."
  (let ((result (handler-case
                    (amoebum:cultivar-expand nil "ctx-nil")
                  (error (c)
                    (list :error (princ-to-string c))))))
    (is (listp result))
    (is (not (getf result :error)))
    (is (null (getf result :content)))))

;;; ------------------------------------------------------------------
;;; NXT-110.4 — Yore happy-path: search-context -> fetch-context
;;; ------------------------------------------------------------------

(test yore-happy-path-search-then-fetch
  "Full Yore pipeline: make-yore-adapter -> yore-search-context ->
yore-fetch-context all return valid stub values."
  (let* ((adapter       (amoebum:make-yore-adapter :enabled-p t))
         ;; Step 1: search
         (search-result (amoebum:yore-search-context adapter "recent commit messages"))
         ;; Step 2: fetch — use a stub id that would come from search results
         (fetch-result  (amoebum:yore-fetch-context  adapter "yore-ctx-happy-001")))
    ;; Search must return a list (possibly empty in stub mode)
    (is (listp search-result)
        "yore-search-context should return a list")
    ;; Fetch returns NIL in stub mode — must not signal
    (is (or (null fetch-result) (listp fetch-result))
        "yore-fetch-context should return NIL or a plist")))

(test yore-happy-path-search-with-limit
  "yore-search-context with :limit keyword returns a list without signalling."
  (let* ((adapter (amoebum:make-yore-adapter :enabled-p t))
         (result (handler-case
                     (amoebum:yore-search-context adapter "some query" :limit 5)
                   (error (c)
                     (list :error (princ-to-string c))))))
    ;; Should not be an error plist
    (is (not (and (listp result) (getf result :error)))
        "yore-search-context :limit must not signal an error")
    (is (listp result))))

(test yore-happy-path-fetch-returns-nil-in-stub-mode
  "yore-fetch-context stub with an enabled adapter returns NIL (no live service)."
  (let* ((adapter (amoebum:make-yore-adapter :enabled-p t))
         (result (handler-case
                     (amoebum:yore-fetch-context adapter "yore-ctx-test")
                   (error (c)
                     (list :error (princ-to-string c))))))
    (is (not (and (listp result) (getf result :error)))
        "yore-fetch-context must not signal an error in stub mode")))

;;; ------------------------------------------------------------------
;;; NXT-110.5 — Yore disabled adapter returns empty results
;;; ------------------------------------------------------------------

(test yore-disabled-adapter-search-returns-empty-list
  "yore-search-context on a disabled adapter returns NIL."
  (let* ((adapter (amoebum:make-yore-adapter))
         (result  (amoebum:yore-search-context adapter "some query")))
    (is (listp result))
    (is (null result)
        "Disabled adapter must return NIL from search")))

(test yore-disabled-adapter-fetch-returns-nil
  "yore-fetch-context on a disabled adapter returns NIL."
  (let* ((adapter (amoebum:make-yore-adapter))
         (result  (amoebum:yore-fetch-context adapter "yore-ctx-disabled")))
    (is (null result)
        "Disabled adapter must return NIL from fetch")))

;;; ------------------------------------------------------------------
;;; NXT-110.6 — Yore nil adapter is handled gracefully
;;; ------------------------------------------------------------------

(test yore-nil-adapter-search-does-not-signal
  "yore-search-context with NIL adapter returns NIL without signalling."
  (let ((result (handler-case
                    (amoebum:yore-search-context nil "any query")
                  (error (c)
                    (list :error (princ-to-string c))))))
    (is (not (and (listp result) (getf result :error)))
        "NIL adapter must not cause an error in search")
    (is (null result))))

(test yore-nil-adapter-fetch-does-not-signal
  "yore-fetch-context with NIL adapter returns NIL without signalling."
  (let ((result (handler-case
                    (amoebum:yore-fetch-context nil "yore-ctx-nil")
                  (error (c)
                    (list :error (princ-to-string c))))))
    (is (not (and (listp result) (getf result :error)))
        "NIL adapter must not cause an error in fetch")))
