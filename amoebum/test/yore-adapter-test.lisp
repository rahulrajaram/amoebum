(in-package :amoebum/test)

;;; ============================================================
;;; NXT-107: Yore Adapter Tests
;;; ============================================================

(def-suite yore-adapter-suite
  :description "Unit tests for the NXT-107 Yore adapter stub."
  :in amoebum-suite)

(in-suite yore-adapter-suite)

;;; ------------------------------------------------------------------
;;; Constructor and struct accessors
;;; ------------------------------------------------------------------

(test yore-adapter-default-construction
  "make-yore-adapter with no args produces a disabled adapter at the default endpoint."
  (let ((a (amoebum:make-yore-adapter)))
    (is (amoebum:yore-adapter-p a))
    (is (string= "http://localhost:9090" (amoebum:yore-adapter-endpoint a)))
    (is (not (amoebum:yore-adapter-enabled-p a)))))

(test yore-adapter-custom-endpoint
  "make-yore-adapter accepts a custom :endpoint."
  (let ((a (amoebum:make-yore-adapter :endpoint "http://yore.internal:7777")))
    (is (string= "http://yore.internal:7777" (amoebum:yore-adapter-endpoint a)))))

(test yore-adapter-enable
  "make-yore-adapter respects :enabled-p T."
  (let ((a (amoebum:make-yore-adapter :enabled-p t)))
    (is (amoebum:yore-adapter-enabled-p a))))

;;; ------------------------------------------------------------------
;;; Global parameter
;;; ------------------------------------------------------------------

(test yore-global-adapter-default-nil
  "*yore-adapter* defaults to NIL."
  (is (null amoebum:*yore-adapter*)))

;;; ------------------------------------------------------------------
;;; yore-search-context — disabled adapter
;;; ------------------------------------------------------------------

(test yore-search-context-disabled-returns-empty-list
  "yore-search-context returns NIL when the adapter is disabled."
  (let* ((a (amoebum:make-yore-adapter))
         (result (amoebum:yore-search-context a "recent commits")))
    (is (listp result))
    (is (null result))))

(test yore-search-context-nil-adapter-returns-empty
  "yore-search-context returns NIL when ADAPTER is NIL."
  (let ((result (amoebum:yore-search-context nil "recent commits")))
    (is (listp result))
    (is (null result))))

(test yore-search-context-enabled-returns-list
  "yore-search-context stub returns a list (possibly empty) when enabled."
  (let* ((a (amoebum:make-yore-adapter :enabled-p t))
         (result (amoebum:yore-search-context a "test")))
    (is (listp result))))

(test yore-search-context-with-limit
  "yore-search-context accepts a :limit keyword without error."
  (let* ((a (amoebum:make-yore-adapter))
         (result (amoebum:yore-search-context a "query" :limit 10)))
    (is (listp result))))

;;; ------------------------------------------------------------------
;;; yore-fetch-context — disabled adapter
;;; ------------------------------------------------------------------

(test yore-fetch-context-disabled-returns-nil
  "yore-fetch-context returns NIL when the adapter is disabled."
  (let* ((a (amoebum:make-yore-adapter))
         (result (amoebum:yore-fetch-context a "yore-ctx-42")))
    (is (null result))))

(test yore-fetch-context-nil-adapter-returns-nil
  "yore-fetch-context returns NIL when ADAPTER is NIL."
  (let ((result (amoebum:yore-fetch-context nil "yore-ctx-42")))
    (is (null result))))

(test yore-fetch-context-enabled-returns-nil-stub
  "yore-fetch-context stub returns NIL even when adapter is enabled (no real service)."
  (let* ((a (amoebum:make-yore-adapter :enabled-p t))
         (result (amoebum:yore-fetch-context a "yore-ctx-99")))
    ;; Stub returns NIL; a real implementation would return a plist.
    (is (null result))))
