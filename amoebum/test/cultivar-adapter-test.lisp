(in-package :amoebum/test)

;;; ============================================================
;;; NXT-106: Cultivar Adapter Tests
;;; ============================================================

(def-suite cultivar-adapter-suite
  :description "Unit tests for the NXT-106 Cultivar adapter stub."
  :in amoebum-suite)

(in-suite cultivar-adapter-suite)

;;; ------------------------------------------------------------------
;;; Constructor and struct accessors
;;; ------------------------------------------------------------------

(test cultivar-adapter-default-construction
  "make-cultivar-adapter with no args produces a disabled adapter at the default endpoint."
  (let ((a (amoebum:make-cultivar-adapter)))
    (is (amoebum:cultivar-adapter-p a))
    (is (string= "http://localhost:8080" (amoebum:cultivar-adapter-endpoint a)))
    (is (not (amoebum:cultivar-adapter-enabled-p a)))))

(test cultivar-adapter-custom-endpoint
  "make-cultivar-adapter accepts a custom :endpoint."
  (let ((a (amoebum:make-cultivar-adapter :endpoint "http://cultivar.internal:9000")))
    (is (string= "http://cultivar.internal:9000" (amoebum:cultivar-adapter-endpoint a)))))

(test cultivar-adapter-enable
  "make-cultivar-adapter respects :enabled-p T."
  (let ((a (amoebum:make-cultivar-adapter :enabled-p t)))
    (is (amoebum:cultivar-adapter-enabled-p a))))

;;; ------------------------------------------------------------------
;;; Global parameter
;;; ------------------------------------------------------------------

(test cultivar-global-adapter-default-nil
  "*cultivar-adapter* defaults to NIL."
  (is (null amoebum:*cultivar-adapter*)))

;;; ------------------------------------------------------------------
;;; cultivar-resolve — disabled adapter
;;; ------------------------------------------------------------------

(test cultivar-resolve-disabled-returns-empty
  "cultivar-resolve returns (:RESULTS NIL) when the adapter is disabled."
  (let* ((a (amoebum:make-cultivar-adapter))
         (result (amoebum:cultivar-resolve a "find something")))
    (is (listp result))
    (is (null (getf result :results)))))

(test cultivar-resolve-nil-adapter-returns-empty
  "cultivar-resolve returns (:RESULTS NIL) when ADAPTER is NIL."
  (let ((result (amoebum:cultivar-resolve nil "find something")))
    (is (listp result))
    (is (null (getf result :results)))))

(test cultivar-resolve-enabled-returns-results-key
  "cultivar-resolve stub returns a plist with :RESULTS even when enabled."
  (let* ((a (amoebum:make-cultivar-adapter :enabled-p t))
         (result (amoebum:cultivar-resolve a "test query")))
    (is (listp result))
    (is (member :results result))))

;;; ------------------------------------------------------------------
;;; cultivar-preview — disabled adapter
;;; ------------------------------------------------------------------

(test cultivar-preview-disabled-returns-nil-preview
  "cultivar-preview returns (:PREVIEW NIL) when the adapter is disabled."
  (let* ((a (amoebum:make-cultivar-adapter))
         (result (amoebum:cultivar-preview a "ctx-001")))
    (is (listp result))
    (is (null (getf result :preview)))))

(test cultivar-preview-nil-adapter
  "cultivar-preview returns (:PREVIEW NIL) when ADAPTER is NIL."
  (let ((result (amoebum:cultivar-preview nil "ctx-001")))
    (is (listp result))
    (is (null (getf result :preview)))))

(test cultivar-preview-enabled-returns-preview-key
  "cultivar-preview stub returns a plist with :PREVIEW even when enabled."
  (let* ((a (amoebum:make-cultivar-adapter :enabled-p t))
         (result (amoebum:cultivar-preview a "ctx-001")))
    (is (listp result))
    (is (member :preview result))))

;;; ------------------------------------------------------------------
;;; cultivar-expand — disabled adapter
;;; ------------------------------------------------------------------

(test cultivar-expand-disabled-returns-nil-content
  "cultivar-expand returns (:CONTENT NIL) when the adapter is disabled."
  (let* ((a (amoebum:make-cultivar-adapter))
         (result (amoebum:cultivar-expand a "ctx-002")))
    (is (listp result))
    (is (null (getf result :content)))))

(test cultivar-expand-nil-adapter
  "cultivar-expand returns (:CONTENT NIL) when ADAPTER is NIL."
  (let ((result (amoebum:cultivar-expand nil "ctx-002")))
    (is (listp result))
    (is (null (getf result :content)))))

(test cultivar-expand-enabled-returns-content-key
  "cultivar-expand stub returns a plist with :CONTENT even when enabled."
  (let* ((a (amoebum:make-cultivar-adapter :enabled-p t))
         (result (amoebum:cultivar-expand a "ctx-002")))
    (is (listp result))
    (is (member :content result))))
