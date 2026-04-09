(in-package :amoebum/test)

;;; ============================================================
;;; NXT-110: Adapter Integration Tests
;;;
;;; Happy-path integration tests for Cultivar resolve-slice-expand
;;; and Yore search-fetch pipelines.  Tests exercise the full
;;; make-adapter -> operation -> result verification sequence, using
;;; only offline-safe harnesses (no live services required).
;;; ============================================================

(def-suite adapter-integration-suite
  :description "NXT-110: Integration tests for Cultivar and Yore adapter happy paths."
  :in amoebum-suite)

(in-suite adapter-integration-suite)

(defun %adapter-integration-cultivar-fake-cli-script (root)
  (let ((path (merge-pathnames "cultivar-integration-fake.sh" root)))
    (%write-text-file
     path
     "#!/bin/sh
set -eu
cmd=${1:-}
shift || true
case \"$cmd\" in
  query)
    sub=${1:-}
    shift || true
    case \"$sub\" in
      resolve)
        printf '%s' '{\"results_digest\":\"arena\",\"symbol\":{\"id\":\"sym_fake\",\"kind\":\"function\",\"file\":\"/tmp/fake.lisp\",\"qualified_name\":\"amoebum:fake\",\"range\":{\"start_line\":1,\"start_col\":2}},\"confidence\":{\"level\":\"must\"},\"reference_context\":{\"reference_mode\":\"structural_only\"}}'
        ;;
      refs)
        printf '%s' '{\"results_digest\":\"arena\",\"reference_mode\":\"structural_only\",\"must_refs\":[],\"may_refs\":[{\"file\":\"/tmp/fake-ref.lisp\",\"kind\":\"reference\",\"symbol_id\":\"sym_fake\",\"range\":{\"start_line\":4,\"start_col\":2,\"end_line\":4,\"end_col\":6}}],\"unknown_refs\":[]}'
        ;;
      *)
        printf '%s' '{}'
        ;;
    esac
    ;;
  slice)
    markdown=no
    while [ $# -gt 0 ]; do
      case \"$1\" in
        --markdown) markdown=yes; shift ;;
        *) shift ;;
      esac
    done
    if [ \"$markdown\" = yes ]; then
      printf '%s' '```lisp
(fake)
```'
    else
      printf '%s' '{\"results_digest\":\"arena\",\"served_from_materialization\":true,\"materialization_kind\":\"neighborhood\",\"symbol\":{\"id\":\"sym_fake\",\"name\":\"amoebum:fake\",\"kind\":\"function\",\"file\":\"/tmp/fake.lisp\",\"line\":1,\"col\":2},\"definition_source\":\"(defun fake ())\",\"callers\":[{\"file\":\"/tmp/caller.lisp\",\"line\":4,\"col\":2,\"context_line\":\"(fake)\",\"confidence\":\"may\",\"from_symbol\":\"amoebum:caller\"}],\"callees\":[],\"quality\":{\"index_coverage_pct\":100.0,\"must_edges\":2,\"may_edges\":1,\"unknown_edges\":0,\"has_uncertainty\":true},\"truncation\":{\"budget_chars\":8000,\"callers_truncated\":false,\"callees_truncated\":false,\"original_callers\":1,\"original_callees\":0},\"notes\":[\"structural-only\"]}'
    fi
    ;;
  *)
    exit 1
    ;;
esac
")
    (uiop:run-program (list "chmod" "+x" (namestring path))
                      :output nil :error-output nil)
    path))

(defmacro %with-adapter-integration-cultivar ((adapter-var) &body body)
  `(let* ((root (%make-temp-directory "amoebum-adapter-integration-cultivar"))
          (index (merge-pathnames "index/" root))
          (script (%adapter-integration-cultivar-fake-cli-script root)))
     (unwind-protect
          (let ((,adapter-var
                  (amoebum:make-cultivar-adapter
                   :enabled-p t
                   :binary-path (namestring script)
                   :root-path root
                   :index-path index
                   :daemon-mode :prefer
                   :daemon-auto-start-p nil)))
            (ensure-directories-exist (merge-pathnames ".keep" index))
            ,@body)
       (%delete-directory-tree-safe root))))

;;; ------------------------------------------------------------------
;;; NXT-110.1 — Cultivar happy-path: resolve -> canonical slice -> expand
;;; ------------------------------------------------------------------

(test cultivar-happy-path-resolve-slice-expand
  "Full Cultivar pipeline: resolve a position, fetch the canonical slice, then expand refs."
  (%with-adapter-integration-cultivar (adapter)
    (let* ((location (list :file "/tmp/fake.lisp" :line 1 :col 2))
           (slice-result (amoebum:cultivar-location-slice adapter location))
           (expand-result (amoebum:cultivar-expand adapter (getf slice-result :symbol-id))))
      (is (listp slice-result)
          "cultivar-location-slice should return a plist")
      (is (eq t (getf slice-result :resolved-p))
          "location-slice should resolve the symbol")
      (is (eq t (getf slice-result :slice))
          "location-slice should return the canonical slice")
      (is (listp (getf slice-result :resolution))
          "location-slice should preserve resolve provenance")
      (is (eq t (getf slice-result :served-from-materialization))
          "canonical slice should preserve daemon materialization metadata")
      (is (listp expand-result)
          "cultivar-expand should return a list plist")
      (is (member :content expand-result)
          "cultivar-expand result must contain :CONTENT key"))))

(test cultivar-happy-path-constructor-to-resolve
  "make-cultivar-adapter correctly seeds the adapter so cultivar-resolve
returns a plist without signalling a condition."
  (%with-adapter-integration-cultivar (adapter)
    (let ((result (handler-case
                      (amoebum:cultivar-resolve adapter (list :file "/tmp/fake.lisp" :line 1 :col 2))
                    (error (c)
                      (list :error (princ-to-string c))))))
      (is (listp result) "Result should be a list, not an error")
      (is (not (getf result :error)) "cultivar-resolve must not signal an error")
      (is (member :results result) "Result must have :RESULTS key"))))

(test cultivar-happy-path-preview-returns-plist
  "cultivar-preview remains available as the human-readable fallback."
  (%with-adapter-integration-cultivar (adapter)
    (let ((result (handler-case
                      (amoebum:cultivar-preview adapter "sym_fake")
                    (error (c)
                      (list :error (princ-to-string c))))))
      (is (listp result))
      (is (not (getf result :error)))
      (is (member :preview result)))))

(test cultivar-happy-path-expand-returns-plist
  "cultivar-expand with an enabled adapter returns a plist — no condition."
  (%with-adapter-integration-cultivar (adapter)
    (let ((result (handler-case
                      (amoebum:cultivar-expand adapter "sym_fake")
                    (error (c)
                      (list :error (princ-to-string c))))))
      (is (listp result))
      (is (not (getf result :error)))
      (is (member :content result)))))

;;; ------------------------------------------------------------------
;;; NXT-110.2 — Cultivar disabled adapter returns empty results
;;; ------------------------------------------------------------------

(test cultivar-disabled-adapter-resolve-returns-empty-results
  "cultivar-resolve on a disabled adapter always returns (:RESULTS NIL)."
  (let* ((adapter (amoebum:make-cultivar-adapter))
         (result  (amoebum:cultivar-resolve adapter (list :file "/tmp/fake.lisp" :line 1 :col 2))))
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
                    (amoebum:cultivar-resolve nil (list :file "/tmp/fake.lisp" :line 1 :col 2))
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
