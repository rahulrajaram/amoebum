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

(test cultivar-daemon-status-defaults-to-stopped
  "Default adapters report a stopped daemon surface with auto-start disabled."
  (let* ((a (amoebum:make-cultivar-adapter))
         (status (amoebum:cultivar-daemon-status a)))
    (is (eq :off (getf status :mode)))
    (is (not (getf status :auto-start-p)))
    (is (not (getf status :running-p)))
    (is (stringp (getf status :socket-path)))
    (is (null (getf status :last-slice)))))

(test cultivar-daemon-status-reads-common-lisp-health-report
  "Daemon status should expose the latest recorded Common Lisp Cultivar health verdict."
  (let* ((root (%make-temp-directory "amoebum-cultivar-health"))
         (report (merge-pathnames ".agent/cultivar-cl-health.status" root)))
    (unwind-protect
         (progn
           (ensure-directories-exist report)
           (%write-text-file
            report
            "schema_version=1
generated_at=2026-04-16
status=advisory
reference_mode=structural_only
index_health_summary=UNHEALTHY
doctor_exit_code=1
summary=Common Lisp index degraded but still usable for structural slices.
navigation_warning=Fresh-symbol navigation is advisory only; fall back to rg plus direct file reads.
")
           (let* ((adapter (amoebum:make-cultivar-adapter
                            :root-path root))
                  (status (amoebum:cultivar-daemon-status adapter))
                  (health (getf status :cl-health)))
             (is (eq :advisory (getf health :status)))
             (is (equal "structural_only" (getf health :reference-mode)))
             (is (equal "UNHEALTHY" (getf health :index-health-summary)))
             (is (= 1 (getf health :doctor-exit-code)))
             (is (search "Fresh-symbol navigation is advisory only"
                         (or (getf health :navigation-warning) "")
                         :test #'char-equal))))
      (%delete-directory-tree-safe root))))

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

;;; ------------------------------------------------------------------
;;; cultivar-slice — disabled adapter
;;; ------------------------------------------------------------------

(test cultivar-slice-disabled-returns-nil-slice
  "cultivar-slice returns (:SLICE NIL) when the adapter is disabled."
  (let* ((a (amoebum:make-cultivar-adapter))
         (result (amoebum:cultivar-slice a "sym_001")))
    (is (listp result))
    (is (null (getf result :slice)))))

(test cultivar-slice-nil-adapter
  "cultivar-slice returns (:SLICE NIL) when ADAPTER is NIL."
  (let ((result (amoebum:cultivar-slice nil "sym_001")))
    (is (listp result))
    (is (null (getf result :slice)))))

(test cultivar-location-slice-disabled-returns-unresolved-stub
  "cultivar-location-slice returns an unresolved nil-slice stub when the adapter is disabled."
  (let* ((a (amoebum:make-cultivar-adapter))
         (result (amoebum:cultivar-location-slice
                  a
                  (list :file "/tmp/fake.lisp" :line 1 :col 2))))
    (is (listp result))
    (is (not (getf result :resolved-p)))
    (is (null (getf result :slice)))))

(test cultivar-location-slice-nil-adapter
  "cultivar-location-slice returns an unresolved nil-slice stub when ADAPTER is NIL."
  (let ((result (amoebum:cultivar-location-slice
                 nil
                 (list :file "/tmp/fake.lisp" :line 1 :col 2))))
    (is (listp result))
    (is (not (getf result :resolved-p)))
    (is (null (getf result :slice)))))

;;; ------------------------------------------------------------------
;;; Daemon-preferred startup tests
;;; ------------------------------------------------------------------

(defun %cultivar-fake-cli-script (root)
  (let ((path (merge-pathnames "cultivar-fake.sh" root)))
    (%write-text-file
     path
     "#!/bin/sh
set -eu
LOGFILE=${CULTIVAR_FAKE_LOG:-}
if [ -n \"$LOGFILE\" ]; then
  printf '%s\\n' \"$*\" >> \"$LOGFILE\"
fi
cmd=${1:-}
shift || true
case \"$cmd\" in
  start)
    INDEX=
    while [ $# -gt 0 ]; do
      case \"$1\" in
        --index) INDEX=$2; shift 2 ;;
        --root) shift 2 ;;
        *) shift ;;
      esac
    done
    mkdir -p \"$INDEX\"
    : > \"$INDEX/.cultivar.sock\"
    exit 0
    ;;
  query)
    sub=${1:-}
    shift || true
    case \"$sub\" in
      resolve)
        printf '%s' '{\"results_digest\":\"arena\",\"symbol\":{\"id\":\"sym_fake\",\"kind\":\"function\",\"file\":\"/tmp/fake.lisp\",\"qualified_name\":\"amoebum:fake\",\"range\":{\"start_line\":1,\"start_col\":2}},\"confidence\":{\"level\":\"must\"},\"reference_context\":{\"reference_mode\":\"structural_only\"}}'
        ;;
      refs)
        printf '%s' '{\"results_digest\":\"arena\",\"reference_mode\":\"structural_only\",\"must_refs\":[],\"may_refs\":[{\"file\":\"/tmp/fake.lisp\",\"kind\":\"reference\",\"symbol_id\":\"sym_fake\",\"range\":{\"start_line\":1,\"start_col\":2,\"end_line\":1,\"end_col\":6}}],\"unknown_refs\":[]}'
        ;;
      *)
        printf '%s' '{}'
        ;;
    esac
    ;;
  slice)
    MARKDOWN=no
    while [ $# -gt 0 ]; do
      case \"$1\" in
        --markdown) MARKDOWN=yes; shift ;;
        *) shift ;;
      esac
    done
    if [ \"$MARKDOWN\" = yes ]; then
      printf '%s' '```lisp
(fake)
```'
    else
      printf '%s' '{\"results_digest\":\"arena\",\"served_from_materialization\":true,\"materialization_kind\":\"neighborhood\",\"symbol\":{\"id\":\"sym_fake\",\"name\":\"amoebum:fake\",\"kind\":\"function\",\"file\":\"/tmp/fake.lisp\",\"line\":1,\"col\":2},\"definition_source\":\"(defun fake ())\",\"callers\":[{\"file\":\"/tmp/caller.lisp\",\"line\":4,\"col\":2,\"context_line\":\"(fake)\",\"confidence\":\"may\",\"from_symbol\":\"amoebum:caller\"}],\"callees\":[],\"quality\":{\"index_coverage_pct\":100.0,\"must_edges\":2,\"may_edges\":1,\"unknown_edges\":0,\"has_uncertainty\":true},\"truncation\":{\"budget_chars\":8000,\"llm_window\":\"8k\",\"callers_truncated\":false,\"callees_truncated\":false,\"original_callers\":1,\"original_callees\":0},\"notes\":[\"structural-only\"]}'
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

(defmacro %with-cultivar-daemon-fixture ((root-var script-var index-var log-var) &body body)
  `(let* ((,root-var (%make-temp-directory "amoebum-cultivar-daemon"))
          (,index-var (merge-pathnames "index/" ,root-var))
          (,log-var (merge-pathnames "fake-cultivar.log" ,root-var))
          (,script-var (%cultivar-fake-cli-script ,root-var))
          (old-fake-log (uiop:getenv "CULTIVAR_FAKE_LOG")))
     (unwind-protect
          (progn
            (ensure-directories-exist (merge-pathnames ".keep" ,index-var))
            (setf (uiop:getenv "CULTIVAR_FAKE_LOG") (namestring ,log-var))
            ,@body)
       (setf (uiop:getenv "CULTIVAR_FAKE_LOG") (or old-fake-log ""))
       (%delete-directory-tree-safe ,root-var))))

(test cultivar-daemon-preferred-auto-starts-on-first-query
  "Daemon-preferred adapters auto-start the repo-local daemon and preserve results-digest."
  (%with-cultivar-daemon-fixture (tmp-root fake-cli index log)
    (let* ((adapter (amoebum:make-cultivar-adapter
                     :enabled-p t
                     :binary-path (namestring fake-cli)
                     :root-path tmp-root
                     :index-path index
                     :daemon-mode :prefer
                     :daemon-auto-start-p t))
           (result (amoebum:cultivar-resolve
                    adapter
                    (list :file "/tmp/fake.lisp" :line 1 :col 2)))
           (status (amoebum:cultivar-daemon-status adapter)))
      (is (equal "sym_fake" (getf result :symbol-id)))
      (is (equal "arena" (getf result :results-digest)))
      (is (getf status :running-p))
      (is (probe-file (getf status :socket-path)))
      (let ((log (uiop:read-file-string log)))
        (is (search "start --root" log :test #'char-equal))
        (is (search "query resolve" log :test #'char-equal))))))

(test cultivar-slice-preserves-canonical-materialization-metadata
  "Canonical slice requests preserve daemon materialization markers and structured payload fields."
  (%with-cultivar-daemon-fixture (tmp-root fake-cli index log)
    (let* ((adapter (amoebum:make-cultivar-adapter
                     :enabled-p t
                     :binary-path (namestring fake-cli)
                     :root-path tmp-root
                     :index-path index
                     :daemon-mode :prefer
                     :daemon-auto-start-p t))
           (slice (amoebum:cultivar-slice adapter "sym_fake")))
      (is (eq t (getf slice :slice)))
      (is (equal "sym_fake" (getf slice :symbol-id)))
      (is (eq t (getf slice :served-from-materialization)))
      (is (equal "neighborhood" (getf slice :materialization-kind)))
      (is (equal "amoebum:fake"
                 (getf (getf slice :symbol) :name)))
      (is (= 1 (length (getf slice :callers))))
      (is (equal "structural-only" (first (getf slice :notes))))
      (let ((log (uiop:read-file-string log)))
        (is (search "slice sym_fake --context 2 --budget 8000"
                    log
                    :test #'char-equal))))))

(test cultivar-location-slice-preserves-resolution-and-last-slice-provenance
  "Location-slice resolves a source position into the canonical slice and records the query provenance."
  (%with-cultivar-daemon-fixture (tmp-root fake-cli index _log)
    (let* ((adapter (amoebum:make-cultivar-adapter
                     :enabled-p t
                     :binary-path (namestring fake-cli)
                     :root-path tmp-root
                     :index-path index
                     :daemon-mode :prefer
                     :daemon-auto-start-p t))
           (result (amoebum:cultivar-location-slice
                    adapter
                    (list :file "/tmp/fake.lisp" :line 1 :col 2)))
           (status (amoebum:cultivar-daemon-status adapter))
           (last-slice (getf status :last-slice)))
      (is (eq t (getf result :resolved-p)))
      (is (eq t (getf result :slice)))
      (is (equal "sym_fake" (getf result :symbol-id)))
      (is (equal "/tmp/fake.lisp"
                 (getf (getf result :query) :file)))
      (is (listp (getf result :resolution)))
      (is (equal :location-slice (getf last-slice :origin)))
      (is (equal "/tmp/fake.lisp" (getf last-slice :query-file)))
      (is (= 1 (getf last-slice :query-line)))
      (is (= 2 (getf last-slice :query-col)))
      (is (eq t (getf last-slice :served-from-materialization))))))

(test cultivar-daemon-preferred-throttles-and-reuses-running-socket
  "Once the socket exists, repeated queries reuse it instead of relaunching start."
  (%with-cultivar-daemon-fixture (tmp-root fake-cli index log)
    (let ((adapter (amoebum:make-cultivar-adapter
                    :enabled-p t
                    :binary-path (namestring fake-cli)
                    :root-path tmp-root
                    :index-path index
                    :daemon-mode :prefer
                    :daemon-auto-start-p t)))
      (amoebum:cultivar-resolve adapter (list :file "/tmp/fake.lisp" :line 1 :col 2))
      (amoebum:cultivar-expand adapter "sym_fake")
      (let* ((lines (uiop:split-string (uiop:read-file-string log) :separator '(#\Newline)))
             (non-empty (remove "" lines :test #'string=))
             (start-count (count-if (lambda (line)
                                      (search "start --root" line :test #'char-equal))
                                    non-empty)))
        (is (= 1 start-count))
        (is (= 2 (count-if (lambda (line)
                             (search "query " line :test #'char-equal))
                           non-empty)))))))

;;; ============================================================
;;; NXT-274: live CLI round-trip tests
;;;
;;; Gated on the presence of:
;;;   - a runnable cultivar binary (CULTIVAR_BINARY env var or the
;;;     NXT-273 release path)
;;;   - a pre-built index under .agent/cultivar-index/
;;;
;;; Target symbol: amoebum:chat-ui-submit-input at
;;; amoebum/src/ui/chat-input.lisp:55:10 (0-indexed, updated for NXT-281
;;; smoke report).
;;; ============================================================

(defun %cultivar-live-repo-root ()
  "Return the repo root so tests can synthesize absolute paths
without relying on *default-pathname-defaults*."
  (let ((here (or *load-truename* *compile-file-truename*)))
    (when here
      (make-pathname
       :defaults here
       :name nil :type nil
       :directory (let ((d (pathname-directory here)))
                    (when (and (listp d) (>= (length d) 3))
                      (butlast d 2)))))))

(defun %cultivar-live-adapter ()
  "Build an enabled cultivar-adapter pointing at the real binary
and index, or return NIL when either is absent."
  (let* ((bin (or (uiop:getenv "CULTIVAR_BINARY")
                  "/home/rahul/Documents/cultivar/target/release/cultivar"))
         (repo-root (%cultivar-live-repo-root))
         (idx (and repo-root
                   (merge-pathnames ".agent/cultivar-index/" repo-root))))
    (when (and bin (probe-file bin)
               idx (probe-file idx))
      (amoebum:make-cultivar-adapter
       :enabled-p t
       :binary-path bin
       :index-path idx))))

(defmacro %skip-unless-cultivar-running (&body body)
  "Execute BODY only when a live cultivar binary + index are
reachable. Otherwise record a passing assertion so the test
suite does not silently grow gaps."
  `(let ((adapter (%cultivar-live-adapter)))
     (if (null adapter)
         (is-true t "cultivar binary or index missing — skipping live test")
         (progn ,@body))))

(test cultivar-live-resolve-chat-submit-input
  "Live `cultivar query resolve` resolves chat-ui-submit-input."
  (%skip-unless-cultivar-running
    (let* ((repo (%cultivar-live-repo-root))
           (file (namestring
                  (merge-pathnames "amoebum/src/ui/chat-input.lisp" repo)))
           (result (amoebum:cultivar-resolve
                    adapter
                    (list :file file :line 55 :col 10))))
      (is (listp result))
      (is (member :symbol-id result)
          "resolve must return a :symbol-id key")
      (is (stringp (getf result :symbol-id))
          "symbol-id must be a string when resolution succeeds")
      (is (search "chat_ui_submit_input" (or (getf result :symbol-id) ""))
          "symbol-id should mention chat_ui_submit_input")
      (is (equal "function" (getf result :kind)))
      (is (equal "amoebum:chat-ui-submit-input"
                 (getf result :qualified-name))))))

(test cultivar-live-refs-returns-may-locations
  "Live `cultivar query refs` returns MAY references under structural-only mode."
  (%skip-unless-cultivar-running
    (let* ((repo (%cultivar-live-repo-root))
           (file (namestring
                  (merge-pathnames "amoebum/src/ui/chat-input.lisp" repo)))
           (resolved (amoebum:cultivar-resolve
                      adapter
                      (list :file file :line 55 :col 10)))
           (sym-id (getf resolved :symbol-id)))
      (is (stringp sym-id) "prerequisite: resolve must return a symbol-id")
      (when (stringp sym-id)
        (let ((refs (amoebum:cultivar-expand adapter sym-id)))
          (is (listp refs))
          (is (listp (getf refs :content))
              ":content must be a list of location plists")
          (is (>= (getf refs :may-refs 0) 1)
              "at least one MAY ref expected under structural-only mode")
          (let ((first (first (getf refs :content))))
            (when first
              (is (stringp (getf first :file))))))))))

(test cultivar-live-preview-markdown-slice
  "Live `cultivar slice --markdown` returns a non-empty markdown string."
  (%skip-unless-cultivar-running
    (let* ((repo (%cultivar-live-repo-root))
           (file (namestring
                  (merge-pathnames "amoebum/src/ui/chat-input.lisp" repo)))
           (resolved (amoebum:cultivar-resolve
                      adapter
                      (list :file file :line 55 :col 10)))
           (sym-id (getf resolved :symbol-id)))
      (is (stringp sym-id) "prerequisite: resolve must return a symbol-id")
      (when (stringp sym-id)
        (let ((preview (amoebum:cultivar-preview adapter sym-id :context 1)))
          (is (listp preview))
          (is (stringp (getf preview :preview))
              "slice preview must be a markdown string")
          (is (search "chat-ui-submit-input" (or (getf preview :preview) ""))
              "slice should contain the symbol's source text"))))))

(test cultivar-live-canonical-slice-json
  "Live canonical slice returns structured JSON fields and may expose materialization provenance."
  (%skip-unless-cultivar-running
    (let* ((repo (%cultivar-live-repo-root))
           (file (namestring
                  (merge-pathnames "amoebum/src/ui/chat-input.lisp" repo)))
           (slice (amoebum:cultivar-location-slice
                   adapter
                   (list :file file :line 55 :col 10))))
      (is (eq t (getf slice :resolved-p)))
      (is (eq t (getf slice :slice)))
      (is (listp (getf slice :symbol)))
      (is (stringp (getf (getf slice :symbol) :name)))
      (is (member :callers slice))
      (is (member :quality slice))
      (let* ((status (amoebum:cultivar-daemon-status adapter))
             (last-slice (getf status :last-slice)))
        (is (equal :location-slice (getf last-slice :origin)))
        (is (equal file (getf last-slice :query-file)))))))
