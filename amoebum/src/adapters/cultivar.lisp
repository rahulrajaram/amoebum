(in-package :amoebum)

;;; ============================================================
;;; NXT-106 / NXT-274: Cultivar Adapter
;;;
;;; Wraps the `cultivar` CLI binary to provide symbol resolution,
;;; reference expansion, span preview, and context-pressure
;;; metadata against a pre-built cultivar index.
;;;
;;; Transport: `uiop:run-program` invoking the CLI at
;;; `cultivar-adapter-binary-path` with `--index
;;; cultivar-adapter-index-path`. When daemon-preferred mode is
;;; enabled, the adapter will start a repo-local `cultivar start`
;;; process on first demand and continue issuing normal CLI
;;; commands; Cultivar's own CLI then auto-routes supported query
;;; shapes to the live daemon when its Unix socket is reachable.
;;; JSON responses are parsed with `jonathan:parse` (already a
;;; transitive dependency via pseudopod).
;;;
;;; Gating: if the adapter is NIL, disabled, the binary is
;;; missing, or the index directory does not exist, every public
;;; function returns its legacy empty plist. This preserves the
;;; NXT-106 contract for callers that have no cultivar
;;; installation.
;;; ============================================================

(defun %cultivar-default-root-path ()
  "Return the default repo root for the amoebum checkout."
  (let* ((here (or *load-truename* *compile-file-truename*))
         (root (cond
                 (here
                  ;; amoebum/src/adapters/cultivar.lisp → go up 3 levels to
                  ;; the amoebum/ subdir, then up once more to the repo root.
                  (make-pathname
                   :defaults here
                   :name nil :type nil
                   :directory
                   (let ((dir (pathname-directory here)))
                     (when (and (listp dir) (>= (length dir) 4))
                       (butlast dir 4)))))
                 (t (uiop:getcwd)))))
    (or root (uiop:getcwd))))

(defun %cultivar-index-path-for-root (root)
  (merge-pathnames ".agent/cultivar-index/"
                   (uiop:ensure-directory-pathname root)))

(defun %cultivar-default-index-path ()
  "Return the default cultivar index directory for this repo."
  (%cultivar-index-path-for-root (%cultivar-default-root-path)))

(defun %cultivar-default-binary-path ()
  "Return the binary path, honoring the CULTIVAR_BINARY env var."
  (or (uiop:getenv "CULTIVAR_BINARY")
      "/home/rahul/Documents/cultivar/target/release/cultivar"))

(defstruct (cultivar-adapter
            (:constructor %make-cultivar-adapter))
  "Adapter for the Cultivar CLI.

ENDPOINT         — legacy URL slot retained for backwards
                   compatibility with pre-NXT-274 callers. Unused
                   by the CLI transport.
ENABLED-P        — master gate; NIL means every call returns its
                   legacy empty plist.
INDEX-PATH       — pathname of the cultivar index directory
                   (defaults to .agent/cultivar-index/ under the
                   amoebum repo root).
BINARY-PATH      — absolute path to the cultivar CLI binary
                   (defaults to the NXT-273 release build path;
                   overridden by the CULTIVAR_BINARY env var).
ROOT-PATH        — repo root used when launching `cultivar start`.
DAEMON-MODE      — :OFF for direct CLI-only mode, :PREFER to
                   auto-start and prefer a repo-local daemon.
DAEMON-AUTO-START-P
                 — when true and DAEMON-MODE is :PREFER, launch
                   `cultivar start` on first demand.
DAEMON-LAST-START-AT / STATUS / REASON
                 — lightweight observability for recent startup
                   attempts; used to avoid tight restart loops.
LAST-SLICE
                 — plist describing the most recent canonical
                   slice request Amoebum observed. Used by the
                   status line and `/cultivar` operator command."
  (endpoint "http://localhost:8080" :type string)
  (enabled-p nil :type boolean)
  (index-path nil)
  (binary-path nil :type (or null string))
  (root-path nil)
  (daemon-mode :off :type keyword)
  (daemon-auto-start-p nil :type boolean)
  (daemon-last-start-at nil)
  (daemon-last-start-status nil)
  (daemon-last-start-reason nil)
  (last-slice nil))

(defun make-cultivar-adapter (&key (endpoint "http://localhost:8080")
                                    (enabled-p nil)
                                    (root-path nil root-path-supplied-p)
                                    (index-path nil index-path-supplied-p)
                                    (binary-path nil binary-path-supplied-p)
                                    (daemon-mode :off)
                                    (daemon-auto-start-p nil))
  "Construct a CULTIVAR-ADAPTER.

Keyword arguments:
  :ENDPOINT     — legacy HTTP URL (retained for compatibility).
  :ENABLED-P    — when NIL all calls return empty stubs (default NIL).
  :ROOT-PATH    — repo root pathname used for daemon startup.
  :INDEX-PATH   — pathname of the cultivar index directory. Defaults
                  to .agent/cultivar-index/ under the repo root.
  :BINARY-PATH  — absolute path to the `cultivar` binary. Defaults
                  to /home/rahul/Documents/cultivar/target/release/cultivar,
                  overridden by the CULTIVAR_BINARY env var.
  :DAEMON-MODE  — :OFF (default) or :PREFER.
  :DAEMON-AUTO-START-P — when true, launch a repo-local daemon on
                  first demand when the socket is absent."
  (let ((resolved-root (if root-path-supplied-p
                           root-path
                           (%cultivar-default-root-path))))
    (%make-cultivar-adapter
     :endpoint    endpoint
     :enabled-p   (and enabled-p t)
     :root-path   resolved-root
     :index-path  (if index-path-supplied-p
                      index-path
                      (%cultivar-index-path-for-root resolved-root))
     :binary-path (if binary-path-supplied-p
                      binary-path
                      (%cultivar-default-binary-path))
     :daemon-mode daemon-mode
     :daemon-auto-start-p (and daemon-auto-start-p t))))

(defparameter *cultivar-adapter* nil
  "The process-wide default CULTIVAR-ADAPTER instance.
NIL means no adapter is configured.")

;;; ------------------------------------------------------------------
;;; Internal helpers
;;; ------------------------------------------------------------------

(defparameter *cultivar-daemon-start-throttle-seconds* 5
  "Minimum seconds between daemon start attempts for the same
adapter when the socket is absent.")

(defun %cultivar-root-namestring (adapter)
  (if (null adapter)
      (namestring (%cultivar-default-root-path))
      (let ((root (cultivar-adapter-root-path adapter)))
        (typecase root
          (pathname (namestring (uiop:ensure-directory-pathname root)))
          (string root)
          (t (namestring (%cultivar-default-root-path)))))))

(defun %cultivar-usable-p (adapter)
  "T iff ADAPTER is a non-NIL, enabled adapter whose binary and
index both exist on disk. Used to gate every live call."
  (and adapter
       (cultivar-adapter-p adapter)
       (cultivar-adapter-enabled-p adapter)
       (let ((bin (cultivar-adapter-binary-path adapter))
             (idx (cultivar-adapter-index-path adapter)))
         (and bin
              (probe-file bin)
              idx
              (probe-file (if (pathnamep idx)
                              idx
                              (pathname idx)))))))

(defun %cultivar-index-namestring (adapter)
  (let ((idx (and adapter (cultivar-adapter-index-path adapter))))
    (if (pathnamep idx) (namestring idx) idx)))

(defun %cultivar-daemon-socket-path (adapter)
  (let ((idx (and adapter (cultivar-adapter-index-path adapter))))
    (when idx
      (merge-pathnames ".cultivar.sock"
                       (if (pathnamep idx)
                           (uiop:ensure-directory-pathname idx)
                           (uiop:ensure-directory-pathname (pathname idx)))))))

(defun %cultivar-daemon-running-p (adapter)
  (let ((socket (%cultivar-daemon-socket-path adapter)))
    (and socket (probe-file socket) t)))

(defun %cultivar-daemon-preferred-p (adapter)
  (and adapter
       (cultivar-adapter-p adapter)
       (eq (cultivar-adapter-daemon-mode adapter) :prefer)))

(defun %cultivar-daemon-start-throttled-p (adapter)
  (let ((last-at (cultivar-adapter-daemon-last-start-at adapter)))
    (and last-at
         (< (- (get-universal-time) last-at)
            *cultivar-daemon-start-throttle-seconds*))))

(defun %cultivar-start-daemon (adapter)
  (unless (%cultivar-usable-p adapter)
    (return-from %cultivar-start-daemon nil))
  (let* ((binary (cultivar-adapter-binary-path adapter))
         (root (%cultivar-root-namestring adapter))
         (index (%cultivar-index-namestring adapter)))
    (handler-case
        (let ((process (uiop:launch-program
                        (list binary "start" "--root" root "--index" index)
                        :output nil
                        :error-output nil)))
          (setf (cultivar-adapter-daemon-last-start-at adapter) (get-universal-time)
                (cultivar-adapter-daemon-last-start-status adapter) :launched
                (cultivar-adapter-daemon-last-start-reason adapter) nil)
          #+sb-thread
          (sb-thread:make-thread
           (lambda ()
             (ignore-errors (uiop:wait-process process)))
           :name "amoebum-cultivar-daemon-reaper")
          t)
      (error (condition)
        (setf (cultivar-adapter-daemon-last-start-at adapter) (get-universal-time)
              (cultivar-adapter-daemon-last-start-status adapter) :error
              (cultivar-adapter-daemon-last-start-reason adapter)
              (princ-to-string condition))
        nil))))

(defun %cultivar-wait-for-daemon-socket (adapter &key (attempts 20) (sleep-seconds 0.05))
  (loop repeat attempts
        when (%cultivar-daemon-running-p adapter)
          do (return t)
        do (sleep sleep-seconds)
        finally (return nil)))

(defun %cultivar-ensure-daemon-ready (adapter)
  (when (and (%cultivar-daemon-preferred-p adapter)
             (cultivar-adapter-daemon-auto-start-p adapter)
             (not (%cultivar-daemon-running-p adapter))
             (not (%cultivar-daemon-start-throttled-p adapter)))
    (%cultivar-start-daemon adapter)
    (%cultivar-wait-for-daemon-socket adapter))
  (%cultivar-daemon-running-p adapter))

(defun cultivar-daemon-status (adapter)
  "Return a small status plist for ADAPTER's repo-local daemon.

This is intentionally lightweight on the amoebum side: it reports
the configured mode, whether auto-start is enabled, where the
socket would live, and the latest observed launch state. It does
not speak Cultivar's socket protocol directly in v1."
  (let ((socket (%cultivar-daemon-socket-path adapter)))
    (list :mode (and adapter (cultivar-adapter-daemon-mode adapter))
          :auto-start-p (and adapter
                             (cultivar-adapter-daemon-auto-start-p adapter))
          :running-p (%cultivar-daemon-running-p adapter)
          :socket-path (and socket (namestring socket))
          :index-path (and adapter (%cultivar-index-namestring adapter))
          :root-path (and adapter (%cultivar-root-namestring adapter))
          :last-start-at (and adapter
                              (cultivar-adapter-daemon-last-start-at adapter))
          :last-start-status (and adapter
                                  (cultivar-adapter-daemon-last-start-status adapter))
          :last-start-reason (and adapter
                                  (cultivar-adapter-daemon-last-start-reason adapter))
          :last-slice (and adapter
                           (cultivar-adapter-last-slice adapter)))))

(defun %cultivar-run (adapter args &key (parse-json t))
  "Invoke the cultivar CLI as ADAPTER with ARGS (a list of strings).

Returns two values: the parsed JSON payload (hash-table) when
PARSE-JSON is true and the command succeeded, or the raw stdout
string when PARSE-JSON is false. The secondary value is a plist
of diagnostic keys: (:exit-code N :stderr S).

On non-zero exit, JSON parse failure, or any condition from
`uiop:run-program`, returns (values NIL (:error <reason>))."
  (%cultivar-ensure-daemon-ready adapter)
  (let* ((binary (cultivar-adapter-binary-path adapter))
         (index (%cultivar-index-namestring adapter))
         (full-args (append args (list "--index" index))))
    (handler-case
        (multiple-value-bind (stdout stderr exit-code)
            (uiop:run-program (cons binary full-args)
                              :output :string
                              :error-output :string
                              :ignore-error-status t)
          (cond
            ((not (zerop exit-code))
             (values nil (list :error :non-zero-exit
                               :exit-code exit-code
                               :stderr stderr)))
            ((not parse-json)
             (values stdout (list :exit-code exit-code
                                  :stderr stderr)))
            (t
             (handler-case
                 (values (jonathan:parse stdout :as :hash-table)
                         (list :exit-code exit-code :stderr stderr))
               (error (c)
                 (values nil (list :error :json-parse
                                   :condition (princ-to-string c)
                                   :stdout stdout)))))))
      (error (c)
        (values nil (list :error :run-program
                          :condition (princ-to-string c)))))))

(defun %cultivar-hash-get (ht key)
  "Look up KEY in HT, tolerating both string and keyword keys."
  (when (hash-table-p ht)
    (or (gethash key ht)
        (gethash (string-downcase key) ht))))

(defun %cultivar-hash-list (ht key)
  (let ((value (%cultivar-hash-get ht key)))
    (if (listp value) value '())))

(defun %cultivar-slice-symbol (payload)
  (let ((symbol (%cultivar-hash-get payload "symbol")))
    (when (hash-table-p symbol)
      (list :id (%cultivar-hash-get symbol "id")
            :name (%cultivar-hash-get symbol "name")
            :kind (%cultivar-hash-get symbol "kind")
            :file (%cultivar-hash-get symbol "file")
            :line (%cultivar-hash-get symbol "line")
            :col (%cultivar-hash-get symbol "col")))))

(defun %cultivar-slice-callers (payload)
  (loop for caller in (%cultivar-hash-list payload "callers")
        when (hash-table-p caller)
          collect (list :file (%cultivar-hash-get caller "file")
                        :line (%cultivar-hash-get caller "line")
                        :col (%cultivar-hash-get caller "col")
                        :context-line (%cultivar-hash-get caller "context_line")
                        :confidence (%cultivar-hash-get caller "confidence")
                        :from-symbol (%cultivar-hash-get caller "from_symbol"))))

(defun %cultivar-slice-callees (payload)
  (loop for callee in (%cultivar-hash-list payload "callees")
        when (hash-table-p callee)
          collect (list :symbol-id (%cultivar-hash-get callee "symbol_id")
                        :name (%cultivar-hash-get callee "name")
                        :kind (%cultivar-hash-get callee "kind")
                        :file (%cultivar-hash-get callee "file")
                        :line (%cultivar-hash-get callee "line"))))

(defun %cultivar-slice-truncation (payload)
  (let ((truncation (%cultivar-hash-get payload "truncation")))
    (when (hash-table-p truncation)
      (list :budget-chars (%cultivar-hash-get truncation "budget_chars")
            :original-callers (%cultivar-hash-get truncation "original_callers")
            :original-callees (%cultivar-hash-get truncation "original_callees")
            :callers-truncated (%cultivar-hash-get truncation "callers_truncated")
            :callees-truncated (%cultivar-hash-get truncation "callees_truncated")
            :llm-window (%cultivar-hash-get truncation "llm_window")))))

(defun %cultivar-slice-quality (payload)
  (let ((quality (%cultivar-hash-get payload "quality")))
    (when (hash-table-p quality)
      (list :index-coverage-pct (%cultivar-hash-get quality "index_coverage_pct")
            :must-edges (%cultivar-hash-get quality "must_edges")
            :may-edges (%cultivar-hash-get quality "may_edges")
            :unknown-edges (%cultivar-hash-get quality "unknown_edges")
            :has-uncertainty (%cultivar-hash-get quality "has_uncertainty")
            :warning (%cultivar-hash-get quality "warning")))))

(defun %cultivar-record-last-slice (adapter &key origin symbol-id query
                                                 results-digest
                                                 served-from-materialization
                                                 materialization-kind
                                                 quality notes)
  (when (and adapter (cultivar-adapter-p adapter))
    (setf (cultivar-adapter-last-slice adapter)
          (list :recorded-at (get-universal-time)
                :origin origin
                :symbol-id symbol-id
                :query-file (and (listp query) (getf query :file))
                :query-line (and (listp query) (getf query :line))
                :query-col (and (listp query) (getf query :col))
                :results-digest results-digest
                :served-from-materialization served-from-materialization
                :materialization-kind materialization-kind
                :quality-warning (and (listp quality)
                                      (getf quality :warning))
                :notes (if (listp notes) notes '())))))

;;; ------------------------------------------------------------------
;;; Public API
;;; ------------------------------------------------------------------

(defun cultivar-resolve (adapter query)
  "Resolve QUERY against the Cultivar index via ADAPTER.

QUERY is either a plist (:file F :line L :col C) designating a
source position, or a legacy string (in which case the call is
treated as a no-op and returns the empty stub — string-keyword
queries are not supported by the CLI transport).

Returns a plist. In the live path:
  (:results <list> :symbol-id <id> :kind <kind> :file <path>
   :line <n> :col <n> :qualified-name <str>
   :confidence <level> :reference-mode <mode>)

In the disabled / missing-binary / missing-index / non-plist
path returns the legacy (:results NIL) stub."
  (unless (%cultivar-usable-p adapter)
    (return-from cultivar-resolve (list :results '())))
  (unless (and (listp query) (getf query :file))
    (return-from cultivar-resolve (list :results '())))
  (let* ((file (getf query :file))
         (line (or (getf query :line) 0))
         (col  (or (getf query :col) 0))
         (args (list "query" "resolve"
                     "--file" (princ-to-string file)
                     "--line" (princ-to-string line)
                     "--col"  (princ-to-string col))))
    (multiple-value-bind (payload diag) (%cultivar-run adapter args)
      (declare (ignore diag))
      (if (hash-table-p payload)
          (let* ((symbol (%cultivar-hash-get payload "symbol"))
                 (range  (and (hash-table-p symbol)
                              (%cultivar-hash-get symbol "range")))
                 (conf   (%cultivar-hash-get payload "confidence"))
                 (refctx (%cultivar-hash-get payload "reference_context")))
            (list :results (if symbol (list symbol) '())
                  :results-digest (%cultivar-hash-get payload "results_digest")
                  :symbol-id (and symbol (%cultivar-hash-get symbol "id"))
                  :kind       (and symbol (%cultivar-hash-get symbol "kind"))
                  :file       (and symbol (%cultivar-hash-get symbol "file"))
                  :line       (and range (%cultivar-hash-get range "start_line"))
                  :col        (and range (%cultivar-hash-get range "start_col"))
                  :qualified-name (and symbol (%cultivar-hash-get symbol "qualified_name"))
                  :confidence (and (hash-table-p conf)
                                   (%cultivar-hash-get conf "level"))
                  :reference-mode (and (hash-table-p refctx)
                                       (%cultivar-hash-get refctx "reference_mode"))))
          (list :results '())))))

(defun cultivar-expand (adapter context-id)
  "Return a plist of references for CONTEXT-ID (a cultivar symbol-id).

Result:
  (:content <list-of-location-plists>
   :must-refs <n> :may-refs <n> :unknown-refs <n>
   :reference-mode <mode>)

Each location plist has (:file :start-line :start-col
:end-line :end-col :kind :confidence). Under structural-only
mode (cl-lsp unavailable) every reference is reported as MAY —
callers should treat these as candidates, not authoritative
call-graph answers.

Disabled / unusable adapter returns the legacy (:content NIL) stub."
  (unless (%cultivar-usable-p adapter)
    (return-from cultivar-expand (list :content nil)))
  (unless (and context-id (stringp context-id))
    (return-from cultivar-expand (list :content nil)))
  (let ((args (list "query" "refs" "--symbol-id" context-id)))
    (multiple-value-bind (payload diag) (%cultivar-run adapter args)
      (declare (ignore diag))
      (if (hash-table-p payload)
          (let* ((must (%cultivar-hash-get payload "must_refs"))
                 (may  (%cultivar-hash-get payload "may_refs"))
                 (unk  (%cultivar-hash-get payload "unknown_refs"))
                 (mode (%cultivar-hash-get payload "reference_mode"))
                 (locations
                   (loop for bucket in (list (cons :must must)
                                             (cons :may may)
                                             (cons :unknown unk))
                         for confidence = (car bucket)
                         for items = (cdr bucket)
                         when (listp items)
                           append (loop for ref in items
                                        for range = (and (hash-table-p ref)
                                                         (%cultivar-hash-get ref "range"))
                                        collect
                                        (list :file (and (hash-table-p ref)
                                                         (%cultivar-hash-get ref "file"))
                                              :kind (and (hash-table-p ref)
                                                         (%cultivar-hash-get ref "kind"))
                                              :symbol-id (and (hash-table-p ref)
                                                              (%cultivar-hash-get ref "symbol_id"))
                                              :start-line (and range (%cultivar-hash-get range "start_line"))
                                              :start-col  (and range (%cultivar-hash-get range "start_col"))
                                              :end-line   (and range (%cultivar-hash-get range "end_line"))
                                              :end-col    (and range (%cultivar-hash-get range "end_col"))
                                              :confidence confidence)))))
            (list :content locations
                  :results-digest (%cultivar-hash-get payload "results_digest")
                  :must-refs (if (listp must) (length must) 0)
                  :may-refs  (if (listp may) (length may) 0)
                  :unknown-refs (if (listp unk) (length unk) 0)
                  :reference-mode mode))
          (list :content nil)))))

(defun cultivar-slice (adapter symbol-id &key (context 2) (budget 8000))
  "Return the canonical JSON slice plist for SYMBOL-ID.

This is the machine-facing Cultivar retrieval surface. Amoebum
uses the daemon-friendly canonical shape by default:

  cultivar slice <id> --context 2 --budget 8000

When the daemon is warm, this lets Cultivar reuse neighborhood
materializations. The returned plist mirrors the stable JSON
shape closely enough for tool callers:

  (:symbol <plist>
   :definition-source <string-or-nil>
   :callers <list>
   :callees <list>
   :truncation <plist-or-nil>
   :quality <plist-or-nil>
   :notes <list>
   :served-from-materialization <boolean-or-nil>
   :materialization-kind <string-or-nil>)

Disabled / unusable adapter or a non-string SYMBOL-ID returns
the stub (:slice NIL)."
  (unless (%cultivar-usable-p adapter)
    (return-from cultivar-slice (list :slice nil)))
  (unless (and symbol-id (stringp symbol-id))
    (return-from cultivar-slice (list :slice nil)))
  (let ((args (list "slice" symbol-id
                    "--context" (princ-to-string context)
                    "--budget" (princ-to-string budget))))
    (multiple-value-bind (payload diag) (%cultivar-run adapter args)
      (declare (ignore diag))
      (if (hash-table-p payload)
          (let* ((symbol (%cultivar-slice-symbol payload))
                 (resolved-id (or (and (listp symbol) (getf symbol :id))
                                  symbol-id))
                 (results-digest (%cultivar-hash-get payload "results_digest"))
                 (served-from-materialization
                   (%cultivar-hash-get payload "served_from_materialization"))
                 (materialization-kind
                   (%cultivar-hash-get payload "materialization_kind"))
                 (quality (%cultivar-slice-quality payload))
                 (notes (%cultivar-hash-list payload "notes"))
                 (result
                   (list :slice t
                         :symbol-id resolved-id
                         :results-digest results-digest
                         :served-from-materialization served-from-materialization
                         :materialization-kind materialization-kind
                         :symbol symbol
                         :definition-source (%cultivar-hash-get payload "definition_source")
                         :callers (%cultivar-slice-callers payload)
                         :callees (%cultivar-slice-callees payload)
                         :truncation (%cultivar-slice-truncation payload)
                         :quality quality
                         :notes notes)))
            (%cultivar-record-last-slice
             adapter
             :origin :symbol-slice
             :symbol-id resolved-id
             :results-digest results-digest
             :served-from-materialization served-from-materialization
             :materialization-kind materialization-kind
             :quality quality
             :notes notes)
            result)
          (list :slice nil)))))

(defun cultivar-location-slice (adapter query &key (context 2) (budget 8000))
  "Resolve QUERY and then fetch the canonical JSON slice for it.

QUERY must be a plist containing :FILE and optional :LINE / :COL.
This is Amoebum's high-level machine-facing retrieval path for
editor locations: it keeps the canonical `cultivar slice`
payload while preserving the resolve provenance that led to it.

Result:
  (:resolved-p <boolean>
   :slice <boolean>
   :query <plist>
   :resolution <resolve-plist-or-stub>
   :symbol-id <string-or-nil>
   ...canonical slice fields...)

Disabled / unusable adapter, malformed QUERY, or unresolved
locations return (:resolved-p NIL :slice NIL ...) without
signalling."
  (let ((normalized-query
          (and (listp query)
               (getf query :file)
               (list :file (getf query :file)
                     :line (or (getf query :line) 0)
                     :col (or (getf query :col) 0)))))
    (unless (%cultivar-usable-p adapter)
      (return-from cultivar-location-slice
        (list :resolved-p nil :slice nil :query normalized-query :resolution (list :results '()))))
    (unless normalized-query
      (return-from cultivar-location-slice
        (list :resolved-p nil :slice nil :query nil :resolution (list :results '()))))
    (let* ((resolution (cultivar-resolve adapter normalized-query))
           (symbol-id (getf resolution :symbol-id)))
      (unless (and symbol-id (stringp symbol-id))
        (return-from cultivar-location-slice
          (list :resolved-p nil
                :slice nil
                :query normalized-query
                :resolution resolution
                :symbol-id nil)))
      (let ((slice (cultivar-slice adapter symbol-id :context context :budget budget)))
        (when (eq t (getf slice :slice))
          (%cultivar-record-last-slice
           adapter
           :origin :location-slice
           :query normalized-query
           :symbol-id (or (getf slice :symbol-id) symbol-id)
           :results-digest (getf slice :results-digest)
           :served-from-materialization (getf slice :served-from-materialization)
           :materialization-kind (getf slice :materialization-kind)
           :quality (getf slice :quality)
           :notes (getf slice :notes)))
        (append (list :resolved-p t
                      :query normalized-query
                      :resolution resolution)
                slice)))))

(defun cultivar-preview (adapter context-id &key (context 3))
  "Return a human-readable markdown slice string for CONTEXT-ID.

CONTEXT-ID is a cultivar symbol-id. CONTEXT is the number of
source context lines to include on either side of the
definition. Result plist: (:preview <markdown-string>
:symbol-id <id>).

This deliberately uses the non-canonical `--markdown` rendering
path for human inspection. Machine-facing callers should prefer
`cultivar-slice`, which preserves the daemon-friendly canonical
JSON shape.

Disabled / unusable adapter or a non-string CONTEXT-ID returns
the legacy (:preview NIL) stub."
  (unless (%cultivar-usable-p adapter)
    (return-from cultivar-preview (list :preview nil)))
  (unless (and context-id (stringp context-id))
    (return-from cultivar-preview (list :preview nil)))
  (let ((args (list "slice" context-id
                    "--markdown"
                    "--context" (princ-to-string context))))
    (multiple-value-bind (output diag) (%cultivar-run adapter args :parse-json nil)
      (declare (ignore diag))
      (if (and output (stringp output) (plusp (length output)))
          (list :preview output :symbol-id context-id)
          (list :preview nil)))))

;;; ------------------------------------------------------------------
;;; NXT-109: Context pressure metadata
;;; ------------------------------------------------------------------

(defun cultivar-context-pressure (adapter &key symbol-id (budget 1000) (llm-window "8k"))
  "Return a context-pressure metadata plist for ADAPTER.

When SYMBOL-ID is provided and the adapter is usable, invokes
`cultivar slice <id> --budget <budget> --llm-window <win>` and
returns the budget / truncation metadata from the slice JSON:

  (:available-contexts N
   :staleness-seconds 0
   :budget-chars B
   :llm-window W
   :callers-truncated P
   :callees-truncated P
   :original-callers C
   :original-callees C)

When SYMBOL-ID is NIL or the adapter is unusable, returns the
legacy stub (:available-contexts 0 :staleness-seconds 0)."
  (let ((stub (list :available-contexts 0 :staleness-seconds 0)))
    (unless (%cultivar-usable-p adapter)
      (return-from cultivar-context-pressure stub))
    (unless (and symbol-id (stringp symbol-id))
      (return-from cultivar-context-pressure stub))
    (let ((args (list "slice" symbol-id
                      "--budget" (princ-to-string budget)
                      "--llm-window" llm-window)))
      (multiple-value-bind (payload diag) (%cultivar-run adapter args)
        (declare (ignore diag))
        (if (hash-table-p payload)
            (let* ((trunc (%cultivar-hash-get payload "truncation"))
                   (quality (%cultivar-hash-get payload "quality")))
              (list :available-contexts
                    (if (hash-table-p quality)
                        (or (%cultivar-hash-get quality "must_edges") 0)
                        0)
                    :results-digest (%cultivar-hash-get payload "results_digest")
                    :staleness-seconds 0
                    :budget-chars (and (hash-table-p trunc)
                                       (%cultivar-hash-get trunc "budget_chars"))
                    :llm-window (and (hash-table-p trunc)
                                     (%cultivar-hash-get trunc "llm_window"))
                    :callers-truncated (and (hash-table-p trunc)
                                            (%cultivar-hash-get trunc "callers_truncated"))
                    :callees-truncated (and (hash-table-p trunc)
                                            (%cultivar-hash-get trunc "callees_truncated"))
                    :original-callers (and (hash-table-p trunc)
                                           (%cultivar-hash-get trunc "original_callers"))
                    :original-callees (and (hash-table-p trunc)
                                           (%cultivar-hash-get trunc "original_callees"))))
            stub)))))
