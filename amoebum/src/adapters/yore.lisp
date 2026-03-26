(in-package :amoebum)

;;; ============================================================
;;; NXT-107: Yore Adapter
;;;
;;; Provides a stub adapter for the Yore service v1 JSON contract,
;;; covering search-context and fetch-context.
;;; The adapter is disabled by default; all operations return
;;; empty/nil stubs until :enabled-p is set to T and wired to
;;; a live endpoint.
;;; ============================================================

(defstruct (yore-adapter
            (:constructor %make-yore-adapter))
  "Adapter for the Yore context search/retrieval service.

ENDPOINT is the base URL for the Yore HTTP API (default
\"http://localhost:9090\").  ENABLED-P must be T for operations to
be forwarded; when NIL every call returns a safe empty stub."
  (endpoint "http://localhost:9090" :type string)
  (enabled-p nil :type boolean))

(defun make-yore-adapter (&key (endpoint "http://localhost:9090")
                                (enabled-p nil))
  "Construct a YORE-ADAPTER.

ENDPOINT  — base URL string (default \"http://localhost:9090\").
ENABLED-P — boolean; when NIL all calls return stub values (default NIL)."
  (%make-yore-adapter :endpoint  endpoint
                       :enabled-p (and enabled-p t)))

;;; Global default adapter (nil means no adapter configured).
(defparameter *yore-adapter* nil
  "The process-wide default YORE-ADAPTER instance.
NIL means no adapter is configured.")

;;; ------------------------------------------------------------------
;;; Public API
;;; ------------------------------------------------------------------

(defun yore-search-context (adapter query &key limit)
  "Search for contexts matching QUERY via ADAPTER.

Returns a (possibly empty) list of context plists.  When the adapter
is disabled or ADAPTER is NIL the stub returns NIL.

ADAPTER — a YORE-ADAPTER instance (or NIL for stub behaviour).
QUERY   — a string search expression.
LIMIT   — optional positive integer upper bound on result count."
  (declare (ignore query limit))
  (when (and adapter
             (yore-adapter-p adapter)
             (yore-adapter-enabled-p adapter))
    ;; Real implementation would POST to
    ;;   (yore-adapter-endpoint adapter) + "/v1/context/search"
    ;; with JSON body {:query QUERY, :limit LIMIT} and parse the
    ;; response array into a list of plists.
    (return-from yore-search-context '()))
  '())

(defun yore-fetch-context (adapter context-id)
  "Fetch a single context by CONTEXT-ID via ADAPTER.

Returns a plist for the context, or NIL when the adapter is disabled
or ADAPTER is NIL (stub behaviour).

ADAPTER     — a YORE-ADAPTER instance (or NIL for stub behaviour).
CONTEXT-ID  — a string identifier for the context to retrieve."
  (declare (ignore context-id))
  (when (and adapter
             (yore-adapter-p adapter)
             (yore-adapter-enabled-p adapter))
    ;; Real implementation would GET
    ;;   (yore-adapter-endpoint adapter) + "/v1/context/" CONTEXT-ID
    ;; and deserialise the JSON object into a plist.
    (return-from yore-fetch-context nil))
  nil)

;;; ------------------------------------------------------------------
;;; NXT-109: Context pressure metadata
;;; ------------------------------------------------------------------

(defun yore-context-pressure (adapter)
  "Return a pressure-metadata plist for ADAPTER.

The plist has the following keys:
  :indexed-documents     — integer count of documents in the Yore
                           search index (stub: always 0)
  :last-index-seconds-ago — integer seconds since the index was last
                            rebuilt (stub: always 0)

When ADAPTER is NIL or disabled the stub values are returned
unconditionally; no network I/O is performed in this release.

ADAPTER — a YORE-ADAPTER instance (or NIL for stub behaviour)."
  (declare (ignore adapter))
  ;; Stub implementation — real version would query
  ;;   GET (yore-adapter-endpoint adapter) + "/v1/status"
  ;; and parse {:indexed_documents N, :last_index_seconds_ago M}.
  (list :indexed-documents      0
        :last-index-seconds-ago 0))

;;; ------------------------------------------------------------------
;;; NXT-109: Combined context-pressure summary for the status bar
;;; ------------------------------------------------------------------

(defun %context-pressure-summary (&key
                                    (ide-ctx   *ide-context*)
                                    (cultivar  *cultivar-adapter*)
                                    (yore      *yore-adapter*))
  "Return a single-line status string combining IDE, Cultivar, and Yore pressure.

The string has the form:
  \"ctx: IDE <N>sel/<D>diag | cult <C>ctx | yore <Y>docs\"

where:
  N  = number of selections in the IDE context (0 when no IDE context)
  D  = number of diagnostics in the IDE context (0 when no IDE context)
  C  = :available-contexts from CULTIVAR-CONTEXT-PRESSURE
  Y  = :indexed-documents from YORE-CONTEXT-PRESSURE

Arguments:
  :IDE-CTX  — an IDE-CONTEXT object or NIL (default *IDE-CONTEXT*)
  :CULTIVAR — a CULTIVAR-ADAPTER instance or NIL (default *CULTIVAR-ADAPTER*)
  :YORE     — a YORE-ADAPTER instance or NIL (default *YORE-ADAPTER*)"
  (let* ((sel-count  (if (ide-context-p ide-ctx)
                         (length (ide-context-selections  ide-ctx))
                         0))
         (diag-count (if (ide-context-p ide-ctx)
                         (length (ide-context-diagnostics ide-ctx))
                         0))
         (cult-pressure (cultivar-context-pressure cultivar))
         (yore-pressure (yore-context-pressure     yore))
         (cult-ctxs  (or (getf cult-pressure :available-contexts)  0))
         (yore-docs  (or (getf yore-pressure  :indexed-documents)   0)))
    (format nil "ctx: IDE ~Dsel/~Ddiag | cult ~Dctx | yore ~Ddocs"
            sel-count diag-count cult-ctxs yore-docs)))
