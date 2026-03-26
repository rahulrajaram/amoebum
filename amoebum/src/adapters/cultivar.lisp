(in-package :amoebum)

;;; ============================================================
;;; NXT-106: Cultivar Adapter
;;;
;;; Provides a stub adapter for the Cultivar service MCP contract,
;;; covering query.resolve, context.preview, and context.expand.
;;; The adapter is disabled by default; all operations return
;;; empty/nil stubs until :enabled-p is set to T and wired to
;;; a live endpoint.
;;; ============================================================

(defstruct (cultivar-adapter
            (:constructor %make-cultivar-adapter))
  "Adapter for the Cultivar query/context service.

ENDPOINT is the base URL for the Cultivar HTTP API (default
\"http://localhost:8080\").  ENABLED-P must be T for operations to
be forwarded; when NIL every call returns a safe empty stub."
  (endpoint "http://localhost:8080" :type string)
  (enabled-p nil :type boolean))

(defun make-cultivar-adapter (&key (endpoint "http://localhost:8080")
                                    (enabled-p nil))
  "Construct a CULTIVAR-ADAPTER.

ENDPOINT  — base URL string (default \"http://localhost:8080\").
ENABLED-P — boolean; when NIL all calls return stub values (default NIL)."
  (%make-cultivar-adapter :endpoint  endpoint
                           :enabled-p (and enabled-p t)))

;;; Global default adapter (nil means no adapter configured).
(defparameter *cultivar-adapter* nil
  "The process-wide default CULTIVAR-ADAPTER instance.
NIL means no adapter is configured.")

;;; ------------------------------------------------------------------
;;; Public API
;;; ------------------------------------------------------------------

(defun cultivar-resolve (adapter query)
  "Resolve QUERY against the Cultivar service via ADAPTER.

Returns a plist of the form (:RESULTS <list>).  When the adapter is
disabled or ADAPTER is NIL the stub returns (:RESULTS NIL).

ADAPTER — a CULTIVAR-ADAPTER instance (or NIL to use a no-op stub).
QUERY   — a string query expression."
  (declare (ignore query))
  (when (and adapter
             (cultivar-adapter-p adapter)
             (cultivar-adapter-enabled-p adapter))
    ;; Real implementation would POST to
    ;;   (cultivar-adapter-endpoint adapter) + "/query/resolve"
    ;; with QUERY as the request body and parse the JSON response.
    (return-from cultivar-resolve
      (list :results '())))
  (list :results '()))

(defun cultivar-preview (adapter context-id)
  "Fetch a preview for CONTEXT-ID from the Cultivar service via ADAPTER.

Returns a plist of the form (:PREVIEW <value>).  When the adapter is
disabled or ADAPTER is NIL the stub returns (:PREVIEW NIL).

ADAPTER     — a CULTIVAR-ADAPTER instance (or NIL for stub behaviour).
CONTEXT-ID  — a string identifier for the context to preview."
  (declare (ignore context-id))
  (when (and adapter
             (cultivar-adapter-p adapter)
             (cultivar-adapter-enabled-p adapter))
    ;; Real implementation would GET
    ;;   (cultivar-adapter-endpoint adapter) + "/context/preview/" CONTEXT-ID
    (return-from cultivar-preview
      (list :preview nil)))
  (list :preview nil))

(defun cultivar-expand (adapter context-id)
  "Fetch the full expanded content for CONTEXT-ID via ADAPTER.

Returns a plist of the form (:CONTENT <value>).  When the adapter is
disabled or ADAPTER is NIL the stub returns (:CONTENT NIL).

ADAPTER     — a CULTIVAR-ADAPTER instance (or NIL for stub behaviour).
CONTEXT-ID  — a string identifier for the context to expand."
  (declare (ignore context-id))
  (when (and adapter
             (cultivar-adapter-p adapter)
             (cultivar-adapter-enabled-p adapter))
    ;; Real implementation would GET
    ;;   (cultivar-adapter-endpoint adapter) + "/context/expand/" CONTEXT-ID
    (return-from cultivar-expand
      (list :content nil)))
  (list :content nil))

;;; ------------------------------------------------------------------
;;; NXT-109: Context pressure metadata
;;; ------------------------------------------------------------------

(defun cultivar-context-pressure (adapter)
  "Return a pressure-metadata plist for ADAPTER.

The plist has the following keys:
  :available-contexts — integer count of contexts available in the
                        Cultivar service index (stub: always 0)
  :staleness-seconds  — integer seconds since the index was last
                        refreshed (stub: always 0)

When ADAPTER is NIL or disabled the stub values are returned
unconditionally; no network I/O is performed in this release.

ADAPTER — a CULTIVAR-ADAPTER instance (or NIL for stub behaviour)."
  (declare (ignore adapter))
  ;; Stub implementation — real version would query
  ;;   GET (cultivar-adapter-endpoint adapter) + "/status"
  ;; and parse {:available_contexts N, :last_refresh_seconds_ago M}.
  (list :available-contexts 0
        :staleness-seconds  0))
