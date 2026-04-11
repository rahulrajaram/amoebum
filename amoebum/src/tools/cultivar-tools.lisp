(in-package :amoebum)

;;; ============================================================
;;; NXT-275: Cultivar deftool wrappers
;;;
;;; Exposes the NXT-274 cultivar adapter as first-class
;;; amoebum tools:
;;;   - cultivar-symbol-resolve   — position → symbol-id/kind/name
;;;   - cultivar-location-slice   — position → canonical JSON slice
;;;   - cultivar-symbol-references — symbol-id → reference locations
;;;   - cultivar-symbol-slice     — symbol-id → canonical JSON slice
;;;   - cultivar-span-preview      — symbol-id → markdown source slice
;;;
;;; Each tool builds (or reuses) an adapter pointing at the
;;; default binary + index and degrades gracefully to a
;;; diagnostic string when cultivar is not available on disk.
;;; ============================================================

(defparameter *cultivar-tool-adapter* nil
  "Process-wide adapter used by the cultivar-* deftool entries.
When NIL the tools build one lazily via %cultivar-tool-adapter.")

(defparameter *cultivar-tool-unavailable-message*
  "cultivar unavailable: set CULTIVAR_BINARY and ensure .agent/cultivar-index exists"
  "Diagnostic string emitted by the cultivar-* tools when the
live binary/index are missing. Also returned under :error to
keep the tool output shape consistent for structured callers.")

(defun %cultivar-tool-adapter ()
  "Return the cultivar adapter used by the tools.

Prefers `*cultivar-tool-adapter*` when set (tests override it),
then falls back to `*cultivar-adapter*`, then constructs and
installs a daemon-preferred adapter with default binary/index
paths."
  (or *cultivar-tool-adapter*
      *cultivar-adapter*
      (setf *cultivar-adapter*
            (make-cultivar-adapter :enabled-p t
                                   :daemon-mode :prefer
                                   :daemon-auto-start-p t))))

(defun %cultivar-tool-unavailable-result ()
  "Plist returned when the adapter is not usable. Callers rely
on :error being non-NIL to detect the degraded path."
  (list :error *cultivar-tool-unavailable-message*
        :message *cultivar-tool-unavailable-message*))

(defun %cultivar-tool-trim-refs (locations max-results)
  "Return the first MAX-RESULTS location plists from LOCATIONS.
Non-positive MAX-RESULTS returns the full list unchanged."
  (cond
    ((not (listp locations)) '())
    ((or (null max-results) (not (integerp max-results)) (not (plusp max-results)))
     locations)
    ((<= (length locations) max-results) locations)
    (t (subseq locations 0 max-results))))

(deftool cultivar-symbol-resolve
    ((file string :description "Absolute source file path to query." :required t)
     (line (integer 0) :description "0-indexed line number in FILE." :required t)
     (column (integer 0) :description "0-indexed column number in FILE." :required t))
  "Resolve the symbol at FILE:LINE:COLUMN via the Cultivar index."
  (:permission :auto)
  (:dangerous nil)
  (:category :code-intel)
  (:timeout 20)
  (let ((adapter (%cultivar-tool-adapter)))
    (if (not (%cultivar-usable-p adapter))
        (%cultivar-tool-unavailable-result)
        (let ((plist (cultivar-resolve adapter
                                       (list :file file
                                             :line line
                                             :col column))))
          (list :file file
                :line line
                :column column
                :results-digest (getf plist :results-digest)
                :symbol-id (getf plist :symbol-id)
                :kind (getf plist :kind)
                :qualified-name (getf plist :qualified-name)
                :symbol-file (getf plist :file)
                :symbol-line (getf plist :line)
                :symbol-col (getf plist :col)
                :confidence (getf plist :confidence)
                :reference-mode (getf plist :reference-mode)
                :results (getf plist :results))))))

(deftool cultivar-location-slice
    ((file string :description "Absolute source file path to query." :required t)
     (line (integer 0) :description "0-indexed line number in FILE." :required t)
     (column (integer 0) :description "0-indexed column number in FILE." :required t))
  "Resolve FILE:LINE:COLUMN and return Cultivar's canonical JSON slice.

Machine-oriented callers should prefer this over chaining
`cultivar-symbol-resolve` and `cultivar-span-preview`, because it
keeps the retrieval in Cultivar's canonical daemon-friendly JSON
shape and preserves resolve provenance in one packet."
  (:permission :auto)
  (:dangerous nil)
  (:category :code-intel)
  (:timeout 20)
  (let ((adapter (%cultivar-tool-adapter)))
    (if (not (%cultivar-usable-p adapter))
        (%cultivar-tool-unavailable-result)
        (let ((plist (cultivar-location-slice
                      adapter
                      (list :file file
                            :line line
                            :col column))))
          (list :file file
                :line line
                :column column
                :resolved-p (getf plist :resolved-p)
                :resolution (getf plist :resolution)
                :symbol-id (getf plist :symbol-id)
                :results-digest (getf plist :results-digest)
                :served-from-materialization
                (getf plist :served-from-materialization)
                :materialization-kind
                (getf plist :materialization-kind)
                :symbol (getf plist :symbol)
                :definition-source (getf plist :definition-source)
                :callers (getf plist :callers)
                :callees (getf plist :callees)
                :truncation (getf plist :truncation)
                :quality (getf plist :quality)
                :notes (getf plist :notes))))))

(deftool cultivar-symbol-references
    ((symbol-id string :description "Cultivar symbol-id to expand." :required t)
     (max-results (or null integer)
                  :description "Cap on returned locations (default 50)."
                  :default 50))
  "Return references for SYMBOL-ID via the Cultivar index."
  (:permission :auto)
  (:dangerous nil)
  (:category :code-intel)
  (:timeout 20)
  (let ((adapter (%cultivar-tool-adapter)))
    (if (not (%cultivar-usable-p adapter))
        (%cultivar-tool-unavailable-result)
        (let* ((plist (cultivar-expand adapter symbol-id))
               (all (getf plist :content))
               (trimmed (%cultivar-tool-trim-refs all max-results)))
          (list :symbol-id symbol-id
                :results-digest (getf plist :results-digest)
                :locations trimmed
                :total (if (listp all) (length all) 0)
                :returned (if (listp trimmed) (length trimmed) 0)
                :must-refs (getf plist :must-refs)
                :may-refs (getf plist :may-refs)
                :unknown-refs (getf plist :unknown-refs)
                :reference-mode (getf plist :reference-mode))))))

(deftool cultivar-symbol-slice
    ((symbol-id string
                :description "Cultivar symbol-id to assemble into the canonical JSON slice."
                :required t))
  "Return Cultivar's canonical daemon-friendly JSON slice for SYMBOL-ID.

This tool intentionally keeps the canonical shape (`context=2`,
`budget=8000`) so Cultivar can reuse daemon neighborhood
materializations when they are warm."
  (:permission :auto)
  (:dangerous nil)
  (:category :code-intel)
  (:timeout 20)
  (let ((adapter (%cultivar-tool-adapter)))
    (if (not (%cultivar-usable-p adapter))
        (%cultivar-tool-unavailable-result)
        (let ((plist (cultivar-slice adapter symbol-id)))
          (list :symbol-id (getf plist :symbol-id)
                :results-digest (getf plist :results-digest)
                :served-from-materialization
                (getf plist :served-from-materialization)
                :materialization-kind
                (getf plist :materialization-kind)
                :symbol (getf plist :symbol)
                :definition-source (getf plist :definition-source)
                :callers (getf plist :callers)
                :callees (getf plist :callees)
                :truncation (getf plist :truncation)
                :quality (getf plist :quality)
                :notes (getf plist :notes))))))

(deftool cultivar-span-preview
    ((symbol-id string :description "Cultivar symbol-id to preview." :required t)
     (context-lines (or null integer)
                    :description "Context lines around the definition (default 8)."
                    :default 8))
  "Return the human-readable markdown source slice for SYMBOL-ID.

Use this when a person wants to read the source excerpt directly.
Machine callers should prefer `cultivar-symbol-slice` so the
retrieval stays in Cultivar's canonical JSON shape."
  (:permission :auto)
  (:dangerous nil)
  (:category :code-intel)
  (:timeout 20)
  (let ((adapter (%cultivar-tool-adapter)))
    (if (not (%cultivar-usable-p adapter))
        (%cultivar-tool-unavailable-result)
        (let ((plist (cultivar-preview adapter symbol-id
                                       :context (or context-lines 8))))
          (list :symbol-id symbol-id
                :context-lines (or context-lines 8)
                :preview (getf plist :preview))))))
