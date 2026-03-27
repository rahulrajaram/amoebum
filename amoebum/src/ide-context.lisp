(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; NXT-092: IDE context ingestion module
;;; ---------------------------------------------------------------------------
;;; Captures IDE state: active selections, diagnostics, and open file lists.
;;; Provides a global *ide-context* variable that can be updated by IDE
;;; integrations and consumed by prompt assembly (NXT-093).
;;; ---------------------------------------------------------------------------

(defstruct (ide-context
            (:constructor %make-ide-context
                (&key
                   (selections '())
                   (diagnostics '())
                   (open-files '())
                   (active-file nil)
                   (timestamp (get-universal-time)))))
  "Captures the current IDE state for context injection into prompts.

  SELECTIONS   -- list of plists, each with :file :start-line :end-line :text
  DIAGNOSTICS  -- list of plists, each with :file :line :severity :message
  OPEN-FILES   -- list of file path strings currently open in the IDE
  ACTIVE-FILE  -- string path of the currently focused file, or NIL
  TIMESTAMP    -- universal-time when this context was last updated"
  (selections  '()  :type list)
  (diagnostics '()  :type list)
  (open-files  '()  :type list)
  active-file
  (timestamp   0    :type integer))

(defparameter *ide-context* nil
  "Global IDE context object, or NIL when no IDE is connected.
Updated by IDE integrations via UPDATE-IDE-CONTEXT!.")

;;; ---------------------------------------------------------------------------
;;; Constructor
;;; ---------------------------------------------------------------------------

(defun make-ide-context (&key
                           (selections '())
                           (diagnostics '())
                           (open-files '())
                           (active-file nil)
                           (timestamp (get-universal-time)))
  "Create and return a fresh IDE-CONTEXT.

  SELECTIONS   -- list of plists (:file STRING :start-line INT :end-line INT :text STRING)
  DIAGNOSTICS  -- list of plists (:file STRING :line INT :severity KEYWORD :message STRING)
  OPEN-FILES   -- list of file path strings
  ACTIVE-FILE  -- string path of the focused file, or NIL
  TIMESTAMP    -- universal-time (defaults to now)"
  (%make-ide-context :selections  selections
                     :diagnostics diagnostics
                     :open-files  open-files
                     :active-file active-file
                     :timestamp   timestamp))

;;; ---------------------------------------------------------------------------
;;; Updater
;;; ---------------------------------------------------------------------------

(defun %ide-context-normalize-selections (raw)
  "Return RAW if it is already a list of plists, else NIL."
  (if (listp raw) raw '()))

(defun %ide-context-normalize-diagnostics (raw)
  (if (listp raw) raw '()))

(defun %ide-context-normalize-open-files (raw)
  (if (listp raw)
      (remove-if-not #'stringp raw)
      '()))

(defun %ide-context-normalize-active-file (raw)
  (if (and (stringp raw) (plusp (length raw)))
      raw
      nil))

(defun update-ide-context! (data)
  "Update *IDE-CONTEXT* from an incoming IDE data plist DATA.

  Recognised keys in DATA:
    :selections   -- list of selection plists
    :diagnostics  -- list of diagnostic plists
    :open-files   -- list of file path strings
    :active-file  -- string or NIL

  Missing keys leave the corresponding field unchanged (if *IDE-CONTEXT*
  already exists) or default to empty / NIL for a fresh context.

  NXT-095: emits :ide-context-attached on first attach.

  Returns the updated *IDE-CONTEXT* object."
  (unless (listp data)
    (setf data '()))
  (let* ((existing *ide-context*)
         (first-attach-p (null existing))
         (ctx (or existing (make-ide-context))))
    (when (getf data :selections)
      (setf (ide-context-selections ctx)
            (%ide-context-normalize-selections (getf data :selections))))
    (when (member :diagnostics data)
      (setf (ide-context-diagnostics ctx)
            (%ide-context-normalize-diagnostics (getf data :diagnostics))))
    (when (member :open-files data)
      (setf (ide-context-open-files ctx)
            (%ide-context-normalize-open-files (getf data :open-files))))
    (when (member :active-file data)
      (setf (ide-context-active-file ctx)
            (%ide-context-normalize-active-file (getf data :active-file))))
    (setf (ide-context-timestamp ctx) (get-universal-time))
    (setf *ide-context* ctx)
    ;; NXT-095: emit :ide-context-attached on first connection
    (when first-attach-p
      (ignore-errors
        (publish (current-event-bus)
                 (make-ide-context-attached-event
                  :active-file (ide-context-active-file ctx)
                  :open-file-count (length (ide-context-open-files ctx))
                  :selection-count (length (ide-context-selections ctx))
                  :diagnostic-count (length (ide-context-diagnostics ctx))))))
    ctx))

;;; ---------------------------------------------------------------------------
;;; NXT-095: clear-ide-context! — drop IDE context and emit event
;;; ---------------------------------------------------------------------------

(defun clear-ide-context! ()
  "Set *IDE-CONTEXT* to NIL and emit an :ide-context-dropped event.

  Safe to call when *IDE-CONTEXT* is already NIL (no event is emitted).
  Returns the previously active context, or NIL."
  (let ((previous *ide-context*))
    (when (ide-context-p previous)
      (setf *ide-context* nil)
      (ignore-errors
        (publish (current-event-bus)
                 (make-ide-context-dropped-event
                  :active-file (ide-context-active-file previous)))))
    previous))

;;; ---------------------------------------------------------------------------
;;; Summary / token estimate
;;; ---------------------------------------------------------------------------

(defun ide-context-summary (&optional (ctx *ide-context*))
  "Return a human-readable summary string for CTX (defaults to *IDE-CONTEXT*).
Returns an empty string when CTX is NIL."
  (unless (ide-context-p ctx)
    (return-from ide-context-summary ""))
  (with-output-to-string (stream)
    (let ((open-count  (length (ide-context-open-files ctx)))
          (sel-count   (length (ide-context-selections ctx)))
          (diag-count  (length (ide-context-diagnostics ctx)))
          (active      (ide-context-active-file ctx)))
      (format stream "IDE context (updated ~D):~%"
              (ide-context-timestamp ctx))
      (format stream "  Active file: ~A~%"
              (or active "none"))
      (format stream "  Open files: ~D~%" open-count)
      (when (plusp sel-count)
        (format stream "  Selections (~D):~%" sel-count)
        (dolist (sel (ide-context-selections ctx))
          (format stream "    ~A lines ~A-~A~%"
                  (or (getf sel :file) "?")
                  (or (getf sel :start-line) "?")
                  (or (getf sel :end-line) "?"))))
      (when (plusp diag-count)
        (format stream "  Diagnostics (~D):~%" diag-count)
        (dolist (diag (ide-context-diagnostics ctx))
          (format stream "    [~A] ~A:~A ~A~%"
                  (or (getf diag :severity) :info)
                  (or (getf diag :file) "?")
                  (or (getf diag :line) "?")
                  (or (getf diag :message) "")))))))

(defun ide-context-token-estimate (&optional (ctx *ide-context*))
  "Estimate the number of tokens that IDE-CONTEXT-PROMPT-FRAGMENT will consume.
Returns 0 when CTX is NIL or empty."
  (unless (ide-context-p ctx)
    (return-from ide-context-token-estimate 0))
  (let ((total 0))
    ;; Fixed overhead for the section header (~15 tokens)
    (incf total 15)
    ;; Active file (~5 tokens)
    (when (ide-context-active-file ctx)
      (incf total (ceiling (length (ide-context-active-file ctx)) 4)))
    ;; Open file paths
    (dolist (path (ide-context-open-files ctx))
      (incf total (ceiling (max 1 (length path)) 4)))
    ;; Selections: header + text body
    (dolist (sel (ide-context-selections ctx))
      (let ((text (or (getf sel :text) "")))
        (incf total 5)
        (incf total (ceiling (max 1 (length text)) 4))))
    ;; Diagnostics: one line each ~10 tokens
    (incf total (* 10 (length (ide-context-diagnostics ctx))))
    total))

;;; ---------------------------------------------------------------------------
;;; NXT-093: Prompt fragment for injection
;;; ---------------------------------------------------------------------------

(defun ide-context-prompt-fragment (&optional (ctx *ide-context*))
  "Return a string suitable for injection into the LLM system prompt, or NIL.

  Returns NIL when CTX is NIL or carries no useful information.
  The returned string is a self-contained section that can be appended to
  the dynamic context built by SYSTEM-PROMPT-DYNAMIC-CONTEXT."
  (unless (ide-context-p ctx)
    (return-from ide-context-prompt-fragment nil))
  (let ((open-files  (ide-context-open-files ctx))
        (selections  (ide-context-selections ctx))
        (diagnostics (ide-context-diagnostics ctx))
        (active-file (ide-context-active-file ctx)))
    ;; Return NIL if there is nothing interesting to report.
    (when (and (null active-file)
               (null open-files)
               (null selections)
               (null diagnostics))
      (return-from ide-context-prompt-fragment nil))
    (with-output-to-string (stream)
      (format stream "IDE Context~%")
      (when active-file
        (format stream "- Active file: ~A~%" active-file))
      (when open-files
        (format stream "- Open files (~D):~%" (length open-files))
        (dolist (path open-files)
          (format stream "  * ~A~%" path)))
      (when selections
        (format stream "- Editor selections (~D):~%" (length selections))
        (dolist (sel selections)
          (let ((file  (or (getf sel :file) "unknown"))
                (start (or (getf sel :start-line) "?"))
                (end   (or (getf sel :end-line) "?"))
                (text  (getf sel :text)))
            (format stream "  ~A lines ~A-~A~%" file start end)
            (when (and (stringp text) (plusp (length text)))
              (format stream "  ```~%~A~%  ```~%" text)))))
      (when diagnostics
        (format stream "- Diagnostics (~D):~%" (length diagnostics))
        (dolist (diag diagnostics)
          (format stream "  [~A] ~A:~A ~A~%"
                  (string-upcase
                   (princ-to-string (or (getf diag :severity) :info)))
                  (or (getf diag :file) "unknown")
                  (or (getf diag :line) "?")
                  (or (getf diag :message) "")))))))

;;; ---------------------------------------------------------------------------
;;; NXT-095: Budget-aware prompt fragment with truncation
;;; ---------------------------------------------------------------------------

(defun ide-context-prompt-fragment/budget (token-budget &optional (ctx *ide-context*))
  "Like IDE-CONTEXT-PROMPT-FRAGMENT but enforces TOKEN-BUDGET.

  When IDE-CONTEXT-TOKEN-ESTIMATE exceeds TOKEN-BUDGET, content is truncated
  in priority order: diagnostics are dropped first, then selections, until the
  estimate fits within budget.  If the context still exceeds budget after
  dropping all diagnostics and selections, returns the fragment without those
  components.

  NXT-095: emits :ide-context-truncated when any content is dropped.

  Returns NIL when CTX is NIL or has no interesting content (same as
  IDE-CONTEXT-PROMPT-FRAGMENT)."
  (unless (ide-context-p ctx)
    (return-from ide-context-prompt-fragment/budget nil))
  (let ((estimate (ide-context-token-estimate ctx)))
    (if (<= estimate token-budget)
        ;; Budget not exceeded — use normal path
        (ide-context-prompt-fragment ctx)
        ;; Budget exceeded — build a truncated copy
        (let* ((selections  (copy-list (ide-context-selections ctx)))
               (diagnostics (copy-list (ide-context-diagnostics ctx)))
               (orig-sel-count  (length selections))
               (orig-diag-count (length diagnostics)))
          ;; Drop diagnostics first (lower priority)
          (loop while (and (plusp (length diagnostics))
                           (> (ide-context-token-estimate
                               (make-ide-context
                                :active-file (ide-context-active-file ctx)
                                :open-files  (ide-context-open-files ctx)
                                :selections  selections
                                :diagnostics diagnostics))
                              token-budget))
                do (pop diagnostics))
          ;; Drop selections next if still over budget
          (loop while (and (plusp (length selections))
                           (> (ide-context-token-estimate
                               (make-ide-context
                                :active-file (ide-context-active-file ctx)
                                :open-files  (ide-context-open-files ctx)
                                :selections  selections
                                :diagnostics diagnostics))
                              token-budget))
                do (pop selections))
          (let* ((truncated-ctx (make-ide-context
                                 :active-file (ide-context-active-file ctx)
                                 :open-files  (ide-context-open-files ctx)
                                 :selections  selections
                                 :diagnostics diagnostics))
                 (selections-dropped  (- orig-sel-count  (length selections)))
                 (diagnostics-dropped (- orig-diag-count (length diagnostics))))
            ;; NXT-095: emit truncation event
            (ignore-errors
              (publish (current-event-bus)
                       (make-ide-context-truncated-event
                        :token-estimate estimate
                        :token-budget   token-budget
                        :selections-dropped  selections-dropped
                        :diagnostics-dropped diagnostics-dropped)))
            (ide-context-prompt-fragment truncated-ctx))))))

;;; ---------------------------------------------------------------------------
;;; NXT-108: Mode-aware context packet builder
;;; ---------------------------------------------------------------------------

(defun %ide-packet-filter-diagnostics (diagnostics mode)
  "Return the diagnostics list filtered/truncated according to MODE.

:compact   — top 3 diagnostics only
:operator  — all diagnostics
:verbose   — all diagnostics"
  (ecase mode
    (:compact  (if (> (length diagnostics) 3)
                   (subseq diagnostics 0 3)
                   diagnostics))
    (:operator diagnostics)
    (:verbose  diagnostics)))

(defun %ide-packet-filter-selections (selections mode)
  "Return the selections list filtered/truncated according to MODE.

:compact   — no selections
:operator  — top 5 selections
:verbose   — all selections"
  (ecase mode
    (:compact  '())
    (:operator (if (> (length selections) 5)
                   (subseq selections 0 5)
                   selections))
    (:verbose  selections)))

(defun ide-context-build-packet (ctx &key (mode :verbose) (budget most-positive-fixnum))
  "Build a structured context packet from CTX, filtered by MODE and BUDGET.

Returns a plist with:
  :selections     — filtered/truncated selection list
  :diagnostics    — filtered/truncated diagnostic list
  :open-files     — list of open file paths (all modes)
  :active-file    — the currently focused file path, or NIL
  :mode           — the mode keyword used for this packet
  :token-estimate — token estimate after mode filtering and budget truncation

MODE controls content inclusion:
  :compact   — active-file + top 3 diagnostics only; no selections
  :operator  — active-file + all diagnostics + top 5 selections
  :verbose   — everything (default)

BUDGET is a positive integer upper bound on token-estimate.  When the
mode-filtered estimate exceeds BUDGET, diagnostics are dropped first,
then selections, until the estimate fits within budget.

CTX may be NIL; in that case an empty packet is returned."
  (unless (ide-context-p ctx)
    (return-from ide-context-build-packet
      (list :selections     '()
            :diagnostics    '()
            :open-files     '()
            :active-file    nil
            :mode           mode
            :token-estimate 0)))
  (let* ((raw-selections  (ide-context-selections  ctx))
         (raw-diagnostics (ide-context-diagnostics ctx))
         ;; Apply mode filtering first
         (sel  (%ide-packet-filter-selections  raw-selections  mode))
         (diag (%ide-packet-filter-diagnostics raw-diagnostics mode)))
    ;; Apply budget truncation: drop diagnostics first, then selections
    (loop while (and (plusp (length diag))
                     (> (ide-context-token-estimate
                         (make-ide-context
                          :active-file (ide-context-active-file ctx)
                          :open-files  (ide-context-open-files  ctx)
                          :selections  sel
                          :diagnostics diag))
                        budget))
          do (pop diag))
    (loop while (and (plusp (length sel))
                     (> (ide-context-token-estimate
                         (make-ide-context
                          :active-file (ide-context-active-file ctx)
                          :open-files  (ide-context-open-files  ctx)
                          :selections  sel
                          :diagnostics diag))
                        budget))
          do (pop sel))
    (let ((estimate (ide-context-token-estimate
                     (make-ide-context
                      :active-file (ide-context-active-file ctx)
                      :open-files  (ide-context-open-files  ctx)
                      :selections  sel
                      :diagnostics diag))))
      (list :selections     sel
            :diagnostics    diag
            :open-files     (ide-context-open-files  ctx)
            :active-file    (ide-context-active-file ctx)
            :mode           mode
            :token-estimate estimate))))

;;; ---------------------------------------------------------------------------
;;; Status-bar segment helper (NXT-093)
;;; ---------------------------------------------------------------------------

(defun %ide-segment-text (&optional (ctx *ide-context*))
  "Return the text for the :ide status-bar segment."
  (if (ide-context-p ctx)
      (let ((n (length (ide-context-open-files ctx))))
        (if (plusp n)
            (format nil "IDE: ~D file~:P" n)
            "IDE: on"))
      "IDE: off"))
