(in-package :amoebum)

;;; Plan-mode gating and permission-mode default-decision dispatch.
;;;
;;; Extracted mechanically from src/permissions.lisp for NXT-440. Owns:
;;;   * plan-mode tool-name allow/deny lists and the plan-mode predicates
;;;     the evaluation pipeline consults to short-circuit mutating tools
;;;     while permitting read-only ones,
;;;   * the shell-tool predicate plan-mode shares with auto-edit,
;;;   * approval-policy normalization and the approval-policy ->
;;;     permission-mode coercion that feeds %effective-permission-mode,
;;;   * the per-server MCP permission lookup helpers consulted before mode
;;;     defaults apply,
;;;   * the +permission-mode-defaults+ dispatch table and the
;;;     %mode-default-decision helper that closes the per-mode "what would
;;;     we do absent a rule" question (plan-mode is a key in the table).
;;;
;;; Block/readonly outcomes, reason strings, and decision-trace fields
;;; are preserved verbatim.

(defparameter *plan-mode-blocked-tool-names*
  '("write-file" "edit-file"))

(defparameter *plan-mode-readonly-allowed-tool-names*
  '("read-file" "glob-files" "grep-content" "search-project"))

(defun %shell-tool-p (tool command)
  (or command
      (member (%tool-name tool) *shell-tool-names* :test #'string=)))

(defun %normalize-approval-policy (value)
  (let ((normalized
          (cond
            ((keywordp value) value)
            ((stringp value)
             (intern (string-upcase
                      (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   (substitute #\- #\_ value)))
                     :keyword))
            ((symbolp value)
             (intern (string-upcase (symbol-name value)) :keyword))
            (t nil))))
    (case normalized
      (:UNTRUSTED :untrusted)
      (:ON-FAILURE :on-failure)
      (:ON_FAILURE :on-failure)
      (:ON-REQUEST :on-request)
      (:ON_REQUEST :on-request)
      (:NEVER :never)
      (otherwise nil))))

(defun %approval-policy->permission-mode (approval-policy)
  (case (%normalize-approval-policy approval-policy)
    (:untrusted :supervised)
    (:on-request :supervised)
    (:on-failure :auto-edit)
    (:never :yolo)
    (otherwise nil)))

(defun %configured-approval-policy ()
  (cfg :approval-policy))

(defun %effective-permission-mode (mode &optional approval-policy)
  (or
   (case mode
     ((:supervised :auto-edit :full-auto :yolo :plan) mode)
     (:no-confirm :yolo)
     (:untrusted :supervised)
     (:on-request :supervised)
     (:on-failure :auto-edit)
     (:never :yolo)
     (otherwise nil))
   (%approval-policy->permission-mode approval-policy)
   (let ((cfg-mode (ignore-errors (config-permission-mode (current-config)))))
     (or (case cfg-mode
           ((:supervised :auto-edit :full-auto :yolo :plan) cfg-mode)
           (:no-confirm :yolo)
           (:untrusted :supervised)
           (:on-request :supervised)
           (:on-failure :auto-edit)
           (:never :yolo)
           (otherwise nil))
         (%approval-policy->permission-mode (%configured-approval-policy))
         :supervised))))

(defun %mcp-tool-server-name (tool-name)
  (when (and tool-name
             (uiop:string-prefix-p "mcp/" tool-name))
    (let* ((rest (subseq tool-name (length "mcp/")))
           (separator (position #\/ rest)))
      (and separator
           (> separator 0)
           (subseq rest 0 separator)))))

(defun %normalize-mcp-permission-decision (value)
  (let ((normalized
          (cond
            ((keywordp value) value)
            ((stringp value)
             (intern (string-upcase
                      (string-trim '(#\Space #\Tab #\Newline #\Return) value))
                     :keyword))
            ((symbolp value)
             (intern (string-upcase (symbol-name value)) :keyword))
            (t nil))))
    (when (member normalized '(:allow :deny :prompt) :test #'eq)
      normalized)))

(defun %mcp-permission-key-kind (key server-name)
  (let ((normalized (%tool-name key)))
    (cond
      ((null normalized) nil)
      ((member normalized '("*" "default") :test #'string=) :default)
      ((uiop:string-prefix-p "mcp/" normalized)
       (let* ((rest (subseq normalized (length "mcp/")))
              (separator (position #\/ rest))
              (entry-server (if separator
                                (subseq rest 0 separator)
                                rest)))
         (if (string= entry-server server-name)
             :server
             nil)))
      ((string= normalized server-name) :server)
      (t nil))))

(defun %mcp-permission-config-pairs (value)
  (cond
    ((hash-table-p value)
     (loop for key being the hash-keys of value using (hash-value decision)
           collect (cons key decision)))
    ((and (listp value)
          (every #'consp value))
     value)
    ((and (listp value)
          (evenp (length value)))
     (loop for (key decision) on value by #'cddr
           collect (cons key decision)))
    (t nil)))

(defun %mcp-server-config-decision (server-name)
  (let ((pairs (%mcp-permission-config-pairs
                (cfg :mcp-server-permissions)))
        (default nil))
    (dolist (entry pairs)
      (let* ((kind (%mcp-permission-key-kind (car entry) server-name))
             (decision (%normalize-mcp-permission-decision (cdr entry))))
        (when decision
          (case kind
            (:server (return-from %mcp-server-config-decision decision))
            (:default (unless default
                        (setf default decision)))))))
    default))

;;; --- Permission Mode Default Decision Table (FP-Refine Phase 2, Target 4) ---

(defparameter +permission-mode-defaults+
  '((:plan       . :prompt)
    (:supervised . :prompt)
    (:full-auto  . :allow)
    (:yolo       . :allow))
  "Maps permission modes to their default decisions.
:auto-edit has special logic via %auto-edit-default-decision.")

(defun %auto-edit-default-decision (tool path command)
  "Compute the default decision for :auto-edit mode.
Shell tools -> :prompt; file tools or path present -> :allow; otherwise :prompt."
  (cond
    ((%shell-tool-p tool command) :prompt)
    ((or path
         (member (%tool-name tool) *auto-edit-tool-names* :test #'string=))
     :allow)
    (t :prompt)))

(defun %mode-default-decision (mode tool path command)
  (if (eq mode :auto-edit)
      (%auto-edit-default-decision tool path command)
      (let ((entry (assoc mode +permission-mode-defaults+)))
        (if entry
            (cdr entry)
            :prompt))))

(defun %plan-mode-enabled-p ()
  (not (null (cfg :plan-mode))))

(defun plan-mode-mutating-tools-blocked-p (&optional
                                             (config (ignore-errors (current-config)))
                                             plan-mode-enabled-override)
  (let ((plan-mode-enabled-p
          (if (null plan-mode-enabled-override)
              (and (config-p config)
                   (not (null (config-value :plan-mode config))))
              (not (null plan-mode-enabled-override)))))
    (and plan-mode-enabled-p
         (not (null *plan-mode-blocked-tool-names*))
         (not (null *shell-tool-names*)))))

(defun %plan-mode-blocked-p (tool command &optional plan-mode-enabled-override)
  (let ((tool-name (%tool-name tool)))
    (and (plan-mode-mutating-tools-blocked-p (ignore-errors (current-config))
                                             plan-mode-enabled-override)
         (or (%shell-tool-p tool command)
             (member tool-name *plan-mode-blocked-tool-names* :test #'string=)))))

(defun %plan-mode-readonly-allowed-p (tool)
  (let ((tool-name (%tool-name tool)))
    (and (%plan-mode-enabled-p)
         (member tool-name
                 *plan-mode-readonly-allowed-tool-names*
                 :test #'string=))))

(defun %plan-mode-actionable-reason ()
  "Plan mode is read-only. Review the captured plan, approve allowed steps with /plan approve, then run /execute to re-enable mutating tools.")

(defun %plan-mode-block-reason (tool-name command)
  (let ((normalized-tool (or tool-name "unknown-tool")))
    (if (and (stringp command) (> (length command) 0))
        (format nil "Plan mode blocked mutating tool ~A for command ~S."
                normalized-tool
                command)
        (format nil "Plan mode blocked mutating tool ~A."
                normalized-tool))))
