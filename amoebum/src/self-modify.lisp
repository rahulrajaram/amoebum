(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; Self-Modifying Agent (I98 + I254 + I256)
;;;
;;; Allows runtime code modifications with safety controls:
;;; - sandboxed-eval: restricted sandbox + timeout
;;; - Approval gate: modifications require human approval
;;; - Approval widget model + approve/deny/edit-then-approve workflow
;;; - Auto-approve rules for safe whitelisted patterns
;;; - Persistent audit trail in ~/.amoebum/audit/self-modifications.sexp
;;; - Modification journal: tracks all applied changes
;;; - Rollback: fmakunbound/remove-method to undo changes
;;; - Resource limits: timeout + cons-cell guardrails
;;; ---------------------------------------------------------------------------

;;; --- Event Types ---

(defparameter +event-type-self-modify-proposed+ "self-modify:proposed")
(defparameter +event-type-self-modify-approved+ "self-modify:approved")
(defparameter +event-type-self-modify-applied+ "self-modify:applied")
(defparameter +event-type-self-modify-rolled-back+ "self-modify:rolled-back")

(define-condition self-modify-unsafe-form (error)
  ((symbol-name :initarg :symbol-name
                :reader self-modify-unsafe-form-symbol-name)
   (reason :initarg :reason
           :initform "unsafe operation blocked in self-modification form"
           :reader self-modify-unsafe-form-reason))
  (:report (lambda (condition stream)
             (format stream "~A: ~A"
                     (self-modify-unsafe-form-reason condition)
                     (self-modify-unsafe-form-symbol-name condition)))))

;;; --- Modification Journal ---

(defstruct (modification-entry
            (:constructor make-modification-entry
                (&key id form-text result status
                      (timestamp (get-universal-time))
                      (agent-id "local")
                      applied-symbols previous-definitions
                      (approval-mode :pending)
                      error-message)))
  (id "" :type string)
  (form-text "" :type string)
  (result nil)
  (status :proposed :type keyword)
  (timestamp (get-universal-time) :type integer)
  (agent-id "local" :type string)
  (applied-symbols '() :type list)
  (previous-definitions '() :type list)
  (approval-mode :pending :type keyword)
  (error-message nil :type (or null string)))

(defvar *modification-journal* '()
  "Chronological journal of all self-modification attempts.")

(defvar *modification-counter* 0
  "Counter for generating unique modification IDs.")

(defparameter *self-modify-auto-approve-prefixes*
  '("(defun "
    "(defmacro "
    "(defparameter "
    "(defvar "
    "(defconstant ")
  "Prefix allowlist that can auto-approve self-modification proposals.")

(defparameter *self-modify-forbidden-symbol-names*
  '("OPEN"
    "WITH-OPEN-FILE"
    "WITH-OPEN-STREAM"
    "LOAD"
    "REQUIRE"
    "COMPILE"
    "COMPILE-FILE"
    "EVAL"
    "INTERN"
    "EXPORT"
    "IMPORT"
    "USE-PACKAGE"
    "UNUSE-PACKAGE"
    "SHADOW"
    "SHADOWING-IMPORT"
    "MAKE-PACKAGE"
    "DELETE-PACKAGE"
    "RENAME-PACKAGE"
    "RUN-PROGRAM"
    "LAUNCH-PROGRAM"
    "DELETE-FILE"
    "RENAME-FILE"
    "LOAD-FOREIGN-LIBRARY"
    "LOAD-SHARED-OBJECT")
  "Denylist of operation symbols blocked in self-modification forms.")

(defparameter *self-modify-forbidden-package-names*
  '("UIOP"
    "ASDF"
    "SB-EXT"
    "SB-SYS"
    "SB-ALIEN"
    "SB-POSIX"
    "CFFI"
    "CFFI-SYS")
  "Denylist of package qualifiers blocked in self-modification forms.")

(defparameter *self-modification-audit-path-override* nil
  "When non-NIL, overrides the self-modification audit trail path.")

(defparameter *self-modification-agent-id-override* nil
  "When non-NIL, override the agent ID recorded in self-modification entries.")

(defparameter *self-modify-max-eval-seconds* 5
  "Default max evaluation timeout for self-modification forms.")

(defparameter *self-modify-max-cons-cells* 200000
  "Default soft cap on estimated cons cells allocated by a self-modification form.")

(defparameter +self-modify-estimated-cons-cell-bytes+ 16
  "Estimated size of one cons cell in bytes on 64-bit SBCL runtimes.")

(defun %next-modification-id ()
  "Generate a unique modification ID."
  (format nil "mod-~A-~A" (incf *modification-counter*) (get-universal-time)))

(defun self-modification-audit-path ()
  "Return the self-modification audit trail path."
  (or *self-modification-audit-path-override*
      (merge-pathnames #P".amoebum/audit/self-modifications.sexp"
                       (user-homedir-pathname))))

(defun %self-modification-agent-id (&optional explicit-agent-id)
  (let* ((candidate (or explicit-agent-id
                        *self-modification-agent-id-override*
                        *current-session-id*
                        (uiop:getenv "AMOEBUM_AGENT_ID")
                        "local"))
         (text (string-trim '(#\Space #\Tab #\Newline #\Return)
                            (typecase candidate
                              (string candidate)
                              (symbol (symbol-name candidate))
                              (t (princ-to-string candidate))))))
    (if (plusp (length text))
        text
        "local")))

(defun %resolve-positive-resource-limit (value fallback)
  (let ((candidate (or value fallback)))
    (cond
      ((null candidate) nil)
      ((and (integerp candidate) (plusp candidate))
       candidate)
      ((and (realp candidate) (> candidate 0))
       (ceiling candidate))
      (t
       nil))))

(defun %trim-whitespace (text)
  (string-trim '(#\Space #\Tab #\Newline #\Return) (or text "")))

(defun %string-prefix-ci-p (prefix text)
  (let ((prefix-length (length prefix))
        (text-length (length text)))
    (and (<= prefix-length text-length)
         (string-equal prefix text :end2 prefix-length))))

(defun %safe-object-string (value)
  (handler-case
      (let ((*print-length* 20)
            (*print-level* 8)
            (*print-pretty* nil))
        (princ-to-string value))
    (error ()
      "#<unprintable>")))

(defun %auto-approve-form-text-p (form-text)
  (let ((trimmed (string-downcase (%trim-whitespace form-text))))
    (loop for prefix in *self-modify-auto-approve-prefixes*
          thereis (%string-prefix-ci-p (string-downcase prefix) trimmed))))

(defun %append-self-modification-audit (entry action &key note)
  (handler-case
      (let ((path (self-modification-audit-path)))
        (ensure-directories-exist path)
        (with-open-file (stream path
                                :direction :output
                                :if-exists :append
                                :if-does-not-exist :create
                                :external-format :utf-8)
          (prin1 (list :timestamp (get-universal-time)
                       :action action
                       :id (modification-entry-id entry)
                       :agent-id (modification-entry-agent-id entry)
                       :status (modification-entry-status entry)
                       :approval-mode (modification-entry-approval-mode entry)
                       :code (modification-entry-form-text entry)
                       :form-text (modification-entry-form-text entry)
                       :result (%safe-object-string
                                (modification-entry-result entry))
                       :error-message
                       (and (modification-entry-error-message entry)
                            (%safe-object-string
                             (modification-entry-error-message entry)))
                       :applied-symbols
                       (mapcar #'%safe-object-string
                               (modification-entry-applied-symbols entry))
                       :note (and note (%safe-object-string note)))
                 stream)
          (terpri stream)))
    (error ()
      nil))
  entry)

(defun modification-journal ()
  "Return the modification journal."
  *modification-journal*)

(defun clear-modification-journal ()
  "Clear the modification journal."
  (setf *modification-journal* '()
        *modification-counter* 0))

(defun find-modification (id)
  "Find a modification entry by ID."
  (find id *modification-journal* :key #'modification-entry-id :test #'string=))

;;; --- Sandboxed Eval ---

(defun %collect-defined-symbols (form)
  "Extract symbol names that would be defined by FORM.
   Handles defun, defmethod, defparameter, defvar, defclass, defstruct, defmacro."
  (when (and (consp form) (symbolp (car form)))
    (let ((op (car form)))
      (cond
        ((member op '(defun defmacro defparameter defvar defconstant defclass defstruct
                      defgeneric))
         (when (and (cdr form) (symbolp (cadr form)))
           (list (cadr form))))
        ((eq op 'defmethod)
         (when (and (cdr form) (symbolp (cadr form)))
           (list (cadr form))))
        ((eq op 'progn)
         (mapcan #'%collect-defined-symbols (cdr form)))
        (t '())))))

(defun %capture-previous-definition (symbol)
  "Capture the current definition of SYMBOL for rollback."
  (list :symbol symbol
        :fboundp (fboundp symbol)
        :function (when (fboundp symbol)
                    (handler-case (symbol-function symbol) (error () nil)))
        :boundp (boundp symbol)
        :value (when (boundp symbol)
                 (handler-case (symbol-value symbol) (error () nil)))))

(defun %unsafe-symbol-p (symbol)
  (and (symbolp symbol)
       (not (keywordp symbol))
       (let* ((name (string-upcase (symbol-name symbol)))
              (pkg (symbol-package symbol))
              (pkg-name (and pkg (string-upcase (package-name pkg)))))
         (or (member name *self-modify-forbidden-symbol-names* :test #'string=)
             (and pkg-name
                  (member pkg-name
                          *self-modify-forbidden-package-names*
                          :test #'string=))))))

(defun %find-unsafe-symbol (form)
  (labels ((walk (node)
             (cond
               ((symbolp node)
                (and (%unsafe-symbol-p node) node))
               ((consp node)
                (if (eq (car node) 'quote)
                    nil
                    (or (walk (car node))
                        (loop for item in (cdr node)
                              thereis (walk item)))))
               ((vectorp node)
                (loop for item across node
                      thereis (walk item)))
               (t
                nil))))
    (walk form)))

(defun %parse-self-modification-form (form-text package)
  (let* ((source (or form-text ""))
         (resolved-package (or (find-package package)
                               (find-package :amoebum.sandbox))))
    (let ((*package* resolved-package))
      (multiple-value-bind (form position)
          (sandbox-read-from-string source :eof-error-p t)
        (let ((remainder (%trim-whitespace
                          (if (<= position (length source))
                              (subseq source position)
                              ""))))
          (when (plusp (length remainder))
            (error "Self-modification expects exactly one Lisp form."))
          form)))))

(defun %validate-self-modification-form (form)
  (let ((unsafe-symbol (%find-unsafe-symbol form)))
    (when unsafe-symbol
      (error 'self-modify-unsafe-form
             :reason "unsafe operation blocked in self-modification form"
             :symbol-name
             (if (and (symbol-package unsafe-symbol)
                      (package-name (symbol-package unsafe-symbol)))
                 (format nil "~A::~A"
                         (package-name (symbol-package unsafe-symbol))
                         (symbol-name unsafe-symbol))
                 (symbol-name unsafe-symbol))))))

(defun %estimated-cons-cells (bytes)
  (ceiling (max 0 bytes) +self-modify-estimated-cons-cell-bytes+))

(defun sandboxed-eval (form-text
                       &key timeout-seconds
                         max-cons-cells
                         (package :amoebum.sandbox))
  "Evaluate FORM-TEXT in a restricted sandbox with timeout.
Returns (values result errorp error-message)."
  (handler-case
      (let* ((form (%parse-self-modification-form form-text package))
             (resolved-timeout
               (or (%resolve-positive-resource-limit
                    timeout-seconds
                    *self-modify-max-eval-seconds*)
                   5))
             (resolved-max-cons-cells
               (%resolve-positive-resource-limit
                max-cons-cells
                *self-modify-max-cons-cells*))
             (result nil)
             (errorp nil)
             (error-msg nil)
             #+sbcl
             (consed-before (sb-ext:get-bytes-consed))
             #+sbcl
             (consed-after nil))
        (%validate-self-modification-form form)
        #+sbcl
        (handler-case
            (sb-ext:with-timeout resolved-timeout
              (setf result (eval form)))
          (sb-ext:timeout ()
            (setf errorp t
                  error-msg
                  (format nil
                          "Evaluation timed out after ~As (limit ~D seconds)."
                          resolved-timeout
                          resolved-timeout)))
          (error (c)
            (setf errorp t
                  error-msg (format nil "Evaluation error: ~A" c))))
        #-sbcl
        (handler-case
            (setf result (eval form))
          (error (c)
            (setf errorp t
                  error-msg (format nil "Evaluation error: ~A" c))))
        #+sbcl
        (when (not errorp)
          (setf consed-after (sb-ext:get-bytes-consed))
          (when resolved-max-cons-cells
            (let* ((delta-bytes (max 0 (- consed-after consed-before)))
                   (cons-cells (%estimated-cons-cells delta-bytes)))
              (when (> cons-cells resolved-max-cons-cells)
                (setf result nil
                      errorp t
                      error-msg
                      (format nil
                              "Evaluation exceeded cons-cell limit (~D > ~D estimated cons cells)."
                              cons-cells
                              resolved-max-cons-cells))))))
        (values result errorp error-msg))
    (error (c)
      (values nil t (format nil "Parse error: ~A" c)))))

;;; --- Approval Gate ---

(defvar *self-modify-auto-approve-p* nil
  "When T, skip approval gate (for testing only).")

(defun propose-modification (form-text &key (event-bus (current-event-bus)) agent-id)
  "Propose a code modification. Returns a modification-entry in :proposed status."
  (let* ((id (%next-modification-id))
         (entry (make-modification-entry :id id
                                         :form-text form-text
                                         :agent-id (%self-modification-agent-id agent-id)
                                         :status :proposed
                                         :approval-mode :pending)))
    (push entry *modification-journal*)
    (publish event-bus
             (make-event :type +event-type-self-modify-proposed+
                         :source "self-modify"
                         :payload (list :id id :form form-text)))
    (%append-self-modification-audit entry :proposed)
    (when (%auto-approve-form-text-p form-text)
      (setf (modification-entry-status entry) :approved
            (modification-entry-approval-mode entry) :auto)
      (publish event-bus
               (make-event :type +event-type-self-modify-approved+
                           :source "self-modify"
                           :payload (list :id id :auto-approved t)))
      (%append-self-modification-audit entry :auto-approved
                                       :note "approved by prefix allowlist"))
    entry))

(defun approve-modification (id &key (event-bus (current-event-bus)))
  "Approve a proposed modification by ID."
  (let ((entry (find-modification id)))
    (unless entry
      (error "Modification ~A not found." id))
    (unless (member (modification-entry-status entry) '(:proposed :approved) :test #'eq)
      (error "Modification ~A is not in approvable state (current: ~A)."
             id (modification-entry-status entry)))
    (setf (modification-entry-status entry) :approved)
    (setf (modification-entry-approval-mode entry) :manual)
    (publish event-bus
             (make-event :type +event-type-self-modify-approved+
                         :source "self-modify"
                         :payload (list :id id)))
    (%append-self-modification-audit entry :approved)
    entry))

(defun deny-modification (id &key reason)
  "Deny a proposed modification by ID."
  (let ((entry (find-modification id)))
    (unless entry
      (error "Modification ~A not found." id))
    (when (eq :applied (modification-entry-status entry))
      (error "Modification ~A is already applied; rollback instead." id))
    (setf (modification-entry-status entry) :denied
          (modification-entry-approval-mode entry) :manual
          (modification-entry-error-message entry)
          (or reason "Denied by user decision"))
    (%append-self-modification-audit entry :denied)
    entry))

(defun edit-modification (id new-form-text &key approve-p (event-bus (current-event-bus)))
  "Edit a proposed modification. Optionally approve after editing."
  (let ((entry (find-modification id)))
    (unless entry
      (error "Modification ~A not found." id))
    (when (eq :applied (modification-entry-status entry))
      (error "Modification ~A is already applied and cannot be edited." id))
    (let ((trimmed (%trim-whitespace new-form-text))
          (previous-form (modification-entry-form-text entry)))
      (when (zerop (length trimmed))
        (error "Edited self-modification form cannot be blank."))
      (setf (modification-entry-form-text entry) trimmed
            (modification-entry-result entry) nil
            (modification-entry-error-message entry) nil
            (modification-entry-applied-symbols entry) '()
            (modification-entry-previous-definitions entry) '()
            (modification-entry-status entry) (if approve-p :approved :proposed)
            (modification-entry-approval-mode entry)
            (if approve-p :manual :pending))
      (%append-self-modification-audit
       entry
       (if approve-p :edit-then-approve :edited)
       :note (list :previous-form previous-form))
      (when approve-p
        (publish event-bus
                 (make-event :type +event-type-self-modify-approved+
                             :source "self-modify"
                             :payload (list :id id :edited t))))
      entry)))

(defun render-modification-approval-widget (entry)
  "Render a textual approval widget for a self-modification proposal."
  (with-output-to-string (out)
    (format out "Self-Modification Approval~%")
    (format out "  ID: ~A~%" (modification-entry-id entry))
    (format out "  Status: ~A~%" (modification-entry-status entry))
    (format out "  Approval mode: ~A~%" (modification-entry-approval-mode entry))
    (format out "  Code:~%~A~%~%"
            (modification-entry-form-text entry))
    (format out "Actions:~%")
    (format out "  /self-modify approve ~A~%" (modification-entry-id entry))
    (format out "  /self-modify deny ~A~%" (modification-entry-id entry))
    (format out "  /self-modify edit-approve ~A <lisp-form>~%"
            (modification-entry-id entry))))

(defun %read-form-for-symbol-collection (form-text)
  (handler-case
      (%parse-self-modification-form form-text :amoebum)
    (error ()
      nil)))

;;; --- Apply Modification ---

(defun apply-modification (id
                           &key (event-bus (current-event-bus))
                             timeout-seconds
                             max-cons-cells)
  "Apply an approved modification. Returns the modification-entry."
  (let ((entry (find-modification id)))
    (unless entry
      (error "Modification ~A not found." id))
    (unless (or (eq :approved (modification-entry-status entry))
                *self-modify-auto-approve-p*)
      (error "Modification ~A is not approved (current: ~A)."
             id (modification-entry-status entry)))
    ;; Capture previous definitions before applying
    (let* ((form (%read-form-for-symbol-collection
                  (modification-entry-form-text entry)))
           (symbols (%collect-defined-symbols form))
           (prev-defs (mapcar #'%capture-previous-definition symbols)))
      (setf (modification-entry-applied-symbols entry) symbols
            (modification-entry-previous-definitions entry) prev-defs)
      ;; Evaluate in the full amoebum package (not sandbox) for actual modification.
      ;; Safety validation still runs inside SANDBOXED-EVAL.
      (multiple-value-bind (result errorp error-msg)
          (sandboxed-eval (modification-entry-form-text entry)
                          :timeout-seconds timeout-seconds
                          :max-cons-cells max-cons-cells
                          :package :amoebum)
        (cond
          (errorp
           (setf (modification-entry-status entry) :failed
                 (modification-entry-error-message entry) error-msg)
           (%append-self-modification-audit entry :apply-failed)
           entry)
          (t
           (setf (modification-entry-status entry) :applied
                 (modification-entry-result entry) result)
           (publish event-bus
                    (make-event :type +event-type-self-modify-applied+
                                :source "self-modify"
                                :payload (list :id id :symbols symbols)))
           (%append-self-modification-audit entry :applied)
           entry))))))

;;; --- Rollback ---

(defun rollback-modification (id &key (event-bus (current-event-bus)))
  "Rollback an applied modification, restoring previous definitions."
  (let ((entry (find-modification id)))
    (unless entry
      (error "Modification ~A not found." id))
    (unless (eq :applied (modification-entry-status entry))
      (error "Modification ~A is not in applied state." id))
    (dolist (prev-def (modification-entry-previous-definitions entry))
      (let ((symbol (getf prev-def :symbol)))
        (when symbol
          (if (getf prev-def :fboundp)
              (when (getf prev-def :function)
                (setf (symbol-function symbol) (getf prev-def :function)))
              (when (fboundp symbol)
                (fmakunbound symbol)))
          (if (getf prev-def :boundp)
              (setf (symbol-value symbol) (getf prev-def :value))
              (when (boundp symbol)
                (makunbound symbol))))))
    (setf (modification-entry-status entry) :rolled-back)
    (publish event-bus
             (make-event :type +event-type-self-modify-rolled-back+
                         :source "self-modify"
                         :payload (list :id id)))
    (%append-self-modification-audit entry :rolled-back)
    entry))

;;; --- Journal Queries ---

(defun pending-modifications ()
  "Return all modifications in :proposed status."
  (remove-if-not (lambda (e) (eq :proposed (modification-entry-status e)))
                 *modification-journal*))

(defun applied-modifications ()
  "Return all modifications in :applied status."
  (remove-if-not (lambda (e) (eq :applied (modification-entry-status e)))
                 *modification-journal*))

(defun undo-last-modification (&key (event-bus (current-event-bus)))
  "Rollback the most recently applied self-modification."
  (let ((entry (find-if (lambda (candidate)
                          (eq :applied (modification-entry-status candidate)))
                        *modification-journal*)))
    (unless entry
      (error "No applied self-modification is available to undo."))
    (rollback-modification (modification-entry-id entry) :event-bus event-bus)))

(defun %entry-matches-status-p (entry status)
  (cond
    ((null status) t)
    ((listp status)
     (member (modification-entry-status entry) status :test #'eq))
    (t
     (eq (modification-entry-status entry) status))))

(defun %entry-diff-summary (entry)
  (let ((symbols (modification-entry-applied-symbols entry)))
    (if (null symbols)
        "No symbol-level diff available."
        (with-output-to-string (out)
          (format out "Updated symbols: ")
          (loop for symbol in symbols
                for index from 0 do
                  (when (> index 0)
                    (format out ", "))
                  (format out "~A" symbol))))))

(defun list-modifications (&key status limit (include-diff-p t))
  "Return modification history as a list of plists (newest first)."
  (let* ((filtered (remove-if-not
                    (lambda (entry) (%entry-matches-status-p entry status))
                    *modification-journal*))
         (count (if (and (integerp limit) (plusp limit))
                    limit
                    most-positive-fixnum)))
    (loop for entry in filtered
          for index from 0
          while (< index count)
          collect (append
                   (list :id (modification-entry-id entry)
                         :timestamp (modification-entry-timestamp entry)
                         :agent-id (modification-entry-agent-id entry)
                         :status (modification-entry-status entry)
                         :approval-mode (modification-entry-approval-mode entry)
                         :code (modification-entry-form-text entry)
                         :result (modification-entry-result entry)
                         :error-message (modification-entry-error-message entry)
                         :applied-symbols (modification-entry-applied-symbols entry))
                   (when include-diff-p
                     (list :diff (%entry-diff-summary entry)))))))

(defun %format-modification-timestamp (universal-time)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time 0)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour minute second)))

(defun modification-history-browser (&key status limit (include-diff-p t))
  "Render a textual history browser for self-modification entries."
  (let ((entries (list-modifications :status status
                                     :limit limit
                                     :include-diff-p include-diff-p)))
    (if (null entries)
        "No self-modifications recorded."
        (with-output-to-string (out)
          (format out "Self-Modification History (~D entries):~%" (length entries))
          (dolist (entry entries)
            (format out "  ~A [~A] agent=~A at ~A~%"
                    (getf entry :id)
                    (getf entry :status)
                    (getf entry :agent-id)
                    (%format-modification-timestamp (getf entry :timestamp)))
            (format out "    code: ~A~%" (getf entry :code))
            (when (getf entry :error-message)
              (format out "    error: ~A~%" (getf entry :error-message)))
            (when include-diff-p
              (format out "    diff: ~A~%" (getf entry :diff))))))))

(defun modification-journal-summary ()
  "Return a summary string of the modification journal."
  (with-output-to-string (out)
    (format out "Modification Journal (~A entries):~%" (length *modification-journal*))
    (dolist (entry (reverse *modification-journal*))
      (format out "  ~A [~A] ~A~%"
              (modification-entry-id entry)
              (modification-entry-status entry)
              (if (> (length (modification-entry-form-text entry)) 60)
                  (subseq (modification-entry-form-text entry) 0 60)
                  (modification-entry-form-text entry))))))
