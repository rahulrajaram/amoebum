(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; Self-Modifying Agent (I98)
;;;
;;; Allows runtime code modifications with safety controls:
;;; - sandboxed-eval: restricted sandbox + timeout
;;; - Approval gate: modifications require human approval
;;; - Modification journal: tracks all applied changes
;;; - Rollback: fmakunbound/remove-method to undo changes
;;; ---------------------------------------------------------------------------

;;; --- Event Types ---

(defparameter +event-type-self-modify-proposed+ "self-modify:proposed")
(defparameter +event-type-self-modify-approved+ "self-modify:approved")
(defparameter +event-type-self-modify-applied+ "self-modify:applied")
(defparameter +event-type-self-modify-rolled-back+ "self-modify:rolled-back")

;;; --- Modification Journal ---

(defstruct (modification-entry
            (:constructor make-modification-entry
                (&key id form-text result status
                      (timestamp (get-universal-time))
                      applied-symbols previous-definitions
                      error-message)))
  (id "" :type string)
  (form-text "" :type string)
  (result nil)
  (status :proposed :type keyword)
  (timestamp (get-universal-time) :type integer)
  (applied-symbols '() :type list)
  (previous-definitions '() :type list)
  (error-message nil :type (or null string)))

(defvar *modification-journal* '()
  "Chronological journal of all self-modification attempts.")

(defvar *modification-counter* 0
  "Counter for generating unique modification IDs.")

(defun %next-modification-id ()
  "Generate a unique modification ID."
  (format nil "mod-~A-~A" (incf *modification-counter*) (get-universal-time)))

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

(defun sandboxed-eval (form-text &key (timeout-seconds 10) (package :amoebum.sandbox))
  "Evaluate FORM-TEXT in a restricted sandbox with timeout.
Returns (values result errorp error-message)."
  (handler-case
      (let* ((readtable (or *sandbox-restricted-readtable*
                            (make-restricted-readtable)))
             (form (let ((*readtable* readtable)
                         (*read-eval* nil)
                         (*package* (or (find-package package)
                                        (find-package :amoebum.sandbox))))
                     (read-from-string form-text)))
             (result nil)
             (errorp nil)
             (error-msg nil))
        #+sbcl
        (handler-case
            (sb-ext:with-timeout timeout-seconds
              (setf result (eval form)))
          (sb-ext:timeout ()
            (setf errorp t
                  error-msg (format nil "Evaluation timed out after ~As" timeout-seconds)))
          (error (c)
            (setf errorp t
                  error-msg (format nil "Evaluation error: ~A" c))))
        #-sbcl
        (handler-case
            (setf result (eval form))
          (error (c)
            (setf errorp t
                  error-msg (format nil "Evaluation error: ~A" c))))
        (values result errorp error-msg))
    (error (c)
      (values nil t (format nil "Parse error: ~A" c)))))

;;; --- Approval Gate ---

(defvar *self-modify-auto-approve-p* nil
  "When T, skip approval gate (for testing only).")

(defun propose-modification (form-text &key (event-bus (current-event-bus)))
  "Propose a code modification. Returns a modification-entry in :proposed status."
  (let* ((id (%next-modification-id))
         (entry (make-modification-entry :id id :form-text form-text :status :proposed)))
    (push entry *modification-journal*)
    (publish event-bus
             (make-event :type +event-type-self-modify-proposed+
                         :source "self-modify"
                         :payload (list :id id :form form-text)))
    entry))

(defun approve-modification (id &key (event-bus (current-event-bus)))
  "Approve a proposed modification by ID."
  (let ((entry (find-modification id)))
    (unless entry
      (error "Modification ~A not found." id))
    (unless (eq :proposed (modification-entry-status entry))
      (error "Modification ~A is not in proposed state (current: ~A)."
             id (modification-entry-status entry)))
    (setf (modification-entry-status entry) :approved)
    (publish event-bus
             (make-event :type +event-type-self-modify-approved+
                         :source "self-modify"
                         :payload (list :id id)))
    entry))

;;; --- Apply Modification ---

(defun apply-modification (id &key (event-bus (current-event-bus)) (timeout-seconds 10))
  "Apply an approved modification. Returns the modification-entry."
  (let ((entry (find-modification id)))
    (unless entry
      (error "Modification ~A not found." id))
    (unless (or (eq :approved (modification-entry-status entry))
                *self-modify-auto-approve-p*)
      (error "Modification ~A is not approved (current: ~A)."
             id (modification-entry-status entry)))
    ;; Capture previous definitions before applying
    (let* ((form (handler-case
                     (let ((*read-eval* nil))
                       (read-from-string (modification-entry-form-text entry)))
                   (error () nil)))
           (symbols (%collect-defined-symbols form))
           (prev-defs (mapcar #'%capture-previous-definition symbols)))
      (setf (modification-entry-applied-symbols entry) symbols
            (modification-entry-previous-definitions entry) prev-defs)
      ;; Evaluate in the full amoebum package (not sandbox) for actual modification
      (multiple-value-bind (result errorp error-msg)
          (sandboxed-eval (modification-entry-form-text entry)
                          :timeout-seconds timeout-seconds
                          :package :amoebum)
        (cond
          (errorp
           (setf (modification-entry-status entry) :failed
                 (modification-entry-error-message entry) error-msg)
           entry)
          (t
           (setf (modification-entry-status entry) :applied
                 (modification-entry-result entry) result)
           (publish event-bus
                    (make-event :type +event-type-self-modify-applied+
                                :source "self-modify"
                                :payload (list :id id :symbols symbols)))
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
