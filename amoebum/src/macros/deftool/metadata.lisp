(in-package :amoebum)

;;;; NXT-395: deftool metadata helpers.
;;;;
;;;; This module owns the stable metadata / compile-time bookkeeping substrate
;;;; used by the `deftool` macroexpansion pipeline and by runtime tool lookup.
;;;; The historical definitions lived inline in `src/macros/deftool.lisp`;
;;;; only file boundaries change here.

(defparameter *toolset* (pseudopod:make-toolset))
(defparameter *tool-metadata* (make-hash-table :test #'equal))
(defparameter *tool-history* (make-hash-table :test #'equal))
(defparameter *tool-history-max-versions* 10)
(defparameter *deftool-compile-time-tool-names* (make-hash-table :test #'equal))

(defun find-tool-metadata (tool-name)
  "Canonical tool-metadata lookup by normalized name."
  (and (hash-table-p *tool-metadata*)
       (gethash (normalize-name tool-name) *tool-metadata*)))

(defconstant +missing-tool-argument+ :amoebum/missing-tool-argument)
(defparameter +allowed-permission-modes+ '(:auto :supervised :full-auto))

(defstruct (tool-metadata
            (:constructor make-tool-metadata
                (&key name permission dangerous-p category timeout-seconds
                 source-file source-line parameter-specs defined-at mcp-server)))
  name
  permission
  dangerous-p
  category
  timeout-seconds
  source-file
  source-line
  parameter-specs
  defined-at
  mcp-server)

(defun %ensure-toolset ()
  (unless (and (boundp '*toolset*)
               (pseudopod:toolset-p *toolset*))
    (setf *toolset* (pseudopod:make-toolset)))
  *toolset*)

(defun %tool-name-string (name)
  (let ((value (string-downcase
                (string-trim '(#\Space #\Tab #\Newline #\Return)
                             (cond
                               ((symbolp name) (symbol-name name))
                               ((stringp name) name)
                               (t (princ-to-string name)))))))
    (if (plusp (length value))
        value
        (error "Tool name must not be blank."))))

(defun %hash-table-keys (table)
  (loop for key being the hash-keys of table collect key))

(defun %getf-boolean (plist key default)
  (if (member key plist :test #'eq)
      (not (null (getf plist key)))
      default))

(defun %blank-string-p (value)
  (or (null value)
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  (princ-to-string value))))))

(defun %normalize-parameter-spec (spec)
  (unless (and (consp spec) (symbolp (first spec)))
    (error "Invalid deftool parameter spec: ~S" spec))
  (destructuring-bind (name type &rest options) spec
    (unless (evenp (length options))
      (error "Parameter options must be key/value pairs for ~S." name))
    (let ((description (getf options :description))
          (required (%getf-boolean options :required nil))
          (default-supplied-p (member :default options :test #'eq)))
      (list :name name
            :type type
            :description (and description (princ-to-string description))
            :required required
            :default (getf options :default)
            :default-supplied-p (not (null default-supplied-p))))))

(defun %deftool-type-spec-valid-p (type-spec)
  (handler-case
      (progn
        (typep nil type-spec)
        t)
    (error ()
      nil)))

(defun %validate-tool-parameter-specs (tool-name normalized-parameters)
  (dolist (parameter normalized-parameters)
    (let ((parameter-name (getf parameter :name))
          (parameter-type (getf parameter :type))
          (required-p (getf parameter :required))
          (description (getf parameter :description)))
      (unless (%deftool-type-spec-valid-p parameter-type)
        (error "DEFTTOOL ~S parameter ~S has invalid type spec ~S."
               tool-name
               parameter-name
               parameter-type))
      (when (and required-p
                 (or (null description)
                     (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                 description)))))
        (warn 'missing-tool-description
              :tool-name tool-name
              :parameter parameter-name
              :reason "Required parameter is missing :description.")))))

(defun %validate-dangerous-permission-combination (tool-name permission dangerous-p)
  (when (and dangerous-p (eq permission :auto))
    (warn 'dangerous-auto-permission
          :tool-name tool-name
          :parameter :permission
          :reason "Dangerous tool declared with :permission :auto; use :supervised (the default).")))

(defun reset-deftool-compile-validation-state ()
  (clrhash *deftool-compile-time-tool-names*)
  t)

(defun %record-deftool-name-for-validation (tool-name)
  (let ((normalized (%tool-name-string tool-name)))
    (if (gethash normalized *deftool-compile-time-tool-names*)
        (progn
          (warn 'duplicate-tool-name
                :tool-name normalized)
          t)
        (progn
          (setf (gethash normalized *deftool-compile-time-tool-names*) t)
          nil))))
