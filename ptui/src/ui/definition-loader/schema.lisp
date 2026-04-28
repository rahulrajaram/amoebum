(defpackage :ptui.ui.definition-loader
  (:use :cl)
  (:export
   #:definition-loader-error
   #:definition-loader-error-path
   #:definition-loader-error-index
   #:definition-loader-error-form
   #:definition-loader-error-detail
   #:definition-unit
   #:make-definition-unit
   #:definition-unit-kind
   #:definition-unit-name
   #:definition-unit-lambda-list
   #:definition-unit-options
   #:definition-unit-body
   #:definition-unit-source-form
   #:definition-load-result
   #:definition-load-result-path
   #:definition-load-result-form-count
   #:definition-load-result-panel-names
   #:definition-load-result-app-names
   #:definition-load-result-widget-names
   #:+default-allowed-top-level-operators+
   #:+default-packet-directives+
   #:read-definition-forms
   #:validate-packet-form
   #:validate-packet-forms
   #:packet-form-definition-units
   #:packet-forms-definition-units
   #:compile-definition-unit
   #:compile-definition-units
   #:translate-packet-form
   #:translate-packet-forms
   #:expand-definition-forms
   #:validate-definition-form
   #:load-definition-forms
   #:load-definition-file
   #:app-runner-symbol
   #:run-loaded-app
   #:load-and-run-definition-file))

(in-package :ptui.ui.definition-loader)

(defparameter +default-allowed-top-level-operators+
  '("DEFPACKAGE" "IN-PACKAGE" "DEFUN" "DEFVAR" "DEFPARAMETER" "DEFCONSTANT"
    "DEFWIDGET" "DEFPANEL" "DEFAPP" "PROGN" "EVAL-WHEN")
  "Allowlist used by VALIDATE-DEFINITION-FORM for top-level operators.")

(defparameter +default-packet-directives+
  '("PTUI" "PTUI-DEFINITION" "DEFINITION"
    "DEFPACKAGE" "IN-PACKAGE" "PANEL" "APP" "WIDGET"
    "BREADCRUMBS" "PROJECT-TREE")
  "Allowlist used by VALIDATE-PACKET-FORM for keyword-based packet directives.")

(define-condition definition-loader-error (error)
  ((path :initarg :path
         :reader definition-loader-error-path)
   (index :initarg :index
          :reader definition-loader-error-index)
   (form :initarg :form
         :reader definition-loader-error-form)
   (detail :initarg :detail
           :reader definition-loader-error-detail))
  (:report (lambda (condition stream)
             (format stream "PTUI definition loader error (~A form #~D): ~A~%  Form: ~S"
                     (definition-loader-error-path condition)
                     (definition-loader-error-index condition)
                     (definition-loader-error-detail condition)
                     (definition-loader-error-form condition)))))

(defstruct (definition-load-result
            (:constructor make-definition-load-result
                (&key path form-count panel-names app-names widget-names)))
  (path "<memory>" :type (or string pathname))
  (form-count 0 :type fixnum)
  (panel-names '() :type list)
  (app-names '() :type list)
  (widget-names '() :type list))

(defstruct (definition-unit
            (:constructor make-definition-unit
                (&key kind name lambda-list options body source-form)))
  (kind :raw-form :type keyword)
  (name nil)
  (lambda-list '() :type list)
  (options '() :type list)
  (body '() :type list)
  (source-form nil))

(defstruct (packet-argument-rule
            (:constructor make-packet-argument-rule (&key position predicate detail)))
  (position 0 :type fixnum)
  predicate
  (detail "" :type string))

(defstruct (packet-option-rule
            (:constructor make-packet-option-rule (&key key predicate detail)))
  key
  predicate
  (detail "" :type string))

(defstruct (packet-directive-schema
            (:constructor make-packet-directive-schema
                (&key name wrapper-p min-children child-detail argument-rules option-rules)))
  (name "" :type string)
  (wrapper-p nil :type boolean)
  (min-children 0 :type fixnum)
  child-detail
  (argument-rules '() :type list)
  (option-rules '() :type list))

(defun %loader-error (path index form detail)
  (error 'definition-loader-error
         :path path
         :index index
         :form form
         :detail detail))

(defun %make-raw-form-unit (form &key source-form)
  (make-definition-unit
   :kind :raw-form
   :body (list form)
   :source-form (or source-form form)))

(defun %make-widget-definition-unit (name lambda-list body source-form)
  (make-definition-unit
   :kind :widget
   :name name
   :lambda-list lambda-list
   :body body
   :source-form source-form))

(defun %make-panel-definition-unit (name lambda-list sections source-form)
  (make-definition-unit
   :kind :panel
   :name name
   :lambda-list lambda-list
   :body sections
   :source-form source-form))

(defun %make-app-definition-unit (name options body source-form)
  (make-definition-unit
   :kind :app
   :name name
   :options options
   :body body
   :source-form source-form))

(defun %form-operator-name (form)
  (when (and (consp form) (symbolp (car form)))
    (string-upcase (symbol-name (car form)))))

(defun %definition-form-kind (form)
  (let ((operator-name (%form-operator-name form)))
    (cond
      ((null operator-name) :unknown)
      ((string= operator-name "DEFPANEL") :panel)
      ((string= operator-name "DEFAPP") :app)
      ((string= operator-name "DEFWIDGET") :widget)
      (t :other))))

(defun %definition-form-name (form)
  (when (and (consp (cdr form))
             (symbolp (second form)))
    (second form)))

(defun %declarative-directive-name (form)
  (when (and (consp form) (keywordp (car form)))
    (string-upcase (symbol-name (car form)))))

(defun %directive-wrapper-p (directive-name)
  (member directive-name '("PTUI" "PTUI-DEFINITION" "DEFINITION")
          :test #'string=))

(defun %packet-schema-error (path index form detail &optional context)
  (%loader-error path index form
                 (if context
                     (format nil "Packet schema violation (~A): ~A" context detail)
                     (format nil "Packet schema violation: ~A" detail))))

(defun %packet-supported-directives-string (allowed-directives)
  (format nil "~{~A~^, ~}"
          (mapcar (lambda (name)
                    (format nil ":~(~A~)" name))
                  allowed-directives)))

(defun %packet-directive-options (form)
  (cdddr form))

(defun %option-value (options key default)
  (let ((marker (gensym "MISSING")))
    (let ((value (getf options key marker)))
      (if (eq value marker)
          default
          value))))

(defun %validate-directive-options (path index form options allowed-keys context)
  (unless (evenp (length options))
    (%packet-schema-error path index form
                          "Directive options must be a plist with even key/value entries."
                          context))
  (loop for key in options by #'cddr
        do
           (unless (keywordp key)
             (%packet-schema-error path index form
                                   (format nil "Directive option key ~S must be a keyword." key)
                                   context))
           (unless (member key allowed-keys :test #'eq)
             (%packet-schema-error
              path index form
              (format nil "Unknown directive option ~S. Allowed options: ~{~S~^, ~}."
                      key allowed-keys)
              context))))

(defun %package-designator-p (value)
  (typep value '(or symbol string)))

(defun %one-symbol-lambda-list-p (value)
  (and (listp value)
       (= (length value) 1)
       (symbolp (first value))))

(defun %project-tree-lambda-list-p (value)
  (and (listp value)
       (>= (length value) 2)
       (symbolp (first value))
       (symbolp (second value))))

(defun %positive-integer-p (value)
  (and (integerp value) (> value 0)))

(defparameter *packet-directive-schemas*
  (list
   (make-packet-directive-schema
    :name "PTUI"
    :wrapper-p t
    :min-children 1
    :child-detail "Wrapper children must be keyword directives.")
   (make-packet-directive-schema
    :name "PTUI-DEFINITION"
    :wrapper-p t
    :min-children 1
    :child-detail "Wrapper children must be keyword directives.")
   (make-packet-directive-schema
    :name "DEFINITION"
    :wrapper-p t
    :min-children 1
    :child-detail "Wrapper children must be keyword directives.")
   (make-packet-directive-schema
    :name "DEFPACKAGE"
    :argument-rules
    (list (make-packet-argument-rule
           :position 1
           :predicate #'%package-designator-p
           :detail "Directive :defpackage requires a symbol or string package name.")))
   (make-packet-directive-schema
    :name "IN-PACKAGE"
    :argument-rules
    (list (make-packet-argument-rule
           :position 1
           :predicate #'%package-designator-p
           :detail "Directive :in-package requires a symbol or string package designator.")))
   (make-packet-directive-schema
    :name "PANEL"
    :argument-rules
    (list (make-packet-argument-rule
           :position 1
           :predicate #'symbolp
           :detail "Directive :panel requires a symbol name as its second argument.")
          (make-packet-argument-rule
           :position 2
           :predicate #'listp
           :detail "Directive :panel requires a lambda-list as its third argument.")))
   (make-packet-directive-schema
    :name "APP"
    :argument-rules
    (list (make-packet-argument-rule
           :position 1
           :predicate #'symbolp
           :detail "Directive :app requires a symbol name as its second argument.")
          (make-packet-argument-rule
           :position 2
           :predicate #'listp
           :detail "Directive :app requires an option plist list as its third argument.")))
   (make-packet-directive-schema
    :name "WIDGET"
    :argument-rules
    (list (make-packet-argument-rule
           :position 1
           :predicate #'symbolp
           :detail "Directive :widget requires a symbol name as its second argument.")
          (make-packet-argument-rule
           :position 2
           :predicate #'listp
           :detail "Directive :widget requires a lambda-list as its third argument.")))
   (make-packet-directive-schema
    :name "BREADCRUMBS"
    :argument-rules
    (list (make-packet-argument-rule
           :position 1
           :predicate #'symbolp
           :detail "Directive :breadcrumbs requires a symbol widget name as its second argument.")
          (make-packet-argument-rule
           :position 2
           :predicate #'%one-symbol-lambda-list-p
           :detail "Directive :breadcrumbs requires a one-symbol lambda-list (segments)."))
    :option-rules
    (list (make-packet-option-rule
           :key :prefix
           :predicate #'stringp
           :detail "Directive :breadcrumbs option :prefix must be a string.")
          (make-packet-option-rule
           :key :separator
           :predicate #'stringp
           :detail "Directive :breadcrumbs option :separator must be a string.")))
   (make-packet-directive-schema
    :name "PROJECT-TREE"
    :argument-rules
    (list (make-packet-argument-rule
           :position 1
           :predicate #'symbolp
           :detail "Directive :project-tree requires a symbol widget name as its second argument.")
          (make-packet-argument-rule
           :position 2
           :predicate #'%project-tree-lambda-list-p
           :detail "Directive :project-tree requires a lambda-list beginning with (rows selected-index ...)."))
    :option-rules
    (list (make-packet-option-rule
           :key :height
           :predicate #'%positive-integer-p
           :detail "Directive :project-tree option :height must be a positive integer.")
          (make-packet-option-rule
           :key :marker
           :predicate #'stringp
           :detail "Directive :project-tree option :marker must be a string."))))
  "Declarative packet-validation schema keyed by directive name.")

(defun %find-packet-directive-schema (directive-name)
  (find directive-name *packet-directive-schemas*
        :test #'string=
        :key #'packet-directive-schema-name))
