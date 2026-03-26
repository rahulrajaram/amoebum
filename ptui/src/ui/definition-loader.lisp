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

(defun %validate-packet-argument-rules (path index form schema context)
  (dolist (rule (packet-directive-schema-argument-rules schema))
    (let ((value (nth (packet-argument-rule-position rule) form)))
      (unless (funcall (packet-argument-rule-predicate rule) value)
        (%packet-schema-error path index form
                              (packet-argument-rule-detail rule)
                              context)))))

(defun %validate-packet-option-rules (path index form schema context)
  (let* ((options (%packet-directive-options form))
         (option-rules (packet-directive-schema-option-rules schema))
         (allowed-keys (mapcar #'packet-option-rule-key option-rules)))
    (when option-rules
      (%validate-directive-options path index form options allowed-keys context)
      (dolist (rule option-rules)
        (let ((marker (gensym "MISSING"))
              (key (packet-option-rule-key rule)))
          (let ((value (getf options key marker)))
            (unless (eq value marker)
              (unless (funcall (packet-option-rule-predicate rule) value)
                (%packet-schema-error path index form
                                      (packet-option-rule-detail rule)
                                      context)))))))))

(defun %validate-wrapper-directive (form path index allowed-directives context directive-name schema)
  (let ((children (cdr form)))
    (when (< (length children) (packet-directive-schema-min-children schema))
      (%packet-schema-error path index form
                            "Wrapper directive requires at least one child directive."
                            context))
    (loop for child in children
          for child-index from 1
          do (unless (and (consp child) (keywordp (car child)))
               (%packet-schema-error
                path index child
                (or (packet-directive-schema-child-detail schema)
                    "Wrapper children must be keyword directives.")
                (format nil ":~(~A~) child #~D"
                        directive-name child-index)))
             (validate-packet-form child
                                   :path path
                                   :index index
                                   :allowed-directives allowed-directives
                                   :context
                                   (format nil ":~(~A~) child #~D"
                                           directive-name child-index)))))

(defun %validate-directive-with-schema (form path index allowed-directives context directive-name schema)
  (if (packet-directive-schema-wrapper-p schema)
      (%validate-wrapper-directive form path index allowed-directives context directive-name schema)
      (progn
        (%validate-packet-argument-rules path index form schema context)
        (%validate-packet-option-rules path index form schema context))))

(defun validate-packet-form (form &key (path "<memory>") (index 0)
                                  (allowed-directives +default-packet-directives+)
                                  context)
  "Validate one packet FORM against ALLOWED-DIRECTIVES.
Non-keyword top-level forms are accepted for backward compatibility."
  (let ((directive-name (%declarative-directive-name form)))
    (cond
      ((null directive-name) t)
      ((not (member directive-name allowed-directives :test #'string=))
       (%packet-schema-error
        path index form
        (format nil
                "Unknown packet directive :~(~A~). Supported directives: ~A."
                directive-name
                (%packet-supported-directives-string allowed-directives))
        context))
      (t
       (let ((schema (%find-packet-directive-schema directive-name)))
         (unless schema
           (%packet-schema-error
            path index form
            (format nil
                    "No validation schema registered for :~(~A~)."
                    directive-name)
            context))
         (%validate-directive-with-schema form path index allowed-directives context directive-name schema))))
  t))

(defun validate-packet-forms (forms &key (path "<memory>")
                                    (allowed-directives +default-packet-directives+))
  "Validate packet FORMS before declarative expansion."
  (loop for form in forms
        for index from 1
        do (validate-packet-form form
                                 :path path
                                 :index index
                                 :allowed-directives allowed-directives))
  t)

(defun %breadcrumbs-definition-units (form path index)
  (destructuring-bind (directive name lambda-list &rest options) form
    (declare (ignore directive))
    (unless (symbolp name)
      (%loader-error path index form
                     "Declarative :breadcrumbs requires a symbol widget name."))
    (unless (and (listp lambda-list)
                 (= (length lambda-list) 1)
                 (symbolp (first lambda-list)))
      (%loader-error path index form
                     "Declarative :breadcrumbs requires a one-symbol lambda-list (segments)."))
    (%validate-directive-options path index form options '(:prefix :separator) ":breadcrumbs")
    (let ((segments-var (first lambda-list))
          (prefix (%option-value options :prefix "Path: "))
          (separator (%option-value options :separator " / ")))
      (unless (stringp prefix)
        (%loader-error path index form
                       "Declarative :breadcrumbs option :prefix must be a string."))
      (unless (stringp separator)
        (%loader-error path index form
                       "Declarative :breadcrumbs option :separator must be a string."))
      (list
       (%make-widget-definition-unit
        name
        lambda-list
        (list
         `(ptui.widgets.core:make-text-widget
           (with-output-to-string (stream)
             (write-string ,prefix stream)
             (loop for segment in ,segments-var
                   for firstp = t then nil
                   do (unless firstp
                        (write-string ,separator stream))
                      (princ segment stream)))))
        form)))))

(defun %project-tree-definition-units (form path index)
  (destructuring-bind (directive name lambda-list &rest options) form
    (declare (ignore directive))
    (unless (symbolp name)
      (%loader-error path index form
                     "Declarative :project-tree requires a symbol widget name."))
    (unless (and (listp lambda-list)
                 (>= (length lambda-list) 2)
                 (symbolp (first lambda-list))
                 (symbolp (second lambda-list)))
      (%loader-error path index form
                     "Declarative :project-tree requires lambda-list (rows selected-index ...)."))
    (%validate-directive-options path index form options '(:height :marker) ":project-tree")
    (let ((rows-var (first lambda-list))
          (selected-index-var (second lambda-list))
          (height (%option-value options :height 10))
          (marker (%option-value options :marker ">")))
      (unless (and (integerp height) (> height 0))
        (%loader-error path index form
                       "Declarative :project-tree option :height must be a positive integer."))
      (unless (stringp marker)
        (%loader-error path index form
                       "Declarative :project-tree option :marker must be a string."))
      (list
       (%make-widget-definition-unit
        name
        lambda-list
        (list
         `(ptui.views:list-view
           ,rows-var
           (lambda (entry index selected-p)
             (declare (ignore index))
             (ptui.widgets.core:make-text-widget
              (format nil "~A ~A"
                      (if selected-p ,marker " ")
                      entry)))
           ,height nil ,selected-index-var nil))
        form)))))

(defun %packet-form-definition-units (form path index)
  (let ((directive-name (%declarative-directive-name form)))
    (cond
      ((null directive-name)
       (list (%make-raw-form-unit form)))
      ((%directive-wrapper-p directive-name)
       (mapcan (lambda (inner)
                 (%packet-form-definition-units inner path index))
               (cdr form)))
      ((string= directive-name "DEFPACKAGE")
       (list (%make-raw-form-unit `(defpackage ,@(cdr form))
                                  :source-form form)))
      ((string= directive-name "IN-PACKAGE")
       (list (%make-raw-form-unit `(in-package ,@(cdr form))
                                  :source-form form)))
      ((string= directive-name "PANEL")
       (destructuring-bind (directive name lambda-list &rest sections) form
         (declare (ignore directive))
         (unless (symbolp name)
           (%loader-error path index form
                          "Declarative :panel requires a symbol name."))
         (unless (listp lambda-list)
           (%loader-error path index form
                          "Declarative :panel requires a lambda-list list."))
         (list (%make-panel-definition-unit name lambda-list sections form))))
      ((string= directive-name "APP")
       (destructuring-bind (directive name options &rest body) form
         (declare (ignore directive))
         (unless (symbolp name)
           (%loader-error path index form
                          "Declarative :app requires a symbol name."))
         (unless (listp options)
           (%loader-error path index form
                          "Declarative :app requires an option plist list."))
         (list (%make-app-definition-unit name options body form))))
      ((string= directive-name "WIDGET")
       (destructuring-bind (directive name lambda-list &rest widget-body) form
         (declare (ignore directive))
         (unless (symbolp name)
           (%loader-error path index form
                          "Declarative :widget requires a symbol name."))
         (unless (listp lambda-list)
           (%loader-error path index form
                          "Declarative :widget requires a lambda-list list."))
         (list (%make-widget-definition-unit name lambda-list widget-body form))))
      ((string= directive-name "BREADCRUMBS")
       (%breadcrumbs-definition-units form path index))
      ((string= directive-name "PROJECT-TREE")
       (%project-tree-definition-units form path index))
      (t
       (%loader-error path index form
                      (format nil
                              "Unknown declarative directive :~A. Supported directives: :ptui, :panel, :app, :widget, :breadcrumbs, :project-tree, :defpackage, :in-package."
                              (string-downcase directive-name)))))))

(defun packet-form-definition-units (form &key (path "<memory>") (index 1))
  "Translate one packet FORM into explicit PTUI definition units."
  (%packet-form-definition-units form path index))

(defun packet-forms-definition-units (forms &key (path "<memory>"))
  "Translate packet FORMS into explicit PTUI definition units."
  (let ((units '()))
    (loop for form in forms
          for index from 1
          do (setf units
                   (nconc units
                          (packet-form-definition-units form :path path :index index))))
    units))

(defun %compile-raw-form-unit (unit path index)
  (declare (ignore path index))
  (copy-list (definition-unit-body unit)))

(defun %compile-widget-definition-unit (unit path index)
  (declare (ignore path index))
  (list
   `(ptui.widgets.defwidget:defwidget
      ,(definition-unit-name unit)
      ,(definition-unit-lambda-list unit)
      ,@(definition-unit-body unit))))

(defun %compile-panel-definition-unit (unit path index)
  (declare (ignore path index))
  (list
   `(ptui.ui.panel:defpanel
      ,(definition-unit-name unit)
      ,(definition-unit-lambda-list unit)
      ,@(definition-unit-body unit))))

(defun %compile-app-definition-unit (unit path index)
  (declare (ignore path index))
  (list
   `(ptui.ui.app:defapp
      ,(definition-unit-name unit)
      ,(definition-unit-options unit)
      ,@(definition-unit-body unit))))

(defparameter *definition-unit-compilers*
  '((:raw-form . %compile-raw-form-unit)
    (:widget . %compile-widget-definition-unit)
    (:panel . %compile-panel-definition-unit)
    (:app . %compile-app-definition-unit))
  "Compiler registry for PTUI definition units.")

(defun compile-definition-unit (unit &key (path "<memory>") (index 1))
  "Compile one explicit definition UNIT into executable Lisp forms."
  (check-type unit definition-unit)
  (let ((compiler-name (cdr (assoc (definition-unit-kind unit)
                                   *definition-unit-compilers*
                                   :test #'eq))))
    (unless compiler-name
      (%loader-error path index unit
                     (format nil "No compiler registered for definition unit kind ~S."
                             (definition-unit-kind unit))))
    (funcall compiler-name unit path index)))

(defun compile-definition-units (units &key (path "<memory>"))
  "Compile explicit PTUI definition UNITS into executable Lisp forms."
  (let ((forms '()))
    (loop for unit in units
          for index from 1
          do (setf forms
                   (nconc forms
                          (compile-definition-unit unit :path path :index index))))
    forms))

(defun read-definition-forms (path)
  "Read all forms from PATH and return them as a list."
  (let ((eof (gensym "EOF"))
        (forms '()))
    (with-open-file (stream path :direction :input :if-does-not-exist nil)
      (unless stream
        (%loader-error path 0 path "Definition file does not exist or is not readable."))
      (loop for form = (read stream nil eof)
            until (eq form eof)
            do (push form forms)))
    (nreverse forms)))

(defun translate-packet-form (form &key (path "<memory>") (index 1))
  "Translate one packet FORM into executable forms."
  (compile-definition-units
   (packet-form-definition-units form :path path :index index)
   :path path))

(defun translate-packet-forms (forms &key (path "<memory>"))
  "Translate packet FORMS into executable defwidget/defpanel/defapp forms."
  (compile-definition-units
   (packet-forms-definition-units forms :path path)
   :path path))

(defun expand-definition-forms (forms &key (path "<memory>"))
  "Expand declarative PTUI directives into executable Lisp forms."
  (translate-packet-forms forms :path path))

(defun validate-definition-form (form &key (path "<memory>") (index 0)
                                      (allowed-operators +default-allowed-top-level-operators+))
  "Validate a top-level definition FORM against ALLOWED-OPERATORS."
  (unless (consp form)
    (%loader-error path index form
                   "Top-level form must be a non-empty list."))
  (let ((operator (car form))
        (operator-name (%form-operator-name form)))
    (unless (and (symbolp operator) operator-name)
      (%loader-error path index form
                     "Top-level form head must be a symbol."))
    (unless (member operator-name allowed-operators :test #'string=)
      (%loader-error path index form
                     (format nil
                             "Top-level operator ~S is not allowlisted."
                             operator-name)))
    (case (%definition-form-kind form)
      (:panel
       (unless (symbolp (%definition-form-name form))
         (%loader-error path index form
                        "DEFPANEL requires a symbol name as the second argument."))
       (unless (listp (third form))
         (%loader-error path index form
                        "DEFPANEL requires a lambda-list as the third argument.")))
      (:app
       (unless (symbolp (%definition-form-name form))
         (%loader-error path index form
                        "DEFAPP requires a symbol name as the second argument."))
       (unless (listp (third form))
         (%loader-error path index form
                        "DEFAPP requires an option plist list as the third argument.")))
      (:widget
       (unless (symbolp (%definition-form-name form))
         (%loader-error path index form
                        "DEFWIDGET requires a symbol name as the second argument."))
       (unless (listp (third form))
         (%loader-error path index form
                        "DEFWIDGET requires a lambda-list as the third argument.")))))
  t)

(defun %verify-widget-registration (widget-names path)
  (dolist (widget-name widget-names)
    (unless (ptui.widgets.defwidget:find-widget widget-name)
      (%loader-error path 0 widget-name
                     (format nil "Widget ~S was not registered after load."
                             widget-name)))))

(defun app-runner-symbol (app-name)
  "Return the generated RUN-<APP-NAME> symbol for DEFAPP APP-NAME."
  (check-type app-name symbol)
  (let ((package (or (symbol-package app-name) *package*)))
    (intern (format nil "RUN-~A" (symbol-name app-name)) package)))

(defun %verify-app-registration (app-names path)
  (dolist (app-name app-names)
    (let ((runner (app-runner-symbol app-name)))
      (unless (fboundp runner)
        (%loader-error path 0 app-name
                       (format nil "App ~S runner ~S was not defined after load."
                               app-name runner))))))

(defun %rehome-symbol-for-package (symbol source-package target-package)
  (cond
    ((not (symbolp symbol)) symbol)
    ((keywordp symbol) symbol)
    ((or (null source-package) (null target-package)) symbol)
    ((eq (symbol-package symbol) source-package)
     (intern (symbol-name symbol) target-package))
    (t symbol)))

(defun %rehome-form-for-package (form source-package target-package)
  (cond
    ((symbolp form)
     (%rehome-symbol-for-package form source-package target-package))
    ((stringp form)
     form)
    ((consp form)
     (cons (%rehome-form-for-package (car form) source-package target-package)
           (%rehome-form-for-package (cdr form) source-package target-package)))
    ((vectorp form)
     (map 'vector
          (lambda (entry)
            (%rehome-form-for-package entry source-package target-package))
          form))
    (t form)))

(defun load-definition-forms (forms &key (path "<memory>") package (validate t)
                                    (allowed-operators +default-allowed-top-level-operators+)
                                    (packet-directives +default-packet-directives+)
                                    source-package)
  "Load FORMS into the current image and verify panel/app registration.
FORMS may include declarative directives such as (:panel ...), (:app ...),
and wrappers like (:ptui ...)."
  (when validate
    (validate-packet-forms forms
                           :path path
                           :allowed-directives packet-directives))
  (let* ((definition-units (packet-forms-definition-units forms :path path))
         (expanded-forms (compile-definition-units definition-units :path path))
         (widget-names '())
         (panel-names '())
         (app-names '()))
    (let ((*package* (if package
                         (or (find-package package)
                             (%loader-error path 0 package
                                            "Requested package does not exist."))
                         *package*)))
      (loop for form in expanded-forms
            for index from 1
            for normalized-form =
               (%rehome-form-for-package form source-package *package*)
             do (when validate
                  (validate-definition-form normalized-form
                                            :path path
                                            :index index
                                            :allowed-operators allowed-operators))
                (case (%definition-form-kind normalized-form)
                  (:panel
                   (let ((name (%definition-form-name normalized-form)))
                     (pushnew name panel-names :test #'eq)
                     (pushnew name widget-names :test #'eq)))
                  (:widget
                   (pushnew (%definition-form-name normalized-form) widget-names :test #'eq))
                  (:app
                   (pushnew (%definition-form-name normalized-form) app-names :test #'eq)))
                (handler-case
                    (eval normalized-form)
                  (error (condition)
                    (%loader-error path index normalized-form
                                   (format nil "Evaluation failed: ~A" condition))))))
    (%verify-widget-registration widget-names path)
    (%verify-app-registration app-names path)
    (make-definition-load-result
     :path path
     :form-count (length expanded-forms)
     :panel-names (nreverse panel-names)
     :app-names (nreverse app-names)
     :widget-names (nreverse widget-names))))

(defun load-definition-file (path &key package (validate t)
                                  (allowed-operators +default-allowed-top-level-operators+)
                                  (packet-directives +default-packet-directives+))
  "Load a PTUI definition file from PATH."
  (let ((forms (read-definition-forms path))
        (source-package *package*))
    (load-definition-forms forms
                           :path path
                           :package package
                           :validate validate
                           :allowed-operators allowed-operators
                           :packet-directives packet-directives
                           :source-package source-package)))

(defun run-loaded-app (app-name &key override-backend)
  "Run an app previously defined via DEFAPP."
  (let ((runner (app-runner-symbol app-name)))
    (unless (fboundp runner)
      (error "App runner ~S is not fbound. Load the definition first." runner))
    (if override-backend
        (funcall (symbol-function runner) :override-backend override-backend)
        (funcall (symbol-function runner)))))

(defun load-and-run-definition-file (path app-name &key package (validate t)
                                          (allowed-operators +default-allowed-top-level-operators+)
                                          (packet-directives +default-packet-directives+)
                                          override-backend)
  "Load PATH and run APP-NAME in one call."
  (load-definition-file path
                        :package package
                        :validate validate
                        :allowed-operators allowed-operators
                        :packet-directives packet-directives)
  (run-loaded-app app-name :override-backend override-backend))
