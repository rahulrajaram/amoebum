(defpackage :ptui.ui.definition-loader
  (:use :cl)
  (:export
   #:definition-loader-error
   #:definition-loader-error-path
   #:definition-loader-error-index
   #:definition-loader-error-form
   #:definition-loader-error-detail
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

(defun %loader-error (path index form detail)
  (error 'definition-loader-error
         :path path
         :index index
         :form form
         :detail detail))

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
      ((%directive-wrapper-p directive-name)
       (let ((children (cdr form)))
         (when (null children)
           (%packet-schema-error path index form
                                 "Wrapper directive requires at least one child directive."
                                 context))
         (loop for child in children
               for child-index from 1
               do (unless (and (consp child) (keywordp (car child)))
                    (%packet-schema-error
                     path index child
                     "Wrapper children must be keyword directives."
                     (format nil ":~(~A~) child #~D"
                             directive-name child-index)))
                  (validate-packet-form child
                                        :path path
                                        :index index
                                        :allowed-directives allowed-directives
                                        :context
                                        (format nil ":~(~A~) child #~D"
                                                directive-name child-index)))))
      ((string= directive-name "DEFPACKAGE")
       (unless (and (consp (cdr form))
                    (typep (second form) '(or symbol string)))
         (%packet-schema-error path index form
                               "Directive :defpackage requires a symbol or string package name."
                               context)))
      ((string= directive-name "IN-PACKAGE")
       (unless (and (consp (cdr form))
                    (typep (second form) '(or symbol string)))
         (%packet-schema-error path index form
                               "Directive :in-package requires a symbol or string package designator."
                               context)))
      ((string= directive-name "PANEL")
       (unless (and (consp (cdr form)) (symbolp (second form)))
         (%packet-schema-error path index form
                               "Directive :panel requires a symbol name as its second argument."
                               context))
       (unless (listp (third form))
         (%packet-schema-error path index form
                               "Directive :panel requires a lambda-list as its third argument."
                               context)))
      ((string= directive-name "APP")
       (unless (and (consp (cdr form)) (symbolp (second form)))
         (%packet-schema-error path index form
                               "Directive :app requires a symbol name as its second argument."
                               context))
       (unless (listp (third form))
         (%packet-schema-error path index form
                               "Directive :app requires an option plist list as its third argument."
                               context)))
      ((string= directive-name "WIDGET")
       (unless (and (consp (cdr form)) (symbolp (second form)))
         (%packet-schema-error path index form
                               "Directive :widget requires a symbol name as its second argument."
                               context))
       (unless (listp (third form))
         (%packet-schema-error path index form
                               "Directive :widget requires a lambda-list as its third argument."
                               context)))
      ((string= directive-name "BREADCRUMBS")
       (unless (and (consp (cdr form)) (symbolp (second form)))
         (%packet-schema-error path index form
                               "Directive :breadcrumbs requires a symbol widget name as its second argument."
                               context))
       (unless (and (listp (third form))
                    (= (length (third form)) 1)
                    (symbolp (first (third form))))
         (%packet-schema-error path index form
                               "Directive :breadcrumbs requires a one-symbol lambda-list (segments)."
                               context))
       (let ((options (%packet-directive-options form)))
         (%validate-directive-options path index form options '(:prefix :separator) context)
         (let ((prefix (%option-value options :prefix "Path: "))
               (separator (%option-value options :separator " / ")))
           (unless (stringp prefix)
             (%packet-schema-error path index form
                                   "Directive :breadcrumbs option :prefix must be a string."
                                   context))
           (unless (stringp separator)
             (%packet-schema-error path index form
                                   "Directive :breadcrumbs option :separator must be a string."
                                   context)))))
      ((string= directive-name "PROJECT-TREE")
       (unless (and (consp (cdr form)) (symbolp (second form)))
         (%packet-schema-error path index form
                               "Directive :project-tree requires a symbol widget name as its second argument."
                               context))
       (unless (and (listp (third form))
                    (>= (length (third form)) 2)
                    (symbolp (first (third form)))
                    (symbolp (second (third form))))
         (%packet-schema-error path index form
                               "Directive :project-tree requires a lambda-list beginning with (rows selected-index ...)."
                               context))
       (let ((options (%packet-directive-options form)))
         (%validate-directive-options path index form options '(:height :marker) context)
         (let ((height (%option-value options :height 10))
               (marker (%option-value options :marker ">")))
           (unless (and (integerp height) (> height 0))
             (%packet-schema-error path index form
                                   "Directive :project-tree option :height must be a positive integer."
                                   context))
           (unless (stringp marker)
             (%packet-schema-error path index form
                                   "Directive :project-tree option :marker must be a string."
                                   context))))))
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

(defun %translate-breadcrumbs-directive (form path index)
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
       `(ptui.widgets.defwidget:defwidget ,name ,lambda-list
          (ptui.widgets.core:make-text-widget
           (with-output-to-string (stream)
             (write-string ,prefix stream)
             (loop for segment in ,segments-var
                   for firstp = t then nil
                   do (unless firstp
                        (write-string ,separator stream))
                      (princ segment stream)))))))))

(defun %translate-project-tree-directive (form path index)
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
       `(ptui.widgets.defwidget:defwidget ,name ,lambda-list
          (ptui.views:list-view
           ,rows-var
           (lambda (entry index selected-p)
             (declare (ignore index))
             (ptui.widgets.core:make-text-widget
              (format nil "~A ~A"
                      (if selected-p ,marker " ")
                      entry)))
           ,height nil ,selected-index-var nil))))))

(defun %expand-declarative-form (form path index)
  (let ((directive-name (%declarative-directive-name form)))
    (cond
      ((null directive-name)
       (list form))
      ((%directive-wrapper-p directive-name)
       (mapcan (lambda (inner)
                 (%expand-declarative-form inner path index))
               (cdr form)))
      ((string= directive-name "DEFPACKAGE")
       (list `(defpackage ,@(cdr form))))
      ((string= directive-name "IN-PACKAGE")
       (list `(in-package ,@(cdr form))))
      ((string= directive-name "PANEL")
       (destructuring-bind (directive name lambda-list &rest sections) form
         (declare (ignore directive))
         (unless (symbolp name)
           (%loader-error path index form
                          "Declarative :panel requires a symbol name."))
         (unless (listp lambda-list)
           (%loader-error path index form
                          "Declarative :panel requires a lambda-list list."))
         (list `(ptui.ui.panel:defpanel ,name ,lambda-list ,@sections))))
      ((string= directive-name "APP")
       (destructuring-bind (directive name options &rest body) form
         (declare (ignore directive))
         (unless (symbolp name)
           (%loader-error path index form
                          "Declarative :app requires a symbol name."))
         (unless (listp options)
           (%loader-error path index form
                          "Declarative :app requires an option plist list."))
         (list `(ptui.ui.app:defapp ,name ,options ,@body))))
      ((string= directive-name "WIDGET")
       (destructuring-bind (directive name lambda-list &rest widget-body) form
         (declare (ignore directive))
         (unless (symbolp name)
           (%loader-error path index form
                          "Declarative :widget requires a symbol name."))
         (unless (listp lambda-list)
           (%loader-error path index form
                          "Declarative :widget requires a lambda-list list."))
         (list `(ptui.widgets.defwidget:defwidget ,name ,lambda-list
                  ,@widget-body))))
      ((string= directive-name "BREADCRUMBS")
       (%translate-breadcrumbs-directive form path index))
      ((string= directive-name "PROJECT-TREE")
       (%translate-project-tree-directive form path index))
      (t
       (%loader-error path index form
                      (format nil
                              "Unknown declarative directive :~A. Supported directives: :ptui, :panel, :app, :widget, :breadcrumbs, :project-tree, :defpackage, :in-package."
                              (string-downcase directive-name)))))))

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
  (%expand-declarative-form form path index))

(defun translate-packet-forms (forms &key (path "<memory>"))
  "Translate packet FORMS into executable defwidget/defpanel/defapp forms."
  (let ((expanded '()))
    (loop for form in forms
          for index from 1
          do (setf expanded
                   (nconc expanded
                          (translate-packet-form form :path path :index index))))
    expanded))

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
  (let* ((expanded-forms (expand-definition-forms forms :path path))
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
