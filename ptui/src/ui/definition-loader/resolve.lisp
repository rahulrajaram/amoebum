(in-package :ptui.ui.definition-loader)

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
