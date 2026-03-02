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
   #:read-definition-forms
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
      (t
       (%loader-error path index form
                      (format nil
                              "Unknown declarative directive :~A. Supported directives: :ptui, :panel, :app, :widget, :defpackage, :in-package."
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

(defun expand-definition-forms (forms &key (path "<memory>"))
  "Expand declarative PTUI directives into executable Lisp forms."
  (let ((expanded '()))
    (loop for form in forms
          for index from 1
          do (setf expanded
                   (nconc expanded
                          (%expand-declarative-form form path index))))
    expanded))

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
                                    source-package)
  "Load FORMS into the current image and verify panel/app registration.
FORMS may include declarative directives such as (:panel ...), (:app ...),
and wrappers like (:ptui ...)."
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
                                  (allowed-operators +default-allowed-top-level-operators+))
  "Load a PTUI definition file from PATH."
  (let ((forms (read-definition-forms path))
        (source-package *package*))
    (load-definition-forms forms
                           :path path
                           :package package
                           :validate validate
                           :allowed-operators allowed-operators
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
                                          override-backend)
  "Load PATH and run APP-NAME in one call."
  (load-definition-file path
                        :package package
                        :validate validate
                        :allowed-operators allowed-operators)
  (run-loaded-app app-name :override-backend override-backend))
