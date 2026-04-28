(in-package :ptui.ui.definition-loader)

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
