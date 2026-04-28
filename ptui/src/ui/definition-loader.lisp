(in-package :ptui.ui.definition-loader)

;;;; Public orchestration entry points.
;;;;
;;;; The implementation has been split into:
;;;;   definition-loader/schema.lisp    — package, conditions, structs, schema table.
;;;;   definition-loader/parse.lisp     — declarative directive parsing into IR units.
;;;;   definition-loader/validate.lisp  — packet- and top-level-form validation.
;;;;   definition-loader/resolve.lisp   — IR-to-Lisp compilation and package rehoming.
;;;;
;;;; This residual file holds only the cross-cluster orchestration glue
;;;; (load-definition-forms, load-definition-file, run-loaded-app,
;;;; load-and-run-definition-file) that combines validate, parse, resolve,
;;;; and registration verification into one user-facing pipeline.

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
