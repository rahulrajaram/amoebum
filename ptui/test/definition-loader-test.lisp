(defpackage :ptui.test.definition-loader
  (:use :cl :fiveam)
  (:export #:run-all #:definition-loader-suite))

(in-package :ptui.test.definition-loader)

(def-suite definition-loader-suite
  :description "PTUI definition loader parsing/validation/registration tests.")

(in-suite definition-loader-suite)

(defun %write-forms-to-temp-file (forms)
  (let* ((name (format nil "ptui-definition-loader-~A.lisp" (gensym)))
         (path (merge-pathnames name (uiop:temporary-directory))))
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (dolist (form forms)
        (write form :stream stream)
        (terpri stream)))
    path))

(defun %write-source-to-temp-file (source)
  (let* ((name (format nil "ptui-definition-loader-source-~A.lisp" (gensym)))
         (path (merge-pathnames name (uiop:temporary-directory))))
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string source stream)
      (terpri stream))
    path))

(test expand-definition-forms-translates-declarative-directives
  (let* ((forms
           '((:ptui
              (:panel sample-panel (title)
               (:layout
                (:column
                 (main :flex 1
                  (ptui.widgets.core:make-text-widget title)))))
              (:app sample-app (:fps 4)
               (sample-panel "hello")))))
         (expanded (ptui.ui.definition-loader:expand-definition-forms forms)))
    (is (= (length expanded) 2))
    (is (string= (symbol-name (caar expanded)) "DEFPANEL"))
    (is (string= (symbol-name (caadr expanded)) "DEFAPP"))))

(test validate-definition-form-rejects-non-allowlisted-ops
  (signals ptui.ui.definition-loader:definition-loader-error
    (ptui.ui.definition-loader:validate-definition-form
     '(run-shell-command "rm -rf /")
     :path "<test>"
     :index 1)))

(test load-definition-forms-registers-panel-and-app
  (let* ((panel 'loader-test-panel)
         (app 'loader-test-app)
         (result
           (ptui.ui.definition-loader:load-definition-forms
            `((:panel ,panel (title)
               (:layout
                (:column
                 (main :flex 1
                  (ptui.widgets.core:make-text-widget title)))))
              (:app ,app (:fps 3)
               (,panel "loaded by declarative forms")))
            :path "<memory>")))
    (is (= (ptui.ui.definition-loader:definition-load-result-form-count result) 2))
    (is (member panel (ptui.ui.definition-loader:definition-load-result-panel-names result)
                :test #'eq))
    (is (not (null (ptui.widgets.defwidget:find-widget panel))))
    (is (fboundp (ptui.ui.definition-loader:app-runner-symbol app)))))

(test load-definition-file-registers-package-scoped-symbols
  (let* ((suffix (symbol-name (gensym "DEF-FILE-")))
         (package-name (format nil "PTUI.TEST.DEFINITION.FILE.~A" suffix))
         (panel-name (string-upcase (format nil "FILE-PANEL-~A" suffix)))
         (app-name (string-upcase (format nil "FILE-APP-~A" suffix)))
         (source
           (format nil
                   "(:ptui~%
  (:defpackage ~S (:use :cl))~%
  (:in-package ~S)~%
  (:panel ~A (title)~%
    (:layout (:column (main :flex 1 (ptui.widgets.core:make-text-widget title)))))~%
  (:app ~A (:fps 2)~%
    (~A \"from file\")))~%"
                   package-name
                   package-name
                   panel-name
                   app-name
                   panel-name))
         (path (%write-source-to-temp-file source)))
    (unwind-protect
         (let* ((result (ptui.ui.definition-loader:load-definition-file path))
                (pkg (find-package package-name))
                (panel-sym (and pkg (find-symbol panel-name pkg)))
                (app-sym (and pkg (find-symbol app-name pkg))))
           (is (= (ptui.ui.definition-loader:definition-load-result-form-count result) 4))
           (is (not (null pkg)))
           (is (symbolp panel-sym))
           (is (symbolp app-sym))
           (is (not (null (ptui.widgets.defwidget:find-widget panel-sym))))
           (is (fboundp (ptui.ui.definition-loader:app-runner-symbol app-sym))))
      (ignore-errors (delete-file path)))))

(defun run-all ()
  (run! 'definition-loader-suite))
