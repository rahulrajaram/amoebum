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

(defun %golden-packet-path (filename)
  (merge-pathnames (format nil "examples/golden-packets/~A" filename)
                   (asdf:system-source-directory :ptui)))

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

(test translate-packet-forms-translates-pattern-directives-with-stable-symbols
  (let* ((forms
           '((:ptui
              (:breadcrumbs breadcrumb-line (segments))
              (:project-tree project-tree-list (rows selected-index) :height 12 :marker "*")
              (:panel packet-panel (segments rows selected-index)
               (:layout
                (:column
                 (breadcrumbs :fixed 1 (breadcrumb-line segments))
                 (tree :flex 1 (project-tree-list rows selected-index)))))
              (:app packet-app (:fps 5)
               (packet-panel (list "a" "b")
                             (list "src/" "test/")
                             0)))))
         (translated (ptui.ui.definition-loader:translate-packet-forms forms :path "<test>")))
    (is (= (length translated) 4))
    (is (string= (symbol-name (caar translated)) "DEFWIDGET"))
    (is (eq (second (first translated)) 'breadcrumb-line))
    (is (string= (symbol-name (car (second translated))) "DEFWIDGET"))
    (is (eq (second (second translated)) 'project-tree-list))
    (is (string= (symbol-name (car (third translated))) "DEFPANEL"))
    (is (eq (second (third translated)) 'packet-panel))
    (is (string= (symbol-name (car (fourth translated))) "DEFAPP"))
    (is (eq (second (fourth translated)) 'packet-app))))

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

(test load-definition-forms-registers-pattern-generated-widgets
  (let* ((breadcrumb 'pattern-breadcrumb-widget)
         (tree 'pattern-tree-widget)
         (panel 'pattern-panel)
         (app 'pattern-app)
         (result
           (ptui.ui.definition-loader:load-definition-forms
            `((:breadcrumbs ,breadcrumb (segments) :prefix "Path: ")
              (:project-tree ,tree (rows selected-index) :height 8)
              (:panel ,panel (segments rows selected-index)
               (:layout
                (:column
                 (header :fixed 1 (,breadcrumb segments))
                 (body :flex 1 (,tree rows selected-index)))))
              (:app ,app (:fps 4)
               (,panel
                (list "amoebum" "ptui")
                (list "src/" "test/")
                1)))
            :path "<memory>")))
    (is (= (ptui.ui.definition-loader:definition-load-result-form-count result) 4))
    (is (member panel (ptui.ui.definition-loader:definition-load-result-panel-names result)
                :test #'eq))
    (is (member breadcrumb (ptui.ui.definition-loader:definition-load-result-widget-names result)
                :test #'eq))
    (is (member tree (ptui.ui.definition-loader:definition-load-result-widget-names result)
                :test #'eq))
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

(test validate-packet-forms-accepts-golden-g1-and-g2
  (dolist (fixture '("g1-status-strip.lisp"
                     "g2-project-tree-breadcrumbs.lisp"))
    (let* ((path (%golden-packet-path fixture))
           (forms (ptui.ui.definition-loader:read-definition-forms path)))
      (is-true (ptui.ui.definition-loader:validate-packet-forms forms :path path)))))

(test validate-packet-forms-rejects-golden-g3-unknown-directive
  (let* ((path (%golden-packet-path "g3-invalid-unknown-directive.lisp"))
         (forms (ptui.ui.definition-loader:read-definition-forms path))
         (error-signaled nil)
         (detail nil))
    (handler-case
        (ptui.ui.definition-loader:validate-packet-forms forms :path path)
      (ptui.ui.definition-loader:definition-loader-error (condition)
        (setf error-signaled t
              detail (ptui.ui.definition-loader:definition-loader-error-detail condition))))
    (is-true error-signaled)
    (is (not (null detail)))
    (is (not (null (search "Unknown packet directive :pattern" detail :test #'char-equal))))))

(test validate-packet-form-rejects-malformed-project-tree-options
  (let ((detail nil))
    (handler-case
        (ptui.ui.definition-loader:validate-packet-form
         '(:project-tree bad-tree (rows selected-index) :height 0)
         :path "<test>"
         :index 1)
      (ptui.ui.definition-loader:definition-loader-error (condition)
        (setf detail (ptui.ui.definition-loader:definition-loader-error-detail condition))))
    (is (not (null detail)))
    (is (not (null (search ":height must be a positive integer" detail :test #'char-equal))))))

(defun run-all ()
  (run! 'definition-loader-suite))
