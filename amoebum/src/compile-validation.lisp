(in-package :amoebum)

(define-condition tool-definition-warning (style-warning)
  ((tool-name :initarg :tool-name
              :initform nil
              :reader tool-definition-warning-tool-name)
   (parameter :initarg :parameter
              :initform nil
              :reader tool-definition-warning-parameter)
   (reason :initarg :reason
           :initform nil
           :reader tool-definition-warning-reason))
  (:report
   (lambda (condition stream)
     (let ((tool (tool-definition-warning-tool-name condition))
           (parameter (tool-definition-warning-parameter condition))
           (reason (tool-definition-warning-reason condition)))
       (cond
         ((and tool parameter reason)
          (format stream "Tool ~S parameter ~S: ~A." tool parameter reason))
         ((and tool reason)
          (format stream "Tool ~S: ~A." tool reason))
         (reason
          (format stream "~A." reason))
         (t
          (format stream "Tool definition warning.")))))))

(define-condition unmapped-type-warning (tool-definition-warning)
  ((type-spec :initarg :type-spec
              :reader unmapped-type-warning-type-spec))
  (:report
   (lambda (condition stream)
     (format stream
             "Tool ~S parameter ~S uses type specifier ~S that does not map cleanly to JSON schema."
             (tool-definition-warning-tool-name condition)
             (tool-definition-warning-parameter condition)
             (unmapped-type-warning-type-spec condition)))))

(define-condition missing-tool-description (tool-definition-warning)
  ()
  (:report
   (lambda (condition stream)
     (let ((tool (tool-definition-warning-tool-name condition))
           (parameter (tool-definition-warning-parameter condition)))
       (if parameter
           (format stream "Tool ~S parameter ~S is missing a description."
                   tool
                   parameter)
           (format stream "Tool ~S is missing a non-empty description."
                   tool))))))

(define-condition duplicate-tool-name (tool-definition-warning)
  ()
  (:report
   (lambda (condition stream)
     (format stream
             "Duplicate tool name ~S seen during macroexpansion."
             (tool-definition-warning-tool-name condition)))))

(define-condition dangerous-auto-permission (tool-definition-warning)
  ()
  (:report
   (lambda (condition stream)
     (format stream
             "Tool ~S is marked dangerous but uses :permission :auto."
             (tool-definition-warning-tool-name condition)))))

(define-condition unknown-tool-reference (tool-definition-warning)
  ((hook-point :initarg :hook-point
               :initform nil
               :reader unknown-tool-reference-hook-point)
   (reference :initarg :reference
              :initform nil
              :reader unknown-tool-reference-reference))
  (:report
   (lambda (condition stream)
     (let ((hook-point (unknown-tool-reference-hook-point condition))
           (reference (unknown-tool-reference-reference condition)))
       (if hook-point
           (format stream
                   "DEFHOOK ~S references unknown tool ~S."
                   hook-point
                   reference)
           (format stream "Unknown tool reference ~S." reference))))))

(define-condition invalid-permission-mode (error)
  ((tool-name :initarg :tool-name
              :initform nil
              :reader invalid-permission-mode-tool-name)
   (permission :initarg :permission
               :reader invalid-permission-mode-permission)
   (allowed-values :initarg :allowed-values
                   :initform '(:auto :supervised :full-auto)
                   :reader invalid-permission-mode-allowed-values))
  (:report
   (lambda (condition stream)
     (let ((tool (invalid-permission-mode-tool-name condition)))
       (if tool
           (format stream
                   "Tool ~S uses invalid permission mode ~S; expected one of ~S."
                   tool
                   (invalid-permission-mode-permission condition)
                   (invalid-permission-mode-allowed-values condition))
           (format stream
                   "Invalid permission mode ~S; expected one of ~S."
                   (invalid-permission-mode-permission condition)
                   (invalid-permission-mode-allowed-values condition)))))))

(defstruct (macro-lint-issue
            (:constructor make-macro-lint-issue
                (&key severity file macro-name message)))
  severity
  file
  macro-name
  message)

(defun %macro-lint-path-text (path)
  (cond
    ((pathnamep path) (namestring path))
    ((null path) "")
    (t (princ-to-string path))))

(defun %macro-lint-extension-p (pathname)
  (string-equal (or (pathname-type pathname) "") "lisp"))

(defun %collect-lisp-files-recursive (directory)
  (let ((root (uiop:ensure-directory-pathname directory))
        (files '()))
    (labels ((walk (path)
               (dolist (file (or (ignore-errors (uiop:directory-files path)) '()))
                 (when (%macro-lint-extension-p file)
                   (push file files)))
               (dolist (subdir (or (ignore-errors (uiop:subdirectories path)) '()))
                 (walk subdir))))
      (walk root))
    (sort files #'string< :key #'%macro-lint-path-text)))

(defun %default-macro-lint-files ()
  (let* ((amoebum-root
           (uiop:ensure-directory-pathname (asdf:system-source-directory "amoebum")))
         (repo-root (uiop:ensure-directory-pathname (merge-pathnames #P"../" amoebum-root)))
         (amoebum-src (merge-pathnames #P"amoebum/src/" repo-root))
         (ptui-src (merge-pathnames #P"ptui/src/widgets/" repo-root)))
    (append (%collect-lisp-files-recursive amoebum-src)
            (%collect-lisp-files-recursive ptui-src))))

(defun %macro-form-kind (form)
  (when (consp form)
    (let ((head (first form)))
      (when (symbolp head)
        (let ((name (string-upcase (symbol-name head))))
          (cond
            ((string= name "DEFTOOL") :deftool)
            ((string= name "DEFHOOK") :defhook)
            ((string= name "DEFWIDGET") :defwidget)
            (t nil)))))))

(defun %in-package-target (form)
  (when (and (consp form)
             (symbolp (first form))
             (string= (string-upcase (symbol-name (first form))) "IN-PACKAGE")
             (= (length form) 2))
    (let ((designator (second form)))
      (or (and (packagep designator) designator)
          (and (symbolp designator) (or (find-package designator)
                                        (find-package (symbol-name designator))))
          (and (stringp designator) (find-package designator))
          nil))))

(defun %lint-file-macro-forms (pathname)
  (let ((current-package (or (find-package :cl-user) *package*))
        (form-count 0)
        (issues '()))
    (labels ((record-warning (macro-kind condition)
               (push (make-macro-lint-issue
                      :severity :warning
                      :file pathname
                      :macro-name macro-kind
                      :message (princ-to-string condition))
                     issues))
             (record-error (macro-kind condition)
               (push (make-macro-lint-issue
                      :severity :error
                      :file pathname
                      :macro-name macro-kind
                      :message (princ-to-string condition))
                     issues))
             (expand-form (form macro-kind)
               (handler-bind
                   ((warning
                      (lambda (condition)
                        (record-warning macro-kind condition)
                        (let ((restart (find-restart 'muffle-warning condition)))
                          (when restart
                            (invoke-restart restart))))))
                 (let ((*package* current-package))
                   (handler-case
                       (macroexpand-1 form)
                     (error (condition)
                       (record-error macro-kind condition)))))))
      (handler-case
          (with-open-file (stream pathname :direction :input :external-format :utf-8)
            (loop
              with eof = (gensym "EOF")
              for form = (let ((*read-eval* nil)
                               (*package* current-package))
                           (read stream nil eof))
              until (eq form eof)
              do (let ((next-package (%in-package-target form)))
                   (when next-package
                     (setf current-package next-package)))
                 (let ((macro-kind (%macro-form-kind form)))
                   (when macro-kind
                     (incf form-count)
                     (expand-form form macro-kind)))))
        (error (condition)
          (record-error :reader (format nil "Unable to read file: ~A" condition))))
      (values form-count (nreverse issues)))))

(defun %normalize-lint-paths (paths)
  (if (null paths)
      (%default-macro-lint-files)
      (let ((out '()))
        (dolist (entry paths (nreverse out))
          (let* ((as-path (pathname entry))
                 (resolved (or (ignore-errors (truename as-path)) as-path)))
            (cond
              ((uiop:directory-pathname-p resolved)
               (setf out (nconc (reverse (%collect-lisp-files-recursive resolved)) out)))
              ((%macro-lint-extension-p resolved)
               (push resolved out))))))))

(defun run-macro-lint (&key paths)
  (let ((issues '())
        (macro-form-count 0)
        (files (%normalize-lint-paths paths)))
    (when (fboundp 'reset-deftool-compile-validation-state)
      (reset-deftool-compile-validation-state))
    (with-compilation-unit ()
      (dolist (file files)
        (multiple-value-bind (file-form-count file-issues)
            (%lint-file-macro-forms file)
          (incf macro-form-count file-form-count)
          (setf issues (nconc issues file-issues)))))
    (let* ((ordered-issues issues)
           (warning-count (count :warning ordered-issues
                                 :key #'macro-lint-issue-severity :test #'eq))
           (error-count (count :error ordered-issues
                               :key #'macro-lint-issue-severity :test #'eq)))
      (list :ok-p (zerop (+ warning-count error-count))
            :file-count (length files)
            :macro-form-count macro-form-count
            :warning-count warning-count
            :error-count error-count
            :issues ordered-issues))))

(defun %macro-lint-issue-line (issue)
  (format nil "- [~A] ~A (~A): ~A"
          (string-downcase (symbol-name (macro-lint-issue-severity issue)))
          (%macro-lint-path-text (macro-lint-issue-file issue))
          (string-downcase (symbol-name (macro-lint-issue-macro-name issue)))
          (macro-lint-issue-message issue)))

(defun format-macro-lint-report (report)
  (let ((issues (getf report :issues))
        (file-count (getf report :file-count 0))
        (macro-form-count (getf report :macro-form-count 0))
        (warning-count (getf report :warning-count 0))
        (error-count (getf report :error-count 0)))
    (if (null issues)
        (format nil
                "Macro lint clean: files=~D macro-forms=~D warnings=0 errors=0."
                file-count
                macro-form-count)
        (with-output-to-string (stream)
          (format stream
                  "Macro lint found ~D issue~:P across ~D files (~D macro forms): warnings=~D errors=~D.~%"
                  (+ warning-count error-count)
                  file-count
                  macro-form-count
                  warning-count
                  error-count)
          (dolist (issue issues)
            (format stream "~A~%"
                    (%macro-lint-issue-line issue)))))))
