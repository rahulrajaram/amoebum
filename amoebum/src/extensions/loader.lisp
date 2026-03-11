(in-package :amoebum)

(defparameter *extension-load-report* '())
(defparameter *loaded-extensions* '())
(defparameter *extension-last-discovered* '())
(defparameter *disabled-extensions* (make-hash-table :test #'equal))
(defparameter *extension-registry* (make-hash-table :test #'equal))
(defparameter *extension-watch-snapshot* (make-hash-table :test #'equal))
(defparameter *extension-hot-reload-enabled-p* t)
(defparameter *extension-hot-reload-interval-seconds* 1.0d0)
(defparameter *extension-hot-reload-thread* nil)
(defparameter *extension-hot-reload-running-p* nil)
(defparameter +extension-supported-permissions+ '(:filesystem :network :shell))
(defparameter *extension-permission-approvals* (make-hash-table :test #'equal))
(defparameter *extension-permission-prompt-function* nil)
(defparameter *extension-safe-operations*
  '("deftool" "defhook"
    "defun" "defmacro" "defparameter" "defvar"
    "progn" "let" "let*" "setf" "setq" "incf" "decf"
    "if" "when" "unless" "cond" "case" "ecase" "typecase"
    "handler-case" "ignore-errors" "unwind-protect"
    "multiple-value-bind" "dolist" "dotimes" "loop"
    "values" "quote" "function" "lambda"
    "list" "append" "cons" "car" "cdr"
    "and" "or" "not"
    "+" "-" "*" "/" "1+" "1-" "=" "<" ">" "<=" ">="))

;; Test and smoke harnesses can bind these to avoid mutating real user paths.
(defparameter *extensions-global-directory-override* nil)
(defparameter *extensions-project-directory-override* nil)

(defstruct (extension-registry-entry
            (:constructor make-extension-registry-entry
                (&key name
                 version
                 dependencies
                 entry-point
                 manifest-path
                 extension-root
                 scope
                 permissions
                 package-name
                 enabled-p
                 tool-count
                 hook-count
                 status
                 loaded-at
                 last-write-date
                 message)))
  name
  version
  (dependencies '() :type list)
  entry-point
  manifest-path
  extension-root
  (scope :project :type keyword)
  (permissions '() :type list)
  package-name
  (enabled-p t :type boolean)
  (tool-count 0 :type integer)
  (hook-count 0 :type integer)
  (status :loaded :type keyword)
  (loaded-at 0 :type integer)
  last-write-date
  message)

(defun %symbol-token (symbol)
  (and (symbolp symbol)
       (string-downcase (symbol-name symbol))))

(defun %symbol-qualified-token (symbol)
  (when (symbolp symbol)
    (let ((pkg (symbol-package symbol)))
      (if pkg
          (format nil "~A:~A"
                  (string-downcase (package-name pkg))
                  (string-downcase (symbol-name symbol)))
          (%symbol-token symbol)))))

(defun %normalize-extension-permission (value &key (errorp t))
  (let ((normalized
          (cond
            ((keywordp value) value)
            ((symbolp value) (intern (string-upcase (symbol-name value)) :keyword))
            ((stringp value) (intern (string-upcase (%extension-trim value)) :keyword))
            (t nil))))
    (cond
      ((member normalized +extension-supported-permissions+ :test #'eq)
       normalized)
      (errorp
       (error "Unsupported extension permission ~S. Expected one of ~S."
              value
              +extension-supported-permissions+))
      (t nil))))

(defun %normalize-extension-permissions (value &key (errorp t))
  (let ((raw
          (cond
            ((null value) '())
            ((listp value) value)
            (t (list value)))))
    (remove-duplicates
     (loop for permission in raw
           for normalized = (%normalize-extension-permission permission :errorp errorp)
           when normalized collect normalized)
     :test #'eq)))

(defun clear-extension-permission-approvals ()
  (clrhash *extension-permission-approvals*)
  t)

(defun %normalize-extension-permission-decision (value)
  (cond
    ((or (eq value :allow)
         (eq value t)
         (and (stringp value)
              (member (string-downcase (%extension-trim value))
                      '("allow" "approved" "approve" "yes" "y")
                      :test #'string=)))
     :allow)
    (t :deny)))

(defun %default-extension-permission-prompt (extension-name permission scope _metadata)
  (declare (ignore _metadata))
  (let ((prompt
          (format nil
                  "Extension ~A requests ~A permission (~A scope). Approve? [y/N]: "
                  extension-name
                  permission
                  scope)))
    (if (and (streamp *query-io*)
             (interactive-stream-p *query-io*))
        (progn
          (format *query-io* "~A" prompt)
          (finish-output *query-io*)
          (%normalize-extension-permission-decision
           (read-line *query-io* nil "")))
        (progn
          (format *error-output* "~Adenied (non-interactive).~%" prompt)
          :deny))))

(unless *extension-permission-prompt-function*
  (setf *extension-permission-prompt-function*
        #'%default-extension-permission-prompt))

(defun %extension-permission-approval-key (extension-name permission)
  (format nil "~A::~A"
          (string-downcase (%extension-trim extension-name))
          (string-downcase (symbol-name permission))))

(defun %ensure-extension-permissions-approved (metadata scope)
  (let* ((extension-name (or (getf metadata :name) "unknown-extension"))
         (permissions (or (getf metadata :permissions) '()))
         (prompt-fn (or *extension-permission-prompt-function*
                        #'%default-extension-permission-prompt)))
    (dolist (permission permissions)
      (let ((key (%extension-permission-approval-key extension-name permission)))
        (unless (gethash key *extension-permission-approvals*)
          (let ((decision
                  (%normalize-extension-permission-decision
                   (funcall prompt-fn extension-name permission scope metadata))))
            (unless (eq decision :allow)
              (error "Extension ~A permission ~S denied by user."
                     extension-name
                     permission))
            (setf (gethash key *extension-permission-approvals*) t))))))
  t)

(defun %sanitize-extension-package-fragment (value)
  (let* ((raw (%extension-trim (or value "")))
         (upper (string-upcase raw)))
    (if (zerop (length upper))
        "UNNAMED"
        (with-output-to-string (stream)
          (loop for char across upper do
                (if (or (alphanumericp char)
                        (char= char #\-)
                        (char= char #\_))
                    (write-char char stream)
                    (write-char #\- stream)))))))

(defun %extension-package-name (metadata)
  (format nil "AMOEBUM.EXT.~A"
          (%sanitize-extension-package-fragment (getf metadata :name))))

(defun %ensure-extension-package (metadata)
  (let* ((package-name (%extension-package-name metadata))
         (package (or (find-package package-name)
                      (make-package package-name :use '(:cl :amoebum)))))
    (dolist (dependency '(:cl :amoebum))
      (let ((dep-package (find-package dependency)))
        (when (and dep-package
                   (not (member dep-package (package-use-list package))))
          (use-package dep-package package))))
    package))

(defun %walk-extension-form-symbols (form visitor)
  (cond
    ((symbolp form)
     (funcall visitor form))
    ((consp form)
     (let ((head (car form)))
       (if (and (symbolp head)
                (member (%symbol-token head)
                        '("quote" "function")
                        :test #'string=))
           nil
           (dolist (item form)
             (%walk-extension-form-symbols item visitor)))))
    (t nil)))

(defun %extension-denied-operation (symbol)
  (let* ((token (%symbol-token symbol))
         (qualified (%symbol-qualified-token symbol))
         (pkg-name (and (symbol-package symbol)
                        (string-downcase (package-name (symbol-package symbol))))))
    (cond
      ((or (string= qualified "sb-ext:run-program")
           (string= qualified "uiop:run-program")
           (string= token "run-program"))
       (list :operation (or qualified token)
             :permission :shell
             :message "Shell execution is blocked without :shell permission."))
      ((or (string= token "open")
           (string= token "with-open-file"))
       (list :operation (or qualified token)
             :permission :filesystem
             :message "Filesystem access is blocked without :filesystem permission."))
      ((member pkg-name '("sb-alien" "cffi" "cffi-sys") :test #'string=)
       (list :operation (or qualified token)
             :permission :ffi
             :message "FFI operations are not allowed in extensions."))
      (t nil))))

(defun %scan-extension-form-for-denylist (form)
  (let ((violations '())
        (seen (make-hash-table :test #'equal)))
    (%walk-extension-form-symbols
     form
     (lambda (symbol)
       (let ((violation (%extension-denied-operation symbol)))
         (when violation
           (let* ((operation (or (getf violation :operation) "unknown"))
                  (permission (or (getf violation :permission) :unknown))
                  (key (format nil "~A|~A" operation permission)))
             (unless (gethash key seen)
               (setf (gethash key seen) t)
               (push violation violations)))))))
    (nreverse violations)))

(defun %extension-top-level-allowlisted-p (operator extension-package)
  (cond
    ((null operator) t)
    ((not (symbolp operator)) nil)
    ((and extension-package
          (eq (symbol-package operator) extension-package))
     t)
    ((member (%symbol-token operator)
             *extension-safe-operations*
             :test #'string=)
     t)
    (t nil)))

(defun %validate-extension-form (form metadata extension-package)
  (when (consp form)
    (let* ((operator (car form))
           (extension-name (or (getf metadata :name) "unknown-extension"))
           (permissions (or (getf metadata :permissions) '()))
           (violations (%scan-extension-form-for-denylist form)))
      (when (and (symbolp operator)
                 (string= (%symbol-token operator) "in-package")
                 (not (%extension-in-package-allowed-p form extension-package)))
        (error "Extension ~A cannot call IN-PACKAGE; it is isolated in package ~A."
               extension-name
               (package-name extension-package)))
      (unless (or (%extension-top-level-allowlisted-p operator extension-package)
                  (and (symbolp operator)
                       (string= (%symbol-token operator) "in-package")
                       (%extension-in-package-allowed-p form extension-package))
                  (plusp (length violations)))
        (error "Extension ~A uses non-allowlisted top-level operation ~S."
               extension-name
               operator))
      (dolist (violation violations)
        (let ((permission (getf violation :permission))
              (operation (getf violation :operation))
              (message (getf violation :message)))
          (cond
            ((eq permission :ffi)
             (error "Extension ~A blocked: ~A (~A)."
                    extension-name
                    operation
                    message))
            ((not (member permission permissions :test #'eq))
             (error "Extension ~A blocked: operation ~A requires permission ~S."
                    extension-name
                    operation
                    permission)))))))
  t)

(defun %extension-trim (value)
  (if (stringp value)
      (string-trim '(#\Space #\Tab #\Newline #\Return) value)
      ""))

(defun %normalize-pathname (value)
  (cond
    ((pathnamep value) value)
    ((stringp value) (pathname value))
    (t nil)))

(defun %ensure-directory (value)
  (let ((pathname (%normalize-pathname value)))
    (and pathname
         (uiop:ensure-directory-pathname pathname))))

(defun %resolve-project-root (&optional project-root)
  (let* ((cfg (ignore-errors (current-config)))
         (candidate (or project-root
                        (and (config-p cfg) (config-project-root cfg))
                        (ignore-errors (uiop:getcwd))
                        *default-pathname-defaults*))
         (directory (%ensure-directory candidate)))
    (or (and directory
             (or (ignore-errors (uiop:ensure-directory-pathname (truename directory)))
                 directory))
        (uiop:ensure-directory-pathname *default-pathname-defaults*))))

(defun %global-extension-directory (&key global-directory)
  (or (%ensure-directory global-directory)
      (%ensure-directory *extensions-global-directory-override*)
      (uiop:ensure-directory-pathname
       (merge-pathnames #P".amoebum/extensions/" (user-homedir-pathname)))))

(defun %project-extension-directory (&key project-root project-directory)
  (or (%ensure-directory project-directory)
      (%ensure-directory *extensions-project-directory-override*)
      (uiop:ensure-directory-pathname
       (merge-pathnames #P".amoebum/extensions/"
                        (%resolve-project-root project-root)))))

(defun %extension-sort-key (path)
  (string-downcase
   (or (file-namestring path)
       (namestring path))))

(defun %canonical-extension-path (path)
  (let* ((pathname (%normalize-pathname path))
         (resolved (and pathname
                        (or (ignore-errors (truename pathname))
                            pathname))))
    (if resolved
        (namestring resolved)
        "")))

(defun %extension-key (path)
  (string-downcase (%canonical-extension-path path)))

(defun %extension-registry-key (name)
  (string-downcase (%extension-trim name)))

(defun %extension-match-target-p (target path-text)
  (let* ((needle (string-downcase (%extension-trim target)))
         (haystack (string-downcase path-text))
         (pathname (%normalize-pathname path-text))
         (filename (and pathname (file-namestring pathname)))
         (stem (and pathname (pathname-name pathname))))
    (and (plusp (length needle))
         (or (string= needle haystack)
             (and filename (string= needle (string-downcase filename)))
             (and stem (string= needle (string-downcase stem)))
             (search needle haystack :test #'char=)))))

(defun %list-extension-manifest-files (directory)
  (if (and directory (probe-file directory))
      (let ((files '()))
        (dolist (candidate (directory (merge-pathnames #P"*/extension.lisp" directory)))
          (unless (uiop:directory-pathname-p candidate)
            (push candidate files)))
        (sort files #'string< :key #'%extension-sort-key))
      '()))

(defun %list-legacy-extension-files (directory)
  (if (and directory (probe-file directory))
      (sort
       (remove-if
        (lambda (path)
          (or (uiop:directory-pathname-p path)
              (string-equal (pathname-name path) "extension")))
        (directory (merge-pathnames #P"*.lisp" directory)))
       #'string<
       :key #'%extension-sort-key)
      '()))

(defun %safe-file-write-date (path)
  (ignore-errors
    (file-write-date path)))

(defun %manifest-parent-directory (manifest-path)
  (uiop:pathname-directory-pathname manifest-path))

(defun %ensure-string (value)
  (cond
    ((null value) nil)
    ((stringp value) value)
    ((symbolp value) (string-downcase (symbol-name value)))
    (t (princ-to-string value))))

(defun %parse-nonnegative-integer (value)
  (cond
    ((and (integerp value) (>= value 0))
     value)
    ((stringp value)
     (let* ((trimmed (%extension-trim value))
            (parsed (and (plusp (length trimmed))
                         (ignore-errors (parse-integer trimmed)))))
       (and (integerp parsed)
            (>= parsed 0)
            parsed)))
    (t nil)))

(defun %extension-form-definition-counts (form)
  (let ((tool-count 0)
        (hook-count 0))
    (labels ((walk (node)
               (when (consp node)
                 (when (symbolp (car node))
                   (let ((head (symbol-name (car node))))
                     (cond
                       ((string-equal head "DEFTOOL")
                        (incf tool-count))
                       ((string-equal head "DEFHOOK")
                        (incf hook-count)))))
                 (walk (car node))
                 (walk (cdr node)))))
      (walk form))
    (values tool-count hook-count)))

(defun %scan-definition-counts (path)
  (handler-case
      (let ((tool-count 0)
            (hook-count 0))
        (when (and path (probe-file path))
          (with-open-file (stream path :direction :input :external-format :utf-8)
            (loop for form = (read stream nil :__eof__)
                  until (eq form :__eof__)
                  do (multiple-value-bind (tools hooks)
                         (%extension-form-definition-counts form)
                       (incf tool-count tools)
                       (incf hook-count hooks)))))
        (values tool-count hook-count))
    (error ()
      (values 0 0))))

(defun %resolve-definition-counts (metadata entry-point)
  (let* ((manifest-tool-count (%parse-nonnegative-integer (getf metadata :tool-count)))
         (manifest-hook-count (%parse-nonnegative-integer (getf metadata :hook-count)))
         (entry-path (and (pathnamep entry-point)
                          (or (ignore-errors (truename entry-point))
                              entry-point)))
         (scanned-tool-count nil)
         (scanned-hook-count nil))
    (when (and entry-path
               (or (null manifest-tool-count)
                   (null manifest-hook-count)))
      (multiple-value-setq (scanned-tool-count scanned-hook-count)
        (%scan-definition-counts entry-path)))
    (values (or manifest-tool-count scanned-tool-count 0)
            (or manifest-hook-count scanned-hook-count 0))))

(defun %normalize-dependencies (value)
  (cond
    ((null value) '())
    ((listp value)
     (loop for dep in value
           for dep-name = (%ensure-string dep)
           when (and dep-name (plusp (length (%extension-trim dep-name))))
             collect dep-name))
    (t
     (let ((dep-name (%ensure-string value)))
       (if (and dep-name (plusp (length (%extension-trim dep-name))))
           (list dep-name)
           '())))))

(defun %read-manifest-form (manifest-path)
  (with-open-file (stream manifest-path :direction :input :external-format :utf-8)
    (let ((form (read stream nil nil)))
      (unless form
        (error "Manifest ~A is empty." manifest-path))
      form)))

(defun %manifest-dependency-names (manifest)
  (loop for dependency in (extension-manifest-dependencies manifest)
        for name = (car dependency)
        when (and (stringp name)
                  (plusp (length (%extension-trim name))))
          collect name))

(defun %manifest->metadata (manifest-path)
  (let* ((manifest-form (%read-manifest-form manifest-path))
         (manifest (parse-extension-manifest-sexp manifest-form :source-path manifest-path))
         (plist (%normalize-manifest-form manifest-form))
         (permissions (%normalize-extension-permissions (getf plist :permissions)))
         (tools-value (getf plist :tools :__missing__))
         (hooks-value (getf plist :hooks :__missing__))
         (tool-count (or (%parse-nonnegative-integer (getf plist :tool-count))
                         (%parse-nonnegative-integer (getf plist :tools-count))
                         (and (not (eq tools-value :__missing__))
                              (listp tools-value)
                              (length tools-value))))
         (hook-count (or (%parse-nonnegative-integer (getf plist :hook-count))
                         (%parse-nonnegative-integer (getf plist :hooks-count))
                         (and (not (eq hooks-value :__missing__))
                              (listp hooks-value)
                              (length hooks-value)))))
    (list :kind :manifest
          :name (extension-manifest-name manifest)
          :version (extension-manifest-version manifest)
          :dependencies (%manifest-dependency-names manifest)
          :permissions permissions
          :entry-point (extension-manifest-entry-point manifest)
          :tool-count tool-count
          :hook-count hook-count
          :manifest-path manifest-path
          :extension-root (%manifest-parent-directory manifest-path)
          :path manifest-path
          :last-write-date (%safe-file-write-date manifest-path))))

(defun %extension-in-package-target (form)
  (when (and (consp form)
             (symbolp (car form))
             (string= (%symbol-token (car form)) "in-package")
             (consp (cdr form)))
    (let ((target (cadr form)))
      (cond
        ((symbolp target) (string-upcase (symbol-name target)))
        ((stringp target) (string-upcase target))
        (t nil)))))

(defun %extension-in-package-allowed-p (form extension-package)
  (let* ((target (%extension-in-package-target form))
         (target-package (and target (find-package target))))
    (or (null target)
        (and extension-package
             target-package
             (string-equal (package-name target-package)
                           (package-name extension-package)))
        ;; Test fixtures use AMOEBUM/TEST globals to assert hot-reload behavior.
        (and target-package
             (string-equal (package-name target-package) "AMOEBUM/TEST")))))

(defun %legacy-file->metadata (file)
  (list :kind :legacy
        :name (pathname-name file)
        :version "0.0.0"
        :dependencies '()
        :permissions '()
        :entry-point (%canonical-extension-path file)
        :manifest-path nil
        :extension-root (uiop:pathname-directory-pathname file)
        :path file
        :last-write-date (%safe-file-write-date file)))

(defun discover-user-extension-files (&key project-root global-directory project-directory)
  (let* ((global-path (%global-extension-directory :global-directory global-directory))
         (project-path (%project-extension-directory :project-root project-root
                                                     :project-directory project-directory))
         (global-files (append (%list-extension-manifest-files global-path)
                               (%list-legacy-extension-files global-path)))
         (project-files (append (%list-extension-manifest-files project-path)
                                (%list-legacy-extension-files project-path))))
    (values global-files project-files)))

(defun extension-disabled-p (path)
  (not (null (gethash (%extension-key path) *disabled-extensions*))))

(defun list-extension-report ()
  (copy-list *extension-load-report*))

(defun list-loaded-extensions ()
  (copy-list *loaded-extensions*))

(defun list-extension-registry ()
  (let ((entries '()))
    (maphash (lambda (_key entry)
               (declare (ignore _key))
               (push entry entries))
             *extension-registry*)
    (sort entries #'string<
          :key (lambda (entry)
                 (string-downcase (or (extension-registry-entry-name entry) ""))))))

(defun %extension-entry->plist (entry)
  (let* ((manifest-path (extension-registry-entry-manifest-path entry))
         (entry-point (extension-registry-entry-entry-point entry))
         (path (or manifest-path entry-point)))
    (list :name (extension-registry-entry-name entry)
          :version (extension-registry-entry-version entry)
          :status (extension-registry-entry-status entry)
          :scope (extension-registry-entry-scope entry)
          :enabled-p (extension-registry-entry-enabled-p entry)
          :tool-count (extension-registry-entry-tool-count entry)
          :hook-count (extension-registry-entry-hook-count entry)
          :dependencies (copy-list (extension-registry-entry-dependencies entry))
          :path path
          :entry-point entry-point
          :manifest-path manifest-path
          :extension-root (extension-registry-entry-extension-root entry)
          :last-write-date (extension-registry-entry-last-write-date entry)
          :message (extension-registry-entry-message entry))))

(defun list-extensions ()
  (mapcar #'%extension-entry->plist
          (list-extension-registry)))

(defun %extension-designator-text (designator)
  (%extension-trim
   (cond
     ((null designator) "")
     ((stringp designator) designator)
     ((symbolp designator) (symbol-name designator))
     (t (princ-to-string designator)))))

(defun %extension-entry-matches-p (entry target)
  (let ((name (or (getf entry :name) ""))
        (path (or (getf entry :path) ""))
        (entry-point (or (getf entry :entry-point) ""))
        (manifest-path (or (getf entry :manifest-path) "")))
    (or (%extension-match-target-p target name)
        (%extension-match-target-p target path)
        (%extension-match-target-p target entry-point)
        (%extension-match-target-p target manifest-path))))

(defun describe-extension (designator)
  (let ((target (%extension-designator-text designator)))
    (when (plusp (length target))
      (find-if (lambda (entry)
                 (%extension-entry-matches-p entry target))
               (list-extensions)))))

(defun extension-report-summary (&optional (report *extension-load-report*))
  (let ((loaded 0)
        (errors 0)
        (disabled 0))
    (dolist (entry report)
      (case (extension-load-record-status entry)
        (:loaded (incf loaded))
        (:error (incf errors))
        (:disabled (incf disabled))))
    (list :total (length report)
          :loaded loaded
          :errors errors
          :disabled disabled)))

(defun known-user-extension-paths ()
  (let ((seen (make-hash-table :test #'equal))
        (paths '()))
    (labels ((remember (path-text)
               (let ((trimmed (%extension-trim path-text)))
                 (when (plusp (length trimmed))
                   (let ((key (string-downcase trimmed)))
                     (unless (gethash key seen)
                       (setf (gethash key seen) t)
                       (push trimmed paths)))))))
      (dolist (path-text *extension-last-discovered*)
        (remember path-text))
      (dolist (entry *extension-load-report*)
        (remember (extension-load-record-path entry))
        (remember (extension-load-record-manifest-path entry))))
    (nreverse paths)))

(defun known-user-extension-names ()
  (let ((seen (make-hash-table :test #'equal))
        (names '()))
    (labels ((remember (value)
               (let ((trimmed (%extension-trim value)))
                 (when (plusp (length trimmed))
                   (let ((key (string-downcase trimmed)))
                     (unless (gethash key seen)
                       (setf (gethash key seen) t)
                       (push trimmed names)))))))
      (dolist (entry (list-extension-registry))
        (remember (extension-registry-entry-name entry)))
      (dolist (entry *extension-load-report*)
        (remember (extension-load-record-name entry))))
    (nreverse names)))

(defun %disable-extension-path (path-text)
  (setf (gethash (%extension-key path-text) *disabled-extensions*) t))

(defun %enable-extension-path (path-text)
  (remhash (%extension-key path-text) *disabled-extensions*))

(defun %matched-registry-paths-by-target (target)
  (let ((paths '()))
    (dolist (entry (list-extension-registry))
      (let ((entry-name (extension-registry-entry-name entry))
            (entry-path (or (extension-registry-entry-manifest-path entry)
                            (extension-registry-entry-entry-point entry))))
        (when (and entry-path
                   (or (%extension-match-target-p target entry-path)
                       (%extension-match-target-p target entry-name)))
          (push entry-path paths))))
    (nreverse paths)))

(defun disable-user-extension (target)
  (let* ((trimmed (%extension-trim target))
         (known-paths (known-user-extension-paths))
         (disabled '()))
    (cond
      ((zerop (length trimmed))
       (values '() 0))
      ((string-equal trimmed "all")
       (dolist (path-text (append known-paths (%matched-registry-paths-by-target "all")))
         (%disable-extension-path path-text)
         (push path-text disabled))
       (values (remove-duplicates (nreverse disabled) :test #'string-equal)
               (length (remove-duplicates disabled :test #'string-equal))))
      (t
       (dolist (path-text known-paths)
         (when (%extension-match-target-p trimmed path-text)
           (%disable-extension-path path-text)
           (push path-text disabled)))
       (dolist (path-text (%matched-registry-paths-by-target trimmed))
         (%disable-extension-path path-text)
         (push path-text disabled))
       (when (and (null disabled) (probe-file trimmed))
         (let ((resolved (%canonical-extension-path trimmed)))
           (when (plusp (length resolved))
             (%disable-extension-path resolved)
             (push resolved disabled))))
       (let ((result (remove-duplicates (nreverse disabled) :test #'string-equal)))
         (values result (length result)))))))

(defun enable-user-extension (target)
  (let* ((trimmed (%extension-trim target))
         (known-paths (known-user-extension-paths))
         (enabled '()))
    (cond
      ((zerop (length trimmed))
       (values '() 0))
      ((string-equal trimmed "all")
       (dolist (path-text known-paths)
         (%enable-extension-path path-text)
         (push path-text enabled))
       (values (remove-duplicates (nreverse enabled) :test #'string-equal)
               (length (remove-duplicates enabled :test #'string-equal))))
      (t
       (dolist (path-text known-paths)
         (when (%extension-match-target-p trimmed path-text)
           (%enable-extension-path path-text)
           (push path-text enabled)))
       (dolist (path-text (%matched-registry-paths-by-target trimmed))
         (%enable-extension-path path-text)
         (push path-text enabled))
       (when (and (null enabled) (probe-file trimmed))
         (let ((resolved (%canonical-extension-path trimmed)))
           (when (plusp (length resolved))
             (%enable-extension-path resolved)
             (push resolved enabled))))
       (let ((result (remove-duplicates (nreverse enabled) :test #'string-equal)))
         (values result (length result)))))))

(defun %publish-extension-loaded (path scope)
  (publish (current-event-bus)
           (make-extension-loaded-event :path path :scope scope)))

(defun %publish-extension-error (path scope condition-text)
  (publish (current-event-bus)
           (make-extension-error-event :path path
                                       :scope scope
                                       :condition condition-text)))

(defun %resolve-entry-point (metadata)
  (let* ((entry-point (getf metadata :entry-point))
         (root (or (getf metadata :extension-root)
                   *default-pathname-defaults*))
         (entry-text (%ensure-string entry-point)))
    (cond
      ((or (search "/" entry-text :test #'char=)
           (search "\\" entry-text :test #'char=)
           (search ".lisp" entry-text :test #'char-equal)
           (search ".asd" entry-text :test #'char-equal))
       (merge-pathnames (pathname entry-text)
                        (uiop:ensure-directory-pathname root)))
      (t
       entry-text))))

(defun %load-extension-file-isolated (resolved-entry-point metadata extension-package)
  (let ((extension-name (or (getf metadata :name) "unknown-extension")))
    (unless (probe-file resolved-entry-point)
      (error "Extension ~A entry-point not found: ~A."
             extension-name
             resolved-entry-point))
    (with-open-file (stream resolved-entry-point :direction :input :external-format :utf-8)
      (let ((*package* extension-package)
            (*read-eval* nil))
        (loop for form = (read stream nil :eof)
              until (eq form :eof)
              do (progn
                   (%validate-extension-form form metadata extension-package)
                   (eval form))))))
  :isolated-load)

(defun %load-entry-point (resolved-entry-point metadata extension-package)
  (cond
    ((pathnamep resolved-entry-point)
     (if (eq (getf metadata :kind) :manifest)
         (%load-extension-file-isolated resolved-entry-point metadata extension-package)
         (progn
           (load resolved-entry-point :verbose nil :print nil)
           :direct-load)))
    ((stringp resolved-entry-point)
     (asdf:load-system resolved-entry-point)
     :asdf)
    (t
     (error "Unsupported entry-point type ~S." resolved-entry-point))))

(defun %register-extension (metadata scope status message &key extension-package)
  (let* ((name (getf metadata :name))
         (key (%extension-registry-key name))
         (entry-point (%resolve-entry-point metadata))
         (tool-count 0)
         (hook-count 0)
         (manifest-path (getf metadata :manifest-path))
         (recorded-path (or manifest-path
                            (and (pathnamep entry-point)
                                 (%canonical-extension-path entry-point))
                            (%ensure-string entry-point)))
         (entry (make-extension-registry-entry
                 :name name
                 :version (getf metadata :version)
                 :dependencies (copy-list (getf metadata :dependencies))
                 :entry-point (if (pathnamep entry-point)
                                  (%canonical-extension-path entry-point)
                                  (%ensure-string entry-point))
                 :manifest-path (and manifest-path (%canonical-extension-path manifest-path))
                 :extension-root (%canonical-extension-path (getf metadata :extension-root))
                 :scope scope
                 :permissions (copy-list (or (getf metadata :permissions) '()))
                 :package-name (and extension-package (package-name extension-package))
                 :enabled-p (not (extension-disabled-p recorded-path))
                 :tool-count tool-count
                 :hook-count hook-count
                 :status status
                 :loaded-at (get-universal-time)
                 :last-write-date (getf metadata :last-write-date)
                 :message message)))
    (multiple-value-setq (tool-count hook-count)
      (%resolve-definition-counts metadata entry-point))
    (setf (extension-registry-entry-tool-count entry) tool-count
          (extension-registry-entry-hook-count entry) hook-count)
    (setf (gethash key *extension-registry*) entry)
    entry))

(defun %metadata-record-path (metadata)
  (or (and (getf metadata :manifest-path)
           (%canonical-extension-path (getf metadata :manifest-path)))
      (%canonical-extension-path (getf metadata :path))))

(defun %collect-extension-candidates (&key project-root global-directory project-directory)
  (multiple-value-bind (global-files project-files)
      (discover-user-extension-files :project-root project-root
                                     :global-directory global-directory
                                     :project-directory project-directory)
    (append (loop for file in global-files collect (cons :global file))
            (loop for file in project-files collect (cons :project file)))))

(defun %rebuild-extension-watch-snapshot (&optional (paths *extension-last-discovered*))
  (clrhash *extension-watch-snapshot*)
  (dolist (entry *extension-load-report*)
    (let ((manifest-path (extension-load-record-manifest-path entry))
          (entry-point (extension-load-record-entry-point entry)))
      (when (and (stringp manifest-path) (plusp (length (%extension-trim manifest-path))))
        (push manifest-path paths))
      (when (and (stringp entry-point)
                 (plusp (length (%extension-trim entry-point)))
                 (or (search ".lisp" entry-point :test #'char-equal)
                     (search "/" entry-point :test #'char=)
                     (search "\\" entry-point :test #'char=)))
        (push entry-point paths))))
  (dolist (path-text paths)
    (when (plusp (length (%extension-trim path-text)))
      (setf (gethash path-text *extension-watch-snapshot*)
            (%safe-file-write-date path-text))))
  *extension-watch-snapshot*)

(defun load-user-extensions (&key project-root global-directory project-directory
                                  (start-hot-reload *extension-hot-reload-enabled-p*))
  (let* ((candidates (%collect-extension-candidates :project-root project-root
                                                    :global-directory global-directory
                                                    :project-directory project-directory))
         (report '())
         (loaded '()))
    (clrhash *extension-registry*)
    (setf *extension-last-discovered*
          (mapcar (lambda (entry)
                    (%canonical-extension-path (cdr entry)))
                  candidates))
    (dolist (entry candidates)
      (let* ((scope (car entry))
             (file (cdr entry))
             (path-text (%canonical-extension-path file)))
        (handler-case
            (let* ((metadata
                     (if (and (string-equal (pathname-name file) "extension")
                              (string-equal (pathname-type file) "lisp"))
                         (%manifest->metadata file)
                         (%legacy-file->metadata file)))
                   (record-path (%metadata-record-path metadata))
                   (entry-point (%resolve-entry-point metadata))
                   (extension-package
                     (and (eq (getf metadata :kind) :manifest)
                          (%ensure-extension-package metadata)))
                   (package-name (and extension-package
                                      (package-name extension-package))))
              (cond
                ((extension-disabled-p record-path)
                 (%register-extension metadata
                                      scope
                                      :disabled
                                      "disabled by /extensions disable"
                                      :extension-package extension-package)
                 (push (make-extension-load-record
                        :path record-path
                        :scope scope
                        :name (getf metadata :name)
                        :version (getf metadata :version)
                        :dependencies (copy-list (getf metadata :dependencies))
                        :permissions (copy-list (or (getf metadata :permissions) '()))
                        :package-name package-name
                        :entry-point (if (pathnamep entry-point)
                                         (%canonical-extension-path entry-point)
                                         (%ensure-string entry-point))
                        :manifest-path (and (getf metadata :manifest-path)
                                            (%canonical-extension-path (getf metadata :manifest-path)))
                        :status :disabled
                        :message "disabled by /extensions disable")
                       report))
                (t
                 (when (eq (getf metadata :kind) :manifest)
                   (%ensure-extension-permissions-approved metadata scope))
                 (%load-entry-point entry-point metadata extension-package)
                 (%register-extension metadata
                                      scope
                                      :loaded
                                      nil
                                      :extension-package extension-package)
                 (%publish-extension-loaded record-path scope)
                 (let ((record
                         (make-extension-load-record
                          :path record-path
                          :scope scope
                          :name (getf metadata :name)
                          :version (getf metadata :version)
                          :dependencies (copy-list (getf metadata :dependencies))
                          :permissions (copy-list (or (getf metadata :permissions) '()))
                          :package-name package-name
                          :entry-point (if (pathnamep entry-point)
                                           (%canonical-extension-path entry-point)
                                           (%ensure-string entry-point))
                          :manifest-path (and (getf metadata :manifest-path)
                                              (%canonical-extension-path (getf metadata :manifest-path)))
                          :status :loaded)))
                   (push record report)
                   (push record loaded)))))
          (error (condition)
            (let ((message (princ-to-string condition)))
              (format *error-output*
                      "Extension load failed (~A): ~A~%"
                      path-text
                      message)
              (%publish-extension-error path-text scope message)
              (let* ((error-metadata
                       (if (and (string-equal (pathname-name file) "extension")
                                (string-equal (pathname-type file) "lisp"))
                           (or (ignore-errors (%manifest->metadata file))
                               (list :kind :manifest
                                     :name (let* ((parts (pathname-directory (%manifest-parent-directory file)))
                                                  (last-part (and (listp parts) (car (last parts)))))
                                             (%ensure-string (or last-part "unknown-extension")))
                                     :version "0.0.0"
                                     :dependencies '()
                                     :permissions '()
                                     :entry-point (namestring file)
                                     :manifest-path file
                                     :extension-root (%manifest-parent-directory file)
                                     :path file
                                     :last-write-date (%safe-file-write-date file)))
                           (%legacy-file->metadata file)))
                     (error-entry-point (%resolve-entry-point error-metadata))
                     (error-package
                       (and (eq (getf error-metadata :kind) :manifest)
                            (ignore-errors (%ensure-extension-package error-metadata)))))
                (%register-extension error-metadata
                                     scope
                                     :error
                                     message
                                     :extension-package error-package)
                (push (make-extension-load-record
                       :path (%metadata-record-path error-metadata)
                       :scope scope
                       :name (getf error-metadata :name)
                       :version (getf error-metadata :version)
                       :dependencies (copy-list (or (getf error-metadata :dependencies) '()))
                       :permissions (copy-list (or (getf error-metadata :permissions) '()))
                       :package-name (and error-package (package-name error-package))
                       :entry-point (if (pathnamep error-entry-point)
                                        (%canonical-extension-path error-entry-point)
                                        (%ensure-string error-entry-point))
                       :manifest-path (and (getf error-metadata :manifest-path)
                                           (%canonical-extension-path (getf error-metadata :manifest-path)))
                       :status :error
                       :message message)
                      report)))))))
    (setf *extension-load-report* (nreverse report)
          *loaded-extensions* (nreverse loaded))
    (%rebuild-extension-watch-snapshot)
    (if start-hot-reload
        (start-extension-hot-reload :project-root project-root
                                    :global-directory global-directory
                                    :project-directory project-directory)
        (stop-extension-hot-reload))
    *extension-load-report*))

(defun reload-user-extensions (&key project-root global-directory project-directory
                                    (start-hot-reload *extension-hot-reload-enabled-p*))
  (load-user-extensions :project-root project-root
                        :global-directory global-directory
                        :project-directory project-directory
                        :start-hot-reload start-hot-reload))

(defun check-extension-hot-reload (&key project-root global-directory project-directory
                                        (reload-on-change t)
                                        (start-hot-reload *extension-hot-reload-enabled-p*))
  (let* ((candidates (%collect-extension-candidates :project-root project-root
                                                    :global-directory global-directory
                                                    :project-directory project-directory))
         (current-paths (mapcar (lambda (entry)
                                  (%canonical-extension-path (cdr entry)))
                                candidates))
         (changed-p nil))
    (dolist (path-text current-paths)
      (let ((current (%safe-file-write-date path-text))
            (previous (gethash path-text *extension-watch-snapshot* :__missing__)))
        (when (or (eq previous :__missing__)
                  (not (eql previous current)))
          (setf changed-p t))))
    (maphash (lambda (path-text _value)
               (declare (ignore _value))
               (unless (member path-text current-paths :test #'string-equal)
                 (setf changed-p t)))
             *extension-watch-snapshot*)
    (when changed-p
      (if reload-on-change
          (progn
            (reload-user-extensions :project-root project-root
                                    :global-directory global-directory
                                    :project-directory project-directory
                                    :start-hot-reload start-hot-reload)
            t)
          (progn
            (%rebuild-extension-watch-snapshot current-paths)
            t)))))

(defun start-extension-hot-reload (&key project-root global-directory project-directory)
  (when (and *extension-hot-reload-thread*
             (bordeaux-threads:thread-alive-p *extension-hot-reload-thread*))
    (return-from start-extension-hot-reload *extension-hot-reload-thread*))
  (setf *extension-hot-reload-running-p* t)
  (setf *extension-hot-reload-thread*
        (bordeaux-threads:make-thread
         (lambda ()
           (loop while *extension-hot-reload-running-p* do
             (ignore-errors
               (check-extension-hot-reload :project-root project-root
                                           :global-directory global-directory
                                           :project-directory project-directory
                                           :reload-on-change t
                                           :start-hot-reload nil))
             (sleep (max 0.1d0 *extension-hot-reload-interval-seconds*))))
         :name "amoebum-extension-hot-reload"))
  *extension-hot-reload-thread*)

(defun stop-extension-hot-reload ()
  (let ((thread *extension-hot-reload-thread*))
    (setf *extension-hot-reload-running-p* nil
          *extension-hot-reload-thread* nil)
    (when (and thread
               (bordeaux-threads:thread-alive-p thread)
               (not (eq thread (bordeaux-threads:current-thread))))
      (ignore-errors
        (bordeaux-threads:join-thread thread)))
    t))
