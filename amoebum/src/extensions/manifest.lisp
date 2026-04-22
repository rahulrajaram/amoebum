(in-package :amoebum)

(defparameter +extension-capabilities+
  '(:tools :hooks :widgets :keymaps)
  "Capabilities an extension manifest may declare in :CAPABILITIES.")

(defstruct (extension-manifest
            (:constructor %make-extension-manifest
                (&key name
                 version
                 author
                 description
                 license
                 (dependencies '())
                 (capabilities '())
                 entry-point
                 source-path)))
  (name "" :type string)
  (version "" :type string)
  (author "" :type string)
  (description "" :type string)
  (license "" :type string)
  (dependencies '() :type list)
  (capabilities '() :type list)
  (entry-point "extension-main.lisp" :type string)
  (source-path nil :type (or null string pathname)))

(defun make-extension-manifest (&key name
                                     version
                                     author
                                     description
                                     license
                                     (dependencies '())
                                     capabilities
                                     provides
                                     entry-point
                                     source-path)
  (%make-extension-manifest
   :name (%trim-string name)
   :version (%trim-string version)
   :author (%trim-string author)
   :description (%trim-string description)
   :license (%trim-string license)
   :dependencies (or dependencies '())
   :capabilities (or capabilities provides '())
   :entry-point (%trim-string (or entry-point "extension-main.lisp"))
   :source-path source-path))

(defun extension-manifest-provides (manifest)
  (extension-manifest-capabilities manifest))

(defun (setf extension-manifest-provides) (value manifest)
  (setf (extension-manifest-capabilities manifest) value))

(defun %trim-string (value)
  (if (stringp value)
      (string-trim '(#\Space #\Tab #\Newline #\Return) value)
      ""))

(defun %normalize-manifest-name (value)
  (let ((text (%trim-string value)))
    (string-downcase text)))

(defun %normalize-manifest-form (sexp)
  (cond
    ((null sexp) nil)
    ((and (listp sexp) (keywordp (first sexp)))
     sexp)
    ((and (listp sexp)
          (symbolp (first sexp))
          (member (string-downcase (symbol-name (first sexp)))
                  '("extension" "extension-manifest" "manifest")
                  :test #'string=))
     (second sexp))
    ((and (listp sexp)
          (every (lambda (entry)
                   (and (consp entry)
                        (or (keywordp (car entry))
                            (symbolp (car entry)))))
                 sexp))
     (loop for (key . rest) in sexp
           append (list (if (keywordp key)
                            key
                            (intern (string-upcase (symbol-name key)) :keyword))
                        (if (= (length rest) 1)
                            (first rest)
                            rest))))
    (t nil)))

(defun %parse-capability (value)
  (let* ((name (cond
                 ((keywordp value) value)
                 ((symbolp value) (intern (string-upcase (symbol-name value)) :keyword))
                 ((stringp value) (intern (string-upcase (%trim-string value)) :keyword))
                 (t nil))))
    (unless (member name +extension-capabilities+ :test #'eq)
      (error "Unknown extension capability ~S. Expected one of ~S."
             value +extension-capabilities+))
    name))

(defun %parse-dependency-entry (entry)
  (cond
    ((and (listp entry)
          (>= (length entry) 1)
          (or (stringp (first entry))
              (symbolp (first entry))
              (keywordp (first entry))))
     (let ((name (first entry))
           (constraint (second entry)))
       (cons (%normalize-manifest-name
              (if (stringp name) name (string-downcase (string name))))
             (%trim-string constraint))))
    ((and (listp entry)
          (or (getf entry :name) (getf entry :extension)))
     (let ((name (or (getf entry :name) (getf entry :extension)))
           (constraint (or (getf entry :version-constraint)
                           (getf entry :constraint)
                           (getf entry :version)
                           "")))
       (cons (%normalize-manifest-name
              (if (stringp name) name (string-downcase (string name))))
             (%trim-string constraint))))
    (t
     (error "Invalid dependency entry ~S. Expected (name constraint)." entry))))

(defun parse-extension-manifest-sexp (sexp &key source-path)
  (let* ((plist (%normalize-manifest-form sexp))
         (name (%normalize-manifest-name (getf plist :name)))
         (version (%trim-string (getf plist :version)))
         (author (%trim-string (getf plist :author)))
         (description (%trim-string (getf plist :description)))
         (license (%trim-string (getf plist :license)))
         (dependencies-raw (or (getf plist :dependencies) '()))
         (capabilities-raw (or (getf plist :capabilities)
                               (getf plist :provides)
                               '()))
         (entry-point (%trim-string (or (getf plist :entry-point)
                                        "extension-main.lisp"))))
    (unless (and plist (listp plist))
      (error "Invalid extension manifest form ~S." sexp))
    (when (zerop (length name))
      (error "Extension manifest missing required :name field."))
    (when (zerop (length version))
      (error "Extension manifest ~A missing required :version field." name))
    (when (zerop (length entry-point))
      (error "Extension manifest ~A missing required :entry-point field." name))
    (make-extension-manifest
     :name name
     :version version
     :author author
     :description description
     :license license
     :dependencies (mapcar #'%parse-dependency-entry dependencies-raw)
     :capabilities (remove-duplicates (mapcar #'%parse-capability capabilities-raw) :test #'eq)
     :entry-point entry-point
     :source-path source-path)))

(defun read-extension-manifest-file (path)
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (let ((form (read stream nil nil)))
      (unless form
        (error "Manifest file ~A is empty." path))
      (parse-extension-manifest-sexp form :source-path path))))

(defun %split-version (version)
  (let ((text (%trim-string version)))
    (if (zerop (length text))
        '(0)
        (mapcar (lambda (chunk)
                  (if (cl-ppcre:scan "^[0-9]+$" chunk)
                      (parse-integer chunk)
                      (string-downcase chunk)))
                (uiop:split-string text :separator '(#\.))))))

(defun %compare-version-components (left right)
  (cond
    ((and (integerp left) (integerp right))
     (cond ((< left right) -1)
           ((> left right) 1)
           (t 0)))
    (t
     (let ((ltext (string-downcase (princ-to-string left)))
           (rtext (string-downcase (princ-to-string right))))
       (cond ((string< ltext rtext) -1)
             ((string> ltext rtext) 1)
             (t 0))))))

(defun compare-extension-version (left right)
  (let ((left-parts (%split-version left))
        (right-parts (%split-version right)))
    (loop for l in left-parts
          for r in right-parts
          do (let ((cmp (%compare-version-components l r)))
               (when (/= cmp 0)
                 (return cmp)))
          finally
             (return
               (let ((left-len (length left-parts))
                     (right-len (length right-parts)))
                 (cond ((< left-len right-len)
                        (if (every (lambda (value) (equal value 0))
                                   (nthcdr left-len right-parts))
                            0
                            -1))
                       ((> left-len right-len)
                        (if (every (lambda (value) (equal value 0))
                                   (nthcdr right-len left-parts))
                            0
                            1))
                       (t 0)))))))

(defun %parse-version-clauses (constraint)
  (let* ((trimmed (%trim-string constraint))
         (tokens (remove-if (lambda (item) (zerop (length item)))
                            (uiop:split-string trimmed :separator '(#\Space #\Tab #\Newline)))))
    (cond
      ((or (zerop (length trimmed))
           (string= trimmed "*"))
       '())
      ((and (plusp (length trimmed))
            (member (char trimmed 0) '(#\= #\< #\>)))
       (let ((clauses '())
             (index 0))
         (loop while (< index (length tokens))
               do (let* ((token (nth index tokens))
                         (matched (cl-ppcre:register-groups-bind (op version)
                                      ("^(>=|<=|=|>|<)(.+)$" token)
                                    (list op version))))
                    (cond
                      (matched
                       (destructuring-bind (op version) matched
                         (push (list op version) clauses)
                         (incf index)))
                      ((and (member token '(">=" "<=" "=" ">" "<") :test #'string=)
                            (< (1+ index) (length tokens)))
                       (push (list token (nth (1+ index) tokens)) clauses)
                       (incf index 2))
                      (t
                       (error "Invalid version constraint token ~S in ~S." token constraint)))))
         (nreverse clauses)))
      (t
       (list (list "=" trimmed))))))

(defun version-satisfies-constraint-p (version constraint)
  (every (lambda (clause)
           (destructuring-bind (op expected) clause
             (let ((cmp (compare-extension-version version expected)))
               (cond
                 ((string= op "=") (= cmp 0))
                 ((string= op ">=") (>= cmp 0))
                 ((string= op "<=") (<= cmp 0))
                 ((string= op ">") (> cmp 0))
                 ((string= op "<") (< cmp 0))
                 (t (error "Unsupported version operator ~S." op))))))
         (%parse-version-clauses constraint)))

(defun %manifest-by-name (manifests)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (manifest manifests)
      (let ((name (extension-manifest-name manifest)))
        (when (gethash name table)
          (error "Duplicate extension manifest name ~A." name))
        (setf (gethash name table) manifest)))
    table))

;;;; -----------------------------------------------------------------------
;;;; Metadata builders (NXT-386 extraction).
;;;;
;;;; The functions below convert discovered extension files into the loose
;;;; metadata plists consumed by extensions/loader.lisp. They depend on
;;;; helpers defined in extensions/discovery.lisp (path normalization,
;;;; %safe-file-write-date, %manifest-parent-directory, %ensure-string,
;;;; %parse-nonnegative-integer, %canonical-extension-path) and on the
;;;; manifest parser declared above.

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

(defun %manifest-extension-file-p (file)
  (and (string-equal (pathname-name file) "extension")
       (string-equal (pathname-type file) "lisp")))

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

(defun %metadata-record-path (metadata)
  (or (and (getf metadata :manifest-path)
           (%canonical-extension-path (getf metadata :manifest-path)))
      (%canonical-extension-path (getf metadata :path))))

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

(defun %source-file->metadata (file)
  (if (%manifest-extension-file-p file)
      (%manifest->metadata file)
      (%legacy-file->metadata file)))

(defun %fallback-error-metadata (file)
  (if (%manifest-extension-file-p file)
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

;;;; -----------------------------------------------------------------------
;;;; Multi-manifest dependency resolver (existing public entry point).

(defun resolve-extension-manifests (manifests &key (errorp t))
  (let* ((registry (%manifest-by-name manifests))
         (dependents (make-hash-table :test #'equal))
         (in-degree (make-hash-table :test #'equal))
         (missing '())
         (version-mismatches '()))
    (dolist (manifest manifests)
      (setf (gethash (extension-manifest-name manifest) in-degree) 0))
    (dolist (manifest manifests)
      (let ((owner-name (extension-manifest-name manifest)))
        (dolist (dependency (extension-manifest-dependencies manifest))
          (destructuring-bind (dep-name . dep-constraint) dependency
            (let ((provider (gethash dep-name registry)))
              (cond
                ((null provider)
                 (push (list :extension owner-name
                             :dependency dep-name
                             :constraint dep-constraint)
                       missing))
                ((not (version-satisfies-constraint-p
                       (extension-manifest-version provider)
                       dep-constraint))
                 (push (list :extension owner-name
                             :dependency dep-name
                             :required dep-constraint
                             :actual (extension-manifest-version provider))
                       version-mismatches))
                (t
                 (push owner-name (gethash dep-name dependents))
                 (incf (gethash owner-name in-degree)))))))))
    (let* ((queue (sort (loop for manifest in manifests
                              for name = (extension-manifest-name manifest)
                              when (zerop (gethash name in-degree))
                                collect name)
                        #'string<))
           (ordered-names '()))
      (loop while queue
            do (let ((name (pop queue)))
                 (push name ordered-names)
                 (dolist (dependent (sort (copy-list (gethash name dependents))
                                          #'string<))
                   (decf (gethash dependent in-degree))
                   (when (zerop (gethash dependent in-degree))
                     (setf queue (merge 'list (list dependent) queue #'string<))))))
      (let* ((processed (length ordered-names))
             (total (length manifests))
             (cycle-nodes
               (if (= processed total)
                   '()
                   (sort
                    (loop for manifest in manifests
                          for name = (extension-manifest-name manifest)
                          unless (member name ordered-names :test #'string=)
                            collect name)
                    #'string<)))
             (report (list :missing (nreverse missing)
                           :version-mismatches (nreverse version-mismatches)
                           :cycles cycle-nodes))
             (ordered-manifests
               (mapcar (lambda (name) (gethash name registry))
                       (nreverse ordered-names))))
        (when (and errorp
                   (or missing version-mismatches cycle-nodes))
          (error "Extension dependency resolution failed: missing=~S version-mismatches=~S cycles=~S"
                 (nreverse missing)
                 (nreverse version-mismatches)
                 cycle-nodes))
        (values ordered-manifests report)))))
