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
