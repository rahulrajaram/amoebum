(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; Codebase Indexer (I96)
;;;
;;; Indexes Common Lisp symbols via sb-introspect and optionally LSP.
;;; Generates token-efficient repo maps for system prompt context.
;;; Supports incremental updates via file modification times.
;;; ---------------------------------------------------------------------------

(defstruct (symbol-entry
            (:constructor make-symbol-entry
                (&key name package kind file line signature doc)))
  (name "" :type string)
  (package "" :type string)
  (kind :function :type keyword)
  (file nil :type (or null string pathname))
  (line 0 :type integer)
  (signature "" :type string)
  (doc "" :type string))

(defstruct (codebase-index
            (:constructor %make-codebase-index))
  (entries '() :type list)
  (file-mtimes (make-hash-table :test #'equal) :type hash-table)
  (created-at (get-universal-time) :type integer)
  (updated-at (get-universal-time) :type integer)
  (project-root nil)
  (language nil :type (or null keyword)))

(defun make-codebase-index (&key project-root language)
  "Create an empty codebase index."
  (%make-codebase-index :project-root project-root :language language))

;;; --- Symbol Kind Detection ---

(defun %symbol-kind (symbol)
  "Determine the kind of a symbol."
  (cond
    ((and (fboundp symbol)
          (typep (symbol-function symbol) 'generic-function))
     :generic-function)
    ((and (fboundp symbol) (macro-function symbol))
     :macro)
    ((and (fboundp symbol) (special-operator-p symbol))
     :special-form)
    ((fboundp symbol) :function)
    ((find-class symbol nil) :class)
    ((boundp symbol) :variable)
    (t :symbol)))

;;; --- CL Indexer via sb-introspect ---

(defun %symbol-source-location (symbol kind)
  "Get source file and line for SYMBOL using sb-introspect."
  #+sbcl
  (handler-case
      (let* ((source-type (case kind
                            ((:function :generic-function) :function)
                            (:macro :function)
                            (:class :class)
                            (:variable :variable)
                            (otherwise :function)))
             (sources (sb-introspect:find-definition-sources-by-name symbol source-type)))
        (when sources
          (let* ((source (first sources))
                 (namestring (and (sb-introspect:definition-source-pathname source)
                                  (namestring (sb-introspect:definition-source-pathname source))))
                 (form-path (sb-introspect:definition-source-form-path source)))
            (values namestring
                    (if (and form-path (integerp (first form-path)))
                        (first form-path)
                        0)))))
    (error () (values nil 0)))
  #-sbcl
  (values nil 0))

(defun %symbol-lambda-list (symbol)
  "Get the lambda list for a function symbol."
  #+sbcl
  (handler-case
      (let ((ll (sb-introspect:function-lambda-list symbol)))
        (if ll
            (format nil "(~{~A~^ ~})" (mapcar #'princ-to-string ll))
            ""))
    (error () ""))
  #-sbcl
  "")

(defun %symbol-documentation (symbol kind)
  "Get documentation string for SYMBOL."
  (let ((doc-type (case kind
                    ((:function :generic-function :macro) 'function)
                    (:variable 'variable)
                    (:class 'type)
                    (otherwise nil))))
    (if doc-type
        (handler-case
            (or (documentation symbol doc-type) "")
          (error () ""))
        "")))

(defun index-package-symbols (index package-designator &key (external-only t))
  "Index all symbols in PACKAGE-DESIGNATOR into INDEX."
  (let ((pkg (find-package package-designator)))
    (unless pkg (return-from index-package-symbols index))
    (let ((entries '()))
      (flet ((process-symbol (sym)
               (when (or (fboundp sym) (boundp sym) (find-class sym nil))
                 (let* ((kind (%symbol-kind sym))
                        (sig (if (member kind '(:function :generic-function :macro))
                                 (%symbol-lambda-list sym)
                                 ""))
                        (doc (%symbol-documentation sym kind)))
                   (multiple-value-bind (file line)
                       (%symbol-source-location sym kind)
                     (push (make-symbol-entry
                            :name (symbol-name sym)
                            :package (package-name pkg)
                            :kind kind
                            :file file
                            :line (or line 0)
                            :signature sig
                            :doc (if (> (length doc) 200)
                                     (subseq doc 0 200)
                                     doc))
                           entries))))))
        (if external-only
            (do-external-symbols (sym pkg) (process-symbol sym))
            (do-symbols (sym pkg)
              (when (eq (symbol-package sym) pkg)
                (process-symbol sym)))))
      (setf (codebase-index-entries index)
            (append (codebase-index-entries index) entries))
      (setf (codebase-index-updated-at index) (get-universal-time))
      index)))

;;; --- File-Based Indexing ---

(defun %lisp-files-in-directory (directory)
  "Find all .lisp files in DIRECTORY recursively."
  (let ((pattern (merge-pathnames "**/*.lisp"
                                   (uiop:ensure-directory-pathname directory))))
    (directory pattern)))

(defun %file-needs-reindex-p (index file)
  "Check if FILE has been modified since last index."
  (let ((mtime (or (ignore-errors (file-write-date file)) 0))
        (key (namestring file)))
    (let ((cached-mtime (gethash key (codebase-index-file-mtimes index))))
      (or (null cached-mtime)
          (> mtime cached-mtime)))))

(defun %record-file-mtime (index file)
  "Record the modification time of FILE in INDEX."
  (let ((mtime (or (ignore-errors (file-write-date file)) 0)))
    (setf (gethash (namestring file) (codebase-index-file-mtimes index)) mtime)))

(defun index-directory (index directory &key (incremental t))
  "Index all .lisp files in DIRECTORY. If INCREMENTAL, skip unchanged files."
  (let ((files (%lisp-files-in-directory directory))
        (count 0))
    (dolist (file files)
      (when (or (not incremental) (%file-needs-reindex-p index file))
        ;; For file-based indexing, we just record the file exists and its mtime
        ;; Full symbol indexing requires the package to be loaded
        (%record-file-mtime index file)
        (incf count)))
    (setf (codebase-index-updated-at index) (get-universal-time))
    count))

;;; --- Repo Map Generation ---

(defun generate-repo-map (index &key (max-tokens 4096) (include-docs nil))
  "Generate a token-efficient repo map string from INDEX.
   Outputs a compact summary suitable for LLM system prompts."
  (let ((by-package (make-hash-table :test #'equal)))
    ;; Group by package
    (dolist (entry (codebase-index-entries index))
      (push entry (gethash (symbol-entry-package entry) by-package)))
    (with-output-to-string (out)
      (format out "# Repo Map~%")
      (let ((token-estimate 0))
        (maphash
         (lambda (pkg entries)
           (when (< token-estimate max-tokens)
             (format out "~%## ~A~%" pkg)
             (incf token-estimate 5)
             (dolist (entry (sort (copy-list entries) #'string<
                                  :key #'symbol-entry-name))
               (when (< token-estimate max-tokens)
                 (let ((line (format nil "  ~A ~A~A~A~%"
                                     (case (symbol-entry-kind entry)
                                       (:function "fn")
                                       (:generic-function "gf")
                                       (:macro "mac")
                                       (:class "cls")
                                       (:variable "var")
                                       (otherwise "sym"))
                                     (symbol-entry-name entry)
                                     (if (plusp (length (symbol-entry-signature entry)))
                                         (format nil " ~A" (symbol-entry-signature entry))
                                         "")
                                     (if (and include-docs
                                              (plusp (length (symbol-entry-doc entry))))
                                         (format nil " — ~A" (symbol-entry-doc entry))
                                         ""))))
                   (write-string line out)
                   (incf token-estimate (ceiling (length line) 4)))))))
         by-package)))))

;;; --- Index Queries ---

(defun index-find-symbol (index name &key package kind)
  "Find entries matching NAME, optionally filtered by PACKAGE and KIND."
  (remove-if-not
   (lambda (entry)
     (and (string-equal name (symbol-entry-name entry))
          (or (null package)
              (string-equal package (symbol-entry-package entry)))
          (or (null kind)
              (eq kind (symbol-entry-kind entry)))))
   (codebase-index-entries index)))

(defun index-find-by-file (index file-path)
  "Find all entries defined in FILE-PATH."
  (remove-if-not
   (lambda (entry)
     (and (symbol-entry-file entry)
          (search file-path (princ-to-string (symbol-entry-file entry))
                  :test #'char-equal)))
   (codebase-index-entries index)))

(defun index-statistics (index)
  "Return statistics about the index."
  (let ((kinds (make-hash-table :test #'eq))
        (packages (make-hash-table :test #'equal)))
    (dolist (entry (codebase-index-entries index))
      (incf (gethash (symbol-entry-kind entry) kinds 0))
      (incf (gethash (symbol-entry-package entry) packages 0)))
    (list :total-entries (length (codebase-index-entries index))
          :files-tracked (hash-table-count (codebase-index-file-mtimes index))
          :kinds (let ((result '()))
                   (maphash (lambda (k v) (push (cons k v) result)) kinds)
                   result)
          :packages (let ((result '()))
                      (maphash (lambda (k v) (push (cons k v) result)) packages)
                      result))))
