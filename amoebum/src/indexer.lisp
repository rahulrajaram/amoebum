(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; Codebase Indexer (I96, I248)
;;;
;;; Indexes Common Lisp symbols via sb-introspect and optionally LSP.
;;; Generates token-efficient repo maps for system prompt context.
;;; Supports incremental updates via file modification times.
;;; ---------------------------------------------------------------------------

(defparameter +default-repo-map-token-target+ 1200
  "Default target token budget for generated repo maps.")

(defvar *active-codebase-index* nil
  "Global project index instance used by slash commands and prompt assembly.")

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
  (entries-by-file (make-hash-table :test #'equal) :type hash-table)
  (file-mtimes (make-hash-table :test #'equal) :type hash-table)
  (indexed-systems '() :type list)
  (repo-map "" :type string)
  (repo-map-token-estimate 0 :type integer)
  (last-run-stats '() :type list)
  (created-at (get-universal-time) :type integer)
  (updated-at (get-universal-time) :type integer)
  (project-root nil)
  (language nil :type (or null keyword)))

(defun make-codebase-index (&key project-root language)
  "Create an empty codebase index."
  (%make-codebase-index :project-root project-root :language language))

(defun %normalize-file-path (path)
  "Return a stable, absolute namestring for PATH where possible."
  (when path
    (let* ((pathname (etypecase path
                       (pathname path)
                       (string (pathname path))
                       (symbol (pathname (symbol-name path)))))
           (truename-path (ignore-errors (truename pathname))))
      (namestring (or truename-path pathname)))))

(defun %directory-prefix (directory)
  (when directory
    (let* ((dir-path (uiop:ensure-directory-pathname directory))
           (normalized (%normalize-file-path dir-path)))
      (and normalized
           (if (or (uiop:string-suffix-p normalized "/")
                   (uiop:string-suffix-p normalized "\\"))
               normalized
               (concatenate 'string normalized "/"))))))

(defun %path-under-prefix-p (path prefix)
  (and path prefix
       (let ((prefix-length (length prefix)))
         (and (<= prefix-length (length path))
              (string-equal prefix path :end2 prefix-length)))))

(defun %path-under-any-prefix-p (path prefixes)
  (some (lambda (prefix)
          (%path-under-prefix-p path prefix))
        prefixes))

(defun %file-mtime (file)
  (or (ignore-errors (file-write-date (pathname file))) 0))

(defun %copy-hash-table (table &key (value-transform #'identity))
  (let ((copy (make-hash-table :test (hash-table-test table))))
    (maphash (lambda (key value)
               (setf (gethash key copy) (funcall value-transform value)))
             table)
    copy))

(defun %hash-set-from-list (items)
  (let ((set (make-hash-table :test #'equal)))
    (dolist (item items set)
      (setf (gethash item set) t))))

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

(defun %symbol-definition-kinds (symbol)
  "Return all supported definition kinds for SYMBOL."
  (let ((kinds '()))
    (when (macro-function symbol)
      (push :macro kinds))
    (when (and (fboundp symbol)
               (not (macro-function symbol))
               (not (special-operator-p symbol)))
      (push (if (typep (symbol-function symbol) 'generic-function)
                :generic-function
                :function)
            kinds))
    (when (find-class symbol nil)
      (push :class kinds))
    (when (boundp symbol)
      (push :variable kinds))
    (nreverse kinds)))

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
      (dolist (entry entries)
        (let ((file (symbol-entry-file entry)))
          (when file
            (push entry (gethash (%normalize-file-path file)
                                 (codebase-index-entries-by-file index))))))
      (setf (codebase-index-entries index)
            (append (codebase-index-entries index) entries))
      (setf (codebase-index-updated-at index) (get-universal-time))
      index)))

;;; --- File-Based Indexing ---

(defun %collect-lisp-files-recursive (directory)
  "Collect .lisp/.lsp files recursively under DIRECTORY."
  (labels ((walk (dir acc)
             (let* ((safe-dir (uiop:ensure-directory-pathname dir))
                    (lisp-files (ignore-errors (uiop:directory-files safe-dir "*.lisp")))
                    (lsp-files (ignore-errors (uiop:directory-files safe-dir "*.lsp")))
                    (subdirs (ignore-errors (uiop:subdirectories safe-dir))))
               (dolist (file (append lisp-files lsp-files))
                 (let ((normalized (%normalize-file-path file)))
                   (when normalized
                     (pushnew normalized acc :test #'equal))))
               (dolist (subdir subdirs)
                 (setf acc (walk subdir acc)))
               acc)))
    (if (probe-file directory)
        (walk directory '())
        '())))

(defun %file-needs-reindex-p (index file)
  "Check if FILE has been modified since last index."
  (let* ((key (%normalize-file-path file))
         (mtime (%file-mtime file)))
    (let ((cached-mtime (gethash key (codebase-index-file-mtimes index))))
      (or (null cached-mtime)
          (/= mtime cached-mtime)))))

(defun %record-file-mtime (index file)
  "Record the modification time of FILE in INDEX."
  (let ((key (%normalize-file-path file))
        (mtime (%file-mtime file)))
    (when key
      (setf (gethash key (codebase-index-file-mtimes index)) mtime))))

(defun index-directory (index directory &key (incremental t))
  "Index all .lisp files in DIRECTORY. If INCREMENTAL, skip unchanged files."
  (let ((files (%collect-lisp-files-recursive directory))
        (count 0))
    (dolist (file files)
      (when (or (not incremental) (%file-needs-reindex-p index file))
        ;; For file-based indexing, we just record the file exists and its mtime
        ;; Full symbol indexing requires the package to be loaded
        (%record-file-mtime index file)
        (incf count)))
    (setf (codebase-index-updated-at index) (get-universal-time))
    count))

;;; --- ASDF System Indexing (I248) ---

(defun %default-project-root ()
  (or (ignore-errors
        (uiop:pathname-parent-directory-pathname
         (asdf:system-source-directory "amoebum")))
      (uiop:getcwd)))

(defun %loaded-system-names ()
  "Return names of already-loaded ASDF systems."
  (let ((loaded (ignore-errors (asdf:already-loaded-systems))))
    (remove-duplicates
     (append (or loaded '()) '("amoebum" "pseudopod" "ptui" "sw4rm-sdk"))
     :test #'string-equal)))

(defun %resolve-system-roots (system-names &key project-root)
  "Resolve SYSTEM-NAMES to source directory prefixes."
  (let* ((project-prefix (%directory-prefix (or project-root (%default-project-root))))
         (roots '()))
    (dolist (name system-names)
      (let* ((resolved-name (string-downcase (princ-to-string name)))
             (source-dir (ignore-errors (asdf:system-source-directory resolved-name)))
             (prefix (%directory-prefix source-dir)))
        (when (and prefix
                   (or (null project-prefix)
                       (%path-under-prefix-p prefix project-prefix)))
          (pushnew prefix roots :test #'string-equal))))
    (nreverse roots)))

(defun %collect-symbol-entries-by-file (roots &key file-filter)
  "Collect symbol entries for definitions whose source files live under ROOTS.
When FILE-FILTER is provided, only include entries whose normalized file path
exists in that hash set."
  (let ((result (make-hash-table :test #'equal))
        (package-default-file (make-hash-table :test #'equal))
        (seen-definition-keys (make-hash-table :test #'equal))
        (pending-macros '()))
    (labels ((definition-key (package-name symbol-name kind)
               (format nil "~A|~A|~A" package-name symbol-name kind))
             (record-entry (entry file)
               (setf (gethash (definition-key (symbol-entry-package entry)
                                              (symbol-entry-name entry)
                                              (symbol-entry-kind entry))
                              seen-definition-keys)
                     t)
               (unless (gethash (symbol-entry-package entry) package-default-file)
                 (setf (gethash (symbol-entry-package entry) package-default-file) file))
               (push entry (gethash file result))))
      (dolist (pkg (list-all-packages))
        (do-symbols (sym pkg)
          (when (eq (symbol-package sym) pkg)
            (dolist (kind (%symbol-definition-kinds sym))
              (multiple-value-bind (file line)
                  (%symbol-source-location sym kind)
                (let ((normalized-file (%normalize-file-path file)))
                  (cond
                    ((and normalized-file
                          (%path-under-any-prefix-p normalized-file roots)
                          (or (null file-filter)
                              (gethash normalized-file file-filter)))
                     (let* ((doc (%symbol-documentation sym kind))
                            (signature (if (member kind '(:function :generic-function :macro))
                                           (%symbol-lambda-list sym)
                                           ""))
                            (entry (make-symbol-entry
                                    :name (symbol-name sym)
                                    :package (package-name pkg)
                                    :kind kind
                                    :file normalized-file
                                    :line (or line 0)
                                    :signature signature
                                    :doc (if (> (length doc) 200)
                                             (subseq doc 0 200)
                                             doc))))
                       (record-entry entry normalized-file)))
                    ((and (eq kind :macro)
                          (macro-function sym))
                     ;; Some SBCL macro definitions do not report a source pathname.
                     ;; Keep a deferred macro candidate and attach it to the package's
                     ;; first known file after the scan.
                     (push (list :symbol sym :package (package-name pkg))
                           pending-macros))))))))))
    (dolist (pending pending-macros)
      (let* ((package-name (getf pending :package))
             (sym (getf pending :symbol))
             (fallback-file (gethash package-name package-default-file))
             (definition-key (format nil "~A|~A|~A" package-name (symbol-name sym) :macro)))
        (when (and fallback-file
                   (or (null file-filter)
                       (gethash fallback-file file-filter))
                   (not (gethash definition-key seen-definition-keys)))
          (setf (gethash definition-key seen-definition-keys) t)
          (push (make-symbol-entry
                 :name (symbol-name sym)
                 :package package-name
                 :kind :macro
                 :file fallback-file
                 :line 0
                 :signature (%symbol-lambda-list sym)
                 :doc (let ((doc (%symbol-documentation sym :macro)))
                        (if (> (length doc) 200)
                            (subseq doc 0 200)
                            doc)))
                (gethash fallback-file result)))))
    result))

(defun %flatten-entry-map (entry-map)
  (let ((entries '()))
    (maphash (lambda (_file file-entries)
               (declare (ignore _file))
               (setf entries (nconc file-entries entries)))
             entry-map)
    (sort entries #'string<
          :key (lambda (entry)
                 (format nil "~A:~A:~A"
                         (symbol-entry-package entry)
                         (symbol-entry-name entry)
                         (symbol-entry-kind entry))))))

(defun %changed-files (index files &key refresh)
  (if refresh
      files
      (remove-if-not (lambda (file)
                       (%file-needs-reindex-p index file))
                     files)))

(defun %deleted-files (index current-files)
  (let ((current-set (%hash-set-from-list current-files))
        (deleted '()))
    (maphash (lambda (file _mtime)
               (declare (ignore _mtime))
               (unless (gethash file current-set)
                 (push file deleted)))
             (codebase-index-file-mtimes index))
    deleted))

(defun index-loaded-asdf-systems (index &key systems (refresh nil)
                                    (repo-map-token-target +default-repo-map-token-target+))
  "Index definitions from loaded ASDF systems using sb-introspect.

Returns two values:
  1) INDEX (updated in place)
  2) stats plist with keys:
     :systems, :files-tracked, :files-changed, :files-deleted,
     :entries, :repo-map-tokens, :reindexed-p."
  (let* ((project-root (or (codebase-index-project-root index)
                           (%default-project-root)))
         (effective-systems (or systems (%loaded-system-names)))
         (roots (%resolve-system-roots effective-systems :project-root project-root))
         (all-files (remove-duplicates
                     (loop for root in roots append (%collect-lisp-files-recursive root))
                     :test #'equal))
         (changed-files (%changed-files index all-files :refresh refresh))
         (deleted-files (%deleted-files index all-files))
         (full-rebuild-p (or refresh
                             (null (codebase-index-entries index))
                             (null (codebase-index-indexed-systems index))))
         (reindexed-p (or full-rebuild-p
                          (plusp (length changed-files))
                          (plusp (length deleted-files))))
         (entry-map (if (and reindexed-p (not full-rebuild-p))
                        (%copy-hash-table (codebase-index-entries-by-file index)
                                          :value-transform #'copy-list)
                        (make-hash-table :test #'equal))))
    (unless roots
      (let ((stats (list :systems 0
                         :files-tracked 0
                         :files-changed 0
                         :files-deleted 0
                         :entries (length (codebase-index-entries index))
                         :repo-map-tokens (codebase-index-repo-map-token-estimate index)
                         :reindexed-p nil)))
        (setf (codebase-index-last-run-stats index) stats)
        (return-from index-loaded-asdf-systems (values index stats))))
    (when reindexed-p
      (when (not full-rebuild-p)
        (dolist (deleted deleted-files)
          (remhash deleted entry-map))
        (dolist (changed changed-files)
          (remhash changed entry-map)))
      (let ((scanned (%collect-symbol-entries-by-file roots
                                                      :file-filter (and (not full-rebuild-p)
                                                                        (%hash-set-from-list changed-files)))))
        (maphash (lambda (file entries)
                   (setf (gethash file entry-map) entries))
                 scanned))
      (clrhash (codebase-index-file-mtimes index))
      (dolist (file all-files)
        (%record-file-mtime index file))
      (setf (codebase-index-entries-by-file index) entry-map
            (codebase-index-entries index) (%flatten-entry-map entry-map)
            (codebase-index-updated-at index) (get-universal-time)))
    (setf (codebase-index-indexed-systems index)
          (mapcar (lambda (name)
                    (string-downcase (princ-to-string name)))
                  effective-systems))
    (let* ((repo-map (generate-repo-map index :max-tokens repo-map-token-target))
           (repo-tokens (ceiling (length repo-map) 4))
           (stats (list :systems (length roots)
                        :files-tracked (length all-files)
                        :files-changed (length changed-files)
                        :files-deleted (length deleted-files)
                        :entries (length (codebase-index-entries index))
                        :repo-map-tokens repo-tokens
                        :reindexed-p reindexed-p)))
      (setf (codebase-index-repo-map index) repo-map
            (codebase-index-repo-map-token-estimate index) repo-tokens
            (codebase-index-last-run-stats index) stats)
      (values index stats))))

(defun ensure-project-codebase-index (&key (refresh nil)
                                     systems
                                     (repo-map-token-target +default-repo-map-token-target+)
                                     project-root)
  "Ensure `*active-codebase-index*` exists and is current."
  (unless *active-codebase-index*
    (setf *active-codebase-index*
          (make-codebase-index :project-root (or project-root (%default-project-root))
                               :language :common-lisp)))
  (index-loaded-asdf-systems *active-codebase-index*
                             :systems systems
                             :refresh refresh
                             :repo-map-token-target repo-map-token-target))

;;; --- Repo Map Generation ---

(defun generate-repo-map (index &key (max-tokens +default-repo-map-token-target+)
                                   (include-docs nil))
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
                                         (format nil " - ~A" (symbol-entry-doc entry))
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
                      result)
          :indexed-systems (copy-list (codebase-index-indexed-systems index))
          :repo-map-tokens (codebase-index-repo-map-token-estimate index)
          :last-run-stats (copy-list (codebase-index-last-run-stats index)))))
