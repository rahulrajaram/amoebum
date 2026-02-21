(in-package :amoebum)

(defun %resolve-search-root (root)
  (let* ((candidate
           (cond
             ((null root) (config-project-root (current-config)))
             ((pathnamep root) root)
             ((stringp root) (pathname root))
             (t (error "ROOT must be a pathname, string, or NIL. Got ~S." root))))
         (resolved (or (ignore-errors (truename candidate)) candidate))
         (directory (uiop:ensure-directory-pathname resolved)))
    (unless (probe-file directory)
      (error "Search root does not exist: ~A" (%path-text directory)))
    directory))

(defun %ensure-non-negative-integer (name value)
  (when (and value (< value 0))
    (error "~A must be non-negative, got ~S." name value))
  value)

(defun %recursive-glob-pattern-p (pattern)
  (not (null (search "**" pattern :test #'char=))))

(defun %normalized-path-text (path)
  (%normalize-slashes (%path-text path)))

(defun %relative-path-text (path root)
  (let ((relative (%normalize-slashes (enough-namestring path root))))
    (if (uiop:string-prefix-p "./" relative)
        (subseq relative 2)
        relative)))

(defun %path-mtime (path)
  (or (ignore-errors (file-write-date path)) 0))

(defun %regular-file-p (path)
  (let ((probed (probe-file path)))
    (and probed
         (not (uiop:directory-pathname-p probed)))))

(defun %collect-files-recursive (root)
  (let ((visited (make-hash-table :test #'equal))
        (results '()))
    (labels ((directory-key (directory)
               (let* ((resolved (or (ignore-errors (truename directory)) directory))
                      (as-dir (uiop:ensure-directory-pathname resolved)))
                 (%normalized-path-text as-dir)))
             (walk (directory)
               (let ((key (directory-key directory)))
                 (unless (gethash key visited)
                   (setf (gethash key visited) t)
                   (dolist (file (or (ignore-errors (uiop:directory-files directory)) '()))
                     (push file results))
                   (dolist (subdir (or (ignore-errors (uiop:subdirectories directory)) '()))
                     (walk subdir))))))
      (walk root))
    results))

(defun %glob-candidates (root pattern)
  (if (%recursive-glob-pattern-p pattern)
      (%collect-files-recursive root)
      (or (ignore-errors
            (directory (merge-pathnames (pathname pattern) root)))
          '())))

(defun %matching-files-sorted (tool root pattern &key limit)
  (let* ((pattern-path (merge-pathnames (pathname pattern) root))
         (scanner (cl-ppcre:create-scanner (%glob->regex pattern-path)))
         (seen (make-hash-table :test #'equal))
         (matches '()))
    (dolist (candidate (%glob-candidates root pattern))
      (when (%regular-file-p candidate)
        (let* ((resolved (or (ignore-errors (truename candidate)) candidate))
               (key (%normalized-path-text resolved))
               (relative (%relative-path-text resolved root)))
          (unless (gethash key seen)
            (setf (gethash key seen) t)
            (when (cl-ppcre:scan scanner key)
              (%ensure-tool-path-allowed tool resolved)
              (push resolved matches))))))
    (let ((sorted (sort matches #'> :key #'%path-mtime)))
      (if limit
          (subseq sorted 0 (min limit (length sorted)))
          sorted))))

(defun %read-file-content-safe (path)
  (handler-case
      (uiop:read-file-string path :external-format :utf-8)
    (error () "")))

(defun %search-documents-for-files (files)
  (mapcar (lambda (file)
            (ptui.search.engine:make-search-document
             :path (%path-text file)
             :content (%read-file-content-safe file)))
          files))

(defun %mtime-from-table (table key)
  (multiple-value-bind (value present-p)
      (gethash key table)
    (if present-p value 0)))

(defun %content-match-better-p (left right mtime-table)
  (let* ((left-path (ptui.search.engine:search-content-match-path left))
         (right-path (ptui.search.engine:search-content-match-path right))
         (left-mtime (%mtime-from-table mtime-table left-path))
         (right-mtime (%mtime-from-table mtime-table right-path))
         (left-score (ptui.search.engine:search-content-match-score left))
         (right-score (ptui.search.engine:search-content-match-score right))
         (left-line (ptui.search.engine:search-content-match-line left))
         (right-line (ptui.search.engine:search-content-match-line right))
         (left-column (ptui.search.engine:search-content-match-column left))
         (right-column (ptui.search.engine:search-content-match-column right)))
    (cond
      ((> left-mtime right-mtime) t)
      ((< left-mtime right-mtime) nil)
      ((> left-score right-score) t)
      ((< left-score right-score) nil)
      ((string< left-path right-path) t)
      ((string< right-path left-path) nil)
      ((< left-line right-line) t)
      ((> left-line right-line) nil)
      (t
       (< left-column right-column)))))

(defun %search-widget-match->plist (match root mtime-table)
  (let* ((path-text (ptui.search.engine:search-content-match-path match))
         (path (pathname path-text))
         (modified-at (%mtime-from-table mtime-table path-text)))
    (list :path path-text
          :relative-path (%relative-path-text path root)
          :modified-at modified-at
          :line (ptui.search.engine:search-content-match-line match)
          :column (ptui.search.engine:search-content-match-column match)
          :text (ptui.search.engine:search-content-match-text match)
          :matched-text (ptui.search.engine:search-content-match-matched-text match)
          :context-before (ptui.search.engine:search-content-match-context-before match)
          :context-after (ptui.search.engine:search-content-match-context-after match))))

(defun %grep-matches-via-search-widget (files pattern before after limit case-insensitive root)
  (let ((mtime-table (make-hash-table :test #'equal)))
    (dolist (file files)
      (setf (gethash (%path-text file) mtime-table)
            (%path-mtime file)))
    (let* ((state (ptui.components.search-widget:make-search-widget-state
                   :mode :content
                   :visible-count (max 1 (or limit 200))
                   :limit nil
                   :regex-mode t
                   :case-insensitive case-insensitive
                   :multiline-mode nil
                   :before-context before
                   :after-context after))
           (documents (%search-documents-for-files files)))
      (ptui.components.search-widget:search-widget-start-content-search
       state
       pattern
       documents
       :limit nil
       :regex-mode t
       :case-insensitive case-insensitive
       :multiline-mode nil
       :before-context before
       :after-context after)
      (let* ((raw (copy-list (ptui.components.search-widget:search-widget-content-results state)))
             (sorted (sort raw
                           (lambda (left right)
                             (%content-match-better-p left right mtime-table))))
             (limited (if limit
                          (subseq sorted 0 (min limit (length sorted)))
                          sorted)))
        (mapcar (lambda (match)
                  (%search-widget-match->plist match root mtime-table))
                limited)))))

(deftool glob-files ((pattern string :description "Glob pattern to match" :required t)
                     (root (or null pathname) :description "Search root directory" :default nil)
                     (limit (or null integer) :description "Maximum results to return" :default 200))
  "Find files matching glob patterns and return results sorted by modification time."
  (:permission :auto)
  (:dangerous nil)
  (:category :search)
  (:timeout 30)
  (%ensure-non-negative-integer "LIMIT" limit)
  (let* ((root-path (%resolve-search-root root))
         (matches (%matching-files-sorted :glob-files root-path pattern :limit limit)))
    (list :root (%path-text root-path)
          :pattern pattern
          :count (length matches)
          :matches
          (mapcar (lambda (path)
                    (list :path (%path-text path)
                          :relative-path (%relative-path-text path root-path)
                          :modified-at (%path-mtime path)))
                  matches))))

(deftool grep-content ((pattern string :description "Regular expression to search for" :required t)
                       (path-glob string :description "Glob filter for candidate files" :default "**/*")
                       (root (or null pathname) :description "Search root directory" :default nil)
                       (before integer :description "Context lines before each match" :default 0)
                       (after integer :description "Context lines after each match" :default 0)
                       (limit (or null integer) :description "Maximum matches to return" :default 200)
                       (case-insensitive boolean :description "Enable case-insensitive regex matching" :default nil))
  "Search file contents with regex and include line-numbered context for each match."
  (:permission :auto)
  (:dangerous nil)
  (:category :search)
  (:timeout 60)
  (%ensure-non-negative-integer "BEFORE" before)
  (%ensure-non-negative-integer "AFTER" after)
  (%ensure-non-negative-integer "LIMIT" limit)
  (let* ((root-path (%resolve-search-root root))
         (files (%matching-files-sorted :grep-content root-path path-glob :limit nil))
         (matches (%grep-matches-via-search-widget files
                                                   pattern
                                                   before
                                                   after
                                                   limit
                                                   case-insensitive
                                                   root-path)))
    (list :root (%path-text root-path)
          :pattern pattern
          :path-glob path-glob
          :count (length matches)
          :matches matches)))
