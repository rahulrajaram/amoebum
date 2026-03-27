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
      (error "Search root does not exist: ~A" (coerce-path-string directory)))
    directory))

(defun %ensure-non-negative-integer (name value)
  (when (and value (< value 0))
    (error "~A must be non-negative, got ~S." name value))
  value)

(defun %recursive-glob-pattern-p (pattern)
  (not (null (search "**" pattern :test #'char=))))

(defun %normalized-path-text (path)
  (%normalize-slashes (coerce-path-string path)))

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
              (push resolved matches))))))
    (let ((sorted (sort matches #'> :key #'%path-mtime)))
      (if limit
          (subseq sorted 0 (min limit (length sorted)))
          sorted))))

(defparameter +search-max-file-bytes+ 262144
  "Skip files larger than 256 KB to avoid heap pressure during grep.")

(defun %read-file-content-safe (path)
  (handler-case
      (let ((size (ignore-errors (with-open-file (s path) (file-length s)))))
        (if (and size (> size +search-max-file-bytes+))
            ""
            (uiop:read-file-string path :external-format :utf-8)))
    (error () "")))

(defun %make-file-search-document-stream (files)
  "Create a lazy document stream so fallback grep does not preload every file."
  (let* ((vector (coerce files 'vector))
         (cursor 0)
         (total (length vector)))
    (ptui.components.search-widget:make-search-widget-stream
     :total-items total
     :next (lambda ()
             (if (>= cursor total)
                 (values nil t)
                 (let* ((file (aref vector cursor))
                        (done-p (= (1+ cursor) total))
                        (document
                          (ptui.search.engine:make-search-document
                           :path (coerce-path-string file)
                           :content (%read-file-content-safe file))))
                   (incf cursor)
                   (values document done-p))))
     :cancel (lambda ()
               (setf cursor total)
               t))))

;;; --- ripgrep-backed grep ---

(defun %rg-executable ()
  "Return the path to rg, or NIL if not found."
  (let ((candidates (list (merge-pathnames ".cargo/bin/rg"
                                           (user-homedir-pathname))
                          #P"/usr/bin/rg"
                          #P"/usr/local/bin/rg")))
    (find-if #'probe-file candidates)))

(defun %rg-build-argv (pattern root-path path-glob before after limit
                       case-insensitive multiline-mode output-mode)
  "Build argument list for rg --json invocation."
  (declare (ignore root-path output-mode))
  (let ((args (list "--json" "--no-heading")))
    (when (and before (plusp before))
      (push (format nil "-B~D" before) args))
    (when (and after (plusp after))
      (push (format nil "-A~D" after) args))
    (when case-insensitive
      (push "-i" args))
    (when multiline-mode
      (push "--multiline" args))
    ;; File glob filter (rg uses --glob, not shell globs)
    (when (and path-glob (not (string= path-glob "**/*")))
      (push "--glob" args)
      (push path-glob args))
    ;; The pattern and search root
    (nconc (nreverse args) (list "--" pattern "."))))

(defun %rg-strip-newline (text)
  "Remove trailing newline from rg line text."
  (if (and (plusp (length text))
           (char= (char text (1- (length text))) #\Newline))
      (subseq text 0 (1- (length text)))
      text))

(defun %rg-resolve-path (path-text root-path)
  "Resolve an rg-reported path against ROOT-PATH, preserving absolute paths."
  (let* ((path (pathname path-text))
         (resolved (if (uiop:absolute-pathname-p path)
                       path
                       (merge-pathnames path root-path))))
    (or (ignore-errors (truename resolved)) resolved)))

(defun %rg-parse-json-lines (stream root-path limit)
  "Parse rg --json output. Returns (values matches file-count match-count)."
  (let ((matches '())
        (match-count 0)
        (files-seen (make-hash-table :test #'equal))
        (context-before '())
        (context-after-target nil))
    (labels ((flush-context-after ()
               ;; Attach accumulated context-after to the previous match
               (when context-after-target
                 (setf (getf context-after-target :context-after)
                       (nreverse (getf context-after-target :context-after)))
                 (setf context-after-target nil)))
             (process-line (line)
               (let* ((json (handler-case (jonathan:parse line :as :hash-table)
                              (error () (return-from process-line))))
                      (type (gethash "type" json)))
                 (cond
                   ((string= type "context")
                    (let* ((data (gethash "data" json))
                           (line-number (gethash "line_number" data 0))
                           (text (%rg-strip-newline
                                  (gethash "text" (gethash "lines" data) "")))
                           (context-entry (list :line line-number :text text)))
                      (if context-after-target
                          ;; Accumulating context-after for previous match
                          (push context-entry (getf context-after-target :context-after))
                          ;; Accumulating context-before for next match
                          (push context-entry context-before))))
                   ((string= type "match")
                    (flush-context-after)
                    (when (and limit (>= match-count limit))
                      (return-from process-line))
                    (let* ((data (gethash "data" json))
                           (path-text (gethash "text" (gethash "path" data) ""))
                           (resolved-path (%rg-resolve-path path-text root-path))
                           (resolved-path-text (coerce-path-string resolved-path))
                           (line-number (gethash "line_number" data 0))
                           (line-text (%rg-strip-newline
                                       (gethash "text" (gethash "lines" data) "")))
                           (submatches (gethash "submatches" data))
                           (first-sub (and submatches (plusp (length submatches))
                                           (elt submatches 0)))
                           (matched-text (if first-sub
                                             (gethash "text" (gethash "match" first-sub) "")
                                             ""))
                           (column (if first-sub
                                       (1+ (gethash "start" first-sub 0))
                                       1))
                           (relative (%relative-path-text resolved-path root-path))
                           (match-plist
                             (list :path resolved-path-text
                                   :relative-path relative
                                   :modified-at (%path-mtime resolved-path)
                                   :line line-number
                                   :column column
                                   :text line-text
                                   :matched-text matched-text
                                   :context-before (nreverse context-before)
                                   :context-after '())))
                      (setf (gethash resolved-path-text files-seen) t)
                      (push match-plist matches)
                      (incf match-count)
                      (setf context-before '())
                      (setf context-after-target match-plist)))
                   ((string= type "begin")
                    (setf context-before '())
                    (flush-context-after))
                   ((string= type "end")
                    (flush-context-after)
                    (setf context-before '()))))))
      (loop for line = (read-line stream nil nil)
            while line
            do (process-line line))
      (flush-context-after))
    (values (nreverse matches)
            (hash-table-count files-seen)
            match-count)))

(defun %grep-via-rg (pattern root-path path-glob before after limit
                     case-insensitive multiline-mode output-mode)
  "Run ripgrep and parse results. Returns (values matches file-count match-count)."
  (let* ((rg (%rg-executable))
         (argv (%rg-build-argv pattern root-path path-glob
                               before after limit
                               case-insensitive multiline-mode output-mode))
         (cmd (cons (namestring rg) argv)))
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program cmd
                          :output :string
                          :error-output :string
                          :directory root-path
                          :ignore-error-status t)
      (declare (ignore error-output))
      ;; rg exits 0=matches, 1=no matches, 2=error
      (if (> exit-code 1)
          (values '() 0 0)
          (with-input-from-string (s output)
            (multiple-value-bind (matches file-count match-count)
                (%rg-parse-json-lines s root-path limit)
              (values matches file-count match-count)))))))

(defun %mtime-from-table (table key)
  (multiple-value-bind (value present-p)
      (gethash key table)
    (if present-p value 0)))

(defun %grep-match-better-p (left right)
  (let ((left-mtime (or (getf left :modified-at) 0))
        (right-mtime (or (getf right :modified-at) 0))
        (left-path (or (getf left :path) ""))
        (right-path (or (getf right :path) ""))
        (left-line (or (getf left :line) most-positive-fixnum))
        (right-line (or (getf right :line) most-positive-fixnum))
        (left-column (or (getf left :column) most-positive-fixnum))
        (right-column (or (getf right :column) most-positive-fixnum)))
    (cond
      ((> left-mtime right-mtime) t)
      ((< left-mtime right-mtime) nil)
      ((string< left-path right-path) t)
      ((string< right-path left-path) nil)
      ((< left-line right-line) t)
      ((> left-line right-line) nil)
      (t
       (< left-column right-column)))))

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

(defun %normalize-grep-output-mode (mode)
  (cond
    ((or (null mode) (eq mode :content))
     :content)
    ((member mode '(:files_with_matches :count) :test #'eq)
     mode)
    ((stringp mode)
     (let ((normalized (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) mode))))
       (cond
         ((string= normalized "content") :content)
         ((string= normalized "files_with_matches") :files_with_matches)
         ((string= normalized "count") :count)
         (t (error "Unsupported OUTPUT-MODE ~S. Expected content, files_with_matches, or count."
                   mode)))))
    (t
     (error "Unsupported OUTPUT-MODE ~S. Expected content, files_with_matches, or count."
            mode))))

(defun %grep-files-with-matches (matches)
  (let ((seen (make-hash-table :test #'equal))
        (files '()))
    (dolist (match (sort (copy-list matches) #'%grep-match-better-p))
      (let ((path (getf match :path)))
        (unless (gethash path seen)
          (setf (gethash path seen) t)
          (push path files))))
    (nreverse files)))

(defun %grep-output-payload (mode matches)
  (let* ((sorted-matches (sort (copy-list matches) #'%grep-match-better-p))
         (files (%grep-files-with-matches sorted-matches))
         (match-count (length sorted-matches))
         (file-count (length files)))
    (append
     (list :output-mode mode
           :match-count match-count
           :file-count file-count)
     (ecase mode
       (:content
        (list :count match-count
              :matches sorted-matches))
       (:files_with_matches
        (list :count file-count
              :matches files))
       (:count
        (list :count match-count))))))

(defun %grep-matches-via-search-widget (files pattern before after limit case-insensitive multiline-mode root)
  (let ((mtime-table (make-hash-table :test #'equal)))
    (dolist (file files)
      (setf (gethash (coerce-path-string file) mtime-table)
            (%path-mtime file)))
    (let* ((state (ptui.components.search-widget:make-search-widget-state
                   :mode :content
                   :visible-count (max 1 (or limit 200))
                   :limit nil
                   :regex-mode t
                   :case-insensitive case-insensitive
                   :multiline-mode multiline-mode
                   :before-context before
                   :after-context after))
           (stream (%make-file-search-document-stream files))
           (batch-size (ptui.components.search-widget:search-widget-batch-size state)))
      (ptui.components.search-widget:search-widget-start-content-search-stream
       state
       pattern
       stream
       :limit nil
       :regex-mode t
       :case-insensitive case-insensitive
       :multiline-mode multiline-mode
       :before-context before
       :after-context after)
      (loop while (eq (ptui.components.search-widget:search-widget-status state) :streaming)
            do (ptui.components.search-widget:search-widget-step state
                                                                 :max-items batch-size))
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
    (list :root (coerce-path-string root-path)
          :pattern pattern
          :count (length matches)
          :matches
          (mapcar (lambda (path)
                    (list :path (coerce-path-string path)
                          :relative-path (%relative-path-text path root-path)
                          :modified-at (%path-mtime path)))
                  matches))))

(deftool grep-content ((pattern string :description "Regular expression to search for" :required t)
                       (path-glob string :description "Glob filter for candidate files" :default "**/*")
                       (root (or null pathname) :description "Search root directory" :default nil)
                       (before integer :description "Context lines before each match" :default 0)
                       (after integer :description "Context lines after each match" :default 0)
                       (limit (or null integer) :description "Maximum matches to return" :default 200)
                       (case-insensitive boolean :description "Enable case-insensitive regex matching" :default nil)
                       (multiline boolean :description "Enable multiline regex matching where patterns may span lines" :default nil)
                       (output-mode (member :content :files_with_matches :count)
                                    :description "Result mode: :content matches, :files_with_matches paths-only, or :count"
                                    :default :content))
  "Search file contents with regex and include line-numbered context for each match."
  (:permission :auto)
  (:dangerous nil)
  (:category :search)
  (:timeout 60)
  (%ensure-non-negative-integer "BEFORE" before)
  (%ensure-non-negative-integer "AFTER" after)
  (%ensure-non-negative-integer "LIMIT" limit)
  (let* ((mode (%normalize-grep-output-mode output-mode))
         (root-path (%resolve-search-root root)))
    (if (%rg-executable)
        ;; Fast path: use ripgrep
        (multiple-value-bind (matches file-count match-count)
            (%grep-via-rg pattern root-path path-glob
                          before after limit
                          case-insensitive multiline mode)
          (append
           (list :root (coerce-path-string root-path)
                 :pattern pattern
                 :path-glob path-glob
                 :multiline multiline)
           (%grep-output-payload mode matches)))
        ;; Fallback: in-process search widget (old path)
        (let* ((files (%matching-files-sorted :grep-content root-path path-glob :limit nil))
               (matches (%grep-matches-via-search-widget files
                                                         pattern
                                                         before
                                                         after
                                                         limit
                                                         case-insensitive
                                                         multiline
                                                         root-path)))
          (append
           (list :root (coerce-path-string root-path)
                 :pattern pattern
                 :path-glob path-glob
                 :multiline multiline)
           (%grep-output-payload mode matches))))))
