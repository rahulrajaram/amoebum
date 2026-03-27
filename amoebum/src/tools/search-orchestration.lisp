(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; Search Orchestration (I122)
;;;
;;; Combines backend search results, applies deterministic filtering, and
;;; returns a stable merged result shape for LLM tool consumption.
;;; ---------------------------------------------------------------------------

(defparameter *search-orchestration-default-limit* 200
  "Default merged result limit when LIMIT is NIL.")

(defparameter *search-orchestration-max-limit* 1000
  "Maximum allowed merged result limit.")

(defstruct (search-orchestration-hit
            (:constructor make-search-orchestration-hit
                (&key backend kind path relative-path modified-at
                 line column text matched-text context-before context-after)))
  backend
  kind
  path
  relative-path
  modified-at
  line
  column
  text
  matched-text
  context-before
  context-after)

(defun %search-orchestration-trim (value)
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (or (and value (princ-to-string value)) "")))

(defun %search-orchestration-empty-string-p (value)
  (zerop (length (%search-orchestration-trim value))))

(defun %search-orchestration-normalize-query (query)
  (let ((normalized (%search-orchestration-trim query)))
    (when (zerop (length normalized))
      (error "QUERY must not be empty."))
    normalized))

(defun %search-orchestration-normalize-limit (limit)
  (%ensure-non-negative-integer "LIMIT" limit)
  (let ((effective (or limit *search-orchestration-default-limit*)))
    (when (> effective *search-orchestration-max-limit*)
      (error "LIMIT ~D exceeds maximum ~D."
             effective
             *search-orchestration-max-limit*))
    effective))

(defun %search-orchestration-normalize-extensions (extensions)
  (let ((parts
          (cond
            ((null extensions) nil)
            ((stringp extensions)
             (remove-if #'%search-orchestration-empty-string-p
                        (cl-ppcre:split "[,\\s]+" extensions)))
            ((listp extensions)
             (remove-if #'%search-orchestration-empty-string-p
                        (mapcar #'%search-orchestration-trim extensions)))
            (t
             (error "EXTENSIONS must be NIL, string, or list of strings.")))))
    (remove-duplicates
     (mapcar (lambda (entry)
               (let* ((trimmed (%search-orchestration-trim entry))
                      (without-dot (if (and (> (length trimmed) 0)
                                            (char= (char trimmed 0) #\.))
                                       (subseq trimmed 1)
                                       trimmed)))
                 (string-downcase without-dot)))
             parts)
     :test #'string=)))

(defun %search-orchestration-file-extension (path-text)
  (let* ((type (pathname-type (pathname path-text))))
    (and type (string-downcase (princ-to-string type)))))

(defun %search-orchestration-extension-allowed-p (hit extensions)
  (if (null extensions)
      t
      (member (%search-orchestration-file-extension
               (search-orchestration-hit-path hit))
              extensions
              :test #'string=)))

(defun %search-orchestration-path-match-p (query candidate &key case-insensitive)
  (and (stringp candidate)
       (search query candidate :test (if case-insensitive #'char-equal #'char=))))

(defun %search-orchestration-file-hits (files root query &key case-insensitive)
  (let ((hits '()))
    (dolist (file files)
      (let* ((path-text (coerce-path-string file))
             (relative-path (%relative-path-text file root)))
        (when (%search-orchestration-path-match-p query
                                                  relative-path
                                                  :case-insensitive case-insensitive)
          (push (make-search-orchestration-hit
                 :backend :files
                 :kind :file
                 :path path-text
                 :relative-path relative-path
                 :modified-at (%path-mtime file))
                hits))))
    hits))

(defun %search-orchestration-content-hit-from-plist (entry)
  (make-search-orchestration-hit
   :backend :content
   :kind :content
   :path (getf entry :path)
   :relative-path (getf entry :relative-path)
   :modified-at (getf entry :modified-at)
   :line (getf entry :line)
   :column (getf entry :column)
   :text (getf entry :text)
   :matched-text (getf entry :matched-text)
   :context-before (getf entry :context-before)
   :context-after (getf entry :context-after)))

(defun %search-orchestration-content-match-plists (files root path-glob query before after case-insensitive
                                                   use-rg-content)
  (if use-rg-content
      (multiple-value-bind (matches file-count match-count)
          (%grep-via-rg query root path-glob
                        before after nil
                        case-insensitive nil :content)
        (declare (ignore file-count match-count))
        matches)
      (%grep-matches-via-search-widget files
                                       query
                                       before
                                       after
                                       nil
                                       case-insensitive
                                       nil
                                       root)))

(defun %search-orchestration-content-hits (files root path-glob query before after case-insensitive
                                           use-rg-content)
  (mapcar #'%search-orchestration-content-hit-from-plist
          (%search-orchestration-content-match-plists files
                                                      root
                                                      path-glob
                                                      query
                                                      before
                                                      after
                                                      case-insensitive
                                                      use-rg-content)))

(defun %search-orchestration-backend-rank (backend)
  (ecase backend
    (:content 0)
    (:files 1)))

(defun %search-orchestration-hit-better-p (left right)
  (let ((left-mtime (or (search-orchestration-hit-modified-at left) 0))
        (right-mtime (or (search-orchestration-hit-modified-at right) 0)))
    (cond
      ((> left-mtime right-mtime) t)
      ((< left-mtime right-mtime) nil)
      ((< (%search-orchestration-backend-rank (search-orchestration-hit-backend left))
          (%search-orchestration-backend-rank (search-orchestration-hit-backend right)))
       t)
      ((> (%search-orchestration-backend-rank (search-orchestration-hit-backend left))
          (%search-orchestration-backend-rank (search-orchestration-hit-backend right)))
       nil)
      ((string< (search-orchestration-hit-path left)
                (search-orchestration-hit-path right))
       t)
      ((string< (search-orchestration-hit-path right)
                (search-orchestration-hit-path left))
       nil)
      ((< (or (search-orchestration-hit-line left) most-positive-fixnum)
          (or (search-orchestration-hit-line right) most-positive-fixnum))
       t)
      ((> (or (search-orchestration-hit-line left) most-positive-fixnum)
          (or (search-orchestration-hit-line right) most-positive-fixnum))
       nil)
      (t
       (< (or (search-orchestration-hit-column left) most-positive-fixnum)
          (or (search-orchestration-hit-column right) most-positive-fixnum))))))

(defun %search-orchestration-hit->plist (hit)
  (append
   (list :backend (search-orchestration-hit-backend hit)
         :kind (search-orchestration-hit-kind hit)
         :path (search-orchestration-hit-path hit)
         :relative-path (search-orchestration-hit-relative-path hit)
         :modified-at (search-orchestration-hit-modified-at hit))
   (when (eq (search-orchestration-hit-kind hit) :content)
     (list :line (search-orchestration-hit-line hit)
           :column (search-orchestration-hit-column hit)
           :text (search-orchestration-hit-text hit)
           :matched-text (search-orchestration-hit-matched-text hit)
           :context-before (search-orchestration-hit-context-before hit)
           :context-after (search-orchestration-hit-context-after hit)))))

(defun orchestrate-search (query &key
                                 root
                                 (path-glob "**/*")
                                 (include-content t)
                                 (include-files t)
                                 extensions
                                 (before 0)
                                 (after 0)
                                 (case-insensitive nil)
                                 limit)
  "Run merged search orchestration over file-path and content backends."
  (%ensure-non-negative-integer "BEFORE" before)
  (%ensure-non-negative-integer "AFTER" after)
  (let* ((normalized-query (%search-orchestration-normalize-query query))
         (effective-limit (%search-orchestration-normalize-limit limit))
         (root-path (%resolve-search-root root))
         (normalized-extensions (%search-orchestration-normalize-extensions extensions))
         (use-rg-content (and include-content (%rg-executable))))
    (unless (or include-content include-files)
      (error "At least one backend must be enabled (INCLUDE-CONTENT or INCLUDE-FILES)."))
    (let* ((file-candidates
             (and include-files
                  (%matching-files-sorted :glob-files root-path path-glob :limit nil)))
           (content-candidates
             (and include-content
                  (not use-rg-content)
                  (%matching-files-sorted :grep-content root-path path-glob :limit nil)))
           (file-hits
             (if include-files
                 (%search-orchestration-file-hits file-candidates
                                                  root-path
                                                  normalized-query
                                                  :case-insensitive case-insensitive)
                 '()))
           (content-hits
             (if include-content
                 (%search-orchestration-content-hits content-candidates
                                                    root-path
                                                    path-glob
                                                    normalized-query
                                                    before
                                                    after
                                                    case-insensitive
                                                    use-rg-content)
                 '()))
           (merged (append content-hits file-hits))
           (filtered
             (remove-if-not (lambda (hit)
                              (%search-orchestration-extension-allowed-p
                               hit
                               normalized-extensions))
                            merged))
           (sorted (sort filtered #'%search-orchestration-hit-better-p))
           (limited (subseq sorted 0 (min effective-limit (length sorted))))
           (results (mapcar #'%search-orchestration-hit->plist limited)))
      (list :query normalized-query
            :root (coerce-path-string root-path)
            :path-glob path-glob
            :limit effective-limit
            :extensions normalized-extensions
            :include-content include-content
            :include-files include-files
            :backend-counts (list :content (length content-hits)
                                  :files (length file-hits))
            :count (length results)
            :results results))))

(deftool search-project ((query string :description "Search query (regex for content and substring for file paths)." :required t)
                         (root (or null pathname) :description "Search root directory." :default nil)
                         (path-glob string :description "Glob filter applied before backend search." :default "**/*")
                         (include-content boolean :description "Include content-search backend results." :default t)
                         (include-files boolean :description "Include file-path backend results." :default t)
                         (extensions (or null string) :description "Optional extension filter (comma/space separated)." :default nil)
                         (before integer :description "Context lines before each content match." :default 0)
                         (after integer :description "Context lines after each content match." :default 0)
                         (case-insensitive boolean :description "Enable case-insensitive matching." :default nil)
                         (limit (or null integer) :description "Maximum merged results to return." :default nil))
  "Run combined backend search orchestration with deterministic filtering and shaping."
  (:permission :auto)
  (:dangerous nil)
  (:category :search)
  (:timeout 60)
  (orchestrate-search query
                      :root root
                      :path-glob path-glob
                      :include-content include-content
                      :include-files include-files
                      :extensions extensions
                      :before before
                      :after after
                      :case-insensitive case-insensitive
                      :limit limit))
