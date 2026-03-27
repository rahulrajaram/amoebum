(defpackage :ptui.search.engine
  (:use :cl)
  (:export
   #:search-file-match
   #:search-file-match-p
   #:make-search-file-match
   #:search-file-match-path
   #:search-file-match-score
   #:search-file-match-kind
   #:search-file-match-spans
   #:rank-file-matches
   #:search-document
   #:search-document-p
   #:make-search-document
   #:search-document-path
   #:search-document-content
   #:search-content-match
   #:search-content-match-p
   #:make-search-content-match
   #:search-content-match-path
   #:search-content-match-line
   #:search-content-match-column
   #:search-content-match-text
   #:search-content-match-matched-text
   #:search-content-match-score
   #:search-content-match-context-before
   #:search-content-match-context-after
   #:search-content-scan-result
   #:search-content-scan-result-p
   #:make-search-content-scan-result
   #:search-content-scan-result-matches
   #:search-content-scan-result-match-count
   #:search-content-scan-result-scanned-documents
   #:search-content-scan-result-total-documents
   #:search-content-scan-result-canceled-p
   #:search-content-options
   #:search-content-options-p
   #:make-search-content-options
   #:search-content-options-limit
   #:search-content-options-regex-mode
   #:search-content-options-case-insensitive
   #:search-content-options-multiline-mode
   #:search-content-options-before-context
   #:search-content-options-after-context
   #:search-content-options-on-match
   #:search-content-options-on-progress
   #:search-content-options-cancel-fn
   #:scan-content-matches
   #:search-content-matches))

(in-package :ptui.search.engine)

(defstruct (search-file-match
            (:constructor make-search-file-match
                (&key path score kind spans depth)))
  (path "" :type string)
  (score 0 :type integer)
  (kind :fuzzy :type keyword)
  (spans '() :type list)
  (depth 0 :type fixnum))

(defstruct (search-document
            (:constructor make-search-document (&key path content)))
  (path "" :type string)
  (content "" :type string))

(defstruct (search-content-match
            (:constructor make-search-content-match
                (&key path line column text matched-text score context-before context-after)))
  (path "" :type string)
  (line 1 :type fixnum)
  (column 1 :type fixnum)
  (text "" :type string)
  (matched-text "" :type string)
  (score 0 :type integer)
  (context-before '() :type list)
  (context-after '() :type list))

(defstruct (search-content-scan-result
            (:constructor make-search-content-scan-result
                (&key
                  (matches '())
                  (match-count 0)
                  (scanned-documents 0)
                  (total-documents 0)
                  (canceled-p nil))))
  (matches '() :type list)
  (match-count 0 :type fixnum)
  (scanned-documents 0 :type fixnum)
  (total-documents 0 :type fixnum)
  (canceled-p nil :type boolean))

(defstruct (search-content-options
            (:constructor make-search-content-options
                (&key
                  limit
                  (regex-mode t)
                  (case-insensitive nil)
                  (multiline-mode nil)
                  (before-context 0)
                  (after-context 0)
                  on-match
                  on-progress
                  cancel-fn)))
  limit
  (regex-mode t :type boolean)
  (case-insensitive nil :type boolean)
  (multiline-mode nil :type boolean)
  (before-context 0 :type (integer 0 *))
  (after-context 0 :type (integer 0 *))
  on-match
  on-progress
  cancel-fn)

(defstruct (search-content-runtime
            (:constructor make-search-content-runtime
                (&key scanner documents total-documents)))
  scanner
  (documents '() :type list)
  (total-documents 0 :type fixnum)
  (all-matches '() :type list)
  (match-count 0 :type fixnum)
  (scanned-documents 0 :type fixnum)
  (canceled-p nil :type boolean))

(defun %normalize-path-text (value)
  (let ((text (typecase value
                (pathname (namestring value))
                (string value)
                (t (princ-to-string value)))))
    (substitute #\/ #\\ text)))

(defun %path-depth (path)
  (count #\/ path))

(defun %range->spans (indexes)
  (if (null indexes)
      '()
      (let ((ordered (sort (copy-list indexes) #'<))
            (out '()))
        (let ((start (first ordered))
              (previous (first ordered)))
          (dolist (index (rest ordered))
            (if (= index (1+ previous))
                (setf previous index)
                (progn
                  (push (cons start (1+ previous)) out)
                  (setf start index
                        previous index))))
          (push (cons start (1+ previous)) out))
        (nreverse out))))

(defun %subsequence-score (query candidate)
  (let ((indexes '())
        (cursor 0)
        (query-length (length query))
        (candidate-length (length candidate)))
    (loop for q-index from 0 below query-length do
      (let ((found nil))
        (loop for c-index from cursor below candidate-length do
          (when (char-equal (char query q-index) (char candidate c-index))
            (setf found c-index
                  cursor (1+ c-index))
            (return)))
        (unless found
          (return-from %subsequence-score (values nil nil)))
        (push found indexes)))
    (let ((ordered (nreverse indexes))
          (score 0)
          (previous -2))
      (dolist (index ordered)
        (incf score 20)
        (when (= index (1+ previous))
          (incf score 14))
        (when (or (zerop index)
                  (find (char candidate (1- index)) "/_-." :test #'char=))
          (incf score 8))
        (when (>= previous 0)
          (decf score (min 6 (max 0 (- index previous 1)))))
        (setf previous index))
      (when ordered
        (incf score (max 0 (- 40 (first ordered)))))
      (values score (%range->spans ordered)))))

(defun %score-file-candidate (query candidate)
  (let* ((normalized-query (string-downcase (or query "")))
         (normalized-candidate (string-downcase candidate))
         (query-length (length normalized-query))
         (depth (%path-depth candidate))
         (depth-weight (max 0 (- 240 (* 20 depth)))))
    (cond
      ((zerop query-length)
       (make-search-file-match :path candidate
                               :score (+ 50000 depth-weight (- (length candidate)))
                               :kind :all
                               :spans '()
                               :depth depth))
      ((string= normalized-query normalized-candidate)
       (make-search-file-match :path candidate
                               :score (+ 400000 depth-weight (- (length candidate)))
                               :kind :exact
                               :spans (list (cons 0 (length candidate)))
                               :depth depth))
      ((uiop:string-prefix-p normalized-query normalized-candidate)
       (make-search-file-match :path candidate
                               :score (+ 300000 depth-weight (- (length candidate)))
                               :kind :prefix
                               :spans (list (cons 0 query-length))
                               :depth depth))
      (t
       (let ((substring-index (search normalized-query normalized-candidate)))
         (cond
           (substring-index
            (make-search-file-match :path candidate
                                    :score (+ 200000
                                              depth-weight
                                              (* -5 substring-index)
                                              (- (length candidate)))
                                    :kind :substring
                                    :spans (list (cons substring-index
                                                       (+ substring-index query-length)))
                                    :depth depth))
           (t
            (multiple-value-bind (subseq-score spans)
                (%subsequence-score normalized-query normalized-candidate)
              (when subseq-score
                (make-search-file-match :path candidate
                                        :score (+ 100000
                                                  depth-weight
                                                  subseq-score
                                                  (- (length candidate)))
                                        :kind :fuzzy
                                        :spans spans
                                        :depth depth))))))))))

(defun %file-match-better-p (left right)
  (cond
    ((> (search-file-match-score left) (search-file-match-score right))
     t)
    ((< (search-file-match-score left) (search-file-match-score right))
     nil)
    ((< (search-file-match-depth left) (search-file-match-depth right))
     t)
    ((> (search-file-match-depth left) (search-file-match-depth right))
     nil)
    (t
     (string< (search-file-match-path left)
              (search-file-match-path right)))))

(defun rank-file-matches (query candidates &key limit)
  "Rank CANDIDATES (paths/pathnames) against QUERY and return search-file-match entries."
  (when (and limit (< limit 0))
    (error "LIMIT must be non-negative, got ~S." limit))
  (let ((seen (make-hash-table :test #'equal))
        (ranked '()))
    (map nil
         (lambda (candidate)
           (let* ((path (%normalize-path-text candidate))
                  (unique-key (string-downcase path)))
             (unless (gethash unique-key seen)
               (setf (gethash unique-key seen) t)
               (let ((entry (%score-file-candidate query path)))
                 (when entry
                   (push entry ranked))))))
         candidates)
    (let ((sorted (sort ranked #'%file-match-better-p)))
      (if limit
          (subseq sorted 0 (min limit (length sorted)))
          sorted))))

(defun %ensure-search-document (entry)
  (cond
    ((search-document-p entry) entry)
    ((and (listp entry)
          (getf entry :path)
          (getf entry :content))
     (make-search-document :path (%normalize-path-text (getf entry :path))
                           :content (princ-to-string (getf entry :content))))
    (t
     (error "Expected SEARCH-DOCUMENT or plist with :path/:content, got ~S." entry))))

(defun %split-lines+starts (text)
  (let ((lines '())
        (starts (list 0))
        (cursor 0)
        (length* (length text)))
    (loop for index from 0 below length* do
      (when (char= (char text index) #\Newline)
        (push (subseq text cursor index) lines)
        (setf cursor (1+ index))
        (push cursor starts)))
    (push (subseq text cursor length*) lines)
    (values (coerce (nreverse lines) 'vector)
            (coerce (nreverse starts) 'vector))))

(defun %line-index-for-offset (line-starts offset)
  (let ((low 0)
        (high (1- (length line-starts)))
        (best 0))
    (loop while (<= low high) do
      (let* ((mid (truncate (+ low high) 2))
             (start (aref line-starts mid)))
        (if (<= start offset)
            (setf best mid
                  low (1+ mid))
            (setf high (1- mid)))))
    best))

(defun %context-lines (lines start end)
  (loop for index from start below end
        collect (list :line (1+ index)
                      :text (aref lines index))))

(defun %content-score (line-index column-index match-length)
  (+ 120000
     (* 250 (max 1 match-length))
     (- 1000 (* 3 line-index) column-index)))

(defun %content-match-better-p (left right)
  (cond
    ((> (search-content-match-score left) (search-content-match-score right))
     t)
    ((< (search-content-match-score left) (search-content-match-score right))
     nil)
    ((string< (search-content-match-path left) (search-content-match-path right))
     t)
    ((string< (search-content-match-path right) (search-content-match-path left))
     nil)
    ((< (search-content-match-line left) (search-content-match-line right))
     t)
    ((> (search-content-match-line left) (search-content-match-line right))
     nil)
    (t
     (< (search-content-match-column left) (search-content-match-column right)))))

(defun %line-mode-matches (scanner text line-starts)
  (let ((ranges '()))
    (dotimes (line-index (length line-starts))
      (let ((line-start (aref line-starts line-index))
            (line-end (if (< line-index (1- (length line-starts)))
                          (1- (aref line-starts (1+ line-index)))
                          (length text))))
        (let ((line (subseq text line-start line-end)))
          (cl-ppcre:do-matches (start end scanner line)
            (push (cons (+ line-start start) (+ line-start end)) ranges)))))
    (nreverse ranges)))

(defun %multiline-matches (scanner text)
  (let ((ranges '()))
    (cl-ppcre:do-matches (start end scanner text)
      (push (cons start end) ranges))
    (nreverse ranges)))

(defun %document-content-matches (scanner document before-context after-context multiline-mode)
  (let* ((path (%normalize-path-text (search-document-path document)))
         (content (search-document-content document))
         (matches '()))
    (multiple-value-bind (lines line-starts)
        (%split-lines+starts content)
      (let ((ranges (if multiline-mode
                        (%multiline-matches scanner content)
                        (%line-mode-matches scanner content line-starts))))
        (dolist (range ranges)
          (let* ((start (car range))
                 (end (cdr range))
                 (line-index (%line-index-for-offset line-starts start))
                 (line-start (aref line-starts line-index))
                 (line-text (if (< line-index (length lines))
                                (aref lines line-index)
                                ""))
                 (column-index (- start line-start))
                 (before-start (max 0 (- line-index before-context)))
                 (after-end (min (length lines) (+ line-index after-context 1)))
                 (match-text (if (<= end (length content))
                                 (subseq content start end)
                                 "")))
            (push (make-search-content-match
                   :path path
                   :line (1+ line-index)
                   :column (1+ column-index)
                   :text line-text
                   :matched-text match-text
                   :score (%content-score line-index column-index (- end start))
                   :context-before (%context-lines lines before-start line-index)
                  :context-after (%context-lines lines (1+ line-index) after-end))
                  matches)))))
    (nreverse matches)))

(defun %validate-search-content-options (options)
  (let ((limit (search-content-options-limit options))
        (before-context (search-content-options-before-context options))
        (after-context (search-content-options-after-context options))
        (on-match (search-content-options-on-match options))
        (on-progress (search-content-options-on-progress options))
        (cancel-fn (search-content-options-cancel-fn options)))
    (when (and limit (< limit 0))
      (error "LIMIT must be non-negative, got ~S." limit))
    (when (< before-context 0)
      (error "BEFORE-CONTEXT must be non-negative, got ~S." before-context))
    (when (< after-context 0)
      (error "AFTER-CONTEXT must be non-negative, got ~S." after-context))
    (when on-match
      (check-type on-match function))
    (when on-progress
      (check-type on-progress function))
    (when cancel-fn
      (check-type cancel-fn function))
    options))

(defun %resolve-search-content-options (options)
  (let ((resolved (or options (make-search-content-options))))
    (check-type resolved search-content-options)
    (%validate-search-content-options resolved)))

(defun %empty-search-content-scan-result ()
  (make-search-content-scan-result
   :matches '()
   :match-count 0
   :scanned-documents 0
   :total-documents 0
   :canceled-p nil))

(defun %make-search-content-runtime (pattern documents options)
  (let* ((effective-pattern
           (if (search-content-options-regex-mode options)
               pattern
               (cl-ppcre:quote-meta-chars pattern)))
         (normalized-documents (map 'list #'%ensure-search-document documents)))
    (make-search-content-runtime
     :scanner (cl-ppcre:create-scanner
               effective-pattern
               :case-insensitive-mode
               (search-content-options-case-insensitive options)
               :multi-line-mode (search-content-options-multiline-mode options)
               :single-line-mode (search-content-options-multiline-mode options))
     :documents normalized-documents
     :total-documents (length normalized-documents))))

(defun %content-scan-cancelled-p (runtime options)
  (let ((cancel-fn (search-content-options-cancel-fn options)))
    (when (and cancel-fn (funcall cancel-fn))
      (setf (search-content-runtime-canceled-p runtime) t))))

(defun %emit-content-scan-progress (runtime options &key (done nil) latest-match)
  (let ((on-progress (search-content-options-on-progress options)))
    (when on-progress
      (funcall on-progress
               :match-count (search-content-runtime-match-count runtime)
               :scanned-documents (search-content-runtime-scanned-documents runtime)
               :total-documents (search-content-runtime-total-documents runtime)
               :done done
               :cancelled (search-content-runtime-canceled-p runtime)
               :latest-match latest-match))))

(defun %record-content-scan-match (runtime options match)
  (incf (search-content-runtime-match-count runtime))
  (push match (search-content-runtime-all-matches runtime))
  (let ((on-match (search-content-options-on-match options)))
    (when on-match
      (funcall on-match match)))
  (%emit-content-scan-progress runtime options :latest-match match))

(defun %scan-content-document (runtime options document)
  (incf (search-content-runtime-scanned-documents runtime))
  (dolist (match (%document-content-matches
                  (search-content-runtime-scanner runtime)
                  document
                  (search-content-options-before-context options)
                  (search-content-options-after-context options)
                  (search-content-options-multiline-mode options)))
    (%record-content-scan-match runtime options match)
    (when (%content-scan-cancelled-p runtime options)
      (return-from %scan-content-document nil)))
  t)

(defun %finalize-search-content-scan (runtime options)
  (let* ((sorted (sort (search-content-runtime-all-matches runtime)
                       #'%content-match-better-p))
         (limit (search-content-options-limit options))
         (limited (if limit
                      (subseq sorted 0 (min limit (length sorted)))
                      sorted)))
    (%emit-content-scan-progress runtime options :done t)
    (make-search-content-scan-result
     :matches limited
     :match-count (search-content-runtime-match-count runtime)
     :scanned-documents (search-content-runtime-scanned-documents runtime)
     :total-documents (search-content-runtime-total-documents runtime)
     :canceled-p (search-content-runtime-canceled-p runtime))))

(defun %scan-content-documents (pattern documents options)
  (let ((runtime (%make-search-content-runtime pattern documents options)))
    (%emit-content-scan-progress runtime options)
    (dolist (document (search-content-runtime-documents runtime))
      (when (%content-scan-cancelled-p runtime options)
        (return))
      (%scan-content-document runtime options document)
      (when (search-content-runtime-canceled-p runtime)
        (return))
      (%emit-content-scan-progress runtime options))
    (%finalize-search-content-scan runtime options)))

(defun scan-content-matches (pattern documents &key options)
  "Search DOCUMENTS for PATTERN with SEARCH-CONTENT-OPTIONS streaming callbacks.
Returns a SEARCH-CONTENT-SCAN-RESULT."
  (let* ((options (%resolve-search-content-options options))
         (query (or pattern "")))
    (if (zerop (length query))
        (%empty-search-content-scan-result)
        (%scan-content-documents query documents options))))

(defun search-content-matches (pattern documents &key options)
  "Search DOCUMENTS for PATTERN and return ranked search-content-match entries."
  (search-content-scan-result-matches
   (scan-content-matches pattern
                         documents
                         :options options)))
