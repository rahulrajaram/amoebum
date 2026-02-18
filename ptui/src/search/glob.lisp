(defpackage :ptui.search.glob
  (:use :cl)
  (:export
   #:glob-matcher
   #:glob-matcher-pattern
   #:glob-matcher-expanded-patterns
   #:glob-matcher-case-sensitive-p
   #:compile-glob-matcher
   #:glob-matcher-match-p
   #:glob-match-p
   #:glob-ignore-rule
   #:glob-ignore-rule-negated-p
   #:glob-ignore-rule-patterns
   #:read-gitignore-rules
   #:glob-entry
   #:glob-entry-path
   #:glob-entry-relative-path
   #:glob-entry-modified-at
   #:glob-scan-result
   #:glob-scan-result-pattern
   #:glob-scan-result-root
   #:glob-scan-result-matches
   #:glob-scan-result-scanned-files
   #:glob-scan-result-canceled-p
   #:scan-glob-files))

(in-package :ptui.search.glob)

(defstruct (glob-matcher
            (:constructor %make-glob-matcher
                (&key pattern expanded-patterns segment-vectors case-sensitive-p)))
  (pattern "" :type string)
  (expanded-patterns '() :type list)
  (segment-vectors #() :type vector)
  (case-sensitive-p t :type boolean))

(defstruct (glob-ignore-rule
            (:constructor %make-glob-ignore-rule
                (&key negated-p patterns)))
  (negated-p nil :type boolean)
  (patterns '() :type list))

(defstruct (glob-entry
            (:constructor make-glob-entry
                (&key path relative-path modified-at)))
  (path "" :type string)
  (relative-path "" :type string)
  (modified-at 0 :type integer))

(defstruct (glob-scan-result
            (:constructor make-glob-scan-result
                (&key pattern root matches scanned-files canceled-p)))
  (pattern "" :type string)
  (root "" :type string)
  (matches '() :type list)
  (scanned-files 0 :type fixnum)
  (canceled-p nil :type boolean))

(defun %normalize-slashes (text)
  (let* ((string (if (stringp text) text (princ-to-string text)))
         (buffer (copy-seq string)))
    (loop for i from 0 below (length buffer) do
          (when (char= (char buffer i) #\\)
            (setf (char buffer i) #\/)))
    buffer))

(defun %trim-prefix (text prefix)
  (if (uiop:string-prefix-p prefix text)
      (subseq text (length prefix))
      text))

(defun %normalize-pattern-text (pattern)
  (let ((normalized (%normalize-slashes pattern)))
    (setf normalized (%trim-prefix normalized "./"))
    (setf normalized (%trim-prefix normalized "/"))
    normalized))

(defun %normalize-relative-path-text (path-text)
  (let ((normalized (%normalize-slashes path-text)))
    (setf normalized (%trim-prefix normalized "./"))
    (setf normalized (%trim-prefix normalized "/"))
    normalized))

(defun %split-path-segments (path-text)
  (let ((parts (uiop:split-string path-text :separator "/")))
    (coerce (remove "" parts :test #'string=) 'vector)))

(defun %char=with-mode (left right case-sensitive-p)
  (if case-sensitive-p
      (char= left right)
      (char-equal left right)))

(defun %char-class-contains-p (spec target case-sensitive-p)
  (let* ((len (length spec))
         (index 0)
         (negated-p nil)
         (matched-p nil))
    (when (and (> len 0)
               (member (char spec 0) '(#\! #\^) :test #'char=))
      (setf negated-p t
            index 1))
    (labels ((normalize-char (char)
               (if case-sensitive-p
                   char
                   (char-downcase char)))
             (target-char ()
               (normalize-char target))
             (match-char (char)
               (char= (normalize-char char) (target-char)))
             (match-range (start end)
               (let* ((target-code (char-code (target-char)))
                      (start-code (char-code (normalize-char start)))
                      (end-code (char-code (normalize-char end))))
                 (<= (min start-code end-code)
                     target-code
                     (max start-code end-code)))))
      (loop while (< index len) do
            (let ((char (char spec index)))
              (cond
                ((and (char= char #\\)
                      (< (1+ index) len))
                 (when (match-char (char spec (1+ index)))
                   (setf matched-p t))
                 (incf index 2))
                ((and (< (+ index 2) len)
                      (char= (char spec (1+ index)) #\-))
                 (when (match-range char (char spec (+ index 2)))
                   (setf matched-p t))
                 (incf index 3))
                (t
                 (when (match-char char)
                   (setf matched-p t))
                 (incf index 1))))))
    (if negated-p
        (not matched-p)
        matched-p)))

(defun %find-char-class-end (pattern start)
  (let ((len (length pattern))
        (escaped-p nil))
    (loop for index from start below len do
          (let ((char (char pattern index)))
            (cond
              ((and (char= char #\])
                    (not escaped-p))
               (return index))
              ((and (char= char #\\)
                    (not escaped-p))
               (setf escaped-p t))
              (t
               (setf escaped-p nil)))))))

(defun %segment-match-p (pattern-segment path-segment case-sensitive-p)
  (let ((pattern-length (length pattern-segment))
        (path-length (length path-segment))
        (memo (make-hash-table :test #'equal)))
    (labels ((memoized-match (pattern-index path-index)
               (let ((key (cons pattern-index path-index)))
                 (multiple-value-bind (value present-p)
                     (gethash key memo)
                   (if present-p
                       value
                       (setf (gethash key memo)
                             (match pattern-index path-index))))))
             (match (pattern-index path-index)
               (if (= pattern-index pattern-length)
                   (= path-index path-length)
                   (let ((char (char pattern-segment pattern-index)))
                     (cond
                       ((char= char #\*)
                        (or (memoized-match (1+ pattern-index) path-index)
                            (and (< path-index path-length)
                                 (memoized-match pattern-index (1+ path-index)))))
                       ((char= char #\?)
                        (and (< path-index path-length)
                             (memoized-match (1+ pattern-index) (1+ path-index))))
                       ((char= char #\[)
                        (let ((end (%find-char-class-end pattern-segment (1+ pattern-index))))
                          (if (and end (< path-index path-length))
                              (let ((spec (subseq pattern-segment (1+ pattern-index) end))
                                    (target (char path-segment path-index)))
                                (and (%char-class-contains-p spec target case-sensitive-p)
                                     (memoized-match (1+ end) (1+ path-index))))
                              (and (< path-index path-length)
                                   (%char=with-mode char
                                                    (char path-segment path-index)
                                                    case-sensitive-p)
                                   (memoized-match (1+ pattern-index)
                                                   (1+ path-index))))))
                       ((char= char #\\)
                        (let ((literal (if (< (1+ pattern-index) pattern-length)
                                           (char pattern-segment (1+ pattern-index))
                                           char))
                              (next-index (if (< (1+ pattern-index) pattern-length)
                                              (+ pattern-index 2)
                                              (1+ pattern-index))))
                          (and (< path-index path-length)
                               (%char=with-mode literal
                                                (char path-segment path-index)
                                                case-sensitive-p)
                               (memoized-match next-index (1+ path-index)))))
                       (t
                        (and (< path-index path-length)
                             (%char=with-mode char
                                              (char path-segment path-index)
                                              case-sensitive-p)
                             (memoized-match (1+ pattern-index)
                                             (1+ path-index)))))))))
      (memoized-match 0 0))))

(defun %segments-match-p (pattern-segments path-segments case-sensitive-p)
  (let* ((pattern-length (length pattern-segments))
         (path-length (length path-segments))
         (memo (make-hash-table :test #'equal)))
    (labels ((memoized-match (pattern-index path-index)
               (let ((key (cons pattern-index path-index)))
                 (multiple-value-bind (value present-p)
                     (gethash key memo)
                   (if present-p
                       value
                       (setf (gethash key memo)
                             (match pattern-index path-index))))))
             (match (pattern-index path-index)
               (cond
                 ((= pattern-index pattern-length)
                  (= path-index path-length))
                 ((string= (aref pattern-segments pattern-index) "**")
                  (or (memoized-match (1+ pattern-index) path-index)
                      (and (< path-index path-length)
                           (memoized-match pattern-index (1+ path-index)))))
                 ((< path-index path-length)
                  (and (%segment-match-p (aref pattern-segments pattern-index)
                                         (aref path-segments path-index)
                                         case-sensitive-p)
                       (memoized-match (1+ pattern-index) (1+ path-index))))
                 (t nil))))
      (memoized-match 0 0))))

(defun %find-first-unescaped (text target)
  (let ((escaped-p nil))
    (loop for index from 0 below (length text) do
          (let ((char (char text index)))
            (cond
              ((and (char= char target)
                    (not escaped-p))
               (return index))
              ((and (char= char #\\)
                    (not escaped-p))
               (setf escaped-p t))
              (t
               (setf escaped-p nil)))))))

(defun %find-matching-brace (text open-index)
  (let ((depth 0)
        (escaped-p nil))
    (loop for index from open-index below (length text) do
          (let ((char (char text index)))
            (cond
              ((and (char= char #\\)
                    (not escaped-p))
               (setf escaped-p t))
              ((and (char= char #\{)
                    (not escaped-p))
               (incf depth))
              ((and (char= char #\})
                    (not escaped-p))
               (decf depth)
               (when (zerop depth)
                 (return index)))
              (t
               (setf escaped-p nil)))))))

(defun %split-brace-alternatives (text)
  (let ((parts '())
        (depth 0)
        (start 0)
        (escaped-p nil))
    (loop for index from 0 below (length text) do
          (let ((char (char text index)))
            (cond
              ((and (char= char #\\)
                    (not escaped-p))
               (setf escaped-p t))
              ((and (char= char #\{)
                    (not escaped-p))
               (incf depth)
               (setf escaped-p nil))
              ((and (char= char #\})
                    (not escaped-p))
               (decf depth)
               (setf escaped-p nil))
              ((and (char= char #\,)
                    (zerop depth)
                    (not escaped-p))
               (push (subseq text start index) parts)
               (setf start (1+ index)))
              (t
               (setf escaped-p nil)))))
    (push (subseq text start) parts)
    (nreverse parts)))

(defun %expand-braces (pattern)
  (let* ((open-index (%find-first-unescaped pattern #\{)))
    (if (null open-index)
        (list pattern)
        (let ((close-index (%find-matching-brace pattern open-index)))
          (if (null close-index)
              (list pattern)
              (let* ((prefix (subseq pattern 0 open-index))
                     (inner (subseq pattern (1+ open-index) close-index))
                     (suffix (subseq pattern (1+ close-index)))
                     (alternatives (%split-brace-alternatives inner))
                     (expanded '()))
                (dolist (alternative alternatives)
                  (dolist (candidate (%expand-braces (concatenate 'string
                                                                 prefix
                                                                 alternative
                                                                 suffix)))
                    (push candidate expanded)))
                (nreverse expanded)))))))

(defun compile-glob-matcher (pattern &key (case-sensitive t))
  "Compile PATTERN into reusable matcher state."
  (unless (stringp pattern)
    (error "PATTERN must be a string, got ~S." pattern))
  (let* ((normalized (%normalize-pattern-text pattern))
         (expanded (remove-duplicates
                    (mapcar #'%normalize-pattern-text (%expand-braces normalized))
                    :test #'string=))
         (segment-vectors
           (coerce (mapcar #'%split-path-segments expanded) 'vector)))
    (%make-glob-matcher
     :pattern normalized
     :expanded-patterns expanded
     :segment-vectors segment-vectors
     :case-sensitive-p (not (null case-sensitive)))))

(defun glob-matcher-match-p (matcher path)
  "Return T when PATH matches MATCHER."
  (check-type matcher glob-matcher)
  (unless (stringp path)
    (error "PATH must be a string, got ~S." path))
  (let* ((normalized (%normalize-relative-path-text path))
         (path-segments (%split-path-segments normalized))
         (case-sensitive-p (glob-matcher-case-sensitive-p matcher)))
    (loop for segments across (glob-matcher-segment-vectors matcher)
          thereis (%segments-match-p segments path-segments case-sensitive-p))))

(defun glob-match-p (pattern path &key (case-sensitive t))
  "Convenience wrapper: compile PATTERN and match PATH."
  (glob-matcher-match-p (compile-glob-matcher pattern :case-sensitive case-sensitive)
                        path))

(defun %path-text (path)
  (uiop:native-namestring path))

(defun %normalized-path-text (path)
  (%normalize-slashes (%path-text path)))

(defun %relative-path-text (path root)
  (let ((relative (%normalize-slashes (enough-namestring path root))))
    (%normalize-relative-path-text relative)))

(defun %regular-file-p (path)
  (let ((probed (probe-file path)))
    (and probed
         (not (uiop:directory-pathname-p probed)))))

(defun %ensure-root-directory (root)
  (let* ((pathname (etypecase root
                     (pathname root)
                     (string (pathname root))))
         (resolved (or (ignore-errors (truename pathname)) pathname))
         (directory (uiop:ensure-directory-pathname resolved)))
    (unless (probe-file directory)
      (error "Root directory does not exist: ~A" (%path-text directory)))
    directory))

(defun %ignore-pattern-scanners (pattern)
  (let* ((base (%normalize-slashes pattern))
         (anchored-p (and (> (length base) 0)
                          (char= (char base 0) #\/)))
         (trimmed (if anchored-p
                      (subseq base 1)
                      base))
         (directory-only-p (and (> (length trimmed) 0)
                                (char= (char trimmed (1- (length trimmed))) #\/)))
         (trimmed (if directory-only-p
                      (subseq trimmed 0 (1- (length trimmed)))
                      trimmed)))
    (when (> (length trimmed) 0)
      (let* ((has-slash-p (position #\/ trimmed))
             (patterns
               (cond
                 (anchored-p
                  (list trimmed))
                 (has-slash-p
                  (list trimmed
                        (concatenate 'string "**/" trimmed)))
                 (t
                  (list trimmed
                        (concatenate 'string "**/" trimmed)))))
             (patterns
               (if directory-only-p
                   (mapcan (lambda (value)
                             (list value
                                   (concatenate 'string value "/**")))
                           patterns)
                   patterns)))
        (remove-duplicates
         (mapcar #'%normalize-pattern-text patterns)
         :test #'string=)))))

(defun %parse-ignore-line (line)
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) (or line ""))))
    (cond
      ((zerop (length trimmed))
       nil)
      ((char= (char trimmed 0) #\#)
       nil)
      (t
       (let* ((negated-p (char= (char trimmed 0) #\!))
              (pattern (if negated-p
                           (subseq trimmed 1)
                           trimmed))
              (scanners (%ignore-pattern-scanners pattern)))
         (when scanners
           (%make-glob-ignore-rule
            :negated-p negated-p
            :patterns scanners)))))))

(defun read-gitignore-rules (root &key (gitignore-name ".gitignore"))
  "Read .gitignore-style rules rooted at ROOT."
  (let* ((root-directory (%ensure-root-directory root))
         (gitignore-path (merge-pathnames (pathname gitignore-name) root-directory))
         (rules '()))
    (when (probe-file gitignore-path)
      (with-open-file (stream gitignore-path
                              :direction :input
                              :if-does-not-exist nil
                              :external-format :utf-8)
        (loop for line = (read-line stream nil nil)
              while line do
              (let ((rule (%parse-ignore-line line)))
                (when rule
                  (push rule rules))))))
    (nreverse rules)))

(defun %ignore-rules-from-pattern-list (patterns)
  (let ((rules '()))
    (dolist (pattern patterns)
      (let ((rule (%parse-ignore-line pattern)))
        (when rule
          (push rule rules))))
    (nreverse rules)))

(defun %ignored-relative-path-p (rules relative-path)
  (let ((normalized (%normalize-relative-path-text relative-path))
        (ignored-p nil))
    (when (uiop:string-prefix-p ".git/" normalized)
      (setf ignored-p t))
    (dolist (rule rules ignored-p)
      (when (loop for pattern in (glob-ignore-rule-patterns rule)
                  thereis (glob-match-p pattern normalized))
        (setf ignored-p (not (glob-ignore-rule-negated-p rule)))))))

(defun %entry-more-recent-p (left right)
  (let ((left-time (glob-entry-modified-at left))
        (right-time (glob-entry-modified-at right)))
    (cond
      ((> left-time right-time) t)
      ((< left-time right-time) nil)
      (t
       (string< (glob-entry-relative-path left)
                (glob-entry-relative-path right))))))

(defun %scan-directory-files (root matcher rules &key limit on-match cancel-fn)
  (let ((visited (make-hash-table :test #'equal))
        (seen-files (make-hash-table :test #'equal))
        (matches '())
        (scanned-files 0)
        (canceled-p nil))
    (labels ((cancelled-p ()
               (and cancel-fn (funcall cancel-fn)))
             (directory-key (directory)
               (let* ((resolved (or (ignore-errors (truename directory)) directory))
                      (as-directory (uiop:ensure-directory-pathname resolved)))
                 (%normalized-path-text as-directory)))
             (maybe-prune-results ()
               (when (and limit
                          (> (length matches) (* 2 (max 1 limit))))
                 (setf matches
                       (subseq (sort matches #'%entry-more-recent-p)
                               0
                               (min limit (length matches))))))
             (visit-directory (directory)
               (when (cancelled-p)
                 (setf canceled-p t)
                 (return-from visit-directory nil))
               (let ((key (directory-key directory)))
                 (unless (gethash key visited)
                   (setf (gethash key visited) t)
                   (dolist (file (or (ignore-errors (uiop:directory-files directory)) '()))
                     (when (cancelled-p)
                       (setf canceled-p t)
                       (return-from visit-directory nil))
                     (when (%regular-file-p file)
                       (let* ((resolved (or (ignore-errors (truename file)) file))
                              (seen-key (%normalized-path-text resolved)))
                         (unless (gethash seen-key seen-files)
                           (setf (gethash seen-key seen-files) t)
                           (incf scanned-files)
                           (let ((relative (%relative-path-text resolved root)))
                             (unless (%ignored-relative-path-p rules relative)
                               (when (glob-matcher-match-p matcher relative)
                                 (let ((entry
                                         (make-glob-entry
                                          :path (%path-text resolved)
                                          :relative-path relative
                                          :modified-at (or (ignore-errors
                                                             (file-write-date resolved))
                                                           0))))
                                   (push entry matches)
                                   (when on-match
                                     (funcall on-match entry))
                                   (maybe-prune-results)))))))))
                   (dolist (subdir (or (ignore-errors (uiop:subdirectories directory)) '()))
                     (when (cancelled-p)
                       (setf canceled-p t)
                       (return-from visit-directory nil))
                     (let ((relative-subdir (%relative-path-text subdir root)))
                       (unless (%ignored-relative-path-p rules relative-subdir)
                         (visit-directory subdir))))))))
      (visit-directory root))
    (values matches scanned-files canceled-p)))

(defun scan-glob-files (pattern
                        &key
                          (root (uiop:getcwd))
                          (limit 200)
                          (respect-gitignore t)
                          (ignore-patterns '())
                          on-match
                          cancel-fn
                          (case-sensitive t))
  "Scan ROOT for files matching PATTERN and return a GLOB-SCAN-RESULT."
  (when limit
    (check-type limit (integer 1 *)))
  (when on-match
    (check-type on-match function))
  (when cancel-fn
    (check-type cancel-fn function))
  (let* ((root-directory (%ensure-root-directory root))
         (matcher (if (typep pattern 'glob-matcher)
                      pattern
                      (compile-glob-matcher pattern :case-sensitive case-sensitive)))
         (rules (append (if respect-gitignore
                            (read-gitignore-rules root-directory)
                            '())
                        (%ignore-rules-from-pattern-list ignore-patterns))))
    (multiple-value-bind (matches scanned-files canceled-p)
        (%scan-directory-files root-directory
                               matcher
                               rules
                               :limit limit
                               :on-match on-match
                               :cancel-fn cancel-fn)
      (let ((sorted (sort matches #'%entry-more-recent-p)))
        (when limit
          (setf sorted (subseq sorted 0 (min limit (length sorted)))))
        (make-glob-scan-result
         :pattern (glob-matcher-pattern matcher)
         :root (%path-text root-directory)
         :matches sorted
         :scanned-files scanned-files
         :canceled-p canceled-p)))))
