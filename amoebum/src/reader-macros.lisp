(in-package :amoebum)

(defparameter +amoebum-readtable-name+ :amoebum-readtable)

(define-condition malformed-regex (error)
  ((pattern :initarg :pattern
            :reader malformed-regex-pattern)
   (reason :initarg :reason
           :reader malformed-regex-reason))
  (:report (lambda (condition stream)
             (format stream "Malformed regex ~S: ~A"
                     (malformed-regex-pattern condition)
                     (malformed-regex-reason condition)))))

(defun reader-project-root ()
  (let ((cfg (ignore-errors (current-config))))
    (uiop:ensure-directory-pathname
     (or (and cfg (ignore-errors (config-project-root cfg)))
         *default-pathname-defaults*))))

(defun resolve-reader-path (path-text &key (project-root (reader-project-root)))
  (unless (stringp path-text)
    (error "#p reader macro expects a string, got ~S." path-text))
  (let* ((root (uiop:ensure-directory-pathname project-root))
         (path (pathname path-text)))
    (if (uiop:absolute-pathname-p path)
        path
        (merge-pathnames path root))))

(defun %reader-recursive-glob-pattern-p (pattern)
  (not (null (search "**" pattern :test #'char=))))

(defun %reader-regex-escape-char (char stream)
  (when (find char "\\.^$|()[]{}+?" :test #'char=)
    (write-char #\\ stream))
  (write-char char stream))

(defun %reader-glob->regex (pattern)
  (let* ((source (%normalize-slashes pattern))
         (len (length source)))
    (with-output-to-string (stream)
      (write-char #\^ stream)
      (loop for i from 0 below len do
            (let ((ch (char source i)))
              (cond
                ((char= ch #\*)
                 (if (and (< (1+ i) len)
                          (char= (char source (1+ i)) #\*))
                     (progn
                       (incf i)
                       (if (and (< (1+ i) len)
                                (char= (char source (1+ i)) #\/))
                           (progn
                             (incf i)
                             (write-string "(?:.*/)?" stream))
                           (write-string ".*" stream)))
                     (write-string "[^/]*" stream)))
                ((char= ch #\?)
                 (write-string "[^/]" stream))
                ((char= ch #\[)
                 (let ((close (position #\] source :start (1+ i))))
                   (if close
                       (progn
                         (write-string (subseq source i (1+ close)) stream)
                         (setf i close))
                       (%reader-regex-escape-char ch stream))))
                ((char= ch #\{)
                 (let ((close (position #\} source :start (1+ i))))
                   (if close
                       (let ((inner (subseq source (1+ i) close))
                             (alternatives ()))
                         (setf alternatives
                               (uiop:split-string inner :separator ","))
                         (write-string "(?:" stream)
                         (loop for alt in alternatives
                               for idx from 0 do
                                 (when (> idx 0)
                                   (write-char #\| stream))
                                 (write-string (cl-ppcre:quote-meta-chars alt) stream))
                         (write-char #\) stream)
                         (setf i close))
                       (%reader-regex-escape-char ch stream))))
                (t
                 (%reader-regex-escape-char ch stream)))))
      (write-char #\$ stream))))

(defun %reader-normalized-path-text (path)
  (%normalize-slashes (namestring path)))

(defun %reader-relative-path-text (path root)
  (let ((relative (%normalize-slashes (enough-namestring path root))))
    (if (uiop:string-prefix-p "./" relative)
        (subseq relative 2)
        relative)))

(defun %reader-regular-file-p (path)
  (let ((probed (probe-file path)))
    (and probed
         (not (uiop:directory-pathname-p probed)))))

(defun %reader-collect-files-recursive (root)
  (let ((visited (make-hash-table :test #'equal))
        (results '()))
    (labels ((directory-key (directory)
               (let* ((resolved (or (ignore-errors (truename directory)) directory))
                      (as-dir (uiop:ensure-directory-pathname resolved)))
                 (%reader-normalized-path-text as-dir)))
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

(defun %reader-glob-candidates (root pattern)
  (if (%reader-recursive-glob-pattern-p pattern)
      (%reader-collect-files-recursive root)
      (or (ignore-errors
            (directory (merge-pathnames (pathname pattern) root)))
          '())))

(defun glob-paths (pattern &key (project-root (reader-project-root)))
  (unless (stringp pattern)
    (error "#g reader macro expects a string, got ~S." pattern))
  (let* ((root (uiop:ensure-directory-pathname project-root))
         (scanner (cl-ppcre:create-scanner (%reader-glob->regex pattern)))
         (seen (make-hash-table :test #'equal))
         (matches '()))
    (dolist (candidate (%reader-glob-candidates root pattern))
      (when (%reader-regular-file-p candidate)
        (let* ((resolved (or (ignore-errors (truename candidate)) candidate))
               (key (%reader-normalized-path-text resolved))
               (relative (%reader-relative-path-text resolved root)))
          (unless (gethash key seen)
            (setf (gethash key seen) t)
            (when (cl-ppcre:scan scanner relative)
              (push resolved matches))))))
    (sort matches #'string< :key #'namestring)))

(defun compile-reader-regex (pattern)
  (handler-case
      (cl-ppcre:create-scanner pattern)
    (error (condition)
      (error 'malformed-regex
             :pattern pattern
             :reason condition))))

(defun %read-dispatch-string (stream reader-name)
  (let ((value (read stream t nil t)))
    (unless (stringp value)
      (error "~A reader macro expects a string, got ~S." reader-name value))
    value))

(defun %path-reader-dispatch (stream sub-char arg)
  (declare (ignore sub-char arg))
  (resolve-reader-path (%read-dispatch-string stream "#p")))

(defun %glob-reader-dispatch (stream sub-char arg)
  (declare (ignore sub-char arg))
  (glob-paths (%read-dispatch-string stream "#g")))

(defun %regex-reader-dispatch (stream sub-char arg)
  (declare (ignore sub-char arg))
  (compile-reader-regex (%read-dispatch-string stream "#r")))

(named-readtables:defreadtable :amoebum-readtable
  (:merge :standard)
  (:dispatch-macro-char #\# #\p #'%path-reader-dispatch)
  (:dispatch-macro-char #\# #\g #'%glob-reader-dispatch)
  (:dispatch-macro-char #\# #\r #'%regex-reader-dispatch))

(defun activate-amoebum-readtable ()
  (setf *readtable*
        (named-readtables:find-readtable +amoebum-readtable-name+)))

(eval-when (:load-toplevel :execute)
  (when (eq *package* (find-package :amoebum))
    (activate-amoebum-readtable)))
