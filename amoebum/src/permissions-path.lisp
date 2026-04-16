(in-package :amoebum)

(defun %join-path-segments (segments)
  (if segments
      (format nil "~{~A~^/~}" segments)
      ""))

(defun %ascii-alpha-p (char)
  (or (and (>= (char-code char) (char-code #\a))
           (<= (char-code char) (char-code #\z)))
      (and (>= (char-code char) (char-code #\A))
           (<= (char-code char) (char-code #\Z)))))

(defun %permission-path-root-only-p (kind root candidate)
  (case kind
    (:absolute (string= candidate "/"))
    (:drive (string= candidate root))
    (:unc (string= candidate root))
    (:relative (string= candidate "."))
    (otherwise nil)))

(defun %permission-path-root-and-rest (source)
  (cond
    ((and (>= (length source) 2)
          (char= (char source 1) #\:)
          (%ascii-alpha-p (char source 0)))
     (values :drive
             (format nil "~A:/" (string-downcase (subseq source 0 1)))
             (string-left-trim "/" (subseq source 2))))
    ((uiop:string-prefix-p "//" source)
     (let* ((parts (uiop:split-string (subseq source 2) :separator "/"))
            (server (first parts))
            (share (second parts))
            (remaining (cddr parts)))
       (values :unc
               (cond
                 ((and server share)
                  (format nil "//~A/~A"
                          (string-downcase server)
                          (string-downcase share)))
                 (server
                  (format nil "//~A" (string-downcase server)))
                 (t "//"))
               (%join-path-segments remaining))))
    ((uiop:string-prefix-p "/" source)
     (values :absolute
             "/"
             (string-left-trim "/" source)))
    (t
     (values :relative "" source))))

(defun %normalize-permission-path-segments (rest kind)
  (let ((segments '()))
    (dolist (segment (if (string= rest "")
                         '()
                         (uiop:split-string rest :separator "/")))
      (cond
        ((or (string= segment "")
             (string= segment "."))
         nil)
        ((string= segment "..")
         (if (and segments
                  (not (string= (car segments) "..")))
             (pop segments)
             (when (eq kind :relative)
               (push segment segments))))
        (t
         (push segment segments))))
    (nreverse segments)))

(defun %build-normalized-permission-path (kind root normalized-segments)
  (let ((joined (%join-path-segments normalized-segments)))
    (case kind
      (:absolute (if (string= joined "")
                     "/"
                     (concatenate 'string "/" joined)))
      (:drive (if (string= joined "")
                  root
                  (concatenate 'string root joined)))
      (:unc (if (string= joined "")
                root
                (concatenate 'string root "/" joined)))
      (otherwise (if (string= joined "")
                     "."
                     joined)))))

(defun %permission-path-parent-directory (path)
  (let ((text (%path-string path)))
    (when (and text (> (length text) 0))
      (let* ((pathname (pathname text))
             (directory (make-pathname :name nil :type nil :defaults pathname))
             (directory-text (%path-string directory)))
        (when (and directory-text (> (length directory-text) 0))
          (%normalize-path directory-text))))))

(defun %optional-package (designator)
  (or (find-package designator)
      (ignore-errors (require designator))
      (find-package designator)))

(defun %unicode-normalize-path (path)
  (let ((text (%path-string path)))
    (if (or (null text)
            (null *permission-path-unicode-normalization-form*))
        text
        (let* ((package (%optional-package :sb-unicode))
               (normalize-symbol (and package
                                      (find-symbol "NORMALIZE-STRING" package)))
               (normalize-fn (and normalize-symbol
                                  (fboundp normalize-symbol)
                                  (symbol-function normalize-symbol))))
          (if normalize-fn
              (or (ignore-errors
                    (funcall normalize-fn
                             text
                             *permission-path-unicode-normalization-form*))
                  text)
              text)))))

(defun %fold-path-identity-text (path)
  (let ((normalized (%unicode-normalize-path path)))
    (if (and normalized (not *permission-path-case-sensitive-p*))
        (string-downcase normalized)
        normalized)))

(defun %sb-posix-function (name)
  (let* ((package (%optional-package :sb-posix))
         (symbol (and package (find-symbol name package))))
    (and symbol
         (fboundp symbol)
         (symbol-function symbol))))

(defun %path-inode-signature (path &key (follow-symlinks-p t))
  (let* ((candidate (%normalize-request-path path))
         (stat-fn (%sb-posix-function (if follow-symlinks-p
                                          "STAT"
                                          "LSTAT")))
         (dev-fn (%sb-posix-function "STAT-DEV"))
         (ino-fn (%sb-posix-function "STAT-INO")))
    (when (and candidate stat-fn dev-fn ino-fn)
      (let ((stat (ignore-errors (funcall stat-fn candidate))))
        (when stat
          (let ((dev (ignore-errors (funcall dev-fn stat)))
                (ino (ignore-errors (funcall ino-fn stat))))
            (when (and dev ino)
              (list :dev dev :ino ino))))))))

(defun %capture-path-identity (path)
  (let* ((request-path (%normalize-request-path path))
         (target-path (%normalize-path path))
         (effective-path (or target-path request-path))
         (parent-path (%permission-path-parent-directory effective-path)))
    (when effective-path
      (list :request-path request-path
            :target-path effective-path
            :target-folded (%fold-path-identity-text effective-path)
            :target-inode (%path-inode-signature effective-path)
            :parent-path parent-path
            :parent-folded (and parent-path
                                (%fold-path-identity-text parent-path))
            :parent-inode (and parent-path
                               (%path-inode-signature parent-path))
            :captured-at (get-universal-time)))))

(defun %identity-field-matches-p (left right &key (stringp nil))
  (or (null left)
      (null right)
      (if stringp
          (string= left right)
          (equal left right))))

(defun %path-identity-records-compatible-p (expected observed)
  (and expected
       observed
       (%identity-field-matches-p (getf expected :target-folded)
                                  (getf observed :target-folded)
                                  :stringp t)
       (%identity-field-matches-p (getf expected :target-inode)
                                  (getf observed :target-inode))
       (%identity-field-matches-p (getf expected :parent-folded)
                                  (getf observed :parent-folded)
                                  :stringp t)
       (%identity-field-matches-p (getf expected :parent-inode)
                                  (getf observed :parent-inode))))

(defun %permission-path-identity-cache-key (tool path)
  (let ((tool-name (%tool-name tool))
        (request-path (%normalize-request-path path)))
    (when (and tool-name request-path)
      (list :tool tool-name :request-path request-path))))

(defun %trim-permission-path-identity-cache ()
  (when (> (hash-table-count *permission-path-identity-check-cache*)
           *permission-path-identity-check-cache-limit*)
    (clrhash *permission-path-identity-check-cache*)))

(defun %record-permission-path-identity-check (tool path decision decision-id)
  (let* ((cache-key (%permission-path-identity-cache-key tool path))
         (snapshot (%capture-path-identity path)))
    (when (and cache-key snapshot)
      (setf (gethash cache-key *permission-path-identity-check-cache*)
            (list :decision decision
                  :decision-id decision-id
                  :identity snapshot))
      (%trim-permission-path-identity-cache))
    snapshot))

(defun %path-identity-equal-p (left right)
  (let ((left-text (%path-string left))
        (right-text (%path-string right)))
    (and left-text
         right-text
         (or (string= left-text right-text)
             (let ((left-folded (%fold-path-identity-text left-text))
                   (right-folded (%fold-path-identity-text right-text)))
               (and left-folded
                    right-folded
                    (string= left-folded right-folded)))
             (let ((left-inode (%path-inode-signature left-text))
                   (right-inode (%path-inode-signature right-text)))
               (and left-inode
                    right-inode
                    (equal left-inode right-inode)))))))

(defun %assert-path-identity-stable-at-use-time (&key tool path)
  (let* ((cache-key (%permission-path-identity-cache-key tool path))
         (cached (and cache-key
                      (gethash cache-key *permission-path-identity-check-cache*)))
         (expected (and (listp cached) (getf cached :identity))))
    (when (and expected (functionp *permission-path-identity-recheck-hook*))
      (funcall *permission-path-identity-recheck-hook*
               :tool (%tool-name tool)
               :request-path (%normalize-request-path path)
               :expected expected))
    (let ((observed (and expected (%capture-path-identity path))))
      (when (and expected
                 (not (%path-identity-records-compatible-p expected observed)))
        (error 'tool-permission-denied
               :tool-name (%tool-name tool)
               :arguments nil
               :reason-code :path-identity-changed
               :message (format nil "Path identity changed for ~A before use-time recheck."
                                (%path-string path))
               :reason "canonical path identity changed before file use")))))

(defun %normalize-pattern-path (pattern)
  (let ((raw (%path-string pattern)))
    (when raw
      (let* ((trimmed (%normalize-drive-letter (%trim-path-whitespace raw))))
        (when (> (length trimmed) 0)
          (let ((slash-normalized (%normalize-slashes trimmed)))
            (if (%contains-glob-char-p slash-normalized)
                (%trim-trailing-slash
                 (%collapse-dot-segments
                  (%ensure-absolute-path slash-normalized)))
                (%normalize-path slash-normalized))))))))

(defun normalize-permission-path (path &key (preserve-trailing-slash-p nil))
  (let* ((raw (%path-string path))
         (trimmed (and raw (string-trim '(#\Space #\Tab #\Newline #\Return) raw))))
    (when (and trimmed (> (length trimmed) 0))
      (let* ((had-trailing-separator-p (%path-has-trailing-separator-p trimmed))
             (source (substitute #\/ #\\ trimmed)))
        (multiple-value-bind (kind root rest)
            (%permission-path-root-and-rest source)
          (let* ((normalized-segments (%normalize-permission-path-segments rest kind))
                 (normalized (%build-normalized-permission-path kind root normalized-segments)))
            (if (and preserve-trailing-slash-p
                     had-trailing-separator-p
                     (not (%permission-path-root-only-p kind root normalized))
                     (not (char= (char normalized (1- (length normalized))) #\/)))
                (concatenate 'string normalized "/")
                normalized)))))))
