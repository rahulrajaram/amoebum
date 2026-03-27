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
