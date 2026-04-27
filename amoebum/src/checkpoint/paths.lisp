(in-package :amoebum)

(defun %checkpoint-trim (value)
  (if (stringp value)
      (string-trim '(#\Space #\Tab #\Newline #\Return) value)
      ""))

(defun %checkpoint-project-root (&key project-root config)
  (uiop:ensure-directory-pathname
   (or project-root
       (and (config-p config) (config-project-root config))
       (and (ignore-errors (current-config))
            (config-project-root (ignore-errors (current-config))))
       (ignore-errors (uiop:getcwd))
       *default-pathname-defaults*)))

(defun checkpoint-directory (&key project-root config)
  (or (and *checkpoint-directory-override*
           (uiop:ensure-directory-pathname *checkpoint-directory-override*))
      (merge-pathnames #P".amoebum/checkpoints/"
                       (%checkpoint-project-root :project-root project-root
                                                 :config config))))

(defun session-snapshot-directory ()
  (or (and *session-snapshot-directory-override*
           (uiop:ensure-directory-pathname *session-snapshot-directory-override*))
      (merge-pathnames #P".amoebum/session-snapshots/"
                       (user-homedir-pathname))))

(defun %checkpoint-id-from-time (&optional (timestamp (get-universal-time)))
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time timestamp 0)
    (format nil "~4,'0D~2,'0D~2,'0DT~2,'0D~2,'0D~2,'0DZ"
            year month day hour minute second)))

(defun %checkpoint-project-name (&key project-root config)
  (let* ((root (%checkpoint-project-root :project-root project-root :config config))
         (segments (ignore-errors (pathname-directory root)))
         (leaf (and (listp segments) (car (last segments)))))
    (cond
      ((stringp leaf) leaf)
      ((symbolp leaf) (symbol-name leaf))
      (t "project"))))

(defun %checkpoint-slugify (value &optional (fallback "project"))
  (let* ((raw (string-downcase (%checkpoint-trim (princ-to-string value))))
         (buffer (make-string-output-stream))
         (last-was-dash nil))
    (loop for ch across raw do
      (cond
        ((or (alphanumericp ch) (char= ch #\_) (char= ch #\-))
         (write-char ch buffer)
         (setf last-was-dash nil))
        (last-was-dash nil)
        (t
         (write-char #\- buffer)
         (setf last-was-dash t))))
    (let* ((candidate (%checkpoint-trim (get-output-stream-string buffer)))
           (trimmed (string-trim "-" candidate)))
      (if (plusp (length trimmed))
          trimmed
          fallback))))

(defun %checkpoint-path (checkpoint-id &key project-root config)
  (let* ((project-name (%checkpoint-project-name :project-root project-root
                                                 :config config))
         (project-slug (%checkpoint-slugify project-name))
         (filename (format nil "~A-~A.core" project-slug checkpoint-id)))
    (merge-pathnames (pathname filename)
                     (checkpoint-directory :project-root project-root
                                           :config config))))

(defun %next-checkpoint-path (&key project-root config timestamp)
  (let ((base-id (%checkpoint-id-from-time timestamp)))
    (loop for suffix from 0
          for id = (if (zerop suffix)
                       base-id
                       (format nil "~A-~D" base-id suffix))
          for path = (%checkpoint-path id :project-root project-root
                                          :config config)
          unless (probe-file path)
            do (return path))))

(defun %session-snapshot-path (snapshot-id)
  (merge-pathnames (pathname (format nil "~A.sexp" snapshot-id))
                   (session-snapshot-directory)))

(defun %next-session-snapshot-path (&key timestamp)
  (let ((base-id (%checkpoint-id-from-time timestamp)))
    (loop for suffix from 0
          for id = (if (zerop suffix)
                       base-id
                       (format nil "~A-~D" base-id suffix))
          for path = (%session-snapshot-path id)
          unless (probe-file path)
            do (return path))))

(defun %checkpoint-sort-key (value)
  (string-downcase
   (cond
     ((keywordp value) (symbol-name value))
     ((symbolp value) (symbol-name value))
     (t (princ-to-string value)))))

(defun %checkpoint-limit-records (records limit)
  (if (and (integerp limit) (>= limit 0))
      (subseq records 0 (min limit (length records)))
      records))

(defun %checkpoint-session-record (id path created-at &key auto-p (trigger :manual))
  (make-session-checkpoint
   :id id
   :path path
   :created-at created-at
   :auto-p (not (null auto-p))
   :trigger trigger))

(defun %checkpoint-files (&key project-root config)
  (let* ((dir (checkpoint-directory :project-root project-root :config config))
         (core-pattern (merge-pathnames #P"*.core" dir))
         (sexp-pattern (merge-pathnames #P"*.sexp" dir)))
    (sort (append (copy-list (directory core-pattern))
                  (copy-list (directory sexp-pattern)))
          (lambda (left right)
            (let ((left-date (or (ignore-errors (file-write-date left)) 0))
                  (right-date (or (ignore-errors (file-write-date right)) 0)))
              (if (= left-date right-date)
                  (string> (namestring left) (namestring right))
                  (> left-date right-date)))))))

(defun %checkpoint-created-at (payload path)
  (let ((created-at (and (listp payload) (getf payload :created-at))))
    (if (integerp created-at)
        created-at
        (or (ignore-errors (file-write-date path))
            (get-universal-time)))))

(defun %session-snapshot-files ()
  (let* ((dir (session-snapshot-directory))
         (pattern (merge-pathnames #P"*.sexp" dir)))
    (sort (copy-list (directory pattern))
          #'>
          :key (lambda (path)
                 (or (ignore-errors (file-write-date path)) 0)))))
