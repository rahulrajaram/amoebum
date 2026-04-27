(in-package :amoebum)

(defun conversation-session-directory (&key project-root)
  (merge-pathnames #P".amoebum/session/"
                   (%conversation-normalize-project-root project-root)))

(defun conversation-session-path (session-id &key project-root)
  (let* ((trimmed (%conversation-trim session-id))
         (resolved (if (plusp (length trimmed))
                       trimmed
                       (%conversation-generate-session-id)))
         (filename (format nil "~A.sexp" resolved)))
    (merge-pathnames (pathname filename)
                     (conversation-session-directory :project-root project-root))))

(defun %conversation-session-record (session-id path updated-at state message-count)
  (amoebum.fp:update
   '()
   (:session-id session-id)
   (:path path)
   (:updated-at updated-at)
   (:state (or state :idle))
   (:message-count message-count)))

(defun %conversation-limit-records (records limit)
  (if (and (integerp limit) (>= limit 0))
      (subseq records 0 (min limit (length records)))
      records))

(defun conversation-save (conversation
                          &key
                            (save-manifest-p t)
                            (save-fork-file-p t))
  (check-type conversation conversation-state)
  (unless (session-persistence-enabled-p)
    (return-from conversation-save nil))
  (let* ((root (%conversation-normalize-project-root
                (conversation-state-project-root conversation)))
         (session-id (or (conversation-state-session-id conversation)
                         (%conversation-generate-session-id)))
         (manifest-path (or (conversation-state-session-path conversation)
                            (conversation-session-path session-id :project-root root))))
    (%conversation-refresh-active-fork-record! conversation)
    (let* ((payload (%conversation-state-payload conversation))
           (active-fork (conversation-active-fork-name conversation))
           (fork-path (conversation-fork-path session-id
                                              active-fork
                                              :project-root root)))
      (when save-manifest-p
        (%conversation-write-sexp-file manifest-path payload))
      (when save-fork-file-p
        (%conversation-write-sexp-file fork-path payload))
      (setf (conversation-state-project-root conversation) root
            (conversation-state-session-id conversation) session-id
            (conversation-state-session-path conversation) manifest-path)
      (if save-manifest-p
          manifest-path
          fork-path))))

(defun %conversation-session-id-from-path (path payload)
  (or (getf payload :session-id)
      (%conversation-trim (pathname-name path))
      (%conversation-generate-session-id)))

(defstruct (conversation-load-context
            (:constructor %make-conversation-load-context
                (&key session-path project-root)))
  session-path
  project-root
  resolved-path
  payload
  root
  session-id
  manifest-path
  active-fork
  fork-path
  fork-payload
  effective-payload
  entries
  created-at
  updated-at
  forks
  branch-point)

(defun %conversation-payload-valid-p (payload)
  (and (listp payload) (keywordp (first payload))))

(defun %conversation-load-open (context)
  (let ((resolved-path (and (conversation-load-context-session-path context)
                            (probe-file (conversation-load-context-session-path context)))))
    (when resolved-path
      (let ((payload (%conversation-read-sexp-file resolved-path)))
        (when (%conversation-payload-valid-p payload)
          (setf (conversation-load-context-resolved-path context) resolved-path
                (conversation-load-context-payload context) payload)
          context)))))

(defun %conversation-load-fork-payload (context)
  (let* ((payload (conversation-load-context-payload context))
         (resolved-path (conversation-load-context-resolved-path context))
         (root (%conversation-normalize-project-root
                (or (conversation-load-context-project-root context)
                    (make-pathname :name nil :type nil :defaults resolved-path))))
         (session-id (%conversation-session-id-from-path resolved-path payload))
         (active-fork (%conversation-normalize-fork-name
                       (or (getf payload :active-fork)
                           +conversation-default-fork-name+)))
         (fork-path (conversation-fork-path session-id active-fork :project-root root))
         (fork-payload (and (probe-file fork-path)
                            (%conversation-read-sexp-file fork-path)))
         (effective-payload (if (%conversation-payload-valid-p fork-payload)
                                fork-payload
                                payload)))
    (setf (conversation-load-context-root context) root
          (conversation-load-context-session-id context) session-id
          (conversation-load-context-manifest-path context)
          (conversation-session-path session-id :project-root root)
          (conversation-load-context-active-fork context) active-fork
          (conversation-load-context-fork-path context) fork-path
          (conversation-load-context-fork-payload context) fork-payload
          (conversation-load-context-effective-payload context) effective-payload)
    context))

(defun %conversation-load-history (context)
  (let* ((payload (conversation-load-context-payload context))
         (effective (conversation-load-context-effective-payload context))
         (entries (%conversation-copy-entries
                   (mapcar #'%conversation-entry-coerce
                           (or (getf effective :entries) '()))))
         (created-at (or (getf payload :created-at)
                         (getf effective :created-at)
                         (%conversation-now)))
         (updated-at (or (getf effective :updated-at)
                         (getf payload :updated-at)
                         created-at))
         (forks (%conversation-normalize-forks
                 (or (getf effective :forks)
                     (getf payload :forks))
                 (conversation-load-context-active-fork context)
                 entries))
         (fork-record (%conversation-find-fork-record forks
                                                      (conversation-load-context-active-fork context)))
         (branch-point (or (and fork-record (getf fork-record :branch-point))
                           (getf effective :fork-branch-point)
                           (getf payload :fork-branch-point))))
    (setf (conversation-load-context-entries context) entries
          (conversation-load-context-created-at context) created-at
          (conversation-load-context-updated-at context) updated-at
          (conversation-load-context-forks context) forks
          (conversation-load-context-branch-point context) branch-point)
    context))

(defun %conversation-load-build (context)
  (let ((payload (conversation-load-context-payload context))
        (effective (conversation-load-context-effective-payload context)))
    (%make-conversation-state
     :session-id (conversation-load-context-session-id context)
     :state (%conversation-normalize-state
             (or (getf effective :state) (getf payload :state)))
     :entries (conversation-load-context-entries context)
     :created-at (if (integerp (conversation-load-context-created-at context))
                     (conversation-load-context-created-at context)
                     (%conversation-now))
     :updated-at (if (integerp (conversation-load-context-updated-at context))
                     (conversation-load-context-updated-at context)
                     (%conversation-now))
     :active-fork (conversation-load-context-active-fork context)
     :fork-branch-point (%conversation-sanitize-branch-point
                         (conversation-load-context-branch-point context))
     :forks (conversation-load-context-forks context)
     :project-root (conversation-load-context-root context)
     :session-path (conversation-load-context-manifest-path context))))

(defun conversation-load (session-path &key project-root)
  (let ((context (%conversation-load-open
                  (%make-conversation-load-context
                   :session-path session-path
                   :project-root project-root))))
    (when context
      (%conversation-load-fork-payload context)
      (%conversation-load-history context)
      (%conversation-load-build context))))

(defun %conversation-file-write-date (path)
  (or (ignore-errors (file-write-date path))
      0))

(defun %conversation-session-files (&key project-root)
  (let* ((directory (conversation-session-directory :project-root project-root))
         (pattern (merge-pathnames #P"*.sexp" directory))
         (files (ignore-errors (directory pattern))))
    (if files
        (sort (copy-list files)
              #'>
              :key #'%conversation-file-write-date)
        '())))

(defun conversation-list-sessions (&key project-root limit)
  (let ((records '()))
    (dolist (path (%conversation-session-files :project-root project-root))
      (let* ((payload (%conversation-read-sexp-file path))
             (session-id (%conversation-session-id-from-path path payload))
             (updated-at (or (and (listp payload) (getf payload :updated-at))
                             (and (listp payload) (getf payload :created-at))
                             (%conversation-file-write-date path)
                             (%conversation-now)))
             (state (and (listp payload) (%conversation-normalize-state (getf payload :state))))
             (entry-count (length (or (and (listp payload) (getf payload :entries))
                                      '()))))
        (push (%conversation-session-record session-id
                                            path
                                            updated-at
                                            state
                                            entry-count)
              records)))
    (%conversation-limit-records (nreverse records) limit)))

(defun conversation-load-latest (&key project-root)
  (let* ((sessions (conversation-list-sessions :project-root project-root :limit 1))
         (latest-path (and sessions (getf (first sessions) :path))))
    (and latest-path
         (conversation-load latest-path :project-root project-root))))

(defun conversation-load-session (session-id &key project-root)
  (let ((session-path (conversation-session-path session-id
                                                 :project-root project-root)))
    (conversation-load session-path :project-root project-root)))
