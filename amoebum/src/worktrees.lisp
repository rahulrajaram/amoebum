(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; NXT-336: Amoebum-local worktree runtime wrapper
;;;
;;; Keep repo-root, scratch-root, and lock-root policy in Amoebum while
;;; delegating git worktree lifecycle mechanics to sw4rm-sdk.
;;; ---------------------------------------------------------------------------

(defstruct (worktree-runtime
            (:constructor %make-worktree-runtime
                (&key repo-root scratch-root lock-root coordinator)))
  (repo-root nil :type pathname)
  (scratch-root nil :type pathname)
  (lock-root nil :type pathname)
  coordinator)

(defstruct (worktree-metadata
            (:constructor %make-worktree-metadata
                (&key id branch path)))
  (id nil :type (or null string))
  (branch nil :type (or null string))
  (path nil :type (or null string)))

(defun %normalize-worktree-string (value)
  (cond
    ((null value) nil)
    ((stringp value)
     (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
       (unless (zerop (length trimmed))
         trimmed)))
    (t
     (%normalize-worktree-string (princ-to-string value)))))

(defun %normalize-worktree-path-string (value)
  (let ((normalized (%normalize-worktree-string value)))
    (when normalized
      (namestring
       (uiop:ensure-directory-pathname
        (pathname normalized))))))

(defun make-worktree-metadata (&key id branch path)
  (%make-worktree-metadata
   :id (%normalize-worktree-string id)
   :branch (%normalize-worktree-string branch)
   :path (%normalize-worktree-path-string path)))

(defun coerce-worktree-metadata (&key worktree worktree-id worktree-branch worktree-path)
  (let* ((base
           (cond
             ((null worktree) nil)
             ((worktree-metadata-p worktree) worktree)
             ((listp worktree)
              (make-worktree-metadata :id (getf worktree :id)
                                      :branch (getf worktree :branch)
                                      :path (getf worktree :path)))
             (t
              (error "Unsupported worktree metadata value ~S." worktree))))
         (id (or (%normalize-worktree-string worktree-id)
                 (and base (worktree-metadata-id base))))
         (branch (or (%normalize-worktree-string worktree-branch)
                     (and base (worktree-metadata-branch base))))
         (path (or (%normalize-worktree-path-string worktree-path)
                   (and base (worktree-metadata-path base)))))
    (when (or id branch path)
      (make-worktree-metadata :id id :branch branch :path path))))

(defun worktree-metadata-plist (metadata)
  (let ((resolved (coerce-worktree-metadata :worktree metadata)))
    (when resolved
      (list :id (worktree-metadata-id resolved)
            :branch (worktree-metadata-branch resolved)
            :path (worktree-metadata-path resolved)))))

(defun %worktree-project-root (&key project-root config)
  (uiop:ensure-directory-pathname
   (or project-root
       (and (config-p config) (config-project-root config))
       (and (ignore-errors (current-config))
            (config-project-root (ignore-errors (current-config))))
       (ignore-errors (uiop:getcwd))
       *default-pathname-defaults*)))

(defun default-worktree-repo-root (&key project-root config)
  (%worktree-project-root :project-root project-root
                          :config config))

(defun default-worktree-scratch-root (&key project-root config)
  (merge-pathnames #P".amoebum/worktrees/"
                   (default-worktree-repo-root :project-root project-root
                                               :config config)))

(defun default-worktree-lock-root (&key project-root config scratch-root)
  (merge-pathnames #P"locks/"
                   (uiop:ensure-directory-pathname
                    (or scratch-root
                        (default-worktree-scratch-root :project-root project-root
                                                       :config config)))))

(defun make-worktree-runtime (&key project-root config repo-root scratch-root lock-root)
  (let* ((resolved-repo-root
           (uiop:ensure-directory-pathname
            (or repo-root
                (default-worktree-repo-root :project-root project-root
                                            :config config))))
         (resolved-scratch-root
           (uiop:ensure-directory-pathname
            (or scratch-root
                (default-worktree-scratch-root :project-root resolved-repo-root))))
         (resolved-lock-root
           (uiop:ensure-directory-pathname
            (or lock-root
                (default-worktree-lock-root :scratch-root resolved-scratch-root)))))
    (ensure-directories-exist resolved-scratch-root)
    (ensure-directories-exist resolved-lock-root)
    (%make-worktree-runtime
     :repo-root resolved-repo-root
     :scratch-root resolved-scratch-root
     :lock-root resolved-lock-root
     :coordinator (sw4rm-sdk:make-git-worktree-coordinator
                   resolved-repo-root
                   :lock-dir resolved-lock-root))))

(defun current-worktree-runtime (&optional (config (ignore-errors (current-config))))
  (make-worktree-runtime :config config))

(defun %worktree-id-path-component (worktree-id)
  (let* ((raw (string-downcase
               (string-trim '(#\Space #\Tab #\Newline #\Return)
                            (princ-to-string worktree-id))))
         (buffer (make-string-output-stream))
         (last-was-dash nil))
    (loop for ch across raw do
      (cond
        ((or (alphanumericp ch)
             (char= ch #\_)
             (char= ch #\-))
         (write-char ch buffer)
         (setf last-was-dash nil))
        (last-was-dash
         nil)
        (t
         (write-char #\- buffer)
         (setf last-was-dash t))))
    (let ((component (string-trim "-" (get-output-stream-string buffer))))
      (unless (plusp (length component))
        (error "Invalid worktree id ~S." worktree-id))
      component)))

(defun worktree-runtime-path (runtime worktree-id)
  (check-type runtime worktree-runtime)
  (merge-pathnames
   (format nil "~A/" (%worktree-id-path-component worktree-id))
   (worktree-runtime-scratch-root runtime)))

(defun %worktree-path-namestring (path)
  (let ((pathname (etypecase path
                    (pathname path)
                    (string (pathname path)))))
    (namestring (uiop:ensure-directory-pathname pathname))))

(defun spawn-local-worktree (runtime worktree-id branch &key base-ref)
  (check-type runtime worktree-runtime)
  (sw4rm-sdk:spawn-worktree
   (worktree-runtime-coordinator runtime)
   (princ-to-string worktree-id)
   (namestring (worktree-runtime-path runtime worktree-id))
   branch
   :base-ref (or base-ref "HEAD")))

(defun collect-local-worktree (runtime worktree-id)
  (check-type runtime worktree-runtime)
  (let* ((collected (sw4rm-sdk:collect-worktree
                     (worktree-runtime-coordinator runtime)
                     (princ-to-string worktree-id)))
         (record (getf collected :record))
         (live (or (getf collected :live)
                   (let* ((record-path (and record (getf record :path)))
                          (live-worktrees
                            (sw4rm-sdk:git-worktree-list
                             (worktree-runtime-repo-root runtime))))
                     (and record-path
                          (find (%worktree-path-namestring record-path)
                                live-worktrees
                                :key (lambda (item)
                                       (%worktree-path-namestring
                                        (getf item :path)))
                                :test #'string=))))))
    (list :record record
          :live live)))

(defun kill-local-worktree (runtime worktree-id &key force)
  (check-type runtime worktree-runtime)
  (sw4rm-sdk:kill-worktree
   (worktree-runtime-coordinator runtime)
   (princ-to-string worktree-id)
   :force force))
