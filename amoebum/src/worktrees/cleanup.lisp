(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; NXT-354: Worktree abandonment markers and cleanup policy
;;;
;;; Keep cleanup policy behind a dedicated module boundary while preserving the
;;; public runtime API exported from the main :amoebum package.
;;; ---------------------------------------------------------------------------

(defparameter *default-worktree-cleanup-grace-period-seconds* 900
  "Default retention window before Amoebum auto-removes abandoned local worktrees.")

(defparameter *worktree-abandoned-terminal-statuses* '(:failed :cancelled :timeout)
  "Delegated terminal statuses that qualify a local worktree as abandoned.")

(defun %worktree-abandoned-root (runtime)
  (merge-pathnames #P"abandoned/"
                   (worktree-runtime-lock-root runtime)))

(defun %worktree-abandoned-marker-path (runtime worktree-id)
  (merge-pathnames
   (format nil "~A.sexp" (%worktree-id-path-component worktree-id))
   (%worktree-abandoned-root runtime)))

(defun %write-worktree-marker (path payload)
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (let ((*print-circle* nil)
          (*print-pretty* nil)
          (*print-readably* t))
      (write payload :stream stream :readably t)))
  payload)

(defun %read-worktree-marker (path)
  (when (probe-file path)
    (with-open-file (stream path :direction :input)
      (let ((*read-eval* nil))
        (read stream nil nil)))))

(defun %delete-worktree-marker (path)
  (when (probe-file path)
    (delete-file path))
  t)

(defun %worktree-abandoned-marker-files (runtime)
  (or (ignore-errors
        (directory (merge-pathnames "*.sexp"
                                    (%worktree-abandoned-root runtime))))
      '()))

(defun %worktree-marker-p (value)
  (and (listp value)
       (getf value :worktree-id)))

(defun mark-local-worktree-abandoned (&key runtime
                                           repo-root
                                           worktree
                                           worktree-id
                                           worktree-path
                                           worktree-branch
                                           status
                                           finished-at)
  (multiple-value-bind (resolved-runtime metadata resolved-repo-root)
      (%resolve-worktree-runtime-and-metadata
       :runtime runtime
       :repo-root repo-root
       :worktree worktree
       :worktree-id worktree-id
       :worktree-path worktree-path
       :worktree-branch worktree-branch)
    (let ((resolved-status (%normalize-worktree-status status)))
      (when (and resolved-runtime
                 metadata
                 (worktree-metadata-id metadata)
                 resolved-status)
        (%write-worktree-marker
         (%worktree-abandoned-marker-path resolved-runtime
                                          (worktree-metadata-id metadata))
         (list :worktree-id (worktree-metadata-id metadata)
               :repo-root (and resolved-repo-root
                               (%worktree-path-namestring resolved-repo-root))
               :branch (worktree-metadata-branch metadata)
               :path (worktree-metadata-path metadata)
               :status resolved-status
               :finished-at (or finished-at (get-universal-time))
               :updated-at (get-universal-time)))))))

(defun %read-local-worktree-abandonment (runtime metadata)
  (when (and runtime metadata (worktree-metadata-id metadata))
    (let ((marker (%read-worktree-marker
                   (%worktree-abandoned-marker-path runtime
                                                    (worktree-metadata-id metadata)))))
      (and (%worktree-marker-p marker)
           marker))))

(defun inspect-local-worktree (&key runtime
                                    repo-root
                                    worktree
                                    worktree-id
                                    worktree-path
                                    worktree-branch
                                    base-ref)
  (multiple-value-bind (resolved-runtime metadata resolved-repo-root)
      (%resolve-worktree-runtime-and-metadata
       :runtime runtime
       :repo-root repo-root
       :worktree worktree
       :worktree-id worktree-id
       :worktree-path worktree-path
       :worktree-branch worktree-branch)
    (let* ((live-record (and resolved-repo-root
                             (%find-live-local-worktree resolved-repo-root
                                                        metadata)))
           (record (%reconstruct-local-worktree-record metadata live-record))
           (resolved-id (or (getf record :worktree-id)
                            (and metadata (worktree-metadata-id metadata))))
           (resolved-path (or (getf record :path)
                              (and metadata (worktree-metadata-path metadata))))
           (resolved-branch (or (getf record :branch)
                                (and metadata (worktree-metadata-branch metadata))))
           (marker (%read-local-worktree-abandonment resolved-runtime metadata))
           (resolved-base-ref (or (%normalize-worktree-string base-ref)
                                  (and resolved-repo-root
                                       (%default-worktree-base-ref
                                        resolved-repo-root))))
           (dirty-entries nil)
           (dirty-error nil)
           (unique-commit-count nil)
           (unique-error nil)
           (live-p (not (null live-record))))
      (when (and live-p resolved-path (probe-file (pathname resolved-path)))
        (multiple-value-setq (dirty-entries dirty-error)
          (%git-output-lines resolved-path '("status" "--porcelain")))
        (when resolved-base-ref
          (multiple-value-bind (count-text count-error)
              (%git-output-string
               resolved-path
               (list "rev-list" "--count"
                     (format nil "~A..HEAD" resolved-base-ref)))
            (if count-text
                (setf unique-commit-count
                      (parse-integer count-text :junk-allowed t))
                (setf unique-error count-error)))))
      (let ((cleanup-classification
              (cond
                ((null live-p) :missing)
                ((and dirty-entries (plusp (length dirty-entries))) :dirty)
                ((and unique-commit-count (> unique-commit-count 0))
                 :review-required)
                (t
                 :safe-to-prune)))
            (abandoned-p (not (null marker))))
        (list :id resolved-id
              :path resolved-path
              :branch resolved-branch
              :repo-root (and resolved-repo-root
                              (%worktree-path-namestring resolved-repo-root))
              :record record
              :live live-record
              :live-p live-p
              :abandoned-p abandoned-p
              :lifecycle-state (cond
                                 (abandoned-p :abandoned)
                                 (live-p :active)
                                 (t :missing))
              :terminal-status (and marker (getf marker :status))
              :finished-at (and marker (getf marker :finished-at))
              :base-ref resolved-base-ref
              :dirty-p (and dirty-entries (plusp (length dirty-entries)))
              :dirty-entries dirty-entries
              :unique-commit-count unique-commit-count
              :cleanup-classification cleanup-classification
              :error-message (or dirty-error unique-error))))))

(defun cleanup-abandoned-local-worktree (&key runtime
                                              repo-root
                                              worktree
                                              worktree-id
                                              worktree-path
                                              worktree-branch
                                              status
                                              finished-at
                                              now
                                              grace-period-seconds
                                              base-ref
                                              force)
  (multiple-value-bind (resolved-runtime metadata resolved-repo-root)
      (%resolve-worktree-runtime-and-metadata
       :runtime runtime
       :repo-root repo-root
       :worktree worktree
       :worktree-id worktree-id
       :worktree-path worktree-path
       :worktree-branch worktree-branch)
    (let* ((inspection (inspect-local-worktree
                        :runtime resolved-runtime
                        :repo-root resolved-repo-root
                        :worktree metadata
                        :base-ref base-ref))
           (resolved-id (getf inspection :id))
           (marker (%read-local-worktree-abandonment resolved-runtime metadata))
           (timestamp (or now (get-universal-time)))
           (resolved-status (or (%normalize-worktree-status status)
                                (and marker (getf marker :status))))
           (resolved-finished-at (or finished-at
                                     (and marker (getf marker :finished-at))
                                     timestamp))
           (age-seconds (max 0 (- timestamp resolved-finished-at)))
           (grace-seconds (if (null grace-period-seconds)
                              *default-worktree-cleanup-grace-period-seconds*
                              grace-period-seconds))
           (grace-expired-p (>= age-seconds grace-seconds))
           (classification (getf inspection :cleanup-classification))
           (action nil)
           (reason nil)
           (error-message nil))
      (cond
        ((null resolved-status)
         (setf action :skip
               reason :missing-status))
        ((not (member resolved-status
                      *worktree-abandoned-terminal-statuses*
                      :test #'eq))
         (setf action :skip
               reason :non-abandoned-status))
        ((not (getf inspection :live-p))
         (setf action :already-removed
               reason :missing))
        ((not grace-expired-p)
         (setf action :retain
               reason :grace-period))
        ((eq classification :dirty)
         (setf action :review-required
               reason :dirty))
        ((eq classification :review-required)
         (setf action :review-required
               reason :unique-commits))
        ((eq classification :safe-to-prune)
         (handler-case
             (progn
               (kill-local-worktree resolved-runtime
                                    resolved-id
                                    :force force
                                    :worktree metadata)
               (setf action :deleted
                     reason :expired-safe))
           (error (condition)
             (setf action :error
                   reason :delete-failed
                   error-message (princ-to-string condition)))))
        (t
         (setf action :retain
               reason :unknown)))
      (when (and resolved-runtime
                 resolved-id
                 (member action '(:deleted :already-removed) :test #'eq))
        (%delete-worktree-marker
         (%worktree-abandoned-marker-path resolved-runtime resolved-id)))
      (append inspection
              (list :action action
                    :reason reason
                    :terminal-status resolved-status
                    :finished-at resolved-finished-at
                    :age-seconds age-seconds
                    :grace-period-seconds grace-seconds
                    :grace-expired-p grace-expired-p
                    :error-message (or error-message
                                       (getf inspection :error-message)))))))

(defun cleanup-abandoned-local-worktrees (&key runtime
                                               repo-root
                                               now
                                               grace-period-seconds
                                               base-ref
                                               force)
  (let ((resolved-runtime (%runtime-for-worktree-source
                           :runtime runtime
                           :repo-root repo-root
                           :worktree-path nil)))
    (unless resolved-runtime
      (return-from cleanup-abandoned-local-worktrees '()))
    (let ((results '()))
      (dolist (marker-path (%worktree-abandoned-marker-files resolved-runtime))
        (let ((marker (%read-worktree-marker marker-path)))
          (when (%worktree-marker-p marker)
            (push (cleanup-abandoned-local-worktree
                   :runtime resolved-runtime
                   :repo-root (and (getf marker :repo-root)
                                   (pathname (getf marker :repo-root)))
                   :worktree (make-worktree-metadata
                              :id (getf marker :worktree-id)
                              :branch (getf marker :branch)
                              :path (getf marker :path))
                   :status (getf marker :status)
                   :finished-at (getf marker :finished-at)
                   :now now
                   :grace-period-seconds grace-period-seconds
                   :base-ref base-ref
                   :force force)
                  results))))
      (nreverse results))))
