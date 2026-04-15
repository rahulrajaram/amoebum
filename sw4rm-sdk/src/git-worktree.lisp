;;;; git-worktree.lisp
;;;; Local git-worktree coordination helpers with advisory locks.

(in-package :sw4rm-sdk)

(defgeneric spawn-worktree (coordinator worktree-id worktree-path branch &key base-ref)
  (:documentation "Spawn a worktree through COORDINATOR."))

(defgeneric collect-worktree (coordinator worktree-id)
  (:documentation "Collect worktree metadata through COORDINATOR."))

(defgeneric inspect-worktree (coordinator worktree-id &key worktree-path branch)
  (:documentation "Inspect worktree lifecycle state through COORDINATOR."))

(defgeneric merge-worktree (coordinator request)
  (:documentation "Merge worktree described by REQUEST through COORDINATOR."))

(defgeneric kill-worktree (coordinator worktree-id &key force)
  (:documentation "Delete a worktree through COORDINATOR."))

(defmacro with-worktree-lock ((lock-path) &body body)
  "Acquire advisory lock at LOCK-PATH for BODY duration."
  (let ((stream-sym (gensym "LOCK-STREAM-"))
        (path-sym (gensym "LOCK-PATH-")))
    `(let* ((,path-sym ,lock-path)
            (,stream-sym (open ,path-sym
                               :direction :output
                               :if-exists nil
                               :if-does-not-exist :create)))
       (unless ,stream-sym
         (error 'worktree-error
                :message (format nil "Worktree lock busy: ~A" ,path-sym)
                :worktree-id ,path-sym
                :state :lock-busy))
       (unwind-protect
            (progn ,@body)
         (when ,stream-sym
           (close ,stream-sym))
         (ignore-errors (delete-file ,path-sym))))))

(defun %run-git (repo-root &rest args)
  "Run git ARGS in REPO-ROOT and return stdout string."
  (multiple-value-bind (stdout stderr status)
      (uiop:run-program (append (list "git") args)
                        :directory repo-root
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (unless (zerop status)
      (error 'worktree-error
             :message (format nil "git ~{~A~^ ~} failed: ~A" args stderr)
             :worktree-id (or (second args) "")
             :state :git-failed))
    stdout))

(defun git-worktree-add (repo-root worktree-path branch &key base-ref)
  "Create a new git worktree at WORKTREE-PATH."
  (%run-git repo-root
            "worktree" "add"
            worktree-path
            (or base-ref branch))
  (when (and branch (> (length branch) 0))
    (%run-git worktree-path "checkout" "-B" branch))
  worktree-path)

(defun git-worktree-remove (repo-root worktree-path &key force)
  "Remove git worktree at WORKTREE-PATH."
  (apply #'%run-git repo-root
         (append (list "worktree" "remove")
                 (when force (list "--force"))
                 (list worktree-path)))
  t)

(defun git-worktree-prune (repo-root)
  "Prune stale git worktree metadata."
  (%run-git repo-root "worktree" "prune")
  t)

(defun git-worktree-list (repo-root)
  "Return parsed git worktree list as plist records."
  (let ((lines (uiop:split-string
                (%run-git repo-root "worktree" "list" "--porcelain")
                :separator '(#\Newline)))
        (results nil)
        (current nil))
    (dolist (line lines)
      (cond
        ((string= line "")
         (when current
           (push current results)
           (setf current nil)))
        ((uiop:string-prefix-p "worktree " line)
         (setf current (list :path (subseq line (length "worktree ")))))
        ((uiop:string-prefix-p "HEAD " line)
         (setf (getf current :head) (subseq line (length "HEAD "))))
        ((uiop:string-prefix-p "branch " line)
         (setf (getf current :branch) (subseq line (length "branch "))))
        (t
         (push line (getf current :flags)))))
    (when current
      (push current results))
    (nreverse results)))

(defclass git-worktree-coordinator ()
  ((repo-root
    :initarg :repo-root
    :accessor git-worktree-coordinator-repo-root)
   (lock-dir
    :initarg :lock-dir
    :accessor git-worktree-coordinator-lock-dir
    :initform nil)
   (state
    :initform (make-hash-table :test #'equal)
    :accessor git-worktree-coordinator-state))
  (:documentation "Coordinates spawn/collect/kill flow for worktrees."))

(defclass remote-worktree-coordinator ()
  ((spawn-fn
    :initarg :spawn-fn
    :reader remote-worktree-coordinator-spawn-fn)
   (collect-fn
    :initarg :collect-fn
    :reader remote-worktree-coordinator-collect-fn)
   (inspect-fn
    :initarg :inspect-fn
    :reader remote-worktree-coordinator-inspect-fn
    :initform nil)
   (merge-fn
    :initarg :merge-fn
    :reader remote-worktree-coordinator-merge-fn
    :initform nil)
   (kill-fn
    :initarg :kill-fn
    :reader remote-worktree-coordinator-kill-fn))
  (:documentation "Backend-neutral remote worktree service shim."))

(defun make-git-worktree-coordinator (repo-root &key lock-dir)
  "Construct a git worktree coordinator."
  (make-instance 'git-worktree-coordinator
                 :repo-root repo-root
                 :lock-dir (or lock-dir (merge-pathnames ".sw4rm-locks/" repo-root))))

(defun make-remote-worktree-coordinator (&key spawn-fn collect-fn inspect-fn merge-fn kill-fn)
  "Construct a remote worktree coordinator around backend callbacks."
  (unless spawn-fn
    (error "REMOTE-WORKTREE-COORDINATOR requires SPAWN-FN."))
  (unless collect-fn
    (error "REMOTE-WORKTREE-COORDINATOR requires COLLECT-FN."))
  (unless kill-fn
    (error "REMOTE-WORKTREE-COORDINATOR requires KILL-FN."))
  (make-instance 'remote-worktree-coordinator
                 :spawn-fn spawn-fn
                 :collect-fn collect-fn
                 :inspect-fn inspect-fn
                 :merge-fn merge-fn
                 :kill-fn kill-fn))

(defun %ensure-lock-dir (coordinator)
  (ensure-directories-exist (git-worktree-coordinator-lock-dir coordinator)))

(defun %worktree-lock-path (coordinator worktree-id)
  (merge-pathnames
   (format nil "~A.lock" worktree-id)
   (git-worktree-coordinator-lock-dir coordinator)))

(defmethod spawn-worktree ((coordinator git-worktree-coordinator)
                           worktree-id
                           worktree-path
                           branch
                           &key base-ref)
  "Spawn a worktree under coordinator lock."
  (%ensure-lock-dir coordinator)
  (with-worktree-lock ((%worktree-lock-path coordinator worktree-id))
    (git-worktree-add (git-worktree-coordinator-repo-root coordinator)
                      worktree-path
                      branch
                      :base-ref base-ref)
    (setf (gethash worktree-id (git-worktree-coordinator-state coordinator))
          (list :worktree-id worktree-id
                :path worktree-path
                :branch branch
                :state :spawned
                :updated-at (get-universal-time)))
    (gethash worktree-id (git-worktree-coordinator-state coordinator))))

(defmethod collect-worktree ((coordinator git-worktree-coordinator) worktree-id)
  "Return tracked metadata for WORKTREE-ID plus live git status."
  (let ((record (gethash worktree-id (git-worktree-coordinator-state coordinator))))
    (unless record
      (error 'worktree-error
             :message (format nil "Unknown worktree-id ~A" worktree-id)
             :worktree-id worktree-id
             :state :unknown))
    (let ((live (git-worktree-list (git-worktree-coordinator-repo-root coordinator))))
      (list :record record
            :live (find (getf record :path)
                        live
                        :key (lambda (item) (getf item :path))
                        :test #'string=)))))

(defmethod inspect-worktree ((coordinator git-worktree-coordinator) worktree-id
                             &key worktree-path branch)
  (declare (ignore worktree-path branch))
  (let* ((collected (collect-worktree coordinator worktree-id))
         (record (getf collected :record))
         (live (getf collected :live)))
    (list :id (and record (getf record :worktree-id))
          :path (or (and live (getf live :path))
                    (and record (getf record :path)))
          :branch (or (and live (getf live :branch))
                      (and record (getf record :branch)))
          :record record
          :live live
          :live-p (not (null live))
          :lifecycle-state (if live :active :missing)
          :backend :local)))

(defmethod merge-worktree ((coordinator git-worktree-coordinator) request)
  (declare (ignore request))
  (error 'worktree-error
         :message "Git worktree coordinator does not implement merge-worktree."
         :worktree-id ""
         :state :unsupported))

(defmethod kill-worktree ((coordinator git-worktree-coordinator) worktree-id &key force)
  "Remove a tracked worktree under coordinator lock."
  (%ensure-lock-dir coordinator)
  (let ((record (gethash worktree-id (git-worktree-coordinator-state coordinator))))
    (unless record
      (error 'worktree-error
             :message (format nil "Unknown worktree-id ~A" worktree-id)
             :worktree-id worktree-id
             :state :unknown))
    (with-worktree-lock ((%worktree-lock-path coordinator worktree-id))
      (git-worktree-remove (git-worktree-coordinator-repo-root coordinator)
                           (getf record :path)
                           :force force)
      (remhash worktree-id (git-worktree-coordinator-state coordinator))
      (git-worktree-prune (git-worktree-coordinator-repo-root coordinator))
      t)))

(defmethod spawn-worktree ((coordinator remote-worktree-coordinator)
                           worktree-id
                           worktree-path
                           branch
                           &key base-ref)
  (funcall (remote-worktree-coordinator-spawn-fn coordinator)
           worktree-id
           worktree-path
           branch
           :base-ref base-ref))

(defmethod collect-worktree ((coordinator remote-worktree-coordinator) worktree-id)
  (funcall (remote-worktree-coordinator-collect-fn coordinator)
           worktree-id))

(defmethod inspect-worktree ((coordinator remote-worktree-coordinator) worktree-id
                             &key worktree-path branch)
  (let ((fn (remote-worktree-coordinator-inspect-fn coordinator)))
    (if fn
        (funcall fn worktree-id :worktree-path worktree-path :branch branch)
        (collect-worktree coordinator worktree-id))))

(defmethod merge-worktree ((coordinator remote-worktree-coordinator) request)
  (let ((fn (remote-worktree-coordinator-merge-fn coordinator)))
    (unless fn
      (error 'worktree-error
             :message "Remote worktree coordinator does not implement merge-worktree."
             :worktree-id (or (getf request :worktree-id) "")
             :state :unsupported))
    (funcall fn request)))

(defmethod kill-worktree ((coordinator remote-worktree-coordinator) worktree-id &key force)
  (funcall (remote-worktree-coordinator-kill-fn coordinator)
           worktree-id
           :force force))
