;;;; git-worktree.lisp
;;;; Local git-worktree coordination helpers with advisory locks.

(in-package :sw4rm-sdk)

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

(defun make-git-worktree-coordinator (repo-root &key lock-dir)
  "Construct a git worktree coordinator."
  (make-instance 'git-worktree-coordinator
                 :repo-root repo-root
                 :lock-dir (or lock-dir (merge-pathnames ".sw4rm-locks/" repo-root))))

(defun %ensure-lock-dir (coordinator)
  (ensure-directories-exist (git-worktree-coordinator-lock-dir coordinator)))

(defun %worktree-lock-path (coordinator worktree-id)
  (merge-pathnames
   (format nil "~A.lock" worktree-id)
   (git-worktree-coordinator-lock-dir coordinator)))

(defun spawn-worktree (coordinator worktree-id worktree-path branch &key base-ref)
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

(defun collect-worktree (coordinator worktree-id)
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

(defun kill-worktree (coordinator worktree-id &key force)
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
