(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; NXT-336: Amoebum-local worktree runtime wrapper
;;;
;;; Keep repo-root, scratch-root, and lock-root policy in Amoebum while
;;; delegating git worktree lifecycle mechanics to sw4rm-sdk.
;;; ---------------------------------------------------------------------------

(defstruct (worktree-runtime
            (:constructor %make-worktree-runtime
                (&key repo-root scratch-root lock-root coordinator backend)))
  (repo-root nil :type pathname)
  (scratch-root nil :type pathname)
  (lock-root nil :type pathname)
  coordinator
  (backend :local :type keyword))

(defstruct (worktree-metadata
            (:constructor %make-worktree-metadata
                (&key id branch path)))
  (id nil :type (or null string))
  (branch nil :type (or null string))
  (path nil :type (or null string)))

(defvar *current-delegated-agent-id* nil)
(defvar *current-delegated-agent-backend* nil)
(defvar *current-delegated-agent-worktree* nil)

;; Loaded later from src/worktrees/cleanup.lisp.
(declaim (ftype function
                mark-local-worktree-abandoned
                inspect-local-worktree
                cleanup-abandoned-local-worktree
                cleanup-abandoned-local-worktrees))
;; Loaded later from src/worktrees/handoffs.lisp.
(declaim (ftype function
                create-worktree-conflict-handoff
                clear-worktree-conflict-handoffs
                list-worktree-conflict-handoffs
                find-worktree-conflict-handoff
                accept-worktree-conflict-handoff
                defer-worktree-conflict-handoff
                resolve-worktree-conflict-handoff
                abandon-worktree-conflict-handoff))
;; Loaded later from src/worktrees/merge.lisp.
(declaim (ftype function
                resolve-worktree-merge-target
                preflight-local-worktree-merge
                merge-local-worktree))

(defmacro with-delegated-agent-worktree-context ((&key agent-id backend worktree)
                                                 &body body)
  `(let ((*current-delegated-agent-id* ,agent-id)
         (*current-delegated-agent-backend* ,backend)
         (*current-delegated-agent-worktree*
           (coerce-worktree-metadata :worktree ,worktree)))
     ,@body))

(defun current-delegated-agent-id ()
  *current-delegated-agent-id*)

(defun current-delegated-agent-backend ()
  *current-delegated-agent-backend*)

(defun current-delegated-agent-worktree ()
  *current-delegated-agent-worktree*)

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

(defun current-delegated-agent-push-branch ()
  (let ((metadata (current-delegated-agent-worktree)))
    (and metadata
         (%strip-live-worktree-branch-ref
          (worktree-metadata-branch metadata)))))

(defun %maybe-user-session-registry ()
  (let ((symbol (find-symbol "*USER-SESSION-REGISTRY*" :amoebum)))
    (when (and symbol (boundp symbol))
      (let ((value (symbol-value symbol)))
        (when (ignore-errors (typep value 'sw4rm-sdk:local-registry))
          value)))))

(defun %worktree-env-name-valid-p (name)
  (and (stringp name)
       (plusp (length name))
       (let ((first (char name 0)))
         (and (or (alpha-char-p first)
                  (char= first #\_))
              (loop for char across name
                    always (or (alpha-char-p char)
                               (digit-char-p char)
                               (char= char #\_)))))))

(defun %delegated-agent-provider-secret-keys (registry agent-id)
  (sort (copy-list
         (or (ignore-errors
               (sw4rm-sdk:local-registry-list-provider-secret-keys
                registry
                agent-id
                agent-id))
             '()))
        #'string<))

(defun current-delegated-agent-secret-env-overrides (&optional agent-id)
  (let* ((resolved-agent-id
           (%normalize-worktree-string
            (or agent-id
                (current-delegated-agent-id))))
         (registry (%maybe-user-session-registry)))
    (when (and registry resolved-agent-id)
      (let ((overrides '()))
        (dolist (key (%delegated-agent-provider-secret-keys
                      registry
                      resolved-agent-id)
                     (nreverse overrides))
          (let* ((env-name (%normalize-worktree-string key))
                 (secret-value
                   (ignore-errors
                     (sw4rm-sdk:local-registry-resolve-provider-secret
                      registry
                      resolved-agent-id
                      resolved-agent-id
                      env-name
                      :signal-if-missing nil))))
            (when (and (%worktree-env-name-valid-p env-name)
                       (stringp secret-value)
                       (plusp (length secret-value)))
              (push (cons env-name secret-value) overrides))))))))

(defun %worktree-path-namestring (path)
  (let ((pathname (etypecase path
                    (pathname path)
                    (string (pathname path)))))
    (namestring (uiop:ensure-directory-pathname pathname))))

(defun %worktree-trim-output (value)
  (let ((normalized (%normalize-worktree-string value)))
    (or normalized "")))

(defun %normalize-worktree-status (status)
  (cond
    ((keywordp status) status)
    ((symbolp status)
     (intern (string-upcase (symbol-name status)) :keyword))
    ((stringp status)
     (intern (string-upcase (%worktree-trim-output status)) :keyword))
    (t nil)))

(defun %strip-live-worktree-branch-ref (branch)
  (let ((normalized (%normalize-worktree-string branch)))
    (cond
      ((null normalized) nil)
      ((uiop:string-prefix-p "refs/heads/" normalized)
       (subseq normalized (length "refs/heads/")))
      (t
       normalized))))

(defun %run-worktree-git (directory args)
  (let ((working-directory
          (etypecase directory
            (pathname directory)
            (string (pathname directory)))))
    (multiple-value-bind (stdout stderr status)
        (uiop:run-program (append (list "git") args)
                          :directory working-directory
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (values stdout stderr status))))

(defun %git-output-string (directory args)
  (multiple-value-bind (stdout stderr status)
      (%run-worktree-git directory args)
    (if (zerop status)
        (values (%normalize-worktree-string stdout) nil)
        (values nil (%normalize-worktree-string stderr)))))

(defun %git-output-lines (directory args)
  (multiple-value-bind (stdout stderr status)
      (%run-worktree-git directory args)
    (if (zerop status)
        (values (remove-if (lambda (line)
                             (zerop (length line)))
                           (uiop:split-string stdout
                                              :separator '(#\Newline)))
                nil)
        (values nil (%normalize-worktree-string stderr)))))

(defun %git-ref-exists-p (directory ref)
  (when (%normalize-worktree-string ref)
    (multiple-value-bind (_stdout _stderr status)
        (%run-worktree-git directory
                           (list "rev-parse" "--verify" "--quiet" ref))
      (declare (ignore _stdout _stderr))
      (zerop status))))

(defun %git-current-head-state (directory)
  (multiple-value-bind (branch _branch-error)
      (%git-output-string directory '("rev-parse" "--abbrev-ref" "HEAD"))
    (declare (ignore _branch-error))
    (let ((resolved-branch (%normalize-worktree-string branch)))
      (if (or (null resolved-branch)
              (string= resolved-branch "HEAD"))
          (multiple-value-bind (commit _commit-error)
              (%git-output-string directory '("rev-parse" "HEAD"))
            (declare (ignore _commit-error))
            (list :detached-p t
                  :ref commit))
          (list :detached-p nil
                :ref resolved-branch)))))

(defun %restore-git-head-state (directory head-state)
  (let ((ref (%normalize-worktree-string (getf head-state :ref))))
    (when ref
      (%run-worktree-git directory
                         (if (getf head-state :detached-p)
                             (list "checkout" "--detach" ref)
                             (list "checkout" ref))))))

(defun %normalize-worktree-relative-path (value)
  (let ((normalized (%normalize-worktree-string value)))
    (when normalized
      (let* ((slashified (substitute #\/ #\\ normalized))
             (without-dot (if (uiop:string-prefix-p "./" slashified)
                              (subseq slashified 2)
                              slashified))
             (trimmed (string-right-trim "/" without-dot)))
        (unless (or (zerop (length trimmed))
                    (string= trimmed "."))
          trimmed)))))

(defun %worktree-relative-path-to-root (repo-root path)
  (when (and repo-root path)
    (let* ((root (uiop:ensure-directory-pathname (pathname repo-root)))
           (candidate (uiop:ensure-directory-pathname (pathname path)))
           (relative (%normalize-worktree-relative-path
                      (enough-namestring candidate root))))
      (when (and relative
                 (not (uiop:string-prefix-p "/" relative))
                 (not (string= relative ".."))
                 (not (uiop:string-prefix-p "../" relative)))
        relative))))

(defun %worktree-relative-path-ancestors (relative-path)
  (let ((segments (uiop:split-string relative-path :separator '(#\/))))
    (loop for size from (length segments) downto 1
          collect (format nil "~{~A~^/~}"
                          (subseq segments 0 size)))))

(defun %git-status-entry-path (entry)
  (let* ((start (min 3 (length entry)))
         (payload (%normalize-worktree-string (subseq entry start)))
         (arrow-index (and payload
                           (search " -> " payload :from-end t))))
    (%normalize-worktree-relative-path
     (if arrow-index
         (subseq payload (+ arrow-index 4))
         payload))))

(defun %git-status-entry-ignored-p (entry ignored-path-prefixes)
  (let ((path (%git-status-entry-path entry)))
    (and path
         (some (lambda (prefix)
                 (or (string= path prefix)
                     (uiop:string-prefix-p (format nil "~A/" prefix)
                                           path)))
               ignored-path-prefixes))))

(defun %managed-worktree-status-ignored-paths (runtime repo-root)
  (when (and runtime repo-root)
    (remove-duplicates
     (loop for managed-root in (list (worktree-runtime-scratch-root runtime)
                                     (worktree-runtime-lock-root runtime))
           for relative = (%worktree-relative-path-to-root repo-root managed-root)
           when relative
             append (%worktree-relative-path-ancestors relative))
     :test #'string=)))

(defun %git-clean-working-tree-p (directory &key ignored-path-prefixes)
  (multiple-value-bind (entries error-message)
      (%git-output-lines directory '("status" "--porcelain"))
    (if error-message
        (values nil error-message)
        (let ((relevant-entries
                (if ignored-path-prefixes
                    (remove-if (lambda (entry)
                                 (%git-status-entry-ignored-p
                                  entry
                                  ignored-path-prefixes))
                               entries)
                    entries)))
          (values (null relevant-entries) nil)))))

(defun %default-worktree-base-ref (repo-root)
  (or (multiple-value-bind (branch _error)
          (%git-output-string repo-root '("rev-parse" "--abbrev-ref" "HEAD"))
        (declare (ignore _error))
        (let ((resolved (%normalize-worktree-string branch)))
          (unless (or (null resolved)
                      (string= resolved "HEAD"))
            resolved)))
      (multiple-value-bind (commit _error)
          (%git-output-string repo-root '("rev-parse" "HEAD"))
        (declare (ignore _error))
        commit)))

(defun %derive-worktree-repo-root (worktree-path)
  (let* ((resolved-path (%normalize-worktree-path-string worktree-path))
         (pathname (and resolved-path (pathname resolved-path))))
    (when (and pathname (probe-file pathname))
      (or (multiple-value-bind (common-dir _stderr status)
              (%run-worktree-git pathname
                                 '("rev-parse"
                                   "--path-format=absolute"
                                   "--git-common-dir"))
            (declare (ignore _stderr))
            (when (zerop status)
              (let* ((normalized (%normalize-worktree-path-string common-dir))
                     (trimmed (and normalized
                                   (string-right-trim "/" normalized)))
                     (git-suffix "/.git"))
                     (when (and trimmed
                                (uiop:string-suffix-p trimmed git-suffix))
                  (uiop:ensure-directory-pathname
                   (pathname
                    (subseq trimmed
                            0
                            (- (length trimmed)
                               (length git-suffix)))))))))
      (multiple-value-bind (stdout _stderr status)
          (%run-worktree-git pathname '("rev-parse" "--show-toplevel"))
        (declare (ignore _stderr))
        (when (zerop status)
          (uiop:ensure-directory-pathname
           (pathname (%worktree-trim-output stdout)))))))))

(defun %copy-worktree-data (value)
  (and value (copy-tree value)))
