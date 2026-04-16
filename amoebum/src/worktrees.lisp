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

(defun %normalize-worktree-backend (value)
  (case value
    ((nil :local) :local)
    ((:remote :sw4rm :networked) :remote)
    (otherwise
     (error "Unsupported worktree backend ~S." value))))

(defun %default-worktree-backend (&optional (config (ignore-errors (current-config))))
  (let ((mode (ignore-errors (%configured-swarm-delegation-mode config))))
    (if (eq mode :networked)
        :remote
        :local)))

(defun make-worktree-runtime (&key project-root config repo-root scratch-root lock-root
                                   coordinator backend)
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
                (default-worktree-lock-root :scratch-root resolved-scratch-root))))
         (resolved-backend (%normalize-worktree-backend
                            (or backend
                                (and coordinator :remote)
                                (%default-worktree-backend config))))
         (resolved-coordinator (or coordinator
                                  (sw4rm-sdk:make-git-worktree-coordinator
                                   resolved-repo-root
                                   :lock-dir resolved-lock-root))))
    (ensure-directories-exist resolved-scratch-root)
    (ensure-directories-exist resolved-lock-root)
    (%make-worktree-runtime
     :repo-root resolved-repo-root
     :scratch-root resolved-scratch-root
     :lock-root resolved-lock-root
     :backend resolved-backend
     :coordinator resolved-coordinator)))

(defun current-worktree-runtime (&optional (config (ignore-errors (current-config))))
  (make-worktree-runtime :config config))

(defun worktree-runtime-local-p (runtime)
  (eq (worktree-runtime-backend runtime) :local))

(defun worktree-runtime-remote-p (runtime)
  (eq (worktree-runtime-backend runtime) :remote))

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

(defun %normalize-worktree-name-segment (value)
  (let ((normalized (%normalize-worktree-string value)))
    (when normalized
      (let ((buffer (make-string-output-stream))
            (last-was-dash nil))
        (loop for ch across (string-downcase normalized) do
          (cond
            ((or (alphanumericp ch)
                 (char= ch #\_)
                 (char= ch #\-)
                 (char= ch #\.))
             (write-char ch buffer)
             (setf last-was-dash nil))
            (last-was-dash
             nil)
            (t
             (write-char #\- buffer)
             (setf last-was-dash t))))
        (let ((segment (string-trim "-." (get-output-stream-string buffer))))
          (unless (plusp (length segment))
            (error "Invalid worktree naming segment ~S." value))
          segment)))))

(defun resolve-worktree-id (&key worktree worktree-id worktree-branch workflow-id node-id)
  (let* ((base (coerce-worktree-metadata :worktree worktree))
         (explicit-id (or (%normalize-worktree-string worktree-id)
                          (and base (worktree-metadata-id base))))
         (explicit-branch (or (%normalize-worktree-string worktree-branch)
                              (and base (worktree-metadata-branch base))))
         (workflow-segment (%normalize-worktree-name-segment workflow-id))
         (node-segment (%normalize-worktree-name-segment node-id)))
    (cond
      (explicit-id
       (%worktree-id-path-component explicit-id))
      (explicit-branch
       (%worktree-id-path-component explicit-branch))
      ((and workflow-segment node-segment)
       (%worktree-id-path-component
        (format nil "sw4rm/~A/~A" workflow-segment node-segment)))
      (workflow-segment
       (%worktree-id-path-component
        (format nil "sw4rm/~A/local" workflow-segment)))
      (node-segment
       (%worktree-id-path-component
        (format nil "sw4rm/local/~A" node-segment)))
      (t
       nil))))

(defun resolve-worktree-branch (&key worktree worktree-branch worktree-id workflow-id node-id)
  (let* ((base (coerce-worktree-metadata :worktree worktree))
         (explicit-branch (or (%normalize-worktree-string worktree-branch)
                              (and base (worktree-metadata-branch base))))
         (workflow-segment (%normalize-worktree-name-segment workflow-id))
         (node-segment (%normalize-worktree-name-segment node-id))
         (local-segment (%normalize-worktree-name-segment
                         (or (%normalize-worktree-string worktree-id)
                             (and base (worktree-metadata-id base))
                             (resolve-worktree-id :worktree base
                                                  :workflow-id workflow-id
                                                  :node-id node-id)))))
    (cond
      (explicit-branch
       explicit-branch)
      ((and workflow-segment node-segment)
       (format nil "sw4rm/~A/~A" workflow-segment node-segment))
      (workflow-segment
       (format nil "sw4rm/~A/local" workflow-segment))
      (node-segment
       (format nil "sw4rm/local/~A" node-segment))
      (local-segment
       (format nil "sw4rm/local/~A" local-segment))
      (t
       nil))))

(defun resolve-worktree-workflow-branch (&key worktree worktree-branch)
  (let* ((base (coerce-worktree-metadata :worktree worktree
                                         :worktree-branch worktree-branch))
         (branch (%normalize-worktree-string
                  (or worktree-branch
                      (and base (worktree-metadata-branch base))))))
    (when branch
      (let ((segments (uiop:split-string branch :separator '(#\/))))
        (when (and (>= (length segments) 3)
                   (string= "sw4rm" (first segments)))
          (cond
            ((string= "local" (second segments))
             nil)
            ((and (string= "workflow" (second segments))
                  (>= (length segments) 3))
             (format nil "sw4rm/workflow/~A" (third segments)))
            (t
             (format nil "sw4rm/workflow/~A" (second segments)))))))))

(defun resolve-worktree-metadata (&key runtime
                                       worktree
                                       worktree-id
                                       worktree-branch
                                       worktree-path
                                       workflow-id
                                       node-id)
  (let* ((base (coerce-worktree-metadata :worktree worktree))
         (resolved-id (resolve-worktree-id :worktree base
                                           :worktree-id worktree-id
                                           :worktree-branch worktree-branch
                                           :workflow-id workflow-id
                                           :node-id node-id))
         (resolved-branch (resolve-worktree-branch :worktree base
                                                   :worktree-branch worktree-branch
                                                   :worktree-id (or worktree-id resolved-id)
                                                   :workflow-id workflow-id
                                                   :node-id node-id))
         (resolved-path (or (%normalize-worktree-path-string worktree-path)
                            (and base (worktree-metadata-path base))
                            (and runtime resolved-id
                                 (namestring
                                  (worktree-runtime-path runtime resolved-id))))))
    (when (or resolved-id resolved-branch resolved-path)
      (make-worktree-metadata :id resolved-id
                              :branch resolved-branch
                              :path resolved-path))))

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

(defun %runtime-for-worktree-source (&key runtime repo-root worktree-path)
  (or runtime
      (and repo-root
           (make-worktree-runtime :repo-root repo-root))
      (let ((derived-root (%derive-worktree-repo-root worktree-path)))
        (and derived-root
             (make-worktree-runtime :repo-root derived-root)))))

(defun %resolve-worktree-runtime-and-metadata (&key runtime
                                                    repo-root
                                                    worktree
                                                    worktree-id
                                                    worktree-path
                                                    worktree-branch)
  (let* ((base (coerce-worktree-metadata :worktree worktree
                                         :worktree-id worktree-id
                                         :worktree-path worktree-path
                                         :worktree-branch worktree-branch))
         (resolved-runtime (%runtime-for-worktree-source
                            :runtime runtime
                            :repo-root repo-root
                            :worktree-path (and base
                                                (worktree-metadata-path base))))
         (resolved-metadata (resolve-worktree-metadata
                             :runtime resolved-runtime
                             :worktree base
                             :worktree-id worktree-id
                             :worktree-path worktree-path
                             :worktree-branch worktree-branch))
         (resolved-repo-root (or repo-root
                                 (and resolved-runtime
                                      (worktree-runtime-repo-root resolved-runtime))
                                 (%derive-worktree-repo-root
                                  (and resolved-metadata
                                       (worktree-metadata-path resolved-metadata))))))
    (values resolved-runtime resolved-metadata resolved-repo-root)))

(defun %find-live-local-worktree (repo-root metadata)
  (when (and repo-root metadata)
    (let* ((live-worktrees (sw4rm-sdk:git-worktree-list repo-root))
           (resolved-path (worktree-metadata-path metadata))
           (resolved-branch (worktree-metadata-branch metadata)))
      (or (and resolved-path
               (find (%worktree-path-namestring resolved-path)
                     live-worktrees
                     :key (lambda (item)
                            (%worktree-path-namestring (getf item :path)))
                     :test #'string=))
          (and resolved-branch
               (find resolved-branch
                     live-worktrees
                     :key (lambda (item)
                            (%strip-live-worktree-branch-ref
                             (getf item :branch)))
                     :test #'string=))
          nil))))

(defun %reconstruct-local-worktree-record (metadata live-record)
  (when (or metadata live-record)
    (list :worktree-id (or (and metadata (worktree-metadata-id metadata))
                           (and live-record
                                (let ((path (getf live-record :path)))
                                  (and path
                                       (file-namestring
                                        (pathname (%worktree-path-namestring path)))))))
          :path (or (and live-record
                         (%worktree-path-namestring (getf live-record :path)))
                    (and metadata (worktree-metadata-path metadata)))
          :branch (or (and live-record
                           (%strip-live-worktree-branch-ref
                            (getf live-record :branch)))
                      (and metadata (worktree-metadata-branch metadata)))
          :state (if live-record :spawned :missing)
          :updated-at (get-universal-time))))

(defun %copy-worktree-data (value)
  (and value (copy-tree value)))

(defun %local-worktree-preflight-result (inspection status target-ref
                                          &key merge-base
                                               worktree-files
                                               target-files
                                               conflicts
                                               conflict-kind
                                               error-message)
  (append inspection
          (list :status status
                :target-ref target-ref
                :merge-base merge-base
                :worktree-files worktree-files
                :target-files target-files
                :conflict-kind conflict-kind
                :conflict-p (not (null conflicts))
                :conflicts (or conflicts '())
                :error-message error-message)))

(defun %git-diff-name-set (directory revision-range)
  (multiple-value-bind (files error-message)
      (%git-output-lines directory
                         (list "diff" "--name-only" revision-range))
    (if error-message
        (values nil error-message)
        (values (sort (remove-duplicates files :test #'string=)
                      #'string<)
                nil))))

(defun %compute-local-worktree-preflight-data (repo-root worktree-path target-ref)
  (multiple-value-bind (head head-error)
      (%git-output-string worktree-path '("rev-parse" "HEAD"))
    (if (null head)
        (values nil head-error)
        (multiple-value-bind (merge-base merge-base-error)
            (%git-output-string repo-root
                                (list "merge-base" target-ref head))
          (if (null merge-base)
              (values nil merge-base-error)
              (multiple-value-bind (worktree-files worktree-error)
                  (%git-diff-name-set worktree-path
                                      (format nil "~A..HEAD" merge-base))
                (if worktree-error
                    (values nil worktree-error)
                    (multiple-value-bind (target-files target-error)
                        (%git-diff-name-set repo-root
                                            (format nil "~A..~A"
                                                    merge-base
                                                    target-ref))
                      (if target-error
                          (values nil target-error)
                          (let ((conflicts (sort (intersection worktree-files
                                                             target-files
                                                             :test #'string=)
                                                 #'string<)))
                            (values (list :merge-base merge-base
                                          :worktree-files worktree-files
                                          :target-files target-files
                                          :conflicts conflicts
                                          :conflict-kind (and conflicts
                                                              :file-overlap))
                                    nil)))))))))))

(defun resolve-worktree-merge-target (&key runtime
                                           repo-root
                                           worktree
                                           worktree-id
                                           worktree-path
                                           worktree-branch
                                           target-ref)
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
                        :base-ref target-ref))
           (resolved-base-ref (%normalize-worktree-string
                               (getf inspection :base-ref)))
           (workflow-target (and (null (%normalize-worktree-string target-ref))
                                 (resolve-worktree-workflow-branch
                                  :worktree metadata)))
           (resolved-target-ref (or (%normalize-worktree-string target-ref)
                                    workflow-target
                                    resolved-base-ref))
           (target-exists-p (and resolved-repo-root
                                 resolved-target-ref
                                 (%git-ref-exists-p resolved-repo-root
                                                    resolved-target-ref)))
           (preflight-target-ref (or (and target-exists-p resolved-target-ref)
                                     resolved-base-ref
                                     resolved-target-ref)))
      (append inspection
              (list :target-ref resolved-target-ref
                    :target-exists-p target-exists-p
                    :preflight-target-ref preflight-target-ref
                    :seed-ref resolved-base-ref
                    :workflow-branch workflow-target
                    :workflow-target-p (and workflow-target
                                            resolved-target-ref
                                            (string= workflow-target
                                                     resolved-target-ref)))))))

(defun preflight-local-worktree-merge (&key runtime
                                            repo-root
                                            worktree
                                            worktree-id
                                            worktree-path
                                            worktree-branch
                                            target-ref)
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
                        :base-ref target-ref))
           (resolved-target-ref (or (%normalize-worktree-string target-ref)
                                    (getf inspection :base-ref)))
           (resolved-path (getf inspection :path)))
      (cond
        ((not (getf inspection :live-p))
         (%local-worktree-preflight-result inspection
                                           :missing
                                           resolved-target-ref))
        ((getf inspection :dirty-p)
         (%local-worktree-preflight-result inspection
                                           :dirty
                                           resolved-target-ref))
        ((null resolved-target-ref)
         (%local-worktree-preflight-result
          inspection
          :error
          nil
          :error-message "Unable to resolve merge target ref."))
        (t
         (multiple-value-bind (preflight-data error-message)
             (%compute-local-worktree-preflight-data
              resolved-repo-root
              resolved-path
              resolved-target-ref)
           (if error-message
               (%local-worktree-preflight-result
                inspection
                :error
                resolved-target-ref
                :error-message error-message)
               (%local-worktree-preflight-result
                inspection
                (if (getf preflight-data :conflicts)
                    :conflict
                    :clean)
                resolved-target-ref
                :merge-base (getf preflight-data :merge-base)
                :worktree-files (getf preflight-data :worktree-files)
                :target-files (getf preflight-data :target-files)
                :conflicts (getf preflight-data :conflicts)
                :conflict-kind (getf preflight-data :conflict-kind)))))))))

(defun %local-worktree-merge-result (merge-target merge-status
                                     &key preflight
                                          created-target-p
                                          deleted-worktree-p
                                          handoff
                                          error-message
                                          reason)
  (append merge-target
          (list :merge-status merge-status
                :preflight (%copy-worktree-data preflight)
                :created-target-p created-target-p
                :deleted-worktree-p deleted-worktree-p
                :handoff-id (and handoff (getf handoff :handoff-id))
                :negotiation-room-id (and handoff
                                          (getf handoff :negotiation-room-id))
                :artifact-id (and handoff (getf handoff :artifact-id))
	                :reason reason
	                :error-message error-message)))

(defun %worktree-git-error-message (args stderr)
  (or (%normalize-worktree-string stderr)
      (format nil "Git command failed: git ~{~A~^ ~}" args)))

(defun %ensure-local-worktree-merge-target! (merge-target preflight
                                                          repo-root
                                                          target-ref)
  (if (getf merge-target :target-exists-p)
      (values nil nil)
      (let ((seed-ref (%normalize-worktree-string
                       (getf merge-target :seed-ref))))
        (if (null seed-ref)
            (values nil
                    (%local-worktree-merge-result
                     merge-target
                     :blocked
                     :preflight preflight
                     :reason :missing-seed-ref))
            (multiple-value-bind (_stdout stderr status)
                (%run-worktree-git repo-root
                                   (list "branch" target-ref seed-ref))
              (declare (ignore _stdout))
              (if (zerop status)
                  (values t nil)
                  (values nil
                          (%local-worktree-merge-result
                           merge-target
                           :error
                           :preflight preflight
                           :error-message
                           (%worktree-git-error-message
                            (list "branch" target-ref seed-ref)
                            stderr)
                           :reason :create-target-failed))))))))

(defun %checkout-local-worktree-merge-target! (merge-target preflight
                                                               repo-root
                                                               head-state
                                                               target-ref
                                                               created-target-p)
  (if (and (not (getf head-state :detached-p))
           (string= (getf head-state :ref) target-ref))
      (values nil nil)
      (multiple-value-bind (_stdout stderr status)
          (%run-worktree-git repo-root
                             (list "checkout" target-ref))
        (declare (ignore _stdout))
        (if (zerop status)
            (values t nil)
            (values nil
                    (%local-worktree-merge-result
                     merge-target
                     :error
                     :preflight preflight
                     :created-target-p created-target-p
                     :error-message
                     (%worktree-git-error-message
                      (list "checkout" target-ref)
                      stderr)
                     :reason :checkout-target-failed))))))

(defun %merge-local-worktree-branch-into-target! (merge-target preflight
                                                                 repo-root
                                                                 worktree-branch
                                                                 created-target-p)
  (let ((args (list "merge" "--no-edit" worktree-branch)))
    (multiple-value-bind (_stdout stderr status)
        (%run-worktree-git repo-root args)
      (declare (ignore _stdout))
      (when (not (zerop status))
        (ignore-errors
          (%run-worktree-git repo-root '("merge" "--abort")))
        (%local-worktree-merge-result
         merge-target
         :error
         :preflight preflight
         :created-target-p created-target-p
         :error-message (%worktree-git-error-message args stderr)
         :reason :merge-command-failed)))))

(defun %maybe-delete-local-worktree-after-merge (runtime
                                                 merge-target
                                                 repo-root
                                                 worktree
                                                 worktree-id
                                                 worktree-path
                                                 worktree-branch)
  (eq t (kill-local-worktree
         (or runtime
             (make-worktree-runtime :repo-root repo-root))
         (or worktree-id
             (getf merge-target :id))
         :force t
         :worktree worktree
         :worktree-path worktree-path
         :worktree-branch worktree-branch)))

(defun %perform-clean-local-worktree-merge (&key runtime
                                                 merge-target
                                                 preflight
                                                 repo-root
                                                 target-ref
                                                 worktree
                                                 worktree-id
                                                 worktree-path
                                                 worktree-branch
                                                 delete-worktree-p)
  (let* ((head-state (%git-current-head-state repo-root))
         (restore-head-p nil)
         (created-target-p nil)
         (deleted-worktree-p nil))
    (unwind-protect
         (progn
           (multiple-value-bind (created-target-p* early-result)
               (%ensure-local-worktree-merge-target! merge-target
                                                     preflight
                                                     repo-root
                                                     target-ref)
             (when early-result
               (return-from %perform-clean-local-worktree-merge early-result))
             (setf created-target-p created-target-p*))
           (multiple-value-bind (restore-head-p* early-result)
               (%checkout-local-worktree-merge-target! merge-target
                                                       preflight
                                                       repo-root
                                                       head-state
                                                       target-ref
                                                       created-target-p)
             (when early-result
               (return-from %perform-clean-local-worktree-merge early-result))
             (setf restore-head-p restore-head-p*))
           (let ((merge-error
                   (%merge-local-worktree-branch-into-target! merge-target
                                                              preflight
                                                              repo-root
                                                              (%normalize-worktree-string
                                                               (getf merge-target :branch))
                                                              created-target-p)))
             (when merge-error
               (return-from %perform-clean-local-worktree-merge merge-error)))
           (when delete-worktree-p
             (setf deleted-worktree-p
                   (%maybe-delete-local-worktree-after-merge runtime
                                                             merge-target
                                                             repo-root
                                                             worktree
                                                             worktree-id
                                                             worktree-path
                                                             worktree-branch)))
           (%local-worktree-merge-result
            merge-target
            :merged
            :preflight preflight
            :created-target-p created-target-p
            :deleted-worktree-p deleted-worktree-p))
      (when restore-head-p
        (ignore-errors
          (%restore-git-head-state repo-root head-state))))))

(defun merge-local-worktree (&key runtime
                                  repo-root
                                  worktree
                                  worktree-id
                                  worktree-path
                                  worktree-branch
                                  target-ref
                                  (delete-worktree-p t)
                                  agent-id
                                  backend
                                  task
                                  result)
  (let* ((merge-target (resolve-worktree-merge-target
                        :runtime runtime
                        :repo-root repo-root
                        :worktree worktree
                        :worktree-id worktree-id
                        :worktree-path worktree-path
                        :worktree-branch worktree-branch
                        :target-ref target-ref))
         (resolved-repo-root (and (getf merge-target :repo-root)
                                  (pathname (getf merge-target :repo-root))))
         (resolved-target-ref (%normalize-worktree-string
                               (getf merge-target :target-ref)))
         (preflight-target-ref (%normalize-worktree-string
                                (getf merge-target :preflight-target-ref)))
         (resolved-worktree-branch (%normalize-worktree-string
                                    (getf merge-target :branch)))
         (status-runtime (or runtime
                             (%runtime-for-worktree-source
                              :repo-root resolved-repo-root
                              :worktree-path (or worktree-path
                                                 (getf merge-target :path)))))
         (ignored-status-path-prefixes
           (%managed-worktree-status-ignored-paths status-runtime
                                                   resolved-repo-root)))
    (cond
      ((null resolved-target-ref)
       (%local-worktree-merge-result merge-target
                                     :blocked
                                     :reason :missing-target))
	      ((or (null preflight-target-ref)
	           (null resolved-repo-root))
	       (%local-worktree-merge-result merge-target
	                                     :blocked
	                                     :reason :missing-preflight-target))
	      ((null resolved-worktree-branch)
	       (%local-worktree-merge-result merge-target
	                                     :blocked
	                                     :reason :missing-worktree-branch))
	      ((and resolved-worktree-branch
	            (string= resolved-worktree-branch resolved-target-ref))
	       (%local-worktree-merge-result merge-target
	                                     :skipped
	                                     :reason :self-target))
      (t
       (let ((preflight (preflight-local-worktree-merge
                         :runtime runtime
                         :repo-root resolved-repo-root
                         :worktree worktree
                         :worktree-id worktree-id
                         :worktree-path worktree-path
                         :worktree-branch worktree-branch
                         :target-ref preflight-target-ref)))
         (case (getf preflight :status)
           (:clean
            (multiple-value-bind (clean-p clean-error)
                (%git-clean-working-tree-p
                 resolved-repo-root
                 :ignored-path-prefixes ignored-status-path-prefixes)
              (cond
                (clean-error
                 (%local-worktree-merge-result merge-target
                                               :error
                                               :preflight preflight
                                               :error-message clean-error
                                               :reason :target-status-error))
	                ((not clean-p)
	                 (%local-worktree-merge-result merge-target
	                                               :blocked
	                                               :preflight preflight
	                                               :reason :target-dirty))
	                (t
	                 (%perform-clean-local-worktree-merge
	                  :runtime runtime
	                  :merge-target merge-target
	                  :preflight preflight
	                  :repo-root resolved-repo-root
	                  :target-ref resolved-target-ref
	                  :worktree worktree
	                  :worktree-id worktree-id
	                  :worktree-path worktree-path
	                  :worktree-branch worktree-branch
	                  :delete-worktree-p delete-worktree-p)))))
	           (:conflict
	            (handler-case
	                (let ((handoff (create-worktree-conflict-handoff
                                :worktree (or worktree
                                              (make-worktree-metadata
                                               :id (getf merge-target :id)
                                               :branch resolved-worktree-branch
                                               :path (getf merge-target :path)))
                                :target-ref resolved-target-ref
                                :preflight preflight
                                :agent-id agent-id
                                :backend backend
                                :task task
                                :result result)))
                  (%local-worktree-merge-result
                   merge-target
                   :conflict-handoff
                   :preflight preflight
                   :handoff handoff
                   :reason (or (getf preflight :conflict-kind)
                               :conflict)))
              (error (condition)
                (%local-worktree-merge-result
                 merge-target
                 :error
                 :preflight preflight
                 :error-message (princ-to-string condition)
                 :reason :handoff-creation-failed))))
           (otherwise
           (%local-worktree-merge-result
             merge-target
             :blocked
             :preflight preflight
             :reason (getf preflight :status)))))))))

(defun %ensure-worktree-runtime (runtime)
  (check-type runtime worktree-runtime)
  runtime)

(defun spawn-worktree (runtime worktree-id branch &key
                              base-ref
                              worktree
                              workflow-id
                              node-id
                              worktree-path)
  (%ensure-worktree-runtime runtime)
  (if (worktree-runtime-local-p runtime)
      (spawn-local-worktree runtime
                            worktree-id
                            branch
                            :base-ref base-ref
                            :worktree worktree
                            :workflow-id workflow-id
                            :node-id node-id
                            :worktree-path worktree-path)
      (let* ((metadata (resolve-worktree-metadata :runtime runtime
                                                  :worktree worktree
                                                  :worktree-id worktree-id
                                                  :worktree-branch branch
                                                  :worktree-path worktree-path
                                                  :workflow-id workflow-id
                                                  :node-id node-id))
             (resolved-id (and metadata (worktree-metadata-id metadata)))
             (resolved-branch (and metadata (worktree-metadata-branch metadata)))
             (resolved-path (and metadata (worktree-metadata-path metadata))))
        (unless resolved-id
          (error "Unable to resolve a remote worktree id from ~S."
                 (list :worktree worktree
                       :worktree-id worktree-id
                       :worktree-branch branch
                       :workflow-id workflow-id
                       :node-id node-id)))
        (sw4rm-sdk:spawn-worktree (worktree-runtime-coordinator runtime)
                                  resolved-id
                                  resolved-path
                                  resolved-branch
                                  :base-ref (or base-ref "HEAD")))))

(defun collect-worktree (runtime worktree-id &key worktree worktree-path worktree-branch)
  (%ensure-worktree-runtime runtime)
  (if (worktree-runtime-local-p runtime)
      (collect-local-worktree runtime
                              worktree-id
                              :worktree worktree
                              :worktree-path worktree-path
                              :worktree-branch worktree-branch)
      (let* ((metadata (resolve-worktree-metadata :runtime runtime
                                                  :worktree worktree
                                                  :worktree-id worktree-id
                                                  :worktree-path worktree-path
                                                  :worktree-branch worktree-branch))
             (resolved-id (or (and metadata (worktree-metadata-id metadata))
                              (%normalize-worktree-string worktree-id)))
             (collected (sw4rm-sdk:collect-worktree
                         (worktree-runtime-coordinator runtime)
                         resolved-id))
             (record (getf collected :record))
             (live (getf collected :live))
             (status (inspect-worktree runtime
                                       resolved-id
                                       :worktree metadata
                                       :worktree-path worktree-path
                                       :worktree-branch worktree-branch)))
        (list :record (or record
                          (and metadata
                               (list :worktree-id (worktree-metadata-id metadata)
                                     :path (worktree-metadata-path metadata)
                                     :branch (worktree-metadata-branch metadata)
                                     :state :remote)))
              :live live
              :status status))))

(defun inspect-worktree (runtime worktree-id &key
                                worktree
                                worktree-path
                                worktree-branch
                                base-ref)
  (%ensure-worktree-runtime runtime)
  (if (worktree-runtime-local-p runtime)
      (inspect-local-worktree :runtime runtime
                              :worktree worktree
                              :worktree-id worktree-id
                              :worktree-path worktree-path
                              :worktree-branch worktree-branch
                              :base-ref base-ref)
      (let* ((metadata (resolve-worktree-metadata :runtime runtime
                                                  :worktree worktree
                                                  :worktree-id worktree-id
                                                  :worktree-path worktree-path
                                                  :worktree-branch worktree-branch))
             (resolved-id (or (and metadata (worktree-metadata-id metadata))
                              (%normalize-worktree-string worktree-id)))
             (inspection (sw4rm-sdk:inspect-worktree
                          (worktree-runtime-coordinator runtime)
                          resolved-id
                          :worktree-path (and metadata
                                              (worktree-metadata-path metadata))
                          :branch (and metadata
                                       (worktree-metadata-branch metadata)))))
        (append (list :id (or (getf inspection :id)
                              (and metadata (worktree-metadata-id metadata)))
                      :path (or (getf inspection :path)
                                (and metadata (worktree-metadata-path metadata)))
                      :branch (or (getf inspection :branch)
                                  (and metadata (worktree-metadata-branch metadata)))
                      :repo-root (and (worktree-runtime-repo-root runtime)
                                      (%worktree-path-namestring
                                       (worktree-runtime-repo-root runtime)))
                      :backend :remote)
                inspection))))

(defun merge-worktree (&key runtime
                            repo-root
                            worktree
                            worktree-id
                            worktree-path
                            worktree-branch
                            target-ref
                            (delete-worktree-p t)
                            agent-id
                            backend
                            task
                            result)
  (%ensure-worktree-runtime runtime)
  (if (worktree-runtime-local-p runtime)
      (merge-local-worktree :runtime runtime
                            :repo-root repo-root
                            :worktree worktree
                            :worktree-id worktree-id
                            :worktree-path worktree-path
                            :worktree-branch worktree-branch
                            :target-ref target-ref
                            :delete-worktree-p delete-worktree-p
                            :agent-id agent-id
                            :backend backend
                            :task task
                            :result result)
      (sw4rm-sdk:merge-worktree
       (worktree-runtime-coordinator runtime)
       (list :repo-root (and repo-root (%worktree-path-namestring repo-root))
             :worktree (worktree-metadata-plist
                        (resolve-worktree-metadata :runtime runtime
                                                   :worktree worktree
                                                   :worktree-id worktree-id
                                                   :worktree-path worktree-path
                                                   :worktree-branch worktree-branch))
             :worktree-id (%normalize-worktree-string worktree-id)
             :worktree-path (%normalize-worktree-path-string worktree-path)
             :worktree-branch (%normalize-worktree-string worktree-branch)
             :target-ref (%normalize-worktree-string target-ref)
             :delete-worktree-p delete-worktree-p
             :agent-id (%normalize-worktree-string agent-id)
             :backend (or backend :remote)
             :task task
             :result result))))

(defun kill-worktree (runtime worktree-id &key force worktree worktree-path worktree-branch)
  (%ensure-worktree-runtime runtime)
  (if (worktree-runtime-local-p runtime)
      (kill-local-worktree runtime
                           worktree-id
                           :force force
                           :worktree worktree
                           :worktree-path worktree-path
                           :worktree-branch worktree-branch)
      (let* ((metadata (resolve-worktree-metadata :runtime runtime
                                                  :worktree worktree
                                                  :worktree-id worktree-id
                                                  :worktree-path worktree-path
                                                  :worktree-branch worktree-branch))
             (resolved-id (or (and metadata (worktree-metadata-id metadata))
                              (%normalize-worktree-string worktree-id))))
        (sw4rm-sdk:kill-worktree (worktree-runtime-coordinator runtime)
                                 resolved-id
                                 :force force))))

(defun spawn-local-worktree (runtime worktree-id branch &key
                                                       base-ref
                                                       worktree
                                                       workflow-id
                                                       node-id
                                                       worktree-path)
  (check-type runtime worktree-runtime)
  (ignore-errors
    (cleanup-abandoned-local-worktrees :runtime runtime))
  (let* ((metadata (resolve-worktree-metadata :runtime runtime
                                              :worktree worktree
                                              :worktree-id worktree-id
                                              :worktree-branch branch
                                              :worktree-path worktree-path
                                              :workflow-id workflow-id
                                              :node-id node-id))
         (resolved-id (and metadata (worktree-metadata-id metadata)))
         (resolved-branch (and metadata (worktree-metadata-branch metadata)))
         (resolved-path (and metadata (worktree-metadata-path metadata))))
    (unless resolved-id
      (error "Unable to resolve a local worktree id from ~S."
             (list :worktree worktree
                   :worktree-id worktree-id
                   :worktree-branch branch
                   :workflow-id workflow-id
                   :node-id node-id)))
    (%delete-worktree-marker
     (%worktree-abandoned-marker-path runtime resolved-id))
    (unless resolved-branch
      (error "Unable to resolve a local worktree branch from ~S."
             (list :worktree worktree
                   :worktree-id worktree-id
                   :worktree-branch branch
                   :workflow-id workflow-id
                   :node-id node-id)))
    (sw4rm-sdk:spawn-worktree
     (worktree-runtime-coordinator runtime)
     resolved-id
     resolved-path
     resolved-branch
     :base-ref (or base-ref "HEAD"))))

(defun collect-local-worktree (runtime worktree-id &key
                                               worktree
                                               worktree-path
                                               worktree-branch)
  (check-type runtime worktree-runtime)
  (ignore-errors
    (cleanup-abandoned-local-worktrees :runtime runtime))
  (multiple-value-bind (_ metadata repo-root)
      (%resolve-worktree-runtime-and-metadata
       :runtime runtime
       :worktree worktree
       :worktree-id worktree-id
       :worktree-path worktree-path
       :worktree-branch worktree-branch)
    (declare (ignore _))
    (let* ((collected (ignore-errors
                        (sw4rm-sdk:collect-worktree
                         (worktree-runtime-coordinator runtime)
                         (princ-to-string
                          (or (and metadata (worktree-metadata-id metadata))
                              worktree-id)))))
           (record (or (and collected (getf collected :record))
                       (%reconstruct-local-worktree-record
                        metadata
                        nil)))
           (live (or (and collected (getf collected :live))
                     (%find-live-local-worktree repo-root metadata))))
      (when (and live (or (null record)
                          (eq (getf record :state) :missing)))
        (setf record (%reconstruct-local-worktree-record metadata live)))
      (list :record record
            :live live
            :status (inspect-local-worktree
                     :runtime runtime
                     :repo-root repo-root
                     :worktree metadata)))))

(defun kill-local-worktree (runtime worktree-id &key
                                           force
                                           worktree
                                           worktree-path
                                           worktree-branch)
  (check-type runtime worktree-runtime)
  (multiple-value-bind (_ metadata repo-root)
      (%resolve-worktree-runtime-and-metadata
       :runtime runtime
       :worktree worktree
       :worktree-id worktree-id
       :worktree-path worktree-path
       :worktree-branch worktree-branch)
    (declare (ignore _))
    (let* ((resolved-id (or (and metadata (worktree-metadata-id metadata))
                            (and worktree-id
                                 (%normalize-worktree-string worktree-id))))
           (resolved-path (or (and metadata (worktree-metadata-path metadata))
                              (and resolved-id
                                   (%worktree-path-namestring
                                    (worktree-runtime-path runtime
                                                           resolved-id))))))
      (or (ignore-errors
            (sw4rm-sdk:kill-worktree
             (worktree-runtime-coordinator runtime)
             (princ-to-string resolved-id)
             :force force))
          (let* ((resolved-pathname (and resolved-path (pathname resolved-path)))
                 (path-missing-p (and resolved-pathname
                                      (not (probe-file resolved-pathname))))
                 (live (and (not path-missing-p)
                            repo-root
                            metadata
                            (%find-live-local-worktree repo-root metadata))))
            (cond
              (path-missing-p
               (when repo-root
                 (sw4rm-sdk:git-worktree-prune repo-root))
               (when resolved-id
                 (%delete-worktree-marker
                  (%worktree-abandoned-marker-path runtime resolved-id)))
               t)
              ((or live resolved-path)
               (sw4rm-sdk:git-worktree-remove
                repo-root
                (or (and live (%worktree-path-namestring (getf live :path)))
                    resolved-path)
                :force force)
               (sw4rm-sdk:git-worktree-prune repo-root)
               (when resolved-id
                 (%delete-worktree-marker
                  (%worktree-abandoned-marker-path runtime resolved-id)))
               t)))))))
