(in-package :amoebum)

(defvar *image-directory-override* nil)
(defvar *image-max-count* 5
  "Maximum number of saved images to keep.")
(defvar *image-pre-save-hooks* '()
  "List of functions called before saving an image.")
(defvar *image-post-restore-hooks* '()
  "List of functions called after restoring an image.")
(defvar *image-fd-cleanup-hooks* '()
  "Functions called to close file descriptors before image save.")
(defvar *image-network-drain-hooks* '()
  "Functions called to drain network resources before image save.")
(defvar *image-agent-checkpoint-hooks* '()
  "Functions called to serialize agent state before image save.")
(defvar *image-terminal-snapshot-hooks* '()
  "Functions called to enrich terminal state snapshots before image save.")
(defvar *image-terminal-reopen-hooks* '()
  "Functions called to re-open terminal resources after restore.")
(defvar *image-mcp-reconnect-hooks* '()
  "Functions called to re-establish MCP connections after restore.")
(defvar *image-api-reauth-hooks* '()
  "Functions called to re-authenticate API clients after restore.")
(defvar *image-tracked-streams* '()
  "List of streams tracked for pre-save file-descriptor cleanup.")
(defvar *image-last-terminal-state* nil
  "Most recent terminal snapshot captured during pre-save cleanup.")
(defvar *image-last-pre-save-report* nil
  "Most recent pre-save cleanup report.")
(defvar *image-last-post-restore-report* nil
  "Most recent post-restore init report.")

(defun image-directory (&key project-root config)
  "Return the directory for saved images."
  (or (and *image-directory-override*
           (uiop:ensure-directory-pathname *image-directory-override*))
      (merge-pathnames #P".amoebum/images/"
                       (%checkpoint-project-root :project-root project-root
                                                 :config config))))

(defun %image-path (name &key project-root config)
  "Build image file path."
  (merge-pathnames (pathname (format nil "~A.core" name))
                   (image-directory :project-root project-root :config config)))

(defun %image-files (&key project-root config)
  "List existing image files, sorted newest first."
  (let* ((dir (image-directory :project-root project-root :config config))
         (pattern (merge-pathnames #P"*.core" dir)))
    (sort (copy-list (directory pattern))
          #'>
          :key (lambda (path)
                 (or (ignore-errors (file-write-date path)) 0)))))

(defun rotate-images (&key project-root config (max-count *image-max-count*))
  "Delete old images, keeping at most MAX-COUNT."
  (let* ((images (%image-files :project-root project-root :config config))
         (excess (nthcdr max-count images))
         (deleted 0))
    (dolist (old excess)
      (handler-case
          (progn (delete-file old) (incf deleted))
        (error () nil)))
    deleted))

(defun list-saved-images (&key project-root config)
  "Return alist of (name . path) for saved images."
  (mapcar (lambda (path)
            (cons (pathname-name path) path))
          (%image-files :project-root project-root :config config)))

(defun register-image-tracked-stream (stream)
  "Track STREAM so image pre-save cleanup can close it."
  (when (streamp stream)
    (pushnew stream *image-tracked-streams* :test #'eq))
  stream)

(defun %close-image-tracked-streams ()
  "Close tracked streams and drop closed entries from the registry."
  (let ((closed-count 0)
        (remaining '()))
    (dolist (stream *image-tracked-streams*)
      (cond
        ((not (streamp stream)) nil)
        ((not (open-stream-p stream)) nil)
        (t
         (handler-case
             (progn
               (close stream)
               (incf closed-count))
           (error ()
             (push stream remaining))))))
    (setf *image-tracked-streams* (nreverse remaining))
    closed-count))

(defun register-image-fd-cleanup-hook (fn)
  (pushnew fn *image-fd-cleanup-hooks* :test #'eq))

(defun register-image-network-drain-hook (fn)
  (pushnew fn *image-network-drain-hooks* :test #'eq))

(defun register-image-agent-checkpoint-hook (fn)
  (pushnew fn *image-agent-checkpoint-hooks* :test #'eq))

(defun register-image-terminal-snapshot-hook (fn)
  (pushnew fn *image-terminal-snapshot-hooks* :test #'eq))

(defun register-image-terminal-reopen-hook (fn)
  (pushnew fn *image-terminal-reopen-hooks* :test #'eq))

(defun register-image-mcp-reconnect-hook (fn)
  (pushnew fn *image-mcp-reconnect-hooks* :test #'eq))

(defun register-image-api-reauth-hook (fn)
  (pushnew fn *image-api-reauth-hooks* :test #'eq))

(defun %invoke-image-hook (hook label &key argument argument-supplied-p)
  (handler-case
      (if argument-supplied-p
          (handler-case
              (funcall hook argument)
            (program-error ()
              (funcall hook)))
          (funcall hook))
    (error (condition)
      (format *error-output* "~A hook error: ~A~%" label condition)
      :image-hook-error)))

(defun %run-image-hooks (hooks label &key argument argument-supplied-p collect-results-p)
  "Run HOOKS, returning successful hook count and optional results list."
  (let ((success-count 0)
        (results '()))
    (dolist (hook hooks)
      (let ((result (%invoke-image-hook hook
                                        label
                                        :argument argument
                                        :argument-supplied-p argument-supplied-p)))
        (unless (eq result :image-hook-error)
          (incf success-count)
          (when collect-results-p
            (push result results)))))
    (values success-count (nreverse results))))

(defun %hook-result->count (value)
  (if (and (integerp value) (>= value 0))
      value
      1))

(defun %run-image-counting-hooks (hooks label &key argument argument-supplied-p)
  (multiple-value-bind (success-count results)
      (%run-image-hooks hooks
                        label
                        :argument argument
                        :argument-supplied-p argument-supplied-p
                        :collect-results-p t)
    (if (null results)
        success-count
        (reduce #'+ results :initial-value 0 :key #'%hook-result->count))))

(defun %default-terminal-state-snapshot ()
  (list :captured-at (get-universal-time)
        :term (uiop:getenv "TERM")
        :columns (uiop:getenv "COLUMNS")
        :lines (uiop:getenv "LINES")
        :cwd (ignore-errors (uiop:getcwd))))

(defun %capture-terminal-state ()
  (let* ((base (%default-terminal-state-snapshot))
         (hook-results
           (nth-value 1
                      (%run-image-hooks *image-terminal-snapshot-hooks*
                                        "terminal-snapshot"
                                        :argument base
                                        :argument-supplied-p t
                                        :collect-results-p t))))
    (if hook-results
        (append base (list :hook-states hook-results))
        base)))

(defun %emit-system-restored-event (event-bus report)
  (publish (or event-bus (current-event-bus))
           (make-event :type "system:restored"
                       :source :amoebum
                       :severity :info
                       :payload report)))

(defun %image-pre-save-cleanup ()
  "Perform cleanup before saving an image."
  (let* ((fd-cleanup-count
           (%run-image-counting-hooks *image-fd-cleanup-hooks*
                                      "pre-save/fd-cleanup"))
         (network-drain-count
           (%run-image-counting-hooks *image-network-drain-hooks*
                                      "pre-save/network-drain"))
         (agent-checkpoint-count
           (%run-image-counting-hooks *image-agent-checkpoint-hooks*
                                      "pre-save/agent-checkpoint"))
         (terminal-state (%capture-terminal-state))
         (extension-hook-count
           (%run-image-counting-hooks *image-pre-save-hooks* "pre-save/extension"))
         (report (list :captured-at (get-universal-time)
                       :fd-cleanup-count fd-cleanup-count
                       :network-drain-count network-drain-count
                       :agent-checkpoint-count agent-checkpoint-count
                       :terminal-state terminal-state
                       :extension-hook-count extension-hook-count)))
    (setf *image-last-terminal-state* terminal-state
          *image-last-pre-save-report* report)
    report))

(defun %image-post-restore-init (&key event-bus terminal-state)
  "Reinitialize after restoring an image."
  #+sbcl
  (setf (sb-ext:bytes-consed-between-gcs) (* 64 1024 1024))
  (let* ((resolved-terminal-state (or terminal-state *image-last-terminal-state*))
         (terminal-reopen-count
           (%run-image-counting-hooks *image-terminal-reopen-hooks*
                                      "post-restore/terminal-reopen"
                                      :argument resolved-terminal-state
                                      :argument-supplied-p t))
         (mcp-reconnect-count
           (%run-image-counting-hooks *image-mcp-reconnect-hooks*
                                      "post-restore/mcp-reconnect"))
         (api-reauth-count
           (%run-image-counting-hooks *image-api-reauth-hooks*
                                      "post-restore/api-reauth"))
         (report (list :restored-at (get-universal-time)
                       :terminal-state resolved-terminal-state
                       :terminal-reopen-count terminal-reopen-count
                       :mcp-reconnect-count mcp-reconnect-count
                       :api-reauth-count api-reauth-count)))
    (%emit-system-restored-event event-bus report)
    (let ((extension-hook-count
            (%run-image-counting-hooks *image-post-restore-hooks*
                                       "post-restore/extension"
                                       :argument report
                                       :argument-supplied-p t)))
      (setf report (append report (list :extension-hook-count extension-hook-count)))
      (setf *image-last-post-restore-report* report)
      report)))

(defstruct (save-amoebum-image-request
            (:constructor make-save-amoebum-image-request
                (&key path project-root config name toplevel-fn
                      (rotate-p t) resolved-name resolved-path)))
  path
  project-root
  config
  name
  toplevel-fn
  (rotate-p t)
  resolved-name
  resolved-path)

(defun %resolve-save-amoebum-image-request (request)
  (let ((resolved-name (or (save-amoebum-image-request-name request)
                           (%checkpoint-id-from-time))))
    (setf (save-amoebum-image-request-resolved-name request) resolved-name
          (save-amoebum-image-request-resolved-path request)
          (or (save-amoebum-image-request-path request)
              (%image-path resolved-name
                           :project-root (save-amoebum-image-request-project-root request)
                           :config (save-amoebum-image-request-config request))))
    request))

(defun %prepare-save-amoebum-image (request)
  (ensure-directories-exist (save-amoebum-image-request-resolved-path request))
  (%image-pre-save-cleanup)
  request)

(defun %checkpoint-before-image-save (request)
  (handler-case
      (checkpoint-session :project-root (save-amoebum-image-request-project-root request)
                          :config (save-amoebum-image-request-config request)
                          :trigger :image-save
                          :auto-p nil)
    (error () nil))
  request)

(defun %rotate-images-before-save (request)
  (when (save-amoebum-image-request-rotate-p request)
    (rotate-images :project-root (save-amoebum-image-request-project-root request)
                   :config (save-amoebum-image-request-config request)))
  request)

(defun %default-restored-image-toplevel ()
  (lambda ()
    (%image-post-restore-init)
    #+sbcl
    (handler-case
        (handler-bind
            ((serious-condition
               (lambda (c)
                 (unless (typep c 'sb-sys:interactive-interrupt)
                   (ignore-errors
                     (let ((bt (with-output-to-string (s)
                                 (sb-debug:print-backtrace :stream s :count 40))))
                       (log-runtime-condition
                        c
                        :kind "restore-error"
                        :source :checkpoint
                        :message "Uncaught condition in restored Amoebum image."
                        :details (list :condition-type (type-of c)
                                       :argv (rest sb-ext:*posix-argv*))
                        :include-backtrace-p nil)
                       (with-open-file (f (merge-pathnames ".amoebum/runtime/full-backtrace.log"
                                                           (user-homedir-pathname))
                                          :direction :output :if-exists :supersede
                                          :if-does-not-exist :create)
                         (format f "Condition: ~A~%Type: ~A~%~%~A~%" c (type-of c) bt)
                         (finish-output f))))
                   (ignore-errors
                     (format *error-output* "Restore error (~A): ~A~%" (type-of c) c)
                     (format *error-output* "Crash log: ~A~%"
                             (namestring (crash-log-path)))
                     (write-line "Full backtrace: ~/.amoebum/runtime/full-backtrace.log"
                                 *error-output*))
                   (sb-ext:exit :code 1 :abort t)))))
          (progn
            (main)
            (sb-ext:exit :code 0 :abort t)))
      (sb-sys:interactive-interrupt ()
        (sb-ext:exit :code 0 :abort t)))
    #-sbcl
    (progn
      (main)
      (uiop:quit 0))))

(defun %save-amoebum-image-request (request)
  #+sbcl
  (progn
    (sb-ext:gc :full t)
    (sb-ext:save-lisp-and-die
     (save-amoebum-image-request-resolved-path request)
     :toplevel (or (save-amoebum-image-request-toplevel-fn request)
                   (%default-restored-image-toplevel))
     :executable t
     :purify t))
  #-sbcl
  (error "Image save/restore requires SBCL (sb-ext:save-lisp-and-die).")
  (save-amoebum-image-request-resolved-path request))

(defun save-amoebum-image (&key path project-root config
                                (name nil)
                                (toplevel-fn nil)
                                (rotate-p t))
  "Save the current Lisp image to PATH.
Pre-save cleanup runs before save; post-restore init runs on resume.
If ROTATE-P, old images are pruned."
  (let ((request (%resolve-save-amoebum-image-request
                  (make-save-amoebum-image-request
                   :path path
                   :project-root project-root
                   :config config
                   :name name
                   :toplevel-fn toplevel-fn
                   :rotate-p rotate-p))))
    (%prepare-save-amoebum-image request)
    (%checkpoint-before-image-save request)
    (%rotate-images-before-save request)
    (%save-amoebum-image-request request)))

(defun register-image-pre-save-hook (fn)
  "Register a function to call before saving an image."
  (pushnew fn *image-pre-save-hooks* :test #'eq))

(defun register-image-post-restore-hook (fn)
  "Register a function to call after restoring an image."
  (pushnew fn *image-post-restore-hooks* :test #'eq))

(eval-when (:load-toplevel :execute)
  (register-image-fd-cleanup-hook #'%close-image-tracked-streams))
