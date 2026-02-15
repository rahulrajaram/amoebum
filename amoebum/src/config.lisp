(in-package :amoebum)

(defparameter *default-config-values*
  '(:model "moonshot-v1-128k"
    :permission-mode :supervised
    :memory-backend :auto
    :plan-mode nil))

(defparameter *known-permission-modes*
  '(:supervised :auto-edit :full-auto :yolo :no-confirm))

(defparameter *known-memory-backends*
  '(:auto :file :haake-cli :haake-mcp))

(defparameter *current-config* nil)

(defparameter *config-change-emitter* nil)

(defvar *config-loader-values* nil)

(defstruct (config
            (:constructor make-config
                (&key model permission-mode memory-backend project-root
                 (values (make-hash-table :test 'eq)))))
  model
  permission-mode
  memory-backend
  project-root
  values)

(define-condition configuration-error (error)
  ((key :initarg :key
        :reader configuration-error-key)
   (value :initarg :value
          :reader configuration-error-value)
   (reason :initarg :reason
           :reader configuration-error-reason))
  (:report (lambda (condition stream)
             (format stream "Invalid configuration ~S=~S (~A)."
                     (configuration-error-key condition)
                     (configuration-error-value condition)
                     (configuration-error-reason condition)))))

(defun %pathname-or-string-p (value)
  (or (pathnamep value)
      (stringp value)))

(defun %default-value (key project-root)
  (if (eq key :project-root)
      project-root
      (getf *default-config-values* key)))

(defun %valid-config-value-p (key value)
  (case key
    (:model (stringp value))
    (:permission-mode (member value *known-permission-modes* :test #'eq))
    (:memory-backend (member value *known-memory-backends* :test #'eq))
    (:plan-mode (or (eq value t) (eq value nil)))
    (:project-root (%pathname-or-string-p value))
    (t t)))

(defun %signal-invalid-value (key value reason project-root)
  (restart-case
      (error 'configuration-error
             :key key
             :value value
             :reason reason)
    (use-default ()
      :report (lambda (stream)
                (format stream "Use default value for ~S." key))
      (%default-value key project-root))))

(defun %normalize-project-root (project-root)
  (let ((resolved
          (cond
            ((pathnamep project-root) project-root)
            ((stringp project-root) (pathname project-root))
            (t *default-pathname-defaults*))))
    (uiop:ensure-directory-pathname (or (ignore-errors (truename resolved))
                                        resolved))))

(defun %merge-value (cfg key value source project-root)
  (declare (ignore source))
  (let ((validated
          (if (%valid-config-value-p key value)
              value
              (%signal-invalid-value key value "failed validation" project-root))))
    (setf (gethash key (config-values cfg)) validated)
    (case key
      (:model (setf (config-model cfg) validated))
      (:permission-mode (setf (config-permission-mode cfg) validated))
      (:memory-backend (setf (config-memory-backend cfg) validated))
      (:project-root (setf (config-project-root cfg)
                           (%normalize-project-root validated)))))
  cfg)

(defun %base-config (&key project-root)
  (let* ((root (%normalize-project-root project-root))
         (cfg (make-config :model (getf *default-config-values* :model)
                           :permission-mode (getf *default-config-values* :permission-mode)
                           :memory-backend (getf *default-config-values* :memory-backend)
                           :project-root root)))
    (setf (gethash :model (config-values cfg)) (config-model cfg)
          (gethash :permission-mode (config-values cfg)) (config-permission-mode cfg)
          (gethash :memory-backend (config-values cfg)) (config-memory-backend cfg)
          (gethash :plan-mode (config-values cfg)) (getf *default-config-values* :plan-mode)
          (gethash :project-root (config-values cfg)) (config-project-root cfg))
    cfg))

(defun configure (&rest entries)
  "Used by config files to write key/value pairs for the active load context."
  (unless *config-loader-values*
    (error "CONFIGURE can only be used while loading configuration files."))
  (unless (evenp (length entries))
    (error "CONFIGURE expects an even number of key/value arguments."))
  (loop for (key value) on entries by #'cddr do
        (setf (gethash key *config-loader-values*) value))
  *config-loader-values*)

(defun %load-layer (path)
  (let ((values (make-hash-table :test 'eq)))
    (when (and path (probe-file path))
      (let ((*package* (find-package :amoebum))
            (*config-loader-values* values))
        (load (probe-file path) :verbose nil :print nil)))
    values))

(defun %hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun %global-config-path ()
  (merge-pathnames #P".amoebum/config.lisp" (user-homedir-pathname)))

(defun %project-config-path (project-root)
  (merge-pathnames #P".amoebum/config.lisp" (%normalize-project-root project-root)))

(defun load-config (&key project-root global-config-path project-config-path)
  (let* ((root (%normalize-project-root project-root))
         (cfg (%base-config :project-root root))
         (global-values (%load-layer (or global-config-path (%global-config-path))))
         (project-values (%load-layer (or project-config-path (%project-config-path root)))))
    (dolist (key (%hash-keys global-values))
      (%merge-value cfg key (gethash key global-values) :global root))
    (dolist (key (%hash-keys project-values))
      (%merge-value cfg key (gethash key project-values) :project root))
    cfg))

(defun reload-config (&key project-root global-config-path project-config-path)
  (setf *current-config*
        (load-config :project-root project-root
                     :global-config-path global-config-path
                     :project-config-path project-config-path)))

(defun current-config ()
  (or *current-config*
      (setf *current-config* (load-config))))

(defun config-value (key &optional (cfg (current-config)))
  (gethash key (config-values cfg)))

(defun emit-config-changed (key old-value new-value)
  (let ((event (make-config-changed-event :key key
                                          :old-value old-value
                                          :new-value new-value)))
    (publish (current-event-bus) event)
    (if (functionp *config-change-emitter*)
        (funcall *config-change-emitter* key old-value new-value)
        event)))

(defun setconfig (key value)
  (let* ((cfg (current-config))
         (old-value (config-value key cfg))
         (root (config-project-root cfg)))
    (%merge-value cfg key value :runtime root)
    (emit-config-changed key old-value (config-value key cfg))
    (config-value key cfg)))
