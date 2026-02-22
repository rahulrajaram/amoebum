(in-package :amoebum)

(defparameter *default-config-values*
  '(:model "moonshot-v1-128k"
    :provider-override nil
    :api-base-url nil
    :context-window-limit nil
    :permission-mode :supervised
    :approval-policy :on-request
    :sandbox-policy :strict
    :sandbox-mode :workspace-write
    :swarm-delegation-mode :local
    :memory-backend :auto
    :web-search-searxng-url nil
    :web-search-duckduckgo-url "https://duckduckgo.com/html/"
    :web-search-allow-domains nil
    :web-search-block-domains nil
    :web-search-user-agent "amoebum-web-search/0.1"
    :web-fetch-timeout-seconds 20
    :web-fetch-cache-ttl-seconds 900
    :web-fetch-max-markdown-bytes 10240
    :web-fetch-user-agent "amoebum-web-fetch/0.1"
    :haake-command "haake"
    :haake-project-id nil
    :haake-agent "amoebum"
    :haake-autodetect t
    :notifications-enabled t
    :notification-events '(:task-complete :error :approval-needed)
    :notification-sound-enabled t
    :notification-desktop-enabled t
    :notification-log-enabled t
    :notification-sound-player nil
    :notification-desktop-command nil
    :notification-log-path nil
    :notification-sound-task-complete nil
    :notification-sound-error nil
    :notification-sound-approval-needed nil
    :auto-checkpoint-idle-seconds 300
    :plan-mode nil))

(defparameter *known-permission-modes*
  '(:supervised :auto-edit :full-auto :yolo :no-confirm))

(defparameter *known-approval-policies*
  '(:untrusted :on-failure :on-request :never))

(defparameter *known-sandbox-policies*
  '(:strict :off))

(defparameter *known-sandbox-modes*
  '(:read-only :workspace-write :danger-full-access))

(defparameter *known-swarm-delegation-modes*
  '(:local :networked))

(defparameter *known-memory-backends*
  '(:auto :file :haake-cli :haake-mcp))

(defparameter *current-config* nil)

(defparameter *config-change-emitter* nil)

(defvar *config-loader-values* nil)

(defstruct (config
            (:constructor make-config
                (&key model permission-mode memory-backend project-root
                 (values (make-hash-table :test 'eq))
                 (sources (make-hash-table :test 'eq)))))
  model
  permission-mode
  memory-backend
  project-root
  values
  sources)

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

(defun %string-sequence-p (value)
  (or (null value)
      (stringp value)
      (and (listp value)
           (every #'stringp value))
      (and (vectorp value)
           (every #'stringp (coerce value 'list)))))

(defun %keyword-like-sequence-p (value)
  (labels ((keyword-like-p (entry)
             (or (keywordp entry)
                 (stringp entry)
                 (symbolp entry))))
    (or (null value)
        (keyword-like-p value)
        (and (listp value)
             (every #'keyword-like-p value))
        (and (vectorp value)
             (every #'keyword-like-p (coerce value 'list))))))

(defun %default-value (key project-root)
  (if (eq key :project-root)
      project-root
      (getf *default-config-values* key)))

(defun %default-config-keys ()
  (append
   (loop for (key _value) on *default-config-values* by #'cddr collect key)
   '(:project-root)))

(defun %trim-string (value)
  (if (stringp value)
      (string-trim '(#\Space #\Tab #\Newline #\Return) value)
      ""))

(defun %non-empty-string-p (value)
  (and (stringp value)
       (> (length (%trim-string value)) 0)))

(defun %valid-config-value-p (key value)
  (case key
    (:model (stringp value))
    (:provider-override
     (or (null value)
         (and (or (stringp value)
                  (symbolp value)
                  (keywordp value))
              (member (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                  (string value))
                                     )
                      '("anthropic-provider" "anthropic"
                        "openai-compatible-provider" "openai-compat" "openai"
                        "kimi-provider" "kimi")
                      :test #'string=))))
    (:api-base-url
     (or (null value)
         (and (stringp value)
              (> (length (string-trim '(#\Space #\Tab #\Newline #\Return) value))
                 0))))
    (:context-window-limit
     (or (null value)
         (and (integerp value)
              (> value 0))))
    (:permission-mode
     (member value *known-permission-modes* :test #'eq))
    (:approval-policy
     (member (%approval-policy-keyword value)
             *known-approval-policies*
             :test #'eq))
    (:sandbox-policy
     (member (%sandbox-policy-keyword value)
             *known-sandbox-policies*
             :test #'eq))
    (:sandbox-mode
     (member (%sandbox-mode-keyword value)
             *known-sandbox-modes*
             :test #'eq))
    (:swarm-delegation-mode
     (member (%swarm-delegation-mode-keyword value)
             *known-swarm-delegation-modes*
             :test #'eq))
    (:memory-backend
     (member value *known-memory-backends* :test #'eq))
    (:web-search-searxng-url
     (or (null value)
         (and (stringp value)
              (> (length (string-trim '(#\Space #\Tab #\Newline #\Return) value))
                 0))))
    (:web-search-duckduckgo-url
     (or (null value)
         (and (stringp value)
              (> (length (string-trim '(#\Space #\Tab #\Newline #\Return) value))
                 0))))
    (:web-search-allow-domains
     (%string-sequence-p value))
    (:web-search-block-domains
     (%string-sequence-p value))
    (:web-search-user-agent
     (or (null value)
         (and (stringp value)
              (> (length (string-trim '(#\Space #\Tab #\Newline #\Return) value))
                 0))))
    (:web-fetch-timeout-seconds
     (and (integerp value) (> value 0)))
    (:web-fetch-cache-ttl-seconds
     (and (integerp value) (> value 0)))
    (:web-fetch-max-markdown-bytes
     (and (integerp value) (> value 0)))
    (:web-fetch-user-agent
     (or (null value)
         (and (stringp value)
              (> (length (string-trim '(#\Space #\Tab #\Newline #\Return) value))
                 0))))
    (:haake-command
     (and (stringp value)
          (> (length (string-trim '(#\Space #\Tab #\Newline #\Return) value))
             0)))
    (:haake-project-id
     (or (null value)
         (and (stringp value)
              (> (length (string-trim '(#\Space #\Tab #\Newline #\Return) value))
                 0))))
    (:haake-agent
     (and (stringp value)
          (> (length (string-trim '(#\Space #\Tab #\Newline #\Return) value))
             0)))
    (:haake-autodetect (or (eq value t) (eq value nil)))
    (:notifications-enabled (or (eq value t) (eq value nil)))
    (:notification-events
     (%keyword-like-sequence-p value))
    (:notification-sound-enabled (or (eq value t) (eq value nil)))
    (:notification-desktop-enabled (or (eq value t) (eq value nil)))
    (:notification-log-enabled (or (eq value t) (eq value nil)))
    (:notification-sound-player
     (or (null value)
         (and (stringp value)
              (> (length (string-trim '(#\Space #\Tab #\Newline #\Return) value))
                 0))))
    (:notification-desktop-command
     (or (null value)
         (and (stringp value)
              (> (length (string-trim '(#\Space #\Tab #\Newline #\Return) value))
                 0))))
    (:notification-log-path
     (or (null value)
         (%pathname-or-string-p value)))
    (:notification-sound-task-complete
     (or (null value)
         (%pathname-or-string-p value)))
    (:notification-sound-error
     (or (null value)
         (%pathname-or-string-p value)))
    (:notification-sound-approval-needed
     (or (null value)
         (%pathname-or-string-p value)))
    (:auto-checkpoint-idle-seconds
     (and (integerp value)
          (>= value 0)))
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
  (let ((validated
          (if (%valid-config-value-p key value)
              value
              (%signal-invalid-value key value "failed validation" project-root)))
        (final-value nil))
    (setf final-value
          (cond
            ((eq key :project-root)
             (%normalize-project-root validated))
            ((eq key :sandbox-policy)
             (%sandbox-policy-keyword validated))
            ((eq key :approval-policy)
             (%approval-policy-keyword validated))
            ((eq key :sandbox-mode)
             (%sandbox-mode-keyword validated))
            ((eq key :swarm-delegation-mode)
             (%swarm-delegation-mode-keyword validated))
            (t
             validated)))
    (setf (gethash key (config-values cfg)) final-value
          (gethash key (config-sources cfg)) source)
    (case key
      (:model (setf (config-model cfg) final-value))
      (:permission-mode (setf (config-permission-mode cfg) final-value))
      (:memory-backend (setf (config-memory-backend cfg) final-value))
      (:project-root (setf (config-project-root cfg)
                           final-value)))
  cfg))

(defun %base-config (&key project-root)
  (let* ((root (%normalize-project-root project-root))
         (cfg (make-config :model (getf *default-config-values* :model)
                           :permission-mode (getf *default-config-values* :permission-mode)
                           :memory-backend (getf *default-config-values* :memory-backend)
                           :project-root root)))
    (dolist (key (%default-config-keys))
      (setf (gethash key (config-values cfg)) (%default-value key root)
            (gethash key (config-sources cfg)) :built-in))
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

(defun %path-namestring (path)
  (when path
    (namestring (or (ignore-errors (truename path))
                    path))))

(defun %paths-equal-p (left right)
  (and left
       right
       (string= (%path-namestring left)
                (%path-namestring right))))

(defun %parent-directory (directory-path)
  (let* ((normalized (uiop:ensure-directory-pathname directory-path))
         (components (pathname-directory normalized)))
    (when (and (listp components)
               (> (length components) 1))
      (uiop:ensure-directory-pathname
       (make-pathname :directory (butlast components)
                      :name nil
                      :type nil
                      :defaults normalized)))))

(defun %directory-config-path (&key directory-root)
  (loop for current = (%normalize-project-root directory-root)
          then (%parent-directory current)
        while current
        for candidate = (merge-pathnames #P".amoebum/config.lisp" current)
        for discovered = (and (probe-file candidate)
                              (probe-file candidate))
        when discovered
          do (return discovered)))

(defun %keyword-from-value (value)
  (cond
    ((keywordp value)
     value)
    ((symbolp value)
     (intern (string-upcase (symbol-name value)) :keyword))
    ((stringp value)
     (let* ((trimmed (%trim-string value))
            (normalized (substitute #\- #\_ (string-downcase trimmed))))
       (when (> (length normalized) 0)
         (intern (string-upcase normalized) :keyword))))
    (t
     nil)))

(defun %permission-mode-keyword (value)
  (let ((normalized (%keyword-from-value value)))
    (case normalized
      (:UNTRUSTED :supervised)
      (:ON-FAILURE :auto-edit)
      (:ON-REQUEST :supervised)
      (:NEVER :yolo)
      (otherwise normalized))))

(defun %approval-policy-keyword (value)
  (let ((normalized (%keyword-from-value value)))
    (case normalized
      (:ON_FAILURE :on-failure)
      (:ON-FAILURE :on-failure)
      (:ON_REQUEST :on-request)
      (:ON-REQUEST :on-request)
      (:UNTRUSTED :untrusted)
      (:NEVER :never)
      (otherwise normalized))))

(defun %sandbox-policy-keyword (value)
  (%keyword-from-value value))

(defun %sandbox-mode-keyword (value)
  (let ((normalized (%keyword-from-value value)))
    (case normalized
      (:READ_ONLY :read-only)
      (:READ-ONLY :read-only)
      (:WORKSPACE_WRITE :workspace-write)
      (:WORKSPACE-WRITE :workspace-write)
      (:DANGER_FULL_ACCESS :danger-full-access)
      (:DANGER-FULL-ACCESS :danger-full-access)
      (otherwise normalized))))

(defun %swarm-delegation-mode-keyword (value)
  (let ((normalized (%keyword-from-value value)))
    (case normalized
      (:NETWORKED :networked)
      (:LOCAL :local)
      (otherwise normalized))))

(defun %environment-config-values ()
  (let ((values (make-hash-table :test 'eq))
        (model (uiop:getenv "AMOEBUM_MODEL"))
        (permission-mode (uiop:getenv "AMOEBUM_PERMISSION_MODE"))
        (approval-policy (uiop:getenv "AMOEBUM_APPROVAL_POLICY"))
        (sandbox-mode (uiop:getenv "AMOEBUM_SANDBOX_MODE"))
        (swarm-delegation-mode (uiop:getenv "AMOEBUM_SWARM_DELEGATION_MODE")))
    (when (%non-empty-string-p model)
      (setf (gethash :model values) (%trim-string model)))
    (when (%non-empty-string-p permission-mode)
      (setf (gethash :permission-mode values)
            (%permission-mode-keyword permission-mode)))
    (when (%non-empty-string-p approval-policy)
      (setf (gethash :approval-policy values)
            (%approval-policy-keyword approval-policy)))
    (when (%non-empty-string-p sandbox-mode)
      (setf (gethash :sandbox-mode values)
            (%sandbox-mode-keyword sandbox-mode)))
    (when (%non-empty-string-p swarm-delegation-mode)
      (setf (gethash :swarm-delegation-mode values)
            (%swarm-delegation-mode-keyword swarm-delegation-mode)))
    values))

(defun %starts-with-string-p (prefix string)
  (and (stringp prefix)
       (stringp string)
       (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun %cli-argument-values (arguments)
  (let ((values (make-hash-table :test 'eq))
        (args (copy-list (or arguments '()))))
    (labels ((consume-value (flag)
               (unless args
                 (error "Missing value for CLI option ~A." flag))
               (let ((value (pop args)))
                 (if (%non-empty-string-p value)
                     (%trim-string value)
                     (error "Missing value for CLI option ~A." flag)))))
      (loop while args do
        (let ((argument (pop args)))
          (cond
            ((string= argument "--model")
             (setf (gethash :model values) (consume-value "--model")))
            ((%starts-with-string-p "--model=" argument)
             (setf (gethash :model values)
                   (%trim-string (subseq argument (length "--model=")))))
            ((string= argument "--permission-mode")
             (setf (gethash :permission-mode values)
                   (%permission-mode-keyword (consume-value "--permission-mode"))))
            ((%starts-with-string-p "--permission-mode=" argument)
             (setf (gethash :permission-mode values)
                   (%permission-mode-keyword
                    (%trim-string (subseq argument (length "--permission-mode="))))))
            ((string= argument "--approval-policy")
             (setf (gethash :approval-policy values)
                   (%approval-policy-keyword (consume-value "--approval-policy"))))
            ((%starts-with-string-p "--approval-policy=" argument)
             (setf (gethash :approval-policy values)
                   (%approval-policy-keyword
                    (%trim-string (subseq argument (length "--approval-policy="))))))
            ((string= argument "--sandbox-mode")
             (setf (gethash :sandbox-mode values)
                   (%sandbox-mode-keyword (consume-value "--sandbox-mode"))))
            ((%starts-with-string-p "--sandbox-mode=" argument)
             (setf (gethash :sandbox-mode values)
                   (%sandbox-mode-keyword
                    (%trim-string (subseq argument (length "--sandbox-mode="))))))
            ((string= argument "--swarm-delegation-mode")
             (setf (gethash :swarm-delegation-mode values)
                   (%swarm-delegation-mode-keyword
                    (consume-value "--swarm-delegation-mode"))))
            ((%starts-with-string-p "--swarm-delegation-mode=" argument)
             (setf (gethash :swarm-delegation-mode values)
                   (%swarm-delegation-mode-keyword
                    (%trim-string
                     (subseq argument (length "--swarm-delegation-mode=")))))))))
      values)))

(defun %coerce-layer-values (values)
  (let ((hash (make-hash-table :test 'eq)))
    (cond
      ((null values) hash)
      ((hash-table-p values)
       (maphash (lambda (key value)
                  (setf (gethash key hash) value))
                values)
       hash)
      ((and (listp values) (evenp (length values)))
       (loop for (key value) on values by #'cddr do
             (setf (gethash key hash) value))
       hash)
      (t
       (error "Layer values must be NIL, a hash table, or a property list.")))))

(defun load-config (&key
                      project-root
                      global-config-path
                      project-config-path
                      directory-root
                      directory-config-path
                      (environment-values :not-supplied)
                      cli-values
                      cli-arguments)
  (let* ((root (%normalize-project-root project-root))
         (cfg (%base-config :project-root root))
         (global-values (%load-layer (or global-config-path (%global-config-path))))
         (project-path (or project-config-path (%project-config-path root)))
         (project-values (%load-layer project-path))
         (resolved-directory-path
           (or directory-config-path
               (%directory-config-path :directory-root (or directory-root *default-pathname-defaults*))))
         (effective-directory-path
           (if (%paths-equal-p project-path resolved-directory-path)
               nil
               resolved-directory-path))
         (directory-values (%load-layer effective-directory-path))
         (env-values
           (if (eq environment-values :not-supplied)
               (%environment-config-values)
               (%coerce-layer-values environment-values)))
         (cli-layer-values
           (cond
             (cli-values (%coerce-layer-values cli-values))
             (cli-arguments (%cli-argument-values cli-arguments))
             (t (make-hash-table :test 'eq)))))
    (dolist (key (%hash-keys global-values))
      (%merge-value cfg key (gethash key global-values) :global root))
    (dolist (key (%hash-keys project-values))
      (%merge-value cfg key (gethash key project-values) :project root))
    (dolist (key (%hash-keys directory-values))
      (%merge-value cfg key (gethash key directory-values) :directory root))
    (dolist (key (%hash-keys env-values))
      (%merge-value cfg key (gethash key env-values) :env root))
    (dolist (key (%hash-keys cli-layer-values))
      (%merge-value cfg key (gethash key cli-layer-values) :cli root))
    cfg))

(defun reload-config (&key
                        project-root
                        global-config-path
                        project-config-path
                        directory-root
                        directory-config-path
                        (environment-values :not-supplied)
                        cli-values
                        cli-arguments)
  (setf *current-config*
        (load-config :project-root project-root
                     :global-config-path global-config-path
                     :project-config-path project-config-path
                     :directory-root directory-root
                     :directory-config-path directory-config-path
                     :environment-values environment-values
                     :cli-values cli-values
                     :cli-arguments cli-arguments))
  (when (fboundp 'clear-resolved-provider-cache)
    (clear-resolved-provider-cache))
  *current-config*)

(defun current-config ()
  (or *current-config*
      (setf *current-config* (load-config))))

(defun config-value (key &optional (cfg (current-config)))
  (gethash key (config-values cfg)))

(defun config-layer-source (key &optional (cfg (current-config)))
  (gethash key (config-sources cfg)))

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
    (when (fboundp 'clear-resolved-provider-cache)
      (clear-resolved-provider-cache))
    (emit-config-changed key old-value (config-value key cfg))
    (config-value key cfg)))
