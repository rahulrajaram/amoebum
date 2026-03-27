(in-package :amoebum)

(defparameter *default-config-values* nil)

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

(defparameter *known-provider-overrides*
  '("anthropic-provider" "anthropic"
    "openai-compatible-provider" "openai-compat" "openai"
    "kimi-provider" "kimi"))

(defstruct (config-schema-entry
            (:constructor make-config-schema-entry
                (&key key type default validator)))
  key
  type
  default
  validator)

(defparameter *config-schema* (make-hash-table :test 'eq))

(defparameter *config-schema-definitions*
  '((:key :model
     :type string
     :default "moonshot-v1-128k"
     :validator (:predicate %non-empty-string-p))
    (:key :provider-override
     :type (or null string symbol keyword)
     :default nil
     :validator (:member *known-provider-overrides*
                 :normalize %provider-override-token
                 :test string=
                 :allow-null t))
    (:key :api-base-url
     :type (or null string)
     :default nil
     :validator (:predicate %non-empty-string-p :allow-null t))
    (:key :context-window-limit
     :type (or null integer)
     :default nil
     :validator (:integer-range :min 1 :allow-null t))
    (:key :stream-budget-abort-threshold-percent
     :type integer
     :default 80
     :validator (:integer-range :min 1 :max 100))
    (:key :permission-mode
     :type (or keyword symbol string)
     :default :supervised
     :validator (:member *known-permission-modes*
                 :normalize %permission-mode-keyword))
    (:key :approval-policy
     :type (or keyword symbol string)
     :default :on-request
     :validator (:member *known-approval-policies*
                 :normalize %approval-policy-keyword))
    (:key :sandbox-policy
     :type (or keyword symbol string)
     :default :strict
     :validator (:member *known-sandbox-policies*
                 :normalize %sandbox-policy-keyword))
    (:key :sandbox-mode
     :type (or keyword symbol string)
     :default :workspace-write
     :validator (:member *known-sandbox-modes*
                 :normalize %sandbox-mode-keyword))
    (:key :swarm-delegation-mode
     :type (or keyword symbol string)
     :default :local
     :validator (:member *known-swarm-delegation-modes*
                 :normalize %swarm-delegation-mode-keyword))
    (:key :memory-backend
     :type keyword
     :default :auto
     :validator (:member *known-memory-backends*))
    (:key :web-search-searxng-url
     :type (or null string)
     :default nil
     :validator (:predicate %non-empty-string-p :allow-null t))
    (:key :web-search-duckduckgo-url
     :type (or null string)
     :default "https://duckduckgo.com/html/"
     :validator (:predicate %non-empty-string-p :allow-null t))
    (:key :web-search-allow-domains
     :type t
     :default nil
     :validator (:predicate %string-sequence-p))
    (:key :web-search-block-domains
     :type t
     :default nil
     :validator (:predicate %string-sequence-p))
    (:key :web-search-user-agent
     :type (or null string)
     :default "amoebum-web-search/0.1"
     :validator (:predicate %non-empty-string-p :allow-null t))
    (:key :web-fetch-timeout-seconds
     :type integer
     :default 20
     :validator (:integer-range :min 1))
    (:key :web-fetch-cache-ttl-seconds
     :type integer
     :default 900
     :validator (:integer-range :min 1))
    (:key :web-fetch-max-markdown-bytes
     :type integer
     :default 10240
     :validator (:integer-range :min 1))
    (:key :web-fetch-user-agent
     :type (or null string)
     :default "amoebum-web-fetch/0.1"
     :validator (:predicate %non-empty-string-p :allow-null t))
    (:key :haake-command
     :type string
     :default "haake"
     :validator (:predicate %non-empty-string-p))
    (:key :haake-project-id
     :type (or null string)
     :default nil
     :validator (:predicate %non-empty-string-p :allow-null t))
    (:key :haake-agent
     :type string
     :default "amoebum"
     :validator (:predicate %non-empty-string-p))
    (:key :haake-autodetect
     :type boolean
     :default t)
    (:key :notifications-enabled
     :type boolean
     :default t)
    (:key :notification-events
     :type t
     :default (:task-complete :error :approval-needed)
     :validator (:predicate %keyword-like-sequence-p))
    (:key :notification-sound-enabled
     :type boolean
     :default t)
    (:key :notification-desktop-enabled
     :type boolean
     :default t)
    (:key :notification-log-enabled
     :type boolean
     :default t)
    (:key :notification-sound-player
     :type (or null string)
     :default nil
     :validator (:predicate %non-empty-string-p :allow-null t))
    (:key :notification-desktop-command
     :type (or null string)
     :default nil
     :validator (:predicate %non-empty-string-p :allow-null t))
    (:key :notification-log-path
     :type (or null pathname string)
     :default nil
     :validator (:predicate %pathname-or-string-p :allow-null t))
    (:key :notification-webhooks
     :type (or null list vector)
     :default nil)
    (:key :notification-sound-task-complete
     :type (or null pathname string)
     :default nil
     :validator (:predicate %pathname-or-string-p :allow-null t))
    (:key :notification-sound-error
     :type (or null pathname string)
     :default nil
     :validator (:predicate %pathname-or-string-p :allow-null t))
    (:key :notification-sound-approval-needed
     :type (or null pathname string)
     :default nil
     :validator (:predicate %pathname-or-string-p :allow-null t))
    (:key :tts-command
     :type (or null string)
     :default "kokoro-tts"
     :validator (:predicate %non-empty-string-p :allow-null t))
    (:key :tts-python-module
     :type boolean
     :default nil)
    (:key :tts-voice
     :type (or null string)
     :default "af_heart"
     :validator (:predicate %non-empty-string-p :allow-null t))
    (:key :tts-auto-speak
     :type boolean
     :default nil)
    (:key :auto-checkpoint-idle-seconds
     :type integer
     :default 1800
     :validator (:integer-range :min 0))
    (:key :auto-checkpoint-max-count
     :type integer
     :default 10
     :validator (:integer-range :min 1))
    (:key :plan-mode
     :type boolean
     :default nil)
    (:key :theme-yaml
     :type (or boolean string)
     :default t
     :validator (:predicate %theme-yaml-config-value-p))
    (:key :project-root
     :type (or pathname string)
     :default nil
     :validator (:predicate %pathname-or-string-p))))

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
      (let ((entry (%config-schema-entry key)))
        (if entry
            (config-schema-entry-default entry)
            (getf *default-config-values* key)))))

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

(defun %theme-yaml-config-value-p (value)
  (or (typep value 'boolean)
      (%non-empty-string-p value)))

(defun %provider-override-token (value)
  (when value
    (and (or (stringp value)
             (symbolp value)
             (keywordp value))
         (let ((normalized (string-downcase (%trim-string (string value)))))
           (and (> (length normalized) 0)
                normalized)))))

(defun %parse-boolean (value)
  (let ((trimmed (string-downcase (%trim-string value))))
    (cond
      ((member trimmed '("1" "true" "t" "yes" "on") :test #'string=) t)
      ((member trimmed '("0" "false" "nil" "no" "off") :test #'string=) nil)
      (t
       nil))))

(defun %register-config-schema-entry (key type default &optional validator)
  (setf (gethash key *config-schema*)
        (make-config-schema-entry :key key
                                  :type type
                                  :default default
                                  :validator validator)))

(defun %validator-function (designator)
  (cond
    ((null designator) nil)
    ((functionp designator) designator)
    ((and (symbolp designator) (fboundp designator))
     (symbol-function designator))
    (t
     (error "Unsupported validator function designator ~S." designator))))

(defun %validator-data (value)
  (if (and (symbolp value) (boundp value))
      (symbol-value value)
      value))

(defun %plist-contains-key-p (plist key)
  (loop for (entry-key nil) on plist by #'cddr
        thereis (eq entry-key key)))

(defun %schema-definition-default-value (definition)
  (if (%plist-contains-key-p definition :default)
      (getf definition :default)
      nil))

(defun %schema-default-values ()
  (loop for definition in *config-schema-definitions*
        for key = (getf definition :key)
        unless (eq key :project-root)
          append (list key (%schema-definition-default-value definition))))

(setf *default-config-values* (%schema-default-values))

(defun %make-predicate-validator (predicate allow-null)
  (let ((predicate-fn (%validator-function predicate)))
    (lambda (value)
      (or (and allow-null (null value))
          (funcall predicate-fn value)))))

(defun %make-member-validator (choices normalize test allow-null)
  (let ((choice-list (%validator-data choices))
        (normalize-fn (and normalize (%validator-function normalize)))
        (test-fn (or (and test (%validator-function test)) #'eq)))
    (lambda (value)
      (or (and allow-null (null value))
          (member (if normalize-fn
                      (funcall normalize-fn value)
                      value)
                  choice-list
                  :test test-fn)))))

(defun %make-integer-range-validator (min max allow-null)
  (lambda (value)
    (or (and allow-null (null value))
        (and (integerp value)
             (or (null min) (>= value min))
             (or (null max) (<= value max))))))

(defun %validator-from-spec (spec)
  (cond
    ((null spec) nil)
    ((or (functionp spec)
         (and (symbolp spec) (fboundp spec)))
     (%validator-function spec))
    ((consp spec)
     (case (first spec)
       (:predicate
        (destructuring-bind (_ predicate &key allow-null) spec
          (declare (ignore _))
          (%make-predicate-validator predicate allow-null)))
       (:member
        (destructuring-bind (_ choices &key normalize test allow-null) spec
          (declare (ignore _))
          (%make-member-validator choices normalize test allow-null)))
       (:integer-range
        (destructuring-bind (&key min max allow-null &allow-other-keys) (rest spec)
          (%make-integer-range-validator min max allow-null)))
       (otherwise
        (error "Unsupported config validator spec ~S." spec))))
    (t
     (error "Unsupported config validator spec ~S." spec))))

(defun %register-config-schema-definition (definition)
  (destructuring-bind (&key key type validator &allow-other-keys) definition
    (%register-config-schema-entry key
                                   type
                                   (%schema-definition-default-value definition)
                                   (%validator-from-spec validator))))

(defun %config-schema-entry (key)
  (%ensure-config-schema)
  (gethash key *config-schema*))

(defun %schema-type-valid-p (type value)
  (or (null type)
      (eq type t)
      (typep value type)))

(defun %valid-config-value-p (key value)
  (let ((entry (%config-schema-entry key)))
    (if (null entry)
        t
        (let ((type-ok (%schema-type-valid-p (config-schema-entry-type entry) value))
              (validator (config-schema-entry-validator entry)))
          (and type-ok
               (if validator
                   (funcall validator value)
                   t))))))

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
    (cond
      ((null normalized) nil)
      ((or (eq normalized :on_failure)
           (eq normalized :on-failure))
       :on-failure)
      ((or (eq normalized :on_request)
           (eq normalized :on-request))
       :on-request)
      ((eq normalized :untrusted)
       :untrusted)
      ((eq normalized :never)
       :never)
      (t
       normalized))))

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

(defun %ensure-config-schema ()
  (when (zerop (hash-table-count *config-schema*))
    (dolist (definition *config-schema-definitions*)
      (%register-config-schema-definition definition)))
  *config-schema*)

(defun %environment-config-values ()
  (let ((values (make-hash-table :test 'eq))
        (model (uiop:getenv "AMOEBUM_MODEL"))
        (permission-mode (uiop:getenv "AMOEBUM_PERMISSION_MODE"))
        (approval-policy (uiop:getenv "AMOEBUM_APPROVAL_POLICY"))
        (sandbox-mode (uiop:getenv "AMOEBUM_SANDBOX_MODE"))
        (swarm-delegation-mode (uiop:getenv "AMOEBUM_SWARM_DELEGATION_MODE"))
        (tts-command (uiop:getenv "AMOEBUM_TTS_COMMAND"))
        (tts-voice (uiop:getenv "AMOEBUM_TTS_VOICE"))
        (tts-python-module (uiop:getenv "AMOEBUM_TTS_PYTHON_MODULE"))
        (tts-auto-speak (uiop:getenv "AMOEBUM_TTS_AUTO_SPEAK")))
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
    (when (%non-empty-string-p tts-command)
      (setf (gethash :tts-command values) (%trim-string tts-command)))
    (when (%non-empty-string-p tts-voice)
      (setf (gethash :tts-voice values) (%trim-string tts-voice)))
    (when (%non-empty-string-p tts-python-module)
      (setf (gethash :tts-python-module values)
            (%parse-boolean tts-python-module)))
    (when (%non-empty-string-p tts-auto-speak)
      (setf (gethash :tts-auto-speak values)
            (%parse-boolean tts-auto-speak)))
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
                     (subseq argument (length "--swarm-delegation-mode="))))))
            ((string= argument "--theme-yaml")
             (setf (gethash :theme-yaml values)
                   (consume-value "--theme-yaml")))
            ((string= argument "--theme")
             (setf (gethash :theme-yaml values)
                   (consume-value "--theme")))
            ((%starts-with-string-p "--theme-yaml=" argument)
             (setf (gethash :theme-yaml values)
                   (%trim-string (subseq argument (length "--theme-yaml=")))))
            ((%starts-with-string-p "--theme=" argument)
             (setf (gethash :theme-yaml values)
                   (%trim-string (subseq argument (length "--theme="))))))))
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

(defun cfg (key &optional default)
  "Concise config accessor. Returns the value for KEY from current-config,
or DEFAULT if the config is nil or the key is missing."
  (or (ignore-errors (config-value key (current-config)))
      default))

(defun config-layer-source (key &optional (cfg (current-config)))
  (gethash key (config-sources cfg)))

(defun describe-config (key &optional (cfg (current-config)))
  (let* ((entry (%config-schema-entry key))
         (source (config-layer-source key cfg)))
    (list :key key
          :value (config-value key cfg)
          :source source
          :source-label (if source (string-downcase (symbol-name source)) "unknown")
          :default (if (eq key :project-root)
                       (config-project-root cfg)
                       (and entry (config-schema-entry-default entry)))
          :type (and entry (config-schema-entry-type entry))
          :validator-present-p (and entry (not (null (config-schema-entry-validator entry)))))))

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
