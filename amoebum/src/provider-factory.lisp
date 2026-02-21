(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; Provider Factory (I135)
;;; ---------------------------------------------------------------------------

(defvar *resolved-provider* nil
  "Cached provider resolved from the current configuration.")

(defvar *resolved-provider-cache-key* nil
  "Cache key corresponding to `*resolved-provider*`.")

(defun clear-resolved-provider-cache ()
  "Clear the cached resolved provider."
  (setf *resolved-provider* nil
        *resolved-provider-cache-key* nil))

(defun %provider-prefix-p (model prefix)
  (let ((lower-model (and (stringp model)
                          (string-downcase model)))
        (lower-prefix (string-downcase prefix)))
    (and lower-model
         (>= (length lower-model) (length lower-prefix))
         (string= (subseq lower-model 0 (length lower-prefix))
                  lower-prefix))))

(defun %trim-env-key (value)
  (let ((trimmed (and (stringp value)
                      (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   value))))
    (and trimmed
         (plusp (length trimmed))
         trimmed)))

(defun %provider-class-from-model (model)
  (cond
    ((%provider-prefix-p model "claude-")
     :anthropic-provider)
    ((or (%provider-prefix-p model "gpt-")
         (%provider-prefix-p model "o1-")
         (%provider-prefix-p model "o3-"))
     :openai-compatible-provider)
    ((%provider-prefix-p model "moonshot-")
     :kimi-provider)
    (t
     :openai-compatible-provider)))

(defun %normalize-provider-override (value)
  (when value
    (let* ((raw-name (typecase value
                       (symbol (symbol-name value))
                       (string value)
                       (keyword (symbol-name value))
                       (t (error "Unknown provider override type: ~S" value))))
           (trimmed (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                 raw-name)))
           (package-separator (position #\: trimmed :from-end t))
           (bare-name (if package-separator
                          (subseq trimmed (1+ package-separator))
                          trimmed)))
      (cond
        ((member bare-name '("anthropic-provider" "anthropic") :test #'string=)
         :anthropic-provider)
        ((member bare-name '("openai-compatible-provider" "openai-compat" "openai")
                 :test #'string=)
         :openai-compatible-provider)
        ((member bare-name '("kimi-provider" "kimi")
                 :test #'string=)
         :kimi-provider)
        (t
         (error "Unknown provider override ~S. Expected one of :anthropic-provider, :openai-compatible-provider, :kimi-provider." value))))))

(defun %provider-env-var-for-class (provider-class)
  (ecase provider-class
    (:anthropic-provider "ANTHROPIC_API_KEY")
    (:openai-compatible-provider "OPENAI_API_KEY")
    (:kimi-provider "MOONSHOT_API_KEY")))

(defun %make-provider (provider-class model api-base-url)
  (let* ((api-key (%trim-env-key (uiop:getenv (%provider-env-var-for-class provider-class))))
         (resolved-base-url (%trim-env-key api-base-url)))
    (case provider-class
      (:anthropic-provider
       (pseudopod:make-anthropic-provider :api-key api-key
                                          :model model))
      (:openai-compatible-provider
       (if resolved-base-url
           (pseudopod:make-openai-compatible-provider :api-key api-key
                                                      :model model
                                                      :base-url resolved-base-url)
           (pseudopod:make-openai-compatible-provider :api-key api-key
                                                      :model model)))
      (:kimi-provider
       (pseudopod:make-kimi-provider :api-key api-key
                                     :model model))
      (otherwise
       (error "Unsupported provider class ~S." provider-class)))))

(defun resolve-provider (&optional (cfg (current-config)))
  "Resolve and cache provider from config values."
  (let* ((model (string-downcase (or (%trim-env-key (config-value :model cfg))
                                    "moonshot-v1-128k")))
         (provider-override (config-value :provider-override cfg))
         (api-base-url (config-value :api-base-url cfg))
         (provider-class (or (%normalize-provider-override provider-override)
                            (%provider-class-from-model model)))
         (cache-key (list provider-class
                          model
                          (%trim-env-key api-base-url))))
    (if (and *resolved-provider*
             (equal cache-key *resolved-provider-cache-key*))
        *resolved-provider*
        (progn
          (setf *resolved-provider* (%make-provider provider-class model api-base-url)
                *resolved-provider-cache-key* cache-key)
          *resolved-provider*))))
