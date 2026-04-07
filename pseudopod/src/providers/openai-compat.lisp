(in-package :pseudopod)

;;; ---------------------------------------------------------------------------
;;; OpenAI-Compatible Provider (I94, I244)
;;;
;;; Generic provider for OpenAI API-compatible services:
;;; OpenAI, Together AI, Groq, Ollama, vLLM, etc.
;;; ---------------------------------------------------------------------------

(defparameter *openai-compatible-backend-defaults*
  '((:openai
     :name "openai"
     :base-url "https://api.openai.com/v1"
     :default-model "gpt-4o"
     :api-key-env "OPENAI_API_KEY"
     :base-url-env "OPENAI_BASE_URL"
     :model-env "OPENAI_MODEL")
    (:openrouter
     :name "openrouter"
     :base-url "https://openrouter.ai/api/v1"
     :default-model "openai/gpt-4o-mini"
     :api-key-env "OPENROUTER_API_KEY"
     :base-url-env "OPENROUTER_BASE_URL"
     :model-env "OPENROUTER_MODEL")
    (:together
     :name "together"
     :base-url "https://api.together.xyz/v1"
     :default-model "meta-llama/Llama-3.3-70B-Instruct-Turbo"
     :api-key-env "TOGETHER_API_KEY"
     :base-url-env "TOGETHER_BASE_URL"
     :model-env "TOGETHER_MODEL")
    (:vllm
     :name "vllm"
     :base-url "http://localhost:8000/v1"
     :default-model "local-model"
     :api-key-env "VLLM_API_KEY"
     :base-url-env "VLLM_BASE_URL"
     :model-env "VLLM_MODEL")
    (:ollama
     :name "ollama"
     :base-url "http://localhost:11434/v1"
     :default-model "llama3.2"
     :api-key-env "OLLAMA_API_KEY"
     :base-url-env "OLLAMA_BASE_URL"
     :model-env "OLLAMA_MODEL")))

(defun %openai-compat-string (value)
  (typecase value
    (string value)
    (symbol (symbol-name value))
    (t nil)))

(defun %openai-compat-trimmed-string (value)
  (let ((string (%openai-compat-string value)))
    (when string
      (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) string)))
        (when (plusp (length trimmed))
          (let ((lower (string-downcase trimmed)))
            (unless (member lower '("nil" "null" "none") :test #'string=)
              trimmed)))))))

(defun %openai-compat-env (name)
  (when name
    (handler-case
        (%openai-compat-trimmed-string (uiop:getenv name))
      (error ()
        nil))))

(defun %openai-compat-backend-config (backend)
  (cdr (assoc backend *openai-compatible-backend-defaults*)))

(defun %openai-compat-normalize-backend (backend)
  (let* ((raw (%openai-compat-trimmed-string backend))
         (normalized (and raw (string-downcase raw))))
    (cond
      ((or (null normalized) (string= normalized ""))
       :openai)
      ((member normalized '("openai" "openai-compatible" "openai_compatible")
               :test #'string=)
       :openai)
      ((member normalized '("openrouter" "open-router") :test #'string=)
       :openrouter)
      ((member normalized '("together" "together-ai" "together_ai")
               :test #'string=)
       :together)
      ((string= normalized "vllm")
       :vllm)
      ((string= normalized "ollama")
       :ollama)
      (t
       (error "Unknown OpenAI-compatible backend ~S" backend)))))

(defun %openai-compat-string-suffix-p (suffix string)
  (and (stringp suffix)
       (stringp string)
       (let ((suffix-length (length suffix))
             (string-length (length string)))
         (and (<= suffix-length string-length)
              (string= suffix
                       (subseq string (- string-length suffix-length)))))))

(defun %openai-compat-ensure-v1-path (url)
  (let ((trimmed (string-right-trim "/" (or url ""))))
    (cond
      ((string= trimmed "")
       trimmed)
      ((%openai-compat-string-suffix-p "/v1" trimmed)
       trimmed)
      (t
       (format nil "~A/v1" trimmed)))))

(defun %openai-compat-backend-base-url (backend config)
  (let* ((base-url-env (getf config :base-url-env))
         (base-url-from-env (%openai-compat-env base-url-env))
         (ollama-host (and (eq backend :ollama)
                           (%openai-compat-env "OLLAMA_HOST"))))
    (cond
      ((and (eq backend :ollama)
            ollama-host)
       (%openai-compat-ensure-v1-path ollama-host))
      (base-url-from-env
       (if (eq backend :ollama)
           (%openai-compat-ensure-v1-path base-url-from-env)
           (string-right-trim "/" base-url-from-env)))
      (t
       (string-right-trim "/" (or (getf config :base-url) ""))))))

(defun %openai-compat-backend-model (config)
  (or (%openai-compat-env (getf config :model-env))
      (%openai-compat-trimmed-string (getf config :default-model))
      "gpt-4o"))

(defun %openai-compat-backend-name (config)
  (or (%openai-compat-trimmed-string (getf config :name))
      "openai"))

(defun %openai-compat-backend-api-key (config api-key api-key-env-var)
  (or (%openai-compat-trimmed-string api-key)
      (%openai-compat-env (or api-key-env-var (getf config :api-key-env)))
      ""))

(defun %openai-compat-normalized-pathname (path)
  (typecase path
    (pathname path)
    (string (ignore-errors (parse-namestring path)))
    (t nil)))

(defun %openai-compat-provider-plist-p (value)
  (and (listp value)
       (evenp (length value))
       (loop for (key _) on value by #'cddr
             always (or (keywordp key) (symbolp key) (stringp key)))))

(defun %openai-compat-keyword (value)
  (let ((raw (%openai-compat-trimmed-string value)))
    (when raw
      (intern (string-upcase (substitute #\- #\_ raw)) :keyword))))

(defun %openai-compat-normalize-plist (plist)
  (loop for (key value) on plist by #'cddr
        for normalized-key = (%openai-compat-keyword key)
        when normalized-key append (list normalized-key value)))

(defun %openai-compat-config-entries (form)
  (cond
    ((null form)
     nil)
    ((%openai-compat-provider-plist-p form)
     (let* ((normalized (%openai-compat-normalize-plist form))
            (providers (getf normalized :providers)))
       (if providers
           (if (listp providers) providers (list providers))
           (list normalized))))
    ((and (listp form) (every #'listp form))
     form)
    ((listp form)
     (list form))
    (t
     nil)))

(defun %openai-compat-normalize-provider-spec (entry)
  (let* ((raw-plist
           (cond
             ((and (consp entry)
                   (or (symbolp (first entry)) (stringp (first entry)))
                   (not (keywordp (first entry)))
                   (%openai-compat-provider-plist-p (rest entry)))
              (append (list :name (first entry))
                      (%openai-compat-normalize-plist (rest entry))))
             ((%openai-compat-provider-plist-p entry)
              (%openai-compat-normalize-plist entry))
             (t
              nil))))
    (when raw-plist
      (let* ((backend-raw (or (getf raw-plist :backend)
                              (getf raw-plist :type)
                              :openai))
             (backend (handler-case
                          (%openai-compat-normalize-backend backend-raw)
                        (error ()
                          nil)))
             (config (and backend (%openai-compat-backend-config backend)))
             (name (or (%openai-compat-trimmed-string (getf raw-plist :name))
                       (%openai-compat-backend-name config)))
             (base-url (or (%openai-compat-trimmed-string (getf raw-plist :base-url))
                           (%openai-compat-backend-base-url backend config)))
             (model (or (%openai-compat-trimmed-string (getf raw-plist :model))
                        (%openai-compat-backend-model config)))
             (api-key (%openai-compat-trimmed-string (getf raw-plist :api-key)))
             (api-key-env-var (%openai-compat-trimmed-string
                               (getf raw-plist :api-key-env-var)))
             (organization (%openai-compat-trimmed-string
                            (getf raw-plist :organization)))
             (timeout-value (getf raw-plist :timeout-seconds))
             (timeout-seconds
               (cond
                 ((integerp timeout-value)
                  timeout-value)
                 ((stringp timeout-value)
                  (ignore-errors (parse-integer timeout-value)))
                 (t
                  nil))))
        (when (and backend name base-url model)
          (list :name (string-downcase name)
                :backend backend
                :base-url base-url
                :model model
                :api-key api-key
                :api-key-env-var api-key-env-var
                :organization organization
                :timeout-seconds timeout-seconds))))))

(defun %openai-compat-read-provider-specs-from-file (path)
  (let ((pathname (%openai-compat-normalized-pathname path)))
    (when pathname
      (let ((existing (probe-file pathname)))
        (when existing
          (handler-case
              (with-open-file (in existing :direction :input)
                (let* ((*read-eval* nil)
                       (form (read in nil nil))
                       (entries (%openai-compat-config-entries form)))
                  (loop for entry in entries
                        for spec = (%openai-compat-normalize-provider-spec entry)
                        when spec collect spec)))
            (error ()
              nil)))))))

(defun %openai-compat-default-provider-config-paths ()
  (let* ((home (user-homedir-pathname))
         (cwd (uiop:getcwd))
         (paths
           (list
            (and home (merge-pathnames #P".config/pseudopod/providers.sexp" home))
            (and home (merge-pathnames #P".config/pseudopod/providers.lisp" home))
            (and home (merge-pathnames #P".pseudopod/providers.sexp" home))
            (and home (merge-pathnames #P".pseudopod/providers.lisp" home))
            (and cwd (merge-pathnames #P".pseudopod/providers.sexp" cwd))
            (and cwd (merge-pathnames #P".pseudopod/providers.lisp" cwd))
            (and cwd (merge-pathnames #P".amoebum/providers.sexp" cwd))
            (and cwd (merge-pathnames #P".amoebum/providers.lisp" cwd)))))
    (remove nil paths)))

(defun %openai-compat-env-provider-spec (backend)
  (let* ((config (%openai-compat-backend-config backend))
         (name (%openai-compat-backend-name config))
         (base-url (%openai-compat-backend-base-url backend config))
         (model (%openai-compat-backend-model config))
         (api-key-env (getf config :api-key-env))
         (base-url-env (getf config :base-url-env))
         (model-env (getf config :model-env))
         (api-key (%openai-compat-env api-key-env))
         (explicit-base-url (%openai-compat-env base-url-env))
         (explicit-model (%openai-compat-env model-env))
         (ollama-host (%openai-compat-env "OLLAMA_HOST"))
         (discover-p
           (or api-key
               explicit-base-url
               explicit-model
               (and (eq backend :ollama) ollama-host))))
    (when discover-p
      (list :name name
            :backend backend
            :base-url base-url
            :model model
            :api-key api-key
            :api-key-env-var api-key-env))))

(defun %openai-compat-merge-provider-specs (&rest lists)
  (let ((table (make-hash-table :test #'equal))
        (order '()))
    (dolist (spec-list lists)
      (dolist (spec spec-list)
        (let ((name (string-downcase (or (getf spec :name) ""))))
          (unless (gethash name table)
            (push name order))
          (setf (gethash name table) spec))))
    (loop for name in (nreverse order)
          for spec = (gethash name table)
          when spec collect spec)))

(defun %openai-compat-provider-from-spec (spec)
  (make-openai-compatible-provider
   :backend (getf spec :backend)
   :name (getf spec :name)
   :base-url (getf spec :base-url)
   :model (getf spec :model)
   :api-key (getf spec :api-key)
   :api-key-env-var (getf spec :api-key-env-var)
   :organization (getf spec :organization)
   :timeout-seconds (or (getf spec :timeout-seconds) 180)))

(defun %openai-compat-find-provider (provider-name providers)
  (let ((needle (string-downcase (or (%openai-compat-trimmed-string provider-name) ""))))
    (find needle
          providers
          :key (lambda (provider)
                 (string-downcase (provider-name provider)))
          :test #'string=)))

(defclass openai-compatible-provider (provider)
  ((backend :initarg :backend :reader openai-compat-backend
            :type keyword :initform :openai)
   (organization :initarg :organization :accessor openai-compat-organization
                 :type (or null string) :initform nil))
  (:default-initargs
   :backend :openai
   :name "openai"
   :base-url "https://api.openai.com/v1"
   :default-model "gpt-4o")
  (:documentation "Provider for OpenAI-compatible chat completions API."))

(defun make-openai-compatible-provider (&key api-key
                                             backend
                                             name
                                             base-url
                                             model
                                             (timeout-seconds 180)
                                             api-key-env-var
                                             organization)
  "Create an OpenAI-compatible provider.

BACKEND can be one of: :openai, :openrouter, :together, :vllm, :ollama."
  (let* ((resolved-backend (%openai-compat-normalize-backend backend))
         (backend-config (%openai-compat-backend-config resolved-backend))
         (resolved-name (or (%openai-compat-trimmed-string name)
                            (%openai-compat-backend-name backend-config)))
         (resolved-base-url
           (or (%openai-compat-trimmed-string base-url)
               (%openai-compat-backend-base-url resolved-backend backend-config)))
         (resolved-model
           (or (%openai-compat-trimmed-string model)
               (%openai-compat-backend-model backend-config)))
         (resolved-key (%openai-compat-backend-api-key backend-config
                                                       api-key
                                                       api-key-env-var)))
    (make-instance 'openai-compatible-provider
                   :backend resolved-backend
                   :name resolved-name
                   :api-key resolved-key
                   :base-url (string-right-trim "/" (or resolved-base-url ""))
                   :default-model resolved-model
                   :timeout-seconds timeout-seconds
                   :organization organization)))

(defun list-providers (&key (config-paths :default))
  "Discover configured OpenAI-compatible providers from env vars and config files."
  (let* ((env-specs
           (remove nil
                   (mapcar #'%openai-compat-env-provider-spec
                           '(:openai :openrouter :together :vllm :ollama))))
         (paths
           (cond
             ((eq config-paths :default)
              (%openai-compat-default-provider-config-paths))
             ((null config-paths)
              nil)
             ((listp config-paths)
              config-paths)
             (t
              (list config-paths))))
         (file-specs
           (loop for path in paths
                 append (%openai-compat-read-provider-specs-from-file path)))
         (merged-specs (%openai-compat-merge-provider-specs env-specs file-specs)))
    (mapcar #'%openai-compat-provider-from-spec merged-specs)))

(defun provider-models (provider &key providers (config-paths :default))
  "Return model metadata for PROVIDER (instance or provider-name)."
  (let* ((discovered (or providers (list-providers :config-paths config-paths)))
         (resolved-provider
           (typecase provider
             (provider provider)
             (string (%openai-compat-find-provider provider discovered))
             (symbol (%openai-compat-find-provider (symbol-name provider) discovered))
             (t nil))))
    (when resolved-provider
      (list-provider-models resolved-provider))))

(defun %openai-compat-headers (provider)
  "Build HTTP headers for OpenAI-compatible API."
  (let ((headers `((:authorization . ,(format nil "Bearer ~A" (provider-api-key provider)))
                   (:content-type . "application/json"))))
    (when (and (slot-boundp provider 'organization)
               (openai-compat-organization provider))
      (push `(:openai-organization . ,(openai-compat-organization provider))
            headers))
    headers))

(defun %openai-compat-endpoint (provider path)
  "Build full URL for OpenAI-compatible API."
  (format nil "~A~A" (provider-base-url provider) path))

(defun %openai-compat-normalize-stream-result (role content tool-calls usage)
  (let ((response (make-hash-table :test #'equal)))
    (setf (gethash "role" response) (or role "assistant"))
    (setf (gethash "content" response) (or content ""))
    (setf (gethash "tool_calls" response) (coerce (or tool-calls '()) 'vector))
    (when usage
      (setf (gethash "usage" response) usage))
    response))

(defun %openai-compat-trim-sse-data (line)
  (cond
    ((uiop:string-prefix-p "data: " line) (subseq line 6))
    ((uiop:string-prefix-p "data:" line)
     (string-left-trim '(#\Space #\Tab) (subseq line 5)))
    (t nil)))

(defun %openai-compat-collect-stream (body-stream callback)
  (let ((snapshot (make-stream-turn-snapshot)))
    (labels ((consume-payload (payload)
               (let* ((json (jonathan:parse payload :as :hash-table :junk-allowed t))
                      (choices (and (hash-table-p json) (gethash "choices" json)))
                      (choice (%first-item choices))
                      (delta (and (hash-table-p choice) (gethash "delta" choice)))
                      (delta-role (and (hash-table-p delta) (gethash "role" delta)))
                      (delta-content (and (hash-table-p delta) (gethash "content" delta)))
                      (delta-tool-calls (and (hash-table-p delta) (gethash "tool_calls" delta)))
                      (event-usage (and (hash-table-p json) (gethash "usage" json))))
                 (when (%non-empty-string-p delta-role)
                   (stream-turn-apply-event! snapshot
                                             (list :type :role
                                                   :role delta-role)))
	                 (when (%non-empty-string-p delta-content)
	                   (stream-turn-apply-event! snapshot
	                                             (list :type :text-delta
	                                                   :text delta-content))
	                   (when callback
	                     (funcall callback delta-content)))
	                 (dolist (tool-call (%sequence->list delta-tool-calls))
	                   (when (hash-table-p tool-call)
	                     (stream-turn-apply-event! snapshot
	                                               (list :type :tool-call-delta
	                                                     :index (%stream-turn-parse-index
	                                                             (gethash "index" tool-call))
	                                                     :tool-call (hash-to-tool-call tool-call)
	                                                     :tool-call-id (gethash "id" tool-call)
	                                                     :name (%stream-tool-call-name tool-call)
	                                                     :arguments (%stream-tool-call-arguments tool-call)
	                                                     :arguments-complete-p
	                                                     (%stream-tool-call-arguments-complete-p
	                                                      (%stream-tool-call-arguments tool-call))))))
	                 (when (hash-table-p event-usage)
	                   (stream-turn-apply-event! snapshot
	                                             (%make-stream-usage-delta-chunk event-usage))))))
      (loop for line = (read-line body-stream nil nil)
            while line do
              (let ((payload (%openai-compat-trim-sse-data line)))
                (when payload
                  (let ((trimmed (string-right-trim '(#\Space #\Tab #\Return) payload)))
                    (cond
                      ((string= trimmed "[DONE]")
                       (loop-finish))
                      ((plusp (length trimmed))
                       (handler-case
                           (consume-payload trimmed)
                         (error () nil))))))))
      (finalize-stream-turn-snapshot! snapshot)
      (multiple-value-bind (role content tool-calls usage)
          (stream-turn-snapshot-values snapshot)
        (values role content tool-calls usage snapshot)))))

(defun %openai-compat-coerce-message (m)
  "Coerce a message to hash-table format for OpenAI API."
  (%openai-normalize-message-for-payload m))

(defun %openai-compat-build-payload (provider messages &key model temperature max-tokens
                                                            top-p tools tool-choice
                                                            system-prompt stream-p
                                                            extra-params)
  "Build JSON payload for OpenAI-compatible chat completions."
  (let ((payload (make-hash-table :test #'equal)))
    (setf (gethash "model" payload) (or model (provider-default-model provider)))
    (let ((all-messages (mapcar #'%openai-compat-coerce-message
                                (%sanitize-tool-calls messages))))
      (when system-prompt
        (push (%make-raw-message "system" system-prompt) all-messages))
      (setf (gethash "messages" payload) all-messages))
    (when temperature
      (setf (gethash "temperature" payload) (coerce temperature 'double-float)))
    (when max-tokens
      (setf (gethash "max_tokens" payload) max-tokens))
    (when top-p
      (setf (gethash "top_p" payload) (coerce top-p 'double-float)))
    (when tools
      (setf (gethash "tools" payload)
            (mapcar (lambda (td)
                      (if (tool-definition-p td)
                          (tool-definition-to-hash td)
                          td))
                    tools)))
    (when tool-choice
      (setf (gethash "tool_choice" payload) tool-choice))
    (when stream-p
      (setf (gethash "stream" payload) t))
    ;; Merge any extra parameters
    (when (hash-table-p extra-params)
      (maphash (lambda (k v) (setf (gethash k payload) v)) extra-params))
    (jonathan:to-json payload)))

(defmethod send-chat-completion ((provider openai-compatible-provider) messages
                                 &key model temperature max-tokens top-p
                                      tools tool-choice system-prompt extra-params)
  (%provider-timed-call provider
    (lambda ()
      (let* ((payload (%openai-compat-build-payload provider messages
                                                     :model model
                                                     :temperature temperature
                                                     :max-tokens max-tokens
                                                     :top-p top-p
                                                     :tools tools
                                                     :tool-choice tool-choice
                                                     :system-prompt system-prompt
                                                     :extra-params extra-params))
             (url (%openai-compat-endpoint provider "/chat/completions"))
             (headers (%openai-compat-headers provider)))
        (multiple-value-bind (body status)
            (dex:request url
                         :method :post
                         :headers headers
                         :content payload
                         :read-timeout (provider-timeout-seconds provider)
                         :connect-timeout 30)
          (unless (<= 200 status 299)
            (error 'pseudopod-api-error
                   :message (format nil "OpenAI-compatible API error (status ~A)" status)
                   :status-code status
                   :body (if (stringp body) body (princ-to-string body))))
          (let ((text (cond ((stringp body) body)
                            ((streamp body)
                             (handler-case (uiop:slurp-stream-string body)
                               (error () "")))
                            (t (princ-to-string body)))))
            (jonathan:parse text :as :hash-table)))))))

(defmethod send-streaming-completion ((provider openai-compatible-provider) messages callback
                                      &key model temperature max-tokens top-p
                                           tools tool-choice system-prompt extra-params)
  "Send an OpenAI-compatible streaming completion request using SSE parsing."
  (%provider-timed-call provider
    (lambda ()
      (let* ((payload (%openai-compat-build-payload provider messages
                                                     :model model
                                                     :temperature temperature
                                                     :max-tokens max-tokens
                                                     :top-p top-p
                                                     :tools tools
                                                     :tool-choice tool-choice
                                                     :system-prompt system-prompt
                                                     :stream-p t
                                                     :extra-params extra-params))
             (url (%openai-compat-endpoint provider "/chat/completions"))
             (headers (%openai-compat-headers provider))
             (result nil))
        (multiple-value-bind (body-stream status)
            (dex:request url
                         :method :post
                         :headers headers
                         :content payload
                         :read-timeout (provider-timeout-seconds provider)
                         :connect-timeout 30
                         :want-stream t)
          (unless (<= 200 status 299)
            (let ((error-body (%coerce-response-body body-stream)))
              (handler-case (close body-stream)
                (error () nil))
              (%signal-http-status-error status error-body :streamp t)))
          (let ((snapshot nil))
            (unwind-protect
                (multiple-value-bind (parsed-role parsed-content tool-call-partials parsed-usage snap)
                    (%openai-compat-collect-stream body-stream callback)
                  (setf result
                        (%openai-compat-normalize-stream-result parsed-role
                                                              parsed-content
                                                              tool-call-partials
                                                              parsed-usage)
                        snapshot snap))
              (handler-case (close body-stream)
                (error () nil)))
            (values result snapshot)))))))

(defmethod list-provider-models ((provider openai-compatible-provider))
  "List models from /models endpoint."
  (handler-case
      (let* ((url (%openai-compat-endpoint provider "/models"))
             (headers (%openai-compat-headers provider)))
        (multiple-value-bind (body status)
            (dex:request url
                         :method :get
                         :headers headers
                         :read-timeout (provider-timeout-seconds provider)
                         :connect-timeout 30)
          (when (<= 200 status 299)
            (let* ((text (cond ((stringp body) body)
                               ((streamp body)
                               (handler-case (uiop:slurp-stream-string body)
                                  (error () nil)))
                               (t nil)))
                   (parsed (when text (jonathan:parse text :as :hash-table)))
                   (data (and (hash-table-p parsed) (gethash "data" parsed))))
              (cond
                ((listp data)
                 (mapcar (lambda (item)
                           (if (hash-table-p item)
                               (hash-to-model-info item)
                               item))
                         data))
                ((vectorp data)
                 (map 'list
                      (lambda (item)
                        (if (hash-table-p item)
                            (hash-to-model-info item)
                            item))
                      data))
                (t
                 nil))))))
    (error () nil)))

(defmethod estimate-provider-tokens ((provider openai-compatible-provider) text)
  "OpenAI-compatible providers use a default rough ~4 chars/token estimate."
  (ceiling (length text) 4))
