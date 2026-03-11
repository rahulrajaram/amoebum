(in-package :pseudopod/test)

(def-suite openai-compat-suite :in pseudopod-suite
  :description "OpenAI-compatible backend presets and discovery tests (I244).")

(in-suite openai-compat-suite)

(defmacro with-fake-env (pairs &body body)
  `(let ((original-getenv (symbol-function 'uiop:getenv)))
     (unwind-protect
         (let ((pairs ',pairs))
           (setf (symbol-function 'uiop:getenv)
                 (lambda (name)
                   (let ((key (if (symbolp name)
                                  (string name)
                                  (princ-to-string name))))
                     (if (assoc key pairs :test #'string=)
                         (cdr (assoc key pairs :test #'string=))
                         (funcall original-getenv name)))))
           ,@body)
       (setf (symbol-function 'uiop:getenv) original-getenv))))

(defmacro with-temp-provider-config ((path-var form) &body body)
  `(let* ((,path-var
            (merge-pathnames
             (format nil "pseudopod-openai-compat-~D-~D.sexp"
                     (get-universal-time)
                     (random 1000000))
             (uiop:temporary-directory))))
     (unwind-protect
         (progn
           (with-open-file (stream ,path-var
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (let ((*print-circle* nil)
                   (*print-pretty* t))
               (prin1 ,form stream)))
           ,@body)
       (ignore-errors (delete-file ,path-var)))))

(defun %find-provider (name providers)
  (find name providers
        :key #'pseudopod:provider-name
        :test #'string=))

(defun %make-model-hash (id)
  (let ((hash (make-hash-table :test #'equal)))
    (setf (gethash "id" hash) id
          (gethash "object" hash) "model"
          (gethash "owned_by" hash) "test")
    hash))

(test openai-compat-backend-presets
  (with-fake-env (("OPENROUTER_BASE_URL" . "")
                  ("OPENROUTER_MODEL" . "")
                  ("TOGETHER_BASE_URL" . "")
                  ("TOGETHER_MODEL" . "")
                  ("VLLM_BASE_URL" . "")
                  ("VLLM_MODEL" . "")
                  ("OLLAMA_BASE_URL" . "")
                  ("OLLAMA_HOST" . "")
                  ("OLLAMA_MODEL" . ""))
    (let ((openrouter (pseudopod:make-openai-compatible-provider
                       :backend :openrouter
                       :api-key "sk-openrouter"))
          (together (pseudopod:make-openai-compatible-provider
                     :backend :together
                     :api-key "sk-together"))
          (vllm (pseudopod:make-openai-compatible-provider
                 :backend :vllm
                 :api-key "sk-vllm"))
          (ollama (pseudopod:make-openai-compatible-provider
                   :backend :ollama
                   :api-key "")))
      (is (eql :openrouter (pseudopod:openai-compat-backend openrouter)))
      (is (string= "https://openrouter.ai/api/v1"
                   (pseudopod:provider-base-url openrouter)))
      (is (string= "openai/gpt-4o-mini"
                   (pseudopod:provider-default-model openrouter)))
      (is (eql :together (pseudopod:openai-compat-backend together)))
      (is (string= "https://api.together.xyz/v1"
                   (pseudopod:provider-base-url together)))
      (is (eql :vllm (pseudopod:openai-compat-backend vllm)))
      (is (string= "http://localhost:8000/v1"
                   (pseudopod:provider-base-url vllm)))
      (is (eql :ollama (pseudopod:openai-compat-backend ollama)))
      (is (string= "http://localhost:11434/v1"
                   (pseudopod:provider-base-url ollama))))))

(test openai-compat-env-discovery
  (with-fake-env (("OPENAI_API_KEY" . "")
                  ("OPENAI_BASE_URL" . "")
                  ("OPENAI_MODEL" . "")
                  ("OPENROUTER_API_KEY" . "sk-openrouter")
                  ("OPENROUTER_BASE_URL" . "")
                  ("OPENROUTER_MODEL" . "")
                  ("TOGETHER_API_KEY" . "sk-together")
                  ("TOGETHER_BASE_URL" . "")
                  ("TOGETHER_MODEL" . "")
                  ("VLLM_BASE_URL" . "http://127.0.0.1:9000/v1")
                  ("VLLM_API_KEY" . "")
                  ("VLLM_MODEL" . "qwen2.5-coder")
                  ("OLLAMA_BASE_URL" . "")
                  ("OLLAMA_API_KEY" . "")
                  ("OLLAMA_HOST" . "http://127.0.0.1:11434"))
    (let* ((providers (pseudopod:list-providers :config-paths nil))
           (openrouter (%find-provider "openrouter" providers))
           (together (%find-provider "together" providers))
           (vllm (%find-provider "vllm" providers))
           (ollama (%find-provider "ollama" providers)))
      (is (null (%find-provider "openai" providers)))
      (is-true openrouter)
      (is (string= "openai/gpt-4o-mini"
                   (pseudopod:provider-default-model openrouter)))
      (is-true together)
      (is-true vllm)
      (is (string= "http://127.0.0.1:9000/v1"
                   (pseudopod:provider-base-url vllm)))
      (is (string= "qwen2.5-coder"
                   (pseudopod:provider-default-model vllm)))
      (is-true ollama)
      (is (string= "http://127.0.0.1:11434/v1"
                   (pseudopod:provider-base-url ollama))))))

(test openai-compat-config-discovery-overrides-env
  (with-fake-env (("OPENAI_API_KEY" . "")
                  ("OPENAI_BASE_URL" . "")
                  ("OPENAI_MODEL" . "")
                  ("OPENROUTER_API_KEY" . "sk-env-openrouter")
                  ("OPENROUTER_BASE_URL" . "")
                  ("OPENROUTER_MODEL" . "env-model"))
    (with-temp-provider-config
        (config-file
         '((:name "openrouter"
            :backend :openrouter
            :api-key "sk-config-openrouter"
            :base-url "https://router.internal/v1"
            :model "config-model")
           (:name "local-ollama"
            :backend :ollama
            :base-url "http://localhost:11555"
            :model "llama3.3")))
      (let* ((providers (pseudopod:list-providers :config-paths (list config-file)))
             (openrouter (%find-provider "openrouter" providers))
             (local-ollama (%find-provider "local-ollama" providers)))
        (is-true openrouter)
        (is (string= "https://router.internal/v1"
                     (pseudopod:provider-base-url openrouter)))
        (is (string= "config-model"
                     (pseudopod:provider-default-model openrouter)))
        (is (string= "sk-config-openrouter"
                     (pseudopod:provider-api-key openrouter)))
        (is-true local-ollama)
        (is (string= "http://localhost:11555"
                     (pseudopod:provider-base-url local-ollama)))))))

(defclass openai-compat-model-test-provider (pseudopod:provider) ())

(defmethod pseudopod:list-provider-models ((provider openai-compat-model-test-provider))
  (declare (ignore provider))
  (list (pseudopod:hash-to-model-info (%make-model-hash "test-model-1"))
        (pseudopod:hash-to-model-info (%make-model-hash "test-model-2"))))

(test provider-models-resolves-instance-and-name
  (let* ((provider (make-instance 'openai-compat-model-test-provider
                                  :name "test-provider"
                                  :api-key "sk-test"
                                  :base-url "https://example.test/v1"
                                  :default-model "test-model-1"))
         (by-instance (pseudopod:provider-models provider))
         (by-name (pseudopod:provider-models "test-provider"
                                             :providers (list provider))))
    (is (= 2 (length by-instance)))
    (is (= 2 (length by-name)))
    (is (every #'pseudopod:model-info-p by-instance))
    (is (string= "test-model-1"
                 (pseudopod:model-info-id (first by-name))))))

(test openai-compat-smoke-sentinel
  (is-true t)
  (format t "OPENAI_COMPAT_SMOKE_OK~%"))
