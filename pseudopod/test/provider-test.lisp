(in-package :pseudopod/test)

;;; ---------------------------------------------------------------------------
;;; Provider Protocol Tests (I94)
;;; ---------------------------------------------------------------------------

(def-suite provider-suite :in pseudopod-suite
  :description "Multi-model provider protocol tests.")

(in-suite provider-suite)

;;; --- Protocol class ---

(test provider-base-class-slots
  (let ((p (make-instance 'pseudopod:provider
                          :name "test-provider"
                          :api-key "sk-test"
                          :base-url "https://example.com"
                          :default-model "test-model")))
    (is (string= "test-provider" (pseudopod:provider-name p)))
    (is (string= "sk-test" (pseudopod:provider-api-key p)))
    (is (string= "https://example.com" (pseudopod:provider-base-url p)))
    (is (string= "test-model" (pseudopod:provider-default-model p)))
    (is (= 180 (pseudopod:provider-timeout-seconds p)))
    (is (eq t (pseudopod:provider-healthy-p p)))
    (is (null (pseudopod:provider-last-error p)))
    (is (= 0 (pseudopod:provider-request-count p)))
    (is (= 0 (pseudopod:provider-error-count p)))))

(test provider-metrics-tracking
  (let ((p (make-instance 'pseudopod:provider
                          :name "metrics-test"
                          :api-key "sk-test"
                          :base-url "https://example.com"
                          :default-model "test-model")))
    (pseudopod:provider-record-request p 100 nil)
    (is (= 1 (pseudopod:provider-request-count p)))
    (is (= 100 (pseudopod:provider-last-latency-ms p)))
    (is (= 0 (pseudopod:provider-error-count p)))
    (is (< (abs (pseudopod:provider-error-rate p)) 0.001))
    (pseudopod:provider-record-request p 200 t)
    (is (= 2 (pseudopod:provider-request-count p)))
    (is (= 200 (pseudopod:provider-last-latency-ms p)))
    (is (= 1 (pseudopod:provider-error-count p)))
    (is (< (abs (- (pseudopod:provider-error-rate p) 0.5)) 0.001))))

(test provider-default-token-estimate
  (let ((p (make-instance 'pseudopod:provider
                          :name "est-test"
                          :api-key "sk-test"
                          :base-url "https://example.com"
                          :default-model "test-model")))
    (let ((est (pseudopod:estimate-provider-tokens p "Hello world test")))
      (is (integerp est))
      (is (> est 0)))))

(test provider-with-provider-macro
  (is (null (pseudopod:current-provider)))
  (let ((p (make-instance 'pseudopod:provider
                          :name "macro-test"
                          :api-key "sk-test"
                          :base-url "https://example.com"
                          :default-model "test-model")))
    (pseudopod:with-provider (p)
      (is (eq p (pseudopod:current-provider))))
    (is (null (pseudopod:current-provider)))))

;;; --- Kimi Provider ---

(test kimi-provider-creation
  (let ((p (pseudopod:make-kimi-provider :api-key "sk-kimi-test"
                                          :model "kimi-k2.5")))
    (is (string= "kimi" (pseudopod:provider-name p)))
    (is (string= "sk-kimi-test" (pseudopod:provider-api-key p)))
    (is (string= "kimi-k2.5" (pseudopod:provider-default-model p)))
    (is (typep p 'pseudopod:kimi-provider))))

(test kimi-provider-wraps-existing-client
  (let* ((client (handler-case
                     (pseudopod:make-client :api-key "sk-wrap-test")
                   (error () nil)))
         (p (when client
              (pseudopod:make-kimi-provider :client client))))
    (when p
      (is (eq client (pseudopod:kimi-provider-client p)))
      (is (string= "kimi" (pseudopod:provider-name p))))))

;;; --- Anthropic Provider ---

(test anthropic-provider-creation
  (let ((p (pseudopod:make-anthropic-provider :api-key "sk-ant-test"
                                               :model "claude-sonnet-4-5-20250929")))
    (is (string= "anthropic" (pseudopod:provider-name p)))
    (is (string= "sk-ant-test" (pseudopod:provider-api-key p)))
    (is (string= "claude-sonnet-4-5-20250929" (pseudopod:provider-default-model p)))
    (is (typep p 'pseudopod:anthropic-provider))))

(test anthropic-provider-lists-known-models
  (let* ((p (pseudopod:make-anthropic-provider :api-key "sk-ant-test"))
         (models (pseudopod:list-provider-models p)))
    (is (listp models))
    (is (>= (length models) 3))
    (is (every #'pseudopod:model-info-p models))
    (is (find "claude-opus-4-6" models
              :key #'pseudopod:model-info-id :test #'string=))))

(test anthropic-provider-api-version
  (let ((p (pseudopod:make-anthropic-provider :api-key "sk-test"
                                               :api-version "2024-01-01")))
    (is (string= "2024-01-01" (pseudopod:anthropic-provider-api-version p)))))

;;; --- OpenAI-Compatible Provider ---

(test openai-compat-provider-creation
  (let ((p (pseudopod:make-openai-compatible-provider :api-key "sk-oai-test"
                                                       :model "gpt-4o")))
    (is (string= "openai" (pseudopod:provider-name p)))
    (is (string= "sk-oai-test" (pseudopod:provider-api-key p)))
    (is (string= "gpt-4o" (pseudopod:provider-default-model p)))
    (is (typep p 'pseudopod:openai-compatible-provider))))

(test openai-compat-custom-name
  (let ((p (pseudopod:make-openai-compatible-provider :api-key "sk-test"
                                                       :name "groq"
                                                       :base-url "https://api.groq.com/openai/v1"
                                                       :model "llama-3.3-70b")))
    (is (string= "groq" (pseudopod:provider-name p)))
    (is (string= "llama-3.3-70b" (pseudopod:provider-default-model p)))))

(test openai-compat-ollama
  (let ((p (pseudopod:make-openai-compatible-provider :api-key ""
                                                       :name "ollama"
                                                       :base-url "http://localhost:11434/v1"
                                                       :model "llama3")))
    (is (string= "ollama" (pseudopod:provider-name p)))
    (is (string= "http://localhost:11434/v1" (pseudopod:provider-base-url p)))))

(test openai-compat-organization
  (let ((p (pseudopod:make-openai-compatible-provider :api-key "sk-test"
                                                       :organization "org-test")))
    (is (string= "org-test" (pseudopod:openai-compat-organization p)))))

;;; --- Cross-provider generic dispatch ---

(test provider-generic-dispatch
  "Test that generic functions dispatch correctly across provider types."
  (let ((providers (list (pseudopod:make-kimi-provider :api-key "sk-test1")
                         (pseudopod:make-anthropic-provider :api-key "sk-test2")
                         (pseudopod:make-openai-compatible-provider :api-key "sk-test3"))))
    (dolist (p providers)
      (is (stringp (pseudopod:provider-name p)))
      (is (integerp (pseudopod:estimate-provider-tokens p "Hello world"))))))

(test provider-timed-call-tracks-success
  "Test that %provider-timed-call tracks successful requests."
  (let ((p (make-instance 'pseudopod:provider
                          :name "timer-test"
                          :api-key "sk-test"
                          :base-url "https://example.com"
                          :default-model "test")))
    (pseudopod::%provider-timed-call p (lambda () 42))
    (is (= 1 (pseudopod:provider-request-count p)))
    (is (= 0 (pseudopod:provider-error-count p)))))

(test provider-timed-call-tracks-error
  "Test that %provider-timed-call tracks errors."
  (let ((p (make-instance 'pseudopod:provider
                          :name "timer-test"
                          :api-key "sk-test"
                          :base-url "https://example.com"
                          :default-model "test")))
    (handler-case
        (pseudopod::%provider-timed-call p (lambda () (error "boom")))
      (error ()))
    (is (= 1 (pseudopod:provider-request-count p)))
    (is (= 1 (pseudopod:provider-error-count p)))))
