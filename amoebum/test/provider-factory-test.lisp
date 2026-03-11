(in-package :amoebum/test)

(def-suite provider-factory-suite
  :description "Provider factory behavior and cache invalidation tests (I135)."
  :in amoebum-suite)

(in-suite provider-factory-suite)

(defmacro with-fake-env (pairs &body body)
  `(let ((original-getenv (symbol-function 'uiop:getenv)))
     (unwind-protect
         (let ((pairs ',pairs))
           (setf (symbol-function 'uiop:getenv)
                 (lambda (name)
                   (let ((key (if (symbolp name)
                                  (string name)
                                  (princ-to-string name))))
                     (or (cdr (assoc key pairs :test #'string=))
                         (funcall original-getenv name)))))
           ,@body)
       (setf (symbol-function 'uiop:getenv) original-getenv))))

(defmacro with-provider-config (options &body body)
  (let ((model (getf options :model))
        (provider-override (getf options :provider-override))
        (api-base-url (getf options :api-base-url)))
    `(let* ((old-config (amoebum:current-config))
            (old-model (amoebum:config-model old-config))
            (old-provider-override (amoebum:config-value :provider-override old-config))
            (old-api-base-url (amoebum:config-value :api-base-url old-config)))
       (unwind-protect
            (progn
              (amoebum:reload-config
               :environment-values (list :model ,model
                                        :provider-override ,provider-override
                                        :api-base-url ,api-base-url))
              (amoebum:clear-resolved-provider-cache)
              ,@body)
         (amoebum:reload-config
          :environment-values (list :model old-model
                                   :provider-override old-provider-override
                                   :api-base-url old-api-base-url))
         (amoebum:clear-resolved-provider-cache)))))

(test provider-factory-resolves-known-model-prefixes
  (with-provider-config (:model "claude-3-5-sonnet"
                               :provider-override nil
                               :api-base-url nil)
    (is (typep (amoebum:resolve-provider) 'pseudopod:anthropic-provider)))
  (with-provider-config (:model "gpt-4o"
                               :provider-override nil
                               :api-base-url nil)
    (is (typep (amoebum:resolve-provider) 'pseudopod:openai-compatible-provider)))
  (with-provider-config (:model "moonshot-v1-128k"
                               :provider-override nil
                               :api-base-url nil)
    (is (typep (amoebum:resolve-provider) 'pseudopod:kimi-provider))))

(test provider-factory-resolves-api-keys-from-environment
  (with-fake-env (("ANTHROPIC_API_KEY" . "anthropic-api-key")
                 ("OPENAI_API_KEY" . "openai-api-key")
                 ("MOONSHOT_API_KEY" . "moonshot-api-key"))
    (with-provider-config (:model "claude-3-5-sonnet"
                                 :provider-override nil
                                 :api-base-url nil)
      (is (string= "anthropic-api-key"
                   (pseudopod:provider-api-key (amoebum:resolve-provider)))))
    (with-provider-config (:model "gpt-4o"
                                 :provider-override nil
                                 :api-base-url nil)
      (is (string= "openai-api-key"
                   (pseudopod:provider-api-key (amoebum:resolve-provider)))))
    (with-provider-config (:model "moonshot-v1-128k"
                                 :provider-override nil
                                 :api-base-url nil)
      (is (string= "moonshot-api-key"
                   (pseudopod:provider-api-key (amoebum:resolve-provider)))))))

(test provider-factory-provider-override-and-openai-base-url
  (with-provider-config (:model "gpt-4o"
                               :provider-override :anthropic-provider
                               :api-base-url nil)
    (is (typep (amoebum:resolve-provider) 'pseudopod:anthropic-provider))
    (is (not (typep (amoebum:resolve-provider) 'pseudopod:openai-compatible-provider))))
  (with-provider-config (:model "gpt-4o"
                               :provider-override :openai-compatible-provider
                               :api-base-url "https://localhost:11434/v1/")
    (let ((provider (amoebum:resolve-provider)))
      (is (typep provider 'pseudopod:openai-compatible-provider))
      (is (string= "https://localhost:11434/v1"
                   (pseudopod:provider-base-url provider))))))


(test provider-factory-caches-and-invalidates-on-reload
  (with-provider-config (:model "claude-3-5-sonnet"
                               :provider-override nil
                               :api-base-url nil)
    (let ((first (amoebum:resolve-provider)))
      (is (eq first (amoebum:resolve-provider)))
      (amoebum:reload-config :environment-values '(:model "claude-3-5-sonnet"))
      (is (not (eq first (amoebum:resolve-provider)))))))

(test provider-factory-invalidates-cache-on-setconfig
  (with-provider-config (:model "claude-3-5-sonnet"
                               :provider-override nil
                               :api-base-url nil)
    (let ((first (amoebum:resolve-provider)))
      (is (typep first 'pseudopod:anthropic-provider))
      (amoebum:setconfig :model "gpt-4o")
      (is (not (eq first (amoebum:resolve-provider)))))
    (is (typep (amoebum:resolve-provider) 'pseudopod:openai-compatible-provider))))

(test provider-factory-invalid-provider-override-errors
  (with-provider-config (:model "gpt-4o"
                               :provider-override nil
                               :api-base-url nil)
    (signals error
      (amoebum:setconfig :provider-override "definitely-invalid-provider"))))

(test provider-factory-agent-scoped-provider-key-isolation
  (let ((registry (sw4rm-sdk:make-local-registry)))
    (let ((amoebum::*user-session-registry* registry))
      (sw4rm-sdk:local-registry-register
       registry
       (sw4rm-sdk:make-agent-config :agent-id "agent-a" :name "Agent A"))
      (sw4rm-sdk:local-registry-register
       registry
       (sw4rm-sdk:make-agent-config :agent-id "agent-b" :name "Agent B"))
      (sw4rm-sdk:local-registry-set-provider-secret
       registry "agent-a" "OPENAI_API_KEY" "openai-agent-a")
      (sw4rm-sdk:local-registry-set-provider-secret
       registry "agent-b" "OPENAI_API_KEY" "openai-agent-b")
      (with-fake-env (("OPENAI_API_KEY" . "openai-global"))
        (with-provider-config (:model "gpt-4o"
                                     :provider-override nil
                                     :api-base-url nil)
          (let ((provider-a (amoebum:resolve-provider (amoebum:current-config)
                                                      :agent-id "agent-a"))
                (provider-b (amoebum:resolve-provider (amoebum:current-config)
                                                      :agent-id "agent-b"))
                (provider-global (amoebum:resolve-provider)))
            (is (typep provider-a 'pseudopod:openai-compatible-provider))
            (is (typep provider-b 'pseudopod:openai-compatible-provider))
            (is (not (eq provider-a provider-b)))
            (is (string= "openai-agent-a" (pseudopod:provider-api-key provider-a)))
            (is (string= "openai-agent-b" (pseudopod:provider-api-key provider-b)))
            (is (string= "openai-global" (pseudopod:provider-api-key provider-global)))))))))
