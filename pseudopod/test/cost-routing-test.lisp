(in-package :pseudopod/test)

(def-suite cost-routing-suite :in pseudopod-suite
  :description "Cost-aware routing and estimation tests (I229).")

(in-suite cost-routing-suite)

(defun make-cost-test-provider (name model &key (healthy t))
  (let ((provider (make-instance 'pseudopod:provider
                                 :name name
                                 :api-key "sk-test"
                                 :base-url "https://example.test"
                                 :default-model model)))
    (setf (pseudopod:provider-healthy-p provider) healthy)
    provider))

(test cost-estimate-uses-model-rates
  (let* ((provider (make-cost-test-provider "kimi" "kimi-k2.5"))
         (messages (list (pseudopod:make-message :role "user"
                                                 :content "12345678")))
         (estimate (pseudopod:cost-estimate provider messages :output-tokens 1000)))
    ;; The routed message text includes role/content framing and rounds up.
    (is (= 3 (getf estimate :input-tokens)))
    (is (= 1000 (getf estimate :output-tokens)))
    (is (< 0.0d0 (getf estimate :total-cost-usd)))))

(test cost-aware-routing-picks-cheapest-healthy-provider
  (let* ((router (pseudopod:make-model-router :strategy :cost-aware))
         (haiku (make-cost-test-provider "anthropic-haiku" "claude-haiku"))
         (gpt4o-mini (make-cost-test-provider "openai-mini" "gpt-4o-mini"))
         (gpt4o (make-cost-test-provider "openai" "gpt-4o")))
    (pseudopod:router-add-provider router gpt4o)
    (pseudopod:router-add-provider router gpt4o-mini)
    (pseudopod:router-add-provider router haiku)
    (let ((selected (pseudopod:router-select-provider
                     router
                     :messages (list (pseudopod:make-message :role "user"
                                                             :content "route this cheaply"))
                     :expected-output-tokens 500)))
      (is (string= "openai-mini" (pseudopod:provider-name selected))))))

(test cost-aware-routing-respects-minimum-context-window
  (let* ((router (pseudopod:make-model-router :strategy :cost-aware))
         (haiku (make-cost-test-provider "anthropic-haiku" "claude-haiku"))
         (gpt4o-mini (make-cost-test-provider "openai-mini" "gpt-4o-mini")))
    (pseudopod:router-add-provider router gpt4o-mini)
    (pseudopod:router-add-provider router haiku)
    (let ((selected (pseudopod:router-select-provider
                     router
                     :minimum-context-window 160000
                     :messages (list (pseudopod:make-message :role "user"
                                                             :content "long context request")))))
      ;; gpt-4o-mini default context (128k) is below requirement, haiku (200k) remains.
      (is (string= "anthropic-haiku" (pseudopod:provider-name selected))))))

(test cost-aware-routing-returns-nil-when-window-unmet
  (let* ((router (pseudopod:make-model-router :strategy :cost-aware))
         (mini (make-cost-test-provider "openai-mini" "gpt-4o-mini")))
    (pseudopod:router-add-provider router mini)
    (is (null (pseudopod:router-select-provider router :minimum-context-window 999999)))))

(test cost-routing-smoke-sentinel
  (is-true t)
  (format t "COST_ROUTING_SMOKE_OK~%"))
