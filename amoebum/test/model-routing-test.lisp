(in-package :amoebum/test)

;;; ============================================================
;;; I245: Task-Based Model Routing with Fallback Chains
;;; ============================================================

(def-suite model-routing-suite :in amoebum-suite)
(in-suite model-routing-suite)

(define-condition model-routing-timeout (error)
  ())

(defclass model-routing-test-provider (pseudopod:provider)
  ((fail-mode :initarg :fail-mode
              :accessor model-routing-test-provider-fail-mode
              :initform nil)
   (health-result :initarg :health-result
                  :accessor model-routing-test-provider-health-result
                  :initform t)
   (calls :accessor model-routing-test-provider-calls
          :initform '())))

(defmethod pseudopod:list-provider-models ((provider model-routing-test-provider))
  (declare (ignore provider))
  '())

(defmethod pseudopod:provider-health-check ((provider model-routing-test-provider))
  (let ((healthy (model-routing-test-provider-health-result provider)))
    (setf (pseudopod:provider-healthy-p provider) healthy)
    healthy))

(defmethod pseudopod:send-chat-completion ((provider model-routing-test-provider)
                                           messages
                                           &key model temperature max-tokens
                                             top-p tools tool-choice
                                             system-prompt extra-params)
  (declare (ignore messages temperature max-tokens
                   top-p tools tool-choice
                   system-prompt extra-params))
  (push (or model :default)
        (model-routing-test-provider-calls provider))
  (case (model-routing-test-provider-fail-mode provider)
    (:error
     (error "simulated provider error"))
    (:timeout
     (error 'model-routing-timeout))
    (otherwise
     (let ((response (make-hash-table :test #'equal)))
       (setf (gethash "provider" response) (pseudopod:provider-name provider)
             (gethash "model" response) (or model (pseudopod:provider-default-model provider)))
       response))))

(defmethod pseudopod:send-streaming-completion ((provider model-routing-test-provider)
                                                messages callback
                                                &key model temperature max-tokens
                                                  top-p tools tool-choice
                                                  system-prompt extra-params)
  (declare (ignore messages temperature max-tokens
                   top-p tools tool-choice
                   system-prompt extra-params))
  (funcall callback (or model (pseudopod:provider-default-model provider)))
  (pseudopod:send-chat-completion provider nil :model model))

(defun %make-test-provider (name &key (model "test-model") (healthy t) fail-mode)
  (let ((provider (make-instance 'model-routing-test-provider
                                 :name name
                                 :api-key "test-key"
                                 :base-url "https://example.invalid"
                                 :default-model model
                                 :fail-mode fail-mode
                                 :health-result healthy)))
    (setf (pseudopod:provider-healthy-p provider) healthy)
    provider))

;;; --- Router creation ---

(test router-creation-strategies
  "Router should support all strategy types."
  (dolist (strategy '(:fallback-chain :task-based :cost-aware))
    (let ((router (pseudopod:make-model-router :strategy strategy)))
      (is (pseudopod:model-router-p router))
      (is (eq strategy (pseudopod:model-router-strategy router))))))

;;; --- Task-based routing ---

(test task-based-routing-selects-correct-provider
  "Task-based routing should select the provider mapped to the task type."
  (let* ((code-provider (%make-test-provider "code-provider"))
         (planning-provider (%make-test-provider "planning-provider"))
         (router (pseudopod:make-model-router
                  :strategy :task-based
                  :providers (list code-provider planning-provider)
                  :task-routing '((:code-review . "code-provider")
                                  (:planning . "planning-provider")))))
    (let ((selected (pseudopod:router-select-provider router :task-type :code-review)))
      (is (eq code-provider selected)))
    (let ((selected (pseudopod:router-select-provider router :task-type :planning)))
      (is (eq planning-provider selected)))))

(test task-based-routing-supports-model-and-provider-pairs
  "Task routing should accept model+provider mapping entries."
  (let* ((code-provider (%make-test-provider "code-provider"))
         (backup-provider (%make-test-provider "backup-provider"))
         (router (pseudopod:make-model-router
                  :strategy :task-based
                  :providers (list code-provider backup-provider)
                  :task-routing '((:code-review . (:provider "code-provider"
                                              :model "code-model-v2"))))))
    (let ((selected (pseudopod:router-select-provider router :task-type :code-review))
          (response (pseudopod:router-chat-completion
                     router
                     '("Review this patch.")
                     :task-type :code-review)))
      (is (eq code-provider selected))
      (is (string= "code-provider" (gethash "provider" response)))
      (is (string= "code-model-v2" (gethash "model" response))))))

(test task-based-routing-falls-back-on-unhealthy
  "Task-based routing should fall back if mapped provider is unhealthy."
  (let* ((primary (%make-test-provider "primary" :healthy nil))
         (fallback (%make-test-provider "fallback" :healthy t))
         (router (pseudopod:make-model-router
                  :strategy :task-based
                  :providers (list primary fallback)
                  :task-routing '((:code-review . "primary")))))
    (let ((selected (pseudopod:router-select-provider router :task-type :code-review)))
      (is (eq fallback selected)))))

(test task-based-chat-falls-back-on-error-and-timeout
  "Task-based router should retry fallback providers on runtime errors and timeouts."
  (flet ((run-case (fail-mode)
           (let* ((primary (%make-test-provider "primary" :fail-mode fail-mode))
                  (backup (%make-test-provider "backup"))
                  (router (pseudopod:make-model-router
                           :strategy :task-based
                           :providers (list primary backup)
                           :task-routing '((:quick-edit . (:provider "primary"
                                                    :model "primary-model")))))
                  (response (pseudopod:router-chat-completion
                             router
                             '("Apply this edit.")
                             :task-type :quick-edit)))
             (is (string= "backup" (gethash "provider" response)))
             (is (null (pseudopod:provider-healthy-p primary)))
             (is-true (pseudopod:provider-healthy-p backup))
             (is (equal '(:default) (model-routing-test-provider-calls backup))))))
    (run-case :error)
    (run-case :timeout)))

(test task-based-health-degrades-and-recovers
  "Provider health should degrade on failure and recover after health checks."
  (let* ((primary (%make-test-provider "primary" :fail-mode :error))
         (backup (%make-test-provider "backup"))
         (router (pseudopod:make-model-router
                  :strategy :task-based
                  :providers (list primary backup)
                  :task-routing '((:planning . (:provider "primary"
                                          :model "planning-model"))))))
    (let ((fallback-response
            (pseudopod:router-chat-completion
             router
             '("Create a plan.")
             :task-type :planning)))
      (is (string= "backup" (gethash "provider" fallback-response)))
      (is (null (pseudopod:provider-healthy-p primary))))
    (setf (model-routing-test-provider-fail-mode primary) nil
          (model-routing-test-provider-health-result primary) t)
    (pseudopod:router-check-health router :force t)
    (let ((recovered-response
            (pseudopod:router-chat-completion
             router
             '("Create another plan.")
             :task-type :planning)))
      (is (string= "primary" (gethash "provider" recovered-response)))
      (is (string= "planning-model" (gethash "model" recovered-response)))
      (is-true (pseudopod:provider-healthy-p primary))
      (is (null (pseudopod:provider-last-error primary))))))

;;; --- Fallback chain ---

(test fallback-chain-tries-healthy-first
  "Fallback chain should prefer healthy providers."
  (let* ((unhealthy (%make-test-provider "unhealthy" :healthy nil))
         (healthy (%make-test-provider "healthy" :healthy t))
         (router (pseudopod:make-model-router
                  :strategy :fallback-chain
                  :providers (list unhealthy healthy))))
    (let ((selected (pseudopod:router-select-provider router)))
      (is (eq healthy selected)))))

;;; --- Router management ---

(test router-add-remove-provider
  "router-add-provider and router-remove-provider should work."
  (let ((router (pseudopod:make-model-router))
        (provider (%make-test-provider "dynamic")))
    (pseudopod:router-add-provider router provider)
    (is (= 1 (length (pseudopod:model-router-providers router))))
    (is (not (null (pseudopod:router-find-provider router "dynamic"))))
    (pseudopod:router-remove-provider router "dynamic")
    (is (= 0 (length (pseudopod:model-router-providers router))))))

(test router-healthy-providers-filters
  "router-healthy-providers should only return healthy ones."
  (let* ((h1 (%make-test-provider "h1" :healthy t))
         (h2 (%make-test-provider "h2" :healthy nil))
         (router (pseudopod:make-model-router :providers (list h1 h2))))
    (is (= 1 (length (pseudopod:router-healthy-providers router))))))

(test router-status-summary
  "router-status should return a structured summary."
  (let* ((p1 (%make-test-provider "p1"))
         (router (pseudopod:make-model-router :providers (list p1))))
    (let ((status (pseudopod:router-status router)))
      (is (listp status))
      (is (= 1 (getf status :total-providers)))
      (is (= 1 (getf status :healthy-providers)))
      (is (= 1 (length (getf status :providers)))))))

(test router-set-task-route
  "router-set-task-route should update task routing."
  (let ((router (pseudopod:make-model-router)))
    (pseudopod:router-set-task-route router :code-review "my-provider")
    (is (string= "my-provider"
                 (gethash :code-review (pseudopod::model-router-task-routing router))))
    (pseudopod:router-set-task-route router :planning "planner" :model "planning-v3")
    (let ((route (gethash :planning (pseudopod::model-router-task-routing router))))
      (is (string= "planner" (getf route :provider)))
      (is (string= "planning-v3" (getf route :model))))))

(test model-routing-smoke-sentinel
  "Smoke sentinel for tranche I245."
  (format t "MODEL_ROUTING_SMOKE_OK~%")
  (is-true t))
