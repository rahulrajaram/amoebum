(in-package :amoebum/test)

;;; ============================================================
;;; I245: Task-Based Model Routing with Fallback Chains
;;; ============================================================

(def-suite model-routing-suite :in amoebum-suite)
(in-suite model-routing-suite)

(defun %make-test-provider (name &key (model "test-model") (healthy t))
  (let ((provider (make-instance 'pseudopod::kimi-provider
                   :api-key "test-key"
                   :name name
                   :default-model model)))
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
                  (gethash :code-review (pseudopod::model-router-task-routing router))))))
