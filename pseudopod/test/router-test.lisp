(in-package :pseudopod/test)

;;; ---------------------------------------------------------------------------
;;; Model Router Tests (I95)
;;; ---------------------------------------------------------------------------

(def-suite router-suite :in pseudopod-suite
  :description "Model router and fallback chain tests.")

(in-suite router-suite)

(defun make-test-provider (name &key (healthy t))
  (let ((p (make-instance 'pseudopod:provider
                          :name name
                          :api-key "sk-test"
                          :base-url "https://example.com"
                          :default-model (format nil "~A-model" name))))
    (setf (pseudopod:provider-healthy-p p) healthy)
    p))

(test router-creation
  (let ((r (pseudopod:make-model-router :strategy :fallback-chain)))
    (is (pseudopod:model-router-p r))
    (is (eq :fallback-chain (pseudopod:model-router-strategy r)))
    (is (null (pseudopod:model-router-providers r)))))

(test router-add-remove-provider
  (let ((r (pseudopod:make-model-router))
        (p1 (make-test-provider "alpha"))
        (p2 (make-test-provider "beta")))
    (pseudopod:router-add-provider r p1)
    (pseudopod:router-add-provider r p2)
    (is (= 2 (length (pseudopod:model-router-providers r))))
    (pseudopod:router-remove-provider r "alpha")
    (is (= 1 (length (pseudopod:model-router-providers r))))
    (is (string= "beta" (pseudopod:provider-name
                          (first (pseudopod:model-router-providers r)))))))

(test router-find-provider
  (let ((r (pseudopod:make-model-router))
        (p1 (make-test-provider "primary")))
    (pseudopod:router-add-provider r p1)
    (is (eq p1 (pseudopod:router-find-provider r "primary")))
    (is (null (pseudopod:router-find-provider r "nonexistent")))))

(test router-healthy-providers
  (let ((r (pseudopod:make-model-router))
        (p1 (make-test-provider "healthy1"))
        (p2 (make-test-provider "sick" :healthy nil))
        (p3 (make-test-provider "healthy2")))
    (pseudopod:router-add-provider r p1)
    (pseudopod:router-add-provider r p2)
    (pseudopod:router-add-provider r p3)
    (is (= 2 (length (pseudopod:router-healthy-providers r))))))

(test router-fallback-chain-strategy
  (let ((r (pseudopod:make-model-router :strategy :fallback-chain))
        (p1 (make-test-provider "primary"))
        (p2 (make-test-provider "backup")))
    (pseudopod:router-add-provider r p1)
    (pseudopod:router-add-provider r p2)
    (let ((selected (pseudopod:router-select-provider r)))
      (is (not (null selected))))))

(test router-task-based-strategy
  (let ((r (pseudopod:make-model-router :strategy :task-based
                                         :task-routing '((:coding . "code-provider")
                                                          (:chat . "chat-provider"))))
        (p1 (make-test-provider "code-provider"))
        (p2 (make-test-provider "chat-provider")))
    (pseudopod:router-add-provider r p1)
    (pseudopod:router-add-provider r p2)
    (let ((selected (pseudopod:router-select-provider r :task-type :coding)))
      (is (string= "code-provider" (pseudopod:provider-name selected))))
    (let ((selected (pseudopod:router-select-provider r :task-type :chat)))
      (is (string= "chat-provider" (pseudopod:provider-name selected))))))

(test router-cost-aware-strategy
  (let ((r (pseudopod:make-model-router :strategy :cost-aware
                                         :cost-tiers '("cheap" "medium" "expensive")))
        (p1 (make-test-provider "expensive"))
        (p2 (make-test-provider "cheap"))
        (p3 (make-test-provider "medium")))
    (pseudopod:router-add-provider r p1)
    (pseudopod:router-add-provider r p2)
    (pseudopod:router-add-provider r p3)
    (let ((selected (pseudopod:router-select-provider r)))
      (is (string= "cheap" (pseudopod:provider-name selected))))))

(test router-cost-aware-skips-unhealthy
  (let ((r (pseudopod:make-model-router :strategy :cost-aware
                                         :cost-tiers '("cheap" "medium")))
        (p1 (make-test-provider "cheap" :healthy nil))
        (p2 (make-test-provider "medium")))
    (pseudopod:router-add-provider r p1)
    (pseudopod:router-add-provider r p2)
    (let ((selected (pseudopod:router-select-provider r)))
      (is (string= "medium" (pseudopod:provider-name selected))))))

(test router-status-report
  (let ((r (pseudopod:make-model-router :strategy :fallback-chain))
        (p1 (make-test-provider "alpha"))
        (p2 (make-test-provider "beta" :healthy nil)))
    (pseudopod:router-add-provider r p1)
    (pseudopod:router-add-provider r p2)
    (let ((status (pseudopod:router-status r)))
      (is (eq :fallback-chain (getf status :strategy)))
      (is (= 2 (getf status :total-providers)))
      (is (= 1 (getf status :healthy-providers)))
      (is (= 2 (length (getf status :providers)))))))

(test router-set-task-route
  (let ((r (pseudopod:make-model-router :strategy :task-based))
        (p (make-test-provider "reviewer")))
    (pseudopod:router-add-provider r p)
    (pseudopod:router-set-task-route r :review "reviewer")
    (let ((selected (pseudopod:router-select-provider r :task-type :review)))
      (is (string= "reviewer" (pseudopod:provider-name selected))))))

(test router-no-providers-returns-nil
  (let ((r (pseudopod:make-model-router)))
    (is (null (pseudopod:router-select-provider r)))))

(test router-all-unhealthy-fallback
  (let ((r (pseudopod:make-model-router :strategy :fallback-chain))
        (p1 (make-test-provider "sick1" :healthy nil))
        (p2 (make-test-provider "sick2" :healthy nil)))
    (pseudopod:router-add-provider r p1)
    (pseudopod:router-add-provider r p2)
    ;; Fallback chain should still include unhealthy providers as last resort
    (let ((selected (pseudopod:router-select-provider r)))
      (is (not (null selected))))))
