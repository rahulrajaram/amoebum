(in-package :amoebum/test)

(def-suite status-bar-mode-suite
  :description "Focused regression coverage for status-bar mode presets."
  :in amoebum-suite)

(in-suite status-bar-mode-suite)

(defclass status-bar-mode-test-provider (pseudopod:provider) ())

(defmethod pseudopod:list-provider-models ((provider status-bar-mode-test-provider))
  (declare (ignore provider))
  '())

(defmethod pseudopod:provider-health-check ((provider status-bar-mode-test-provider))
  (pseudopod:provider-healthy-p provider))

(defun %make-status-bar-mode-provider (name model)
  (make-instance 'status-bar-mode-test-provider
                 :name name
                 :api-key ""
                 :base-url "https://example.invalid"
                 :default-model model))

(defun %seed-status-bar-mode-provider-health (router)
  (let ((kimi (%make-status-bar-mode-provider "Kimi" "moonshot-v1-128k")))
    (setf (pseudopod:provider-request-count kimi) 3
          (pseudopod:provider-error-count kimi) 0
          (pseudopod:provider-healthy-p kimi) t)
    (pseudopod:router-add-provider router kimi)
    (amoebum:provider-health-monitor-reset!)
    (let ((amoebum:*model-router* router))
      (amoebum:provider-health-refresh! :force t))
    router))

(defun %seed-status-bar-mode-worker-state ()
  (setf amoebum:*worker-supervisor* nil
        amoebum::*agent-registry* (make-hash-table :test #'equal))
  (amoebum:clear-workers)
  (setf (gethash "task-mode-1" amoebum::*agent-registry*)
        (amoebum::%make-agent-record
         :id "task-mode-1"
         :type :task
         :task "status bar mode coverage"
         :status :running))
  (amoebum::%store-worker
   (amoebum::%make-worker-record
    :id "w-mode-1"
    :type :agent
    :label "Status mode worker"
    :status :running
    :created-at 1
    :backend :in-process
    :inner-id "task-mode-1")))

(test status-bar-focus-modes-select-distinct-segment-presets
  (let ((old-sup amoebum:*worker-supervisor*)
        (old-agent-registry amoebum::*agent-registry*))
    (unwind-protect
         (let* ((router (%seed-status-bar-mode-provider-health
                         (pseudopod:make-model-router :strategy :fallback-chain))))
           (%seed-status-bar-mode-worker-state)
           (let ((amoebum:*model-router* router))
             (let* ((lean-state (amoebum.ui:make-status-bar-state
                                 :permission-mode :supervised
                                 :focus-mode :lean
                                 :model-name "moonshot-v1-128k"
                                 :branch-name "feature/nxt-104"))
                    (code-state (amoebum.ui:make-status-bar-state
                                 :permission-mode :supervised
                                 :focus-mode :code
                                 :model-name "moonshot-v1-128k"
                                 :branch-name "feature/nxt-104"))
                    (docs-state (amoebum.ui:make-status-bar-state
                                 :permission-mode :supervised
                                 :focus-mode :docs
                                 :model-name "moonshot-v1-128k"
                                 :branch-name "feature/nxt-104"))
                    (arch-state (amoebum.ui:make-status-bar-state
                                 :permission-mode :supervised
                                 :focus-mode :arch
                                 :model-name "moonshot-v1-128k"
                                 :branch-name "feature/nxt-104"))
                    (lean-line (amoebum.ui:status-bar-line lean-state))
                    (code-line (amoebum.ui:status-bar-line code-state))
                    (docs-line (amoebum.ui:status-bar-line docs-state))
                    (arch-line (amoebum.ui:status-bar-line arch-state)))
               (is (eq :arch (amoebum.ui:status-bar-state-focus-mode
                              (amoebum.ui:make-status-bar-state
                               :permission-mode :supervised
                               :focus-mode :bogus
                               :model-name "moonshot-v1-128k"
                               :branch-name "feature/nxt-104"))))
               (is (search "stream idle" lean-line :test #'char-equal))
               (is (search "model moonshot-v1-128k" lean-line :test #'char-equal))
               (is-false (search "mode supervised" lean-line :test #'char-equal))
               (is-false (search "Tokens:" lean-line :test #'char-equal))
               (is-false (search "W:1" lean-line :test #'char-equal))
               (is-false (search "K:OK" lean-line :test #'char-equal))

               (is (search "mode supervised" code-line :test #'char-equal))
               (is (search "Tokens:" code-line :test #'char-equal))
               (is (search "W:1" code-line :test #'char-equal))
               (is-false (search "K:OK" code-line :test #'char-equal))

               (is (search "Tokens:" docs-line :test #'char-equal))
               (is (search "K:OK" docs-line :test #'char-equal))
               (is-false (search "mode supervised" docs-line :test #'char-equal))
               (is-false (search "W:1" docs-line :test #'char-equal))

               (is (search "mode supervised" arch-line :test #'char-equal))
               (is (search "Tokens:" arch-line :test #'char-equal))
               (is (search "K:OK" arch-line :test #'char-equal))
               (is (search "W:1" arch-line :test #'char-equal)))))
      (amoebum:provider-health-monitor-reset!)
      (amoebum:clear-workers)
      (setf amoebum:*worker-supervisor* old-sup
            amoebum::*agent-registry* old-agent-registry))))

(test status-bar-focus-mode-updates-from-config-event
  (let* ((bus (amoebum:make-event-bus :capacity 32))
         (state (amoebum.ui:make-status-bar-state
                 :event-bus bus
                 :permission-mode :supervised
                 :focus-mode :lean
                 :model-name "moonshot-v1-128k"
                 :branch-name "feature/nxt-104")))
    (is (eq :lean (amoebum.ui:status-bar-state-focus-mode state)))
    (amoebum:publish bus
                     (amoebum:make-config-changed-event
                      :key :status-bar-mode
                      :old-value :lean
                      :new-value :docs))
    (is (eq :docs (amoebum.ui:status-bar-state-focus-mode state)))
    (let ((line (amoebum.ui:status-bar-line state)))
      (is (search "Tokens:" line :test #'char-equal))
      (is-false (search "mode supervised" line :test #'char-equal)))))
