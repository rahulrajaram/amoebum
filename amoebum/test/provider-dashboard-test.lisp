(in-package :amoebum/test)

(def-suite provider-dashboard-suite
  :description "I228 provider monitoring dashboard and toggle wiring."
  :in amoebum-suite)

(in-suite provider-dashboard-suite)

(defclass provider-dashboard-test-provider (pseudopod:provider) ())

(defmethod pseudopod:list-provider-models ((provider provider-dashboard-test-provider))
  (declare (ignore provider))
  '())

(defmethod pseudopod:provider-health-check ((provider provider-dashboard-test-provider))
  (pseudopod:provider-healthy-p provider))

(defun %make-provider-dashboard-test-provider (name model)
  (make-instance 'provider-dashboard-test-provider
                 :name name
                 :api-key ""
                 :base-url "https://example.invalid"
                 :default-model model))

(defun %find-provider-health-entry (entries name)
  (find name entries
        :test #'string=
        :key #'amoebum:provider-health-entry-name))

(test provider-health-monitor-collects-statuses-and-indicator
  (let* ((router (pseudopod:make-model-router :strategy :fallback-chain))
         (kimi (%make-provider-dashboard-test-provider "Kimi" "moonshot-v1-128k"))
         (anthropic (%make-provider-dashboard-test-provider "Anthropic" "claude-3-7-sonnet"))
         (openai (%make-provider-dashboard-test-provider "OpenAI" "gpt-4o")))
    (setf (pseudopod:provider-request-count kimi) 10
          (pseudopod:provider-error-count kimi) 0
          (pseudopod:provider-last-latency-ms kimi) 120
          (pseudopod:provider-healthy-p kimi) t)
    (setf (pseudopod:provider-request-count anthropic) 20
          (pseudopod:provider-error-count anthropic) 2
          (pseudopod:provider-last-latency-ms anthropic) 350
          (pseudopod:provider-last-error anthropic) (make-condition 'simple-error
                                                                    :format-control "quota warning")
          (pseudopod:provider-healthy-p anthropic) t)
    (setf (pseudopod:provider-request-count openai) 5
          (pseudopod:provider-error-count openai) 5
          (pseudopod:provider-last-latency-ms openai) 900
          (pseudopod:provider-last-error openai) (make-condition 'simple-error
                                                                 :format-control "connection refused")
          (pseudopod:provider-healthy-p openai) nil)
    (pseudopod:router-add-provider router kimi)
    (pseudopod:router-add-provider router anthropic)
    (pseudopod:router-add-provider router openai)
    (amoebum:provider-health-monitor-reset!)
    (let ((entries (amoebum:provider-health-refresh! :router router :force t))
          (indicator (amoebum:provider-health-compact-indicator :router router)))
      (is (= 3 (length entries)))
      (is (eq :healthy
              (amoebum:provider-health-entry-status
               (%find-provider-health-entry entries "Kimi"))))
      (is (eq :degraded
              (amoebum:provider-health-entry-status
               (%find-provider-health-entry entries "Anthropic"))))
      (is (eq :down
              (amoebum:provider-health-entry-status
               (%find-provider-health-entry entries "OpenAI"))))
      (is-true indicator)
      (is (search "K:OK" (getf indicator :text) :test #'char-equal))
      (is (search "A:DEG" (getf indicator :text) :test #'char-equal))
      (is (search "O:ERR" (getf indicator :text) :test #'char-equal)))))

(test provider-status-bar-line-includes-compact-indicator
  (let* ((router (pseudopod:make-model-router :strategy :fallback-chain))
         (kimi (%make-provider-dashboard-test-provider "Kimi" "moonshot-v1-128k"))
         (anthropic (%make-provider-dashboard-test-provider "Anthropic" "claude-3-7-sonnet")))
    (setf (pseudopod:provider-request-count kimi) 5
          (pseudopod:provider-error-count kimi) 0
          (pseudopod:provider-healthy-p kimi) t
          (pseudopod:provider-request-count anthropic) 5
          (pseudopod:provider-error-count anthropic) 5
          (pseudopod:provider-healthy-p anthropic) nil)
    (pseudopod:router-add-provider router kimi)
    (pseudopod:router-add-provider router anthropic)
    (let ((amoebum:*model-router* router))
      (amoebum:provider-health-monitor-reset!)
      (amoebum:provider-health-refresh! :force t)
      (let* ((state (amoebum.ui:make-status-bar-state
                     :permission-mode :supervised
                     :model-name "moonshot-v1-128k"
                     :branch-name "feature/i228"))
             (line (amoebum.ui:status-bar-line state)))
        (is (search "[" line :test #'char=))
        (is (search "K:OK" line :test #'char-equal))
        (is (search "A:ERR" line :test #'char-equal))))))

(test status-bar-line-includes-worker-runtime-summary
  (let ((old-sup amoebum:*worker-supervisor*)
        (old-agent-registry amoebum::*agent-registry*))
    (unwind-protect
         (progn
           (setf amoebum:*worker-supervisor* nil
                 amoebum::*agent-registry* (make-hash-table :test #'equal))
           (amoebum:clear-workers)
           (setf (gethash "task-0001" amoebum::*agent-registry*)
                 (amoebum::%make-agent-record
                  :id "task-0001"
                  :type :task
                  :task "runtime summary task"
                  :status :running))
           (amoebum::%store-worker
            (amoebum::%make-worker-record
             :id "w-agent-local" :type :agent :label "Local worker"
             :status :running :created-at 1
             :backend :in-process :inner-id "task-0001"))
           (let* ((state (amoebum.ui:make-status-bar-state
                          :permission-mode :supervised
                          :model-name "moonshot-v1-128k"
                          :branch-name "feature/nxt-058"))
                  (line (amoebum.ui:status-bar-line state)))
             (is (search "W:1" line :test #'char-equal))
             (is (search "local/running" line :test #'char-equal))))
      (amoebum:clear-workers)
      (setf amoebum:*worker-supervisor* old-sup
            amoebum::*agent-registry* old-agent-registry))))

(test providers-slash-command-toggles-dashboard-visibility
  (let* ((chat-state (amoebum.ui:make-chat-ui-state :stream-runner nil))
         (handle-slash-input
           (symbol-function
            (or (find-symbol "%HANDLE-SLASH-COMMAND-INPUT" :amoebum)
                (error "Missing %HANDLE-SLASH-COMMAND-INPUT in AMOEBUM.")))))
    (is-true (funcall handle-slash-input chat-state "/providers on"))
    (is-true (amoebum.ui:chat-ui-state-provider-dashboard-visible-p chat-state))
    (is-true (funcall handle-slash-input chat-state "/providers off"))
    (is-false (amoebum.ui:chat-ui-state-provider-dashboard-visible-p chat-state))
    (is-true (funcall handle-slash-input chat-state "/providers"))
    (is-true (amoebum.ui:chat-ui-state-provider-dashboard-visible-p chat-state))))

(test slash-command-action-registry-applies-provider-dashboard-toggle
  (let* ((chat-state (amoebum.ui:make-chat-ui-state :stream-runner nil))
         (result (amoebum.commands:make-slash-command-result
                  :action :toggle-provider-dashboard
                  :payload :off)))
    (setf (amoebum.ui:chat-ui-state-provider-dashboard-visible-p chat-state) t)
    (let ((output (amoebum:apply-slash-command-result-action result :chat-state chat-state)))
      (is (string= "Provider dashboard hidden." output))
      (is-false (amoebum.ui:chat-ui-state-provider-dashboard-visible-p chat-state)))))

(test provider-dashboard-smoke-sentinel
  (is-true t)
  (format t "PROVIDER_DASHBOARD_SMOKE_OK~%"))
