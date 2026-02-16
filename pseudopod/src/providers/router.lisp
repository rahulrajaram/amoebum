(in-package :pseudopod)

;;; ---------------------------------------------------------------------------
;;; Model Router + Fallback Chains (I95)
;;;
;;; Routes requests to providers based on strategy:
;;; - :task-based — select provider by task type hints
;;; - :cost-aware — prefer cheaper providers first
;;; - :fallback-chain — try providers in order, fall back on error
;;; ---------------------------------------------------------------------------

(defstruct (model-router
            (:constructor %make-model-router))
  (strategy :fallback-chain :type keyword)
  (providers '() :type list)
  (task-routing (make-hash-table :test #'equal) :type hash-table)
  (cost-tiers '() :type list)
  (max-retries 2 :type integer)
  (health-check-interval-seconds 300 :type integer)
  (last-health-check-at 0 :type integer))

(defun make-model-router (&key (strategy :fallback-chain)
                                providers
                                task-routing
                                cost-tiers
                                (max-retries 2)
                                (health-check-interval-seconds 300))
  "Create a model router.
PROVIDERS is a list of provider instances.
TASK-ROUTING is an alist mapping task-type keywords to provider names.
COST-TIERS is a list of provider names in cost order (cheapest first)."
  (let ((router (%make-model-router
                 :strategy strategy
                 :providers (or providers '())
                 :max-retries max-retries
                 :health-check-interval-seconds health-check-interval-seconds)))
    (when task-routing
      (dolist (pair task-routing)
        (setf (gethash (car pair) (model-router-task-routing router))
              (cdr pair))))
    (when cost-tiers
      (setf (model-router-cost-tiers router) cost-tiers))
    router))

(defun router-add-provider (router provider)
  "Add PROVIDER to ROUTER's provider list."
  (push provider (model-router-providers router))
  router)

(defun router-remove-provider (router provider-name)
  "Remove provider with PROVIDER-NAME from ROUTER."
  (setf (model-router-providers router)
        (remove provider-name (model-router-providers router)
               :key #'provider-name :test #'string=))
  router)

(defun router-find-provider (router provider-name)
  "Find provider with PROVIDER-NAME in ROUTER."
  (find provider-name (model-router-providers router)
        :key #'provider-name :test #'string=))

(defun router-healthy-providers (router)
  "Return list of healthy providers."
  (remove-if-not #'provider-healthy-p (model-router-providers router)))

(defun router-set-task-route (router task-type provider-name)
  "Set routing for TASK-TYPE to PROVIDER-NAME."
  (setf (gethash task-type (model-router-task-routing router)) provider-name)
  router)

;;; --- Provider Selection ---

(defun %router-select-by-task (router task-type)
  "Select provider based on task type."
  (let* ((provider-name (gethash task-type (model-router-task-routing router)))
         (provider (when provider-name
                     (router-find-provider router provider-name))))
    (if (and provider (provider-healthy-p provider))
        provider
        ;; Fallback to first healthy provider
        (first (router-healthy-providers router)))))

(defun %router-select-by-cost (router)
  "Select cheapest healthy provider."
  (let ((tiers (model-router-cost-tiers router))
        (healthy (router-healthy-providers router)))
    (if tiers
        (or (loop for name in tiers
                  for provider = (find name healthy :key #'provider-name :test #'string=)
                  when provider return provider)
            (first healthy))
        (first healthy))))

(defun %router-select-fallback-chain (router)
  "Return ordered list of providers to try."
  (let ((healthy (router-healthy-providers router))
        (unhealthy (remove-if #'provider-healthy-p (model-router-providers router))))
    ;; Try healthy first, then unhealthy as last resort
    (append healthy unhealthy)))

(defun router-select-provider (router &key task-type)
  "Select a provider based on router strategy."
  (case (model-router-strategy router)
    (:task-based (%router-select-by-task router task-type))
    (:cost-aware (%router-select-by-cost router))
    (:fallback-chain (first (%router-select-fallback-chain router)))
    (otherwise (first (model-router-providers router)))))

;;; --- Routed Requests ---

(defun router-chat-completion (router messages &key model temperature max-tokens
                                                    top-p tools tool-choice
                                                    system-prompt task-type extra-params)
  "Send chat completion via router with fallback."
  (let ((chain (case (model-router-strategy router)
                 (:fallback-chain (%router-select-fallback-chain router))
                 (otherwise (let ((p (router-select-provider router :task-type task-type)))
                              (when p (list p)))))))
    (unless chain
      (error 'pseudopod-error :message "No providers available in router"))
    (let ((last-error nil))
      (dolist (provider chain)
        (handler-case
            (return-from router-chat-completion
              (send-chat-completion provider messages
                                    :model model :temperature temperature
                                    :max-tokens max-tokens :top-p top-p
                                    :tools tools :tool-choice tool-choice
                                    :system-prompt system-prompt
                                    :extra-params extra-params))
          (error (c)
            (setf last-error c)
            (setf (provider-healthy-p provider) nil
                  (provider-last-error provider) c))))
      (error (or last-error
                 (make-condition 'pseudopod-error
                                 :message "All providers failed"))))))

(defun router-streaming-completion (router messages callback &key model temperature
                                                                  max-tokens top-p
                                                                  tools tool-choice
                                                                  system-prompt task-type
                                                                  extra-params)
  "Send streaming completion via router with fallback."
  (let ((chain (case (model-router-strategy router)
                 (:fallback-chain (%router-select-fallback-chain router))
                 (otherwise (let ((p (router-select-provider router :task-type task-type)))
                              (when p (list p)))))))
    (unless chain
      (error 'pseudopod-error :message "No providers available in router"))
    (let ((last-error nil))
      (dolist (provider chain)
        (handler-case
            (return-from router-streaming-completion
              (send-streaming-completion provider messages callback
                                         :model model :temperature temperature
                                         :max-tokens max-tokens :top-p top-p
                                         :tools tools :tool-choice tool-choice
                                         :system-prompt system-prompt
                                         :extra-params extra-params))
          (error (c)
            (setf last-error c)
            (setf (provider-healthy-p provider) nil
                  (provider-last-error provider) c))))
      (error (or last-error
                 (make-condition 'pseudopod-error
                                 :message "All providers failed"))))))

;;; --- Health Management ---

(defun router-check-health (router &key force)
  "Run health checks on all providers if interval has elapsed or FORCE is true."
  (let ((now (get-universal-time))
        (interval (model-router-health-check-interval-seconds router)))
    (when (or force
              (>= (- now (model-router-last-health-check-at router)) interval))
      (setf (model-router-last-health-check-at router) now)
      (dolist (provider (model-router-providers router))
        (handler-case
            (provider-health-check provider)
          (error (c)
            (setf (provider-healthy-p provider) nil
                  (provider-last-error provider) c)))))))

;;; --- Router Status ---

(defun router-status (router)
  "Return an alist summarizing router state."
  (list :strategy (model-router-strategy router)
        :total-providers (length (model-router-providers router))
        :healthy-providers (length (router-healthy-providers router))
        :providers
        (mapcar (lambda (p)
                  (list :name (provider-name p)
                        :model (provider-default-model p)
                        :healthy (provider-healthy-p p)
                        :requests (provider-request-count p)
                        :errors (provider-error-count p)
                        :error-rate (provider-error-rate p)
                        :last-latency-ms (provider-last-latency-ms p)))
                (model-router-providers router))))
