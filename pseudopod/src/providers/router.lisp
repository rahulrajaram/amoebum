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
  (model-costs (make-hash-table :test #'equal) :type hash-table)
  (cost-tiers '() :type list)
  (max-retries 2 :type integer)
  (health-check-interval-seconds 300 :type integer)
  (last-health-check-at 0 :type integer))

(defstruct (cost-model
            (:constructor make-cost-model (&key input-cost-per-1k-tokens
                                                 output-cost-per-1k-tokens
                                                 context-window)))
  (input-cost-per-1k-tokens 0.0d0 :type real)
  (output-cost-per-1k-tokens 0.0d0 :type real)
  (context-window 8192 :type integer))

(defparameter *default-model-costs*
  '(("claude-sonnet" . (:input 0.003d0 :output 0.015d0 :context 200000))
    ("claude-haiku" . (:input 0.0008d0 :output 0.004d0 :context 200000))
    ("gpt-4o" . (:input 0.005d0 :output 0.015d0 :context 128000))
    ("gpt-4o-mini" . (:input 0.00015d0 :output 0.0006d0 :context 128000))
    ("kimi-k2.5" . (:input 0.0003d0 :output 0.0012d0 :context 128000))))

(defun %normalize-model-id (value)
  (let ((text (string-downcase
               (string-trim '(#\Space #\Tab #\Newline #\Return)
                            (or value "")))))
    (cond
      ((or (search "haiku" text :test #'char-equal)
           (string= text "claude-haiku"))
       "claude-haiku")
      ((or (search "sonnet" text :test #'char-equal)
           (string= text "claude-sonnet"))
       "claude-sonnet")
      ((search "gpt-4o-mini" text :test #'char-equal)
       "gpt-4o-mini")
      ((search "gpt-4o" text :test #'char-equal)
       "gpt-4o")
      ((or (search "kimi-k2.5" text :test #'char-equal)
           (search "moonshot" text :test #'char-equal))
       "kimi-k2.5")
      ((plusp (length text))
       text)
      (t
       nil))))

(defun %default-model-cost-table ()
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry *default-model-costs*)
      (destructuring-bind (name . config) entry
        (setf (gethash (%normalize-model-id name) table)
              (make-cost-model
               :input-cost-per-1k-tokens (getf config :input 0.0d0)
               :output-cost-per-1k-tokens (getf config :output 0.0d0)
               :context-window (getf config :context 8192)))))
    table))

(defun %normalize-model-cost-entry (entry)
  (when (consp entry)
    (let ((model-key (%normalize-model-id (car entry)))
          (value (cdr entry)))
      (when model-key
        (cond
          ((cost-model-p value)
           (cons model-key value))
          ((and (listp value)
                (or (getf value :input-cost-per-1k-tokens)
                    (getf value :output-cost-per-1k-tokens)
                    (getf value :context-window)))
           (cons model-key
                 (make-cost-model
                  :input-cost-per-1k-tokens
                  (or (getf value :input-cost-per-1k-tokens)
                      (getf value :input)
                      0.0d0)
                  :output-cost-per-1k-tokens
                  (or (getf value :output-cost-per-1k-tokens)
                      (getf value :output)
                      0.0d0)
                  :context-window
                  (or (getf value :context-window)
                      (getf value :context)
                      8192)))))))))

(defun %message-text (message)
  (cond
    ((message-p message)
     (with-output-to-string (out)
       (dolist (part (message-content message))
         (when (and (content-part-p part)
                    (stringp (content-part-text part)))
           (write-string (content-part-text part) out)
           (write-char #\Space out)))))
    ((stringp message)
     message)
    ((hash-table-p message)
     (let ((content (gethash "content" message)))
       (cond
         ((stringp content)
          content)
         ((listp content)
          (with-output-to-string (out)
            (dolist (part content)
              (when (hash-table-p part)
                (let ((text (gethash "text" part)))
                  (when (stringp text)
                    (write-string text out)
                    (write-char #\Space out)))))))
         ((vectorp content)
          (with-output-to-string (out)
            (loop for part across content do
              (when (hash-table-p part)
                (let ((text (gethash "text" part)))
                  (when (stringp text)
                    (write-string text out)
                    (write-char #\Space out)))))))
         (t
          ""))))
    (t
     (princ-to-string message))))

(defun %estimate-input-tokens (provider messages)
  (loop for message in (or messages '())
        for text = (%message-text message)
        sum (max 0 (estimate-provider-tokens provider (or text "")))))

(defun %router-provider-cost-model (router provider &key model)
  (let* ((model-id (%normalize-model-id (or model (provider-default-model provider))))
         (table (model-router-model-costs router)))
    (and model-id
         (hash-table-p table)
         (gethash model-id table))))

(defun cost-estimate (provider messages &key (output-tokens 0) model cost-model)
  "Estimate provider cost from input/output token counts and model pricing."
  (let* ((resolved-cost-model
           (or cost-model
               (let ((defaults (%default-model-cost-table)))
                 (gethash (%normalize-model-id (or model (provider-default-model provider)))
                          defaults))))
         (input-tokens (%estimate-input-tokens provider messages))
         (input-cost (if resolved-cost-model
                         (* (/ (float input-tokens 1.0d0) 1000.0d0)
                            (float (cost-model-input-cost-per-1k-tokens resolved-cost-model)
                                   1.0d0))
                         0.0d0))
         (output-cost (if resolved-cost-model
                          (* (/ (float (max 0 output-tokens) 1.0d0) 1000.0d0)
                             (float (cost-model-output-cost-per-1k-tokens resolved-cost-model)
                                    1.0d0))
                          0.0d0)))
    (list :provider (provider-name provider)
          :model (or model (provider-default-model provider))
          :input-tokens input-tokens
          :output-tokens (max 0 output-tokens)
          :input-cost-usd input-cost
          :output-cost-usd output-cost
          :total-cost-usd (+ input-cost output-cost)
          :context-window (and resolved-cost-model
                               (cost-model-context-window resolved-cost-model)))))

(defun make-model-router (&key (strategy :fallback-chain)
                                providers
                                task-routing
                                model-costs
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
                 :model-costs (%default-model-cost-table)
                 :max-retries max-retries
                 :health-check-interval-seconds health-check-interval-seconds)))
    (when task-routing
      (dolist (pair task-routing)
        (setf (gethash (car pair) (model-router-task-routing router))
              (cdr pair))))
    (when model-costs
      (dolist (entry model-costs)
        (let ((normalized (%normalize-model-cost-entry entry)))
          (when normalized
            (setf (gethash (car normalized) (model-router-model-costs router))
                  (cdr normalized))))))
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

(defun router-set-task-route (router task-type provider-name &key model)
  "Set routing for TASK-TYPE to PROVIDER-NAME.

When MODEL is provided, route stores a model+provider pair plist."
  (setf (gethash task-type (model-router-task-routing router))
        (if model
            (list :provider provider-name
                  :model model)
            provider-name))
  router)

(defun %task-route-provider-name (route)
  (typecase route
    (null nil)
    (string route)
    (symbol (string-downcase (symbol-name route)))
    (cons
     (let ((provider (or (getf route :provider)
                         (getf route :provider-name))))
       (typecase provider
         (string provider)
         (symbol (string-downcase (symbol-name provider)))
         (t nil))))
    (t nil)))

(defun %task-route-model (route)
  (when (listp route)
    (let ((model (or (getf route :model)
                     (getf route :model-name))))
      (and model
           (princ-to-string model)))))

(defun %task-route-target (router task-type)
  (let* ((route (gethash task-type (model-router-task-routing router)))
         (provider-name (%task-route-provider-name route))
         (provider (and provider-name
                        (router-find-provider router provider-name))))
    (values provider
            (%task-route-model route))))

;;; --- Provider Selection ---

(defun %router-select-by-task (router task-type)
  "Select provider based on task type."
  (let* ((route (gethash task-type (model-router-task-routing router)))
         (provider-name (%task-route-provider-name route))
         (provider (when provider-name
                     (router-find-provider router provider-name))))
    (if (and provider (provider-healthy-p provider))
        provider
        ;; Fallback to first healthy provider
        (first (router-healthy-providers router)))))

(defun %router-select-by-cost (router messages minimum-context-window expected-output-tokens)
  "Select cheapest healthy provider that can satisfy MINIMUM-CONTEXT-WINDOW."
  (let* ((healthy (router-healthy-providers router))
         (costed
           (loop for provider in healthy
                 for provider-cost-model = (%router-provider-cost-model router provider)
                 when provider-cost-model
                   collect (list :provider provider
                                 :cost-model provider-cost-model
                                 :estimate (cost-estimate provider
                                                          messages
                                                          :output-tokens expected-output-tokens
                                                          :cost-model provider-cost-model))))
         (eligible
           (remove-if (lambda (entry)
                        (let ((window (cost-model-context-window
                                       (getf entry :cost-model))))
                          (< window minimum-context-window)))
                      costed)))
    (cond
      ((plusp (length eligible))
       (getf (car (sort eligible
                        (lambda (left right)
                          (let ((left-cost (getf (getf left :estimate) :total-cost-usd))
                                (right-cost (getf (getf right :estimate) :total-cost-usd))
                                (left-name (provider-name (getf left :provider)))
                                (right-name (provider-name (getf right :provider))))
                            (if (= left-cost right-cost)
                                (string< left-name right-name)
                                (< left-cost right-cost))))))
             :provider))
      ((and (zerop minimum-context-window)
            (model-router-cost-tiers router))
       (or (loop for name in (model-router-cost-tiers router)
                 for provider = (find name healthy
                                      :key #'provider-name
                                      :test #'string=)
                 when provider return provider)
           (first healthy)))
      ((zerop minimum-context-window)
       (first healthy))
      (t
       nil))))

(defun %router-select-fallback-chain (router)
  "Return ordered list of providers to try."
  (let ((healthy (router-healthy-providers router))
        (unhealthy (remove-if #'provider-healthy-p (model-router-providers router))))
    ;; Try healthy first, then unhealthy as last resort
    (append healthy unhealthy)))

(defun %router-select-task-chain (router preferred-provider)
  "Return fallback chain for task routing, prioritizing preferred provider when healthy."
  (let ((fallback (%router-select-fallback-chain router)))
    (if (and preferred-provider
             (provider-healthy-p preferred-provider))
        (cons preferred-provider
              (remove preferred-provider fallback :test #'eq))
        fallback)))

(defun router-select-provider (router &key task-type
                                            messages
                                            (minimum-context-window 0)
                                            (expected-output-tokens 0))
  "Select a provider based on router strategy."
  (case (model-router-strategy router)
    (:task-based (%router-select-by-task router task-type))
    (:cost-aware (%router-select-by-cost router
                                         messages
                                         (max 0 minimum-context-window)
                                         (max 0 expected-output-tokens)))
    (:fallback-chain (first (%router-select-fallback-chain router)))
    (otherwise (first (model-router-providers router)))))

;;; --- Routed Requests ---

(defun router-chat-completion (router messages &key model temperature max-tokens
                                                    top-p tools tool-choice
                                                    system-prompt task-type extra-params)
  "Send chat completion via router with fallback."
  (multiple-value-bind (task-provider task-model)
      (%task-route-target router task-type)
    (let ((chain (case (model-router-strategy router)
                   (:fallback-chain (%router-select-fallback-chain router))
                   (:task-based (%router-select-task-chain router task-provider))
                   (otherwise (let ((p (router-select-provider router
                                                               :task-type task-type
                                                               :messages messages)))
                                (when p (list p)))))))
      (unless chain
        (error 'pseudopod-error :message "No providers available in router"))
      (let ((last-error nil))
        (dolist (provider chain)
          (let ((provider-model
                  (or model
                      (and task-model
                           task-provider
                           (eq provider task-provider)
                           task-model))))
            (handler-case
                (let ((result
                        (send-chat-completion provider messages
                                              :model provider-model
                                              :temperature temperature
                                              :max-tokens max-tokens
                                              :top-p top-p
                                              :tools tools
                                              :tool-choice tool-choice
                                              :system-prompt system-prompt
                                              :extra-params extra-params)))
                  (setf (provider-healthy-p provider) t
                        (provider-last-error provider) nil)
                  (return-from router-chat-completion result))
              (error (c)
                (setf last-error c)
                (setf (provider-healthy-p provider) nil
                      (provider-last-error provider) c)))))
        (error (or last-error
                   (make-condition 'pseudopod-error
                                   :message "All providers failed")))))))

(defun router-streaming-completion (router messages callback &key model temperature
                                                                  max-tokens top-p
                                                                  tools tool-choice
                                                                  system-prompt task-type
                                                                  extra-params)
  "Send streaming completion via router with fallback."
  (multiple-value-bind (task-provider task-model)
      (%task-route-target router task-type)
    (let ((chain (case (model-router-strategy router)
                   (:fallback-chain (%router-select-fallback-chain router))
                   (:task-based (%router-select-task-chain router task-provider))
                   (otherwise (let ((p (router-select-provider router
                                                               :task-type task-type
                                                               :messages messages)))
                                (when p (list p)))))))
      (unless chain
        (error 'pseudopod-error :message "No providers available in router"))
      (let ((last-error nil))
        (dolist (provider chain)
          (let ((provider-model
                  (or model
                      (and task-model
                           task-provider
                           (eq provider task-provider)
                           task-model))))
            (handler-case
                (let ((result
                        (send-streaming-completion provider messages callback
                                                   :model provider-model
                                                   :temperature temperature
                                                   :max-tokens max-tokens
                                                   :top-p top-p
                                                   :tools tools
                                                   :tool-choice tool-choice
                                                   :system-prompt system-prompt
                                                   :extra-params extra-params)))
                  (setf (provider-healthy-p provider) t
                        (provider-last-error provider) nil)
                  (return-from router-streaming-completion result))
              (error (c)
                (setf last-error c)
                (setf (provider-healthy-p provider) nil
                      (provider-last-error provider) c)))))
        (error (or last-error
                   (make-condition 'pseudopod-error
                                   :message "All providers failed")))))))

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
