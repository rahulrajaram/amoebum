(in-package :amoebum/test)

;;; ============================================================
;;; I238: Configuration Validation and Runtime Changes
;;; ============================================================

(def-suite config-validation-suite :in amoebum-suite)
(in-suite config-validation-suite)

;;; --- Validation checks ---

(test valid-model-value
  "Model should accept strings."
  (is (amoebum::%valid-config-value-p :model "moonshot-v1-128k"))
  (is (not (amoebum::%valid-config-value-p :model 42)))
  (is (not (amoebum::%valid-config-value-p :model nil))))

(test valid-permission-mode-values
  "Permission mode should accept known keywords."
  (dolist (mode '(:supervised :auto-edit :full-auto :yolo :no-confirm))
    (is (amoebum::%valid-config-value-p :permission-mode mode)))
  (is (not (amoebum::%valid-config-value-p :permission-mode :unknown))))

(test valid-integer-thresholds
  "Integer config keys should validate range."
  (is (amoebum::%valid-config-value-p :stream-budget-abort-threshold-percent 50))
  (is (not (amoebum::%valid-config-value-p :stream-budget-abort-threshold-percent 0)))
  (is (not (amoebum::%valid-config-value-p :stream-budget-abort-threshold-percent 101)))
  (is (amoebum::%valid-config-value-p :web-fetch-timeout-seconds 30))
  (is (not (amoebum::%valid-config-value-p :web-fetch-timeout-seconds -1))))

(test valid-boolean-values
  "Boolean config keys should only accept t or nil."
  (is (amoebum::%valid-config-value-p :haake-autodetect t))
  (is (amoebum::%valid-config-value-p :haake-autodetect nil))
  (is (not (amoebum::%valid-config-value-p :haake-autodetect "yes"))))

(test valid-optional-string-values
  "Optional string keys should accept nil or non-empty strings."
  (is (amoebum::%valid-config-value-p :api-base-url nil))
  (is (amoebum::%valid-config-value-p :api-base-url "https://api.example.com"))
  (is (not (amoebum::%valid-config-value-p :api-base-url ""))))

(test valid-context-window-limit
  "Context window limit should accept nil or positive integers."
  (is (amoebum::%valid-config-value-p :context-window-limit nil))
  (is (amoebum::%valid-config-value-p :context-window-limit 128000))
  (is (not (amoebum::%valid-config-value-p :context-window-limit 0)))
  (is (not (amoebum::%valid-config-value-p :context-window-limit "big"))))

(test valid-memory-backend-values
  "Memory backend should accept known keywords."
  (dolist (backend '(:auto :file :haake-cli :haake-mcp))
    (is (amoebum::%valid-config-value-p :memory-backend backend)))
  (is (not (amoebum::%valid-config-value-p :memory-backend :sqlite))))

(test valid-sandbox-policy
  "Sandbox policy should accept known keywords."
  (is (amoebum::%valid-config-value-p :sandbox-policy :strict))
  (is (amoebum::%valid-config-value-p :sandbox-policy :off))
  (is (not (amoebum::%valid-config-value-p :sandbox-policy :medium))))

;;; --- configuration-error condition ---

(test configuration-error-condition
  "configuration-error should carry key, value, and reason."
  (handler-case
      (error 'amoebum::configuration-error
             :key :model
             :value 42
             :reason "must be a string")
    (amoebum::configuration-error (c)
      (is (eq :model (amoebum::configuration-error-key c)))
      (is (= 42 (amoebum::configuration-error-value c)))
      (is (string= "must be a string" (amoebum::configuration-error-reason c)))
      (is (stringp (princ-to-string c))))))

(test configuration-error-use-default-restart
  "Invalid config should offer use-default restart."
  (let* ((tmp-dir (%make-temp-directory "amoebum-config-validation"))
         (project-path (merge-pathnames #P"project-config.lisp" tmp-dir)))
    (unwind-protect
         (progn
           (%write-text-file project-path
                             "(configure :permission-mode :nonexistent-mode)")
           (let ((cfg (handler-bind
                          ((amoebum::configuration-error
                             (lambda (c)
                               (declare (ignore c))
                               (invoke-restart 'amoebum::use-default))))
                        (amoebum::load-config
                         :project-root "/tmp/"
                         :global-config-path "/nonexistent/g.lisp"
                         :project-config-path project-path
                         :environment-values nil
                         :cli-values nil))))
             ;; Should have fallen back to default
             (is (member (amoebum:config-value :permission-mode cfg)
                         amoebum::*known-permission-modes*))))
      (%delete-directory-tree-safe tmp-dir))))

;;; --- Runtime changes via setconfig ---

(test setconfig-updates-value
  "setconfig should update the config value."
  (let ((old-config amoebum::*current-config*)
        (old-model (amoebum:config-value :model)))
    (unwind-protect
         (progn
           (amoebum:setconfig :model "test-runtime-model")
           (is (string= "test-runtime-model" (amoebum:config-value :model)))
           (is (eq :runtime (amoebum:config-layer-source :model))))
      (amoebum:setconfig :model old-model)
      (setf amoebum::*current-config* old-config))))

(test setconfig-emits-event
  "setconfig should emit a config-changed event."
  (let* ((old-config amoebum::*current-config*)
         (old-model (amoebum:config-value :model))
         (bus (amoebum:make-event-bus))
         (events '()))
    (unwind-protect
         (let ((amoebum::*event-bus* bus))
           (amoebum:subscribe bus
                              amoebum:+event-type-config-changed+
                              (lambda (event) (push event events)))
           (amoebum:setconfig :model "event-test-model")
           (is (= 1 (length events))))
      (amoebum:setconfig :model old-model)
      (setf amoebum::*current-config* old-config))))

(test setconfig-rejects-invalid-value
  "setconfig should signal on invalid value."
  (let ((old-config amoebum::*current-config*))
    (unwind-protect
         (signals amoebum::configuration-error
           (amoebum:setconfig :permission-mode :totally-invalid))
      (setf amoebum::*current-config* old-config))))

;;; --- config-value and config-layer-source ---

(test config-value-returns-stored-value
  "config-value should return the stored value for a key."
  (let ((cfg (amoebum::load-config :project-root "/tmp/"
                                    :global-config-path "/nonexistent/g.lisp"
                                    :project-config-path "/nonexistent/p.lisp"
                                    :environment-values nil
                                    :cli-values nil)))
    (is (stringp (amoebum:config-value :model cfg)))
    (is (null (amoebum:config-value :nonexistent-key cfg)))))

(test config-layer-source-returns-layer
  "config-layer-source should return the layer that set the value."
  (let ((cfg (amoebum::load-config :project-root "/tmp/"
                                    :global-config-path "/nonexistent/g.lisp"
                                    :project-config-path "/nonexistent/p.lisp"
                                    :environment-values nil
                                    :cli-values nil)))
    (is (eq :built-in (amoebum:config-layer-source :model cfg)))
    (is (null (amoebum:config-layer-source :nonexistent-key cfg)))))
