(in-package :amoebum)

;;; I237: multi-layer configuration loading and merging.

(defparameter *config-layer-order*
  '(:built-in :global :project :directory :env :cli))

(defparameter *config-list-merge-keys*
  '(:web-search-allow-domains
    :web-search-block-domains
    :notification-events
    :notification-webhooks))

(defun %list-merge-key-p (key)
  (member key *config-list-merge-keys* :test #'eq))

(defun %list-value->list (value)
  (cond
    ((null value) '())
    ((stringp value) (list value))
    ((listp value) (copy-list value))
    ((vectorp value) (coerce value 'list))
    (t (list value))))

(defun %parse-list-merge-directive (value)
  (when (and (consp value)
             (member (car value) '(:append :prepend) :test #'eq)
             (cdr value))
    (let ((operation (car value))
          (tail (cdr value)))
      (values operation
              (cond
                ((and (= (length tail) 1)
                      (or (listp (car tail))
                          (vectorp (car tail))))
                 (%list-value->list (car tail)))
                (t
                 (%list-value->list tail)))
              t))))

(defun %resolve-layer-value (cfg key value)
  (if (%list-merge-key-p key)
      (multiple-value-bind (operation payload directive-p)
          (%parse-list-merge-directive value)
        (if directive-p
            (let ((existing (%list-value->list (gethash key (config-values cfg)))))
              (case operation
                (:append (append existing payload))
                (:prepend (append payload existing))
                (otherwise value)))
            value))
      value))

(defun %merge-value (cfg key value source project-root)
  (let* ((candidate (%resolve-layer-value cfg key value))
         (validated
           (if (%valid-config-value-p key candidate)
               candidate
               (%signal-invalid-value key candidate "failed validation" project-root)))
         (final-value
           (cond
             ((eq key :project-root)
              (%normalize-project-root validated))
             ((eq key :sandbox-policy)
              (%sandbox-policy-keyword validated))
             ((eq key :approval-policy)
              (%approval-policy-keyword validated))
             ((eq key :sandbox-mode)
              (%sandbox-mode-keyword validated))
             ((eq key :swarm-delegation-mode)
              (%swarm-delegation-mode-keyword validated))
             (t
              validated))))
    (setf (gethash key (config-values cfg)) final-value
          (gethash key (config-sources cfg)) source)
    (case key
      (:model (setf (config-model cfg) final-value))
      (:permission-mode (setf (config-permission-mode cfg) final-value))
      (:memory-backend (setf (config-memory-backend cfg) final-value))
      (:project-root (setf (config-project-root cfg) final-value))
      (:theme-yaml (when (fboundp 'yaml-theme-config-handler)
                     (yaml-theme-config-handler final-value project-root))))
    cfg))

(defun %apply-layer-values (cfg layer-values layer-source project-root)
  (dolist (key (%hash-keys layer-values))
    (%merge-value cfg key (gethash key layer-values) layer-source project-root))
  cfg)

(defun load-config (&key
                      project-root
                      global-config-path
                      project-config-path
                      directory-root
                      directory-config-path
                      (environment-values :not-supplied)
                      cli-values
                      cli-arguments)
  (let* ((root (%normalize-project-root project-root))
         (cfg (%base-config :project-root root))
         (global-values (%load-layer (or global-config-path (%global-config-path))))
         (project-path (or project-config-path (%project-config-path root)))
         (project-values (%load-layer project-path))
         (resolved-directory-path
           (or directory-config-path
               (%directory-config-path :directory-root (or directory-root *default-pathname-defaults*))))
         (effective-directory-path
           (if (%paths-equal-p project-path resolved-directory-path)
               nil
               resolved-directory-path))
         (directory-values (%load-layer effective-directory-path))
         (env-values
           (if (eq environment-values :not-supplied)
               (%environment-config-values)
               (%coerce-layer-values environment-values)))
         (cli-layer-values
           (cond
             (cli-values (%coerce-layer-values cli-values))
             (cli-arguments (%cli-argument-values cli-arguments))
             (t (make-hash-table :test 'eq)))))
    (%apply-layer-values cfg global-values :global root)
    (%apply-layer-values cfg project-values :project root)
    (%apply-layer-values cfg directory-values :directory root)
    (%apply-layer-values cfg env-values :env root)
    (%apply-layer-values cfg cli-layer-values :cli root)
    cfg))

(defun reload-config (&key
                        project-root
                        global-config-path
                        project-config-path
                        directory-root
                        directory-config-path
                        (environment-values :not-supplied)
                        cli-values
                        cli-arguments)
  (setf *current-config*
        (load-config :project-root project-root
                     :global-config-path global-config-path
                     :project-config-path project-config-path
                     :directory-root directory-root
                     :directory-config-path directory-config-path
                     :environment-values environment-values
                     :cli-values cli-values
                     :cli-arguments cli-arguments))
  (when (fboundp 'clear-resolved-provider-cache)
    (clear-resolved-provider-cache))
  *current-config*)
