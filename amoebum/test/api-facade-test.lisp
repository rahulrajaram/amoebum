(in-package :amoebum/test)

(def-suite api-facade-suite
  :description "Public package facade regressions for I370."
  :in amoebum-suite)

(in-suite api-facade-suite)

(defun %external-symbol-p (package-name symbol-name)
  (multiple-value-bind (symbol status)
      (find-symbol symbol-name (find-package package-name))
    (and symbol (eq status :external))))

(defun %external-symbol-count (package-name)
  (let ((count 0))
    (do-external-symbols (_ (find-package package-name) count)
      (declare (ignore _))
      (incf count))))

(defun %sorted-unique-symbol-names (symbol-names)
  (sort (remove-duplicates (copy-list symbol-names) :test #'string=) #'string<))

(defun %external-symbol-names (package-name)
  (let ((names '()))
    (do-external-symbols (symbol (find-package package-name) (%sorted-unique-symbol-names names))
      (push (symbol-name symbol) names))))

(defun %facade-specs ()
  (list (list :package :amoebum.ui
              :symbols amoebum.internal::+amoebum-ui-facade-symbol-names+
              :root-reexports '("MAKE-APPROVAL-DIALOG-STATE"
                                "PROVIDER-HEALTH-ENTRIES"
                                "CHAT-UI-BUILD-TREE"
                                "RUN-CHAT-UI"
                                "ENSURE-WORKER-DASHBOARD-STATE"))
        (list :package :amoebum.commands
              :symbols amoebum.internal::+amoebum-command-facade-symbol-names+)
        (list :package :amoebum.workers
              :symbols amoebum.internal::+amoebum-worker-facade-symbol-names+)
        (list :package :amoebum.config
              :symbols amoebum.internal::+amoebum-config-facade-symbol-names+)
        (list :package :amoebum.notifications
              :symbols amoebum.internal::+amoebum-notification-facade-symbol-names+)
        (list :package :amoebum.sessions
              :symbols amoebum.internal::+amoebum-session-facade-symbol-names+)
        (list :package :amoebum.plan
              :symbols amoebum.internal::+amoebum-plan-facade-symbol-names+)
        (list :package :amoebum.extensions
              :symbols amoebum.internal::+amoebum-extension-facade-symbol-names+)
        (list :package :amoebum.observability
              :symbols amoebum.internal::+amoebum-observability-facade-symbol-names+
              :root-reexports :all)))

(defun %assert-facade-package-exports-expected-surface (package-name symbol-names)
  (let* ((expected (%sorted-unique-symbol-names symbol-names))
         (actual (%external-symbol-names package-name)))
    (is (equal expected actual))))

(defun %assert-root-surface-contract (symbol-names root-reexports)
  (let ((allowed (if (eq root-reexports :all)
                     (%sorted-unique-symbol-names symbol-names)
                     (%sorted-unique-symbol-names root-reexports))))
    (dolist (symbol-name (%sorted-unique-symbol-names symbol-names))
      (if (member symbol-name allowed :test #'string=)
          (is-true (%external-symbol-p :amoebum symbol-name))
          (is-false (%external-symbol-p :amoebum symbol-name))))))

(test amoebum-root-surface-is-smaller-and-facades-own-moved-families
  (is (< (%external-symbol-count :amoebum) 1700))
  (is (> (%external-symbol-count :amoebum.ui) 60))
  (is (> (%external-symbol-count :amoebum.commands) 30))
  (is (> (%external-symbol-count :amoebum.workers) 30))
  (is (find-package :amoebum.tools))
  (is (> (%external-symbol-count :amoebum.config) 20))
  (is (> (%external-symbol-count :amoebum.notifications) 80))
  (is (> (%external-symbol-count :amoebum.sessions) 60))
  (is (> (%external-symbol-count :amoebum.plan) 40))
  (is (> (%external-symbol-count :amoebum.extensions) 30))
  (is (> (%external-symbol-count :amoebum.observability) 80)))

(test installed-facade-packages-export-their-full-declared-surfaces
  (dolist (spec (%facade-specs))
    (%assert-facade-package-exports-expected-surface
     (getf spec :package)
     (getf spec :symbols))))

(test root-package-only-keeps-intended-facade-compatibility-reexports
  (dolist (spec (%facade-specs))
    (%assert-root-surface-contract
     (getf spec :symbols)
     (getf spec :root-reexports '()))))

(test legacy-tools-compatibility-surface-remains-available

  (dolist (symbol-name '("MAKE-CHAT-UI-STATE"
                         "STATUS-BAR-LINE"
                         "LOAD-YAML-THEME"))
    (is-false (%external-symbol-p :amoebum symbol-name))
    (is-true (%external-symbol-p :amoebum.ui symbol-name)))

  (dolist (symbol-name '("DEFTOOL" "DEFSKILL"))
    (is-true (%external-symbol-p :amoebum symbol-name))
    (is-false (%external-symbol-p :amoebum.tools symbol-name))))
