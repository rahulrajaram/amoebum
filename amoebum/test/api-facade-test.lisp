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

(test amoebum-root-surface-is-smaller-and-facades-own-moved-families
  (is (< (%external-symbol-count :amoebum) 1700))
  (is (> (%external-symbol-count :amoebum.ui) 40))
  (is (> (%external-symbol-count :amoebum.commands) 30))
  (is (> (%external-symbol-count :amoebum.workers) 30))
  (is (> (%external-symbol-count :amoebum.config) 20))
  (is (> (%external-symbol-count :amoebum.notifications) 80))
  (is (> (%external-symbol-count :amoebum.sessions) 60))
  (is (> (%external-symbol-count :amoebum.plan) 40))
  (is (> (%external-symbol-count :amoebum.extensions) 30))

  (dolist (symbol-name '("MAKE-CHAT-UI-STATE"
                         "STATUS-BAR-LINE"
                         "LOAD-YAML-THEME"))
    (is-false (%external-symbol-p :amoebum symbol-name))
    (is-true (%external-symbol-p :amoebum.ui symbol-name)))

  (dolist (symbol-name '("MAKE-SLASH-COMMAND"
                         "MAKE-SLASH-COMMAND-PARAMETER"
                         "SLASH-COMMAND-RESULT-OUTPUT"))
    (is-false (%external-symbol-p :amoebum symbol-name))
    (is-true (%external-symbol-p :amoebum.commands symbol-name)))

  (dolist (symbol-name '("WORKER-RECORD-ID"
                         "WORKER-LIST"
                         "WORKER-STATUS-BAR-SEGMENT"))
    (is-false (%external-symbol-p :amoebum symbol-name))
    (is-true (%external-symbol-p :amoebum.workers symbol-name)))

  (dolist (symbol-name '("CURRENT-CONFIG"
                         "SETCONFIG"
                         "RESOLVE-PROVIDER"))
    (is-false (%external-symbol-p :amoebum symbol-name))
    (is-true (%external-symbol-p :amoebum.config symbol-name)))

  (dolist (symbol-name '("MAKE-NOTIFICATION"
                         "SEND-DESKTOP-NOTIFICATION"
                         "SOUND-BACKEND-KIND"))
    (is-false (%external-symbol-p :amoebum symbol-name))
    (is-true (%external-symbol-p :amoebum.notifications symbol-name)))

  (dolist (symbol-name '("CONVERSATION-SAVE"
                         "CHECKPOINT-SESSION"
                         "START-SESSION"
                         "EXPORT-CONVERSATION"))
    (is-false (%external-symbol-p :amoebum symbol-name))
    (is-true (%external-symbol-p :amoebum.sessions symbol-name)))

  (dolist (symbol-name '("ENTER-PLAN-MODE"
                         "CURRENT-PLAN-MODE-STATE"
                         "EXECUTE-APPROVED-PLAN-STEPS"))
    (is-false (%external-symbol-p :amoebum symbol-name))
    (is-true (%external-symbol-p :amoebum.plan symbol-name)))

  (dolist (symbol-name '("LOAD-USER-EXTENSIONS"
                         "LIST-EXTENSION-REGISTRY"
                         "EXTENSION-LOAD-RECORD-STATUS"))
    (is-false (%external-symbol-p :amoebum symbol-name))
    (is-true (%external-symbol-p :amoebum.extensions symbol-name))))
