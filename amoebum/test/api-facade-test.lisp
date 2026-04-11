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
              :symbols amoebum.internal::+amoebum-worker-facade-symbol-names+
              :root-reexports amoebum.internal::+amoebum-worker-root-reexport-names+)
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
              :symbols amoebum.internal::+amoebum-observability-facade-symbol-names+)
        (list :package :amoebum.safety
              :symbols amoebum.internal::+amoebum-safety-facade-symbol-names+)
        (list :package :amoebum.tools
              :symbols amoebum.internal::+amoebum-tools-facade-symbol-names+
              :root-reexports '("*CULTIVAR-ADAPTER*"
                                "*IDE-CONTEXT*"
                                "*TOOL-HISTORY*"
                                "*TOOL-METADATA*"
                                "*TOOLSET*"
                                "*YORE-ADAPTER*"
                                "+EVENT-TYPE-IDE-CONTEXT-ATTACHED+"
                                "+EVENT-TYPE-IDE-CONTEXT-DROPPED+"
                                "+EVENT-TYPE-IDE-CONTEXT-TRUNCATED+"
                                "AMOEBUM-CONTEXT"
                                "CACHED-TOOL-RESULT"
                                "CONTEXT-HOOK-REGISTRY"
                                "CONTEXT-METRICS"
                                "CONTEXT-PERMISSION-MODE"
                                "CONTEXT-TOOL-METRICS"
                                "CULTIVAR-ADAPTER-ENABLED-P"
                                "CULTIVAR-ADAPTER-ENDPOINT"
                                "CULTIVAR-ADAPTER-P"
                                "CULTIVAR-CONTEXT-PRESSURE"
                                "CULTIVAR-DAEMON-STATUS"
                                "CULTIVAR-EXPAND"
                                "CULTIVAR-LOCATION-SLICE"
                                "CULTIVAR-PREVIEW"
                                "CULTIVAR-RESOLVE"
                                "CULTIVAR-SLICE"
                                "DISPATCH-SLASH-COMMAND"
                                "EXECUTE-TOOL"
                                "EXECUTE-TOOL-WITH-RESTARTS"
                                "HOOK-EXECUTION-ERROR"
                                "IDE-CONTEXT-ACTIVE-FILE"
                                "IDE-CONTEXT-ATTACHED-PAYLOAD-ACTIVE-FILE"
                                "IDE-CONTEXT-ATTACHED-PAYLOAD-OPEN-FILE-COUNT"
                                "IDE-CONTEXT-ATTACHED-PAYLOAD-P"
                                "IDE-CONTEXT-BUILD-PACKET"
                                "IDE-CONTEXT-DIAGNOSTICS"
                                "CLEAR-IDE-CONTEXT!"
                                "UPDATE-IDE-CONTEXT!"
                                "IDE-CONTEXT-DROPPED-PAYLOAD-ACTIVE-FILE"
                                "IDE-CONTEXT-DROPPED-PAYLOAD-P"
                                "IDE-CONTEXT-OPEN-FILES"
                                "IDE-CONTEXT-P"
                                "IDE-CONTEXT-PROMPT-FRAGMENT"
                                "IDE-CONTEXT-PROMPT-FRAGMENT/BUDGET"
                                "IDE-CONTEXT-SELECTIONS"
                                "IDE-CONTEXT-SUMMARY"
                                "IDE-CONTEXT-TIMESTAMP"
                                "IDE-CONTEXT-TOKEN-ESTIMATE"
                                "IDE-CONTEXT-TRUNCATED-PAYLOAD-DIAGNOSTICS-DROPPED"
                                "IDE-CONTEXT-TRUNCATED-PAYLOAD-P"
                                "IDE-CONTEXT-TRUNCATED-PAYLOAD-TOKEN-BUDGET"
                                "IDE-CONTEXT-TRUNCATED-PAYLOAD-TOKEN-ESTIMATE"
                                "MAKE-AMOEBUM-CONTEXT"
                                "MAKE-CULTIVAR-ADAPTER"
                                "MAKE-IDE-CONTEXT"
                                "MAKE-YORE-ADAPTER"
                                "TOOL-ARGUMENT-ERROR"
                                "TOOL-ERROR"
                                "TOOL-ERROR-REASON"
                                "TOOL-ERROR-REASON-CODE"
                                "TOOL-EXECUTION-CONTEXT"
                                "TOOL-EXECUTION-ERROR"
                                "TOOL-METADATA-CATEGORY"
                                "TOOL-METADATA-MCP-SERVER"
                                "TOOL-METADATA-P"
                                "TOOL-MISSING-ARGUMENT"
                                "TOOL-NOT-FOUND"
                                "TOOL-NOT-FOUND-ERROR"
                                "TOOL-PERMISSION-DENIED"
                                "TOOL-TIMEOUT"
                                "TOOL-TIMEOUT-ERROR"
                                "TOOL-TYPE-MISMATCH"
                                "YORE-ADAPTER-ENABLED-P"
                                "YORE-ADAPTER-ENDPOINT"
                                "YORE-ADAPTER-P"
                                "YORE-CONTEXT-PRESSURE"
                                "YORE-FETCH-CONTEXT"
                                "YORE-SEARCH-CONTEXT"))))

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
  (is (< (%external-symbol-count :amoebum) 1250))
  (is (> (%external-symbol-count :amoebum.ui) 140))
  (is (> (%external-symbol-count :amoebum.commands) 30))
  (is (> (%external-symbol-count :amoebum.workers) 239))
  (is (find-package :amoebum.tools))
  (is (> (%external-symbol-count :amoebum.tools) 50))
  (is (> (%external-symbol-count :amoebum.config) 20))
  (is (> (%external-symbol-count :amoebum.notifications) 130))
  (is (> (%external-symbol-count :amoebum.sessions) 60))
  (is (> (%external-symbol-count :amoebum.plan) 40))
  (is (> (%external-symbol-count :amoebum.extensions) 30))
  (is (> (%external-symbol-count :amoebum.observability) 80))
  (is (> (%external-symbol-count :amoebum.safety) 30)))

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
