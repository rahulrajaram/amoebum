(in-package :amoebum.internal)

;;; I370/NXT-368: keep facade installation centralized while moving the large
;;; package-surface manifests into grouped domain files under src/api-facades/.

(defparameter +amoebum-raw-api-packages+
  '(:amoebum.sandbox)
  "Packages intentionally left raw/powerful at the public surface.")

(defparameter +amoebum-wrapped-api-packages+
  '(:amoebum.ui :amoebum.commands :amoebum.workers :amoebum.config
    :amoebum.notifications :amoebum.sessions :amoebum.plan
    :amoebum.extensions :amoebum.observability :amoebum.tools
    :amoebum.safety)
  "Facade packages that now own domain-specific wrapped APIs.")

(defun %facade-symbols (source-package symbol-names)
  (let ((source (find-package source-package)))
    (loop for name in symbol-names
          for symbol = (find-symbol name source)
          unless symbol
            do (error "Unable to locate facade symbol ~A in package ~A."
                      name
                      source-package)
          collect symbol)))

(defun %ensure-symbol-in-package (symbol target-package)
  (multiple-value-bind (existing status)
      (find-symbol (symbol-name symbol) target-package)
    (cond
      ((eq existing symbol) symbol)
      ((null status) (import symbol target-package))
      (t
       (error "Facade package ~A already owns ~A via ~A."
              (package-name target-package)
              (symbol-name symbol)
              existing)))))

(defun %install-facade! (target-package symbol-names &key keep-in-root)
  "Install facade for TARGET-PACKAGE by importing SYMBOL-NAMES from :amoebum and
exporting them from TARGET-PACKAGE. Symbols whose names appear in KEEP-IN-ROOT
(a list of uppercase name strings) are also re-exported from :amoebum so that
existing callers using the amoebum: qualifier continue to work."
  (let* ((root (find-package :amoebum))
         (target (find-package target-package))
         (keep-set (when keep-in-root
                     (let ((ht (make-hash-table :test 'equal)))
                       (dolist (n keep-in-root ht)
                         (setf (gethash n ht) t)))))
         (symbols (%facade-symbols root symbol-names)))
    (dolist (symbol symbols)
      (%ensure-symbol-in-package symbol target)
      (export symbol target)
      (multiple-value-bind (_ status)
          (find-symbol (symbol-name symbol) root)
        (declare (ignore _))
        (when (eq status :external)
          (if (and keep-set (gethash (symbol-name symbol) keep-set))
              (export symbol root)
              (unexport symbol root))))))
  target-package)

(defparameter +amoebum-root-export-max+
  1242
  "No-growth ceiling for the root :amoebum export surface after the package split.")

(defparameter +amoebum-package-surface-groups+
  (list (list :group :ui
              :package :amoebum.ui
              :symbols +amoebum-ui-facade-symbol-names+
              :root-reexports +amoebum-ui-root-reexport-names+)
        (list :group :commands
              :package :amoebum.commands
              :symbols +amoebum-command-facade-symbol-names+)
        (list :group :workers
              :package :amoebum.workers
              :symbols +amoebum-worker-facade-symbol-names+
              :root-reexports +amoebum-worker-root-reexport-names+)
        (list :group :config
              :package :amoebum.config
              :symbols +amoebum-config-facade-symbol-names+)
        (list :group :notifications
              :package :amoebum.notifications
              :symbols +amoebum-notification-facade-symbol-names+)
        (list :group :sessions
              :package :amoebum.sessions
              :symbols +amoebum-session-facade-symbol-names+)
        (list :group :plan
              :package :amoebum.plan
              :symbols +amoebum-plan-facade-symbol-names+)
        (list :group :extensions
              :package :amoebum.extensions
              :symbols +amoebum-extension-facade-symbol-names+)
        (list :group :observability
              :package :amoebum.observability
              :symbols +amoebum-observability-facade-symbol-names+)
        (list :group :safety
              :package :amoebum.safety
              :symbols +amoebum-safety-facade-symbol-names+)
        (list :group :tools
              :package :amoebum.tools
              :symbols +amoebum-tools-facade-symbol-names+
              :root-reexports +amoebum-tools-root-reexport-names+))
  "Grouped package-surface manifests used by tests and package-surface audits.")

(eval-when (:load-toplevel :execute)
  (%install-facade! :amoebum.ui +amoebum-ui-facade-symbol-names+)
  (%install-facade! :amoebum.commands +amoebum-command-facade-symbol-names+)
  (%install-facade! :amoebum.workers +amoebum-worker-facade-symbol-names+
                    :keep-in-root +amoebum-worker-root-reexport-names+)
  (%install-facade! :amoebum.config +amoebum-config-facade-symbol-names+)
  (%install-facade! :amoebum.notifications +amoebum-notification-facade-symbol-names+)
  (%install-facade! :amoebum.sessions +amoebum-session-facade-symbol-names+)
  (%install-facade! :amoebum.plan +amoebum-plan-facade-symbol-names+)
  (%install-facade! :amoebum.extensions +amoebum-extension-facade-symbol-names+)
  (%install-facade! :amoebum.observability +amoebum-observability-facade-symbol-names+)
  (%install-facade! :amoebum.safety +amoebum-safety-facade-symbol-names+)
  (%install-facade! :amoebum.tools +amoebum-tools-facade-symbol-names+)
  (dolist (name '("DEFTOOL" "DEFSKILL" "DEFHOOK" "DEFKEYS" "MAIN"
                  "*TOOLSET*" "*TOOL-METADATA*" "*TOOL-HISTORY*"
                  "TOOL-METADATA-P" "TOOL-METADATA-CATEGORY"
                  "TOOL-METADATA-MCP-SERVER"
                  "TOOL-ERROR" "TOOL-ERROR-REASON" "TOOL-ERROR-REASON-CODE"
                  "TOOL-EXECUTION-ERROR" "TOOL-TIMEOUT" "TOOL-TIMEOUT-ERROR"
                  "TOOL-PERMISSION-DENIED" "TOOL-NOT-FOUND"
                  "TOOL-NOT-FOUND-ERROR" "TOOL-ARGUMENT-ERROR"
                  "TOOL-MISSING-ARGUMENT" "TOOL-TYPE-MISMATCH"
                  "HOOK-EXECUTION-ERROR"
                  "DISPATCH-SLASH-COMMAND"
                  "EXECUTE-TOOL" "EXECUTE-TOOL-WITH-RESTARTS"
                  "TOOL-EXECUTION-CONTEXT"
                  "AMOEBUM-CONTEXT" "MAKE-AMOEBUM-CONTEXT"
                  "CONTEXT-PERMISSION-MODE" "CONTEXT-HOOK-REGISTRY"
                  "CONTEXT-METRICS" "CONTEXT-TOOL-METRICS"
                  "CACHED-TOOL-RESULT"
                  "IDE-CONTEXT-P" "MAKE-IDE-CONTEXT"
                  "IDE-CONTEXT-SELECTIONS" "IDE-CONTEXT-DIAGNOSTICS"
                  "IDE-CONTEXT-OPEN-FILES" "IDE-CONTEXT-ACTIVE-FILE"
                  "IDE-CONTEXT-TIMESTAMP" "*IDE-CONTEXT*"
                  "IDE-CONTEXT-SUMMARY" "IDE-CONTEXT-TOKEN-ESTIMATE"
                  "IDE-CONTEXT-PROMPT-FRAGMENT"
                  "IDE-CONTEXT-PROMPT-FRAGMENT/BUDGET"
                  "UPDATE-IDE-CONTEXT!" "CLEAR-IDE-CONTEXT!"
                  "+EVENT-TYPE-IDE-CONTEXT-ATTACHED+"
                  "+EVENT-TYPE-IDE-CONTEXT-TRUNCATED+"
                  "+EVENT-TYPE-IDE-CONTEXT-DROPPED+"
                  "IDE-CONTEXT-ATTACHED-PAYLOAD-P"
                  "IDE-CONTEXT-ATTACHED-PAYLOAD-ACTIVE-FILE"
                  "IDE-CONTEXT-ATTACHED-PAYLOAD-OPEN-FILE-COUNT"
                  "IDE-CONTEXT-TRUNCATED-PAYLOAD-P"
                  "IDE-CONTEXT-TRUNCATED-PAYLOAD-TOKEN-ESTIMATE"
                  "IDE-CONTEXT-TRUNCATED-PAYLOAD-TOKEN-BUDGET"
                  "IDE-CONTEXT-TRUNCATED-PAYLOAD-DIAGNOSTICS-DROPPED"
                  "IDE-CONTEXT-DROPPED-PAYLOAD-P"
                  "IDE-CONTEXT-DROPPED-PAYLOAD-ACTIVE-FILE"
                  "IDE-CONTEXT-BUILD-PACKET"
                  "CULTIVAR-ADAPTER-P" "MAKE-CULTIVAR-ADAPTER"
                  "CULTIVAR-ADAPTER-ENDPOINT"
                  "CULTIVAR-ADAPTER-ENABLED-P"
                  "*CULTIVAR-ADAPTER*" "CULTIVAR-RESOLVE"
                  "CULTIVAR-LOCATION-SLICE" "CULTIVAR-SLICE"
                  "CULTIVAR-PREVIEW" "CULTIVAR-EXPAND"
                  "CULTIVAR-DAEMON-STATUS"
                  "CULTIVAR-CONTEXT-PRESSURE"
                  "YORE-ADAPTER-P" "MAKE-YORE-ADAPTER"
                  "YORE-ADAPTER-ENDPOINT" "YORE-ADAPTER-ENABLED-P"
                  "*YORE-ADAPTER*" "YORE-SEARCH-CONTEXT"
                  "YORE-FETCH-CONTEXT" "YORE-CONTEXT-PRESSURE"))
    (let ((sym (find-symbol name :amoebum)))
      (when sym
        (export sym :amoebum)))))
