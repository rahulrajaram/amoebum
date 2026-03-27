(in-package :sw4rm-sdk.internal)

;;; I373: curated SW4RM domain packages that sit alongside the compatibility
;;; root package. The root surface remains intact; the facades make ownership
;;; by domain easier to navigate.

(defparameter +sw4rm-handoff-facade-symbol-names+
  '("HANDOFF-CLIENT"
    "HANDOFF-REQUEST"
    "MAKE-HANDOFF-REQUEST"
    "HANDOFF-REQUEST-REQUEST-ID"
    "HANDOFF-REQUEST-FROM-AGENT"
    "HANDOFF-REQUEST-TO-AGENT"
    "HANDOFF-REQUEST-REASON"
    "HANDOFF-REQUEST-BUDGET"
    "HANDOFF-REQUEST-DELEGATION-POLICY"
    "HANDOFF-REQUEST-CONTEXT-SNAPSHOT"
    "HANDOFF-REQUEST-CAPABILITIES-REQUIRED"
    "HANDOFF-REQUEST-PRIORITY"
    "HANDOFF-REQUEST-TIMEOUT-MS"
    "HANDOFF-DEFAULT-DELEGATION-POLICY"
    "DELEGATE-TO-SWARM"
    "INITIATE-HANDOFF"
    "ACCEPT-HANDOFF"
    "REJECT-HANDOFF"
    "REJECT-HANDOFF-WITH-OPTIONS"
    "COMPLETE-HANDOFF"
    "GET-PENDING-HANDOFFS"
    "GET-HANDOFF-STATUS"
    "REGISTER-CHILD-DELEGATION"
    "CANCEL-DELEGATION"
    "CANCELLED-DELEGATION-P"
    "CANCELLATION-GRACE-EXPIRED-P"
    "COLLECT-FORCED-PREEMPTIONS"
    "SERIALIZE-HANDOFF-CONTEXT"
    "DESERIALIZE-HANDOFF-CONTEXT"
    "HANDOFF-REJECTED"))

(defparameter +sw4rm-worktree-facade-symbol-names+
  '("WORKTREE-STATE-MACHINE"
    "MAKE-WORKTREE-STATE-MACHINE"
    "VALID-WORKTREE-TRANSITIONS"
    "VALID-WORKTREE-TRANSITION-P"
    "BIND-WORKTREE"
    "UNBIND-WORKTREE"
    "REQUEST-SWITCH"
    "APPROVE-SWITCH-LOCAL"
    "REJECT-SWITCH-LOCAL"
    "REVERT-TO-HOME"
    "CHECK-TTL-EXPIRY"
    "GET-CURRENT-WORKTREE"
    "GET-HOME-WORKTREE"
    "GET-PENDING-SWITCH"))

(defparameter +sw4rm-workflow-facade-symbol-names+
  '("WORKFLOW-NODE"
    "WORKFLOW-EDGE"
    "WORKFLOW-DEFINITION"
    "WORKFLOW-RUN"
    "WORKFLOW-ENGINE"
    "MAKE-WORKFLOW-ENGINE"
    "ADD-NODE"
    "ADD-EDGE"
    "TOPOLOGICAL-SORT"
    "EXECUTE-WORKFLOW"
    "SERIALIZE-WORKFLOW-STATE"
    "RESTORE-WORKFLOW-STATE"
    "REGISTER-WORKFLOW"
    "RUN-WORKFLOW"
    "GET-WORKFLOW-RUN"
    "LIST-WORKFLOW-RUNS"
    "DEFWORKFLOW"
    "MAKE-FEATURE-WORKFLOW-TEMPLATE"
    "MAKE-BUGFIX-WORKFLOW-TEMPLATE"
    "MAKE-REFACTOR-WORKFLOW-TEMPLATE"))

(defparameter +sw4rm-interceptor-facade-symbol-names+
  '("INTERCEPTOR-CHAIN"
    "MAKE-INTERCEPTOR-CHAIN"
    "ADD-INTERCEPTOR"
    "ADD-INTERCEPTOR-FIRST"
    "REMOVE-INTERCEPTOR"
    "CLEAR-INTERCEPTORS"
    "PROCESS-REQUEST"
    "PROCESS-RESPONSE"
    "TIMING-INTERCEPTOR"
    "LOGGING-INTERCEPTOR"
    "GET-TIMINGS"
    "GET-AVERAGE-DURATION"))

(defun %facade-symbols (source-package symbol-names)
  (let ((source (find-package source-package)))
    (loop for name in symbol-names
          for symbol = (find-symbol name source)
          unless symbol
            do (error "Unable to locate facade symbol ~A in package ~A." name source-package)
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

(defun %install-facade! (target-package symbol-names)
  (let* ((root (find-package :sw4rm-sdk))
         (target (find-package target-package))
         (symbols (%facade-symbols root symbol-names)))
    (dolist (symbol symbols)
      (%ensure-symbol-in-package symbol target)
      (export symbol target)))
  target-package)

(eval-when (:load-toplevel :execute)
  (%install-facade! :sw4rm-sdk.handoff +sw4rm-handoff-facade-symbol-names+)
  (%install-facade! :sw4rm-sdk.worktree +sw4rm-worktree-facade-symbol-names+)
  (%install-facade! :sw4rm-sdk.workflow +sw4rm-workflow-facade-symbol-names+)
  (%install-facade! :sw4rm-sdk.interceptors +sw4rm-interceptor-facade-symbol-names+))
