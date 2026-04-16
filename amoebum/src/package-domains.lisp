(defpackage :amoebum.sandbox
  (:use)
  (:import-from :cl
                #:+
                #:-
                #:*
                #:/
                #:<
                #:<=
                #:=
                #:>
                #:>=
                #:1+
                #:1-
                #:abs
                #:and
                #:append
                #:aref
                #:car
                #:cdr
                #:cadr
                #:caddr
                #:case
                #:coerce
                #:cond
                #:cons
                #:copy-list
                #:copy-seq
                #:defparameter
                #:defun
                #:destructuring-bind
                #:dolist
                #:dotimes
                #:eq
                #:equal
                #:eql
                #:error
                #:evenp
                #:every
                #:find
                #:first
                #:format
                #:fourth
                #:funcall
                #:if
                #:incf
                #:labels
                #:lambda
                #:last
                #:length
                #:let
                #:list
                #:list*
                #:loop
                #:mapcar
                #:max
                #:member
                #:min
                #:mod
                #:nconc
                #:not
                #:nth
                #:null
                #:numberp
                #:oddp
                #:or
                #:pop
                #:position
                #:progn
                #:push
                #:reduce
                #:remove
                #:rest
                #:reverse
                #:second
                #:setf
                #:some
                #:sort
                #:string
                #:string-downcase
                #:string-upcase
                #:string=
                #:subseq
                #:svref
                #:third
                #:unless
                #:values
                #:vector
                #:when)
  (:import-from :amoebum
                #:+sandbox-max-output-size+
                #:+sandbox-max-read-size+
                #:safe-open
                #:safe-run-program
                #:truncate-sandbox-output)
  (:export #:+sandbox-max-output-size+
           #:+sandbox-max-read-size+
           #:safe-open
           #:safe-run-program
           #:truncate-sandbox-output
           ;; Read orchestration (I105)
           #:read-orchestration-error
           #:validate-read-arguments
           #:orchestrate-read
           #:orchestrate-read-via-pipeline
           #:format-read-error-for-user
           #:+event-type-read-orchestration-cache+
           #:clear-read-orchestration-cache
           #:read-orchestration-cache-metrics
           #:*read-orchestration-max-line-limit*
           #:*read-orchestration-max-file-size-bytes*
           #:*read-orchestration-supported-extensions*))

(defpackage :amoebum.tools
  (:use :cl))

(defpackage :amoebum.config
  (:use :cl))

(defpackage :amoebum.notifications
  (:use :cl))

(defpackage :amoebum.sessions
  (:use :cl))

(defpackage :amoebum.plan
  (:use :cl))

(defpackage :amoebum.extensions
  (:use :cl))

(defpackage :amoebum.observability
  (:use :cl))

(defpackage :amoebum.commands
  (:use :cl)
  (:import-from :amoebum
                #:make-slash-command-result)
  (:export #:make-slash-command-result))

(defpackage :amoebum.commands.plan
  (:use :cl)
  (:import-from :amoebum
                #:approve-plan-steps
                #:clear-plan-step-approvals
                #:current-plan-mode-state
                #:enter-plan-mode
                #:exit-plan-mode
                #:make-slash-command-result
                #:plan-input-gating-snapshot
                #:plan-mode-active-p
                #:plan-mode-state-approved-step-indexes
                #:plan-mode-state-last-output-path
                #:plan-mode-state-last-plan-markdown
                #:plan-mode-state-review-decision
                #:plan-mode-state-review-last-presented-at
                #:plan-mode-state-review-notes
                #:plan-mode-state-review-pending-p
                #:plan-mode-state-steps
                #:plan-step-indexes
                #:refresh-plan-review-markdown
                #:reorder-plan-step
                #:set-plan-review-decision
                #:set-plan-step-approvals
                #:setconfig
                #:toggle-plan-mode)
  (:export #:%plan-command-handler))

(defpackage :amoebum.commands.history
  (:use :cl)
  (:import-from :amoebum.commands
                #:make-slash-command-result)
  (:export #:%history-handler))

(defpackage :amoebum.commands.index
  (:use :cl)
  (:import-from :amoebum.commands
                #:make-slash-command-result)
  (:export #:%index-handler))

(defpackage :amoebum.commands.self-modify
  (:use :cl)
  (:import-from :amoebum.commands
                #:make-slash-command-result)
  (:export #:%self-modify-handler))

(defpackage :amoebum.commands.permissions
  (:use :cl)
  (:import-from :amoebum.commands
                #:make-slash-command-result)
  (:export #:%permissions-handler
           #:%permissions-arg-completer))

(defpackage :amoebum.ui
  (:use :cl))

(defpackage :amoebum.workers
  (:use :cl))

(defpackage :amoebum.safety
  (:use :cl))

(defpackage :amoebum.internal
  (:use :cl))
