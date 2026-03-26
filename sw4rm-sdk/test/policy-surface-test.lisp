(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :sw4rm-test)
    (defpackage :sw4rm-test
      (:use :cl :fiveam :sw4rm-sdk))))

(in-package :sw4rm-test)

(def-suite policy-surface-suite
  :description "SW4RM domain facades and worktree transition plans."
  :in sw4rm-suite)

(in-suite policy-surface-suite)

(test facade-packages-export-domain-surfaces
  (let ((worktree-package (find-package :sw4rm-sdk.worktree))
        (interceptor-package (find-package :sw4rm-sdk.interceptors))
        (workflow-package (find-package :sw4rm-sdk.workflow))
        (handoff-package (find-package :sw4rm-sdk.handoff)))
    (is-true worktree-package)
    (is-true interceptor-package)
    (is-true workflow-package)
    (is-true handoff-package)
    (is (eq :external (nth-value 1 (find-symbol "MAKE-WORKTREE-STATE-MACHINE" worktree-package))))
    (is (eq :external (nth-value 1 (find-symbol "PROCESS-REQUEST" interceptor-package))))
    (is (eq :external (nth-value 1 (find-symbol "MAKE-WORKFLOW-ENGINE" workflow-package))))
    (is (eq :external (nth-value 1 (find-symbol "DELEGATE-TO-SWARM" handoff-package))))))

(test evaluate-worktree-transition-returns-explicit-request-switch-plan
  (let ((wsm (sw4rm-sdk.worktree:make-worktree-state-machine :state :bound-home)))
    (let ((plan (sw4rm-sdk::evaluate-worktree-transition
                 wsm
                 :request-switch
                 :worktree-id "wt-2"
                 :repo-id "repo-1"
                 :branch "feature/demo")))
      (is (typep plan 'sw4rm-sdk::worktree-transition-plan))
      (is (eq :switch-pending
              (sw4rm-sdk::worktree-transition-plan-to-state plan)))
      (let ((pending (getf (sw4rm-sdk::worktree-transition-plan-slot-updates plan)
                           :pending-switch)))
        (is (typep pending 'sw4rm-sdk::binding-info))
        (is (string= "wt-2" (sw4rm-sdk::binding-info-worktree-id pending)))
        (is (string= "feature/demo" (sw4rm-sdk::binding-info-branch pending)))))))

(test worktree-transition-plan-application-preserves-lifecycle-behavior
  (let* ((home (sw4rm-sdk::make-binding-info
                :worktree-id "home"
                :repo-id "repo-1"
                :branch "main"
                :bound-at 10))
         (wsm (sw4rm-sdk.worktree:make-worktree-state-machine
               :state :bound-home
               :home-binding home
               :current-binding home)))
    (sw4rm-sdk.worktree:request-switch wsm "wt-2" "repo-1" "feature/demo")
    (is (eq :switch-pending (sw4rm-sdk::worktree-state wsm)))
    (is (string= "wt-2"
                 (sw4rm-sdk::binding-info-worktree-id
                  (sw4rm-sdk.worktree:get-pending-switch wsm))))
    (sw4rm-sdk.worktree:approve-switch-local wsm)
    (is (eq :bound-non-home (sw4rm-sdk::worktree-state wsm)))
    (is (string= "wt-2"
                 (sw4rm-sdk::binding-info-worktree-id
                  (sw4rm-sdk.worktree:get-current-worktree wsm))))
    (sw4rm-sdk.worktree:revert-to-home wsm)
    (is (eq :bound-home (sw4rm-sdk::worktree-state wsm)))
    (is (string= "home"
                 (sw4rm-sdk::binding-info-worktree-id
                  (sw4rm-sdk.worktree:get-current-worktree wsm))))))
