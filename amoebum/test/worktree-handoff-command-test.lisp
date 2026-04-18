(in-package :amoebum/test)

(test worktree-handoff-command-lists-inspects-and-updates-conflicts
  "The /worktree-handoff slash command should expose the full operator resolution lifecycle."
  (unwind-protect
      (progn
        (amoebum:clear-worktree-conflict-handoffs)
        (let* ((snapshot (amoebum:create-worktree-conflict-handoff
                          :worktree (amoebum:make-worktree-metadata
                                     :id "wt-suite-handoff"
                                     :branch "sw4rm/suite/node"
                                     :path "/tmp/wt-suite-handoff/")
                          :target-ref "sw4rm/suite"
                          :preflight '(:status :conflict
                                       :conflicts ("README.md")
                                       :conflict-kind :file-overlap)
                          :agent-id "swarm-suite"
                          :backend :swarm
                          :task "suite handoff"
                          :result '(:status :completed)))
               (handoff-id (getf snapshot :handoff-id)))
          (multiple-value-bind (handled list-result)
              (amoebum:dispatch-slash-command "/worktree-handoff list")
            (is-true handled)
            (let ((output (amoebum.commands:slash-command-result-output list-result)))
              (is (search "Worktree conflict handoffs (1):" output))
              (is (search handoff-id output))))
          (multiple-value-bind (handled inspect-result)
              (amoebum:dispatch-slash-command
               (format nil "/worktree-handoff inspect ~A" handoff-id))
            (is-true handled)
            (let ((output (amoebum.commands:slash-command-result-output inspect-result)))
              (is (search "conflicts: README.md" output))))
          (multiple-value-bind (handled accept-result)
              (amoebum:dispatch-slash-command
               (format nil "/worktree-handoff accept ~A taking ownership" handoff-id))
            (is-true handled)
            (is (search "Accepted worktree handoff"
                        (amoebum.commands:slash-command-result-output accept-result))))
          (let ((accepted (amoebum:find-worktree-conflict-handoff handoff-id)))
            (is (eq :accepted (getf accepted :status)))
            (is (eq :active (getf (getf accepted :resolution) :status))))
          (multiple-value-bind (handled inspect-result)
              (amoebum:dispatch-slash-command
               (format nil "/worktree-handoff inspect ~A" handoff-id))
            (is-true handled)
            (let ((output (amoebum.commands:slash-command-result-output inspect-result)))
              (is (search "resolution: active owned by operator" output))))
          (multiple-value-bind (handled resolve-result)
              (amoebum:dispatch-slash-command
               (format nil "/worktree-handoff resolve ~A merged manually" handoff-id))
            (is-true handled)
            (is (search "Resolved worktree handoff"
                        (amoebum.commands:slash-command-result-output resolve-result))))
          (let ((updated (amoebum:find-worktree-conflict-handoff handoff-id)))
            (is (eq :resolved (getf updated :status)))
            (is (eq :resolved (getf (getf updated :resolution) :status)))
            (is (string= "merged manually" (getf updated :note)))))
        (let* ((abandon-snapshot (amoebum:create-worktree-conflict-handoff
                                  :worktree (amoebum:make-worktree-metadata
                                             :id "wt-suite-abandon"
                                             :branch "sw4rm/suite/abandon"
                                             :path "/tmp/wt-suite-abandon/")
                                  :target-ref "sw4rm/suite"
                                  :preflight '(:status :conflict
                                               :conflicts ("guide.md")
                                               :conflict-kind :file-overlap)
                                  :agent-id "swarm-suite"
                                  :backend :swarm
                                  :task "suite abandon"
                                  :result '(:status :completed)))
               (abandon-handoff-id (getf abandon-snapshot :handoff-id)))
          (amoebum:dispatch-slash-command
           (format nil "/worktree-handoff accept ~A taking ownership"
                   abandon-handoff-id))
          (multiple-value-bind (handled abandon-result)
              (amoebum:dispatch-slash-command
               (format nil "/worktree-handoff abandon ~A operator declined"
                       abandon-handoff-id))
            (is-true handled)
            (is (search "Abandoned worktree handoff"
                        (amoebum.commands:slash-command-result-output abandon-result))))
          (let ((updated (amoebum:find-worktree-conflict-handoff
                          abandon-handoff-id)))
            (is (eq :abandoned (getf updated :status)))
            (is (eq :abandoned (getf (getf updated :resolution) :status)))
            (is (string= "operator declined" (getf updated :note))))))
    (amoebum:clear-worktree-conflict-handoffs)))

(test worktree-handoff-command-toggles-panel-visibility
  "The /worktree-handoff panel subcommand should toggle the PTUI worktree handoff surface."
  (let ((old-state amoebum::*worktree-handoff-dashboard-state*))
    (unwind-protect
         (progn
           (setf amoebum::*worktree-handoff-dashboard-state* nil)
           (multiple-value-bind (handled on-result)
               (amoebum:dispatch-slash-command "/worktree-handoff panel on")
             (is-true handled)
             (is (search "visible"
                         (amoebum.commands:slash-command-result-output on-result)
                         :test #'char-equal))
             (is-true (amoebum::worktree-handoff-dashboard-visible-p)))
           (multiple-value-bind (handled off-result)
               (amoebum:dispatch-slash-command "/worktree-handoff panel off")
             (is-true handled)
             (is (search "hidden"
                         (amoebum.commands:slash-command-result-output off-result)
                         :test #'char-equal))
             (is-false (amoebum::worktree-handoff-dashboard-visible-p))))
      (setf amoebum::*worktree-handoff-dashboard-state* old-state))))
