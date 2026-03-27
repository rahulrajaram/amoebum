(in-package :amoebum/test)

(def-suite permission-command-matching-suite
  :in amoebum-suite
  :description "Command-pattern permission matching tests (I132/I351).")

(in-suite permission-command-matching-suite)

(test command-permission-matches-exact-prefix-and-regex
  (let ((rules (list (amoebum:make-permission-rule
                      :effect :allow
                      :tool :bash
                      :command "git *"
                      :source :global)
                     (amoebum:make-permission-rule
                      :effect :deny
                      :tool :bash
                      :command "git push --force"
                      :source :project)
                     (amoebum:make-permission-rule
                      :effect :deny
                      :tool :bash
                      :command "re:rm\\s+-rf\\s+.*"
                      :source :global))))
    (is (eq (amoebum:evaluate-command-permission
             :tool :bash
             :command "git status"
             :rules rules)
            :allow))
    (is (eq (amoebum:evaluate-command-permission
             :tool :bash
             :command "git push --force"
             :rules rules)
            :deny))
    (is (eq (amoebum:evaluate-command-permission
             :tool :bash
             :command "cat notes.txt | rm -rf /tmp/demo"
             :rules rules)
            :deny))
    (is (null (amoebum:evaluate-command-permission
               :tool :bash
               :command "python -V"
               :rules rules)))))

(test command-permission-matches-exact-prefix-and-regex-across-pipeline-segments
  (let ((rules (list (amoebum:make-permission-rule
                      :effect :allow
                      :tool :bash
                      :command "git *"
                      :source :global)
                     (amoebum:make-permission-rule
                      :effect :deny
                      :tool :bash
                      :command "git push --force"
                      :source :project)
                     (amoebum:make-permission-rule
                      :effect :deny
                      :tool :bash
                      :command "re:rm\\s+-rf\\s+.*"
                      :source :global))))
    (is (eq (amoebum:evaluate-command-permission
             :tool :bash
             :command "echo prep | git status"
             :rules rules)
            :allow))
    (is (eq (amoebum:evaluate-command-permission
             :tool :bash
             :command "echo prep | git push --force"
             :rules rules)
            :deny))
    (is (eq (amoebum:evaluate-command-permission
             :tool :bash
             :command "echo prep && rm -rf /tmp/demo"
             :rules rules)
            :deny))))

(test command-permission-conflicts-resolve-deterministically-with-deny-precedence
  (let* ((allow-rule (amoebum:make-permission-rule
                      :effect :allow
                      :tool :bash
                      :command "git push --force"
                      :source :project))
         (deny-rule (amoebum:make-permission-rule
                     :effect :deny
                     :tool :bash
                     :command "git push --force"
                     :source :project))
         (command "echo prep | git push --force"))
    (is (eq (amoebum:evaluate-command-permission
             :tool :bash
             :command command
             :rules (list allow-rule deny-rule))
            :deny))
    (is (eq (amoebum:evaluate-command-permission
             :tool :bash
             :command command
             :rules (list deny-rule allow-rule))
            :deny))))

(test command-permission-respects-tool-name
  (let ((rules (list (amoebum:make-permission-rule
                      :effect :allow
                      :tool :bash-exec
                      :command "git status"
                      :source :project))))
    (is (eq (amoebum:check-permission
             :tool :bash-exec
             :command "git status"
             :permission-mode :supervised
             :rules rules)
            :allow))
    (is (eq (amoebum:check-permission
             :tool :bash
             :command "git status"
             :permission-mode :supervised
             :rules rules)
            :prompt))))

(test command-permission-overrides-mode-defaults
  (let ((rules (list (amoebum:make-permission-rule
                      :effect :allow
                      :tool :bash
                      :command "git diff"
                      :source :project)
                     (amoebum:make-permission-rule
                      :effect :deny
                      :tool :bash
                      :command "git add -A"
                      :source :project))))
    (is (eq (amoebum:check-permission
             :tool :bash
             :command "git diff"
             :permission-mode :supervised
             :rules rules)
            :allow))
    (is (eq (amoebum:check-permission
             :tool :bash
             :command "git add -A"
             :permission-mode :full-auto
             :rules rules)
            :deny))))

(test dangerous-command-still-escalates-allowed-command-in-full-auto
  (let ((rules (list (amoebum:make-permission-rule
                      :effect :allow
                      :tool :bash
                      :command "rm -rf *"
                      :source :project))))
    (is (eq (amoebum:check-permission
             :tool :bash
             :command "rm -rf /tmp/demo"
             :permission-mode :full-auto
             :rules rules)
            :prompt))
    (is (eq (amoebum:check-permission
             :tool :bash
             :command "rm -rf /tmp/demo"
             :permission-mode :yolo
             :rules rules)
            :allow))))

(test plan-mode-block-retains-plan-specific-trace-details
  (let ((decision (amoebum:check-permission
                   :tool :write-file
                   :path "/tmp/demo.txt"
                   :permission-mode :plan
                   :rules nil)))
    (is (eq decision :deny))
    (let ((trace (amoebum:last-permission-decision-trace)))
      (is (eq (getf trace :decision-source) :plan-mode))
      (is (eq (getf trace :reason-code) :plan-mode-mutating-command-blocked))
      (is (stringp (getf trace :actionable-reason))))))
