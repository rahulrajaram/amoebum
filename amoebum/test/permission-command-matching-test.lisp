(in-package :amoebum/test)

(def-suite permission-command-matching-suite
  :in amoebum-suite
  :description "Command-pattern permission matching tests (I132).")

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
