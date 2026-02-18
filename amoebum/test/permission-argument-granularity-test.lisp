(in-package :amoebum/test)

(def-suite permission-argument-granularity-suite
  :in amoebum-suite
  :description "Argument-level permission policy granularity tests (I133).")

(in-suite permission-argument-granularity-suite)

(test permission-command-family-matches-program-token
  (let ((rules (list (amoebum:make-permission-rule
                      :effect :allow
                      :tool :bash
                      :command "docker"
                      :source :project))))
    (is (eq (amoebum:evaluate-command-permission
             :tool :bash
             :command "docker run ubuntu"
             :rules rules)
            :allow))
    (is (eq (amoebum:evaluate-command-permission
             :tool :bash
             :command "DOCKER run ubuntu"
             :rules rules)
            :allow))
    (is (null (amoebum:evaluate-command-permission
               :tool :bash
               :command "git status"
               :rules rules)))))

(test permission-argument-deny-overrides-command-allow
  (let ((rules (list (amoebum:make-permission-rule
                      :effect :allow
                      :tool :bash
                      :command "docker"
                      :source :project)
                     (amoebum:make-permission-rule
                      :effect :deny
                      :tool :bash
                      :command "docker"
                      :arguments '("--privileged")
                      :source :project))))
    (is (eq (amoebum:check-permission
             :tool :bash
             :command "docker run ubuntu"
             :permission-mode :supervised
             :rules rules)
            :allow))
    (is (eq (amoebum:check-permission
             :tool :bash
             :command "docker run --privileged ubuntu"
             :permission-mode :supervised
             :rules rules)
            :deny))))

(test permission-argument-patterns-support-wildcards
  (let ((rules (list (amoebum:make-permission-rule
                      :effect :allow
                      :tool :bash
                      :command "curl"
                      :source :project)
                     (amoebum:make-permission-rule
                      :effect :deny
                      :tool :bash
                      :command "curl"
                      :arguments '("http://internal-api/*")
                      :source :project))))
    (is (eq (amoebum:check-permission
             :tool :bash
             :command "curl http://example.com/status"
             :permission-mode :supervised
             :rules rules)
            :allow))
    (is (eq (amoebum:check-permission
             :tool :bash
             :command "curl http://internal-api/health"
             :permission-mode :supervised
             :rules rules)
            :deny))))

(test permission-argument-rules-require-all-patterns
  (let ((rules (list (amoebum:make-permission-rule
                      :effect :allow
                      :tool :bash
                      :command "customctl"
                      :source :project)
                     (amoebum:make-permission-rule
                      :effect :deny
                      :tool :bash
                      :command "customctl"
                      :arguments '("--force*" "main")
                      :source :project))))
    (is (eq (amoebum:check-permission
             :tool :bash
             :command "customctl push --force-with-lease main"
             :permission-mode :supervised
             :rules rules)
            :deny))
    (is (eq (amoebum:check-permission
             :tool :bash
             :command "customctl push --force-with-lease feature/i133"
             :permission-mode :supervised
             :rules rules)
            :allow))))

(test permission-command-rules-remain-tool-specific
  (let ((rules (list (amoebum:make-permission-rule
                      :effect :allow
                      :tool :bash
                      :command "docker"
                      :source :project))))
    (is (eq (amoebum:check-permission
             :tool :sh
             :command "docker run ubuntu"
             :permission-mode :supervised
             :rules rules)
            :prompt))))
