(in-package :amoebum/test)

(def-suite permission-argument-granularity-suite
  :in amoebum-suite
  :description "Argument-aware shell permission decision tests (I352).")

(in-suite permission-argument-granularity-suite)

(test permission-command-argument-profile-decomposes-shell-command
  (let* ((profile (amoebum:permission-command-argument-profile
                   "docker run --privileged --name app ubuntu")))
    (is (string= "docker" (or (getf profile :program) "")))
    (is (equal '("docker" "run" "--privileged" "--name" "app" "ubuntu")
               (getf profile :argv)))
    (is (equal '("run" "--privileged" "--name" "app" "ubuntu")
               (getf profile :arguments)))
    (is (equal '("--privileged" "--name")
               (getf profile :flags)))
    (is (equal '("run" "app" "ubuntu")
               (getf profile :positionals)))))

(test permission-program-selector-uses-decomposed-program-token
  (let ((rules (list (amoebum:make-permission-rule
                      :effect :allow
                      :tool :bash
                      :command "*"
                      :arguments '("program:docker")
                      :source :project))))
    (is (eq (amoebum:check-permission
             :tool :bash
             :command "docker run ubuntu"
             :permission-mode :supervised
             :rules rules)
            :allow))
    (is (eq (amoebum:check-permission
             :tool :bash
             :command "git status"
             :permission-mode :supervised
             :rules rules)
            :prompt))))

(test permission-argument-deny-overrides-command-allow
  (let ((rules (list (amoebum:make-permission-rule
                      :effect :allow
                      :tool :bash
                      :command "docker *"
                      :source :project)
                     (amoebum:make-permission-rule
                      :effect :deny
                      :tool :bash
                      :command "docker *"
                      :arguments '("flag:--privileged")
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
                      :command "curl *"
                      :source :project)
                     (amoebum:make-permission-rule
                      :effect :deny
                      :tool :bash
                      :command "curl *"
                      :arguments '("positional:http://internal-api/*")
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
                      :command "customctl *"
                      :source :project)
                     (amoebum:make-permission-rule
                      :effect :deny
                      :tool :bash
                      :command "customctl *"
                      :arguments '("flag:--force*" "positional:main")
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

(test permission-trace-includes-argument-profile-and-rule-arguments
  (let* ((rules (list (amoebum:make-permission-rule
                       :effect :allow
                       :tool :bash
                       :command "docker *"
                       :source :project)
                      (amoebum:make-permission-rule
                       :effect :deny
                       :tool :bash
                       :command "docker *"
                       :arguments '("flag:--privileged")
                       :source :project)))
         (decision (amoebum:check-permission
                    :tool :bash
                    :command "docker run --privileged ubuntu"
                    :permission-mode :supervised
                    :rules rules))
         (trace (amoebum:last-permission-decision-trace))
         (profile (getf trace :command-argument-profile))
         (command-trace (find :command
                              (getf trace :evaluation-trace)
                              :key (lambda (entry) (getf entry :phase))
                              :test #'eq)))
    (is (eq decision :deny))
    (is (string= "docker" (or (getf profile :program) "")))
    (is (equal '("--privileged")
               (getf profile :flags)))
    (is (equal '("flag:--privileged")
               (getf command-trace :arguments)))))
