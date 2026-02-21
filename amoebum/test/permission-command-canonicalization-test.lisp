(in-package :amoebum/test)

(def-suite permission-command-canonicalization-suite
  :in amoebum-suite
  :description "Command canonicalization normalization tests (I134).")

(in-suite permission-command-canonicalization-suite)

(test canonicalize-command-trims-and-normalizes-text
  (let ((rules (list (amoebum:make-permission-rule
                      :effect :allow
                      :tool :bash
                      :command "echo hello world"
                      :source :project))))
    (is (eq (amoebum:check-permission
             :tool :bash
             :command "  echo   hello    world  "
             :permission-mode :supervised
             :rules rules)
            :allow))))

(test canonicalize-command-list-ignores-whitespace-padding
  (let* ((command '("echo" "  hello  " "" "world"))
         (canonical (amoebum:canonicalize-permission-command command))
         (rules (list (amoebum:make-permission-rule
                       :effect :allow
                       :tool :bash
                       :command "echo hello world"
                       :source :project)))
         (permission (amoebum:check-permission
                      :tool :bash
                      :command command
                      :permission-mode :supervised
                      :rules rules)))
    (is (typep canonical 'amoebum::command-canonical-form))
    (is (string= (amoebum:command-canonical-form-normalized canonical)
                 (amoebum:command-canonical-form-normalized
                   (amoebum:canonicalize-permission-command "echo hello world")))
            "Canonical normalization should align list and string inputs.")
    (is (eq permission :allow))))

(test canonicalize-command-exposes-operator-ast-and-signature
  (let* ((canonical (amoebum:canonicalize-permission-command
                     "cat input.txt | grep ok && echo done > output.txt"))
         (metadata (amoebum:command-canonical-form-operator-metadata canonical))
         (ast (amoebum:command-canonical-form-ast canonical))
         (signature (amoebum:command-canonical-form-canonical-signature canonical)))
    (is (typep canonical 'amoebum::command-canonical-form))
    (is (member "|" (amoebum:command-canonical-form-operators canonical) :test #'string=))
    (is (member "&&" (amoebum:command-canonical-form-operators canonical) :test #'string=))
    (is (getf metadata :contains-pipeline))
    (is (getf metadata :contains-logical-and))
    (is (getf metadata :contains-redirection))
    (is (> (length ast) 0))
    (is (stringp signature))
    (is (search "operators=|,&&,>" signature :test #'char=))))

(test dangerous-command-detects-interactive-command-classes
  (is (amoebum:dangerous-command-p "vim README.md"))
  (is (eq (amoebum:check-permission
           :tool :bash
           :command "vim README.md"
           :permission-mode :full-auto
           :rules nil)
          :prompt)))

(test dangerous-command-detects-interactive-shell-wrapper
  (let* ((canonical (amoebum:canonicalize-permission-command
                     "env FOO=1 bash -lc 'less README.md'"))
         (reasons (amoebum:command-canonical-form-dangerous-reason-codes canonical))
         (trace (amoebum:command-canonicalization-trace)))
    (is (amoebum:dangerous-command-p canonical))
    (is (member :interactive-command-class reasons :test #'eq))
    (is (member :shell-wrapper-expanded reasons :test #'eq))
    (is (member :env-wrapper-expanded reasons :test #'eq))
    (is (stringp (getf trace :canonical-signature)))
    (is (equal reasons (getf trace :dangerous-reason-codes)))))
