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
