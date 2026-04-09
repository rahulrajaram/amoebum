;;;; amoebum/test/permissions-unit-test.lisp
;;;;
;;;; NXT-284 — End-to-end unit tests for the top-level permission decision
;;;; pipeline (`amoebum:check-permission`). These complement the finer-grained
;;;; permission-path-*, permission-command-*, and permission-argument-*
;;;; suites by exercising the full pipeline with fixture policies built
;;;; inline, so a regression anywhere along the pipeline
;;;; (rule matching -> specificity -> scope priority -> mode default)
;;;; surfaces here.

(in-package :amoebum/test)

(def-suite permissions-unit-suite
  :in amoebum-suite
  :description "End-to-end top-level permission decision pipeline tests (NXT-284).")

(in-suite permissions-unit-suite)

(defun %nxt284-check (&key tool command path (mode :supervised) rules)
  "Invoke the top-level decision pipeline with history recording disabled
so tests do not pollute the shared permission decision history."
  (amoebum:check-permission
   :tool tool
   :command command
   :path path
   :permission-mode mode
   :rules rules
   :record-history-p nil))

;;; --- 1. Allow by explicit rule ------------------------------------------------

(test nxt284-allow-by-explicit-command-rule
  "An explicit :allow command rule should produce :allow under :supervised mode."
  (let ((rules (list (amoebum:make-permission-rule
                      :effect :allow
                      :tool :bash
                      :command "git status"
                      :source :project))))
    (is (eq :allow
            (%nxt284-check :tool :bash
                           :command "git status"
                           :rules rules)))))

;;; --- 2. Deny by explicit rule -------------------------------------------------

(test nxt284-deny-by-explicit-command-rule
  "An explicit :deny command rule should produce :deny even when a broader
allow is also present."
  (let ((rules (list (amoebum:make-permission-rule
                      :effect :allow
                      :tool :bash
                      :command "git *"
                      :source :project)
                     (amoebum:make-permission-rule
                      :effect :deny
                      :tool :bash
                      :command "git push --force"
                      :source :project))))
    (is (eq :deny
            (%nxt284-check :tool :bash
                           :command "git push --force"
                           :rules rules)))
    ;; the broader rule still matches unrelated subcommands
    (is (eq :allow
            (%nxt284-check :tool :bash
                           :command "git status"
                           :rules rules)))))

;;; --- 3. Fall through to default policy (mode default) ------------------------

(test nxt284-fall-through-to-mode-default
  "With no matching rule, the decision falls through to the permission mode default.
:supervised defaults to :prompt, :full-auto defaults to :allow."
  (let ((rules (list (amoebum:make-permission-rule
                      :effect :allow
                      :tool :bash
                      :command "git *"
                      :source :project))))
    ;; Non-matching command under :supervised -> :prompt
    (is (eq :prompt
            (%nxt284-check :tool :bash
                           :command "python -V"
                           :mode :supervised
                           :rules rules)))
    ;; Non-matching command under :full-auto -> :allow
    (is (eq :allow
            (%nxt284-check :tool :bash
                           :command "python -V"
                           :mode :full-auto
                           :rules rules)))))

;;; --- 4. Multiple matching rules with specificity priority --------------------

(test nxt284-specificity-beats-broader-rule
  "A more specific rule should win over a broader rule with the opposite effect,
regardless of list order."
  ;; Broad allow + narrow deny: narrow deny wins.
  (let ((rules (list (amoebum:make-permission-rule
                      :effect :allow
                      :tool :bash
                      :command "git *"
                      :source :project)
                     (amoebum:make-permission-rule
                      :effect :deny
                      :tool :bash
                      :command "git push --force"
                      :source :project))))
    (is (eq :deny
            (%nxt284-check :tool :bash
                           :command "git push --force"
                           :rules rules))))
  ;; Broad deny + narrow allow: narrow allow wins.
  (let ((rules (list (amoebum:make-permission-rule
                      :effect :deny
                      :tool :bash
                      :command "rm *"
                      :source :project)
                     (amoebum:make-permission-rule
                      :effect :allow
                      :tool :bash
                      :command "rm -i /tmp/scratch"
                      :source :project))))
    (is (eq :allow
            (%nxt284-check :tool :bash
                           :command "rm -i /tmp/scratch"
                           :rules rules)))))

;;; --- 5. Rule scoping (session vs project vs global) --------------------------

(test nxt284-scope-priority-session-beats-project-beats-global
  "When two rules match with equal specificity, the higher-scoped rule wins.
scope order: :session (30) > :extension (20) > :project (10) > :global (0)."
  ;; Session :deny should beat project :allow at equal specificity.
  (let ((rules (list (amoebum:make-permission-rule
                      :effect :allow
                      :tool :bash
                      :command "curl https://example.com"
                      :source :project)
                     (amoebum:make-permission-rule
                      :effect :deny
                      :tool :bash
                      :command "curl https://example.com"
                      :source :session))))
    (is (eq :deny
            (%nxt284-check :tool :bash
                           :command "curl https://example.com"
                           :rules rules))))
  ;; Project :deny should beat global :allow at equal specificity.
  (let ((rules (list (amoebum:make-permission-rule
                      :effect :allow
                      :tool :bash
                      :command "wget https://example.com"
                      :source :global)
                     (amoebum:make-permission-rule
                      :effect :deny
                      :tool :bash
                      :command "wget https://example.com"
                      :source :project))))
    (is (eq :deny
            (%nxt284-check :tool :bash
                           :command "wget https://example.com"
                           :rules rules)))))

;;; --- 6. Path-scoped rules exercise the path phase of the pipeline ------------

(test nxt284-path-scoped-allow-and-deny
  "Rules keyed on :path (not :command) should route through the path phase
of the pipeline and yield the expected top-level decision."
  (let ((rules (list (amoebum:make-permission-rule
                      :effect :allow
                      :tool :read-file
                      :path "/tmp/**"
                      :source :project)
                     (amoebum:make-permission-rule
                      :effect :deny
                      :tool :read-file
                      :path "/tmp/secrets/**"
                      :source :project))))
    (is (eq :allow
            (%nxt284-check :tool :read-file
                           :path "/tmp/scratch/notes.txt"
                           :rules rules)))
    (is (eq :deny
            (%nxt284-check :tool :read-file
                           :path "/tmp/secrets/creds.env"
                           :rules rules)))))

;;; --- 7. Edge case: empty policy ---------------------------------------------

(test nxt284-empty-policy-falls-through-to-mode-default
  "An empty rule list should fall through to the mode default on every query."
  (let ((rules '()))
    (is (eq :prompt
            (%nxt284-check :tool :bash
                           :command "git status"
                           :mode :supervised
                           :rules rules)))
    (is (eq :allow
            (%nxt284-check :tool :bash
                           :command "git status"
                           :mode :full-auto
                           :rules rules)))
    (is (eq :allow
            (%nxt284-check :tool :bash
                           :command "git status"
                           :mode :yolo
                           :rules rules)))))

;;; --- 8. Edge case: conflicting rules at equal specificity and scope ----------

(test nxt284-conflicting-rules-deny-wins-at-equal-specificity-and-scope
  "When two rules have identical specificity AND scope but opposite effects,
deny must win. This guards %better-rule-p's deny-precedence tiebreak."
  (let ((rules (list (amoebum:make-permission-rule
                      :effect :allow
                      :tool :bash
                      :command "make deploy"
                      :source :project)
                     (amoebum:make-permission-rule
                      :effect :deny
                      :tool :bash
                      :command "make deploy"
                      :source :project))))
    (is (eq :deny
            (%nxt284-check :tool :bash
                           :command "make deploy"
                           :rules rules)))))

;;; --- 9. Deny is sticky across modes (including :full-auto and :yolo) ---------

(test nxt284-explicit-deny-overrides-permissive-modes
  "Explicit :deny rules must be honoured even in permissive modes like
:full-auto. (:yolo also honours rule denies, since the base decision runs
before the mode default.)"
  (let ((rules (list (amoebum:make-permission-rule
                      :effect :deny
                      :tool :bash
                      :command "rm -rf /"
                      :source :project))))
    (is (eq :deny
            (%nxt284-check :tool :bash
                           :command "rm -rf /"
                           :mode :full-auto
                           :rules rules)))
    (is (eq :deny
            (%nxt284-check :tool :bash
                           :command "rm -rf /"
                           :mode :yolo
                           :rules rules)))))

;;; --- 10. Tool scoping: a rule for one tool does not affect another -----------

(test nxt284-tool-scoping-does-not-leak-across-tools
  "A rule keyed to tool :bash must not affect a decision for tool :read-file
and vice versa."
  (let ((rules (list (amoebum:make-permission-rule
                      :effect :deny
                      :tool :bash
                      :command "cat notes.txt"
                      :source :project))))
    ;; The :bash deny applies to the :bash call.
    (is (eq :deny
            (%nxt284-check :tool :bash
                           :command "cat notes.txt"
                           :rules rules)))
    ;; A :read-file call with no matching rule falls through to mode default.
    ;; Use a project-relative path so the project-root guard does not fire.
    (is (eq :prompt
            (%nxt284-check :tool :read-file
                           :path "notes.txt"
                           :mode :supervised
                           :rules rules)))))
