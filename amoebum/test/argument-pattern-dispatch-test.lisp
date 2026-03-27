(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Argument Pattern Selector Dispatch Table Tests (FP-Refine Phase 2, Target 3)
;;; ---------------------------------------------------------------------------

(def-suite argument-pattern-dispatch-suite :in amoebum-suite
  :description "Tests for argument pattern selector dispatch table.")

(in-suite argument-pattern-dispatch-suite)

;;; --- Table Structure Tests ---

(test argument-pattern-table-has-13-entries
  (is (= 13 (length amoebum::+argument-pattern-selectors+))))

(test argument-pattern-table-has-5-unique-selectors
  (let ((selectors (remove-duplicates
                    (mapcar #'cdr amoebum::+argument-pattern-selectors+))))
    (is (= 5 (length selectors)))))

(test argument-pattern-table-selectors-are-keywords
  (dolist (entry amoebum::+argument-pattern-selectors+)
    (is (keywordp (cdr entry)))))

(test argument-pattern-table-prefixes-end-with-colon
  (dolist (entry amoebum::+argument-pattern-selectors+)
    (is (char= #\: (char (car entry) (1- (length (car entry))))))))

;;; --- Per-Prefix Dispatch Tests ---

(test dispatch-program-prefix
  (multiple-value-bind (sel val) (amoebum::%argument-pattern-components "program:bash")
    (is (eq :program sel))
    (is (string= "bash" val))))

(test dispatch-prog-prefix
  (multiple-value-bind (sel val) (amoebum::%argument-pattern-components "prog:zsh")
    (is (eq :program sel))
    (is (string= "zsh" val))))

(test dispatch-flag-prefix
  (multiple-value-bind (sel val) (amoebum::%argument-pattern-components "flag:--verbose")
    (is (eq :flag sel))
    (is (string= "--verbose" val))))

(test dispatch-flags-prefix
  (multiple-value-bind (sel val) (amoebum::%argument-pattern-components "flags:-v")
    (is (eq :flag sel))
    (is (string= "-v" val))))

(test dispatch-option-prefix
  (multiple-value-bind (sel val) (amoebum::%argument-pattern-components "option:--output")
    (is (eq :flag sel))
    (is (string= "--output" val))))

(test dispatch-options-prefix
  (multiple-value-bind (sel val) (amoebum::%argument-pattern-components "options:-o")
    (is (eq :flag sel))
    (is (string= "-o" val))))

(test dispatch-positional-prefix
  (multiple-value-bind (sel val) (amoebum::%argument-pattern-components "positional:file.txt")
    (is (eq :positional sel))
    (is (string= "file.txt" val))))

(test dispatch-position-prefix
  (multiple-value-bind (sel val) (amoebum::%argument-pattern-components "position:0")
    (is (eq :positional sel))
    (is (string= "0" val))))

(test dispatch-pos-prefix
  (multiple-value-bind (sel val) (amoebum::%argument-pattern-components "pos:1")
    (is (eq :positional sel))
    (is (string= "1" val))))

(test dispatch-arg-prefix
  (multiple-value-bind (sel val) (amoebum::%argument-pattern-components "arg:value")
    (is (eq :argument sel))
    (is (string= "value" val))))

(test dispatch-args-prefix
  (multiple-value-bind (sel val) (amoebum::%argument-pattern-components "args:multi")
    (is (eq :argument sel))
    (is (string= "multi" val))))

(test dispatch-token-prefix
  (multiple-value-bind (sel val) (amoebum::%argument-pattern-components "token:raw")
    (is (eq :token sel))
    (is (string= "raw" val))))

(test dispatch-argv-prefix
  (multiple-value-bind (sel val) (amoebum::%argument-pattern-components "argv:0")
    (is (eq :token sel))
    (is (string= "0" val))))

;;; --- Default Fallback Tests ---

(test no-prefix-defaults-to-argument
  (multiple-value-bind (sel val) (amoebum::%argument-pattern-components "plain-value")
    (is (eq :argument sel))
    (is (string= "plain-value" val))))

(test path-like-value-defaults-to-argument
  (multiple-value-bind (sel val) (amoebum::%argument-pattern-components "/usr/bin/ls")
    (is (eq :argument sel))
    (is (string= "/usr/bin/ls" val))))

;;; --- Case Insensitivity Tests ---

(test prefix-case-insensitive-upper
  (multiple-value-bind (sel val) (amoebum::%argument-pattern-components "PROGRAM:bash")
    (is (eq :program sel))
    (is (string= "bash" val))))

(test prefix-case-insensitive-mixed
  (multiple-value-bind (sel val) (amoebum::%argument-pattern-components "Flag:--help")
    (is (eq :flag sel))
    (is (string= "--help" val))))

;;; --- Edge Cases ---

(test nil-input-returns-nil
  (is (null (amoebum::%argument-pattern-components nil))))

(test empty-string-returns-nil
  (is (null (amoebum::%argument-pattern-components ""))))

(test whitespace-only-returns-nil
  (is (null (amoebum::%argument-pattern-components "   "))))

(test prefix-with-whitespace-after-colon
  (multiple-value-bind (sel val) (amoebum::%argument-pattern-components "flag:  --verbose")
    (is (eq :flag sel))
    (is (string= "--verbose" val))))
