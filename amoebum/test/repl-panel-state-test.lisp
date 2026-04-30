(in-package :amoebum/test)

;;;; NXT-575: REPL panel state regression suite.
;;;;
;;;; Locks the contract of `amoebum::repl-state` and `repl-state-submit-input!`:
;;;;   * toggle flips active-p
;;;;   * blank input is a no-op (no history append, no clear)
;;;;   * (+ 1 2) records "3" output and clears input
;;;;   * (/ 1 0) records an :error entry; :output is nil
;;;;   * unsafe form (delete-file ...) is refused by sandboxed-eval
;;;;   * history caps at max-history-entries (oldest dropped first)

(def-suite repl-panel-state-suite
  :in amoebum-suite
  :description "REPL panel state struct + sandboxed-eval submit pipeline.")

(in-suite repl-panel-state-suite)

(test repl-state-toggle-flips-active-p
  (let ((state (amoebum::make-repl-state)))
    (is (not (amoebum::repl-state-active-p state)))
    (amoebum::repl-state-toggle! state)
    (is (amoebum::repl-state-active-p state))
    (amoebum::repl-state-toggle! state)
    (is (not (amoebum::repl-state-active-p state)))))

(test repl-state-blank-input-is-no-op
  (let ((state (amoebum::make-repl-state)))
    (setf (amoebum::repl-state-input-text state) "   ")
    (let ((result (amoebum::repl-state-submit-input! state)))
      (is (null result))
      (is (null (amoebum::repl-state-history state)))
      ;; Blank input should not be cleared because no submit happened.
      (is (string= "   " (amoebum::repl-state-input-text state))))))

(test repl-state-records-successful-eval
  (amoebum.test-support.globals-fixture:with-clean-amoebum-globals
    (let ((state (amoebum::make-repl-state)))
      (setf (amoebum::repl-state-input-text state) "(+ 1 2)")
      (let ((result (amoebum::repl-state-submit-input! state)))
        (is (string= "3" result))
        (is (string= "" (amoebum::repl-state-input-text state)))
        (let ((entry (first (amoebum::repl-state-history state))))
          (is (not (null entry)))
          (is (string= "(+ 1 2)" (getf entry :input)))
          (is (string= "3" (getf entry :output)))
          (is (null (getf entry :error))))))))

(test repl-state-records-runtime-error
  (amoebum.test-support.globals-fixture:with-clean-amoebum-globals
    (let ((state (amoebum::make-repl-state)))
      (setf (amoebum::repl-state-input-text state) "(/ 1 0)")
      (amoebum::repl-state-submit-input! state)
      (let* ((entry (first (amoebum::repl-state-history state)))
             (error-text (getf entry :error)))
        (is (not (null entry)))
        (is (null (getf entry :output)))
        (is (stringp error-text))
        (is (plusp (length error-text)))))))

(test repl-state-rejects-unsafe-form
  (amoebum.test-support.globals-fixture:with-clean-amoebum-globals
    (let ((state (amoebum::make-repl-state)))
      (setf (amoebum::repl-state-input-text state)
            "(delete-file \"/tmp/never-nxt575\")")
      (amoebum::repl-state-submit-input! state)
      (let* ((entry (first (amoebum::repl-state-history state)))
             (error-text (getf entry :error)))
        (is (not (null entry)))
        (is (null (getf entry :output)))
        (is (stringp error-text))
        (is (plusp (length error-text)))))))

(test repl-state-history-caps-at-max-entries
  (amoebum.test-support.globals-fixture:with-clean-amoebum-globals
    (let ((state (amoebum::make-repl-state)))
      (setf (amoebum::repl-state-max-history-entries state) 3)
      (dolist (form '("(+ 1 1)" "(+ 1 2)" "(+ 1 3)" "(+ 1 4)"))
        (setf (amoebum::repl-state-input-text state) form)
        (amoebum::repl-state-submit-input! state))
      (is (= 3 (length (amoebum::repl-state-history state))))
      ;; Newest-first: most recent is "(+ 1 4)" => "5"; oldest "(+ 1 1)" dropped.
      (let ((newest (first (amoebum::repl-state-history state)))
            (oldest (first (last (amoebum::repl-state-history state)))))
        (is (string= "(+ 1 4)" (getf newest :input)))
        (is (string= "(+ 1 2)" (getf oldest :input)))))))
