(in-package :pseudopod/test)

;;; ---------------------------------------------------------------------------
;;; I110: Command Execution Primitive Tests
;;; ---------------------------------------------------------------------------

(def-suite run-command-suite :in pseudopod-suite
  :description "Command execution primitive tests (I110).")

(in-suite run-command-suite)

;;; ---- Tests: Successful command ----

(test run-command-success-basic
  "Run a simple echo command and verify structured result."
  (let ((result (pseudopod:run-command "echo hello")))
    (is-true (pseudopod:command-result-p result))
    (is (string= "hello" (pseudopod:command-result-stdout result)))
    (is (= 0 (pseudopod:command-result-exit-code result)))
    (is (string= "echo hello" (pseudopod:command-result-command result)))))

(test run-command-multiline-stdout
  "Capture multiline stdout correctly."
  (let* ((result (pseudopod:run-command "printf 'line1\nline2\nline3'"))
         (stdout (pseudopod:command-result-stdout result)))
    (is (= 0 (pseudopod:command-result-exit-code result)))
    (is-true (search "line1" stdout))
    (is-true (search "line2" stdout))
    (is-true (search "line3" stdout))))

;;; ---- Tests: Failed command (nonzero exit) ----

(test run-command-nonzero-exit
  "A command that exits non-zero reports the exit code."
  (let ((result (pseudopod:run-command "exit 42")))
    (is-true (pseudopod:command-result-p result))
    (is (= 42 (pseudopod:command-result-exit-code result)))))

(test run-command-failed-command
  "A failing command (false) returns exit code 1."
  (let ((result (pseudopod:run-command "false")))
    (is-true (plusp (pseudopod:command-result-exit-code result)))))

;;; ---- Tests: stdout/stderr capture ----

(test run-command-stderr-capture
  "Stderr output is captured separately from stdout."
  (let ((result (pseudopod:run-command "echo out-msg; echo err-msg >&2")))
    (is (= 0 (pseudopod:command-result-exit-code result)))
    (is-true (search "out-msg" (pseudopod:command-result-stdout result)))
    (is-true (search "err-msg" (pseudopod:command-result-stderr result)))))

(test run-command-empty-output
  "A command producing no output returns empty strings."
  (let ((result (pseudopod:run-command "true")))
    (is (= 0 (pseudopod:command-result-exit-code result)))
    (is (string= "" (pseudopod:command-result-stdout result)))
    (is (string= "" (pseudopod:command-result-stderr result)))))

;;; ---- Tests: Timeout behavior ----

(test run-command-timeout-signals-condition
  "A command exceeding timeout signals PSEUDOPOD-COMMAND-TIMEOUT."
  (signals pseudopod:pseudopod-command-timeout
    (pseudopod:run-command "sleep 60" :timeout 1)))

(test run-command-timeout-is-command-error
  "PSEUDOPOD-COMMAND-TIMEOUT is a subtype of PSEUDOPOD-COMMAND-ERROR."
  (signals pseudopod:pseudopod-command-error
    (pseudopod:run-command "sleep 60" :timeout 1)))

(test run-command-timeout-is-pseudopod-error
  "PSEUDOPOD-COMMAND-TIMEOUT is a subtype of PSEUDOPOD-ERROR."
  (signals pseudopod:pseudopod-error
    (pseudopod:run-command "sleep 60" :timeout 1)))

;;; ---- Tests: Structured result shape ----

(test run-command-result-shape
  "Verify all fields of the command-result struct are present and typed."
  (let ((result (pseudopod:run-command "echo shape-test")))
    (is-true (pseudopod:command-result-p result))
    (is-true (stringp (pseudopod:command-result-stdout result)))
    (is-true (stringp (pseudopod:command-result-stderr result)))
    (is-true (integerp (pseudopod:command-result-exit-code result)))
    (is-true (integerp (pseudopod:command-result-duration-ms result)))
    (is-true (>= (pseudopod:command-result-duration-ms result) 0))
    (is-true (stringp (pseudopod:command-result-command result)))))

(test run-command-duration-positive
  "Duration is measured and non-negative for a real command."
  (let ((result (pseudopod:run-command "sleep 0.1")))
    (is (= 0 (pseudopod:command-result-exit-code result)))
    (is-true (>= (pseudopod:command-result-duration-ms result) 0))))
