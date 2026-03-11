(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Runaway output shell safety tests (I347)
;;; ---------------------------------------------------------------------------

(def-suite shell-runaway-output-suite :in amoebum-suite
  :description "Runaway shell output monitoring, kill, and timeout behavior.")

(in-suite shell-runaway-output-suite)

(test i347-bounded-output-completes
  "Bounded output under byte/line ceilings completes normally."
  (%with-i345-shell-state
    (let ((result (%i345-invoke-bash-exec
                   "command" "printf 'i347-one\\ni347-two\\n'"
                   "timeout-seconds" 5
                   "max-output-chars" 256
                   "max-output-bytes" 2048
                   "max-output-lines" 128)))
      (is (eq :completed (getf result :status)))
      (is (integerp (getf result :exit-code)))
      (is-true (search "i347-one" (or (getf result :stdout) "") :test #'char-equal))
      (is (not (getf result :runaway-output-p)))
      (is (not (getf result :timed-out))))))

(test i347-runaway-output-forces-kill-with-diagnostic
  "Runaway output is forcibly terminated with preserved diagnostics."
  (%with-i345-shell-state
    (let ((result (%i345-invoke-bash-exec
                   "command" "while :; do printf 'i347-runaway-line\\n'; done"
                   "timeout-seconds" 10
                   "max-output-chars" 32
                   "max-output-bytes" 256
                   "max-output-lines" 100000)))
      (is (eq :failed (getf result :status)))
      (is-true (getf result :runaway-output-p))
      (is (eq :byte-limit (getf result :runaway-output-reason)))
      (is (null (getf result :exit-code)))
      (is (getf result :stdout-truncated-p))
      (is-true (search "output limit exceeded" (or (getf result :stderr) "")
                       :test #'char-equal)))))

(test i347-timeout-still-wins-when-no-runaway-output
  "Timeout results stay distinct from runaway output when command is quiet."
  (%with-i345-shell-state
    (let ((result (%i345-invoke-bash-exec
                   "command" "while :; do sleep 1; done"
                   "timeout-seconds" 1
                   "max-output-chars" 128
                   "max-output-bytes" 1024
                   "max-output-lines" 128)))
      (is (eq :timeout (getf result :status)))
      (is-true (getf result :timed-out))
      (is (not (getf result :runaway-output-p)))
      (is (null (getf result :runaway-output-reason))))))
