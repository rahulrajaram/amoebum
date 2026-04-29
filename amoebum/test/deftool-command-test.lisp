(in-package :amoebum/test)

;;;; NXT-577: /deftool slash command regression suite.

(def-suite deftool-command-suite
  :in amoebum-suite
  :description "/deftool slash-command registers and unregisters tools in *toolset*.")

(in-suite deftool-command-suite)

(defun %deftool-make-context ()
  (amoebum::make-slash-command-context))

(defun %deftool-args-table (raw)
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash :ARGS table) raw)
    table))

(defun %deftool-invoke (raw)
  (let ((handler (amoebum::slash-command-handler
                  (amoebum::find-slash-command "deftool"))))
    (funcall handler nil (%deftool-args-table raw) (%deftool-make-context))))

(test deftool-command-is-registered
  (let ((command (amoebum::find-slash-command "deftool")))
    (is (not (null command)))
    (is (string-equal "deftool"
                      (amoebum::slash-command-name command)))))

(test deftool-registers-tool-and-can-call-it
  (amoebum.test-support.globals-fixture:with-clean-amoebum-globals
    (let* ((result (%deftool-invoke
                    "nxt577-greet \"Greet the operator\" (lambda (args) args \"hello-from-nxt577\")"))
           (output (and (amoebum::slash-command-result-p result)
                        (amoebum::slash-command-result-output result))))
      (declare (ignore output))
      (is (amoebum::slash-command-result-p result))
      (let ((tool (pseudopod:find-tool amoebum:*toolset* "nxt577-greet")))
        (is (not (null tool)))
        (let ((value (funcall (pseudopod:tool-definition-fn tool)
                              (make-hash-table :test #'equal))))
          (is (string= "hello-from-nxt577" value)))))))

(test deftool-undo-removes-tool
  (amoebum.test-support.globals-fixture:with-clean-amoebum-globals
    (%deftool-invoke
     "nxt577-undo-target \"To be removed\" (lambda (args) args :ok)")
    (is (not (null (pseudopod:find-tool amoebum:*toolset* "nxt577-undo-target"))))
    (let ((result (%deftool-invoke "--undo nxt577-undo-target")))
      (is (amoebum::slash-command-result-p result))
      (is (search "unregistered"
                  (amoebum::slash-command-result-output result)))
      (is (null (pseudopod:find-tool amoebum:*toolset*
                                     "nxt577-undo-target"))))))

(test deftool-rejects-unsafe-body
  ;; The sandboxed-eval gate (precedent: /self-modify) must refuse forms
  ;; that touch the unsafe-symbol denylist (DELETE-FILE, OPEN, ...).
  (amoebum.test-support.globals-fixture:with-clean-amoebum-globals
    (let* ((result (%deftool-invoke
                    "nxt577-evil \"should be blocked\" (lambda (args) (declare (ignore args)) (delete-file \"/tmp/x\"))"))
           (output (and (amoebum::slash-command-result-p result)
                        (amoebum::slash-command-result-output result))))
      (is (amoebum::slash-command-result-p result))
      (is (search "rejected" output :test #'char-equal))
      (is (null (pseudopod:find-tool amoebum:*toolset* "nxt577-evil"))))))

(test deftool-usage-on-blank-args
  (amoebum.test-support.globals-fixture:with-clean-amoebum-globals
    (let* ((result (%deftool-invoke ""))
           (output (and (amoebum::slash-command-result-p result)
                        (amoebum::slash-command-result-output result))))
      (is (amoebum::slash-command-result-p result))
      (is (search "Usage:" output :test #'char-equal)))))
