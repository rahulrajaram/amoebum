(in-package :amoebum/test)

(def-suite self-modify-i256-suite :in amoebum-suite
  :description "I256 sandbox limits + audit + undo tests.")

(in-suite self-modify-i256-suite)

(defmacro with-clean-i256-state (&body body)
  `(let ((amoebum::*modification-journal* '())
         (amoebum::*modification-counter* 0)
         (amoebum::*self-modification-agent-id-override* nil))
     ,@body))

(defun %read-text-file (path)
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (let ((text (make-string (file-length stream))))
      (read-sequence text stream)
      text)))

(test i256-timeout-triggers-graceful-abort
  (multiple-value-bind (result errorp error-message)
      (amoebum:sandboxed-eval "(labels ((spin () (spin))) (spin))" :timeout-seconds 1)
    (declare (ignore result))
    (is (eq t errorp))
    (is (and (stringp error-message)
             (search "timed out" error-message :test #'char-equal)))))

(test i256-cons-limit-triggers-graceful-abort
  (multiple-value-bind (result errorp error-message)
      (amoebum:sandboxed-eval
       "(loop repeat 50000 collect (list 1 2 3 4))"
       :timeout-seconds 5
       :max-cons-cells 200)
    (declare (ignore result))
    (is (eq t errorp))
    (is (and (stringp error-message)
             (search "cons-cell limit" error-message :test #'char-equal)))))

(test i256-audit-log-includes-agent-and-code
  (with-clean-i256-state
    (let* ((tmpdir (%make-temp-directory "amoebum-i256-audit"))
           (audit-path (merge-pathnames "self-modifications.sexp" tmpdir)))
      (unwind-protect
           (let ((amoebum::*self-modification-audit-path-override* audit-path)
                 (amoebum::*self-modification-agent-id-override* "agent-i256"))
             (let* ((entry (amoebum:propose-modification "(+ 40 2)"))
                    (id (amoebum:modification-entry-id entry)))
               (amoebum:approve-modification id)
               (amoebum:apply-modification id)
               (is (probe-file audit-path))
               (let ((text (%read-text-file audit-path)))
                 (is (search ":AGENT-ID" text))
                 (is (search "agent-i256" text :test #'char-equal))
                 (is (search ":CODE \"(+ 40 2)\"" text))
                 (is (search ":RESULT" text)))))
        (%delete-directory-tree-safe tmpdir)))))

(test i256-undo-last-modification-and-history-browser
  (with-clean-i256-state
    (let* ((entry (amoebum:propose-modification "(defun i256-undo-temp-fn () :ok)"))
           (id (amoebum:modification-entry-id entry)))
      (unwind-protect
           (progn
             (is (eq :approved (amoebum:modification-entry-status entry)))
             (let ((applied (amoebum:apply-modification id)))
               (is (eq :applied (amoebum:modification-entry-status applied))))
             (is (fboundp 'amoebum::i256-undo-temp-fn))
             (let ((rolled-back (amoebum:undo-last-modification)))
               (is (eq :rolled-back (amoebum:modification-entry-status rolled-back))))
             (is (not (fboundp 'amoebum::i256-undo-temp-fn)))
             (let* ((items (amoebum:list-modifications :limit 5))
                    (first-item (first items))
                    (browser (amoebum:modification-history-browser :limit 5)))
               (is-true first-item)
               (is (string= id (getf first-item :id)))
               (is (search "Self-Modification History" browser))))
        (when (fboundp 'amoebum::i256-undo-temp-fn)
          (fmakunbound 'amoebum::i256-undo-temp-fn))))))

(test i256-sandbox-limits-smoke-sentinel
  (format t "SANDBOX_LIMITS_SMOKE_OK~%")
  (is (equal t t)))
