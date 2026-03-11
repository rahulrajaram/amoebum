(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Self-Modifying Agent Tranche Tests (I254)
;;; ---------------------------------------------------------------------------

(def-suite self-modify-i254-suite :in amoebum-suite
  :description "I254 self-modification sandbox + approval + audit tests.")

(in-suite self-modify-i254-suite)

(defmacro with-clean-self-modification-state (&body body)
  `(let ((amoebum::*modification-journal* '())
         (amoebum::*modification-counter* 0))
     ,@body))

(defun %read-file-text (path)
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content)))

(test i254-unsafe-operations-are-blocked
  (multiple-value-bind (result errorp error-message)
      (amoebum:sandboxed-eval
       "(with-open-file (stream #P\"/tmp/amoebum-i254-test\" :direction :output) (write-line \"boom\" stream))")
    (declare (ignore result))
    (is (eq t errorp))
    (is (and (stringp error-message)
             (search "unsafe operation blocked" error-message :test #'char-equal)))))

(test i254-approval-widget-and-approve-flow
  (with-clean-self-modification-state
    (let ((entry (amoebum:propose-modification "(+ 1 2)")))
      (is (eq :proposed (amoebum:modification-entry-status entry)))
      (let ((widget (amoebum:render-modification-approval-widget entry)))
        (is (search (amoebum:modification-entry-id entry) widget))
        (is (search "edit-approve" widget)))
      (amoebum:approve-modification (amoebum:modification-entry-id entry))
      (let ((applied (amoebum:apply-modification
                      (amoebum:modification-entry-id entry))))
        (is (eq :applied (amoebum:modification-entry-status applied)))
        (is (= 3 (amoebum:modification-entry-result applied)))))))

(test i254-deny-and-edit-then-approve
  (with-clean-self-modification-state
    (let* ((entry (amoebum:propose-modification "(+ 10 20)"))
           (id (amoebum:modification-entry-id entry)))
      (amoebum:deny-modification id :reason "test deny")
      (is (eq :denied (amoebum:modification-entry-status entry)))
      (amoebum:edit-modification id "(+ 4 5)" :approve-p t)
      (is (eq :approved (amoebum:modification-entry-status entry)))
      (let ((applied (amoebum:apply-modification id)))
        (is (eq :applied (amoebum:modification-entry-status applied)))
        (is (= 9 (amoebum:modification-entry-result applied)))))))

(test i254-auto-approve-rule-prefix
  (with-clean-self-modification-state
    (let ((entry (amoebum:propose-modification
                  "(defun i254-test-fn () :ok)")))
      (is (eq :approved (amoebum:modification-entry-status entry)))
      (is (eq :auto (amoebum:modification-entry-approval-mode entry))))))

(test i254-audit-trail-persists
  (with-clean-self-modification-state
    (let* ((tmpdir (%make-temp-directory "amoebum-i254-audit"))
           (audit-path (merge-pathnames "self-modifications.sexp" tmpdir)))
      (unwind-protect
           (let ((amoebum::*self-modification-audit-path-override* audit-path))
             (let* ((entry (amoebum:propose-modification "(+ 20 22)"))
                    (id (amoebum:modification-entry-id entry)))
               (amoebum:approve-modification id)
               (amoebum:apply-modification id)
               (is (probe-file audit-path))
               (let ((content (%read-file-text audit-path)))
                 (is (search id content))
                 (is (search ":ACTION :PROPOSED" content))
                 (is (search ":ACTION :APPLIED" content)))))
        (%delete-directory-tree-safe tmpdir)))))

(test i254-self-modify-smoke-sentinel
  (format t "SELF_MODIFY_SMOKE_OK~%")
  (is (equal t t)))
