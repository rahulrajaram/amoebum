(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Self-Modifying Agent Smoke Tests (I98)
;;; ---------------------------------------------------------------------------

(def-suite self-modify-suite :in amoebum-suite
  :description "Self-modification smoke tests.")

(in-suite self-modify-suite)

(defmacro with-clean-journal (&body body)
  `(let ((amoebum::*modification-journal* '())
         (amoebum::*modification-counter* 0))
     ,@body))

(test modification-entry-creation
  (let ((entry (amoebum::make-modification-entry
                :id "test-1"
                :form-text "(+ 1 2)"
                :status :proposed)))
    (is (amoebum::modification-entry-p entry))
    (is (string= "test-1" (amoebum::modification-entry-id entry)))
    (is (eq :proposed (amoebum::modification-entry-status entry)))))

(test propose-modification-creates-entry
  (with-clean-journal
    (let* ((bus (amoebum:make-event-bus))
           (entry (amoebum::propose-modification "(+ 1 2)" :event-bus bus)))
      (is (amoebum::modification-entry-p entry))
      (is (eq :proposed (amoebum::modification-entry-status entry)))
      (is (= 1 (length (amoebum::modification-journal)))))))

(test approve-modification-changes-status
  (with-clean-journal
    (let* ((bus (amoebum:make-event-bus))
           (entry (amoebum::propose-modification "(+ 1 2)" :event-bus bus))
           (id (amoebum::modification-entry-id entry)))
      (amoebum::approve-modification id :event-bus bus)
      (is (eq :approved (amoebum::modification-entry-status entry))))))

(test approve-nonexistent-errors
  (with-clean-journal
    (let ((bus (amoebum:make-event-bus)))
      (signals error (amoebum::approve-modification "nonexistent" :event-bus bus)))))

(test sandboxed-eval-basic
  (multiple-value-bind (result errorp error-msg)
      (amoebum::sandboxed-eval "(+ 1 2)")
    (is (= 3 result))
    (is (null errorp))
    (is (null error-msg))))

(test sandboxed-eval-error
  (multiple-value-bind (result errorp error-msg)
      (amoebum::sandboxed-eval "(error \"boom\")")
    (declare (ignore result))
    (is (eq t errorp))
    (is (stringp error-msg))))

(test sandboxed-eval-parse-error
  (multiple-value-bind (result errorp error-msg)
      (amoebum::sandboxed-eval "(((broken")
    (declare (ignore result))
    (is (eq t errorp))
    (is (stringp error-msg))))

(test apply-modification-simple
  (with-clean-journal
    (let* ((bus (amoebum:make-event-bus))
           (amoebum::*self-modify-auto-approve-p* t)
           (entry (amoebum::propose-modification "(+ 10 20)" :event-bus bus))
           (id (amoebum::modification-entry-id entry)))
      (amoebum::approve-modification id :event-bus bus)
      (let ((result (amoebum::apply-modification id :event-bus bus)))
        (is (eq :applied (amoebum::modification-entry-status result)))
        (is (= 30 (amoebum::modification-entry-result result)))))))

(test apply-unapproved-errors
  (with-clean-journal
    (let* ((bus (amoebum:make-event-bus))
           (entry (amoebum::propose-modification "(+ 1 2)" :event-bus bus))
           (id (amoebum::modification-entry-id entry)))
      (signals error (amoebum::apply-modification id :event-bus bus)))))

(test collect-defined-symbols-defun
  (let ((syms (amoebum::%collect-defined-symbols '(defun foo (x) x))))
    (is (= 1 (length syms)))
    (is (eq 'foo (first syms)))))

(test collect-defined-symbols-progn
  (let ((syms (amoebum::%collect-defined-symbols
               '(progn (defun bar () nil) (defvar *baz* 42)))))
    (is (= 2 (length syms)))))

(test pending-modifications-query
  (with-clean-journal
    (let ((bus (amoebum:make-event-bus)))
      (amoebum::propose-modification "(+ 1 2)" :event-bus bus)
      (amoebum::propose-modification "(+ 3 4)" :event-bus bus)
      (is (= 2 (length (amoebum::pending-modifications)))))))

(test journal-summary
  (with-clean-journal
    (let ((bus (amoebum:make-event-bus)))
      (amoebum::propose-modification "(+ 1 2)" :event-bus bus)
      (let ((summary (amoebum::modification-journal-summary)))
        (is (stringp summary))
        (is (search "1 entries" summary))))))

(test clear-journal
  (with-clean-journal
    (let ((bus (amoebum:make-event-bus)))
      (amoebum::propose-modification "(+ 1 2)" :event-bus bus)
      (amoebum::clear-modification-journal)
      (is (null (amoebum::modification-journal))))))
