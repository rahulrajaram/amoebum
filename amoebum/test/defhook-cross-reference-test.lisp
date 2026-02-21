(in-package :amoebum/test)

(def-suite defhook-cross-reference-suite
  :description "Compile-time defhook tool cross-reference warnings (I204)."
  :in amoebum-suite)

(in-suite defhook-cross-reference-suite)

(defun %capture-style-warnings (thunk)
  (let ((captured '()))
    (handler-bind
        ((style-warning
           (lambda (condition)
             (push condition captured)
             (let ((restart (find-restart 'muffle-warning condition)))
               (when restart
                 (invoke-restart restart))))))
      (funcall thunk))
    (nreverse captured)))

(defun %hash-table->alist (table)
  (let ((out '()))
    (maphash (lambda (key value)
               (push (cons key value) out))
             table)
    out))

(defun %restore-hash-table (table entries)
  (clrhash table)
  (dolist (entry entries)
    (setf (gethash (car entry) table) (cdr entry)))
  table)

(test defhook-cross-reference-warns-on-unknown-tool
  (let* ((registry amoebum::*deftool-compile-time-tool-names*)
         (original (%hash-table->alist registry)))
    (unwind-protect
         (progn
           (clrhash registry)
           (let ((warnings
                   (%capture-style-warnings
                    (lambda ()
                      (macroexpand-1
                       '(amoebum:defhook pre-tool-use (tool-name args)
                         (:match (:tool "missing-tool")
                           :allow)))))))
             (is (= 1 (length warnings)))
             (is (typep (first warnings) 'amoebum::unknown-tool-reference))
             (is (string=
                  "missing-tool"
                  (amoebum::unknown-tool-reference-reference (first warnings))))))
      (%restore-hash-table registry original))))

(test defhook-cross-reference-allows-known-tool
  (let* ((registry amoebum::*deftool-compile-time-tool-names*)
         (original (%hash-table->alist registry)))
    (unwind-protect
         (progn
           (clrhash registry)
           (setf (gethash "known-tool" registry) t)
           (let ((warnings
                   (%capture-style-warnings
                    (lambda ()
                      (macroexpand-1
                       '(amoebum:defhook pre-tool-use (tool-name args)
                         (:match (:tool "known-tool")
                           :allow)))))))
             (is (null warnings))))
      (%restore-hash-table registry original))))
