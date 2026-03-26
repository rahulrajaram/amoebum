(in-package :amoebum/test)

(def-suite macroexpand-golden-suite :in amoebum-suite
  :description "Golden macroexpand regressions for R013 defskill/deftool refactors.")

(in-suite macroexpand-golden-suite)

(defun %macroexpand-golden-root ()
  (merge-pathnames #P"test/snapshots/macroexpand/"
                   (uiop:ensure-directory-pathname
                    (asdf:system-source-directory "amoebum"))))

(defun %render-macroexpand-1 (form)
  (with-output-to-string (stream)
    (let ((*package* (find-package :amoebum))
          (*print-case* :downcase)
          (*print-pretty* t)
          (*print-right-margin* 100)
          (*print-circle* nil)
          (*print-readably* t)
          (*compile-file-truename* nil)
          (*load-truename* #P"/dev/stdin"))
      (pprint (macroexpand-1 form) stream))))

(defun %normalize-golden-text (text)
  (string-right-trim '(#\Newline #\Return) text))

(defun %macroexpand-golden-form (source)
  (let ((*package* (find-package :cl-user)))
    (read-from-string source)))

(defun %assert-macroexpand-golden (form relative-path &key reset-deftool-p)
  (when reset-deftool-p
    (amoebum::reset-deftool-compile-validation-state))
  (let* ((path (merge-pathnames relative-path (%macroexpand-golden-root)))
         (expected (%normalize-golden-text (uiop:read-file-string path)))
         (actual (%normalize-golden-text (%render-macroexpand-1 form))))
    (is (string= expected actual)
        "Expected macroexpand golden ~A to match current expansion."
        (namestring path))))

(test deftool-macroexpand-matches-r013-golden
  (handler-bind ((warning
                   (lambda (condition)
                     (declare (ignore condition))
                     (let ((restart (find-restart 'muffle-warning)))
                       (when restart
                         (invoke-restart restart))))))
    (%assert-macroexpand-golden
     (%macroexpand-golden-form
      "(amoebum:deftool r013-golden-tool
          ((path pathname :description \"Absolute path\" :required t)
           (limit (or null integer) :description \"Line limit\" :default nil)
           (dry-run boolean :description \"Dry run\" :default nil)
           (mode (member :fast :slow) :description \"Mode\" :default :fast))
        \"Golden macroexpand tool\"
        (:permission :auto)
        (:dangerous t)
        (:category :smoke)
        (:timeout 9)
        (list :path path :limit limit :dry-run dry-run :mode mode))")
     #P"deftool.sexp"
     :reset-deftool-p t)))

(test defskill-macroexpand-matches-r013-golden
  (%assert-macroexpand-golden
   (%macroexpand-golden-form
    "(amoebum:defskill r013-golden-skill
        ((count :integer :required t :prompt \"Count:\" :choices '(1 2 3))
         (mode :keyword :required nil :default :fast :choices '(:fast :slow)
          :description \"Mode\"))
      \"Golden macroexpand skill\"
      (:category :smoke)
      (:usage \"/r013-golden-skill <count> [<mode>]\")
      (format nil \"count=~D mode=~A\" count mode))")
   #P"defskill.sexp"))
