(in-package :amoebum/test)

(def-suite permission-path-normalization-suite
  :in amoebum-suite
  :description "Permission path normalization and canonical matching tests (I129).")

(in-suite permission-path-normalization-suite)

(test permission-normalize-posix-equivalents
  (is (string= (amoebum:normalize-permission-path "/tmp/project/./src/../src/main.lisp")
               "/tmp/project/src/main.lisp"))
  (is (string= (amoebum:normalize-permission-path "/tmp//project///src/main.lisp/")
               "/tmp/project/src/main.lisp"))
  (is (string= (amoebum:normalize-permission-path "/tmp/project/src/" :preserve-trailing-slash-p t)
               "/tmp/project/src/")))

(test permission-normalize-windows-drive-equivalents
  (is (string= (amoebum:normalize-permission-path "C:\\Work\\repo\\src\\..\\src\\main.lisp")
               "c:/Work/repo/src/main.lisp"))
  (is (string= (amoebum:normalize-permission-path "c:/Work/repo/src/main.lisp")
               "c:/Work/repo/src/main.lisp"))
  (is (string= (amoebum:normalize-permission-path "C:\\")
               "c:/")))

(test permission-normalize-unc-equivalents
  (is (string= (amoebum:normalize-permission-path "\\\\Server\\Share\\src\\..\\main.lisp")
               "//server/share/main.lisp"))
  (is (string= (amoebum:normalize-permission-path "//server/share/main.lisp")
               "//server/share/main.lisp")))

(test permission-evaluation-uses-canonicalized-path
  (let ((rules (list (amoebum:make-permission-rule
                      :effect :deny
                      :path "C:\\Work\\repo\\src\\main.lisp"
                      :tool :write-file
                      :source :project))))
    (is (eq (amoebum:evaluate-path-permission
             :tool :write-file
             :path "c:/Work/repo/src/./main.lisp"
             :rules rules)
            :deny))))

(test permission-directory-and-glob-patterns-survive-normalization
  (let ((directory-rules (list (amoebum:make-permission-rule
                                :effect :allow
                                :path "/tmp/project/src/"
                                :tool :write-file
                                :source :project)))
        (glob-rules (list (amoebum:make-permission-rule
                           :effect :allow
                           :path "C:\\Work\\repo\\**\\*.lisp"
                           :tool :write-file
                           :source :project))))
    (is (eq (amoebum:evaluate-path-permission
             :tool :write-file
             :path "/tmp/project/src/./nested/file.lisp"
             :rules directory-rules)
            :allow))
    (is (eq (amoebum:evaluate-path-permission
             :tool :write-file
             :path "c:/Work/repo/src/module/main.lisp"
             :rules glob-rules)
            :allow))))
