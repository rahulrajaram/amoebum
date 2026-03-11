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

(defun %i348-write-text-file (path content)
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string content stream)))

(defmacro %with-i348-project-root ((project-root) &body body)
  `(let* ((cfg (amoebum:current-config))
          (old-project-root (amoebum:config-project-root cfg)))
     (unwind-protect
         (progn
           (setf (amoebum:config-project-root cfg)
                 (uiop:ensure-directory-pathname ,project-root))
           ,@body)
       (setf (amoebum:config-project-root cfg) old-project-root))))

(test permission-check-denies-path-traversal-outside-project-root
  (let* ((tmp-root (%make-temp-directory "amoebum-i348-traversal"))
         (project-root (merge-pathnames #P"project/" tmp-root))
         (rules nil))
    (ensure-directories-exist (merge-pathnames #P".keep" project-root))
    (unwind-protect
        (%with-i348-project-root (project-root)
          (is (eq (amoebum:check-permission
                   :tool :read-file
                   :path "../../etc/passwd"
                   :permission-mode :full-auto
                   :rules rules)
                  :deny))
          (let ((trace (amoebum:last-permission-decision-trace)))
            (is (eq (getf trace :decision-source) :project-root-guard))
            (is (eq (getf trace :reason-code)
                    :path-traversal-outside-project-root))
            (is-true (getf trace :outside-project-root-p))
            (is-true (getf trace :path-traversal-attempt-p))))
      (%delete-directory-tree-safe tmp-root))))

(test permission-check-denies-symlink-escape-outside-project-root
  (let* ((tmp-root (%make-temp-directory "amoebum-i348-symlink"))
         (project-root (merge-pathnames #P"project/" tmp-root))
         (outside-dir (merge-pathnames #P"outside/" tmp-root))
         (outside-file (merge-pathnames #P"outside/secret.txt" tmp-root))
         (link-path (merge-pathnames #P"project/link.txt" tmp-root)))
    (ensure-directories-exist (merge-pathnames #P".keep" project-root))
    (ensure-directories-exist (merge-pathnames #P".keep" outside-dir))
    (%i348-write-text-file outside-file "secret")
    (uiop:run-program (list "ln" "-sfn"
                            (namestring outside-file)
                            (namestring link-path))
                      :ignore-error-status nil)
    (unwind-protect
        (%with-i348-project-root (project-root)
          (is (eq (amoebum:check-permission
                   :tool :read-file
                   :path (namestring link-path)
                   :permission-mode :full-auto
                   :rules nil)
                  :deny))
          (let ((trace (amoebum:last-permission-decision-trace)))
            (is (eq (getf trace :decision-source) :project-root-guard))
            (is-true (getf trace :outside-project-root-p))))
      (%delete-directory-tree-safe tmp-root))))

(test permission-check-allows-outside-project-root-when-explicitly-allowed
  (let* ((tmp-root (%make-temp-directory "amoebum-i348-allow"))
         (project-root (merge-pathnames #P"project/" tmp-root))
         (outside-file (merge-pathnames #P"outside/allowed.txt" tmp-root))
         (rules (list (amoebum:make-permission-rule
                       :effect :allow
                       :path (namestring outside-file)
                       :tool :read-file
                       :source :project))))
    (ensure-directories-exist (merge-pathnames #P".keep" project-root))
    (%i348-write-text-file outside-file "allowed")
    (unwind-protect
        (%with-i348-project-root (project-root)
          (is (eq (amoebum:check-permission
                   :tool :read-file
                   :path (namestring outside-file)
                   :permission-mode :full-auto
                   :rules rules)
                  :allow)))
      (%delete-directory-tree-safe tmp-root))))

(test permission-check-uses-canonical-comparison-for-dot-segments
  (let* ((tmp-root (%make-temp-directory "amoebum-i348-canonical"))
         (project-root (merge-pathnames #P"project/" tmp-root))
         (target (merge-pathnames #P"project/src/main.lisp" tmp-root))
         (non-canonical (merge-pathnames #P"project/src/./nested/../main.lisp" tmp-root))
         (rules (list (amoebum:make-permission-rule
                       :effect :deny
                       :path (namestring target)
                       :tool :read-file
                       :source :project))))
    (ensure-directories-exist target)
    (%i348-write-text-file target "(print :ok)")
    (unwind-protect
        (%with-i348-project-root (project-root)
          (is (eq (amoebum:check-permission
                   :tool :read-file
                   :path (namestring non-canonical)
                   :permission-mode :full-auto
                   :rules rules)
                  :deny)))
      (%delete-directory-tree-safe tmp-root))))
