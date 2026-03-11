(in-package :amoebum/test)

(def-suite permission-path-memory-suite
  :in amoebum-suite
  :description "Session-scoped path permission memory and /permissions UI tests (I350).")

(in-suite permission-path-memory-suite)

(defun %i350-temp-root ()
  (uiop:ensure-directory-pathname
   (merge-pathnames
    (make-pathname :directory `(:relative
                                 ,(format nil "amoebum-i350-~D-~D"
                                          (get-universal-time)
                                          (random 1000000))))
    (uiop:ensure-directory-pathname (uiop:temporary-directory)))))

(defmacro %with-i350-path-memory-isolated (&body body)
  `(let ((old-memory amoebum:*path-approval-memory*)
         (old-loaded amoebum::*path-approval-memory-loaded-p*))
     (unwind-protect
          (let ((amoebum:*path-approval-memory* '())
                (amoebum::*path-approval-memory-loaded-p* t))
            ,@body)
       (setf amoebum:*path-approval-memory* old-memory
             amoebum::*path-approval-memory-loaded-p* old-loaded))))

(defmacro %with-i350-project-root ((project-root) &body body)
  `(let* ((cfg (amoebum:current-config))
          (old-project-root (amoebum:config-project-root cfg)))
     (unwind-protect
          (progn
            (setf (amoebum:config-project-root cfg)
                  (uiop:ensure-directory-pathname ,project-root))
            ,@body)
       (setf (amoebum:config-project-root cfg) old-project-root))))

(test i350-allow-once-applies-to-single-followup-check
  (%with-i350-path-memory-isolated
    (let* ((root (%i350-temp-root))
           (project-root (merge-pathnames #P"project/" root))
           (path (namestring (merge-pathnames #P"inside/once-target.txt" project-root))))
      (ensure-directories-exist (merge-pathnames #P".keep" project-root))
      (unwind-protect
          (%with-i350-project-root (project-root)
            (amoebum:remember-path-approval :tool :write-file
                                            :path path
                                            :scope :once
                                            :persist-p nil)
            (is (eq (amoebum:check-permission :tool :write-file
                                              :path path
                                              :permission-mode :supervised
                                              :rules nil)
                    :allow))
            (is (eq (amoebum:check-permission :tool :write-file
                                              :path path
                                              :permission-mode :supervised
                                              :rules nil)
                    :prompt))
            (is (null (amoebum:list-path-approvals :scope :once))))
        (%delete-directory-tree-safe root)))))

(test i350-allow-for-session-reuses-exact-path
  (%with-i350-path-memory-isolated
    (let* ((root (%i350-temp-root))
           (project-root (merge-pathnames #P"project/" root))
           (path (namestring (merge-pathnames #P"inside/session-target.txt" project-root))))
      (ensure-directories-exist (merge-pathnames #P".keep" project-root))
      (unwind-protect
          (%with-i350-project-root (project-root)
            (amoebum:remember-path-approval :tool :write-file
                                            :path path
                                            :scope :session
                                            :persist-p nil)
            (is (eq (amoebum:check-permission :tool :write-file
                                              :path path
                                              :permission-mode :supervised
                                              :rules nil)
                    :allow))
            (is (eq (amoebum:check-permission :tool :write-file
                                              :path path
                                              :permission-mode :supervised
                                              :rules nil)
                    :allow))
            (is (= 1 (length (amoebum:list-path-approvals :scope :session))))
        (%delete-directory-tree-safe root))))))

(test i350-persistence-handoff-reloads-always-approvals
  (let ((root (%i350-temp-root))
        (old-memory amoebum:*path-approval-memory*)
        (old-loaded amoebum::*path-approval-memory-loaded-p*))
    (unwind-protect
         (let* ((target (merge-pathnames #P"src/persisted.txt" root))
                (path (namestring target)))
           (setf amoebum:*path-approval-memory* '()
                 amoebum::*path-approval-memory-loaded-p* t)
           (amoebum:clear-path-approvals :include-persistent t :project-root root)
           (amoebum:remember-path-approval :tool :read-file
                                           :path path
                                           :scope :always
                                           :persist-p t
                                           :project-root root)
           (is-true (probe-file (amoebum:path-approval-store-path :project-root root)))
           (setf amoebum:*path-approval-memory* '()
                 amoebum::*path-approval-memory-loaded-p* nil)
           (is (= 1 (amoebum:load-path-approvals :project-root root)))
           (let ((entries (amoebum:list-path-approvals :scope :always)))
             (is (= 1 (length entries)))
             (is (string= (amoebum:path-approval-entry-tool (first entries)) "read-file"))
             (is (string= (amoebum:path-approval-entry-path (first entries))
                          (amoebum:normalize-permission-path path)))))
      (setf amoebum:*path-approval-memory* old-memory
            amoebum::*path-approval-memory-loaded-p* old-loaded)
      (when (probe-file root)
        (ignore-errors
          (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))))

(test i350-permissions-command-shows-and-resets-session-path-memory
  (%with-i350-path-memory-isolated
    (let ((path "/tmp/i350/ui-target.txt"))
      (amoebum:remember-path-approval :tool :glob-files
                                      :path path
                                      :scope :session
                                      :persist-p nil)
      (multiple-value-bind (handled result)
          (amoebum:dispatch-slash-command "/permissions session")
        (is-true handled)
        (is-true (typep result 'amoebum:slash-command-result))
        (let ((output (or (amoebum:slash-command-result-output result) "")))
          (is-true (search "Session path approvals" output :test #'char-equal))
          (is-true (search "scope=session" output :test #'char-equal))
          (is-true (search path output :test #'char-equal))))
      (multiple-value-bind (handled result)
          (amoebum:dispatch-slash-command "/permissions reset session")
        (is-true handled)
        (is-true (typep result 'amoebum:slash-command-result))
        (let ((output (or (amoebum:slash-command-result-output result) "")))
          (is-true (search "Removed 1 path approval" output :test #'char-equal))))
      (is (null (amoebum:list-path-approvals :scope :session))))))
