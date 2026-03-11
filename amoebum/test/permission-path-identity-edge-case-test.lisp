(in-package :amoebum/test)

(def-suite permission-path-identity-edge-case-suite
  :in amoebum-suite
  :description "Path identity edge-case protections (I349).")

(in-suite permission-path-identity-edge-case-suite)

(defun %i349-run-command (&rest argv)
  (multiple-value-bind (_stdout stderr exit-code)
      (uiop:run-program argv
                        :ignore-error-status t
                        :output :string
                        :error-output :string)
    (declare (ignore _stdout))
    (unless (zerop (or exit-code 1))
      (error "Command failed: ~{~A~^ ~} => ~A" argv stderr))))

(test i349-unicode-normalized-path-identity-matches-exact-rules
  (let* ((tmp-root (%make-temp-directory "amoebum-i349-unicode"))
         (composed-name
           (concatenate 'string "caf" (string (code-char #x00E9)) ".txt"))
         (decomposed-name
           (concatenate 'string "caf" (string (code-char #x0065))
                        (string (code-char #x0301)) ".txt"))
         (composed-path (merge-pathnames (pathname composed-name) tmp-root))
         (decomposed-path (merge-pathnames (pathname decomposed-name) tmp-root))
         (rules (list (amoebum:make-permission-rule
                       :effect :allow
                       :tool :write-file
                       :path (namestring composed-path)
                       :source :project)))
         (decision (amoebum:check-permission
                    :tool :write-file
                    :path (namestring decomposed-path)
                    :permission-mode :supervised
                    :rules rules))
         (normalization-supported-p
           (string= (amoebum::%unicode-normalize-path composed-name)
                    (amoebum::%unicode-normalize-path decomposed-name))))
    (if normalization-supported-p
        (is (eq decision :allow))
        (is (member decision '(:allow :prompt) :test #'eq)))))

(test i349-case-policy-can-enforce-case-insensitive-identity-comparisons
  (let* ((rules (list (amoebum:make-permission-rule
                       :effect :deny
                       :tool :write-file
                       :path "/tmp/I349/CaseSensitive.txt"
                       :source :project)))
         (amoebum::*permission-path-case-sensitive-p* nil))
    (is (eq (amoebum:check-permission
             :tool :write-file
             :path "/tmp/i349/casesensitive.txt"
             :permission-mode :full-auto
             :rules rules)
            :deny))))

(test i349-hardlink-identity-matches-exact-path-rules
  (let* ((tmp-root (%make-temp-directory "amoebum-i349-hardlink"))
         (origin-path (merge-pathnames #P"real/secret.txt" tmp-root))
         (alias-path (merge-pathnames #P"alias/secret-link.txt" tmp-root)))
    (unwind-protect
        (progn
          (%write-text-file origin-path "secret")
          (ensure-directories-exist alias-path)
          (%i349-run-command "ln"
                             "-f"
                             (namestring origin-path)
                             (namestring alias-path))
          (let* ((rules (list (amoebum:make-permission-rule
                               :effect :deny
                               :tool :write-file
                               :path (namestring origin-path)
                               :source :project)))
                 (decision (amoebum:check-permission
                            :tool :write-file
                            :path (namestring alias-path)
                            :permission-mode :full-auto
                            :rules rules))
                 (inode-supported-p
                   (and (amoebum::%path-inode-signature (namestring origin-path))
                        (amoebum::%path-inode-signature (namestring alias-path)))))
            (if inode-supported-p
                (is (eq decision :deny))
                (is (member decision '(:deny :allow :prompt) :test #'eq)))))
      (%delete-directory-tree-safe tmp-root))))

(test i349-write-use-time-recheck-detects-symlink-retarget-race
  (let* ((tmp-root (%make-temp-directory "amoebum-i349-recheck"))
         (link-path (merge-pathnames #P"links/work.txt" tmp-root))
         (target-a (merge-pathnames #P"targets/a.txt" tmp-root))
         (target-b (merge-pathnames #P"targets/b.txt" tmp-root))
         (config (amoebum:current-config))
         (old-mode (amoebum:config-permission-mode config))
         (old-rules amoebum:*permission-rules*)
         (old-hook amoebum::*permission-path-identity-recheck-hook*))
    (unwind-protect
        (progn
          (%write-text-file target-a "A")
          (%write-text-file target-b "B")
          (ensure-directories-exist link-path)
          (%i349-run-command "ln"
                             "-sfn"
                             (namestring target-a)
                             (namestring link-path))
          (setf amoebum:*permission-rules* nil)
          (clrhash amoebum::*permission-path-identity-check-cache*)
          (amoebum:setconfig :permission-mode :full-auto)
          (is (eq (amoebum:check-permission
                   :tool :write-file
                   :path (namestring link-path)
                   :permission-mode :full-auto
                   :rules nil)
                  :allow))
          (let* ((retargeted-p nil)
                 (amoebum::*permission-path-identity-recheck-hook*
                   (lambda (&key tool request-path expected)
                     (declare (ignore tool request-path expected))
                     (unless retargeted-p
                       (%i349-run-command "ln"
                                          "-sfn"
                                          (namestring target-b)
                                          (namestring link-path))
                       (setf retargeted-p t)))))
            (handler-case
                (progn
                  (amoebum::%write-file-string link-path "updated")
                  (fail "Expected write-time path identity recheck to block retargeted symlink."))
              (amoebum:tool-permission-denied (condition)
                (is (eq (amoebum:tool-error-reason-code condition)
                        :path-identity-changed))))))
      (setf amoebum:*permission-rules* old-rules
            amoebum::*permission-path-identity-recheck-hook* old-hook)
      (amoebum:setconfig :permission-mode old-mode)
      (%delete-directory-tree-safe tmp-root))))
