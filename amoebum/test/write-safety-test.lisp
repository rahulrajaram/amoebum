(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Write Safety Tests (I107)
;;; ---------------------------------------------------------------------------

(def-suite write-safety-suite :in amoebum-suite
  :description "Write safety check tests (I107).")

(in-suite write-safety-suite)

(defmacro %with-write-safety-project-root ((project-root) &body body)
  `(let* ((config (amoebum.config:current-config))
          (original-project-root (amoebum.config:config-project-root config))
          (resolved-project-root (or (ignore-errors (truename ,project-root))
                                     ,project-root)))
     (unwind-protect
         (progn
           (setf (amoebum.config:config-project-root config) resolved-project-root)
           ,@body)
       (setf (amoebum.config:config-project-root config) original-project-root))))

;;; --- Forbidden system path tests -------------------------------------------

(test write-safety-blocks-etc-path
  "Writing to /etc/passwd must be denied."
  (signals amoebum:write-safety-denied
    (amoebum:check-write-safety "/etc/passwd"
                                :tool "write-file"
                                :permission-mode :full-auto)))

(test write-safety-blocks-usr-path
  "Writing to /usr/local/bin/foo must be denied."
  (signals amoebum:write-safety-denied
    (amoebum:check-write-safety "/usr/local/bin/foo"
                                :tool "write-file"
                                :permission-mode :full-auto)))

(test write-safety-blocks-boot-path
  "Writing to /boot/vmlinuz must be denied."
  (signals amoebum:write-safety-denied
    (amoebum:check-write-safety "/boot/vmlinuz"
                                :tool "write-file"
                                :permission-mode :full-auto)))

(test write-safety-blocks-proc-path
  "Writing to /proc/self/status must be denied."
  (signals amoebum:write-safety-denied
    (amoebum:check-write-safety "/proc/self/status"
                                :tool "write-file"
                                :permission-mode :full-auto)))

(test write-safety-blocks-dev-path
  "Writing to /dev/null must be denied."
  (signals amoebum:write-safety-denied
    (amoebum:check-write-safety "/dev/null"
                                :tool "write-file"
                                :permission-mode :full-auto)))

;;; --- Deny pattern file name tests ------------------------------------------

(test write-safety-blocks-dotenv
  "Writing to .env file must be denied."
  (signals amoebum:write-safety-denied
    (amoebum:check-write-safety "/home/user/project/.env"
                                :tool "write-file"
                                :permission-mode :full-auto)))

(test write-safety-blocks-env-production
  "Writing to .env.production file must be denied."
  (signals amoebum:write-safety-denied
    (amoebum:check-write-safety "/home/user/project/.env.production"
                                :tool "write-file"
                                :permission-mode :full-auto)))

(test write-safety-blocks-credentials-json
  "Writing to credentials.json must be denied."
  (signals amoebum:write-safety-denied
    (amoebum:check-write-safety "/home/user/project/credentials.json"
                                :tool "write-file"
                                :permission-mode :full-auto)))

(test write-safety-blocks-id-rsa
  "Writing to id_rsa must be denied."
  (signals amoebum:write-safety-denied
    (amoebum:check-write-safety "/home/user/.ssh/id_rsa"
                                :tool "write-file"
                                :permission-mode :full-auto)))

(test write-safety-blocks-secrets-yaml
  "Writing to secrets.yml must be denied."
  (signals amoebum:write-safety-denied
    (amoebum:check-write-safety "/home/user/project/secrets.yml"
                                :tool "write-file"
                                :permission-mode :full-auto)))

(test write-safety-blocks-npmrc
  "Writing to .npmrc must be denied."
  (signals amoebum:write-safety-denied
    (amoebum:check-write-safety "/home/user/.npmrc"
                                :tool "write-file"
                                :permission-mode :full-auto)))

(test write-safety-blocks-netrc
  "Writing to .netrc must be denied."
  (signals amoebum:write-safety-denied
    (amoebum:check-write-safety "/home/user/.netrc"
                                :tool "write-file"
                                :permission-mode :full-auto)))

;;; --- Nil/empty path tests --------------------------------------------------

(test write-safety-blocks-nil-path
  "Writing to nil path must be denied."
  (signals amoebum:write-safety-denied
    (amoebum:check-write-safety nil
                                :tool "write-file"
                                :permission-mode :full-auto)))

(test write-safety-blocks-empty-path
  "Writing to empty string path must be denied."
  (signals amoebum:write-safety-denied
    (amoebum:check-write-safety ""
                                :tool "write-file"
                                :permission-mode :full-auto)))

;;; --- Permission-rule deny tests --------------------------------------------

(test write-safety-blocks-denied-by-rule
  "Writing to a path denied by permission rules must be denied."
  (let ((amoebum:*permission-rules* nil))
    (amoebum:add-permission-rule :effect :deny
                                  :path "/home/user/project/protected/**"
                                  :tool "write-file")
    (signals amoebum:write-safety-denied
      (amoebum:check-write-safety "/home/user/project/protected/data.txt"
                                  :tool "write-file"
                                  :permission-mode :full-auto
                                  :rules amoebum:*permission-rules*))))

;;; --- Allowed write tests ---------------------------------------------------

(test write-safety-allows-normal-path
  "Writing to a normal project path should succeed."
  (let ((amoebum:*permission-rules* nil))
    (amoebum:add-permission-rule :effect :allow
                                  :path "/home/user/project/**"
                                  :tool "write-file")
    (is-true (amoebum:check-write-safety "/home/user/project/src/main.lisp"
                                          :tool "write-file"
                                          :permission-mode :full-auto
                                          :rules amoebum:*permission-rules*))))

(test write-safety-allows-tmp-path
  "Writing to /tmp should succeed (not in forbidden list)."
  (let ((amoebum:*permission-rules* nil))
    (%with-write-safety-project-root ((%amoebum-system-root))
      (is-true (amoebum:check-write-safety
                (namestring (merge-pathnames #P".tmp-test-work/scratch.txt"
                                             (%amoebum-system-root)))
                :tool "write-file"
                :permission-mode :full-auto
                :rules amoebum:*permission-rules*)))))

;;; --- Non-signaling variant tests -------------------------------------------

(test write-safety-check-p-returns-nil-for-blocked
  "write-safety-check-p returns NIL plus reason for blocked paths."
  (multiple-value-bind (ok reason)
      (amoebum:write-safety-check-p "/etc/shadow"
                                     :tool "write-file"
                                     :permission-mode :full-auto)
    (is (null ok))
    (is (stringp reason))
    (is (search "forbidden" reason :test #'char-equal))))

(test write-safety-check-p-returns-t-for-allowed
  "write-safety-check-p returns T for allowed paths."
  (let ((amoebum:*permission-rules* nil))
    (%with-write-safety-project-root ((%amoebum-system-root))
      (multiple-value-bind (ok reason)
          (amoebum:write-safety-check-p
           (namestring (merge-pathnames #P".tmp-test-work/safe-file.txt"
                                        (%amoebum-system-root)))
           :tool "write-file"
           :permission-mode :full-auto)
        (is-true ok)
        (is (null reason))))))

;;; --- Error message clarity tests -------------------------------------------

(test write-safety-error-message-includes-path
  "Error message should include the blocked path."
  (handler-case
      (amoebum:check-write-safety "/etc/hosts"
                                  :tool "write-file"
                                  :permission-mode :full-auto)
    (amoebum:write-safety-denied (c)
      (let ((msg (princ-to-string c)))
        (is (search "/etc/hosts" msg)
            "Error message should include path /etc/hosts, got: ~A" msg)
        (is (search "forbidden" msg :test #'char-equal)
            "Error message should mention 'forbidden', got: ~A" msg)))))

(test write-safety-error-message-includes-pattern-name
  "Error message for deny-pattern should describe the match."
  (handler-case
      (amoebum:check-write-safety "/home/user/.env"
                                  :tool "write-file"
                                  :permission-mode :full-auto)
    (amoebum:write-safety-denied (c)
      (let ((msg (princ-to-string c)))
        (is (search ".env" msg)
            "Error message should reference .env, got: ~A" msg)
        (is (search "sensitive" msg :test #'char-equal)
            "Error message should mention 'sensitive', got: ~A" msg)))))

(test write-safety-denied-condition-has-accessors
  "write-safety-denied condition has path and denial-reason accessors."
  (handler-case
      (amoebum:check-write-safety "/usr/bin/test"
                                  :tool "write-file"
                                  :permission-mode :full-auto)
    (amoebum:write-safety-denied (c)
      (is (stringp (amoebum:write-safety-denied-path c)))
      (is (stringp (amoebum:write-safety-denied-denial-reason c)))
      (is (search "/usr/" (amoebum:write-safety-denied-denial-reason c))))))
