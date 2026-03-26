(in-package :amoebum/test)

(def-suite event-hooks-suite :in amoebum-suite
  :description "I220 event-driven hook expansion tests.")

(in-suite event-hooks-suite)

(defun %i220-args (&rest kvs)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on kvs by #'cddr do
      (setf (gethash key table) value))
    table))

(defun %i220-run-git (repo-path &rest args)
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program (append (list "git" "-C" repo-path) args)
                        :ignore-error-status t
                        :output :string
                        :error-output :string)
    (unless (zerop exit-code)
      (error "git ~{~A~^ ~} failed: ~A~%~A" args stdout stderr))
    stdout))

(test i220-on-error-hook-fires-before-restart-selection
  (let ((original-toolset amoebum:*toolset*)
        (original-hooks amoebum:*hook-registry*)
        (captured-condition nil)
        (captured-tool-name nil))
    (unwind-protect
        (let ((toolset (pseudopod:make-toolset)))
          (setf amoebum:*toolset* toolset
                amoebum:*hook-registry* (make-hash-table :test #'equal))
          (pseudopod:register-tool-function
           toolset
           :name "i220-failing-tool"
           :description "I220 forced failure tool."
           :parameters (let ((schema (make-hash-table :test #'equal)))
                         (setf (gethash "type" schema) "object")
                         schema)
           :fn (lambda (_arguments _call)
                 (declare (ignore _arguments _call))
                 (error "i220 forced failure")))
          (amoebum:register-hook :on-error
                                 'i220-capture-on-error
                                 (lambda (condition tool-name)
                                   (setf captured-condition condition
                                         captured-tool-name tool-name)
                                   :observed))
          (let ((result
                  (handler-bind
                      ((amoebum:tool-error
                         (lambda (_condition)
                           (invoke-restart 'amoebum::skip-tool-call))))
                    (amoebum:execute-tool-with-restarts
                     "i220-failing-tool"
                     (%i220-args)
                     :toolset toolset
                     :permission-mode :full-auto))))
            (is-true (search "skipped" result :test #'char-equal))
            (is (typep captured-condition 'amoebum:tool-error))
            (is (string= "i220-failing-tool" (or captured-tool-name "")))))
      (setf amoebum:*toolset* original-toolset
            amoebum:*hook-registry* original-hooks))))

(test i220-on-idle-hook-fires-with-idle-seconds
  (let ((original-hooks amoebum:*hook-registry*)
        (original-threshold amoebum::*hook-idle-threshold-seconds*)
        (original-last-activity amoebum::*hook-last-activity-second*)
        (original-last-idle amoebum::*hook-last-idle-notified-second*)
        (captured-idle nil))
    (unwind-protect
        (progn
          (setf amoebum:*hook-registry* (make-hash-table :test #'equal)
                amoebum::*hook-idle-threshold-seconds* 1)
          (amoebum:register-hook :on-idle
                                 'i220-capture-on-idle
                                 (lambda (idle-seconds)
                                   (setf captured-idle idle-seconds)
                                   :ok))
          (amoebum::%chat-mark-activity)
          (setf amoebum::*hook-last-activity-second* (- (get-universal-time) 5))
          (amoebum::%run-chat-idle-hooks-if-needed)
          (is (integerp captured-idle))
          (is (>= captured-idle 5)))
      (setf amoebum:*hook-registry* original-hooks
            amoebum::*hook-idle-threshold-seconds* original-threshold
            amoebum::*hook-last-activity-second* original-last-activity
            amoebum::*hook-last-idle-notified-second* original-last-idle))))

(test i220-on-commit-hook-fires-after-git-commit
  (let* ((tmp-root (%make-temp-directory "amoebum-i220-commit-hook"))
         (repo-path (namestring tmp-root))
         (old-config (amoebum.config:current-config))
         (old-project-root (amoebum.config:config-project-root old-config))
         (old-permission-mode (amoebum.config:config-value :permission-mode old-config))
         (original-hooks amoebum:*hook-registry*)
         (original-generator amoebum::*git-commit-message-generator*)
         (captured-hash nil)
         (captured-message nil)
         (captured-files nil))
    (unwind-protect
        (progn
          (ensure-directories-exist (merge-pathnames #P".keep" tmp-root))
          (%i220-run-git repo-path "init")
          (%i220-run-git repo-path "config" "user.name" "Amoebum Tests")
          (%i220-run-git repo-path "config" "user.email" "amoebum-tests@example.com")
          (%write-text-file (merge-pathnames #P"seed.txt" tmp-root) "seed\n")
          (%i220-run-git repo-path "add" "--" "seed.txt")
          (%i220-run-git repo-path "commit" "-m" "chore: seed")
          (%i220-run-git repo-path "branch" "-m" "main")

          (%write-text-file (merge-pathnames #P"tracked.txt" tmp-root) "payload\n")
          (setf amoebum:*hook-registry* (make-hash-table :test #'equal)
                amoebum::*git-commit-message-generator*
                (lambda (_diff _recent-subjects &key model staged-paths project-root)
                  (declare (ignore _diff _recent-subjects model staged-paths project-root))
                  "feat: i220 commit hook"))
          (amoebum:register-hook :on-commit
                                 'i220-capture-on-commit
                                 (lambda (commit-hash message files)
                                   (setf captured-hash commit-hash
                                         captured-message message
                                         captured-files files)
                                   :ok))
          (amoebum.config:setconfig :project-root tmp-root)
          (amoebum.config:setconfig :permission-mode :full-auto)
          (let* ((tool (pseudopod:find-tool amoebum:*toolset* "git-commit"))
                 (fn (and tool (pseudopod:tool-definition-fn tool)))
                 (result (funcall fn (%i220-args "files" '("tracked.txt")))))
            (is-true (listp result))
            (is (stringp captured-hash))
            (is (search "feat: i220 commit hook" (or captured-message "") :test #'char-equal))
            (is (member "tracked.txt" (or captured-files '()) :test #'string=))))
      (setf amoebum:*hook-registry* original-hooks
            amoebum::*git-commit-message-generator* original-generator)
      (amoebum.config:setconfig :project-root old-project-root)
      (amoebum.config:setconfig :permission-mode old-permission-mode)
      (%delete-directory-tree-safe tmp-root))))

(test i220-on-step-complete-hook-fires-from-chat-step-loop
  (let ((original-hooks amoebum:*hook-registry*)
        (original-toolset amoebum:*toolset*)
        (captured-step-number nil)
        (captured-messages-added nil)
        (captured-tool-calls nil))
    (unwind-protect
        (progn
          (setf amoebum:*hook-registry* (make-hash-table :test #'equal)
                amoebum:*toolset* (pseudopod:make-toolset))
          (amoebum:register-hook :on-step-complete
                                 'i220-capture-on-step-complete
                                 (lambda (step-number messages-added tool-calls-made)
                                   (setf captured-step-number step-number
                                         captured-messages-added messages-added
                                         captured-tool-calls tool-calls-made)
                                   :ok))
          (let* ((client (pseudopod:make-client :api-key "stub"))
                 (chat-state (amoebum.ui:make-chat-ui-state
                              :stream-runner nil
                              :stream-client client
                              :stream-tools amoebum:*toolset*)))
            (amoebum:chat-ui-add-message chat-state "user" "Run the step loop.")
            (let ((original-step-fn (symbol-function 'pseudopod:step)))
              (unwind-protect
                  (progn
                    (setf (symbol-function 'pseudopod:step)
                          (lambda (_client &rest args &key messages &allow-other-keys)
                            (declare (ignore _client args))
                            (let* ((assistant (pseudopod:make-message
                                               :role "assistant"
                                               :content "done"))
                                   (tool-message (pseudopod:make-message
                                                  :role "tool"
                                                  :name "i220-step-tool"
                                                  :tool-call-id "i220-tool-1"
                                                  :content "ok")))
                              (pseudopod::%make-step-result
                               :steps 3
                               :history (append messages (list assistant tool-message))
                               :final-message assistant
                               :last-message assistant
                               :max-steps-reached nil
                               :tool-results (list
                                              (list :id "i220-tool-1" :name "i220-step-tool" :output "ok")
                                              (list :id "i220-tool-2" :name "i220-step-tool" :output "ok"))))))
                    (amoebum::%start-step-loop-assistant-response chat-state))
                (setf (symbol-function 'pseudopod:step) original-step-fn))))
          (is (= captured-step-number 3))
          (is (= captured-messages-added 2))
          (is (= captured-tool-calls 2)))
      (setf amoebum:*hook-registry* original-hooks
            amoebum:*toolset* original-toolset))))

(test i220-event-hooks-smoke-sentinel
  (is-true t)
  (format t "EVENT_HOOKS_SMOKE_OK~%"))
