(in-package :amoebum/test)

(def-suite session-resume-suite
  :description "I338 session resume semantics across invocations."
  :in amoebum-suite)

(in-suite session-resume-suite)

(defun %i338-build-saved-session (project-root session-id)
  (let* ((tool-call-id (format nil "call-~A" session-id))
         (conversation (amoebum:make-conversation-state
                        :project-root project-root
                        :session-id session-id
                        :state :tool-executing)))
    (amoebum:conversation-state-add-message
     conversation
     (pseudopod:make-message :role "user" :content "List project files.")
     :save-p nil)
    (amoebum:conversation-state-add-message
     conversation
     (pseudopod:make-message :role "assistant"
                             :content "Running shell tool."
                             :tool-call-id tool-call-id)
     :save-p nil)
    (amoebum:conversation-state-add-message
     conversation
     (pseudopod:make-message :role "tool"
                             :name "shell"
                             :tool-call-id tool-call-id
                             :content "README.md")
     :save-p nil)
    (amoebum:conversation-save conversation)
    conversation))

(test cli-resume-by-id-is-deterministic
  (let* ((tmp-root (%make-temp-directory "amoebum-i338-cli"))
         (old-project-root (amoebum:config-project-root (amoebum:current-config))))
    (unwind-protect
         (progn
           (amoebum:setconfig :project-root tmp-root)
           (%i338-build-saved-session tmp-root "resume-alpha")
           (%i338-build-saved-session tmp-root "resume-beta")
           (let ((resolved (amoebum::%resolve-cli-conversation :resume "resume-alpha")))
             (is (string= "resume-alpha"
                          (amoebum:conversation-state-session-id resolved)))
             (is (eq :tool-executing
                     (amoebum:conversation-state-state resolved)))
             (is (= 3 (length (amoebum:conversation-state-entries resolved))))
             (let ((third (third (amoebum:conversation-state-messages resolved))))
               (is (string= "tool" (pseudopod:message-role third)))
               (is (string= "shell" (or (pseudopod:message-name third) "")))
               (is-true
                (plusp (length (or (pseudopod:message-tool-call-id third) ""))))))
           (is-true
            (handler-case
                (progn
                  (amoebum::%resolve-cli-conversation :resume "does-not-exist")
                  nil)
              (error ()
                t))))
      (amoebum:setconfig :project-root old-project-root)
      (%delete-directory-tree-safe tmp-root))))

(test session-slash-command-resume-restores-chat-state
  (let* ((tmp-root (%make-temp-directory "amoebum-i338-session"))
         (old-project-root (amoebum:config-project-root (amoebum:current-config)))
         (chat-state (amoebum:make-chat-ui-state :stream-runner nil)))
    (unwind-protect
         (progn
           (amoebum:setconfig :project-root tmp-root)
           (%i338-build-saved-session tmp-root "resume-slash")
           (multiple-value-bind (handled result)
               (amoebum:dispatch-slash-command
                "/session resume resume-slash"
                :config (amoebum:current-config)
                :chat-state chat-state)
             (is-true handled)
             (is-true (search "Resumed session resume-slash"
                              (or (amoebum:slash-command-result-output result) "")
                              :test #'char-equal)))
           (let* ((conversation (amoebum:chat-ui-state-conversation chat-state))
                  (messages (amoebum:chat-ui-state-messages chat-state))
                  (third (third messages)))
             (is (string= "resume-slash"
                          (amoebum:conversation-state-session-id conversation)))
             (is (eq :tool-executing
                     (amoebum:conversation-state-state conversation)))
             (is (= 3 (length messages)))
             (is (string= "tool" (pseudopod:message-role third)))
             (is (string= "shell" (or (pseudopod:message-name third) "")))
             (is-true
              (plusp (length (or (pseudopod:message-tool-call-id third) "")))))
           (let ((status-state (amoebum:chat-ui-state-status-bar-state chat-state)))
             (is (plusp (amoebum:chat-ui-state-context-used-tokens chat-state)))
             (is (= (amoebum:chat-ui-state-context-used-tokens chat-state)
                    (amoebum:status-bar-state-context-used-tokens status-state))
                 "Context usage should sync to status bar on resume restore."))
           (multiple-value-bind (handled result)
               (amoebum:dispatch-slash-command
                "/session list"
                :config (amoebum:current-config)
                :chat-state chat-state)
             (is-true handled)
             (is-true (search "resume-slash"
                              (or (amoebum:slash-command-result-output result) "")
                              :test #'char-equal))))
      (amoebum:setconfig :project-root old-project-root)
      (%delete-directory-tree-safe tmp-root))))

(test session-checkpoint-restore-updates-context-usage
  (let* ((tmp-root (%make-temp-directory "amoebum-i338-checkpoint"))
         (old-project-root (amoebum:config-project-root (amoebum:current-config)))
         (chat-state (amoebum:make-chat-ui-state :stream-runner nil)))
    (unwind-protect
         (progn
           (amoebum:setconfig :project-root tmp-root)
           (let* ((conversation (%i338-build-saved-session tmp-root "checkpoint-resume"))
                  (checkpoint (amoebum:checkpoint-session :conversation conversation
                                                           :project-root tmp-root
                                                           :trigger :manual))
                  (checkpoint-id (amoebum:session-checkpoint-id checkpoint))
                  (command (format nil "/checkpoint restore ~A" checkpoint-id)))
             (multiple-value-bind (handled result)
                 (amoebum:dispatch-slash-command
                  command
                  :config (amoebum:current-config)
                  :chat-state chat-state)
               (is-true handled)
               (is-true (search (format nil "Restored checkpoint ~A" checkpoint-id)
                                (or (amoebum:slash-command-result-output result) "")
                                :test #'char-equal)))
             (let* ((conversation (amoebum:chat-ui-state-conversation chat-state))
                    (messages (amoebum:chat-ui-state-messages chat-state))
                    (status-state (amoebum:chat-ui-state-status-bar-state chat-state)))
               (is (string= "checkpoint-resume"
                            (amoebum:conversation-state-session-id conversation)))
               (is (= 3 (length messages)))
               (is-true (plusp (amoebum:chat-ui-state-context-used-tokens chat-state)))
               (is (= (amoebum:chat-ui-state-context-used-tokens chat-state)
                      (amoebum:status-bar-state-context-used-tokens status-state))
                   "Context usage should sync to status bar on checkpoint restore."))))
      (amoebum:setconfig :project-root old-project-root)
      (%delete-directory-tree-safe tmp-root))))

(test session-resume-smoke-sentinel
  (is-true t)
  (format t "SESSION_RESUME_SMOKE_OK~%"))
