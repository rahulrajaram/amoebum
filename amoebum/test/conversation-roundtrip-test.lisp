(in-package :amoebum/test)

(defun %message-first-text (message)
  (let* ((parts (pseudopod:message-content message))
         (part (and (consp parts) (first parts))))
    (or (and part (pseudopod:content-part-text part)) "")))

(test conversation-roundtrip-preserves-tool-call-ids
  "Conversation save/load round-trip preserves message sequence and tool call ids."
  (let* ((tmp-root (%make-temp-directory "amoebum-i141-conversation"))
         (conversation (amoebum.sessions:make-conversation-state :project-root tmp-root))
         (assistant-tool-call-id "tool-call-1")
         (user-message (pseudopod:make-message
                        :role "user"
                        :content "roundtrip user"))
         (assistant-message (pseudopod:make-message
                             :role "assistant"
                             :content "tool-call preview"
                             :tool-call-id assistant-tool-call-id
                             :tool-calls (list (pseudopod:make-tool-call
                                                :id assistant-tool-call-id
                                                :name "mock_tool"
                                                :arguments "{}"))))
         (tool-message (pseudopod:make-message
                        :role "tool"
                        :name "mock_tool"
                        :content "tool output"
                        :tool-call-id assistant-tool-call-id)))
    (unwind-protect
        (progn
          (amoebum.sessions:conversation-state-add-message conversation user-message :save-p nil)
          (amoebum.sessions:conversation-state-add-message conversation assistant-message :save-p nil)
          (amoebum.sessions:conversation-state-add-message conversation tool-message :save-p nil)
          (let ((saved-path (amoebum.sessions:conversation-save conversation))
                (loaded (amoebum.sessions:conversation-load (amoebum.sessions:conversation-state-session-path conversation)
                                                   :project-root tmp-root)))
            (is-true (and saved-path (probe-file saved-path))
                     "Expected conversation-save to persist manifest file.")
            (is-true loaded "Expected conversation-load to return restored conversation.")
            (let ((entries (amoebum.sessions:conversation-state-entries loaded))
                  (messages (amoebum.sessions:conversation-state-messages loaded)))
              (is (= 3 (length entries)))
              (is (= 3 (length messages)))
              (is (string= "user" (amoebum.sessions:conversation-history-entry-role (first entries))))
              (is (null (amoebum.sessions:conversation-history-entry-tool-call-id (first entries))))
              (is (string= "assistant" (amoebum.sessions:conversation-history-entry-role (second entries))))
              (is (string= assistant-tool-call-id
                           (amoebum.sessions:conversation-history-entry-tool-call-id (second entries)))
                  "Assistant tool-call-id should survive round-trip.")
              (is (string= "tool" (amoebum.sessions:conversation-history-entry-role (third entries))))
              (is (string= assistant-tool-call-id
                           (amoebum.sessions:conversation-history-entry-tool-call-id (third entries)))
                  "Tool message tool-call-id should survive round-trip.")
              (is (string= assistant-tool-call-id
                           (pseudopod:message-tool-call-id (second messages)))
                  "Message round-trip should preserve assistant tool-call-id.")
              (let* ((assistant-tool-calls (pseudopod:message-tool-calls (second messages)))
                     (assistant-tool-call (and assistant-tool-calls
                                               (first assistant-tool-calls))))
                (is (= 1 (length assistant-tool-calls))
                    "Assistant message should keep its tool-calls list.")
                (is (string= assistant-tool-call-id
                             (or (pseudopod:tool-call-id assistant-tool-call) ""))
                    "Round-trip should preserve assistant tool-call id inside tool-calls.")
                (is (string= "mock_tool"
                             (or (pseudopod:tool-call-name assistant-tool-call) ""))
                    "Round-trip should preserve assistant tool-call name.")
                (is (string= "{}"
                             (or (pseudopod:tool-call-arguments assistant-tool-call) ""))
                    "Round-trip should preserve assistant tool-call arguments."))
              (is (string= assistant-tool-call-id
                           (pseudopod:message-tool-call-id (third messages)))
                  "Message round-trip should preserve tool tool-call-id.")
              (is (string= "tool output"
                           (%message-first-text (third messages))))
              (is (string= "roundtrip user"
                           (%message-first-text (first messages)))))))
      (%delete-directory-tree-safe tmp-root))))
