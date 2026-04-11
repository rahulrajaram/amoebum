(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Error States & Notification Appearance Tests
;;; ---------------------------------------------------------------------------

(def-suite error-appearance-suite :in amoebum-suite
  :description "Snapshot tests for error states, warnings, and notification displays.")

(in-suite error-appearance-suite)

(defun %error-snapshot-path (name)
  (merge-pathnames (pathname (format nil "~A.snap" name))
                   +chat-snapshot-dir*))

(defun %assert-error-snapshot (buffer name)
  (multiple-value-bind (pass diff)
      (ptui.test-support.harness:assert-snapshot
       buffer
       (%error-snapshot-path name)
       :test-name (string-capitalize (princ-to-string name)))
    (is (eq t pass))
    (is (null diff))))

(defun %error-test-chat-state (&key messages)
  "Build a chat-ui-state for error appearance tests."
  (with-safe-chat-env
    (let ((state (%safe-make-chat-ui-state :branch-name "test/error")))
      (dolist (msg messages)
        (amoebum:chat-ui-add-message state (first msg) (second msg)))
      state)))

;;; --- Error State Tests ---

(test error-stream-cancelled
  "Chat showing a cancelled stream with meta message."
  (let* ((state (%error-test-chat-state
                 :messages (list (list "user" "Explain the architecture.")
                                 (list "assistant" "The system is composed of multiple layers...")
                                 (list "system" "[Stream cancelled by user]"))))
         (buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
    (%assert-error-snapshot buffer "error-stream-cancelled")))

(test error-tool-permission-denied
  "Chat showing a tool permission denied error."
  (let* ((state (%error-test-chat-state
                 :messages (list (list "user" "Delete that file.")
                                 (list "assistant" "I attempted to run the command but it was denied.")
                                 (list "tool" "Error: Permission denied for tool 'bash-exec'. Command: rm -rf /important"))))
         (buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
    (%assert-error-snapshot buffer "error-tool-permission-denied")))

(test error-tool-execution-failed
  "Chat showing a tool execution failure."
  (let* ((state (%error-test-chat-state
                 :messages (list (list "user" "Search for the function definition.")
                                 (list "tool" "Error: search-files failed: ENOENT - directory '/nonexistent' does not exist")
                                 (list "assistant" "The search failed because the directory does not exist."))))
         (buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
    (%assert-error-snapshot buffer "error-tool-execution-failed")))

(test error-budget-warning-in-conversation
  "Conversation with a budget warning message."
  (let* ((state (%error-test-chat-state
                 :messages (list (list "user" "Continue the analysis.")
                                 (list "system" "Warning: Token budget at 90% (10800/12000). Consider compacting.")
                                 (list "assistant" "I will try to be concise. The remaining analysis shows..."))))
         (buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
    (%assert-error-snapshot buffer "error-budget-warning")))

(test error-stream-provider-overloaded
  "Provider overload errors should render as a clear system message."
  (let* ((state (%error-test-chat-state
                 :messages
                 (list
                  (list "user" "what directory are we in?")
                  (list "system"
                        "[Stream failed]
Provider request failed with HTTP 429.
The engine is currently overloaded, please try again later.
Retry your last message in a moment."))))
         (buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
    (%assert-error-snapshot buffer "error-stream-provider-overloaded")))
