(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Focus & Interaction State Appearance Tests
;;; ---------------------------------------------------------------------------

(def-suite focus-appearance-suite :in amoebum-suite
  :description "Snapshot tests for focus indicators and interaction states.")

(in-suite focus-appearance-suite)

(defun %focus-snapshot-path (name)
  (merge-pathnames (pathname (format nil "~A.snap" name))
                   +chat-snapshot-dir*))

(defun %assert-focus-snapshot (buffer name)
  (multiple-value-bind (pass diff)
      (ptui.test-support.harness:assert-snapshot
       buffer
       (%focus-snapshot-path name)
       :test-name (string-capitalize (princ-to-string name)))
    (is (eq t pass))
    (is (null diff))))

(defun %focus-test-chat-state (&key messages (input-text ""))
  "Build a chat-ui-state for focus appearance tests."
  (with-safe-chat-env
    (let ((state (%safe-make-chat-ui-state :branch-name "test/focus")))
      (dolist (msg messages)
        (amoebum:chat-ui-add-message state (first msg) (second msg)))
      (when (plusp (length input-text))
        (amoebum:chat-ui-set-input state input-text))
      state)))

;;; --- Focus Tests ---

(test focus-prompt-box-focused
  "Prompt box with focus indicator (default focus target)."
  (let* ((state (%focus-test-chat-state
                 :messages '(("user" "Hello")
                             ("assistant" "Hi! How can I help?"))
                 :input-text "typing here")))
    ;; Set focus on prompt box
    (let ((runtime (amoebum::chat-ui-state-runtime state)))
      (setf (ptui.ui.runtime:runtime-focus-id runtime) :chat-input))
    (let ((buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
      (%assert-focus-snapshot buffer "focus-prompt-box"))))

(test focus-approval-dialog-focused
  "Approval dialog overlay with focus on it."
  (let* ((state (%focus-test-chat-state
                 :messages '(("assistant" "I will check the files."))))
         (dialog (amoebum::chat-ui-state-approval-dialog-state state)))
    (amoebum:approval-dialog-activate! dialog "glob-files"
                                       :command "glob-files *.lisp"
                                       :decision-id "focus-test-001")
    (let ((buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
      (%assert-focus-snapshot buffer "focus-approval-dialog"))))

(test focus-conversation-with-multiple-roles
  "Full conversation showing visual distinction between all message roles."
  (let* ((state (%focus-test-chat-state
                 :messages (list (list "system" "System: respond concisely.")
                                 (list "user" "What tools are available?")
                                 (list "assistant" "I have access to file and search tools.")
                                 (list "tool" "glob-files: found 42 matching files")
                                 (list "assistant" "I found 42 files matching your query."))))
         (buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
    (%assert-focus-snapshot buffer "focus-full-conversation")))
