(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Visual Styling & Color Appearance Tests
;;; ---------------------------------------------------------------------------

(def-suite style-appearance-suite :in amoebum-suite
  :description "Snapshot tests for markdown rendering, ANSI colors, and role-based styling.")

(in-suite style-appearance-suite)

(defun %style-snapshot-path (name)
  (merge-pathnames (pathname (format nil "~A.snap" name))
                   +chat-snapshot-dir*))

(defun %assert-style-snapshot (buffer name)
  (multiple-value-bind (pass diff)
      (ptui.test-support.harness:assert-snapshot
       buffer
       (%style-snapshot-path name)
       :test-name (string-capitalize (princ-to-string name)))
    (is (eq t pass))
    (is (null diff))))

(defun %style-test-chat-state (&key messages)
  "Build a chat-ui-state for style appearance tests."
  (with-safe-chat-env
    (let ((state (%safe-make-chat-ui-state :branch-name "test/style")))
      (dolist (msg messages)
        (amoebum:chat-ui-add-message state (first msg) (second msg)))
      state)))

;;; --- Markdown Rendering Tests ---

(test style-markdown-code-block
  "Assistant message with a fenced code block."
  (let* ((content (format nil "Here is some code:~%~%```lisp~%(defun hello ()~%  (format t \"Hello, world!~%\"))~%```~%~%That defines a greeting function."))
         (state (%style-test-chat-state
                 :messages (list (list "user" "Show me a hello world function.")
                                 (list "assistant" content))))
         (buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
    (%assert-style-snapshot buffer "style-markdown-code-block")))

(test style-markdown-inline-formatting
  "Assistant message with bold, italic, and inline code."
  (let* ((content "Use **bold** for emphasis, *italic* for nuance, and `inline-code` for identifiers.")
         (state (%style-test-chat-state
                 :messages (list (list "user" "How do I format text?")
                                 (list "assistant" content))))
         (buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
    (%assert-style-snapshot buffer "style-markdown-inline")))

(test style-markdown-list
  "Assistant message with bullet and numbered lists."
  (let* ((content (format nil "Key points:~%~%- First bullet item~%- Second bullet item~%- Third bullet item~%~%Steps:~%~%1. Read the file~%2. Parse the contents~%3. Write the output"))
         (state (%style-test-chat-state
                 :messages (list (list "user" "Summarize the steps.")
                                 (list "assistant" content))))
         (buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
    (%assert-style-snapshot buffer "style-markdown-list")))

(test style-markdown-headings
  "Assistant message with heading levels."
  (let* ((content (format nil "# Main Heading~%~%Overview of the system.~%~%## Sub Heading~%~%Details about the architecture.~%~%### Third Level~%~%Specifics about implementation."))
         (state (%style-test-chat-state
                 :messages (list (list "user" "Give me a structured overview.")
                                 (list "assistant" content))))
         (buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
    (%assert-style-snapshot buffer "style-markdown-headings")))

;;; --- Role-Based Color Tests ---

(test style-message-roles
  "Messages with different roles showing distinct visual treatment."
  (let* ((state (%style-test-chat-state
                 :messages (list (list "system" "System: You are a helpful assistant.")
                                 (list "user" "What is the project structure?")
                                 (list "assistant" "The project has 4 ASDF systems.")
                                 (list "tool" "glob-files returned: main.lisp, config.lisp, chat.lisp")))))
    (let ((buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
      (%assert-style-snapshot buffer "style-message-roles"))))

(test style-status-bar-token-usage-low
  "Status bar with low token usage (green zone)."
  (let* ((state (%safe-make-chat-ui-state :branch-name "main"))
         (status-state (amoebum::chat-ui-state-status-bar-state state)))
    (setf (amoebum::status-bar-state-model-name status-state) "gpt-4o"
          (amoebum::status-bar-state-context-used-tokens status-state) 5000
          (amoebum::status-bar-state-context-max-tokens status-state) 128000)
    (let ((buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
      (%assert-style-snapshot buffer "style-status-bar-low-usage"))))

(test style-status-bar-token-usage-high
  "Status bar with high token usage (red zone)."
  (let* ((state (%safe-make-chat-ui-state :branch-name "main"))
         (status-state (amoebum::chat-ui-state-status-bar-state state)))
    (setf (amoebum::status-bar-state-model-name status-state) "gpt-4o"
          (amoebum::status-bar-state-context-used-tokens status-state) 115000
          (amoebum::status-bar-state-context-max-tokens status-state) 128000)
    (let ((buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
      (%assert-style-snapshot buffer "style-status-bar-high-usage"))))

(test style-status-bar-streaming
  "Status bar showing active streaming state with token rate."
  (let* ((status-state (%snapshot-status-bar-state
                        :branch-name "feat/streaming"
                        :model-name "gpt-4o"
                        :stream-summary (list :status :running
                                             :tokens 256
                                             :tokens-per-second 12.5d0
                                             :activep t)))
         (state (%safe-make-chat-ui-state :branch-name "feat/streaming")))
    (setf (amoebum::chat-ui-state-status-bar-state state) status-state)
    (let ((buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
      (%assert-style-snapshot buffer "style-status-bar-streaming"))))
