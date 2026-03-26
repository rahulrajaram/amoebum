(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Scroll & Navigation Appearance Tests
;;; ---------------------------------------------------------------------------

(def-suite scroll-appearance-suite :in amoebum-suite
  :description "Snapshot tests for scrolling, pagination, and navigation indicators.")

(in-suite scroll-appearance-suite)

(defun %scroll-snapshot-path (name)
  (merge-pathnames (pathname (format nil "~A.snap" name))
                   +chat-snapshot-dir*))

(defun %assert-scroll-snapshot (buffer name)
  (multiple-value-bind (pass diff)
      (ptui.test-support.harness:assert-snapshot
       buffer
       (%scroll-snapshot-path name)
       :test-name (string-capitalize (princ-to-string name)))
    (is (eq t pass))
    (is (null diff))))

(defun %scroll-test-chat-state (&key messages (input-text ""))
  "Build a chat-ui-state for scroll appearance tests."
  (with-safe-chat-env
    (let ((state (%safe-make-chat-ui-state :branch-name "test/scroll")))
      (dolist (msg messages)
        (amoebum:chat-ui-add-message state (first msg) (second msg)))
      (when (plusp (length input-text))
        (amoebum:chat-ui-set-input state input-text))
      state)))

(defun %make-long-conversation ()
  "Generate a list of messages long enough to require scrolling."
  (loop for i from 1 to 15
        collect (list "user" (format nil "User message number ~D about topic ~D." i i))
        collect (list "assistant" (format nil "Assistant response to query ~D. This is a detailed explanation." i))))

;;; --- Message History Scroll Tests ---

(test scroll-history-at-bottom
  "Message history at bottom (most recent messages visible, no scroll offset)."
  (let* ((messages (%make-long-conversation))
         (state (%scroll-test-chat-state :messages messages))
         (buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
    (%assert-scroll-snapshot buffer "scroll-history-bottom")))

(test scroll-history-scrolled-up
  "Message history scrolled up by 5 lines (older messages visible)."
  (with-safe-chat-env
    (let* ((messages (%make-long-conversation))
           (state (%scroll-test-chat-state :messages messages)))
      ;; First render to establish max-scrollback
      (%safe-render-chat-ui state :cols 84 :rows 20)
      ;; Now scroll up
      (amoebum:chat-ui-scroll-history state 5)
      (let ((buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
        (%assert-scroll-snapshot buffer "scroll-history-scrolled-up")))))

(test scroll-history-at-top
  "Message history scrolled all the way to top (oldest messages visible)."
  (with-safe-chat-env
    (let* ((messages (%make-long-conversation))
           (state (%scroll-test-chat-state :messages messages)))
      ;; First render to establish max-scrollback
      (%safe-render-chat-ui state :cols 84 :rows 20)
      ;; Scroll up a large amount (will be clamped to max)
      (amoebum:chat-ui-scroll-history state 500)
      (let ((buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
        (%assert-scroll-snapshot buffer "scroll-history-at-top")))))

;;; --- Prompt Box Scroll Tests ---

(test scroll-prompt-box-long-input
  "Prompt box with long multi-line input that exceeds max-rows."
  (let* ((text (format nil "~{~A~^~%~}"
                       (loop for i from 1 to 8
                             collect (format nil "Input line ~D: editing a long prompt" i))))
         (state (%scroll-test-chat-state :input-text text))
         (buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
    (%assert-scroll-snapshot buffer "scroll-prompt-long-input")))

(test scroll-prompt-box-with-messages
  "Prompt box and message history competing for vertical space."
  (let* ((messages '(("user" "First question about the codebase structure.")
                     ("assistant" "The codebase has 4 ASDF systems: pseudopod, amoebum, sw4rm-sdk, and ptui.")
                     ("user" "Can you show me the main entry point?")))
         (text (format nil "Line 1~%Line 2~%Line 3~%Line 4~%Line 5"))
         (state (%scroll-test-chat-state :messages messages :input-text text))
         (buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
    (%assert-scroll-snapshot buffer "scroll-prompt-with-messages")))

;;; --- Plan Mode Navigation Test ---

(test scroll-plan-mode-presentation
  "Plan mode with steps added, rendered in the chat UI."
  (let* ((saved-plan-state amoebum::*plan-mode-state*)
         (result nil))
    (unwind-protect
        (progn
          ;; Activate plan mode and add steps
          (amoebum.plan:enter-plan-mode :clear-steps-p t)
          (amoebum.plan:add-plan-step "Read the config file"
                                      :file-paths '("amoebum/src/config.lisp"))
          (amoebum.plan:add-plan-step "Modify the parser"
                                      :file-paths '("amoebum/src/pipeline.lisp"))
          (amoebum.plan:add-plan-step "Write unit tests"
                                      :file-paths '("amoebum/test/config-test.lisp"))
          (let* ((state (%scroll-test-chat-state
                         :messages '(("system" "PLAN MODE active. Steps below:"))))
                 (buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
            (setf result buffer)))
      ;; Restore plan mode state
      (setf amoebum::*plan-mode-state* saved-plan-state))
    (when result
      (%assert-scroll-snapshot result "scroll-plan-mode-steps"))))

;;; --- Scroll Step-by-Step Diagnostic Tests ---

(test scroll-up-from-bottom-offset-changes-each-step
  "Each scroll-up-by-1 from bottom must change the rendered offset."
  (with-safe-chat-env
    (let* ((messages (%make-long-conversation))
           (state (%scroll-test-chat-state :messages messages)))
      ;; Initial render to establish max-scrollback
      (%safe-render-chat-ui state :cols 84 :rows 20)
      (let ((initial-max (amoebum::chat-ui-state-max-message-scrollback-lines
                           (amoebum::ensure-chat-ui-state state))))
        ;; Verify we can scroll (content exceeds viewport)
        (is (> initial-max 0)
            "Max scrollback should be > 0 for long conversation")
        ;; Scroll up 1 step at a time and verify offset changes each time
        (let ((prev-scrollback 0)
              (failures '()))
          (loop for step from 1 to (min 15 initial-max) do
            (amoebum:chat-ui-scroll-history state 1)
            ;; Re-render to update max-scrollback and clamp scrollback
            (%safe-render-chat-ui state :cols 84 :rows 20)
            (let ((cur-scrollback
                    (amoebum::chat-ui-state-message-scrollback-lines
                      (amoebum::ensure-chat-ui-state state))))
              (when (= cur-scrollback prev-scrollback)
                (push step failures))
              (setf prev-scrollback cur-scrollback)))
          (is (null failures)
              "Scrollback should increase each step, stuck at steps: ~A" failures))))))

(test scroll-up-from-bottom-with-markdown-content
  "Scroll up from bottom with markdown-style content (trailing newlines)."
  (with-safe-chat-env
    (let* ((messages
             (list (list "user" "Hello, can you help?")
                   (list "assistant"
                         (format nil "Sure! Here's a code example:~%~%```python~%def foo():~%    pass~%```~%~%And some more text.~%~%"))
                   (list "user" "Thanks!")
                   (list "assistant"
                         (format nil "You're welcome!~%~%Here's another block:~%~%```~%line 1~%line 2~%line 3~%```~%~%"))
                   (list "user" "One more question")
                   (list "assistant"
                         (format nil "Final answer with trailing newlines.~%~%~%~%~%"))))
           (state (%scroll-test-chat-state :messages messages)))
      ;; Render to establish state
      (%safe-render-chat-ui state :cols 84 :rows 20)
      (let* ((chat (amoebum::ensure-chat-ui-state state))
             (max-sb (amoebum::chat-ui-state-max-message-scrollback-lines chat)))
        ;; If max scrollback is 0, content fits viewport — test still passes
        (when (> max-sb 0)
          ;; Scroll up 1 step at a time
          (let ((prev 0)
                (failures '()))
            (loop for step from 1 to (min 10 max-sb) do
              (amoebum:chat-ui-scroll-history state 1)
              (%safe-render-chat-ui state :cols 84 :rows 20)
              (let ((cur (amoebum::chat-ui-state-message-scrollback-lines chat)))
                (when (= cur prev)
                  (push step failures))
                (setf prev cur)))
            (is (null failures)
                "Scrollback should increase with markdown content, stuck at: ~A"
                failures)))))))

(test scroll-debug-flag-reads-runtime-env
  "Scroll debug logging must consult the live process environment, not a dumped image value."
  (let ((original (uiop:getenv "AMOEBUM_SCROLL_DEBUG")))
    (unwind-protect
        (progn
          (setf (uiop:getenv "AMOEBUM_SCROLL_DEBUG") "")
          (is-false (amoebum::%scroll-debug-enabled-p))
          (setf (uiop:getenv "AMOEBUM_SCROLL_DEBUG") "1")
          (is-true (amoebum::%scroll-debug-enabled-p))
          (setf (uiop:getenv "AMOEBUM_SCROLL_DEBUG") "0")
          (is-false (amoebum::%scroll-debug-enabled-p)))
      (setf (uiop:getenv "AMOEBUM_SCROLL_DEBUG") (or original "")))))
