(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Input & Prompt Box Appearance Tests
;;; ---------------------------------------------------------------------------

(def-suite input-appearance-suite :in amoebum-suite
  :description "Snapshot tests for prompt box rendering and input editing operations.")

(in-suite input-appearance-suite)

(defun %input-snapshot-path (name)
  (merge-pathnames (pathname (format nil "~A.snap" name))
                   +chat-snapshot-dir*))

(defun %assert-input-snapshot (buffer name)
  (multiple-value-bind (pass diff)
      (ptui.test-support.harness:assert-snapshot
       buffer
       (%input-snapshot-path name)
       :test-name (string-capitalize (princ-to-string name)))
    (is (eq t pass))
    (is (null diff))))

(defmacro with-safe-chat-env (&body body)
  "Execute BODY with *default-pathname-defaults* and *current-config* safely bound.
Protects against stale project root from prior serialization tests."
  `(let ((*default-pathname-defaults*
           (pathname "/home/rahul/Documents/amoebum/"))
         (amoebum::*current-config* nil))
     ,@body))

(defun %safe-make-chat-ui-state (&key (branch-name "test"))
  "Build a chat-ui-state, working around stale project root errors from prior tests."
  (with-safe-chat-env
    (let ((state (amoebum.ui:make-chat-ui-state
                  :status-bar-state (%snapshot-status-bar-state
                                     :branch-name branch-name))))
      ;; Replace tree browser with empty (deterministic, no filesystem access)
      (setf (amoebum::chat-ui-state-tree-browser-state state)
            (amoebum::make-empty-tree-browser-state :label "files"))
      ;; Deactivate tree browser so it doesn't render
      (setf (amoebum::tree-browser-state-active-p
             (amoebum::chat-ui-state-tree-browser-state state))
            nil)
      state)))

(defun %safe-render-chat-ui (state &key (cols 84) (rows 20))
  "Render chat UI with proper path defaults, safe after serialization tests."
  (with-safe-chat-env
    (%render-chat-ui state :cols cols :rows rows)))

(defun %input-test-chat-state (&key (input-text "") messages)
  "Build a chat-ui-state with minimal configuration for input appearance tests."
  (with-safe-chat-env
    (let ((state (%safe-make-chat-ui-state :branch-name "test/input")))
      (dolist (msg messages)
        (amoebum:chat-ui-add-message state (first msg) (second msg)))
      (when (plusp (length input-text))
        (amoebum:chat-ui-set-input state input-text))
      state)))

;;; --- Tests ---

(test input-prompt-box-empty
  "Prompt box with no input text (single line, rounded border)."
  (let* ((state (%input-test-chat-state))
         (buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
    (%assert-input-snapshot buffer "prompt-box-empty")))

(test input-prompt-box-single-line
  "Prompt box with a single line of text."
  (let* ((state (%input-test-chat-state :input-text "Hello, world!"))
         (buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
    (%assert-input-snapshot buffer "prompt-box-single-line")))

(test input-prompt-box-multiline
  "Prompt box with 3 lines of text (within max-rows limit)."
  (let* ((text (format nil "Line one~%Line two~%Line three"))
         (state (%input-test-chat-state :input-text text))
         (buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
    (%assert-input-snapshot buffer "prompt-box-multiline")))

(test input-prompt-box-overflow
  "Prompt box with more lines than max-rows (tests scroll/overflow behavior)."
  (let* ((text (format nil "Line 1~%Line 2~%Line 3~%Line 4~%Line 5~%Line 6~%Line 7~%Line 8"))
         (state (%input-test-chat-state :input-text text))
         (buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
    (%assert-input-snapshot buffer "prompt-box-overflow")))

(test input-after-ctrl-w
  "Text state after Ctrl+W (delete word backward): 'hello world foo' -> 'hello world '."
  (let* ((original "hello world foo")
         (after-ctrl-w (amoebum::%delete-word-backward original))
         (state (%input-test-chat-state :input-text after-ctrl-w))
         (buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
    ;; Also verify the function itself
    (is (string= "hello world " after-ctrl-w))
    (%assert-input-snapshot buffer "prompt-box-after-ctrl-w")))

(test input-after-ctrl-u
  "Text state after Ctrl+U (clear line): empty prompt box."
  (let* ((state (%input-test-chat-state :input-text ""))
         (buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
    (%assert-input-snapshot buffer "prompt-box-after-ctrl-u")))

(test chat-panel-input-handler-dispatches-ctrl-k-and-right
  (let ((state (%input-test-chat-state :input-text "hello world")))
    (amoebum:chat-ui-set-input state "hello world" :cursor-position 5)
    (is-true (amoebum::chat-panel-handle-input-key state :ctrl-k nil 40))
    (is (string= "hello" (amoebum::chat-ui-state-input-text state)))
    (is (= 5 (amoebum::chat-ui-state-cursor-position state)))
    (is-true (amoebum::chat-panel-handle-input-key state :right nil 40))
    (is-false (amoebum::chat-ui-state-cursor-position state))))

;;; --- Unit tests for %delete-word-backward ---

(defun %input-test-chat-state-with-cursor (&key (input-text "") (cursor-position nil) messages)
  "Build a chat-ui-state with cursor position for cursor rendering tests."
  (with-safe-chat-env
    (let ((state (%safe-make-chat-ui-state :branch-name "test/input")))
      (dolist (msg messages)
        (amoebum:chat-ui-add-message state (first msg) (second msg)))
      (when (plusp (length input-text))
        (amoebum:chat-ui-set-input state input-text :cursor-position cursor-position))
      state)))

;;; --- Cursor position snapshot tests ---

(test input-prompt-box-cursor-middle
  "Prompt box with cursor at position 5 in 'Hello world' (between 'Hello' and ' world')."
  (let* ((state (%input-test-chat-state-with-cursor
                 :input-text "Hello world"
                 :cursor-position 5))
         (buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
    (%assert-input-snapshot buffer "prompt-box-cursor-middle")))

(test input-prompt-box-cursor-home
  "Prompt box with cursor at position 0 (beginning of input)."
  (let* ((state (%input-test-chat-state-with-cursor
                 :input-text "Hello world"
                 :cursor-position 0))
         (buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
    (%assert-input-snapshot buffer "prompt-box-cursor-home")))

(test input-prompt-box-cursor-end
  "Prompt box with cursor at nil (end of input) — should show cursor block at end."
  (let* ((state (%input-test-chat-state-with-cursor
                 :input-text "Hello world"
                 :cursor-position nil))
         (buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
    (%assert-input-snapshot buffer "prompt-box-cursor-end")))

;;; --- Cursor-aware editing unit tests ---

(test grapheme-insert-at-middle
  "Insert at cursor position in the middle of text."
  (is (string= "HelXYlo" (amoebum::%grapheme-insert-at "Hello" 3 "XY")))
  (is (string= "XYHello" (amoebum::%grapheme-insert-at "Hello" 0 "XY")))
  (is (string= "HelloXY" (amoebum::%grapheme-insert-at "Hello" 5 "XY"))))

(test grapheme-delete-before
  "Delete grapheme before cursor."
  (multiple-value-bind (text pos) (amoebum::%grapheme-delete-before "Hello" 3)
    (is (string= "Helo" text))
    (is (= 2 pos)))
  (multiple-value-bind (text pos) (amoebum::%grapheme-delete-before "Hello" 0)
    (is (string= "Hello" text))
    (is (= 0 pos))))

(test grapheme-delete-at
  "Delete grapheme at cursor (forward delete)."
  (is (string= "Helo" (amoebum::%grapheme-delete-at "Hello" 2)))
  (is (string= "Hello" (amoebum::%grapheme-delete-at "Hello" 5))))

(test delete-word-backward-at-middle
  "Delete word backward from cursor in the middle."
  (multiple-value-bind (text pos) (amoebum::%delete-word-backward-at "hello world foo" 11)
    ;; Cursor at 11 = after "world", before " foo"
    ;; Deletes "world" leaving "hello " + " foo" = "hello  foo"
    (is (string= "hello  foo" text))
    (is (= 6 pos))))

(test cursor-to-line-col-basic
  "Cursor position maps to correct line and column."
  (let ((lines '("Hello" " world")))
    (multiple-value-bind (line col) (amoebum::%cursor-to-line-col 3 lines)
      (is (= 0 line))
      (is (= 3 col)))
    (multiple-value-bind (line col) (amoebum::%cursor-to-line-col 5 lines)
      ;; Position 5 = end of "Hello" = wraps to start of next line
      (is (= 1 line))
      (is (= 0 col)))
    (multiple-value-bind (line col) (amoebum::%cursor-to-line-col 8 lines)
      (is (= 1 line))
      (is (= 3 col)))))

(test delete-word-backward-basic
  "Delete word backward removes last word."
  (is (string= "hello " (amoebum::%delete-word-backward "hello world")))
  (is (string= "hello world " (amoebum::%delete-word-backward "hello world foo")))
  (is (string= "" (amoebum::%delete-word-backward "hello")))
  (is (string= "" (amoebum::%delete-word-backward "")))
  (is (string= "" (amoebum::%delete-word-backward nil))))

(test delete-word-backward-trailing-whitespace
  "Delete word backward skips trailing whitespace first."
  (is (string= "hello " (amoebum::%delete-word-backward "hello world   ")))
  (is (string= "" (amoebum::%delete-word-backward "   "))))

(test delete-word-backward-path-like
  "Delete word backward on path-like strings (whitespace boundaries only)."
  ;; No whitespace in "src/main.lisp" so entire string is one word
  (is (string= "" (amoebum::%delete-word-backward "src/main.lisp")))
  ;; With whitespace, deletes last whitespace-delimited word
  (is (string= "src/ " (amoebum::%delete-word-backward "src/ main.lisp"))))
