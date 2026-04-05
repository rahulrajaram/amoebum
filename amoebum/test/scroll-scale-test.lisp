(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Scroll Scale / Stress Tests  (NXT-233)
;;; ---------------------------------------------------------------------------
;;; These tests exercise the virtual-scroll and line-entry machinery under
;;; large conversation loads to guard against O(n) widget allocation regressions.
;;; ---------------------------------------------------------------------------

(def-suite scroll-scale-suite :in amoebum-suite
  :description "Scale and stress tests for virtual scroll and large conversation handling.")

(in-suite scroll-scale-suite)

;;; --- helpers ---

(defun %make-scale-conversation (&key (pairs 100) (assistant-length 500))
  "Generate PAIRS user+assistant message pairs.
Each assistant message is ASSISTANT-LENGTH characters."
  (let ((assistant-body
          (let ((s (make-string assistant-length :initial-element #\a)))
            ;; Embed a few newlines so the content spans multiple lines.
            (setf (char s (floor assistant-length 4)) #\Newline)
            (setf (char s (floor assistant-length 2)) #\Newline)
            (setf (char s (floor (* 3 assistant-length) 4)) #\Newline)
            s)))
    (loop for i from 1 to pairs
          collect (list "user"
                        (format nil "User message number ~D about topic ~D." i i))
          collect (list "assistant"
                        (format nil "~A (response ~D)" assistant-body i)))))

(defun %scale-test-chat-state (&key messages)
  "Build a chat-ui-state populated with MESSAGES, inside the safe chat env."
  (with-safe-chat-env
    (let ((state (%safe-make-chat-ui-state :branch-name "test/scale")))
      (dolist (msg messages)
        (amoebum:chat-ui-add-message state (first msg) (second msg)))
      state)))

;;; --- Tests ---

(test scroll-scale-large-conversation-line-count
  "A 200-message conversation should produce substantially more lines than messages."
  (with-safe-chat-env
    (let* ((messages (%make-scale-conversation :pairs 100))
           (state (%scale-test-chat-state :messages messages))
           (chat-state (amoebum::ensure-chat-ui-state state))
           (width 84)
           (lines (amoebum::%message-line-entries
                   chat-state
                   (amoebum::chat-ui-state-messages chat-state)
                   width))
           (message-count (length (amoebum::chat-ui-state-messages chat-state))))
      ;; There should be at least as many display lines as messages.
      (is (> (length lines) message-count)
          "Expected display lines (~D) > message count (~D)"
          (length lines) message-count)
      ;; With 100 user+assistant pairs and multi-line assistant messages the
      ;; total should be well above 300.
      (is (> (length lines) 300)
          "Expected > 300 display lines for 200 messages, got ~D"
          (length lines)))))

(test scroll-scale-virtual-window-bounded
  "The visible line window must be bounded by viewport-height regardless of total lines."
  (with-safe-chat-env
    (let* ((messages (%make-scale-conversation :pairs 100))
           (state (%scale-test-chat-state :messages messages))
           (chat-state (amoebum::ensure-chat-ui-state state))
           (width 84)
           (viewport-height 20))
      (multiple-value-bind (visible-lines total-lines offset new-scrollback max-scrollback)
          (amoebum::%message-line-window
           chat-state
           (amoebum::chat-ui-state-messages chat-state)
           width
           viewport-height
           0)
        (declare (ignore total-lines offset new-scrollback max-scrollback))
        (is (<= (length visible-lines) viewport-height)
            "Visible lines ~D exceeded viewport-height ~D — virtual scroll not working"
            (length visible-lines) viewport-height)))))

(test scroll-scale-scroll-to-extremes
  "Scrolling far past the top and bottom should clamp without crashing."
  (with-safe-chat-env
    (let* ((messages (%make-scale-conversation :pairs 100))
           (state (%scale-test-chat-state :messages messages)))
      ;; Initial render establishes max-scrollback
      (%safe-render-chat-ui state :cols 84 :rows 20)
      ;; Scroll way past the top
      (amoebum:chat-ui-scroll-history state 10000)
      (%safe-render-chat-ui state :cols 84 :rows 20)
      (let* ((chat (amoebum::ensure-chat-ui-state state))
             (sb-after-up (amoebum::chat-ui-state-message-scrollback-lines chat))
             (max-sb (amoebum::chat-ui-state-max-message-scrollback-lines chat)))
        (is (<= sb-after-up max-sb)
            "Scrollback ~D exceeded max ~D after scrolling up by 10000"
            sb-after-up max-sb)
        ;; Scroll way past the bottom
        (amoebum:chat-ui-scroll-history state -10000)
        (%safe-render-chat-ui state :cols 84 :rows 20)
        (let ((sb-after-down (amoebum::chat-ui-state-message-scrollback-lines chat)))
          (is (= sb-after-down 0)
              "Scrollback should be 0 after scrolling down by 10000, got ~D"
              sb-after-down))))))

(test scroll-scale-render-time-bounded
  "Render time for 200-message conversation should not be >5x slower than 50-message."
  (with-safe-chat-env
    (flet ((time-render (pairs)
             (let* ((messages (%make-scale-conversation :pairs pairs))
                    (state (%scale-test-chat-state :messages messages))
                    (t0 (get-internal-real-time)))
               ;; Warm-up render
               (%safe-render-chat-ui state :cols 84 :rows 20)
               ;; Timed render
               (let ((t1 (get-internal-real-time)))
                 (%safe-render-chat-ui state :cols 84 :rows 20)
                 (let ((t2 (get-internal-real-time)))
                   (declare (ignore t0 t1))
                   (- t2 t1))))))
      (let* ((time-small (time-render 50))
             (time-large (time-render 200))
             ;; Avoid divide-by-zero: treat 0 as 1 tick
             (ratio (if (zerop time-small)
                        1.0
                        (float (/ time-large (max 1 time-small))))))
        (is (< ratio 5.0)
            "Render time ratio large/small = ~,2F (expected < 5.0) — possible O(n) regression"
            ratio)))))

(test scroll-scale-line-entry-cache-consistency
  "Two calls to %message-line-entries for the same messages/width must return the same count."
  (with-safe-chat-env
    (let* ((messages (%make-scale-conversation :pairs 100))
           (state (%scale-test-chat-state :messages messages))
           (chat-state (amoebum::ensure-chat-ui-state state))
           (width 84))
      (let* ((lines-1 (amoebum::%message-line-entries
                       chat-state
                       (amoebum::chat-ui-state-messages chat-state)
                       width))
             (lines-2 (amoebum::%message-line-entries
                       chat-state
                       (amoebum::chat-ui-state-messages chat-state)
                       width)))
        (is (= (length lines-1) (length lines-2))
            "Line count changed between calls: ~D vs ~D (cache inconsistency)"
            (length lines-1) (length lines-2))))))

(test scroll-scale-rapid-scroll-stress
  "50 scroll-up + 50 scroll-down steps must complete without errors and end at 0."
  (with-safe-chat-env
    (let* ((messages (%make-scale-conversation :pairs 100))
           (state (%scale-test-chat-state :messages messages)))
      (%safe-render-chat-ui state :cols 84 :rows 20)
      ;; Scroll up 50 steps
      (loop repeat 50 do
        (amoebum:chat-ui-scroll-history state 1)
        (%safe-render-chat-ui state :cols 84 :rows 20))
      ;; Scroll back down 50 steps
      (loop repeat 50 do
        (amoebum:chat-ui-scroll-history state -1)
        (%safe-render-chat-ui state :cols 84 :rows 20))
      ;; Final state: scrollback should be 0 (we scrolled down at least as
      ;; many times as up, and we may not have reached the top in 50 steps)
      (let* ((chat (amoebum::ensure-chat-ui-state state))
             (final-sb (amoebum::chat-ui-state-message-scrollback-lines chat)))
        (is (= final-sb 0)
            "Final scrollback after equal up/down scrolls should be 0, got ~D"
            final-sb)))))

(test scroll-scale-wide-terminal
  "Rendering a large conversation at 200x50 should complete without error."
  (with-safe-chat-env
    (let* ((messages (%make-scale-conversation :pairs 100))
           (state (%scale-test-chat-state :messages messages))
           (buffer (%safe-render-chat-ui state :cols 200 :rows 50)))
      (is (not (null buffer))
          "Wide terminal render returned NIL"))))

(test scroll-scale-narrow-vs-wide-line-count
  "Narrow terminal should produce more display lines than wide terminal (text reflows)."
  (with-safe-chat-env
    (let* ((messages (%make-scale-conversation :pairs 50))
           (state-wide (%scale-test-chat-state :messages messages))
           (state-narrow (%scale-test-chat-state :messages messages))
           (chat-wide (amoebum::ensure-chat-ui-state state-wide))
           (chat-narrow (amoebum::ensure-chat-ui-state state-narrow))
           (msgs-wide (amoebum::chat-ui-state-messages chat-wide))
           (msgs-narrow (amoebum::chat-ui-state-messages chat-narrow))
           (lines-wide (amoebum::%message-line-entries chat-wide msgs-wide 200))
           (lines-narrow (amoebum::%message-line-entries chat-narrow msgs-narrow 40)))
      (is (>= (length lines-narrow) (length lines-wide))
          "Narrow terminal (~D lines) should have >= lines than wide (~D lines)"
          (length lines-narrow) (length lines-wide)))))
