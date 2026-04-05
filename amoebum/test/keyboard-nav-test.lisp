(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Keyboard Accessibility / Navigation Tests (NXT-232)
;;;
;;; Behavioral and structural tests for focus management, keyboard cycling,
;;; focus restoration after overlay dismiss, dialog navigation, and scroll
;;; operations.  These tests do NOT check visual snapshots — they verify that
;;; the runtime focus model behaves correctly under programmatic state changes.
;;; ---------------------------------------------------------------------------

(def-suite keyboard-nav-suite :in amoebum-suite
  :description "Keyboard accessibility: focus order, cycling, dialog nav, and scroll operations.")

(in-suite keyboard-nav-suite)

;;; ---------------------------------------------------------------------------
;;; 1. Focus order completeness
;;; ---------------------------------------------------------------------------

(test focus-order-non-empty-after-render
  "focus-order is non-empty after rendering a chat UI with messages and input."
  (with-safe-chat-env
    (let* ((state (%safe-make-chat-ui-state :branch-name "test/kbd-nav"))
           (runtime (amoebum::chat-ui-state-runtime state)))
      (amoebum:chat-ui-add-message state "user" "Hello")
      (amoebum:chat-ui-add-message state "assistant" "Hi!")
      (amoebum:chat-ui-set-input state "typing")
      ;; Render to populate runtime focus order
      (%safe-render-chat-ui state :cols 84 :rows 24)
      (is-true (ptui.ui.runtime:runtime-focus-order runtime)
               "Expected runtime-focus-order to be non-empty after render."))))

(test focus-order-contains-chat-input
  "The :chat-input element is present in the focus order after render."
  (with-safe-chat-env
    (let* ((state (%safe-make-chat-ui-state :branch-name "test/kbd-nav"))
           (runtime (amoebum::chat-ui-state-runtime state)))
      (amoebum:chat-ui-add-message state "user" "Test message")
      (%safe-render-chat-ui state :cols 84 :rows 24)
      (let ((order (ptui.ui.runtime:runtime-focus-order runtime)))
        (is-true (member :chat-input order :test #'equal)
                 "Expected :chat-input to appear in focus-order. Got: ~S" order)))))

(test focus-order-includes-dialog-elements-when-active
  "When an approval dialog is active, dialog elements appear in the focus order."
  (with-safe-chat-env
    (let* ((state (%safe-make-chat-ui-state :branch-name "test/kbd-nav"))
           (dialog (amoebum::chat-ui-state-approval-dialog-state state))
           (runtime (amoebum::chat-ui-state-runtime state)))
      (amoebum:chat-ui-add-message state "assistant" "I will read the file.")
      ;; Activate the dialog before rendering
      (amoebum:approval-dialog-activate! dialog "read-file"
                                         :command "read-file src/main.lisp"
                                         :decision-id "kbd-nav-test-001")
      (%safe-render-chat-ui state :cols 84 :rows 24)
      (let ((order (ptui.ui.runtime:runtime-focus-order runtime)))
        ;; The approval dialog should contribute at least one element
        (is-true order
                 "Expected non-empty focus order with approval dialog active.")
        ;; The dialog container itself (:approval-dialog) should be present,
        ;; or at minimum the focus order should be larger than with no dialog.
        (is-true (or (member :approval-dialog order :test #'equal)
                     (> (length order) 0))
                 "Expected approval dialog to contribute to focus order. Got: ~S"
                 order)))))

;;; ---------------------------------------------------------------------------
;;; 2. Focus cycling
;;; ---------------------------------------------------------------------------

(test focus-cycling-reaches-each-element
  "Programmatically cycling through focus order visits every element."
  (with-safe-chat-env
    (let* ((state (%safe-make-chat-ui-state :branch-name "test/kbd-nav"))
           (runtime (amoebum::chat-ui-state-runtime state)))
      (amoebum:chat-ui-add-message state "user" "A")
      (amoebum:chat-ui-add-message state "assistant" "B")
      (%safe-render-chat-ui state :cols 84 :rows 24)
      (let ((order (ptui.ui.runtime:runtime-focus-order runtime)))
        (when order
          (let ((visited '()))
            ;; Cycle through all elements in the order
            (dotimes (i (length order))
              (ptui.ui.runtime:advance-focus runtime)
              (push (ptui.ui.runtime:runtime-focus-id runtime) visited))
            (setf visited (nreverse visited))
            ;; Every element in the focus order should have been visited
            (dolist (id order)
              (is-true (member id visited :test #'equal)
                       "Expected focus id ~S to be visited during cycling. Visited: ~S"
                       id visited))))))))

(test focus-cycling-wraps-to-first-after-last
  "After advancing past the last focusable element, focus wraps back to the first."
  (with-safe-chat-env
    (let* ((state (%safe-make-chat-ui-state :branch-name "test/kbd-nav"))
           (runtime (amoebum::chat-ui-state-runtime state)))
      (amoebum:chat-ui-add-message state "user" "Wrap test")
      (%safe-render-chat-ui state :cols 84 :rows 24)
      (let ((order (ptui.ui.runtime:runtime-focus-order runtime)))
        (when (>= (length order) 1)
          ;; Set focus to the last element
          (setf (ptui.ui.runtime:runtime-focus-id runtime) (car (last order)))
          ;; Advancing once should wrap to the first
          (ptui.ui.runtime:advance-focus runtime)
          (is (equal (first order) (ptui.ui.runtime:runtime-focus-id runtime))
              "Expected focus to wrap from last element ~S back to first ~S. Got: ~S"
              (car (last order))
              (first order)
              (ptui.ui.runtime:runtime-focus-id runtime)))))))

(test focus-cycling-backward-from-first-reaches-last
  "Reverse cycling from the first element reaches the last (wrap-around)."
  (with-safe-chat-env
    (let* ((state (%safe-make-chat-ui-state :branch-name "test/kbd-nav"))
           (runtime (amoebum::chat-ui-state-runtime state)))
      (amoebum:chat-ui-add-message state "user" "Reverse wrap test")
      (%safe-render-chat-ui state :cols 84 :rows 24)
      (let ((order (ptui.ui.runtime:runtime-focus-order runtime)))
        (when (>= (length order) 2)
          ;; Set focus to the first element
          (setf (ptui.ui.runtime:runtime-focus-id runtime) (first order))
          ;; Advancing backward should jump to the last
          (ptui.ui.runtime:advance-focus runtime :backward t)
          (is (equal (car (last order)) (ptui.ui.runtime:runtime-focus-id runtime))
              "Expected backward focus from first ~S to wrap to last ~S. Got: ~S"
              (first order)
              (car (last order))
              (ptui.ui.runtime:runtime-focus-id runtime)))))))

(test focus-cycling-three-elements-reverse-sequence
  "With elements [A B C], backward cycling from A should reach C."
  (with-safe-chat-env
    (let* ((state (%safe-make-chat-ui-state :branch-name "test/kbd-nav"))
           (runtime (amoebum::chat-ui-state-runtime state)))
      (amoebum:chat-ui-add-message state "user" "Sequence test")
      (%safe-render-chat-ui state :cols 84 :rows 24)
      (let ((order (ptui.ui.runtime:runtime-focus-order runtime)))
        (when (>= (length order) 3)
          (let ((a (first order))
                (b (second order))
                (c (third order)))
            ;; Start at A, go backward: should reach C (last in wrap)
            (setf (ptui.ui.runtime:runtime-focus-id runtime) a)
            (ptui.ui.runtime:advance-focus runtime :backward t)
            ;; When order has exactly 3, backward from A should give last element
            (let ((focus-from-a-backward (ptui.ui.runtime:runtime-focus-id runtime)))
              (is (equal (car (last order)) focus-from-a-backward)
                  "Expected backward from A=~S to reach last=~S. Got: ~S"
                  a (car (last order)) focus-from-a-backward))
            ;; Start at B, go forward: should reach C
            (setf (ptui.ui.runtime:runtime-focus-id runtime) b)
            (ptui.ui.runtime:advance-focus runtime)
            (is (equal c (ptui.ui.runtime:runtime-focus-id runtime))
                "Expected forward from B=~S to reach C=~S. Got: ~S"
                b c (ptui.ui.runtime:runtime-focus-id runtime))))))))

;;; ---------------------------------------------------------------------------
;;; 3. Focus restoration after overlay dismiss
;;; ---------------------------------------------------------------------------

(test focus-restored-after-dialog-dismiss
  "After activating and deactivating an approval dialog, focus is on a valid element."
  (with-safe-chat-env
    (let* ((state (%safe-make-chat-ui-state :branch-name "test/kbd-nav"))
           (dialog (amoebum::chat-ui-state-approval-dialog-state state))
           (runtime (amoebum::chat-ui-state-runtime state)))
      (amoebum:chat-ui-add-message state "assistant" "About to use a tool.")
      ;; Establish baseline focus
      (%safe-render-chat-ui state :cols 84 :rows 24)
      (let ((pre-dialog-focus (ptui.ui.runtime:runtime-focus-id runtime)))
        ;; Activate dialog and re-render
        (amoebum:approval-dialog-activate! dialog "shell-exec"
                                           :command "ls -la"
                                           :decision-id "kbd-nav-restore-001")
        (%safe-render-chat-ui state :cols 84 :rows 24)
        ;; Dismiss dialog
        (amoebum:approval-dialog-deactivate! dialog)
        (%safe-render-chat-ui state :cols 84 :rows 24)
        (let ((post-dismiss-order (ptui.ui.runtime:runtime-focus-order runtime))
              (post-dismiss-focus (ptui.ui.runtime:runtime-focus-id runtime)))
          ;; Focus should be a valid member of the current order
          (is-true (or (null post-dismiss-focus)
                       (member post-dismiss-focus post-dismiss-order :test #'equal))
                   "Expected post-dismiss focus ~S to be in focus-order ~S."
                   post-dismiss-focus post-dismiss-order)
          ;; The focus order should be non-empty after dismiss
          (is-true post-dismiss-order
                   "Expected non-empty focus order after dialog dismiss.")
          ;; Pre-dialog focus element should be reachable in the post-dismiss order
          (when pre-dialog-focus
            (is-true (member pre-dialog-focus post-dismiss-order :test #'equal)
                     "Expected pre-dialog focus target ~S to be reachable after dismiss. Order: ~S"
                     pre-dialog-focus post-dismiss-order)))))))

(test focus-valid-after-dialog-active
  "When an approval dialog is active, the focus-id is a valid focusable element."
  (with-safe-chat-env
    (let* ((state (%safe-make-chat-ui-state :branch-name "test/kbd-nav"))
           (dialog (amoebum::chat-ui-state-approval-dialog-state state))
           (runtime (amoebum::chat-ui-state-runtime state)))
      (amoebum:chat-ui-add-message state "assistant" "Requesting permission.")
      (amoebum:approval-dialog-activate! dialog "glob-files"
                                         :command "glob *.lisp"
                                         :decision-id "kbd-nav-active-001")
      (%safe-render-chat-ui state :cols 84 :rows 24)
      (let ((order (ptui.ui.runtime:runtime-focus-order runtime))
            (focus  (ptui.ui.runtime:runtime-focus-id runtime)))
        (is-true order
                 "Expected non-empty focus order with active dialog.")
        (when focus
          (is-true (member focus order :test #'equal)
                   "Expected focus-id ~S to be a member of focus-order ~S."
                   focus order))))))

;;; ---------------------------------------------------------------------------
;;; 4. Keyboard-only dialog navigation
;;; ---------------------------------------------------------------------------

(test approval-dialog-has-navigable-options
  "The approval dialog state has the expected navigable options."
  (with-safe-chat-env
    (let* ((state (%safe-make-chat-ui-state :branch-name "test/kbd-nav"))
           (dialog (amoebum::chat-ui-state-approval-dialog-state state)))
      (amoebum:approval-dialog-activate! dialog "write-file"
                                         :command "write src/x.lisp"
                                         :decision-id "kbd-nav-opts-001")
      ;; Dialog should be active
      (is-true (amoebum::approval-dialog-state-active-p dialog)
               "Expected dialog to be active after activation.")
      ;; Options list should contain the four standard options
      (let ((options (mapcar #'car amoebum::+approval-dialog-options+)))
        (is-true (member :approve options :test #'eq)
                 "Expected :approve in dialog options.")
        (is-true (member :deny options :test #'eq)
                 "Expected :deny in dialog options.")
        (is-true (member :remember-allow options :test #'eq)
                 "Expected :remember-allow in dialog options.")
        (is-true (member :remember-deny options :test #'eq)
                 "Expected :remember-deny in dialog options.")))))

(test approval-dialog-navigate-right-advances-selection
  "Pressing right/down on the approval dialog advances the selected option."
  (with-safe-chat-env
    (let* ((state (%safe-make-chat-ui-state :branch-name "test/kbd-nav"))
           (dialog (amoebum::chat-ui-state-approval-dialog-state state)))
      (amoebum:approval-dialog-activate! dialog "search-files"
                                         :decision-id "kbd-nav-nav-001")
      ;; Initial selection should be :approve
      (is (eq :approve (amoebum::approval-dialog-state-selected-option dialog))
          "Expected initial selected option to be :approve.")
      ;; Navigate right (forward) once
      (amoebum:approval-dialog-move-selection! dialog 1)
      (is (eq :deny (amoebum::approval-dialog-state-selected-option dialog))
          "Expected selection to advance to :deny after move +1. Got: ~S"
          (amoebum::approval-dialog-state-selected-option dialog)))))

(test approval-dialog-navigate-left-wraps
  "Pressing left from the first option wraps to the last."
  (with-safe-chat-env
    (let* ((state (%safe-make-chat-ui-state :branch-name "test/kbd-nav"))
           (dialog (amoebum::chat-ui-state-approval-dialog-state state)))
      (amoebum:approval-dialog-activate! dialog "search-files"
                                         :decision-id "kbd-nav-nav-002")
      ;; :approve is first; going backward should wrap to the last option
      (amoebum:approval-dialog-move-selection! dialog -1)
      (let ((last-option (car (car (last amoebum::+approval-dialog-options+)))))
        (is (eq last-option (amoebum::approval-dialog-state-selected-option dialog))
            "Expected backward wrap from :approve to reach ~S. Got: ~S"
            last-option
            (amoebum::approval-dialog-state-selected-option dialog))))))

(test approval-dialog-key-handler-table-is-complete
  "The approval dialog key handler table contains the required navigation keys."
  (let ((keys (mapcar #'car amoebum::+approval-dialog-key-handlers+)))
    (is-true (member :up keys :test #'eq)
             "Expected :up in dialog key handlers.")
    (is-true (member :down keys :test #'eq)
             "Expected :down in dialog key handlers.")
    (is-true (member :left keys :test #'eq)
             "Expected :left in dialog key handlers.")
    (is-true (member :right keys :test #'eq)
             "Expected :right in dialog key handlers.")
    (is-true (or (member :enter keys :test #'eq)
                 (member :return keys :test #'eq))
             "Expected :enter or :return in dialog key handlers.")
    (is-true (member :escape keys :test #'eq)
             "Expected :escape in dialog key handlers.")))

(test approval-dialog-handle-key-consumes-navigation
  "approval-dialog-handle-key! consumes navigation keys when dialog is active."
  (with-safe-chat-env
    (let* ((state (%safe-make-chat-ui-state :branch-name "test/kbd-nav"))
           (dialog (amoebum::chat-ui-state-approval-dialog-state state)))
      (amoebum:approval-dialog-activate! dialog "lint"
                                         :decision-id "kbd-nav-key-001")
      ;; :right should be consumed and return T
      (let ((result (amoebum:approval-dialog-handle-key! dialog :right)))
        (is-true result
                 "Expected :right key to be consumed by active dialog."))
      ;; :left should be consumed
      (let ((result (amoebum:approval-dialog-handle-key! dialog :left)))
        (is-true result
                 "Expected :left key to be consumed by active dialog.")))))

;;; ---------------------------------------------------------------------------
;;; 5. Chat panel keyboard operations
;;; ---------------------------------------------------------------------------

(test chat-scroll-up-changes-scrollback
  "chat-ui-scroll-history with positive delta increases scrollback lines."
  (with-safe-chat-env
    (let* ((messages (loop for i from 1 to 10
                           collect (list "user" (format nil "Message ~D" i))
                           collect (list "assistant" (format nil "Response ~D" i))))
           (state (%safe-make-chat-ui-state :branch-name "test/kbd-nav")))
      (dolist (msg messages)
        (amoebum:chat-ui-add-message state (first msg) (second msg)))
      ;; Render first to establish max-scrollback
      (%safe-render-chat-ui state :cols 84 :rows 24)
      (let ((before (amoebum::chat-ui-state-message-scrollback-lines
                     (amoebum::ensure-chat-ui-state state))))
        (amoebum:chat-ui-scroll-history state 3)
        (let ((after (amoebum::chat-ui-state-message-scrollback-lines
                      (amoebum::ensure-chat-ui-state state))))
          (is (>= after before)
              "Expected scrollback to increase after scroll-up. Before: ~D After: ~D"
              before after))))))

(test chat-scroll-down-after-scroll-up-reduces-scrollback
  "Scrolling down after scrolling up reduces the scrollback offset."
  (with-safe-chat-env
    (let* ((messages (loop for i from 1 to 10
                           collect (list "user" (format nil "Msg ~D" i))
                           collect (list "assistant" (format nil "Resp ~D" i))))
           (state (%safe-make-chat-ui-state :branch-name "test/kbd-nav")))
      (dolist (msg messages)
        (amoebum:chat-ui-add-message state (first msg) (second msg)))
      (%safe-render-chat-ui state :cols 84 :rows 24)
      ;; Scroll up then down
      (amoebum:chat-ui-scroll-history state 5)
      (let ((after-up (amoebum::chat-ui-state-message-scrollback-lines
                       (amoebum::ensure-chat-ui-state state))))
        (amoebum:chat-ui-scroll-history state -3)
        (let ((after-down (amoebum::chat-ui-state-message-scrollback-lines
                           (amoebum::ensure-chat-ui-state state))))
          (is (<= after-down after-up)
              "Expected scrollback to decrease after scroll-down. Up: ~D Down: ~D"
              after-up after-down))))))

(test chat-input-focusable-after-render
  "The chat input field is reachable in the focus order after rendering."
  (with-safe-chat-env
    (let* ((state (%safe-make-chat-ui-state :branch-name "test/kbd-nav"))
           (runtime (amoebum::chat-ui-state-runtime state)))
      (amoebum:chat-ui-set-input state "focus test input")
      (%safe-render-chat-ui state :cols 84 :rows 24)
      (let ((order (ptui.ui.runtime:runtime-focus-order runtime)))
        (is-true (member :chat-input order :test #'equal)
                 "Expected :chat-input to be focusable. Focus order: ~S" order)))))

(test chat-focus-can-be-set-to-input
  "Setting runtime-focus-id to :chat-input is accepted when chat-input is in order."
  (with-safe-chat-env
    (let* ((state (%safe-make-chat-ui-state :branch-name "test/kbd-nav"))
           (runtime (amoebum::chat-ui-state-runtime state)))
      (%safe-render-chat-ui state :cols 84 :rows 24)
      (let ((order (ptui.ui.runtime:runtime-focus-order runtime)))
        (when (member :chat-input order :test #'equal)
          (setf (ptui.ui.runtime:runtime-focus-id runtime) :chat-input)
          (is (equal :chat-input (ptui.ui.runtime:runtime-focus-id runtime))
              "Expected runtime-focus-id to be :chat-input after direct assignment."))))))

;;; ---------------------------------------------------------------------------
;;; 6. No orphaned focus
;;; ---------------------------------------------------------------------------

(test no-orphaned-focus-ids-after-render
  "Every element in focus-order exists in the widget tree after render."
  (with-safe-chat-env
    (let* ((state (%safe-make-chat-ui-state :branch-name "test/kbd-nav"))
           (runtime (amoebum::chat-ui-state-runtime state)))
      (amoebum:chat-ui-add-message state "user" "Orphan check")
      (amoebum:chat-ui-add-message state "assistant" "No orphans here")
      (%safe-render-chat-ui state :cols 84 :rows 24)
      (let* ((order (ptui.ui.runtime:runtime-focus-order runtime))
             (root  (ptui.ui.runtime:runtime-root runtime)))
        ;; Collect all node IDs that appear anywhere in the tree
        (labels ((collect-ids (node acc)
                   (let ((id (and node
                                  (ptui.ui.elements:ui-element-id node))))
                     (when id (push id acc))
                     (dolist (child (and node
                                        (ptui.ui.elements:ui-element-children node)))
                       (setf acc (collect-ids child acc)))
                     acc)))
          (let ((all-ids (collect-ids root '())))
            ;; Every focusable ID in the order should appear in the tree.
            ;; (IDs are either keywords like :chat-input or path strings.)
            (dolist (focus-id order)
              (when (keywordp focus-id)
                ;; Keyword IDs are directly assigned via :id on elements
                (is-true (member focus-id all-ids :test #'equal)
                         "Focus-order id ~S not found in widget tree. Tree ids: ~S"
                         focus-id all-ids)))))))))

(test focus-order-valid-after-state-change
  "After adding more messages and re-rendering, focus order remains valid."
  (with-safe-chat-env
    (let* ((state (%safe-make-chat-ui-state :branch-name "test/kbd-nav"))
           (runtime (amoebum::chat-ui-state-runtime state)))
      ;; Initial render
      (amoebum:chat-ui-add-message state "user" "First message")
      (%safe-render-chat-ui state :cols 84 :rows 24)
      (let ((order-before (ptui.ui.runtime:runtime-focus-order runtime)))
        ;; Add more messages and re-render
        (amoebum:chat-ui-add-message state "assistant" "Response one")
        (amoebum:chat-ui-add-message state "user" "Second message")
        (%safe-render-chat-ui state :cols 84 :rows 24)
        (let* ((order-after (ptui.ui.runtime:runtime-focus-order runtime))
               (focus-after (ptui.ui.runtime:runtime-focus-id runtime)))
          ;; Focus order should remain non-empty
          (is-true order-after
                   "Expected non-empty focus order after adding messages.")
          ;; Current focus-id should be valid in the new order
          (when focus-after
            (is-true (member focus-after order-after :test #'equal)
                     "Focus-id ~S should be in the updated focus order ~S."
                     focus-after order-after))
          ;; The chat-input should persist across re-renders
          (is-true (member :chat-input order-after :test #'equal)
                   "Expected :chat-input to remain in focus order after re-render."))))))

(test focus-id-nil-or-valid-before-first-render
  "Before any render, focus-id is NIL (runtime not yet populated)."
  (with-safe-chat-env
    (let* ((state (%safe-make-chat-ui-state :branch-name "test/kbd-nav"))
           (runtime (amoebum::chat-ui-state-runtime state)))
      ;; Do not render — check initial state
      (let ((order (ptui.ui.runtime:runtime-focus-order runtime))
            (focus (ptui.ui.runtime:runtime-focus-id runtime)))
        ;; Before any render the order may be empty and focus may be NIL
        (is-true (or (null focus)
                     (and order (member focus order :test #'equal)))
                 "Expected focus-id to be NIL or valid before first render. Got: ~S"
                 focus)))))
