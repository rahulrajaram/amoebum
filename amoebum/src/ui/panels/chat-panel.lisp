;;; I304: chat-panel — top-level composition of all sub-panels
;;; Replaces chat-ui-build-tree + handle-chat-ui-event with declarative layout.
(in-package :amoebum)

(defun %chat-panel-history-viewport-height (inner-height
                                            &key
                                              provider-active-p
                                              tree-active-p
                                              plan-active-p
                                              approval-active-p
                                              picker-active-p)
  "Compute the actual allocated history region height for chat-panel."
  (let* ((constraints
           (remove nil
                   (list
                    (when provider-active-p
                      (ptui.layout.constraints:fixed 'provider 5))
                    (when tree-active-p
                      (ptui.layout.constraints:fixed 'tree 10))
                    (when plan-active-p
                      (ptui.layout.constraints:fixed 'plan 12))
                    (ptui.layout.constraints:flex 'history :weight 1)
                    (when approval-active-p
                      (ptui.layout.constraints:fixed 'approval 8))
                    (when picker-active-p
                      (ptui.layout.constraints:fixed 'picker 8))
                    (ptui.layout.constraints:fixed 'input 3)
                    (ptui.layout.constraints:fixed 'status 1))))
         (solved (ptui.layout.solver:solve-constraints constraints
                                                       (max 0 inner-height))))
    (max 0 (or (cdr (assoc 'history solved :test #'eq)) 0))))

(ptui.ui.panel:defpanel chat-panel (chat-state cols rows)
  (:data
    (inner-width (max 20 (- cols 2)) :deps (cols))
    (inner-height (max 8 (- rows 2)) :deps (rows))
    ;; NOTE: use-memo compares deps with EQUAL, and EQUAL on structs is EQ
    ;; (identity).  Since chat-state/approval-state/etc. are mutable structs
    ;; whose references never change, using them as deps would cache values
    ;; forever.  Instead, use the actual computed value as the dep so the memo
    ;; invalidates when the underlying field changes.
    (approval-state (chat-ui-state-approval-dialog-state chat-state) :deps (chat-state))
    (approval-active-p (approval-dialog-state-active-p approval-state)
      :deps ((approval-dialog-state-active-p approval-state)))
    (picker-state (%chat-sync-fuzzy-picker! chat-state) :deps (chat-state))
    (picker-active-p (fuzzy-picker-state-active-p picker-state)
      :deps ((fuzzy-picker-state-active-p picker-state)))
    (tree-state (%ensure-chat-tree-browser-state chat-state) :deps (chat-state))
    (tree-active-p (and (typep tree-state 'tree-browser-state)
                        (tree-browser-state-active-p tree-state))
      :deps ((and (typep tree-state 'tree-browser-state)
                  (tree-browser-state-active-p tree-state))))
    (stream-active-p (token-stream-active-p (chat-ui-state-stream-state chat-state))
      :deps ((token-stream-active-p (chat-ui-state-stream-state chat-state))))
    (exit-warning-active-p (%chat-exit-warning-active-p chat-state)
      :deps (chat-state))
    (plan-state (current-plan-mode-state) :deps ((current-plan-mode-state)))
    (plan-widget (%chat-plan-presentation-widget plan-state chat-state) :deps (plan-state chat-state))
    (plan-active-p (not (null plan-widget)) :deps (plan-widget))
    (provider-visible-p (chat-ui-state-provider-dashboard-visible-p chat-state)
      :deps ((chat-ui-state-provider-dashboard-visible-p chat-state)))
    (history-viewport-height
      (%chat-panel-history-viewport-height
       inner-height
       :provider-active-p provider-visible-p
       :tree-active-p tree-active-p
       :plan-active-p plan-active-p
       :approval-active-p approval-active-p
       :picker-active-p picker-active-p)
      :deps (inner-height provider-visible-p tree-active-p plan-active-p
             approval-active-p picker-active-p)))
  (:effects
    (sync-approval (%sync-pending-approval-dialog! chat-state)
      :deps (chat-state))
    (drain-streams (%drain-stream-events chat-state)
      :deps (chat-state))
    (stream-summary (%publish-status-bar-stream-summary-if-needed chat-state)
      :deps (chat-state))
    (budget-warning (%emit-stream-budget-warning-if-needed chat-state)
      :deps (chat-state))
    (context-usage (%sync-chat-context-usage! chat-state)
      :deps (chat-state))
    (idle-hooks (%run-chat-idle-hooks-if-needed)
      :deps ())
    (auto-checkpoint
      (progn
        (maybe-auto-checkpoint
         :conversation (%ensure-chat-conversation-state chat-state)
         :config (%chat-config)
         :busy-p stream-active-p)
        nil)
      :deps (chat-state))
    (frame-count
      (progn (incf (chat-ui-state-frame-count chat-state)) nil)
      :deps (chat-state))
    (provider-refresh
      (when provider-visible-p (provider-health-refresh!))
      :deps (provider-visible-p)))
  (:layout
    (:column
      (provider :fixed 5 :when provider-visible-p
        (provider-health-panel
         (list :entries (provider-health-entries)
               :updated-at (provider-health-last-updated-at))))
      (tree :fixed 10 :when tree-active-p
        (make-tree-browser-widget tree-state))
      (plan :fixed 12 :when plan-active-p
        plan-widget)
      (history :flex 1
        (let* ((message-lines (%message-line-entries chat-state
                                                     (chat-ui-state-messages chat-state)
                                                     inner-width))
               (message-widgets
                 (mapcar (lambda (entry)
                           (%chat-text-widget (getf entry :text)
                                              (getf entry :id)
                                              (getf entry :role)
                                              :styled-segments (getf entry :styled-segments)))
                         message-lines))
               (history-stack (ptui.widgets.core:make-stack-widget
                               message-widgets
                               :id :chat-history-stack
                               :direction :column :gap 0))
               (history-total-lines
                 (ptui.layout:layout-size-height
                  (ptui.widgets.core:widget-measure history-stack)))
               (scrollback (chat-ui-state-message-scrollback-lines chat-state)))
          (multiple-value-bind (history-offset new-scrollback max-scrollback)
              (%compute-scroll-offset history-total-lines history-viewport-height scrollback)
            (setf (chat-ui-state-message-scrollback-lines chat-state) new-scrollback
                  (chat-ui-state-max-message-scrollback-lines chat-state) max-scrollback)
            (when stream-active-p
              (setf (chat-ui-state-stream-scroll-follow-p chat-state)
                    (zerop new-scrollback)))
            (ptui.widgets.core:make-scroll-widget
             history-stack
             :id :chat-history-scroll
             :viewport-width inner-width
             :viewport-height history-viewport-height
             :offset history-offset))))
      (approval :fixed 8 :when approval-active-p
        (%chat-approval-dialog-widget chat-state approval-state))
      (picker :fixed 8 :when picker-active-p
        (make-fuzzy-picker-widget picker-state))
      (input :fixed 3
        (ptui.components.prompt-box:make-prompt-box-widget
         (chat-ui-state-input-text chat-state)
         :id :chat-input
         :min-width 18
         :max-width inner-width
         :min-rows 1
         :max-rows 4
         :scroll-offset (chat-ui-state-prompt-scroll-offset chat-state)
         :cursor-position (chat-ui-state-cursor-position chat-state)
         :border-style :rounded))
      (status :fixed 1
        (if exit-warning-active-p
            (%chat-text-widget (%chat-exit-warning-text)
                               :chat-status-warning
                               :system)
            (make-status-bar-widget
             (chat-ui-state-status-bar-state chat-state)
             :id :chat-status-bar
             :width inner-width)))))
  (:keys
    (:mode :approval :when approval-active-p
      (:up (approval-dialog-handle-key! approval-state :up))
      (:down (approval-dialog-handle-key! approval-state :down))
      (:left (approval-dialog-handle-key! approval-state :up))
      (:right (approval-dialog-handle-key! approval-state :down))
      (:enter (approval-dialog-handle-key! approval-state :enter))
      (:escape (approval-dialog-handle-key! approval-state :escape))
      (:text (approval-dialog-handle-text!
               approval-state
               (ptui.core.events:key-event-text? ptui.ui.panel::event))))
    (:mode :picker :when picker-active-p
      (:up (chat-panel-handle-fuzzy-picker-key chat-state :up))
      (:down (chat-panel-handle-fuzzy-picker-key chat-state :down))
      (:pgup (chat-panel-handle-fuzzy-picker-key chat-state :pgup))
      (:pgdn (chat-panel-handle-fuzzy-picker-key chat-state :pgdn))
      (:escape (chat-panel-handle-fuzzy-picker-key chat-state :escape))
      (:enter (chat-panel-handle-fuzzy-picker-key chat-state :enter)))
    (:mode :tree :when (and tree-active-p
                            (zerop (length (chat-ui-state-input-text chat-state))))
      (:up (chat-panel-handle-tree-browser-key chat-state :up))
      (:down (chat-panel-handle-tree-browser-key chat-state :down))
      (:left (chat-panel-handle-tree-browser-key chat-state :left))
      (:right (chat-panel-handle-tree-browser-key chat-state :right))
      (:enter (chat-panel-handle-tree-browser-key chat-state :enter))
      (:escape (chat-panel-handle-tree-browser-key chat-state :escape)))
    (:mode :default
      (:text (chat-panel-handle-input-key
               chat-state
               :text
               (ptui.core.events:key-event-text? ptui.ui.panel::event)
               inner-width))
      (:enter (chat-panel-handle-input-key chat-state :enter nil inner-width))
      (:backspace (chat-panel-handle-input-key chat-state :backspace nil inner-width))
      (:delete (chat-panel-handle-input-key chat-state :delete nil inner-width))
      (:ctrl-j (chat-panel-handle-input-key chat-state :ctrl-j nil inner-width))
      (:tab (chat-panel-handle-input-key chat-state :tab nil inner-width))
      (:ctrl-p (chat-panel-handle-input-key chat-state :ctrl-p nil inner-width))
      (:ctrl-n (chat-panel-handle-input-key chat-state :ctrl-n nil inner-width))
      (:ctrl-r (chat-panel-handle-input-key chat-state :ctrl-r nil inner-width))
      (:ctrl-a (chat-panel-handle-input-key chat-state :ctrl-a nil inner-width))
      (:ctrl-e (chat-panel-handle-input-key chat-state :ctrl-e nil inner-width))
      (:left (chat-panel-handle-input-key chat-state :left nil inner-width))
      (:right (chat-panel-handle-input-key chat-state :right nil inner-width))
      (:ctrl-left (chat-panel-handle-input-key chat-state :ctrl-left nil inner-width))
      (:ctrl-right (chat-panel-handle-input-key chat-state :ctrl-right nil inner-width))
      (:home (chat-panel-handle-input-key chat-state :home nil inner-width))
      (:end (chat-panel-handle-input-key chat-state :end nil inner-width))
      (:ctrl-w (chat-panel-handle-input-key chat-state :ctrl-w nil inner-width))
      (:ctrl-u (chat-panel-handle-input-key chat-state :ctrl-u nil inner-width))
      (:ctrl-k (chat-panel-handle-input-key chat-state :ctrl-k nil inner-width))
      (:up (let ((has-text (plusp (length (chat-ui-state-input-text chat-state)))))
             (if has-text
                 (chat-panel-handle-input-key chat-state :up nil inner-width)
                 (chat-ui-scroll-history chat-state 1))))
      (:down (let ((has-text (plusp (length (chat-ui-state-input-text chat-state)))))
               (if has-text
                   (chat-panel-handle-input-key chat-state :down nil inner-width)
                   (chat-ui-scroll-history chat-state -1))))
      (:pgup (chat-ui-scroll-history chat-state 5))
      (:pgdn (chat-ui-scroll-history chat-state -5))
      (:ctrl-c (when stream-active-p
                  (token-stream-request-cancel
                   (chat-ui-state-stream-state chat-state))))
      (:escape (if stream-active-p
                   (token-stream-request-cancel
                    (chat-ui-state-stream-state chat-state))
                   (chat-panel-handle-input-key chat-state :escape nil inner-width))))))
