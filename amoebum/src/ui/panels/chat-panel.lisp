;;; I304: chat-panel — top-level composition of all sub-panels
;;; Replaces chat-ui-build-tree + handle-chat-ui-event with declarative layout.
(in-package :amoebum)

(ptui.ui.panel:defpanel chat-panel (chat-state cols rows)
  (:data
    (inner-width (max 20 (- cols 2)) :deps (cols))
    (inner-height (max 8 (- rows 2)) :deps (rows))
    (approval-state (chat-ui-state-approval-dialog-state chat-state) :deps (chat-state))
    (approval-active-p (approval-dialog-state-active-p approval-state) :deps (approval-state))
    (picker-state (%chat-sync-fuzzy-picker! chat-state) :deps (chat-state))
    (picker-active-p (fuzzy-picker-state-active-p picker-state) :deps (picker-state))
    (tree-state (%ensure-chat-tree-browser-state chat-state) :deps (chat-state))
    (tree-active-p (and (typep tree-state 'tree-browser-state)
                        (tree-browser-state-active-p tree-state))
      :deps (tree-state))
    (stream-active-p (token-stream-active-p (chat-ui-state-stream-state chat-state))
      :deps (chat-state))
    (plan-state (current-plan-mode-state) :deps (chat-state))
    (plan-widget (%chat-plan-presentation-widget plan-state chat-state) :deps (plan-state chat-state))
    (plan-active-p (not (null plan-widget)) :deps (plan-widget))
    (provider-visible-p (chat-ui-state-provider-dashboard-visible-p chat-state)
      :deps (chat-state)))
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
      (plan :fixed 6 :when plan-active-p
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
              (%compute-scroll-offset history-total-lines inner-height scrollback)
            (setf (chat-ui-state-message-scrollback-lines chat-state) new-scrollback
                  (chat-ui-state-max-message-scrollback-lines chat-state) max-scrollback)
            (when stream-active-p
              (setf (chat-ui-state-stream-scroll-follow-p chat-state)
                    (zerop new-scrollback)))
            (ptui.widgets.core:make-scroll-widget
             history-stack
             :id :chat-history-scroll
             :viewport-width inner-width
             :viewport-height inner-height
             :offset history-offset))))
      (approval :fixed 4 :when approval-active-p
        (make-approval-dialog-widget
         (list :tool-name (approval-dialog-state-tool-name approval-state)
               :command (approval-dialog-state-command approval-state)
               :path (approval-dialog-state-path approval-state)
               :reason (approval-dialog-state-reason approval-state)
               :selected-option (approval-dialog-state-selected-option approval-state))))
      (picker :fixed 8 :when picker-active-p
        (make-fuzzy-picker-widget picker-state))
      (stream-hint :fixed 1 :when stream-active-p
        (%chat-text-widget "Streaming... Press Ctrl-C to stop early."
                           :chat-stream-stop-hint
                           :meta))
      (input :fixed 4
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
        (make-status-bar-widget
         (chat-ui-state-status-bar-state chat-state)
         :id :chat-status-bar
         :width inner-width))))
  (:keys
    (:mode :approval :when approval-active-p
      (:left (approval-dialog-handle-key! approval-state :left))
      (:right (approval-dialog-handle-key! approval-state :right))
      (:enter (approval-dialog-handle-key! approval-state :enter)))
    (:mode :picker :when picker-active-p
      (:up (%handle-fuzzy-picker-key chat-state :up))
      (:down (%handle-fuzzy-picker-key chat-state :down))
      (:pgup (%handle-fuzzy-picker-key chat-state :pgup))
      (:pgdn (%handle-fuzzy-picker-key chat-state :pgdn))
      (:escape (%handle-fuzzy-picker-key chat-state :escape))
      (:enter (%handle-fuzzy-picker-key chat-state :enter)))
    (:mode :tree :when (and tree-active-p
                            (zerop (length (chat-ui-state-input-text chat-state))))
      (:up (%handle-tree-browser-key chat-state :up))
      (:down (%handle-tree-browser-key chat-state :down))
      (:left (%handle-tree-browser-key chat-state :left))
      (:right (%handle-tree-browser-key chat-state :right))
      (:enter (%handle-tree-browser-key chat-state :enter))
      (:escape (%handle-tree-browser-key chat-state :escape)))
    (:mode :default
      (:up (let ((has-text (plusp (length (chat-ui-state-input-text chat-state)))))
             (unless has-text (chat-ui-scroll-history chat-state 1))))
      (:down (let ((has-text (plusp (length (chat-ui-state-input-text chat-state)))))
               (unless has-text (chat-ui-scroll-history chat-state -1))))
      (:pgup (chat-ui-scroll-history chat-state 5))
      (:pgdn (chat-ui-scroll-history chat-state -5))
      (:ctrl-c (when stream-active-p
                  (token-stream-request-cancel
                   (chat-ui-state-stream-state chat-state))))
      (:escape (when stream-active-p
                 (token-stream-request-cancel
                  (chat-ui-state-stream-state chat-state)))))))
