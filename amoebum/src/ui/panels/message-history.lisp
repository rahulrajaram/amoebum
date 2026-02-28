;;; I297: message-history-panel
;;; Extracts message history scroll from chat-ui-build-tree.
(in-package :amoebum)

(ptui.ui.panel:defpanel message-history-panel (chat-state inner-width inner-height)
  (:data
    (message-lines (%message-line-entries chat-state
                                          (chat-ui-state-messages chat-state)
                                          inner-width)
      :deps (chat-state inner-width))
    (message-widgets
      (mapcar (lambda (entry)
                (%chat-text-widget (getf entry :text)
                                   (getf entry :id)
                                   (getf entry :role)
                                   :styled-segments (getf entry :styled-segments)))
              message-lines)
      :deps (message-lines)))
  (:layout
    (:column
      (history :flex 1
        (let* ((history-stack (ptui.widgets.core:make-stack-widget
                               message-widgets
                               :id :chat-history-stack
                               :direction :column
                               :gap 0))
               (history-total-lines
                 (ptui.layout:layout-size-height
                  (ptui.widgets.core:widget-measure history-stack)))
               (scrollback (chat-ui-state-message-scrollback-lines chat-state)))
          (multiple-value-bind (history-offset new-scrollback max-scrollback)
              (%compute-scroll-offset history-total-lines inner-height scrollback)
            (setf (chat-ui-state-message-scrollback-lines chat-state) new-scrollback
                  (chat-ui-state-max-message-scrollback-lines chat-state) max-scrollback)
            (when (token-stream-active-p (chat-ui-state-stream-state chat-state))
              (setf (chat-ui-state-stream-scroll-follow-p chat-state)
                    (zerop new-scrollback)))
            (ptui.widgets.core:make-scroll-widget
             history-stack
             :id :chat-history-scroll
             :viewport-width inner-width
             :viewport-height inner-height
             :offset history-offset))))))
  (:keys
    (:up (chat-ui-scroll-history chat-state 1))
    (:down (chat-ui-scroll-history chat-state -1))
    (:pgup (chat-ui-scroll-history chat-state 5))
    (:pgdn (chat-ui-scroll-history chat-state -5))))
