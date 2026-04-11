;;; I297: message-history-panel
;;; Extracts message history scroll from chat-ui-build-tree.
(in-package :amoebum)

(defun build-message-history-widget (chat-state inner-width inner-height)
  "Build the chat history scroll widget and own scrollback bookkeeping."
  (let ((scrollback (chat-ui-state-message-scrollback-lines chat-state)))
    (multiple-value-bind (visible-entries history-total-lines history-offset
                          new-scrollback max-scrollback)
        (%message-line-window chat-state
                              (chat-ui-state-messages chat-state)
                              inner-width
                              inner-height
                              scrollback)
      (when (and (amoebum::%scroll-debug-enabled-p)
                 (not (zerop scrollback)))
        (amoebum::%scroll-debug-log
         "RENDER total=~D viewport=~D scrollback-in=~D offset=~D new-sb=~D max-sb=~D msgs=~D lines=~D"
         history-total-lines inner-height scrollback
         history-offset new-scrollback max-scrollback
         (length (chat-ui-state-messages chat-state))
         history-total-lines))
      (setf (chat-ui-state-message-scrollback-lines chat-state) new-scrollback
            (chat-ui-state-max-message-scrollback-lines chat-state) max-scrollback)
      (when (token-stream-active-p (chat-ui-state-stream-state chat-state))
        (setf (chat-ui-state-stream-scroll-follow-p chat-state)
              (zerop new-scrollback)))
      (let* ((vis-start (max 0 history-offset))
             (vis-end (+ vis-start (length visible-entries)))
             (visible-widgets
               (loop for entry in visible-entries
                     collect (%chat-text-widget
                              (getf entry :text)
                              (getf entry :id)
                              (getf entry :role)
                              :styled-segments (getf entry :styled-segments))))
             (top-spacer-h vis-start)
             (bottom-spacer-h (max 0 (- history-total-lines vis-end)))
             (stack-children
               (append (when (> top-spacer-h 0)
                         (list (ptui.widgets.core:make-spacer-widget
                                0 top-spacer-h :key :vscroll-top)))
                       visible-widgets
                       (when (> bottom-spacer-h 0)
                         (list (ptui.widgets.core:make-spacer-widget
                                0 bottom-spacer-h :key :vscroll-bottom)))))
             (history-stack (ptui.widgets.core:make-stack-widget
                             stack-children
                             :id :chat-history-stack
                             :direction :column
                             :gap 0)))
        (ptui.widgets.core:make-scroll-widget
         history-stack
         :id :chat-history-scroll
         :viewport-width inner-width
         :viewport-height inner-height
         :offset history-offset
         :scroll-bar :auto)))))

(ptui.ui.panel:defpanel message-history-panel (chat-state inner-width inner-height)
  (:layout
    (:column
      (history :flex 1
        (build-message-history-widget chat-state inner-width inner-height))))
  (:keys
    (:up (chat-ui-scroll-history chat-state 1))
    (:down (chat-ui-scroll-history chat-state -1))
    (:pgup (chat-ui-scroll-history chat-state 5))
    (:pgdn (chat-ui-scroll-history chat-state -5))))
