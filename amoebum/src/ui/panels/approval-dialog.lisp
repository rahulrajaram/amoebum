;;; I299: approval-dialog-panel
;;; Extracts approval dialog widget and key handling.
(in-package :amoebum)

(ptui.ui.panel:defpanel approval-dialog-panel (chat-state)
  ;; Sync effects consolidated in chat-panel and %sync-all-state!
  (:layout
    (:column
      (dialog :flex 1 :when (approval-dialog-state-active-p
                              (chat-ui-state-approval-dialog-state chat-state))
        (let ((approval-state (chat-ui-state-approval-dialog-state chat-state)))
          (%chat-approval-dialog-widget chat-state approval-state)))))
  (:keys
    (:mode :active :when (approval-dialog-state-active-p
                           (chat-ui-state-approval-dialog-state chat-state))
      (:left (approval-dialog-handle-key!
               (chat-ui-state-approval-dialog-state chat-state) :left))
      (:right (approval-dialog-handle-key!
                (chat-ui-state-approval-dialog-state chat-state) :right))
      (:enter (approval-dialog-handle-key!
                (chat-ui-state-approval-dialog-state chat-state) :enter)))))
