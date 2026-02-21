;;; I299: approval-dialog-panel
;;; Extracts approval dialog widget and key handling.
(in-package :amoebum)

(ptui.ui.panel:defpanel approval-dialog-panel (chat-state)
  (:effects
    (sync-approval (%sync-pending-approval-dialog! chat-state)
      :deps (chat-state)))
  (:layout
    (:column
      (dialog :flex 1 :when (approval-dialog-state-active-p
                              (chat-ui-state-approval-dialog-state chat-state))
        (let ((approval-state (chat-ui-state-approval-dialog-state chat-state)))
          (make-approval-dialog-widget
           (list :tool-name (approval-dialog-state-tool-name approval-state)
                 :command (approval-dialog-state-command approval-state)
                 :path (approval-dialog-state-path approval-state)
                 :reason (approval-dialog-state-reason approval-state)
                 :selected-option (approval-dialog-state-selected-option approval-state)))))))
  (:keys
    (:mode :active :when (approval-dialog-state-active-p
                           (chat-ui-state-approval-dialog-state chat-state))
      (:left (approval-dialog-handle-key!
               (chat-ui-state-approval-dialog-state chat-state) :left))
      (:right (approval-dialog-handle-key!
                (chat-ui-state-approval-dialog-state chat-state) :right))
      (:enter (approval-dialog-handle-key!
                (chat-ui-state-approval-dialog-state chat-state) :enter)))))
