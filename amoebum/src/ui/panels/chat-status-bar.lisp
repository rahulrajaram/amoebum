;;; I303: chat-status-bar-panel
;;; Extracts status bar, plan presentation, and provider health panel.
(in-package :amoebum)

(defun build-chat-status-bar-widget (chat-state inner-width &key exit-warning-active-p)
  "Build the bottom status region for chat-panel."
  (if exit-warning-active-p
      (%chat-text-widget (%chat-exit-warning-text)
                         :chat-status-warning
                         :warning)
      (make-status-bar-widget
       (chat-ui-state-status-bar-state chat-state)
       :id :chat-status-bar
       :width inner-width)))

(ptui.ui.panel:defpanel chat-status-bar-panel (chat-state inner-width)
  (:data
    (plan-state (current-plan-mode-state) :deps (chat-state)))
  (:layout
    (:column
      (provider :fixed 5 :when (chat-ui-state-provider-dashboard-visible-p chat-state)
        (progn
          (provider-health-refresh!)
          (provider-health-panel
           (list :entries (provider-health-entries)
                 :updated-at (provider-health-last-updated-at)))))
      (plan :fixed 6 :when (not (null (%chat-plan-presentation-widget plan-state chat-state)))
        (%chat-plan-presentation-widget plan-state chat-state))
      (status :fixed 1
        (build-chat-status-bar-widget chat-state inner-width)))))
