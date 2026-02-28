;;; I298: prompt-input-panel
;;; Extracts prompt box from chat-ui-build-tree.
(in-package :amoebum)

(ptui.ui.panel:defpanel prompt-input-panel (chat-state inner-width)
  (:layout
    (:column
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
         :border-style :rounded))))
  (:keys
    ;; Delegate to existing handler
    (:enter (%handle-input-key chat-state :enter
              (ptui.core.events:key-event-text? event)))
    (:backspace (%handle-input-key chat-state :backspace nil))
    (:delete (%handle-input-key chat-state :delete nil))))
