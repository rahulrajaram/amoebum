;;; I300: fuzzy-picker-panel
;;; Extracts fuzzy picker widget and key handling.
(in-package :amoebum)

(ptui.ui.panel:defpanel fuzzy-picker-panel (chat-state)
  (:data
    (picker-state (%chat-sync-fuzzy-picker! chat-state) :deps (chat-state)))
  (:layout
    (:column
      (picker :flex 1 :when (fuzzy-picker-state-active-p picker-state)
        (make-fuzzy-picker-widget picker-state))))
  (:keys
    (:mode :active :when (fuzzy-picker-state-active-p
                           (%ensure-chat-fuzzy-picker-state chat-state))
      (:up (%handle-fuzzy-picker-key chat-state :up))
      (:down (%handle-fuzzy-picker-key chat-state :down))
      (:pgup (%handle-fuzzy-picker-key chat-state :pgup))
      (:pgdn (%handle-fuzzy-picker-key chat-state :pgdn))
      (:escape (%handle-fuzzy-picker-key chat-state :escape))
      (:enter (%handle-fuzzy-picker-key chat-state :enter)))))
