;;; I300: fuzzy-picker-panel
;;; Extracts fuzzy picker widget and key handling.
(in-package :amoebum)

(defun chat-panel-handle-fuzzy-picker-key (chat-state key)
  (let ((picker (%ensure-chat-fuzzy-picker-state chat-state)))
    (when (fuzzy-picker-state-active-p picker)
      (fuzzy-picker-step! picker)
      (case key
        (:up
         (fuzzy-picker-move-selection! picker -1)
         t)
        (:down
         (fuzzy-picker-move-selection! picker 1)
         t)
        (:pgup
         (fuzzy-picker-move-selection! picker -5)
         t)
        ((:pgdn :pgdown)
         (fuzzy-picker-move-selection! picker 5)
         t)
        (:home
         (fuzzy-picker-home-selection! picker)
         t)
        (:end
         (fuzzy-picker-end-selection! picker)
         t)
        (:escape
         (if (chat-ui-state-history-search-active-p chat-state)
             (%chat-deactivate-history-search! chat-state :restore-input-p t)
             (fuzzy-picker-deactivate! picker))
         t)
        ((:enter :return)
         (if (chat-ui-state-history-search-active-p chat-state)
             (%chat-apply-history-picker-selection! chat-state)
             (%chat-apply-fuzzy-picker-selection! chat-state)))
        (otherwise
         nil)))))

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
      (:up (chat-panel-handle-fuzzy-picker-key chat-state :up))
      (:down (chat-panel-handle-fuzzy-picker-key chat-state :down))
      (:pgup (chat-panel-handle-fuzzy-picker-key chat-state :pgup))
      (:pgdn (chat-panel-handle-fuzzy-picker-key chat-state :pgdn))
      (:escape (chat-panel-handle-fuzzy-picker-key chat-state :escape))
      (:enter (chat-panel-handle-fuzzy-picker-key chat-state :enter)))))
