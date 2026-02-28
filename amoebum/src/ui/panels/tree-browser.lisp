;;; I301: tree-browser-panel
;;; Extracts tree browser widget and key handling.
(in-package :amoebum)

(ptui.ui.panel:defpanel tree-browser-panel (chat-state)
  (:data
    (tree-state (%ensure-chat-tree-browser-state chat-state) :deps (chat-state)))
  (:layout
    (:column
      (tree :flex 1 :when (and (typep tree-state 'tree-browser-state)
                                (tree-browser-state-active-p tree-state))
        (make-tree-browser-widget tree-state))))
  (:keys
    (:mode :active :when (let ((ts (%ensure-chat-tree-browser-state chat-state)))
                           (and (typep ts 'tree-browser-state)
                                (tree-browser-state-active-p ts)
                                (zerop (length (chat-ui-state-input-text chat-state)))))
      (:up (%handle-tree-browser-key chat-state :up))
      (:down (%handle-tree-browser-key chat-state :down))
      (:left (%handle-tree-browser-key chat-state :left))
      (:right (%handle-tree-browser-key chat-state :right))
      (:enter (%handle-tree-browser-key chat-state :enter))
      (:escape (%handle-tree-browser-key chat-state :escape)))))
