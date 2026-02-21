;;; I301: tree-browser-panel
;;; Extracts tree browser widget and key handling.
(in-package :amoebum)

(defun chat-panel-handle-tree-browser-key (chat-state key)
  (let ((tree-state (%ensure-chat-tree-browser-state chat-state)))
    (when (and (typep tree-state 'tree-browser-state)
               (zerop (length (chat-ui-state-input-text chat-state)))
               (member key '(:up :down :left :right :enter :return :escape)))
      (cond
        ((eql key :escape)
         (when (tree-browser-state-active-p tree-state)
           (setf (tree-browser-state-active-p tree-state) nil)
           t))
        (t
         (unless (tree-browser-state-active-p tree-state)
           (setf (tree-browser-state-active-p tree-state) t))
         (tree-browser-handle-key! tree-state key))))))

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
      (:up (chat-panel-handle-tree-browser-key chat-state :up))
      (:down (chat-panel-handle-tree-browser-key chat-state :down))
      (:left (chat-panel-handle-tree-browser-key chat-state :left))
      (:right (chat-panel-handle-tree-browser-key chat-state :right))
      (:enter (chat-panel-handle-tree-browser-key chat-state :enter))
      (:escape (chat-panel-handle-tree-browser-key chat-state :escape)))))
