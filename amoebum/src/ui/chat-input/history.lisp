(in-package :amoebum)

;;; NXT-543: history- and fuzzy-picker-application helpers extracted from
;;; ui/chat-input.lisp. These commit a selected entry from the fuzzy or
;;; history picker back into the input buffer and tear down picker state.
;;; Loaded after state/render so callers in events can rely on them.

(defun %chat-apply-fuzzy-picker-selection! (chat-state)
  (let* ((picker (%ensure-chat-fuzzy-picker-state chat-state))
         (selected-path (fuzzy-picker-selected-path picker)))
    (if (and (stringp selected-path)
             (plusp (length selected-path)))
        (let* ((raw-input (chat-ui-state-input-text chat-state))
               (replaced (fuzzy-picker-apply-selection raw-input picker selected-path))
               (next-input (if (and (plusp (length replaced))
                                    (%whitespace-char-p
                                     (char replaced (1- (length replaced)))))
                               replaced
                               (concatenate 'string replaced " "))))
          (setf (chat-ui-state-input-text chat-state) next-input
                (chat-ui-state-prompt-scroll-offset chat-state) nil)
          (fuzzy-picker-deactivate! picker)
          t)
        (progn
          (fuzzy-picker-deactivate! picker)
          t))))

(defun %chat-apply-history-picker-selection! (chat-state)
  (let* ((picker (%ensure-chat-fuzzy-picker-state chat-state))
         (selected-path (fuzzy-picker-selected-path picker))
         (table (chat-ui-state-history-selection-map chat-state))
         (replacement (and (hash-table-p table)
                           (stringp selected-path)
                           (gethash selected-path table))))
    (setf (chat-ui-state-input-text chat-state) (or replacement "")
          (chat-ui-state-prompt-scroll-offset chat-state) nil)
    (%chat-deactivate-history-search! chat-state)
    t))
