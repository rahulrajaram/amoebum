(in-package :amoebum)

;;; NXT-431: conversation-entry coercion and history-picker helpers extracted
;;; from ui/chat-state.lisp so the leaf coordinator stops owning display-shape
;;; normalization for session-history entries.

(defun %ensure-chat-fuzzy-picker-state (chat-state)
  (let ((picker (chat-ui-state-fuzzy-picker-state chat-state)))
    (unless (typep picker 'fuzzy-picker-state)
      (setf picker (make-fuzzy-picker-state)
            (chat-ui-state-fuzzy-picker-state chat-state) picker))
    picker))

(defun %history-picker-candidate-line (entry index)
  (let* ((role (string-upcase (or (conversation-history-entry-role entry) "assistant")))
         (stamp (format-history-timestamp
                 (conversation-history-entry-timestamp entry)))
         (tool-name (conversation-history-entry-name entry))
         (content (%coerce-chat-history-entry-content
                   (conversation-history-entry-content entry)))
         (body (if (%conversation-blank-p content)
                   "(empty)"
                   (%truncate-inline-text content 120))))
    (format nil "~4,'0D [~A] ~A~@[ tool=~A~]: ~A"
            index
            role
            stamp
            tool-name
            body)))

(defun %chat-refresh-history-picker-index! (chat-state)
  (let* ((picker (%ensure-chat-fuzzy-picker-state chat-state))
         (table (chat-ui-state-history-selection-map chat-state))
         (entries (conversation-state-entries (%ensure-chat-conversation-state chat-state)))
         (signature (mapcar (lambda (entry)
                              (list (conversation-history-entry-timestamp entry)
                                    (conversation-history-entry-role entry)
                                    (conversation-history-entry-name entry)
                                    (conversation-history-entry-content entry)))
                            entries))
         (total (length entries))
         (candidates '()))
    (unless (hash-table-p table)
      (setf table (%make-chat-history-selection-table)
            (chat-ui-state-history-selection-map chat-state) table))
    (when (equal signature (chat-ui-state-history-search-signature chat-state))
      (return-from %chat-refresh-history-picker-index! picker))
    (clrhash table)
    (loop for index downfrom (1- total) to 0 do
      (let* ((entry (nth index entries))
             (line (%history-picker-candidate-line entry index))
             (replacement (or (conversation-history-entry-content entry) "")))
        (setf (gethash line table) replacement)
        (push line candidates)))
    (setf (fuzzy-picker-state-project-root picker) ":history"
          (fuzzy-picker-state-files picker) (coerce (nreverse candidates) 'vector)
          (fuzzy-picker-state-ignore-rules picker) '()
          (fuzzy-picker-state-index-ready-p picker) t
          (fuzzy-picker-state-scan-cursor picker) 0
          (fuzzy-picker-state-scan-complete-p picker) nil
          (fuzzy-picker-state-top-results picker) '()
          (fuzzy-picker-state-selected-index picker) 0
          (fuzzy-picker-state-context-label picker) "history"
          (fuzzy-picker-state-empty-message picker) "  [none] no matching messages")
    (setf (chat-ui-state-history-search-signature chat-state) signature)
    picker))

(defun %chat-activate-history-search! (chat-state)
  (let ((picker (%ensure-chat-fuzzy-picker-state chat-state))
        (current-input (chat-ui-state-input-text chat-state)))
    (setf (chat-ui-state-history-search-active-p chat-state) t
          (chat-ui-state-history-search-original-input chat-state) current-input
          (chat-ui-state-history-search-signature chat-state) nil
          (chat-ui-state-input-text chat-state) ""
          (chat-ui-state-prompt-scroll-offset chat-state) nil)
    (%chat-refresh-history-picker-index! chat-state)
    (setf (fuzzy-picker-state-active-p picker) t)
    (fuzzy-picker-set-query! picker "" 0 0)
    (fuzzy-picker-step! picker :batch-size most-positive-fixnum)
    t))

(defun %chat-deactivate-history-search! (chat-state &key (restore-input-p nil))
  (let ((picker (%ensure-chat-fuzzy-picker-state chat-state)))
    (when restore-input-p
      (setf (chat-ui-state-input-text chat-state)
            (chat-ui-state-history-search-original-input chat-state)
            (chat-ui-state-prompt-scroll-offset chat-state) nil))
    (setf (chat-ui-state-history-search-active-p chat-state) nil
          (chat-ui-state-history-search-original-input chat-state) ""
          (chat-ui-state-history-search-signature chat-state) nil)
    (setf (fuzzy-picker-state-context-label picker) "@ file/dir"
          (fuzzy-picker-state-empty-message picker) "  [none] no matching files")
    (fuzzy-picker-deactivate! picker)
    (%prefer-chat-input-focus! chat-state)
    t))

(defun %chat-sync-fuzzy-picker! (chat-state &key (step-p t))
  (let ((picker (%ensure-chat-fuzzy-picker-state chat-state)))
    (if (chat-ui-state-history-search-active-p chat-state)
        (let ((query (chat-ui-state-input-text chat-state)))
          (%chat-refresh-history-picker-index! chat-state)
          (setf (fuzzy-picker-state-active-p picker) t)
          (fuzzy-picker-set-query! picker query 0 (length query))
          (when step-p
            (fuzzy-picker-step! picker :batch-size most-positive-fixnum)))
        (progn
          (setf (fuzzy-picker-state-context-label picker) "@ file/dir"
                (fuzzy-picker-state-empty-message picker) "  [none] no matching files")
          (fuzzy-picker-sync-input! picker
                                    (chat-ui-state-input-text chat-state)
                                    :root (%chat-project-root))
          (when step-p
            (fuzzy-picker-step! picker))))
    picker))

(defun %ensure-chat-tree-browser-state (chat-state)
  (let* ((resolved-root (%resolve-search-root (%chat-project-root)))
         (resolved-root-key (coerce-path-string resolved-root))
         (tree-state (chat-ui-state-tree-browser-state chat-state))
         (current-root-key
           (and (typep tree-state 'tree-browser-state)
                (tree-browser-state-root-path tree-state)
                (coerce-path-string (tree-browser-state-root-path tree-state)))))
    (unless (and (typep tree-state 'tree-browser-state)
                 (equal current-root-key resolved-root-key))
      (setf tree-state
            (handler-case
                (make-git-file-tree-browser-state
                 :root resolved-root
                 :show-root-p t
                 :active-p nil
                 :visible-row-count 3)
              (error ()
                (make-empty-tree-browser-state :label "files"))))
      (setf (chat-ui-state-tree-browser-state chat-state) tree-state))
    tree-state))

(defun %ensure-chat-conversation-state (chat-state)
  (let ((conversation (chat-ui-state-conversation chat-state)))
    (unless (typep conversation 'conversation-state)
      (setf conversation
            (make-conversation-state :project-root (%chat-project-root)))
      (setf (chat-ui-state-conversation chat-state) conversation))
    conversation))

(defun %apply-chat-conversation! (chat-state next-conversation)
  "Install NEXT-CONVERSATION into CHAT-STATE and resync the derived UI copy.
Keeps restore/checkpoint/session callers on one path so durable conversation
history stays the single source of truth for append-only ordering and tool
pairing."
  (when (and (typep chat-state 'chat-ui-state)
             (typep next-conversation 'conversation-state))
    (setf (chat-ui-state-conversation chat-state) next-conversation
          (chat-ui-state-messages chat-state)
          (conversation-state-messages next-conversation)
          (chat-ui-state-message-scrollback-lines chat-state) 0
          (chat-ui-state-max-message-scrollback-lines chat-state) 0)
    (%invalidate-styled-lines-cache)
    (%sync-chat-context-usage! chat-state :allow-auto-compress-p nil))
  next-conversation)
