;;; I304: chat-panel — top-level composition of all sub-panels
;;; Replaces chat-ui-build-tree + handle-chat-ui-event with declarative layout.
(in-package :amoebum)

;;; Forward declaration - defined in prompt-input.lisp which is loaded after this file
(declaim (ftype function chat-panel-handle-input-key))

(defparameter +chat-panel-layout-aliases+
  '((:provider "provider-dashboard" "provider")
    (:tree "tree-browser" "tree")
    (:plan "plan-view" "plan")
    (:handoff "worktree-handoff" "handoff")
    (:history "message-history" "history")
    (:approval "approval-dialog" "approval")
    (:picker "fuzzy-picker" "picker")
    (:input "input-prompt" "prompt" "input")
    (:status "status-bar" "status"))
  "Aliases that map chat panel regions to YAML layout child names.")

(defun %chat-panel-layout-child (region-key &optional (layout *yaml-layout-loaded*))
  "Return the YAML layout child for REGION-KEY, or NIL when no override exists."
  (let ((aliases (rest (assoc region-key +chat-panel-layout-aliases+ :test #'eq))))
    (when (and layout aliases)
      (%yaml-layout-find-first-child layout aliases))))

(defun %chat-panel-layout-visible-p (region-key default &optional (layout *yaml-layout-loaded*))
  "Return the configured visibility for REGION-KEY, falling back to DEFAULT."
  (let ((child (%chat-panel-layout-child region-key layout)))
    (and default
         (if child
             (yaml-layout-child-visible child)
             t))))

(defun %chat-panel-fixed-height (region-key default &optional (layout *yaml-layout-loaded*))
  "Return the fixed height override for REGION-KEY, or DEFAULT."
  (let* ((child (%chat-panel-layout-child region-key layout))
         (height (and child (yaml-layout-child-height child))))
    (cond
      ((integerp height)
       (max 0 height))
      ((and (stringp height)
            (string= (string-downcase height) "content"))
       1)
      (t
       default))))

(defun %chat-panel-flex-weight (region-key default &optional (layout *yaml-layout-loaded*))
  "Return the fill weight override for REGION-KEY, or DEFAULT."
  (let* ((child (%chat-panel-layout-child region-key layout))
         (height (and child (yaml-layout-child-height child))))
    (if (and (stringp height)
             (member (string-downcase height) '("fill" "flex") :test #'string=))
        (or (yaml-layout-child-fill-weight child) default)
        default)))

(defun %chat-panel-prompt-border-style (&optional (layout *yaml-layout-loaded*))
  "Return the prompt box border style requested by the loaded YAML layout."
  (let* ((child (%chat-panel-layout-child :input layout))
         (border (and child (yaml-layout-child-border child))))
    (cond
      ((null border) :rounded)
      ((string= border "rounded") :rounded)
      ((member border '("single" "square" "ascii" "double") :test #'string=) :square)
      (t :rounded))))

(defun %chat-panel-input-content-rows (&optional (layout *yaml-layout-loaded*))
  "Convert the configured input panel height into prompt-box content rows."
  (max 1 (- (%chat-panel-fixed-height :input 3 layout) 2)))

(defun %chat-panel-history-viewport-height (inner-height
                                            &key
                                              provider-active-p
                                              tree-active-p
                                              plan-active-p
                                              handoff-visible-p
                                              approval-active-p
                                              picker-active-p)
  "Compute the actual allocated history region height for chat-panel."
  (let* ((constraints
           (remove nil
                   (list
                    (when (%chat-panel-layout-visible-p :provider provider-active-p)
                      (ptui.layout.constraints:fixed
                       'provider
                       (%chat-panel-fixed-height :provider 5)))
                    (when (%chat-panel-layout-visible-p :tree tree-active-p)
                      (ptui.layout.constraints:fixed
                       'tree
                       (%chat-panel-fixed-height :tree 10)))
                    (when (%chat-panel-layout-visible-p :plan plan-active-p)
                      (ptui.layout.constraints:fixed
                       'plan
                       (%chat-panel-fixed-height :plan 12)))
                    (when (%chat-panel-layout-visible-p :handoff handoff-visible-p)
                      (ptui.layout.constraints:fixed
                       'handoff
                       (%chat-panel-fixed-height :handoff 8)))
                    (ptui.layout.constraints:flex
                     'history
                     :weight (%chat-panel-flex-weight :history 1))
                    (when (%chat-panel-layout-visible-p :approval approval-active-p)
                      (ptui.layout.constraints:fixed
                       'approval
                       (%chat-panel-fixed-height :approval 8)))
                    (when (%chat-panel-layout-visible-p :picker picker-active-p)
                      (ptui.layout.constraints:fixed
                       'picker
                       (%chat-panel-fixed-height :picker 8)))
                    (ptui.layout.constraints:fixed
                     'input
                     (%chat-panel-fixed-height :input 3))
                    (ptui.layout.constraints:fixed
                     'status
                     (%chat-panel-fixed-height :status 1)))))
         (solved (ptui.layout.solver:solve-constraints constraints
                                                       (max 0 inner-height))))
    (max 0 (or (cdr (assoc 'history solved :test #'eq)) 0))))

(ptui.ui.panel:defpanel chat-panel (chat-state cols rows)
  (:data
    (inner-width (max 20 (- cols 2)) :deps (cols))
    (inner-height (max 8 (- rows 2)) :deps (rows))
    ;; NOTE: use-memo compares deps with EQUAL, and EQUAL on structs is EQ
    ;; (identity).  Since chat-state/approval-state/etc. are mutable structs
    ;; whose references never change, using them as deps would cache values
    ;; forever.  Instead, use the actual computed value as the dep so the memo
    ;; invalidates when the underlying field changes.
    (approval-state (chat-ui-state-approval-dialog-state chat-state) :deps (chat-state))
    (approval-active-p (approval-dialog-state-active-p approval-state)
      :deps ((approval-dialog-state-active-p approval-state)))
    (picker-state (%chat-sync-fuzzy-picker! chat-state) :deps (chat-state))
    (picker-active-p (fuzzy-picker-state-active-p picker-state)
      :deps ((fuzzy-picker-state-active-p picker-state)))
    (repl-state (chat-ui-state-repl-panel-state chat-state) :deps (chat-state))
    (repl-active-p (repl-state-active-p repl-state)
      :deps ((repl-state-active-p repl-state)))
    (tree-state (%ensure-chat-tree-browser-state chat-state) :deps (chat-state))
    (tree-active-p (and (typep tree-state 'tree-browser-state)
                        (tree-browser-state-active-p tree-state))
      :deps ((and (typep tree-state 'tree-browser-state)
                  (tree-browser-state-active-p tree-state))))
    (stream-active-p (token-stream-active-p (chat-ui-state-stream-state chat-state))
      :deps ((token-stream-active-p (chat-ui-state-stream-state chat-state))))
    (exit-warning-active-p (%chat-exit-warning-active-p chat-state)
      :deps (chat-state))
    (plan-state (current-plan-mode-state) :deps ((current-plan-mode-state)))
    (plan-widget (%chat-plan-presentation-widget plan-state chat-state) :deps (plan-state chat-state))
    (plan-active-p (not (null plan-widget)) :deps (plan-widget))
    (handoff-visible-p (worktree-handoff-dashboard-visible-p)
      :deps ((worktree-handoff-dashboard-visible-p)))
    (provider-visible-p (chat-ui-state-provider-dashboard-visible-p chat-state)
      :deps ((chat-ui-state-provider-dashboard-visible-p chat-state)))
    (history-viewport-height
      (%chat-panel-history-viewport-height
       inner-height
       :provider-active-p provider-visible-p
       :tree-active-p tree-active-p
       :plan-active-p plan-active-p
       :handoff-visible-p handoff-visible-p
       :approval-active-p approval-active-p
       :picker-active-p picker-active-p)
      :deps (inner-height provider-visible-p tree-active-p plan-active-p
             handoff-visible-p
             approval-active-p picker-active-p)))
  (:effects
    (sync-approval (%sync-pending-approval-dialog! chat-state)
      :deps (chat-state))
    (drain-streams (%drain-stream-events chat-state)
      :deps (chat-state))
    (stream-summary (%publish-status-bar-stream-summary-if-needed chat-state)
      :deps (chat-state))
    (budget-warning (%emit-stream-budget-warning-if-needed chat-state)
      :deps (chat-state))
    (idle-hooks (%run-chat-idle-hooks-if-needed)
      :deps ())
    (yaml-theme-watcher
      (when *yaml-theme-source-path*
        (%yaml-theme-poll-and-publish-if-changed chat-state))
      :deps (chat-state))
    (hot-patch-watcher
      (hot-patch-poll-once chat-state)
      :deps (chat-state))
    (auto-checkpoint
      (progn
        (maybe-auto-checkpoint
         :conversation (%ensure-chat-conversation-state chat-state)
         :config (%chat-config)
         :busy-p stream-active-p)
        nil)
      :deps (chat-state))
    (frame-count
      (progn (incf (chat-ui-state-frame-count chat-state)) nil)
      :deps (chat-state))
    (provider-refresh
      (when provider-visible-p (provider-health-refresh!))
      :deps (provider-visible-p)))
  (:layout
    (:column
      (provider :fixed (%chat-panel-fixed-height :provider 5)
        :when (%chat-panel-layout-visible-p :provider provider-visible-p)
        (provider-health-panel
         (list :entries (provider-health-entries)
               :updated-at (provider-health-last-updated-at))))
      (tree :fixed (%chat-panel-fixed-height :tree 10)
        :when (%chat-panel-layout-visible-p :tree tree-active-p)
        (make-tree-browser-widget tree-state))
      (plan :fixed (%chat-panel-fixed-height :plan 12)
        :when (%chat-panel-layout-visible-p :plan plan-active-p)
        plan-widget)
      (handoff :fixed (%chat-panel-fixed-height :handoff 8)
        :when (%chat-panel-layout-visible-p :handoff handoff-visible-p)
        (worktree-handoff-dashboard (list :limit 4)))
      (history :flex (%chat-panel-flex-weight :history 1)
        (build-message-history-widget chat-state inner-width history-viewport-height))
      (approval :fixed (%chat-panel-fixed-height :approval 8)
        :when (%chat-panel-layout-visible-p :approval approval-active-p)
        (%chat-approval-dialog-widget chat-state approval-state))
      (picker :fixed (%chat-panel-fixed-height :picker 8)
        :when (%chat-panel-layout-visible-p :picker picker-active-p)
        (make-fuzzy-picker-widget picker-state))
      (repl :fixed (%chat-panel-fixed-height :repl 12) :when repl-active-p
        (make-repl-panel-widget repl-state))
      (input :fixed (%chat-panel-fixed-height :input 3)
        (ptui.components.prompt-box:make-prompt-box-widget
         (chat-ui-state-input-text chat-state)
         :id :chat-input
         :min-width 18
         :max-width inner-width
         :min-rows (%chat-panel-input-content-rows)
         :max-rows (%chat-panel-input-content-rows)
         :scroll-offset (chat-ui-state-prompt-scroll-offset chat-state)
         :cursor-position (chat-ui-state-cursor-position chat-state)
         :cursor-visible-p t
         :border-style (%chat-panel-prompt-border-style)))
      (status :fixed (%chat-panel-fixed-height :status 1)
        (build-chat-status-bar-widget
         chat-state
         inner-width
         :exit-warning-active-p exit-warning-active-p))))
  (:keys
    (:mode :approval :when approval-active-p
      (:up (approval-dialog-handle-key! approval-state :up))
      (:down (approval-dialog-handle-key! approval-state :down))
      (:left (approval-dialog-handle-key! approval-state :left))
      (:right (approval-dialog-handle-key! approval-state :right))
      (:enter (approval-dialog-handle-key! approval-state :enter))
      (:escape (approval-dialog-handle-key! approval-state :escape))
      (:text (approval-dialog-handle-text!
               approval-state
               (ptui.core.events:key-event-text? ptui.ui.panel::event))))
    (:mode :picker :when picker-active-p
      (:up (chat-panel-handle-fuzzy-picker-key chat-state :up))
      (:down (chat-panel-handle-fuzzy-picker-key chat-state :down))
      (:pgup (chat-panel-handle-fuzzy-picker-key chat-state :pgup))
      (:pgdn (chat-panel-handle-fuzzy-picker-key chat-state :pgdn))
      (:escape (chat-panel-handle-fuzzy-picker-key chat-state :escape))
      (:enter (chat-panel-handle-fuzzy-picker-key chat-state :enter)))
    (:mode :repl :when repl-active-p
      (:enter (chat-panel-handle-repl-key chat-state :enter nil inner-width))
      (:escape (chat-panel-handle-repl-key chat-state :escape nil inner-width))
      (:backspace (chat-panel-handle-repl-key chat-state :backspace nil inner-width))
      (:ctrl-u (chat-panel-handle-repl-key chat-state :ctrl-u nil inner-width))
      (:text (chat-panel-handle-repl-key
               chat-state :text
               (ptui.core.events:key-event-text? ptui.ui.panel::event)
               inner-width)))
    (:mode :handoff :when (and handoff-visible-p
                               (zerop (length (chat-ui-state-input-text chat-state))))
      (:up (worktree-handoff-dashboard-move-selection -1))
      (:down (worktree-handoff-dashboard-move-selection 1))
      (:left (worktree-handoff-dashboard-cycle-action -1))
      (:right (worktree-handoff-dashboard-cycle-action 1))
      (:enter (worktree-handoff-dashboard-apply-selected-action))
      (:escape (dismiss-worktree-handoff-dashboard)))
    (:mode :tree :when (and tree-active-p
                            (zerop (length (chat-ui-state-input-text chat-state))))
      (:up (chat-panel-handle-tree-browser-key chat-state :up))
      (:down (chat-panel-handle-tree-browser-key chat-state :down))
      (:left (chat-panel-handle-tree-browser-key chat-state :left))
      (:right (chat-panel-handle-tree-browser-key chat-state :right))
      (:enter (chat-panel-handle-tree-browser-key chat-state :enter))
      (:escape (chat-panel-handle-tree-browser-key chat-state :escape)))
    (:mode :default
      (:text (chat-panel-handle-input-key
               chat-state
               :text
               (ptui.core.events:key-event-text? ptui.ui.panel::event)
               inner-width))
      (:enter (chat-panel-handle-input-key chat-state :enter nil inner-width))
      (:backspace (chat-panel-handle-input-key chat-state :backspace nil inner-width))
      (:delete (chat-panel-handle-input-key chat-state :delete nil inner-width))
      (:ctrl-j (chat-panel-handle-input-key chat-state :ctrl-j nil inner-width))
      (:tab (chat-panel-handle-input-key chat-state :tab nil inner-width))
      (:ctrl-p (chat-panel-handle-input-key chat-state :ctrl-p nil inner-width))
      (:ctrl-n (chat-panel-handle-input-key chat-state :ctrl-n nil inner-width))
      (:ctrl-r (chat-panel-handle-input-key chat-state :ctrl-r nil inner-width))
      (:ctrl-g (repl-state-toggle! repl-state))
      (:ctrl-a (chat-panel-handle-input-key chat-state :ctrl-a nil inner-width))
      (:ctrl-e (chat-panel-handle-input-key chat-state :ctrl-e nil inner-width))
      (:left (chat-panel-handle-input-key chat-state :left nil inner-width))
      (:right (chat-panel-handle-input-key chat-state :right nil inner-width))
      (:ctrl-left (chat-panel-handle-input-key chat-state :ctrl-left nil inner-width))
      (:ctrl-right (chat-panel-handle-input-key chat-state :ctrl-right nil inner-width))
      (:home (chat-panel-handle-input-key chat-state :home nil inner-width))
      (:end (chat-panel-handle-input-key chat-state :end nil inner-width))
      (:ctrl-w (chat-panel-handle-input-key chat-state :ctrl-w nil inner-width))
      (:ctrl-u (chat-panel-handle-input-key chat-state :ctrl-u nil inner-width))
      (:ctrl-k (chat-panel-handle-input-key chat-state :ctrl-k nil inner-width))
      (:up (let ((has-text (plusp (length (chat-ui-state-input-text chat-state)))))
             (amoebum::%scroll-debug-log "KEY :up has-text=~A input-len=~D"
                                          has-text
                                          (length (chat-ui-state-input-text chat-state)))
             (if has-text
                 (chat-panel-handle-input-key chat-state :up nil inner-width)
                 (chat-ui-scroll-history chat-state 3))))
      (:down (let ((has-text (plusp (length (chat-ui-state-input-text chat-state)))))
               (amoebum::%scroll-debug-log "KEY :down has-text=~A input-len=~D"
                                            has-text
                                            (length (chat-ui-state-input-text chat-state)))
               (if has-text
                   (chat-panel-handle-input-key chat-state :down nil inner-width)
                   (chat-ui-scroll-history chat-state -3))))
      (:pgup (chat-ui-scroll-history chat-state
               (max 5 (floor history-viewport-height 2))))
      (:pgdn (chat-ui-scroll-history chat-state
               (- (max 5 (floor history-viewport-height 2)))))
      (:ctrl-c (when stream-active-p
                  (token-stream-request-cancel
                   (chat-ui-state-stream-state chat-state))))
      (:escape (if stream-active-p
                   ;; I369: First try cancel, then force reset if stream might be stuck
                   (progn
                     (token-stream-request-cancel
                      (chat-ui-state-stream-state chat-state))
                     ;; Also try force reset as a safety net
                     (ignore-errors
                       (token-stream-force-reset-if-stuck
                        (chat-ui-state-stream-state chat-state)))
                     t)
                   (chat-panel-handle-input-key chat-state :escape nil inner-width))))))
