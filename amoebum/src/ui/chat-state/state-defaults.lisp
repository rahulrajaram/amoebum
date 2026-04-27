(in-package :amoebum)

;;; NXT-431: foundational chat-state defaults and initialization helpers.
;;; Keep the shared :amoebum package so downstream callers and tests keep the
;;; same symbols while ui/chat-state.lisp shrinks back to the coordinator role.

(defparameter +chat-role-order+ '("system" "user" "assistant" "tool"))
(defparameter +max-agentic-iterations+ 100)
(defparameter +context-compression-default-keep-last-turns+ 6)
(defparameter +context-compression-min-summarized-messages+ 2)
(defparameter +context-compression-max-summary-points+ 4)
(defparameter +context-compression-snippet-chars+ 96)
(defparameter +chat-plan-presentation-max-steps+ 6)
(defparameter +chat-plan-presentation-output-viewport-height+ 4)
(defparameter +chat-plan-command-preview-max-lines+ 8)
(defparameter +chat-plan-rationale-snippet-chars+ 160)
(defparameter +demo-max-ui-render-messages+ 4
  "Maximum number of transcript messages rendered in demo mode.
Keeps the perf harness focused on steady-state rendering instead of
replaying an ever-growing synthetic transcript.")
(defparameter +chat-plan-command-heads+
  '("git" "make" "timeout" "sbcl" "bash" "sh" "zsh"
    "python" "python3" "pytest" "uv" "go" "cargo"
    "npm" "pnpm" "yarn" "node" "npx"
    "rg" "grep" "sed" "awk" "find" "ls"
    "docker" "kubectl" "helm"))
(defparameter +chat-exit-confirm-window-ms+ 1500)
(defparameter +heap-monitor-snapshot-interval-seconds+ 30.0d0)
(defparameter +heap-monitor-stop-poll-seconds+ 0.1d0)

(defparameter *hook-idle-threshold-seconds* 60
  "Default idle threshold before :on-idle hooks fire.")
(defparameter *hook-last-activity-second* 0
  "Most recent second when user activity was observed in chat UI.")
(defparameter *hook-last-idle-notified-second* nil
  "Last idle-seconds value reported to :on-idle hooks.")
(defvar *chat-ui-state* nil
  "Current chat UI state for hook warning capture.")
(defvar *%styled-lines-cache*)
(defvar *%render-cache-owner* nil
  "Chat state instance that currently owns the shared render caches.")

(defun %make-chat-stream-tool-call-table ()
  (make-hash-table :test #'equal))

(defun %make-chat-stream-executed-table ()
  (make-hash-table :test #'equal))

(defun %make-chat-history-selection-table ()
  (make-hash-table :test #'equal))

(defstruct (chat-ui-state
            (:constructor make-chat-ui-state
                (&key (runtime (ptui.ui.runtime:make-runtime))
                      (messages '())
                      (input-text "")
                      (message-scrollback-lines 0)
                      (max-message-scrollback-lines 0)
                      (prompt-scroll-offset nil)
                      (stream-state (make-token-stream-state))
                      (stream-runner #'stream-pseudopod-chat)
                      (stream-client nil)
                      (stream-system-prompt +chat-stream-default-system-prompt+)
                      (stream-tools nil)
                      (stream-scroll-follow-p t)
                      (stream-markdown-renderer (make-streaming-markdown-renderer))
                      (stream-response-chunks '())
                      (status-bar-state (make-status-bar-state))
                      (provider-dashboard-visible-p nil)
                      (conversation nil)
                      (context-used-tokens 0)
                      (context-window-limit +default-context-window-limit+)
                      (fuzzy-picker-state (make-fuzzy-picker-state))
                      (history-search-active-p nil)
                      (history-search-original-input "")
                      (history-search-signature nil)
                      (history-selection-map (%make-chat-history-selection-table))
                      (tree-browser-state nil)
                      (stream-tool-calls (%make-chat-stream-tool-call-table))
                      (stream-executed-tool-call-keys (%make-chat-stream-executed-table))
                      (stream-event-journal (make-stream-event-journal))
                      (stream-turn-snapshot (pseudopod:make-stream-turn-snapshot))
                      (stream-completion-pending-p nil)
                      (stream-status-publish-key nil)
                      (frame-count 0)
                      (demo-mode-p nil)
                      (agentic-iteration-count 0)
                      (max-agentic-iterations-override nil)
                      (plan-selected-step-index nil)
                      (cursor-position nil)
                      (ctrl-c-quit-armed-at-ms nil)
                      (approval-dialog-state (make-approval-dialog-state))
                      (stream-thinking-content "")
                      (stream-thinking-visible-p t)
                      (hook-warnings nil))))
  runtime
  (messages '() :type list)
  (input-text "" :type string)
  (message-scrollback-lines 0 :type fixnum)
  (max-message-scrollback-lines 0 :type fixnum)
  (prompt-scroll-offset nil)
  (stream-state (make-token-stream-state) :type token-stream-state)
  (stream-runner #'stream-pseudopod-chat :type (or null function))
  (stream-client nil)
  (stream-system-prompt +chat-stream-default-system-prompt+ :type string)
  (stream-tools nil)
  (stream-scroll-follow-p t :type boolean)
  (stream-markdown-renderer (make-streaming-markdown-renderer)
                            :type streaming-markdown-renderer)
  (stream-response-chunks '() :type list)
  (status-bar-state (make-status-bar-state) :type status-bar-state)
  (provider-dashboard-visible-p nil :type boolean)
  (conversation nil)
  (context-used-tokens 0 :type integer)
  (context-window-limit +default-context-window-limit+ :type integer)
  (fuzzy-picker-state (make-fuzzy-picker-state))
  (history-search-active-p nil :type boolean)
  (history-search-original-input "" :type string)
  (history-search-signature nil)
  (history-selection-map (%make-chat-history-selection-table))
  (tree-browser-state nil)
  (stream-tool-calls (%make-chat-stream-tool-call-table))
  (stream-executed-tool-call-keys (%make-chat-stream-executed-table))
  (stream-event-journal (make-stream-event-journal) :type stream-event-journal)
  (stream-turn-snapshot (pseudopod:make-stream-turn-snapshot)
                        :type pseudopod:stream-turn-snapshot)
  (stream-completion-pending-p nil)
  (stream-status-publish-key nil)
  (frame-count 0 :type fixnum)
  (demo-mode-p nil :type boolean)
  (agentic-iteration-count 0 :type fixnum)
  (max-agentic-iterations-override nil :type (or null fixnum))
  plan-selected-step-index
  (cursor-position nil)
  (ctrl-c-quit-armed-at-ms nil)
  (approval-dialog-state (make-approval-dialog-state) :type approval-dialog-state)
  (stream-thinking-content "" :type string)
  (stream-thinking-visible-p t :type boolean)
  (hook-warnings nil :type list))

(defun %ensure-chat-ui-runtime! (chat-state)
  (when (null (chat-ui-state-runtime chat-state))
    (setf (chat-ui-state-runtime chat-state) (ptui.ui.runtime:make-runtime)))
  chat-state)

(defun %ensure-chat-ui-stream-kernel! (chat-state)
  (unless (typep (chat-ui-state-stream-state chat-state) 'token-stream-state)
    (setf (chat-ui-state-stream-state chat-state) (make-token-stream-state)))
  (unless (or (null (chat-ui-state-stream-runner chat-state))
              (functionp (chat-ui-state-stream-runner chat-state)))
    (setf (chat-ui-state-stream-runner chat-state) #'stream-pseudopod-chat))
  (unless (and (stringp (chat-ui-state-stream-system-prompt chat-state))
               (plusp (length (chat-ui-state-stream-system-prompt chat-state))))
    (setf (chat-ui-state-stream-system-prompt chat-state)
          +chat-stream-default-system-prompt+))
  (unless (typep (chat-ui-state-stream-scroll-follow-p chat-state) 'boolean)
    (setf (chat-ui-state-stream-scroll-follow-p chat-state) t))
  (unless (typep (chat-ui-state-stream-markdown-renderer chat-state)
                 'streaming-markdown-renderer)
    (setf (chat-ui-state-stream-markdown-renderer chat-state)
          (make-streaming-markdown-renderer)))
  (unless (listp (chat-ui-state-stream-response-chunks chat-state))
    (setf (chat-ui-state-stream-response-chunks chat-state) '()))
  (unless (hash-table-p (chat-ui-state-stream-tool-calls chat-state))
    (setf (chat-ui-state-stream-tool-calls chat-state)
          (%make-chat-stream-tool-call-table)))
  (unless (hash-table-p (chat-ui-state-stream-executed-tool-call-keys chat-state))
    (setf (chat-ui-state-stream-executed-tool-call-keys chat-state)
          (%make-chat-stream-executed-table)))
  (unless (stream-event-journal-p (chat-ui-state-stream-event-journal chat-state))
    (setf (chat-ui-state-stream-event-journal chat-state)
          (make-stream-event-journal)))
  (unless (typep (chat-ui-state-stream-turn-snapshot chat-state)
                 'pseudopod:stream-turn-snapshot)
    (setf (chat-ui-state-stream-turn-snapshot chat-state)
          (pseudopod:make-stream-turn-snapshot)))
  chat-state)

(defun %ensure-chat-ui-history-kernel! (chat-state)
  (unless (typep (chat-ui-state-history-search-active-p chat-state) 'boolean)
    (setf (chat-ui-state-history-search-active-p chat-state) nil))
  (unless (stringp (chat-ui-state-history-search-original-input chat-state))
    (setf (chat-ui-state-history-search-original-input chat-state) ""))
  (unless (hash-table-p (chat-ui-state-history-selection-map chat-state))
    (setf (chat-ui-state-history-selection-map chat-state)
          (%make-chat-history-selection-table)))
  chat-state)

(defun %ensure-chat-ui-misc-kernel! (chat-state)
  (unless (typep (chat-ui-state-demo-mode-p chat-state) 'boolean)
    (setf (chat-ui-state-demo-mode-p chat-state) nil))
  (setf (chat-ui-state-status-bar-state chat-state)
        (ensure-status-bar-state
         (chat-ui-state-status-bar-state chat-state)
         :event-bus (current-event-bus)))
  chat-state)

(defun %chat-config ()
  (ignore-errors (current-config)))

(defun %chat-effective-max-iterations (chat-state)
  "Return the effective max agentic iterations for CHAT-STATE.
Uses the per-conversation override if set, otherwise the global default."
  (or (chat-ui-state-max-agentic-iterations-override chat-state)
      +max-agentic-iterations+))

(defun %chat-existing-directory (candidate)
  (let* ((directory (and candidate
                         (ignore-errors (uiop:ensure-directory-pathname candidate))))
         (resolved (and directory
                        (or (ignore-errors (truename directory))
                            directory))))
    (when (and resolved (probe-file resolved))
      resolved)))

(defun %chat-project-root ()
  (let* ((cfg (%chat-config))
         (config-root (and (config-p cfg)
                           (config-project-root cfg)))
         (cwd (ignore-errors (uiop:getcwd)))
         (default *default-pathname-defaults*)
         (fallback (or config-root cwd default)))
    (or (%chat-existing-directory config-root)
        (%chat-existing-directory cwd)
        (%chat-existing-directory default)
        (uiop:ensure-directory-pathname fallback))))

(defun %whitespace-char-p (char)
  (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=))

(defun %normalize-inline-text (text)
  (let ((value (if (stringp text)
                   text
                   (princ-to-string (or text "")))))
    (with-output-to-string (out)
      (let ((previous-space-p nil))
        (loop for char across value do
          (if (%whitespace-char-p char)
              (unless previous-space-p
                (write-char #\Space out)
                (setf previous-space-p t))
              (progn
                (write-char char out)
                (setf previous-space-p nil))))))))

(defun %truncate-inline-text (text limit)
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (%normalize-inline-text text)))
         (length (length trimmed)))
    (cond
      ((<= length (max 0 limit))
       trimmed)
      ((<= (max 0 limit) 3)
       (subseq trimmed 0 (max 0 limit)))
      (t
       (concatenate 'string
                    (subseq trimmed 0 (- limit 3))
                    "...")))))

(defun %content-part-text (part)
  (let ((type (string-downcase (or (pseudopod:content-part-type part) "text"))))
    (cond
      ((string= type "text")
       (or (pseudopod:content-part-text part) ""))
      ((string= type "think")
       (or (pseudopod:content-part-think part) ""))
      (t
       (or (pseudopod:content-part-text part)
           (pseudopod:content-part-think part)
           "")))))

(defun %message-content->text (message)
  (let ((parts (pseudopod:message-content message)))
    (if (null parts)
        ""
        (with-output-to-string (out)
          (loop for part in parts
                for index from 0 do
                  (when (> index 0)
                    (write-char #\Newline out))
                  (write-string (%content-part-text part) out))))))

(defun %ensure-chat-ui-state-kernel! (chat-state)
  (%ensure-chat-ui-runtime! chat-state)
  (%ensure-chat-ui-stream-kernel! chat-state)
  (%ensure-chat-ui-history-kernel! chat-state)
  (%ensure-chat-ui-misc-kernel! chat-state)
  chat-state)

(defun %initialize-chat-ui-state-supports! (chat-state)
  (%ensure-chat-fuzzy-picker-state chat-state)
  (%ensure-chat-tree-browser-state chat-state)
  (%chat-sync-fuzzy-picker! chat-state :step-p nil)
  (%ensure-chat-conversation-state chat-state)
  (when (fboundp 'apply-yaml-layout-to-chat)
    (apply-yaml-layout-to-chat chat-state))
  (%sync-chat-context-usage! chat-state)
  chat-state)

(defun ensure-chat-ui-state (state)
  (let ((chat-state
          (if (and state (typep state 'chat-ui-state))
              state
              (make-chat-ui-state :runtime (ptui.ui.runtime:make-runtime)))))
    (unless (eq chat-state *%render-cache-owner*)
      (setf *%render-cache-owner* chat-state)
      (%invalidate-styled-lines-cache))
    (%ensure-chat-ui-state-kernel! chat-state)
    (%initialize-chat-ui-state-supports! chat-state)))
