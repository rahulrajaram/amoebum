(in-package :amoebum)

;;; Forward declaration - defined in prompt-input.lisp which is loaded before this file
(declaim (ftype function chat-panel-handle-input-key))
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
                      (agentic-iteration-count 0)
                      (max-agentic-iterations-override nil)
                      (plan-selected-step-index nil)
                      (cursor-position nil)
                      (ctrl-c-quit-armed-at-ms nil)
                      (approval-dialog-state (make-approval-dialog-state))
                      ;; Kimi K2.5 thinking overlay state
                      (stream-thinking-content "")
                      (stream-thinking-visible-p t)
                      ;; Hook warnings capture
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
  (agentic-iteration-count 0 :type fixnum)
  (max-agentic-iterations-override nil :type (or null fixnum))
  plan-selected-step-index
  (cursor-position nil)
  (ctrl-c-quit-armed-at-ms nil)
  (approval-dialog-state (make-approval-dialog-state) :type approval-dialog-state)
  ;; Kimi K2.5 thinking overlay state
  (stream-thinking-content "" :type string)
  (stream-thinking-visible-p t :type boolean)
  (hook-warnings nil :type list))

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
         (content (%conversation-trim (conversation-history-entry-content entry)))
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

(defun %chat-model-name (chat-state config)
  (or (and (config-p config) (config-model config))
      (and (typep (chat-ui-state-status-bar-state chat-state) 'status-bar-state)
           (status-bar-state-model-name (chat-ui-state-status-bar-state chat-state)))
      "unknown"))

(defun %chat-context-window-limit (model config)
  (resolve-context-window-limit
   :model model
   :config-limit (and (config-p config)
                      (config-value :context-window-limit config))))

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

(defun %count-role-occurrences (messages)
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (message messages)
      (let ((role (string-downcase (or (pseudopod:message-role message) "assistant"))))
        (incf (gethash role counts 0))))
    counts))

(defun %summary-sample-indexes (count)
  (let* ((max-count +context-compression-max-summary-points+)
         (base (cond
                 ((<= count max-count)
                  (loop for index from 0 below count collect index))
                 (t
                  (list 0
                        (truncate (/ count 3))
                        (truncate (* 2 (/ count 3)))
                        (1- count)))))
         (unique '()))
    (dolist (index base)
      (when (and (integerp index)
                 (>= index 0)
                 (< index count)
                 (not (member index unique :test #'=)))
        (push index unique)))
    (sort unique #'<)))

(defun %summary-message-text (message)
  (%truncate-inline-text
   (%message-content->text message)
   +context-compression-snippet-chars+))

(defun %compression-summary-text (messages)
  (let* ((count (length messages))
         (roles (%count-role-occurrences messages))
         (role-summary
           (format nil "Roles: user=~D assistant=~D system=~D tool=~D."
                   (gethash "user" roles 0)
                   (gethash "assistant" roles 0)
                   (gethash "system" roles 0)
                   (gethash "tool" roles 0)))
         (sample-indexes (%summary-sample-indexes count)))
    (with-output-to-string (out)
      (format out "Compressed summary of ~D earlier messages.~%~A~%"
              count
              role-summary)
      (when sample-indexes
        (format out "Highlights:~%")
        (dolist (index sample-indexes)
          (let* ((message (nth index messages))
                 (role (string-upcase (or (pseudopod:message-role message) "assistant")))
                 (snippet (%summary-message-text message)))
            (format out "- ~A: ~A~%" role snippet)))))))

(defun %keep-last-message-count (keep-last-turns)
  (* 2 (max 1
            (if (and (integerp keep-last-turns) (> keep-last-turns 0))
                keep-last-turns
                +context-compression-default-keep-last-turns+))))

(defun %context-event-bus (chat-state)
  (or (and (typep (chat-ui-state-status-bar-state chat-state) 'status-bar-state)
           (status-bar-state-event-bus (chat-ui-state-status-bar-state chat-state)))
      (current-event-bus)))

(defun %compress-chat-history! (chat-state
                                &key
                                  (keep-last-turns +context-compression-default-keep-last-turns+)
                                  (trigger :auto))
  (let* ((config (%chat-config))
         (model (%chat-model-name chat-state config))
         (messages (chat-ui-state-messages chat-state))
         (before-tokens (count-tokens messages :model model))
         (keep-last-messages (%keep-last-message-count keep-last-turns))
         (drop-count (- (length messages) keep-last-messages)))
    (when (< drop-count +context-compression-min-summarized-messages+)
      (return-from %compress-chat-history!
        (list :compressed-p nil
              :before-tokens before-tokens
              :after-tokens before-tokens
              :saved-tokens 0
              :summarized-messages 0
              :kept-messages (length messages)
              :keep-last-turns keep-last-turns
              :trigger trigger)))
    (let* ((old-prefix (subseq messages 0 drop-count))
           (tail (copy-list (nthcdr drop-count messages)))
           (summary-text (%compression-summary-text old-prefix))
           (summary-message (make-chat-message "system" summary-text))
           (next-messages (cons summary-message tail))
           (after-tokens (count-tokens next-messages :model model))
           (saved-tokens (max 0 (- before-tokens after-tokens)))
           (result (list :compressed-p t
                         :before-tokens before-tokens
                         :after-tokens after-tokens
                         :saved-tokens saved-tokens
                         :summarized-messages drop-count
                         :kept-messages (length tail)
                         :keep-last-turns keep-last-turns
                         :trigger trigger)))
      (setf (chat-ui-state-messages chat-state) next-messages
            (chat-ui-state-message-scrollback-lines chat-state) 0)
      (%invalidate-styled-lines-cache)
      (%sync-chat-context-usage! chat-state :allow-auto-compress-p nil)
      (publish (%context-event-bus chat-state)
               (make-context-compressed-event
                :before-tokens before-tokens
                :after-tokens after-tokens
                :saved-tokens saved-tokens
                :summarized-messages drop-count
                :kept-messages (length tail)
                :trigger (if (keywordp trigger)
                             trigger
                             :auto)))
      result)))

(defun %sync-chat-context-usage! (chat-state &key (allow-auto-compress-p t))
  (let* ((config (%chat-config))
         (model (%chat-model-name chat-state config))
         (limit (%chat-context-window-limit model config))
         (used (count-tokens (chat-ui-state-messages chat-state) :model model))
         (status-state (chat-ui-state-status-bar-state chat-state)))
    (setf (chat-ui-state-context-used-tokens chat-state) used
          (chat-ui-state-context-window-limit chat-state) limit)
    (when (typep status-state 'status-bar-state)
      (setf (status-bar-state-model-name status-state) model
            (status-bar-state-context-used-tokens status-state) used
            (status-bar-state-context-max-tokens status-state) limit))
    (when (and allow-auto-compress-p
               (context-compression-required-p used limit)
               (>= (- (length (chat-ui-state-messages chat-state))
                      (%keep-last-message-count +context-compression-default-keep-last-turns+))
                   +context-compression-min-summarized-messages+))
      (let ((compression
              (%compress-chat-history! chat-state
                                       :keep-last-turns +context-compression-default-keep-last-turns+
                                       :trigger :auto)))
        (when (getf compression :compressed-p)
          (setf used (getf compression :after-tokens)))))
    used))

(defun %sync-chat-context-usage-append! (chat-state new-message
                                          &key (allow-auto-compress-p t))
  (let* ((config (%chat-config))
         (model (%chat-model-name chat-state config))
         (limit (%chat-context-window-limit model config))
         (old-used (or (chat-ui-state-context-used-tokens chat-state) 0))
         (used (+ old-used (count-tokens new-message :model model)))
         (status-state (chat-ui-state-status-bar-state chat-state)))
    (setf (chat-ui-state-context-used-tokens chat-state) used
          (chat-ui-state-context-window-limit chat-state) limit)
    (when (typep status-state 'status-bar-state)
      (setf (status-bar-state-model-name status-state) model
            (status-bar-state-context-used-tokens status-state) used
            (status-bar-state-context-max-tokens status-state) limit))
    (when (and allow-auto-compress-p
               (context-compression-required-p used limit)
               (>= (- (length (chat-ui-state-messages chat-state))
                      (%keep-last-message-count +context-compression-default-keep-last-turns+))
                   +context-compression-min-summarized-messages+))
      (return-from %sync-chat-context-usage-append!
        (%sync-chat-context-usage! chat-state :allow-auto-compress-p t)))
    used))

(defun %sync-chat-context-usage-replacement! (chat-state old-message new-message
                                               &key (allow-auto-compress-p t))
  (let* ((config (%chat-config))
         (model (%chat-model-name chat-state config))
         (limit (%chat-context-window-limit model config))
         (old-used (or (chat-ui-state-context-used-tokens chat-state) 0))
         (used (+ (max 0 (- old-used (count-tokens old-message :model model)))
                  (count-tokens new-message :model model)))
         (status-state (chat-ui-state-status-bar-state chat-state)))
    (setf (chat-ui-state-context-used-tokens chat-state) used
          (chat-ui-state-context-window-limit chat-state) limit)
    (when (typep status-state 'status-bar-state)
      (setf (status-bar-state-model-name status-state) model
            (status-bar-state-context-used-tokens status-state) used
            (status-bar-state-context-max-tokens status-state) limit))
    (when (and allow-auto-compress-p
               (context-compression-required-p used limit)
               (>= (- (length (chat-ui-state-messages chat-state))
                      (%keep-last-message-count +context-compression-default-keep-last-turns+))
                   +context-compression-min-summarized-messages+))
      (return-from %sync-chat-context-usage-replacement!
        (%sync-chat-context-usage! chat-state :allow-auto-compress-p t)))
    used))

(defun %sync-all-state! (chat-state)
  "Single sync point: calls each state-sync function exactly once in the
correct order.  Replaces the previous scattered sync calls in
render-chat-ui-buffer, handle-chat-ui-event, and defpanel effects."
  (%inject-agent-completions chat-state)
  (%inject-voice-transcriptions chat-state)
  ;; I369: Check for and recover from stuck streams before draining events
  (%recover-stuck-stream-if-needed! chat-state)
  (%drain-stream-events chat-state)
  (%sync-pending-approval-dialog! chat-state)
  (%publish-status-bar-stream-summary-if-needed chat-state)
  (%emit-stream-budget-warning-if-needed chat-state)
  (%chat-sync-fuzzy-picker! chat-state)
  (%run-chat-idle-hooks-if-needed))

(defun %recover-stuck-stream-if-needed! (chat-state)
  "Check if the stream is stuck and force reset it if necessary.
This is a safety net to prevent the UI from becoming unresponsive."
  (let ((stream-state (chat-ui-state-stream-state chat-state)))
    (when (token-stream-force-reset-if-stuck stream-state)
      (log-runtime-event :level :warn
                         :kind "stream-stuck-recovery"
                         :source :chat-ui
                         :message "Recovered from stuck stream state."
                         :details nil)
      ;; Transition conversation back to idle so input is unblocked
      (ignore-errors
        (conversation-transition! (%ensure-chat-conversation-state chat-state)
                                  :idle)))))

(defun ensure-chat-ui-state (state)
  (let ((chat-state
          (if (and state (typep state 'chat-ui-state))
              state
              (make-chat-ui-state :runtime (ptui.ui.runtime:make-runtime)))))
    (unless (eq chat-state *%render-cache-owner*)
      (setf *%render-cache-owner* chat-state)
      (%invalidate-styled-lines-cache))
    (when (null (chat-ui-state-runtime chat-state))
      (setf (chat-ui-state-runtime chat-state) (ptui.ui.runtime:make-runtime)))
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
    (unless (typep (chat-ui-state-history-search-active-p chat-state) 'boolean)
      (setf (chat-ui-state-history-search-active-p chat-state) nil))
    (unless (stringp (chat-ui-state-history-search-original-input chat-state))
      (setf (chat-ui-state-history-search-original-input chat-state) ""))
    (unless (hash-table-p (chat-ui-state-history-selection-map chat-state))
      (setf (chat-ui-state-history-selection-map chat-state)
            (%make-chat-history-selection-table)))
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
    (setf (chat-ui-state-status-bar-state chat-state)
          (ensure-status-bar-state
           (chat-ui-state-status-bar-state chat-state)
           :event-bus (current-event-bus)))
    (%ensure-chat-fuzzy-picker-state chat-state)
    (%ensure-chat-tree-browser-state chat-state)
    (%chat-sync-fuzzy-picker! chat-state :step-p nil)
    (%ensure-chat-conversation-state chat-state)
    (when (fboundp 'apply-yaml-layout-to-chat)
      (apply-yaml-layout-to-chat chat-state))
    (%sync-chat-context-usage! chat-state)
    chat-state))

(defun chat-ui-restore-latest-session (&optional state)
  (let* ((chat-state (ensure-chat-ui-state state))
         (project-root (%chat-project-root))
         (restored (conversation-load-latest :project-root project-root)))
    (when (typep restored 'conversation-state)
      (setf (chat-ui-state-conversation chat-state) restored
            (chat-ui-state-messages chat-state) (conversation-state-messages restored)
            (chat-ui-state-message-scrollback-lines chat-state) 0
            (chat-ui-state-max-message-scrollback-lines chat-state) 0)
      (%invalidate-styled-lines-cache)
      (%sync-chat-context-usage! chat-state :allow-auto-compress-p nil))
    chat-state))

(defun %normalize-chat-role (role)
  (let ((normalized
          (string-downcase
           (cond
             ((stringp role) role)
             ((symbolp role) (symbol-name role))
             (t "assistant")))))
    (if (member normalized +chat-role-order+ :test #'string=)
        normalized
        "assistant")))

(defun make-chat-message (role content &key name tool-calls partial)
  (pseudopod:make-message
   :role (%normalize-chat-role role)
   :content content
   :name name
   :tool-calls tool-calls
   :partial partial))

(defun chat-ui-append-message (state message)
  (check-type message pseudopod:message)
  (let ((chat-state (ensure-chat-ui-state state)))
    (conversation-state-add-message (%ensure-chat-conversation-state chat-state)
                                    message
                                    :save-p t)
    (setf (chat-ui-state-messages chat-state)
          (append (chat-ui-state-messages chat-state)
                  (list message)))
    ;; Only auto-scroll to bottom when the user was already at the bottom
    ;; or when scroll-follow is active during streaming.  This preserves the
    ;; user's scroll position when they have scrolled up to read history.
    (when (or (zerop (chat-ui-state-message-scrollback-lines chat-state))
              (chat-ui-state-stream-scroll-follow-p chat-state))
      (setf (chat-ui-state-message-scrollback-lines chat-state) 0))
    (%sync-chat-context-usage-append! chat-state message)
    message))

(defun chat-ui-add-message (state role content &key name tool-calls partial)
  (chat-ui-append-message
   state
   (make-chat-message role content
                      :name name
                      :tool-calls tool-calls
                      :partial partial)))

(defun chat-ui-set-input (state text &key cursor-position)
  "Set the input text. CURSOR-POSITION: nil = end, integer = grapheme index."
  (let ((chat-state (ensure-chat-ui-state state)))
    (setf (chat-ui-state-input-text chat-state)
          (if (stringp text)
              text
              (princ-to-string text))
          (chat-ui-state-prompt-scroll-offset chat-state) nil
          (chat-ui-state-cursor-position chat-state) cursor-position)
    (%chat-sync-fuzzy-picker! chat-state :step-p nil)
    (chat-ui-state-input-text chat-state)))

(defun %blank-string-p (text)
  (every (lambda (char)
           (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=))
         text))

(defun %chat-push-input-text-part (parts text)
  (if (and (stringp text) (plusp (length text)))
      (cons (pseudopod:make-text-part text) parts)
      parts))

(defun %chat-parse-input-content-parts (input)
  (let ((source (if (stringp input) input (princ-to-string input)))
        (cursor 0)
        (parts '()))
    (labels ((emit-text (from to)
               (when (< from to)
                 (setf parts (%chat-push-input-text-part parts (subseq source from to))))))
      (loop
        for marker = (search "![" source :start2 cursor)
        while marker do
          (let* ((alt-end (position #\] source :start (+ marker 2)))
                 (open-paren (and alt-end
                                  (< (1+ alt-end) (length source))
                                  (char= (char source (1+ alt-end)) #\()
                                  (1+ alt-end)))
                 (close-paren (and open-paren
                                   (position #\) source :start (1+ open-paren)))))
            (unless (and alt-end open-paren close-paren)
              (loop-finish))
            (emit-text cursor marker)
            (let ((path (string-trim '(#\Space #\Tab #\Newline #\Return)
                                     (subseq source (+ open-paren 1) close-paren))))
              (if (plusp (length path))
                  (push (%make-image-content-part path) parts)
                  (emit-text marker (1+ close-paren))))
            (setf cursor (1+ close-paren))))
      (emit-text cursor (length source)))
    (nreverse parts)))

(defun chat-ui-submit-input (state)
  (let* ((chat-state (ensure-chat-ui-state state))
         (conversation (%ensure-chat-conversation-state chat-state))
         (input (chat-ui-state-input-text chat-state)))
    (setf (chat-ui-state-agentic-iteration-count chat-state) 0)
    (if (or (null input) (zerop (length input)) (%blank-string-p input))
        nil
        (handler-case
            (let ((content (%chat-parse-input-content-parts input)))
              (if (null content)
                  nil
                  (prog1
                      (progn
                        (conversation-transition! conversation :user-input)
                        (chat-ui-add-message chat-state "user" content))
                    (setf (chat-ui-state-input-text chat-state) ""
                          (chat-ui-state-prompt-scroll-offset chat-state) nil)
                    (fuzzy-picker-deactivate! (%ensure-chat-fuzzy-picker-state chat-state)))))
          (error (condition)
            (chat-ui-add-message
             chat-state
             "system"
             (format nil "Unable to attach image input: ~A" condition))
            nil)))))

(defun %scroll-debug-enabled-p ()
  "Read AMOEBUM_SCROLL_DEBUG at runtime.

Buildapp dumps top-level variable values into the saved image, so this must
not be cached in a DEFVAR if we want toggles from the shell to affect the
launched binary."
  (let ((value (uiop:getenv "AMOEBUM_SCROLL_DEBUG")))
    (and value
         (not (string= value ""))
         (not (member (string-downcase value)
                      '("0" "false" "no" "off")
                      :test #'string=)))))

(defun %scroll-debug-log (fmt &rest args)
  (when (%scroll-debug-enabled-p)
    (ignore-errors
      (with-open-file (out "/tmp/amoebum-scroll-debug.log"
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create)
        (format out "~A ~?~%" (get-universal-time) fmt args)))))

(defun chat-ui-scroll-history (state delta-lines)
  (let* ((chat-state (ensure-chat-ui-state state))
         (stream-active-p (token-stream-active-p (chat-ui-state-stream-state chat-state)))
         (max-scrollback (max 0 (chat-ui-state-max-message-scrollback-lines chat-state)))
         (before-scrollback (chat-ui-state-message-scrollback-lines chat-state))
         (next-scrollback (+ before-scrollback (or delta-lines 0))))
    (setf (chat-ui-state-message-scrollback-lines chat-state)
          (max 0 (min max-scrollback next-scrollback)))
    ;; When user explicitly scrolls up (positive delta), disengage
    ;; scroll-follow so that message appends no longer reset scrollback
    ;; to 0.  This must happen regardless of stream state: scroll-follow-p
    ;; persists as T after streaming finishes, and without this clear,
    ;; any subsequent chat-ui-append-message call would fight the user.
    (when (and (> (or delta-lines 0) 0)
               (> (chat-ui-state-message-scrollback-lines chat-state) 0))
      (setf (chat-ui-state-stream-scroll-follow-p chat-state) nil))
    (when stream-active-p
      (cond
        ((> (chat-ui-state-message-scrollback-lines chat-state) 0)
         (setf (chat-ui-state-stream-scroll-follow-p chat-state) nil))
        (t
         (setf (chat-ui-state-stream-scroll-follow-p chat-state) t))))
    (%scroll-debug-log "SCROLL delta=~D before=~D max=~D next=~D after=~D stream=~A follow=~A"
                       delta-lines before-scrollback max-scrollback next-scrollback
                       (chat-ui-state-message-scrollback-lines chat-state)
                       stream-active-p
                       (chat-ui-state-stream-scroll-follow-p chat-state))
    (chat-ui-state-message-scrollback-lines chat-state)))

(defun chat-role-prefix (role)
  (case (intern (string-upcase (%normalize-chat-role role)) :keyword)
    (:tool "")
    (otherwise "")))

(defun %chat-disarm-ctrl-c-quit! (chat-state)
  (setf (chat-ui-state-ctrl-c-quit-armed-at-ms chat-state) nil)
  chat-state)

(defun %chat-ctrl-c-quit-armed-p (chat-state &key (now-ms (ptui.util.time:monotonic-ms)))
  (let ((armed-at (chat-ui-state-ctrl-c-quit-armed-at-ms chat-state)))
    (when armed-at
      (if (<= (- now-ms armed-at) +chat-exit-confirm-window-ms+)
          t
          (progn
            (%chat-disarm-ctrl-c-quit! chat-state)
            nil)))))

(defun %chat-arm-ctrl-c-quit! (chat-state &key (now-ms (ptui.util.time:monotonic-ms)))
  (setf (chat-ui-state-ctrl-c-quit-armed-at-ms chat-state) now-ms)
  chat-state)

(defun %chat-exit-warning-active-p (chat-state)
  (%chat-ctrl-c-quit-armed-p chat-state))

(defun %chat-exit-warning-text ()
  (let ((seconds-text
          (if (zerop (mod +chat-exit-confirm-window-ms+ 1000))
              (format nil "~D" (truncate (/ +chat-exit-confirm-window-ms+ 1000)))
              (format nil "~,1f" (/ +chat-exit-confirm-window-ms+ 1000.0)))))
    (format nil "Press Ctrl-C again within ~A seconds to exit."
            seconds-text)))

(defun %chat-sleep-until-stop (stop-predicate
                               &key
                                 (seconds +heap-monitor-snapshot-interval-seconds+)
                                 (poll-seconds +heap-monitor-stop-poll-seconds+))
  (let ((remaining (float (max 0 seconds) 1d0))
        (poll (float (max 0.01d0 poll-seconds) 1d0)))
    (loop
      when (funcall stop-predicate) do (return t)
      when (<= remaining 0d0) do (return nil)
      do (let ((sleep-seconds (min remaining poll)))
           (sleep sleep-seconds)
           (decf remaining sleep-seconds)))))

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

(defun %replace-message-at-index! (messages index message)
  (let ((cell (nthcdr index messages)))
    (when cell
      (setf (car cell) message)
      t)))

(defun %stream-tool-call-preview-key (index tool-call-id tool-name arguments)
  (cond
    ((and (stringp tool-call-id) (plusp (length tool-call-id)))
     (concatenate 'string "id:" tool-call-id))
    ((and (stringp tool-name) (plusp (length tool-name)))
     (concatenate 'string "name:" tool-name))
    ((and (stringp arguments) (plusp (length arguments)))
     (concatenate 'string "args:" arguments))
    ((integerp index) index)
    (t
     :unknown)))

(defun %ensure-stream-tool-call-preview (chat-state key &optional index)
  (let* ((table (chat-ui-state-stream-tool-calls chat-state))
         (entry (and (hash-table-p table) (gethash key table)))
         (stable-entry
           (or entry
               (and (hash-table-p table)
                    (integerp index)
                    (not (eq key index))
                    (gethash index table)))))
    (if (and stable-entry (not entry) (integerp index) (not (eq key index)))
        (progn
          (remhash index table)
          (setf (gethash key table) stable-entry)
          stable-entry)
        (or entry
            (let ((fresh (list :key key
                               :index nil
                               :tool-name nil
                               :tool-call-id nil
                               :arguments nil
                               :started-p nil
                               :arguments-complete-p nil
                               :executed-p nil
                               :completed-p nil
                               :execution-error nil
                               :result nil
                               :malformed-p nil)))
              (setf (gethash key table) fresh)
              fresh)))))

(defun %find-stream-tool-call-preview (chat-state key &optional index)
  (let ((table (chat-ui-state-stream-tool-calls chat-state)))
    (and (hash-table-p table)
         (or (gethash key table)
             (and (integerp index)
                  (not (eq key index))
                  (gethash index table))))))

(defun %normalize-stream-tool-name (tool-name)
  (let ((value (if (symbolp tool-name)
                   (symbol-name tool-name)
                   tool-name)))
    (and (stringp value)
         (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                      (string-downcase value)))
                (normalized (if (find #\_ trimmed) (substitute #\- #\_ trimmed)
                              trimmed)))
           (and (plusp (length normalized))
                normalized)))))

(defun %normalize-stream-tool-call (tool-call)
  (if (pseudopod:tool-call-p tool-call)
      (let ((normalized-name (%normalize-stream-tool-name
                             (pseudopod:tool-call-name tool-call)))
            (name (pseudopod:tool-call-name tool-call)))
        (if (and (stringp normalized-name)
                 (not (string= name normalized-name)))
            (pseudopod:make-tool-call
             :id (pseudopod:tool-call-id tool-call)
             :name normalized-name
             :arguments (pseudopod:tool-call-arguments tool-call)
             :extras (pseudopod:tool-call-extras tool-call))
            tool-call))
      nil))

(defun %stream-tool-call-from-event (event)
  (let ((tool-call (getf event :tool-call)))
    (if (pseudopod:tool-call-p tool-call)
        (%normalize-stream-tool-call tool-call)
        (let* ((tool-name (getf event :tool-name))
               (normalized-name (%normalize-stream-tool-name tool-name))
               (arguments (getf event :arguments))
               (tool-call-id (getf event :tool-call-id)))
          (when (and (stringp normalized-name) (plusp (length normalized-name)))
            (pseudopod:make-tool-call
             :id (and (stringp tool-call-id) tool-call-id)
             :name normalized-name
             :arguments (and (stringp arguments) arguments)))))))

(defun %stream-tool-call-preview-signature (chat-state)
  (let (items)
    (maphash (lambda (key value)
               (declare (ignore key))
               (when (listp value)
                 (push (list (getf value :index)
                             (getf value :tool-name)
                             (getf value :tool-call-id)
                             (getf value :arguments)
                             (not (null (getf value :started-p)))
                             (not (null (getf value :arguments-complete-p)))
                             (not (null (getf value :executed-p)))
                             (getf value :execution-error))
                       items)))
             (chat-ui-state-stream-tool-calls chat-state))
    (sort items #'string<
          :key (lambda (item)
                 (with-output-to-string (out)
                   (dolist (field item)
                     (write-string (princ-to-string field) out)
                     (write-char #\| out)))))))

(defun %update-stream-tool-call-preview! (chat-state event)
  (let* ((tool-call (%stream-tool-call-from-event event))
         (index (getf event :index))
         (tool-name (or (and (pseudopod:tool-call-p tool-call)
                             (pseudopod:tool-call-name tool-call))
                        (getf event :tool-name)))
         (tool-call-id (or (and (pseudopod:tool-call-p tool-call)
                                (pseudopod:tool-call-id tool-call))
                           (getf event :tool-call-id)))
         (arguments (or (and (pseudopod:tool-call-p tool-call)
                             (pseudopod:tool-call-arguments tool-call))
                        (getf event :arguments)))
         (key (%stream-tool-call-preview-key index tool-call-id tool-name arguments))
         (entry (%ensure-stream-tool-call-preview chat-state key index))
         (kind (or (getf event :type) (getf event :kind))))
    (when (integerp index)
      (setf (getf entry :index) index))
    (when (and (stringp tool-name) (plusp (length tool-name)))
      (setf (getf entry :tool-name) tool-name))
    (when (and (stringp tool-call-id) (plusp (length tool-call-id)))
      (setf (getf entry :tool-call-id) tool-call-id))
    (when (stringp arguments)
      (setf (getf entry :arguments) arguments))
    (setf (getf entry :started-p)
          (or (getf entry :started-p)
              (eq kind :tool-call-started)
              (eq kind :tool-call-argument-complete)))
    (setf (getf entry :arguments-complete-p)
          (or (getf entry :arguments-complete-p)
              (eq kind :tool-call-argument-complete)))
    entry))
;;; Kimi K2.5 Thinking Overlay
;;; ---------------------------------------------------------------------------

(defparameter +thinking-overlay-max-lines+ 10
  "Maximum number of lines to display in the thinking overlay.")

(defparameter +thinking-overlay-border-chars+
  '((:top-left . "╭") (:top-right . "╮")
    (:bottom-left . "╰") (:bottom-right . "╯")
    (:horizontal . "─") (:vertical . "│")))


;;; ---------------------------------------------------------------------------
(defun %clear-stream-tool-tracking! (chat-state)
  (let ((tool-calls (chat-ui-state-stream-tool-calls chat-state))
        (executed (chat-ui-state-stream-executed-tool-call-keys chat-state))
        (journal (chat-ui-state-stream-event-journal chat-state)))
    (when (hash-table-p tool-calls)
      (clrhash tool-calls))
    (when (hash-table-p executed)
      (clrhash executed))
    (when (stream-event-journal-p journal)
      (stream-event-journal-clear! journal))
    (when (typep (chat-ui-state-stream-turn-snapshot chat-state)
                 'pseudopod:stream-turn-snapshot)
      (pseudopod:reset-stream-turn-snapshot!
       (chat-ui-state-stream-turn-snapshot chat-state)))
    (setf (chat-ui-state-stream-completion-pending-p chat-state) nil)
    chat-state))



(defun chat-ui-append-thinking-chunk (chat-state chunk)
  "Append a chunk of thinking/reasoning content to the overlay."
  (when (and (stringp chunk) (plusp (length chunk)))
    (setf (chat-ui-state-stream-thinking-content chat-state)
          (concatenate 'string
                       (chat-ui-state-stream-thinking-content chat-state)
                       chunk))))

(defun chat-ui-clear-thinking-content (chat-state)
  "Clear the thinking content buffer."
  (setf (chat-ui-state-stream-thinking-content chat-state) "")
  chat-state)

(defun chat-ui-toggle-thinking-visible (chat-state)
  "Toggle the visibility of the thinking overlay."
  (setf (chat-ui-state-stream-thinking-visible-p chat-state)
        (not (chat-ui-state-stream-thinking-visible-p chat-state)))
  chat-state)

(defun %format-thinking-content (content max-lines max-width)
  "Format thinking content for display, truncating to max-lines."
  (let* ((lines (split-sequence:split-sequence #\Newline content))
         (truncated (if (> (length lines) max-lines)
                        (subseq lines 0 max-lines)
                        lines))
         (padded (append truncated
                        (make-list (max 0 (- max-lines (length truncated)))
                                   :initial-element "")))
         (wrapped-lines
           (loop for line in padded
                 append (or (ptui.text.layout:wrap-by-width line max-width)
                           (list ""))))
         (final-lines (if (> (length wrapped-lines) max-lines)
                         (subseq wrapped-lines 0 max-lines)
                         wrapped-lines)))
    (mapcar (lambda (line)
              (let ((len (length line)))
                (if (< len max-width)
                    (concatenate 'string line
                                 (make-string (- max-width len)
                                              :initial-element #\Space))
                    (subseq line 0 max-width))))
            final-lines)))

(defun %make-thinking-overlay-widget (chat-state width)
  "Create the thinking overlay widget with border and content."
  (let* ((content (chat-ui-state-stream-thinking-content chat-state))
         (visible-p (chat-ui-state-stream-thinking-visible-p chat-state))
         (stream-active-p (token-stream-active-p (chat-ui-state-stream-state chat-state)))
         (max-content-width (max 10 (- width 4)))  ; 2 chars for border + padding
         (lines (when (and visible-p
                          (or stream-active-p (plusp (length content))))
                  (%format-thinking-content content +thinking-overlay-max-lines+ max-content-width)))
         (bc +thinking-overlay-border-chars+))
    (if (null lines)
        ;; Empty/invisible: return a zero-height spacer
        (ptui.widgets.core:make-spacer-widget 0 0 :key :thinking-overlay-empty)
        ;; Visible overlay with border
        (let* ((top-border (concatenate 'string
                                       (cdr (assoc :top-left bc))
                                       (make-string max-content-width :initial-element (char (cdr (assoc :horizontal bc)) 0))
                                       (cdr (assoc :top-right bc))))
               (bottom-border (concatenate 'string
                                          (cdr (assoc :bottom-left bc))
                                          (make-string max-content-width :initial-element (char (cdr (assoc :horizontal bc)) 0))
                                          (cdr (assoc :bottom-right bc))))
               (title " 💭 thinking ")
               (title-len (length title))
               (title-padded (concatenate 'string
                                         (subseq top-border 0 2)
                                         title
                                         (subseq top-border (+ 2 title-len))))
               (content-lines
                 (loop for line in lines
                       collect (concatenate 'string
                                           (cdr (assoc :vertical bc))
                                           " " line " "
                                           (cdr (assoc :vertical bc)))))
               (all-lines (cons title-padded
                               (append content-lines (list bottom-border))))
               (text-widgets
                 (loop for line in all-lines
                       for idx from 0
                       collect (ptui.widgets.core:make-text-widget
                                line
                                :id (list :thinking idx)
                                :role :thinking-overlay))))
          (ptui.widgets.core:make-stack-widget
           text-widgets
           :id :thinking-overlay-stack
           :direction :column
           :gap 0)))))

(defun %set-stream-tool-call-execution-status! (chat-state preview-key
                                                &key executed-p execution-error
                                                     result malformed-p
                                                     completed-p)
  (let* ((table (chat-ui-state-stream-tool-calls chat-state))
         (entry (and (hash-table-p table)
                     (gethash preview-key table))))
    (when entry
      (when executed-p
        (setf (getf entry :executed-p) t))
      (when execution-error
        (setf (getf entry :execution-error) execution-error))
      (when completed-p
        (setf (getf entry :completed-p) t))
      (when result
        (setf (getf entry :result) result))
      (when malformed-p
        (setf (getf entry :malformed-p) t))
      (setf (gethash preview-key table) entry))
    entry))

(defun %stream-tool-call-completion-pending-p (chat-state)
  (let ((table (chat-ui-state-stream-tool-calls chat-state))
        (pending nil))
    (maphash
     (lambda (_key entry)
       (declare (ignore _key))
       (when (and (listp entry)
                  (getf entry :executed-p)
                  (not (getf entry :completed-p)))
         (setf pending t)))
     table)
    pending))

(defun %maybe-finalize-streaming-completion-pending-state (chat-state)
  (when (and (chat-ui-state-stream-completion-pending-p chat-state)
             (not (%stream-tool-call-completion-pending-p chat-state)))
    (%maybe-finalize-streaming-assistant-on-complete chat-state)))

(defun %set-tool-call-result! (chat-state event)
  (let* ((tool-call (%stream-tool-call-from-event event))
         (preview-entry (%update-stream-tool-call-preview! chat-state event))
         (preview-key (or (getf event :preview-key)
                          (and (listp preview-entry) (getf preview-entry :key)))))
    (%set-stream-tool-call-execution-status!
     chat-state
     preview-key
     :result (or (getf event :result) "")
     :execution-error (getf event :execution-error)
     :completed-p t)
    chat-state))

(defun %maybe-finalize-streaming-assistant-on-complete (chat-state)
  (let* ((conversation (%ensure-chat-conversation-state chat-state))
         (tool-call-entries (%collect-stream-tool-calls chat-state))
         (malformed-names (%collect-malformed-tool-calls chat-state)))
    (setf (chat-ui-state-stream-completion-pending-p chat-state) nil)
    (when tool-call-entries
      (%set-assistant-message-tool-calls! chat-state tool-call-entries))
    (%finalize-streaming-assistant-message chat-state :partialp nil)
    (let ((assistant-response (%stream-target-assistant-response chat-state)))
      (when (and assistant-response
                 (plan-mode-active-p))
        (ignore-errors
          (capture-plan-steps-from-response
           (%message-content->text assistant-response)
           :state (current-plan-mode-state))))
      (cond
        ;; Malformed tool calls (missing tool_call_id) — ask LLM to retry
        ((and malformed-names
              (< (chat-ui-state-agentic-iteration-count chat-state)
                 (%chat-effective-max-iterations chat-state)))
         ;; Append results for any valid tool calls that did execute
         (when tool-call-entries
           (%append-tool-result-messages! chat-state tool-call-entries))
         (chat-ui-add-message chat-state "user"
                              (%malformed-tool-call-retry-message malformed-names))
         (%clear-stream-tool-tracking! chat-state)
         (incf (chat-ui-state-agentic-iteration-count chat-state))
         (%start-agent-continuation-stream chat-state))
        ;; Normal tool call continuation
        ((and tool-call-entries
              (< (chat-ui-state-agentic-iteration-count chat-state)
                 (%chat-effective-max-iterations chat-state)))
         (%append-tool-result-messages! chat-state tool-call-entries)
         (%clear-stream-tool-tracking! chat-state)
         (incf (chat-ui-state-agentic-iteration-count chat-state))
         (%start-agent-continuation-stream chat-state))
        ;; Max iterations reached
        (tool-call-entries
         (%append-tool-result-messages! chat-state tool-call-entries)
         (%clear-stream-tool-tracking! chat-state)
         (chat-ui-add-message chat-state "assistant"
                              "[Agentic loop stopped: max iterations reached]")
         (conversation-transition! conversation :idle)
         (%checkpoint-after-turn chat-state conversation))
        ;; Normal text-only response
        (t
         (%clear-stream-tool-tracking! chat-state)
         (%emit-post-receive-hook assistant-response)
         (conversation-transition! conversation :idle)
         (%checkpoint-after-turn chat-state conversation))))))

(defun %checkpoint-after-turn (chat-state conversation)
  "Fire an auto-checkpoint after a completed agent interaction turn."
  (declare (ignore chat-state))
  (ignore-errors
    (checkpoint-session :conversation conversation
                        :trigger :turn-complete
                        :auto-p t)))

(defun %stream-tool-call-execution-key (tool-call preview-key)
  (or (and (pseudopod:tool-call-p tool-call)
           (pseudopod:tool-call-id tool-call)
           (plusp (length (pseudopod:tool-call-id tool-call)))
           (concatenate 'string "id:" (pseudopod:tool-call-id tool-call)))
      (and (pseudopod:tool-call-p tool-call)
           (pseudopod:tool-call-name tool-call)
           (plusp (length (pseudopod:tool-call-name tool-call)))
           (concatenate 'string
                        "call:"
                        (pseudopod:tool-call-name tool-call)
                        ":"
                        (let ((arguments (pseudopod:tool-call-arguments tool-call)))
                          (if (stringp arguments)
                              arguments
                              (princ-to-string (or arguments "")))))
      preview-key)))

(defun %tool-call-has-id-p (tool-call)
  "Return T if TOOL-CALL has a non-empty tool-call-id."
  (and (pseudopod:tool-call-p tool-call)
       (stringp (pseudopod:tool-call-id tool-call))
       (plusp (length (pseudopod:tool-call-id tool-call)))))

;;; ---- Serial tool executor ----
;;; Tools run on a single background thread to serialize approval dialogs.
;;; The approval mechanism uses a single-slot *pending-approval*, so concurrent
;;; tool threads would race and cause timeouts.

(defvar *tool-executor-lock* (bt:make-lock "tool-executor-lock"))
(defvar *tool-executor-queue* '())
(defvar *tool-executor-condvar* (bt:make-condition-variable :name "tool-executor-cv"))
(defvar *tool-executor-thread* nil)

(defun %tool-executor-loop ()
  "Background loop: dequeue and run tool workers one at a time."
  (loop
    (let ((worker nil))
      (bt:with-lock-held (*tool-executor-lock*)
        (loop while (null *tool-executor-queue*)
              do (bt:condition-wait *tool-executor-condvar*
                                    *tool-executor-lock*
                                    :timeout 2))
        (when *tool-executor-queue*
          (setf worker (pop *tool-executor-queue*))))
      (when worker
        (handler-case (funcall worker)
          (error (c)
            (ptui.util.log:log-warn "tool-executor error: ~A" c)))))))

(defun %ensure-tool-executor-thread ()
  "Start the serial tool executor thread if not running."
  (bt:with-lock-held (*tool-executor-lock*)
    (when (or (null *tool-executor-thread*)
              (not (bt:thread-alive-p *tool-executor-thread*)))
      (setf *tool-executor-thread*
            (bt:make-thread #'%tool-executor-loop
                            :name "tool-executor")))))

(defun %enqueue-tool-worker (worker)
  "Add a tool worker to the serial execution queue."
  (%ensure-tool-executor-thread)
  (bt:with-lock-held (*tool-executor-lock*)
    (setf *tool-executor-queue*
          (append *tool-executor-queue* (list worker)))
    (bt:condition-notify *tool-executor-condvar*)))

(defun %stream-tool-call-execution-context (chat-state tool-call)
  (let* ((toolset (or (chat-ui-state-stream-tools chat-state) *toolset*))
         (config (%chat-config))
         (permission-mode (and (config-p config)
                               (config-permission-mode config)))
         (stream-state (chat-ui-state-stream-state chat-state))
         (tool-name (and (pseudopod:tool-call-p tool-call)
                         (pseudopod:tool-call-name tool-call))))
    (list :toolset toolset
          :permission-mode permission-mode
          :stream-state stream-state
          :tool-name tool-name)))

(defun %stream-tool-call-cancelled-p (context)
  (let ((stream-state (getf context :stream-state)))
    (and (typep stream-state 'token-stream-state)
         (token-stream-cancel-requested-p stream-state))))

(defun %execute-stream-tool-call-now (chat-state tool-call preview-key execution-key context)
  (let ((result-text "")
        (execution-error nil))
    (if (%stream-tool-call-cancelled-p context)
        (setf execution-error "Tool execution cancelled."
              result-text execution-error)
        (handler-case
            (let ((toolset (getf context :toolset))
                  (tool-name (getf context :tool-name))
                  (permission-mode (getf context :permission-mode))
                  (stream-state (getf context :stream-state)))
              (if (pseudopod:find-tool toolset tool-name)
                  (let ((result
                          (execute-tool
                           tool-call
                           (make-amoebum-context
                            :toolset toolset
                            :permission-mode permission-mode
                            :event-bus (%context-event-bus chat-state)
                            :permission-cancel-thunk
                            (lambda ()
                              (%stream-tool-call-cancelled-p context))))))
                    (setf result-text (sanitize-string-for-llm
                                        (if (stringp result)
                                            result
                                            (princ-to-string (or result ""))))))
                  (let ((err-msg (format nil "Unregistered tool ~A."
                                         (or tool-name "<unknown>"))))
                    (setf execution-error err-msg
                          result-text err-msg))))
          (error (condition)
            (setf execution-error (sanitize-string-for-llm (princ-to-string condition))
                  result-text execution-error))))
    (token-stream-emit-tool-call-result
     (getf context :stream-state)
     :tool-call tool-call
     :preview-key preview-key
     :execution-key execution-key
     :result result-text
     :execution-error execution-error)))

(defun %make-stream-tool-call-worker (chat-state tool-call preview-key execution-key context)
  (lambda ()
    (%execute-stream-tool-call-now
     chat-state
     tool-call
     preview-key
     execution-key
     context)))

(defun %dispatch-stream-tool-call-worker! (chat-state tool-call preview-key execution-key context)
  (let ((worker (%make-stream-tool-call-worker
                 chat-state
                 tool-call
                 preview-key
                 execution-key
                 context)))
    (if (eq (getf context :permission-mode) :full-auto)
        (funcall worker)
        (%enqueue-tool-worker worker))))

(defun %prepare-stream-tool-call-execution! (chat-state event)
  (let* ((tool-call (%stream-tool-call-from-event event))
         (preview-entry (%update-stream-tool-call-preview! chat-state event))
         (preview-key (and (listp preview-entry) (getf preview-entry :key)))
         (execution-key (%stream-tool-call-execution-key tool-call preview-key))
         (executed-table (chat-ui-state-stream-executed-tool-call-keys chat-state)))
    (cond
      ((not (and (pseudopod:tool-call-p tool-call) execution-key))
       nil)
      ((gethash execution-key executed-table)
       nil)
      ((not (%tool-call-has-id-p tool-call))
       ;; The :complete handler will ask the LLM to re-issue with proper IDs.
       (setf (gethash execution-key executed-table) t)
       (%set-stream-tool-call-execution-status!
        chat-state preview-key :malformed-p t)
       nil)
      (t
       (setf (gethash execution-key executed-table) t)
       (%set-stream-tool-call-execution-status! chat-state preview-key :executed-p t)
       (list :tool-call tool-call
             :preview-key preview-key
             :execution-key execution-key
             :context (%stream-tool-call-execution-context chat-state tool-call))))))

(defun %execute-stream-tool-call! (chat-state event)
  (let ((execution (%prepare-stream-tool-call-execution! chat-state event)))
    (unless execution
      (return-from %execute-stream-tool-call! nil))
    (%dispatch-stream-tool-call-worker!
     chat-state
     (getf execution :tool-call)
     (getf execution :preview-key)
     (getf execution :execution-key)
     (getf execution :context))
    t))

(defun %make-stream-event-handler-table ()
  (let ((table (make-hash-table :test #'eq)))
    (labels ((register (k fn)
               (setf (gethash k table) fn)))
      (register :text-delta '%handle-stream-textish-event)
      (register :chunk '%handle-stream-textish-event)
      (register :reasoning '%handle-stream-reasoning-event)
      (register :tool-call-delta '%handle-stream-tool-call-preview-event)
      (register :tool-call-started '%handle-stream-tool-call-started-event)
      (register :tool-call-argument-complete '%handle-stream-tool-call-argument-complete-event)
      (register :tool-call-result '%handle-stream-tool-call-result-event)
      (register :complete '%handle-stream-complete-event)
      (register :cancelled '%handle-stream-cancelled-event)
      (register :failed '%handle-stream-failed-event))
    table))

(defun %handle-stream-reasoning-event (chat-state event conversation)
  "Handle reasoning/thinking content from kimi k2.5."
  (declare (ignore conversation))
  (let ((chunk (getf event :text)))
    (when (and (stringp chunk) (plusp (length chunk)))
      (chat-ui-append-thinking-chunk chat-state chunk))))

(defparameter *chat-stream-event-handlers* (%make-stream-event-handler-table))

(defun %handle-stream-textish-event (chat-state event conversation)
  (declare (ignore conversation))
  (%append-streaming-assistant-chunk chat-state (getf event :text))
  (%emit-stream-chunk-token-events chat-state event)
  (%emit-stream-budget-warning-if-needed chat-state)
  (%enforce-stream-token-budget-if-needed chat-state))

(defun %handle-stream-tool-call-preview-event (chat-state event conversation)
  (declare (ignore conversation))
  (%update-stream-tool-call-preview! chat-state event))

(defun %stream-tool-call-event-metadata (chat-state event)
  (let* ((tool-call (%stream-tool-call-from-event event))
         (index (getf event :index))
         (tool-name (or (and (pseudopod:tool-call-p tool-call)
                             (pseudopod:tool-call-name tool-call))
                        (getf event :tool-name)))
         (tool-call-id (or (and (pseudopod:tool-call-p tool-call)
                                (pseudopod:tool-call-id tool-call))
                           (getf event :tool-call-id)))
         (arguments (or (and (pseudopod:tool-call-p tool-call)
                             (pseudopod:tool-call-arguments tool-call))
                        (getf event :arguments)))
         (key (%stream-tool-call-preview-key index tool-call-id tool-name arguments))
         (prior-entry (%find-stream-tool-call-preview chat-state key index))
         (prior-started-p (and (listp prior-entry)
                               (not (null (getf prior-entry :started-p)))))
         (prior-arguments-complete-p
           (and (listp prior-entry)
                (not (null (getf prior-entry :arguments-complete-p)))))
         (entry (%update-stream-tool-call-preview! chat-state event)))
    (list :prior-started-p prior-started-p
          :prior-arguments-complete-p prior-arguments-complete-p
          :entry entry
          :tool-name (or (getf event :tool-name)
                         (and (listp entry) (getf entry :tool-name)))
          :tool-call-id (or (getf event :tool-call-id)
                            (and (listp entry) (getf entry :tool-call-id)))
          :arguments (or (getf event :arguments)
                         (and (listp entry) (getf entry :arguments)))
          :index (or (getf event :index)
                     (and (listp entry) (getf entry :index))))))

(defun %handle-stream-tool-call-started-event (chat-state event conversation)
  (declare (ignore conversation))
  (let ((metadata (%stream-tool-call-event-metadata chat-state event)))
    (unless (getf metadata :prior-started-p)
      (publish (%context-event-bus chat-state)
               (make-tool-call-started-event
                :tool-name (getf metadata :tool-name)
                :tool-call-id (getf metadata :tool-call-id)
                :arguments (getf metadata :arguments)
                :index (getf metadata :index))))))

(defun %handle-stream-tool-call-argument-complete-event (chat-state event conversation)
  (declare (ignore conversation))
  (let ((metadata (%stream-tool-call-event-metadata chat-state event)))
    (unless (getf metadata :prior-arguments-complete-p)
      (publish (%context-event-bus chat-state)
               (make-tool-call-argument-complete-event
                :tool-name (getf metadata :tool-name)
                :tool-call-id (getf metadata :tool-call-id)
                :arguments (getf metadata :arguments)
                :index (getf metadata :index)))
      (%execute-stream-tool-call! chat-state event))))

(defun %handle-stream-tool-call-result-event (chat-state event conversation)
  (declare (ignore conversation))
  (%set-tool-call-result! chat-state event)
  (%maybe-finalize-streaming-completion-pending-state chat-state))

(defun %handle-stream-complete-event (chat-state event conversation)
  (declare (ignore event conversation))
  (setf (chat-ui-state-stream-completion-pending-p chat-state) t))

(defun %handle-stream-cancelled-event (chat-state event conversation)
  (declare (ignore event))
  (%finalize-streaming-assistant-message chat-state :partialp t)
  (%clear-stream-tool-tracking! chat-state)
  (conversation-transition! conversation :idle))

(defun %handle-stream-failed-event (chat-state event conversation)
  (declare (ignore event))
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (summary (token-stream-progress-summary stream-state))
         (error-message (getf summary :error-message)))
    (when (and (stringp error-message)
               (plusp (length error-message)))
      (%append-streaming-assistant-chunk
       chat-state
       (format nil "\n[stream failed: ~A]\n" error-message))))
  (%finalize-streaming-assistant-message chat-state :partialp t)
  (%clear-stream-tool-tracking! chat-state)
  (conversation-transition! conversation :error-recovery))

(defun %record-chat-stream-event! (chat-state event)
  (let ((journal (and chat-state
                      (chat-ui-state-stream-event-journal chat-state)))
        (snapshot (and chat-state
                       (chat-ui-state-stream-turn-snapshot chat-state))))
    (when (stream-event-journal-p journal)
      (stream-event-journal-append! journal event))
    (when (typep snapshot 'pseudopod:stream-turn-snapshot)
      (pseudopod:stream-turn-apply-event! snapshot event)))
  chat-state)

(defun %classify-streamed-turn-events (events)
  (pseudopod:stream-turn-snapshot-terminal-outcome
   (%stream-turn-snapshot-from-events events)))

(defun %dispatch-stream-event (chat-state event conversation)
  (%record-chat-stream-event! chat-state event)
  (let* ((handler-name (gethash (or (getf event :type) (getf event :kind))
                                *chat-stream-event-handlers*))
         (handler (and handler-name (symbol-function handler-name))))
    (when handler
      (funcall handler chat-state event conversation))))

(defun %stream-status-summary (chat-state)
  (token-stream-progress-summary (chat-ui-state-stream-state chat-state)))

(defun %stream-summary-publish-key (summary)
  (let ((status (or (getf summary :status) :idle))
        (tokens (or (getf summary :tokens) 0))
        (chunks (or (getf summary :chunks) 0))
        (budget-warning-emitted-p (not (null (getf summary :budget-warning-emitted-p))))
        (cancel-requested-p (not (null (getf summary :cancel-requested-p))))
        (tokens-per-second (or (getf summary :tokens-per-second) 0.0d0))
        (elapsed-ms (or (getf summary :elapsed-ms) 0)))
    (list status
          tokens
          chunks
          budget-warning-emitted-p
          cancel-requested-p
          (if (eq status :running)
              (truncate (* 10 tokens-per-second))
              elapsed-ms))))

(defun %publish-status-bar-stream-summary-if-needed (chat-state)
  (let* ((summary (%stream-status-summary chat-state))
         (publish-key (%stream-summary-publish-key summary)))
    (unless (equal publish-key (chat-ui-state-stream-status-publish-key chat-state))
      (publish-status-bar-stream-summary
       summary
       :event-bus (status-bar-state-event-bus
                   (chat-ui-state-status-bar-state chat-state)))
      (setf (chat-ui-state-stream-status-publish-key chat-state) publish-key))
    summary))

(defun %stream-tree-key (chat-state)
  (let* ((summary (%stream-status-summary chat-state))
         (status (getf summary :status))
         (elapsed-ms (or (getf summary :elapsed-ms) 0)))
    (list (status-bar-render-key (chat-ui-state-status-bar-state chat-state))
          status
          (getf summary :tokens)
          (getf summary :chunks)
          (getf summary :budget-warning-emitted-p)
          (if (eq status :running)
              (truncate elapsed-ms 100)
              elapsed-ms)
          (getf summary :cancel-requested-p)
          (getf summary :error-message)
          (%stream-tool-call-preview-signature chat-state))))

(defun %parse-tool-arguments (arguments-string)
  "Parse a JSON arguments string into an alist of (key . value-string) pairs.
Falls back to ((\"args\" . original-string)) on parse failure."
  (if (or (null arguments-string) (string= arguments-string ""))
      '()
      (handler-case
          (let ((parsed (jonathan:parse arguments-string :as :alist)))
            (if (listp parsed)
                (loop for (key . val) in parsed
                      collect (cons (if (stringp key) key (princ-to-string key))
                                    (if (stringp val)
                                        val
                                        (%normalize-inline-text (princ-to-string val)))))
                (list (cons "args" arguments-string))))
        (error () (list (cons "args" arguments-string))))))

(defun %format-tool-argument-lines (arguments-alist indent-width content-width)
  "Format parsed tool arguments into indented display lines.
INDENT-WIDTH is the left padding for continuation lines.
CONTENT-WIDTH is the max width for value text.
Returns a list of strings, one per display line."
  (let ((indent (make-string indent-width :initial-element #\Space))
        (lines '()))
    (dolist (pair arguments-alist)
      (let* ((key (car pair))
             (value (cdr pair))
             (prefix (format nil "~A~A: " indent key))
             (prefix-width (length prefix))
             (value-width (max 10 (- content-width prefix-width)))
             (wrapped (ptui.text.layout:wrap-by-width value (max 10 value-width))))
        (when (null wrapped) (setf wrapped (list "")))
        (let ((cont-indent (make-string prefix-width :initial-element #\Space)))
          (loop for wline in wrapped
                for i from 0 do
                  (push (if (zerop i)
                            (concatenate 'string prefix wline)
                            (concatenate 'string cont-indent wline))
                        lines)))))
    (nreverse lines)))

(defparameter +tool-spinner-frames+
  #("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"))

(defun %tool-call-spinner-glyph (frame-count)
  "Return the current spinner glyph based on the frame counter."
  (aref +tool-spinner-frames+
        (mod (floor frame-count 3) (length +tool-spinner-frames+))))

(defun %stream-tool-call-preview-lines (chat-state width)
  (let ((safe-width (max 24 width))
        (entries '())
        (frame-count (chat-ui-state-frame-count chat-state))
        (stream-active-p (token-stream-active-p
                          (chat-ui-state-stream-state chat-state))))
    (flet ((record-entry (id text role styled-segments)
             (push (list :id id
                         :text text
                         :role role
                         :styled-segments styled-segments)
                   entries)))
      (maphash
       (lambda (key entry)
         (declare (ignore key))
         (when (listp entry)
           (let* ((tool-name (or (getf entry :tool-name) "<tool>"))
                  (arguments (or (getf entry :arguments) ""))
                  (normalized-arguments (%normalize-inline-text arguments))
                  (status (cond
                            ((getf entry :execution-error) "error")
                            ((getf entry :executed-p) "running")
                            ((getf entry :arguments-complete-p) "args-ready")
                            (t "streaming")))
                  (entry-key (or (getf entry :key) tool-name))
                  (header (format nil "tool: ~A [~A]" tool-name status))
                  (arg-indent 6)
                  (arg-body-width (max 12 (- safe-width (+ 2 arg-indent))))
                  (error-text (getf entry :execution-error))
                  (show-spinner (and stream-active-p
                                     (member status '("streaming" "args-ready" "running")
                                             :test #'string=)))
                  (line-idx 0))
             (record-entry
              (list :stream-tool-call entry-key 0)
              header
              :tool-preview-header
              (list
               (list :text "tool: " :role :tool-preview-keyword :dimp t)
               (list :text tool-name :role :tool-preview-tool)
               (list :text (format nil " [~A]" status)
                     :role (cond
                             ((string= status "error") :tool-preview-status-error)
                             ((string= status "running") :tool-preview-status-running)
                             (t :tool-preview-status-streaming)))))
             (incf line-idx)
             (when (and (stringp error-text) (plusp (length error-text)))
               (let ((err-line (format nil "~A! ~A"
                                      (make-string arg-indent :initial-element #\Space)
                                      (%truncate-inline-text error-text
                                                             (max 8
                                                                  (- safe-width arg-indent 2))))))
                 (record-entry
                  (list :stream-tool-call entry-key line-idx)
                  err-line
                  :tool-preview-error
                  (list (list :text err-line :role :tool-preview-error-text :boldp t)))
                 (incf line-idx)))
             (cond
               ((and (null error-text)
                     (plusp (length normalized-arguments))
                     (member status '("streaming" "args-ready" "running")
                             :test #'string=))
                (let* ((line-text
                         (%truncate-inline-text
                          (format nil " TOOL> [~A] ~A ~A"
                                  status
                                  tool-name
                                  normalized-arguments)
                          safe-width))
                       (styled-segments
                         (list
                          (list :text " TOOL> " :role :tool-preview-args)
                          (list :text (format nil "[~A] " status)
                                :role :tool-preview-args)
                          (list :text tool-name :role :tool-preview-args)
                          (list :text " " :role :tool-preview-args)
                          (list :text normalized-arguments :role :tool-preview-args))))
                  (setf entries (remove (list :stream-tool-call entry-key 0)
                                        entries
                                        :key (lambda (entry) (getf entry :id))
                                        :test #'equal))
                  (record-entry
                   (list :stream-tool-call entry-key 0)
                   line-text
                   :tool-preview-header
                   styled-segments)))
               ((plusp (length normalized-arguments))
                (let ((wrapped-args
                        (or (ptui.text.layout:wrap-by-width
                             normalized-arguments arg-body-width)
                            (list ""))))
                  (loop for arg-line in wrapped-args
                        for arg-line-idx from 0 do
                        (let* ((label (if (zerop arg-line-idx) "  args: " "         "))
                               (line-text (concatenate 'string label arg-line))
                               (styled-segments (list (list :text label :role :tool-preview-args-label :dimp t)
                                                     (list :text arg-line :role :tool-preview-args))))
                          (record-entry (list :stream-tool-call-arg entry-key line-idx)
                                        line-text
                                        :tool-preview-args
                                        styled-segments)
                          (incf line-idx))))))
             (when (and show-spinner
                        (null error-text)
                        (not (plusp (length normalized-arguments))))
               (let ((spinner-line
                       (format nil "~A~A"
                               (make-string arg-indent :initial-element #\Space)
                               (%tool-call-spinner-glyph frame-count))))
                 (record-entry (list :stream-tool-call entry-key line-idx)
                               spinner-line
                               :tool-preview-spinner
                               (list (list :text spinner-line
                                           :role :tool-preview-spinner)))
                 (incf line-idx))))))
       (chat-ui-state-stream-tool-calls chat-state)))
    (setf entries (sort entries #'string<
                          :key (lambda (entry) (princ-to-string (getf entry :id)))))
    (nreverse entries)))

(defun %stream-response-text (chat-state)
  (let ((chunks (chat-ui-state-stream-response-chunks chat-state)))
    (if (null chunks)
        ""
        (with-output-to-string (out)
          (dolist (chunk (nreverse (copy-list chunks)))
            (when (stringp chunk)
              (write-string chunk out)))))))

(defun %prime-finalized-streaming-assistant-cache! (chat-state target-index)
  (let* ((renderer (chat-ui-state-stream-markdown-renderer chat-state))
         (content-width (and (typep renderer 'streaming-markdown-renderer)
                             (streaming-markdown-renderer-width renderer))))
    (when (and (integerp target-index)
               (integerp content-width)
               (> content-width 0))
      (setf (gethash (cons target-index content-width)
                     *%styled-lines-cache*)
            (streaming-markdown-renderer-render-lines renderer
                                                      content-width
                                                      :partialp nil
                                                      :cursor-visible-p nil)))))

(defun %set-streaming-assistant-message (chat-state target-index text &key partialp)
  (let ((messages (chat-ui-state-messages chat-state)))
    (when (and (integerp target-index)
               (>= target-index 0)
               (< target-index (length messages)))
      (let* ((existing (nth target-index messages))
             (assistant-message (make-chat-message "assistant"
                                                  (or text "")
                                                  :partial partialp))
             (existing-tool-calls (and existing
                                      (typep existing 'pseudopod:message)
                                      (pseudopod:message-tool-calls existing))))
        (when existing-tool-calls
          (setf (pseudopod:message-tool-calls assistant-message)
                existing-tool-calls))
        (%replace-message-at-index! messages target-index assistant-message)
        (%sync-chat-context-usage-replacement! chat-state existing assistant-message))
      (when (chat-ui-state-stream-scroll-follow-p chat-state)
        (setf (chat-ui-state-message-scrollback-lines chat-state) 0))
      t)))

(defun %materialize-streaming-assistant-message! (chat-state &key partialp)
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (target-index (token-stream-state-target-message-index stream-state)))
    (when (integerp target-index)
      (%set-streaming-assistant-message chat-state
                                        target-index
                                        (%stream-response-text chat-state)
                                        :partialp partialp))))

(defun %append-streaming-assistant-chunk (chat-state chunk)
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (target-index (token-stream-state-target-message-index stream-state))
         (messages (chat-ui-state-messages chat-state)))
    (when (and (integerp target-index)
               (>= target-index 0)
               (< target-index (length messages)))
      (when (and (stringp chunk)
                 (plusp (length chunk)))
        (push chunk (chat-ui-state-stream-response-chunks chat-state)))
      (when (chat-ui-state-stream-scroll-follow-p chat-state)
        (setf (chat-ui-state-message-scrollback-lines chat-state) 0))
      (when (stringp chunk)
        (streaming-markdown-renderer-append-chunk
         (chat-ui-state-stream-markdown-renderer chat-state)
         chunk)))))

(defun %finalize-streaming-assistant-message (chat-state &key partialp)
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (target-index (token-stream-state-target-message-index stream-state))
         (messages (chat-ui-state-messages chat-state)))
    (when (and (integerp target-index)
               (>= target-index 0)
               (< target-index (length messages)))
      (let ((text (%stream-response-text chat-state)))
        (%set-streaming-assistant-message chat-state target-index text :partialp partialp)
        (unless partialp
          (%prime-finalized-streaming-assistant-cache! chat-state target-index))
        (when (not partialp)
          (%record-chat-stream-event! chat-state '(:kind :answer-finalized)))
        (let* ((updated-messages (chat-ui-state-messages chat-state))
               (updated-message (and (integerp target-index)
                                     (>= target-index 0)
                                     (< target-index (length updated-messages))
                                     (nth target-index updated-messages))))
          (when (pseudopod:message-p updated-message)
            (conversation-state-update-entry (%ensure-chat-conversation-state chat-state)
                                             target-index
                                             updated-message)))))
    (setf (chat-ui-state-stream-response-chunks chat-state) '())
    (setf (chat-ui-state-stream-scroll-follow-p chat-state) t)))

(defun %stream-target-assistant-response (chat-state)
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (target-index (token-stream-state-target-message-index stream-state))
         (messages (chat-ui-state-messages chat-state)))
    (and (integerp target-index)
         (>= target-index 0)
         (< target-index (length messages))
         (nth target-index messages))))

(defun %emit-post-receive-hook (response)
  (when response
    (ignore-errors
      (run-hooks :post-receive response)))
  t)

(defun %emit-post-llm-receive-hook (response usage model)
  (when response
    (ignore-errors
      (run-hooks :post-llm-receive response usage model)))
  t)

(defun %resolve-pre-llm-messages (default-messages hook-results)
  (let ((resolved default-messages))
    (dolist (entry (or hook-results '()) resolved)
      (let ((value (cdr entry)))
        (when (listp value)
          (setf resolved value))))))

(defun %emit-stream-budget-warning-if-needed (chat-state)
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (used-tokens (chat-ui-state-context-used-tokens chat-state))
         (limit-tokens (chat-ui-state-context-window-limit chat-state)))
    (when (and (token-stream-active-p stream-state)
               (integerp used-tokens)
               (integerp limit-tokens)
               (> limit-tokens 0))
      (let ((warning
              (token-stream-maybe-budget-warning stream-state
                                                 used-tokens
                                                 limit-tokens)))
        (when warning
          (publish (%context-event-bus chat-state)
                   (make-stream-budget-warning-event
                    :used-tokens (getf warning :used-tokens)
                    :limit-tokens (getf warning :limit-tokens)
                    :usage-percent (getf warning :usage-percent)
                    :threshold-percent (getf warning :threshold-percent))))
        warning))))

(defun %stream-budget-abort-threshold-percent (chat-state)
  (let ((value (cfg :stream-budget-abort-threshold-percent)))
    (if (and (integerp value) (>= value 1) (<= value 100))
        value
        +stream-budget-abort-threshold-percent+)))

(defun %stream-budget-threshold-limit (limit threshold-percent)
  (truncate (* (max 0 limit)
               (/ (max 1 threshold-percent) 100.0d0))))

(defun %stream-tokenize-chunk (chunk)
  (let ((value (if (stringp chunk) chunk "")))
    (remove-if (lambda (token)
                 (or (null token)
                     (zerop (length token))))
               (cl-ppcre:split "\\s+" value))))

(defun %budget-summary-window-messages (chat-state &key (max-messages 8))
  (let* ((messages (chat-ui-state-messages chat-state))
         (safe-max (max 1 (if (and (integerp max-messages) (> max-messages 0))
                              max-messages
                              8)))
         (count (length messages))
         (start (max 0 (- count safe-max))))
    (subseq messages start count)))

(defun %budget-exhaustion-context-summary (chat-state)
  (let* ((window (%budget-summary-window-messages chat-state :max-messages 8)))
    (if (null window)
        "No conversation context available."
        (%compression-summary-text window))))

(defun %apply-stream-budget-exhaustion-resolution (chat-state stream-state resolution)
  (let ((action (getf resolution :action)))
    (case action
      (:extend-budget
       (let* ((extra (max 1 (or (getf resolution :extra-budget) 1)))
              (new-limit (+ (chat-ui-state-context-window-limit chat-state) extra))
              (status-state (chat-ui-state-status-bar-state chat-state)))
         (setf (chat-ui-state-context-window-limit chat-state) new-limit)
         (when (typep status-state 'status-bar-state)
           (setf (status-bar-state-context-max-tokens status-state) new-limit))
         nil))
      (:summarize-and-finish
       (ignore-errors
         (%compress-chat-history! chat-state :trigger :budget-exhausted))
       (let ((partial-output (or (getf resolution :partial-output)
                                 "Budget exhausted. Returning a bounded partial result.")))
         (%append-streaming-assistant-chunk
          chat-state
          (format nil "~%[budget exhausted] ~A~%" partial-output))
         (%materialize-streaming-assistant-message! chat-state :partialp t))
       (token-stream-abort stream-state :budget-exhausted)
       t)
      (:abort-task
       (let ((reason (or (getf resolution :reason)
                         "Budget exhausted; task aborted.")))
         (%append-streaming-assistant-chunk
          chat-state
          (format nil "~%[budget exhausted] ~A~%" reason))
         (%materialize-streaming-assistant-message! chat-state :partialp t))
       (token-stream-abort stream-state :budget-exhausted)
       t)
      (otherwise
       (token-stream-abort stream-state :budget-exhausted)
       t))))

(defun %emit-stream-chunk-token-events (chat-state event)
  (let* ((chunk (getf event :text))
         (tokens (%stream-tokenize-chunk chunk))
         (token-count (or (getf event :token-count) 0)))
    (when (plusp (length tokens))
      (let* ((summary (token-stream-progress-summary (chat-ui-state-stream-state chat-state)))
             (total-tokens (or (getf summary :tokens) 0))
             (chunk-index (or (getf summary :chunks) 0))
             (base-total (max 0 (- total-tokens token-count))))
        (loop for token in tokens
              for token-index from 1 do
                (publish (%context-event-bus chat-state)
                         (make-llm-stream-chunk-event
                          :token token
                          :chunk-index chunk-index
                          :token-index token-index
                          :total-tokens (+ base-total token-index))))))))

(defun %enforce-stream-token-budget-if-needed (chat-state)
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (limit (chat-ui-state-context-window-limit chat-state))
         (threshold-percent (%stream-budget-abort-threshold-percent chat-state))
         (summary (token-stream-progress-summary stream-state))
         (stream-tokens (or (getf summary :tokens) 0))
         (aborted-p (not (null (getf summary :aborted-p)))))
    (when (and (token-stream-active-p stream-state)
               (integerp limit)
               (> limit 0)
               (not aborted-p))
      (let ((threshold-limit (%stream-budget-threshold-limit limit threshold-percent)))
        (when (> stream-tokens threshold-limit)
          ;; Only publish if the warning-level event wasn't already emitted
          (unless (token-stream-state-budget-warning-emitted-p stream-state)
            (publish (%context-event-bus chat-state)
                     (make-stream-budget-warning-event
                      :used-tokens stream-tokens
                      :limit-tokens limit
                      :usage-percent (truncate (/ (* stream-tokens 100.0d0)
                                                  (max 1 limit)))
                      :threshold-percent threshold-percent)))
          (%apply-stream-budget-exhaustion-resolution
           chat-state
           stream-state
           (handle-budget-exhaustion
            :kind :token
            :used stream-tokens
            :budget threshold-limit
            :context-summary (%budget-exhaustion-context-summary chat-state)
            :max-partial-output-chars 280)))))))

(defun %collect-stream-tool-calls (chat-state)
  "Collect pseudopod:tool-call structs from the stream preview table."
  (let ((calls '()))
    (maphash
     (lambda (key entry)
       (declare (ignore key))
       (when (and (listp entry)
                  (getf entry :executed-p)
                  (getf entry :completed-p))
         (let ((tool-name (getf entry :tool-name))
               (tool-call-id (getf entry :tool-call-id))
               (arguments (getf entry :arguments))
               (result (getf entry :result)))
           (push (list :tool-call (pseudopod:make-tool-call
                                   :id tool-call-id
                                   :name (or tool-name "")
                                   :arguments arguments)
                       :result (or result ""))
                 calls))))
     (chat-ui-state-stream-tool-calls chat-state))
    (nreverse calls)))

(defun %collect-malformed-tool-calls (chat-state)
  "Collect tool call names from the preview table that were marked malformed
(missing tool_call_id)."
  (let ((names '()))
    (maphash
     (lambda (key entry)
       (declare (ignore key))
       (when (and (listp entry) (getf entry :malformed-p))
         (push (or (getf entry :tool-name) "<unknown>") names)))
     (chat-ui-state-stream-tool-calls chat-state))
    (nreverse names)))

(defun %malformed-tool-call-retry-message (malformed-names)
  "Build a user message asking the LLM to re-issue malformed tool calls."
  (format nil "Your tool call~P for ~{~A~^, ~} ~
               ~[~;was~:;were~] missing a tool_call_id. ~
               Each tool call must include an id field. ~
               Please re-issue ~[~;it~:;them~]."
          (length malformed-names)
          malformed-names
          (length malformed-names)
          (length malformed-names)))

(defun %append-tool-result-messages! (chat-state tool-call-entries)
  "Append tool-result messages to the conversation for each executed tool call.
Sanitizes ANSI escape codes from tool results to prevent LLM API errors."
  (dolist (entry tool-call-entries)
    (let* ((tc (getf entry :tool-call))
           (result (getf entry :result))
           (tool-call-id (and (pseudopod:tool-call-p tc)
                              (pseudopod:tool-call-id tc)))
           (tool-name (and (pseudopod:tool-call-p tc)
                           (pseudopod:tool-call-name tc)))
           ;; Sanitize ANSI escape codes to prevent 'invalid character \\x1b' errors
           (sanitized-result (sanitize-string-for-llm (or result "")))
           (message (pseudopod:make-message
                     :role "tool"
                     :content sanitized-result
                     :name tool-name
                     :tool-call-id tool-call-id)))
      (chat-ui-append-message chat-state message))))

(defun %set-assistant-message-tool-calls! (chat-state tool-call-entries)
  "Set tool-calls on the current streaming assistant message."
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (target-index (token-stream-state-target-message-index stream-state))
         (messages (chat-ui-state-messages chat-state)))
    (when (and (integerp target-index)
               (>= target-index 0)
               (< target-index (length messages)))
      (let ((message (nth target-index messages))
            (tool-calls (mapcar (lambda (entry) (getf entry :tool-call))
                                tool-call-entries)))
        (when (and (pseudopod:message-p message) tool-calls)
          (setf (pseudopod:message-tool-calls message) tool-calls))))))

(defun %start-agent-continuation-stream (chat-state)
  "Start a new streaming response to continue the agentic tool loop.
Like %start-streaming-assistant-response but without adding a new user message."
  (when (token-stream-active-p (chat-ui-state-stream-state chat-state))
    (return-from %start-agent-continuation-stream nil))
  (let ((runner (chat-ui-state-stream-runner chat-state)))
    (when (functionp runner)
      (let* ((history (copy-list (chat-ui-state-messages chat-state)))
             (target-index (length history))
             (stream-state (chat-ui-state-stream-state chat-state))
             (system-prompt (chat-ui-state-stream-system-prompt chat-state)))
        (setf (chat-ui-state-stream-scroll-follow-p chat-state) t)
        (streaming-markdown-renderer-reset
         (chat-ui-state-stream-markdown-renderer chat-state))
        (setf (chat-ui-state-stream-response-chunks chat-state) '())
        (conversation-transition! (%ensure-chat-conversation-state chat-state)
                                  :streaming)
        (chat-ui-add-message chat-state "assistant" "" :partial t)
        (%clear-stream-tool-tracking! chat-state)
        (token-stream-start
         stream-state
         (lambda (active-stream-state)
           (let ((*stream-chunk-hook-callback* (%make-stream-chunk-hook-callback)))
             (funcall runner
                      active-stream-state
                      ""
                      history
                      :system-prompt system-prompt
                      :client (chat-ui-state-stream-client chat-state)
                      :tools (%resolve-chat-tools chat-state))))
         :target-message-index target-index
         :budget-abort-threshold-percent
         (%stream-budget-abort-threshold-percent chat-state))))))

(defun %drain-stream-events (chat-state)
  (let ((conversation (%ensure-chat-conversation-state chat-state)))
    (token-stream-drain-events
     (chat-ui-state-stream-state chat-state)
     (lambda (event)
       (%dispatch-stream-event chat-state event conversation)))
    (%maybe-finalize-streaming-completion-pending-state chat-state)))

(defun %stream-status-fragment (chat-state)
  (let* ((summary (%stream-status-summary chat-state))
         (status (getf summary :status))
         (tokens (or (getf summary :tokens) 0))
         (elapsed-ms (or (getf summary :elapsed-ms) 0))
         (tps (or (getf summary :tokens-per-second) 0.0d0))
         (error-message (getf summary :error-message)))
    (case status
      (:running
       (format nil "stream ~D tok @ ~,2f tok/s ~,1fs"
               tokens
               tps
               (/ elapsed-ms 1000.0d0)))
      (:cancelled
       (if (getf summary :aborted-p)
           (format nil "stream aborted (~D tok, ~A)"
                   tokens
                   (or (getf summary :abort-reason) :unknown))
           (format nil "stream cancelled (~D tok, ~,1fs)"
                   tokens
                   (/ elapsed-ms 1000.0d0))))
      (:completed
       (format nil "stream complete (~D tok, ~,1fs)"
               tokens
               (/ elapsed-ms 1000.0d0)))
      (:failed
       (if (and (stringp error-message) (plusp (length error-message)))
           (format nil "stream failed: ~A" error-message)
           "stream failed"))
      (otherwise
       nil))))

(defun %resolve-chat-tools (chat-state)
  "Return the tool definitions list to pass to the streaming API.
Falls back to the global *toolset* when stream-tools is nil."
  (or (chat-ui-state-stream-tools chat-state)
      (and (boundp '*toolset*)
           (pseudopod:toolset-p *toolset*)
           (pseudopod:toolset-tools *toolset*))))

(defun %resolve-chat-toolset (chat-state)
  (let ((stream-tools (chat-ui-state-stream-tools chat-state)))
    (cond
      ((pseudopod:toolset-p stream-tools)
       stream-tools)
      ((and (boundp '*toolset*)
            (pseudopod:toolset-p *toolset*))
       *toolset*)
      (t
       (pseudopod:make-toolset)))))

(defun %chat-permission-mode ()
  (let ((config (%chat-config)))
    (and (config-p config)
         (config-permission-mode config))))

(defun %chat-idle-hook-threshold-seconds ()
  (let ((value (cfg :hook-idle-threshold-seconds)))
    (if (and (integerp value)
             (> value 0))
        value
        *hook-idle-threshold-seconds*)))

(defun %chat-mark-activity ()
  (setf *hook-last-activity-second* (get-universal-time)
        *hook-last-idle-notified-second* nil)
  t)

(defun %run-chat-idle-hooks-if-needed ()
  (let* ((now (get-universal-time))
         (last-activity (if (and (integerp *hook-last-activity-second*)
                                 (> *hook-last-activity-second* 0))
                            *hook-last-activity-second*
                            now))
         (idle-seconds (max 0 (- now last-activity)))
         (threshold (%chat-idle-hook-threshold-seconds)))
    (when (and (integerp threshold)
               (>= idle-seconds threshold)
               (or (null *hook-last-idle-notified-second*)
                   (> idle-seconds *hook-last-idle-notified-second*)))
      (run-hooks :on-idle idle-seconds)
      (setf *hook-last-idle-notified-second* idle-seconds)))
  t)

(defun %append-step-history-delta! (chat-state step-history)
  (let ((existing-count (length (chat-ui-state-messages chat-state))))
    (dolist (message (nthcdr existing-count (or step-history '())))
      (when (pseudopod:message-p message)
        (chat-ui-append-message chat-state message)))))

(defun %invoke-pseudopod-step (client step-messages tools toolset chat-state context)
  (handler-case
      (pseudopod:step
       client
       (pseudopod:make-agent-step-context
        :messages step-messages
        :tools tools
        :toolset toolset
        :max-steps (%chat-effective-max-iterations chat-state)
        :on-tool-call
        (lambda (tool-call)
          (values t (execute-tool tool-call context)))
        :on-tool-error
        (lambda (tool-call condition)
          (unless (typep condition 'tool-error)
            (publish (%context-event-bus chat-state)
                     (make-tool-error-event
                      :tool-name (pseudopod:tool-call-name tool-call)
                      :args (ignore-errors
                              (%decode-tool-call-arguments tool-call))
                      :condition (princ-to-string condition)))))))
    (program-error (condition)
      (if (search "odd number of &KEY arguments"
                  (princ-to-string condition)
                  :test #'char-equal)
          (pseudopod:step
           client
           :messages step-messages
           :tools tools
           :toolset toolset
           :max-steps (%chat-effective-max-iterations chat-state)
           :on-tool-call
           (lambda (tool-call)
             (values t (execute-tool tool-call context)))
           :on-tool-error
           (lambda (tool-call condition)
             (unless (typep condition 'tool-error)
               (publish (%context-event-bus chat-state)
                        (make-tool-error-event
                         :tool-name (pseudopod:tool-call-name tool-call)
                         :args (ignore-errors
                                 (%decode-tool-call-arguments tool-call))
                         :condition (princ-to-string condition))))))
          (error condition)))))

(defun %start-step-loop-assistant-response (chat-state)
  (let ((client (chat-ui-state-stream-client chat-state)))
    (when (pseudopod:client-p client)
      (let* ((toolset (%resolve-chat-toolset chat-state))
             (tools (%resolve-chat-tools chat-state))
             (model (pseudopod:client-model client))
             (base-url (pseudopod:client-base-url client))
             (default-messages (copy-list (chat-ui-state-messages chat-state)))
             (context (make-amoebum-context
                       :toolset toolset
                       :permission-mode (%chat-permission-mode)
                       :event-bus (%context-event-bus chat-state))))
        (multiple-value-bind (pre-status pre-results)
            (run-hooks :pre-llm-send default-messages tools model)
          (when (member pre-status '(:block :deny) :test #'eq)
            (conversation-transition! (%ensure-chat-conversation-state chat-state)
                                      :idle)
            (return-from %start-step-loop-assistant-response nil))
          (let* ((step-messages (%resolve-pre-llm-messages default-messages pre-results))
                 (step-request-id (format nil "step-~D" (%usdt-now-ms)))
                 (step-start-ms (%usdt-now-ms))
                 (step-status :ok)
                 (llm-probe-start
                   (usdt-probe-llm-request-start model base-url :step-loop step-request-id))
                 (step-result
                   (unwind-protect
                        (handler-case
                            (%invoke-pseudopod-step
                             client
                             step-messages
                             tools
                             toolset
                             chat-state
                             context)
                          (error (condition)
                            (setf step-status :error)
                            (error condition)))
                     (usdt-probe-llm-request-end model
                                                 base-url
                                                 :step-loop
                                                 step-request-id
                                                 (max 0 (- (%usdt-now-ms) step-start-ms))
                                                 :status step-status)))
                 (response (or (pseudopod:step-result-final-message step-result)
                               (pseudopod:step-result-last-message step-result))))
        (declare (ignore llm-probe-start))
        (run-hooks :on-step-complete
                   (pseudopod:step-result-steps step-result)
                   (max 0
                        (- (length (or (pseudopod:step-result-history step-result) '()))
                           (length (chat-ui-state-messages chat-state))))
                   (length (or (pseudopod:step-result-tool-results step-result) '())))
        (%append-step-history-delta!
         chat-state
         (pseudopod:step-result-history step-result))
        (%emit-post-llm-receive-hook step-result nil model)
        (%emit-post-receive-hook response)
        (conversation-transition! (%ensure-chat-conversation-state chat-state)
                                  :idle)))))))

(defun %resolve-chat-system-prompt (chat-state)
  (let* ((config (%chat-config))
         (project-root (and (config-p config)
                            (config-project-root config)))
         (tools (or (chat-ui-state-stream-tools chat-state)
                    *toolset*))
         (working-directory (or (ignore-errors (uiop:getcwd))
                                *default-pathname-defaults*))
         (assembled
           (ignore-errors
             (assemble-system-prompt
              :project-root project-root
              :cwd working-directory
              :toolset tools))))
    (or assembled
        (chat-ui-state-stream-system-prompt chat-state)
        +chat-stream-default-system-prompt+)))

(defun %make-stream-chunk-hook-callback ()
  (let ((chunk-index 0)
        (total-tokens 0))
    (lambda (chunk)
      (when (and (stringp chunk) (plusp (length chunk)))
        (incf chunk-index)
        (incf total-tokens (%token-stream-estimate-token-count chunk))
        (hook-chain :on-stream-chunk chunk chunk-index total-tokens))
      nil)))

(defun %start-streaming-assistant-response (chat-state user-message)
  (when (and (pseudopod:message-p user-message)
             (not (token-stream-active-p (chat-ui-state-stream-state chat-state))))
    (let ((runner (chat-ui-state-stream-runner chat-state)))
      (if (functionp runner)
          (let* ((prompt (%message-content->text user-message))
                 (history
                   (remove-if
                    (lambda (message)
                      (and (pseudopod:message-p message)
                           (string-equal (or (pseudopod:message-role message) "")
                                         "assistant")
                           (%blank-string-p (%message-content->text message))))
                    (copy-list (chat-ui-state-messages chat-state))))
                 (target-index (length history))
                 (stream-state (chat-ui-state-stream-state chat-state))
                 (system-prompt (%resolve-chat-system-prompt chat-state)))
            (setf (chat-ui-state-stream-system-prompt chat-state) system-prompt)
            (setf (chat-ui-state-stream-scroll-follow-p chat-state) t)
            (streaming-markdown-renderer-reset
             (chat-ui-state-stream-markdown-renderer chat-state))
            (setf (chat-ui-state-stream-response-chunks chat-state) '())
            (conversation-transition! (%ensure-chat-conversation-state chat-state)
                                      :streaming)
            (chat-ui-add-message chat-state "assistant" "" :partial t)
            (%clear-stream-tool-tracking! chat-state)
            (token-stream-start
             stream-state
             (lambda (active-stream-state)
               (let ((*stream-chunk-hook-callback* (%make-stream-chunk-hook-callback)))
                 (funcall runner
                          active-stream-state
                          prompt
                          history
                          :system-prompt system-prompt
                          :client (chat-ui-state-stream-client chat-state)
                          :tools (%resolve-chat-tools chat-state))))
             :target-message-index target-index
             :budget-abort-threshold-percent
             (%stream-budget-abort-threshold-percent chat-state)))
          (%start-step-loop-assistant-response chat-state)))))

(defun %styled-segments->text (segments)
  (with-output-to-string (out)
    (dolist (segment segments)
      (write-string (if (compact-segment-p segment)
                        (compact-segment-text segment)
                        (or (getf segment :text) ""))
                    out))))

(defvar *%styled-lines-cache* (make-hash-table :test #'equal)
  "Cache of rendered styled lines for completed assistant messages.
Key: (message-index . content-width), Value: styled-lines list.")

(defvar *%styled-lines-cache-generation* 0
  "Incremented when the message list changes, invalidating the cache.")

(defvar *%message-wrap-cache* (make-hash-table :test #'equal)
  "Cache of wrapped message lines to avoid re-wrapping every frame.
Key: (message-content-hash . width), Value: wrapped-lines list.")

(defvar *%message-wrap-cache-size* 0
  "Track cache size to prevent unbounded growth.")

(defparameter +max-message-wrap-cache-entries+ 500
  "Maximum number of entries in the message wrap cache.")

(defun %invalidate-styled-lines-cache ()
  "Call when messages are added or modified."
  (incf *%styled-lines-cache-generation*)
  (clrhash *%styled-lines-cache*)
  ;; Also clear wrap cache when messages change
  (clrhash *%message-wrap-cache*)
  (setf *%message-wrap-cache-size* 0)
  ;; Clear per-message entry cache
  (when (boundp '*%message-entry-cache*)
    (%invalidate-message-entry-cache)))

(defun %message-content-hash (message)
  "Generate a simple hash for message content caching."
  (let ((content (%message-content->text message))
        (role (pseudopod:message-role message)))
    (sxhash (concatenate 'string (or role "") "|" (or content "")))))

(defun %get-cached-wrapped-lines (message width)
  "Get cached wrapped lines for a message, or nil if not cached."
  (when (> *%message-wrap-cache-size* +max-message-wrap-cache-entries+)
    ;; Simple LRU eviction: clear half the cache when it gets too big
    (clrhash *%message-wrap-cache*)
    (setf *%message-wrap-cache-size* 0)
    (return-from %get-cached-wrapped-lines nil))
  (let* ((hash (%message-content-hash message))
         (key (cons hash width)))
    (gethash key *%message-wrap-cache*)))

(defun %set-cached-wrapped-lines (message width lines)
  "Cache wrapped lines for a message."
  (when (< *%message-wrap-cache-size* +max-message-wrap-cache-entries+)
    (let* ((hash (%message-content-hash message))
           (key (cons hash width)))
      (unless (gethash key *%message-wrap-cache*)
        (incf *%message-wrap-cache-size*))
      (setf (gethash key *%message-wrap-cache*) lines))))

;;; --- Per-message line-entry cache ---
;;; During streaming, %message-line-entries is called every render.  Completed
;;; messages produce identical entries each time, so caching them avoids O(n)
;;; allocation per frame where n = total lines across all completed messages.
(defvar *%message-entry-cache* (make-hash-table :test #'equal))

(defun %invalidate-message-entry-cache ()
  (clrhash *%message-entry-cache*))

(defun %make-message-entry-block (entries)
  (cons (length entries) entries))

(defun %message-entry-block-count (block)
  (car block))

(defun %message-entry-block-entries (block)
  (cdr block))

(defun %assistant-message-styled-lines (chat-state message message-index content-width)
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (stream-active-p (token-stream-active-p stream-state))
         (target-index (token-stream-state-target-message-index stream-state))
         (partialp (not (null (pseudopod:message-partial message))))
         (streaming-target-p
           (and partialp
                stream-active-p
                (integerp target-index)
                (= target-index message-index)))
         (cursor-visible-p
           (and streaming-target-p
                (stream-cursor-visible-p stream-state))))
    (if streaming-target-p
        ;; Active streaming — cache until content changes
        (let* ((renderer (chat-ui-state-stream-markdown-renderer chat-state))
               (pending-len (length (streaming-markdown-renderer-pending-line renderer)))
               (line-count (length (streaming-markdown-renderer-wrapped-lines renderer)))
               (cache-key (list :streaming message-index content-width
                                line-count pending-len cursor-visible-p)))
          (or (gethash cache-key *%styled-lines-cache*)
              (let ((result (streaming-markdown-renderer-render-lines
                             renderer content-width
                             :partialp partialp
                             :cursor-visible-p cursor-visible-p)))
                ;; Clear old streaming entries for this message before caching new one
                (let ((stale '()))
                  (maphash (lambda (k v)
                             (declare (ignore v))
                             (when (and (consp k) (eq (car k) :streaming)
                                        (eql (second k) message-index)
                                        (not (equal k cache-key)))
                               (push k stale)))
                           *%styled-lines-cache*)
                  (dolist (k stale) (remhash k *%styled-lines-cache*)))
                (setf (gethash cache-key *%styled-lines-cache*) result))))
        ;; Completed message — cache the styled lines
        (let ((cache-key (cons message-index content-width)))
          (or (gethash cache-key *%styled-lines-cache*)
              (setf (gethash cache-key *%styled-lines-cache*)
                    (stream-markdown-styled-lines (%message-content->text message)
                                                  content-width
                                                  :partialp partialp
                                                  :cursor-visible-p cursor-visible-p)))))))

(defun %lerp-rgb (from-r from-g from-b to-r to-g to-b t-val)
  "Linear interpolation between two RGB colors. Returns 3 values (r g b)."
  (let ((clamped (max 0.0 (min 1.0 (coerce t-val 'single-float)))))
    (values (round (+ from-r (* clamped (- to-r from-r))))
            (round (+ from-g (* clamped (- to-g from-g))))
            (round (+ from-b (* clamped (- to-b from-b)))))))

(defun %gradient-styled-segments (text from-rgb to-rgb &key boldp)
  "Return a list of styled segments with per-character gradient color.
FROM-RGB and TO-RGB are lists of (r g b)."
  (let* ((len (length text))
         (segments '()))
    (loop for i from 0 below len
          for ch = (char text i)
          do (let ((t-val (if (<= len 1) 0.0 (/ (float i) (float (1- len))))))
               (multiple-value-bind (r g b)
                   (%lerp-rgb (first from-rgb) (second from-rgb) (third from-rgb)
                              (first to-rgb) (second to-rgb) (third to-rgb)
                              t-val)
                 (push (list (string ch)
                             (%chat-template-cell
                              :fg (ptui.core.color:make-color-rgb r g b)
                              :boldp boldp))
                       segments))))
    (nreverse segments)))

;;; ---------------------------------------------------------------------------
;;; Welcome screen ASCII art with truecolor gradient
;;; ---------------------------------------------------------------------------

;; Sky blue to orange gradient colors (RGB)
(defparameter +welcome-gradient-start+ '(135 206 250))  ; Light sky blue
(defparameter +welcome-gradient-end+ '(255 165 0))      ; Orange

(defun %lerp-color (start end t-val)
  "Linearly interpolate between two RGB colors."
  (flet ((lerp-channel (a b)
           (round (+ a (* (- b a) t-val)))))
    (list (lerp-channel (first start) (first end))
          (lerp-channel (second start) (second end))
          (lerp-channel (third start) (third end)))))

(defun %line-gradient-segments (line start-rgb end-rgb &key bg-rgb)
  "Create styled segments for a line with horizontal gradient coloring.
If BG-RGB is provided, uses gradient background fills."
  (let* ((len (length line))
         (segments '()))
    (loop for i from 0 below len
          for ch = (char line i)
          for t-val = (if (<= len 1) 0.0 (/ (float i) (float (1- len))))
          for rgb = (%lerp-color start-rgb end-rgb t-val)
          for bg = (when bg-rgb (%lerp-color bg-rgb bg-rgb t-val))
          do (push (list (string ch)
                         (%chat-template-cell
                          :fg (ptui.core.color:make-color-rgb 
                               (first rgb) (second rgb) (third rgb))
                          :bg (when bg
                                (ptui.core.color:make-color-rgb
                                 (first bg) (second bg) (third bg)))
                          :boldp t))
                   segments))
    (nreverse segments)))

(defun %welcome-filled-block-line (width start-rgb end-rgb char)
  "Create a line filled entirely with a character using gradient colors."
  (let ((line (make-string width :initial-element char)))
    (%line-gradient-segments line start-rgb end-rgb)))

(defun %welcome-screen-entries (safe-width)
  "Return snapshot-stable empty-state content when there are no messages.
Displays a truecolor gradient ASCII art logo for amoebum with color fills."
  (declare (ignore safe-width))
  (let* ((entries '())
         (art-width 68)
         ;; Gradient positions for visual effect
         (light-start '(100 180 255))    ; Deeper sky blue
         (light-end '(255 200 100))      ; Light orange
         (deep-start '(70 130 200))      ; Deep blue
         (deep-end '(255 140 50)))       ; Deep orange
    
    ;; Top decorative border with half blocks
    (push (list :id :chat-welcome-top-border
                :text (make-string art-width :initial-element #\━)
                :role :system
                :styled-segments (%welcome-filled-block-line 
                                  art-width deep-start deep-end #\━))
          entries)
    
    ;; Amoeba-inspired filled shapes with gradient backgrounds
    ;; AMOEBUM text with filled blocks inside letters
    (let ((text-lines
           '(" █████╗ ███╗   ███╗ ██████╗ ███████╗██████╗ ██╗   ██╗███╗   ███╗ "
             "██╔══██╗████╗ ████║██╔═══██╗██╔════╝██╔══██╗██║   ██║████╗ ████║ "
             "███████║██╔████╔██║██║   ██║█████╗  ██████╔╝██║   ██║██╔████╔██║ "
             "██╔══██║██║╚██╔╝██║██║   ██║██╔══╝  ██╔══██╗██║   ██║██║╚██╔╝██║ "
             "██║  ██║██║ ╚═╝ ██║╚██████╔╝███████╗██████╔╝╚██████╔╝██║ ╚═╝ ██║ "
             "╚═╝  ╚═╝╚═╝     ╚═╝ ╚═════╝ ╚══════╝╚═════╝  ╚═════╝ ╚═╝     ╚═╝ ")))
      (loop for line in text-lines
            for idx from 0
            do (push (list :id (list :chat-welcome-text idx)
                           :text line
                           :role :system
                           :styled-segments (%line-gradient-segments 
                                             line 
                                             +welcome-gradient-start+
                                             +welcome-gradient-end+))
                   entries)))
    
    ;; Bottom border
    (push (list :id :chat-welcome-bottom-border
                :text (make-string art-width :initial-element #\━)
                :role :system
                :styled-segments (%welcome-filled-block-line 
                                  art-width deep-end deep-start #\━))
          entries)
    
    ;; Tagline with subtle styling
    (push (list :id :chat-welcome-tagline
                :text ""
                :role :meta)
          entries)
    (push (list :id :chat-welcome-tagline-text
                :text "              🧬 Single-celled coding assistant 🧬"
                :role :system
                :styled-segments 
                (list (list "              " (%chat-template-cell))
                      (list "🧬" (%chat-template-cell 
                                 :fg (ptui.core.color:make-color-rgb 135 206 250)
                                 :boldp t))
                      (list " Single-celled coding assistant " 
                            (%chat-template-cell))
                      (list "🧬" (%chat-template-cell 
                                 :fg (ptui.core.color:make-color-rgb 255 165 0)
                                 :boldp t))))
          entries)
    
    ;; Helper text
    (push (list :id :chat-empty-fallback
                :text ""
                :role :meta)
          entries)
    (push (list :id :chat-empty-hint
                :text ""
                :role :meta)
          entries)
    (push (list :id :chat-empty-hint-text
                :text "              Type below and press Enter to start"
                :role :meta)
          entries)
    
    (nreverse entries)))

(defun %with-left-gutter (text)
  (concatenate 'string " " (or text "")))

(defparameter +max-ui-render-messages+ 100
  "Maximum number of messages to render in the UI.
   Older messages are skipped to maintain performance.
   The full history is still kept for LLM context.")

(defun %message-line-entries-for-one (chat-state message index safe-width is-last-p)
  "Generate line entries for a single message in display order."
  (declare (ignore is-last-p))
  (let ((entries '()))
    (let* ((role (%normalize-chat-role (pseudopod:message-role message)))
           (role-key (intern (string-upcase role) :keyword))
           (prefix (let ((badge (chat-role-prefix role)))
                     (if (plusp (length badge))
                         (format nil " [~A] " badge)
                         " > ")))
           (prefix-width (ptui.text.width:string-width prefix))
           (content-width (max 1 (- safe-width prefix-width)))
           (indent (make-string prefix-width :initial-element #\Space))
           (label-role role-key))
      (if (string= role "assistant")
          (let ((styled-lines
                  (%assistant-message-styled-lines
                   chat-state message index content-width)))
            (loop for styled-line in styled-lines
                  for line-index from 0
                  do
                     (let* ((prefix-text (if (zerop line-index) prefix indent))
                   (prefix-segment
                              (cons prefix-text
                                    (intern-style
                                     (if (zerop line-index) label-role role-key)
                                     :boldp (and (zerop line-index)
                                                 (not (string= role "system"))))))
                            (content-segments
                              (or styled-line
                                  (list (cons "" (intern-style role-key)))))
                            (segments (append (list prefix-segment)
                                              content-segments)))
                       (push (list :id (list :chat-message index line-index)
                                   :text (%styled-segments->text segments)
                                   :role role-key
                                   :styled-segments segments)
                             entries))))
          ;; Use cache for wrapped lines to avoid re-wrapping every frame
          (let* ((body (%message-content->text message))
                 (cached (%get-cached-wrapped-lines message content-width))
                 (wrapped (or cached
                              (let ((lines (ptui.text.layout:wrap-by-width body content-width)))
                                (setf lines (if (null lines) (list "") lines))
                                (%set-cached-wrapped-lines message content-width lines)
                                lines))))
            (loop for line in wrapped
                  for line-index from 0
                  do
                     (let* ((prefix-part (if (zerop line-index) prefix indent))
                            (line-with-gutter (concatenate 'string prefix-part line))
                            (prefix-segment
                              (cons prefix-part
                                    (intern-style
                                     (if (zerop line-index) label-role role-key)
                                     :boldp (and (zerop line-index)
                                                 (not (string= role "system"))))))
                            (body-seg (cons line (intern-style role-key)))
                            (segments (list prefix-segment body-seg)))
                       (push (list :id (list :chat-message index line-index)
                                   :text line-with-gutter
                                   :role role-key
                                   :styled-segments segments)
                             entries)))))
    (nreverse entries))))

(defun %message-entry-cacheable-p (chat-state message)
  "A message is cacheable if it is NOT the active streaming target."
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (partialp (pseudopod:message-partial message)))
    (not (and partialp (token-stream-active-p stream-state)))))

(defun %get-cached-message-entries (index width)
  (when (boundp '*%message-entry-cache*)
    (gethash (cons index width)
             *%message-entry-cache*)))

(defun %set-cached-message-entries (index width block)
  (when (boundp '*%message-entry-cache*)
    (setf (gethash (cons index width)
                   *%message-entry-cache*)
          block)))

(defun %message-gap-block (index)
  (%make-message-entry-block
   (list (list :id (list :chat-gap index)
               :text ""
               :role :meta))))

(defun %message-preview-entry (preview)
  (let ((entry (copy-list preview)))
    (setf (getf entry :text)
          (%with-left-gutter (getf preview :text "")))
    entry))

(defun %message-entry-blocks (chat-state messages width)
  (let* ((safe-width (max 1 (1- width)))
         (start-index (max 0 (- (length messages) +max-ui-render-messages+)))
         (total-messages (length messages))
         (blocks '())
         (total-lines 0))
    (flet ((push-block (block)
             (push block blocks)
             (incf total-lines (%message-entry-block-count block))))
      (when (> total-messages +max-ui-render-messages+)
        (let* ((skipped (- total-messages +max-ui-render-messages+))
               (entries (list (list :id :chat-skipped-messages-indicator
                                    :text (format nil " ... (~D older messages not shown) ..." skipped)
                                    :role :system)))
               (block (%make-message-entry-block entries)))
          (push-block block)))
      (loop for index from start-index below total-messages
            for message in (nthcdr start-index messages)
            do
               (let* ((is-last-p (= index (1- total-messages)))
                      (cacheable (%message-entry-cacheable-p chat-state message))
                      (cached-block (and cacheable
                                         (%get-cached-message-entries
                                          index
                                          safe-width)))
                      (msg-block
                        (or cached-block
                            (let* ((fresh-entries (%message-line-entries-for-one
                                                   chat-state message index safe-width is-last-p))
                                   (fresh-block (%make-message-entry-block fresh-entries)))
                              (when cacheable
                                (%set-cached-message-entries
                                 index
                                 safe-width
                                 fresh-block))
                              fresh-block))))
                 (push-block msg-block)
                 (unless is-last-p
                   (push-block (%message-gap-block index)))))
      (let ((preview-lines (%stream-tool-call-preview-lines chat-state safe-width)))
        (when preview-lines
          (when (> total-lines 0)
            (let ((gap-block (%make-message-entry-block
                              (list (list :id :chat-stream-tool-gap :text "" :role :meta)))))
              (push-block gap-block)))
          (let* ((preview-entries (mapcar #'%message-preview-entry preview-lines))
                 (preview-block (%make-message-entry-block preview-entries)))
            (push-block preview-block))))
      (unless blocks
        (let* ((welcome-entries (%welcome-screen-entries safe-width))
               (welcome-block (%make-message-entry-block welcome-entries)))
          (push-block welcome-block)))
      (values (nreverse blocks) total-lines))))

(defun %entry-list-slice (entries start count)
  (let ((tail (nthcdr start entries))
        (slice '()))
    (loop repeat count
          while tail
          do (push (car tail) slice)
             (setf tail (cdr tail)))
    (nreverse slice)))

(defun %message-line-window (chat-state messages width viewport-height scrollback-lines)
  (multiple-value-bind (blocks total-lines)
      (%message-entry-blocks chat-state messages width)
    (multiple-value-bind (offset new-scrollback max-scrollback)
        (%compute-scroll-offset total-lines viewport-height scrollback-lines)
      (let ((window-end (+ offset viewport-height))
            (cursor 0)
            (visible '()))
        (dolist (block blocks)
          (let* ((block-count (%message-entry-block-count block))
                 (block-end (+ cursor block-count))
                 (slice-start (max offset cursor))
                 (slice-end (min window-end block-end)))
            (when (< slice-start slice-end)
              (setf visible
                    (nconc visible
                           (%entry-list-slice (%message-entry-block-entries block)
                                              (- slice-start cursor)
                                              (- slice-end slice-start)))))
            (setf cursor block-end)
            (when (>= cursor window-end)
              (return))))
        (values visible total-lines offset new-scrollback max-scrollback)))))

(defun %message-line-entries (chat-state messages width)
  (multiple-value-bind (blocks total-lines)
      (%message-entry-blocks chat-state messages width)
    (declare (ignore total-lines))
    (let ((entries '()))
      (dolist (block blocks entries)
        (setf entries
              (nconc entries
                     (%entry-list-slice (%message-entry-block-entries block)
                                        0
                                        (%message-entry-block-count block))))))))

(defun %chat-text-widget (text id role &key styled-segments)
  (ptui.ui.elements:make-element
   :text
   :id id
   :props (list :text text :role role :styled-segments styled-segments)
   :children '()))

(defun %compute-scroll-offset (total-lines viewport-height scrollback-lines)
  (let* ((max-scrollback (max 0 (- total-lines viewport-height)))
         (bounded-scrollback (max 0 (min max-scrollback scrollback-lines)))
         (offset (- max-scrollback bounded-scrollback)))
    (values offset bounded-scrollback max-scrollback)))

(defun %chat-plan-mode-enabled-p ()
  (not (null (cfg :plan-mode))))

(defun %chat-plan-execution-surface-active-p (&optional (execution-state (current-plan-execution-state)))
  (and (plan-execution-state-p execution-state)
       (let ((run-id (plan-execution-state-run-id execution-state))
             (status (plan-execution-state-status execution-state))
             (continuity (plan-execution-state-continuity-output execution-state))
             (steps (plan-execution-state-steps execution-state)))
         (or (and (stringp run-id)
                  (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) run-id))))
             (and (keywordp status)
                  (not (eq status :idle)))
             continuity
             steps))))

(defun %chat-plan-workspace-visible-p (plan-state execution-state)
  (and (plan-mode-state-p plan-state)
       (plan-mode-state-steps plan-state)
       (or (%chat-plan-mode-enabled-p)
           (%chat-plan-execution-surface-active-p execution-state))))

(defun %chat-plan-presentation-safe-string (value &optional (fallback ""))
  (cond
    ((and (stringp value)
          (plusp (length value)))
     value)
    ((null value)
     fallback)
    (t
     (princ-to-string value))))

(defun %chat-plan-inline-code-spans (text)
  (let* ((source (%chat-plan-presentation-safe-string text ""))
         (length (length source))
         (index 0)
         (spans '()))
    (loop while (< index length) do
      (let ((start (position #\` source :start index)))
        (if (null start)
            (setf index length)
            (let ((end (position #\` source :start (1+ start))))
              (if (null end)
                  (setf index length)
                  (let ((snippet
                          (string-trim '(#\Space #\Tab #\Newline #\Return)
                                       (subseq source (1+ start) end))))
                    (when (plusp (length snippet))
                      (push snippet spans))
                    (setf index (1+ end))))))))
    (nreverse spans)))

(defun %chat-plan-leading-token (text)
  (let* ((trimmed
           (string-trim '(#\Space #\Tab #\Newline #\Return)
                        (%chat-plan-presentation-safe-string text "")))
         (length (length trimmed)))
    (if (zerop length)
        ""
        (let ((end
                (or (position-if #'%whitespace-char-p trimmed)
                    length)))
          (string-downcase (subseq trimmed 0 end))))))

(defun %chat-plan-commandish-p (text)
  (let* ((trimmed
           (string-trim '(#\Space #\Tab #\Newline #\Return)
                        (%chat-plan-presentation-safe-string text "")))
         (length (length trimmed))
         (token (%chat-plan-leading-token trimmed)))
    (and (plusp length)
         (or (member token +chat-plan-command-heads+ :test #'string=)
             (and (>= length 2)
                  (string= (subseq trimmed 0 2) "./"))
             (and (>= length 2)
                  (string= (subseq trimmed 0 2) "~/"))
             (char= (char trimmed 0) #\/)
             (search "&&" trimmed :test #'char=)
             (search "||" trimmed :test #'char=)
             (search "|" trimmed :test #'char=)
             (search ";" trimmed :test #'char=)
             (search ">" trimmed :test #'char=)
             (search "<" trimmed :test #'char=)
             (and (>= length 3)
                  (string= (subseq trimmed (- length 3)) ".sh"))))))

(defun %chat-plan-step-command-previews (step)
  (check-type step plan-step)
  (let* ((description (%chat-plan-presentation-safe-string
                       (plan-step-description step)
                       ""))
         (inline-spans (%chat-plan-inline-code-spans description)))
    (remove-duplicates
     (loop for span in inline-spans
           for normalized = (string-trim '(#\Space #\Tab #\Newline #\Return)
                                         (%normalize-inline-text span))
           when (%chat-plan-commandish-p normalized)
             collect normalized)
     :test #'string=)))

(defun %chat-plan-sorted-steps (plan-state)
  (sort (copy-list (or (plan-mode-state-steps plan-state) '()))
        #'<
        :key #'plan-step-index))

(defun %chat-plan-visible-steps (plan-state)
  (let* ((sorted-steps (%chat-plan-sorted-steps plan-state))
         (visible-count (min (length sorted-steps) +chat-plan-presentation-max-steps+)))
    (subseq sorted-steps 0 visible-count)))

(defun %chat-plan-normalize-path-list (paths)
  (remove nil
          (loop for path in (or paths '())
                for text = (%chat-plan-presentation-safe-string path "")
                for trimmed = (string-trim '(#\Space #\Tab #\Newline #\Return) text)
                when (plusp (length trimmed))
                  collect trimmed)))

(defun %chat-plan-rationale-snippet (step)
  (check-type step plan-step)
  (%truncate-inline-text
   (%chat-plan-presentation-safe-string (plan-step-description step) "")
   +chat-plan-rationale-snippet-chars+))

(defun %chat-plan-step-by-index (steps step-index)
  (when (integerp step-index)
    (find step-index (or steps '()) :key #'plan-step-index :test #'=)))

(defun %chat-plan-resolve-selected-step-index (chat-state plan-state visible-steps)
  (let* ((visible-indexes
           (loop for step in (or visible-steps '())
                 for index = (plan-step-index step)
                 when (integerp index)
                   collect index))
         (approved-visible-indexes
           (loop for index in (plan-mode-state-approved-step-indexes plan-state)
                 when (member index visible-indexes :test #'=)
                   collect index))
         (current-selection (chat-ui-state-plan-selected-step-index chat-state))
         (resolved-selection
           (cond
             ((and (integerp current-selection)
                   (member current-selection visible-indexes :test #'=))
              current-selection)
             (approved-visible-indexes
              (first approved-visible-indexes))
             (visible-indexes
              (first visible-indexes))
             (t
              nil))))
    (setf (chat-ui-state-plan-selected-step-index chat-state) resolved-selection)
    resolved-selection))

(defun %chat-plan-move-selection! (chat-state delta)
  (let* ((plan-state (current-plan-mode-state))
         (execution-state (current-plan-execution-state)))
    (unless (and (%chat-plan-workspace-visible-p plan-state execution-state)
                 (plan-mode-state-p plan-state)
                 (plan-mode-state-steps plan-state)
                 (integerp delta)
                 (/= delta 0))
      (return-from %chat-plan-move-selection! nil))
    (let* ((visible-steps (%chat-plan-visible-steps plan-state))
           (visible-indexes
             (loop for step in visible-steps
                   for index = (plan-step-index step)
                   when (integerp index)
                     collect index)))
      (unless visible-indexes
        (setf (chat-ui-state-plan-selected-step-index chat-state) nil)
        (return-from %chat-plan-move-selection! nil))
      (let* ((current-index
               (%chat-plan-resolve-selected-step-index chat-state
                                                      plan-state
                                                      visible-steps))
             (current-position
               (or (position current-index visible-indexes :test #'=)
                   0))
             (next-position
               (min (1- (length visible-indexes))
                    (max 0 (+ current-position delta))))
             (next-index (nth next-position visible-indexes)))
        (setf (chat-ui-state-plan-selected-step-index chat-state) next-index)
        t))))

(defun %chat-plan-command-preview-lines (plan-state selected-step-index)
  (let* ((steps (%chat-plan-sorted-steps plan-state))
         (approved-indexes (plan-mode-state-approved-step-indexes plan-state))
         (entries '()))
    (dolist (step steps)
      (let* ((step-index (or (plan-step-index step) 0))
             (approved-p (member step-index approved-indexes :test #'=)))
        (dolist (command (%chat-plan-step-command-previews step))
          (push (format nil
                        "DRY-RUN> [step ~D ~A~:[~; | selected~] | non-executed] ~A"
                        step-index
                        (if approved-p "approved" "pending")
                        (and (integerp selected-step-index)
                             (= step-index selected-step-index))
                        command)
                entries))))
    (let ((ordered (nreverse entries)))
      (subseq ordered
              0
              (min (length ordered)
                   +chat-plan-command-preview-max-lines+)))))

(defun %chat-plan-step-status-from-execution-step (execution-step approved-p)
  (if execution-step
      (case (plan-execution-step-status execution-step)
        (:running :running)
        (:completed :done)
        (:done :done)
        (:blocked :blocked)
        (:failed :blocked)
        (:aborted :blocked)
        (otherwise :pending))
      (if approved-p :approved :pending)))

(defun %chat-plan-step-status-event-table (chat-state execution-state)
  (let* ((run-id (and (plan-execution-state-p execution-state)
                      (plan-execution-state-run-id execution-state)))
         (bus (and chat-state (%context-event-bus chat-state))))
    (unless (and (%chat-plan-execution-surface-active-p execution-state)
                 (event-bus-p bus)
                 (stringp run-id)
                 (plusp (length run-id)))
      (return-from %chat-plan-step-status-event-table nil))
    (let ((table (make-hash-table :test #'eql)))
      (dolist (event (event-history bus))
        (when (eq (event-type event) +event-type-plan-step-status+)
          (let ((payload (event-payload event)))
            (when (and (plan-step-status-payload-p payload)
                       (integerp (plan-step-status-payload-step-index payload))
                       (stringp (plan-step-status-payload-run-id payload))
                       (string= (plan-step-status-payload-run-id payload) run-id))
              (setf (gethash (plan-step-status-payload-step-index payload) table)
                    (case (plan-step-status-payload-status payload)
                      (:running :running)
                      (:blocked :blocked)
                      (:done :done)
                      (otherwise :pending)))))))
      table)))

(defun %chat-plan-step-status-event-signature (chat-state execution-state)
  (let* ((run-id (and (plan-execution-state-p execution-state)
                      (plan-execution-state-run-id execution-state)))
         (bus (and chat-state (%context-event-bus chat-state))))
    (unless (and (%chat-plan-execution-surface-active-p execution-state)
                 (event-bus-p bus)
                 (stringp run-id)
                 (plusp (length run-id)))
      (return-from %chat-plan-step-status-event-signature nil))
    (loop for event in (event-history bus)
          for payload = (event-payload event)
          when (and (eq (event-type event) +event-type-plan-step-status+)
                    (plan-step-status-payload-p payload)
                    (integerp (plan-step-status-payload-step-index payload))
                    (stringp (plan-step-status-payload-run-id payload))
                    (string= (plan-step-status-payload-run-id payload) run-id))
            collect (list (event-seq event)
                          (plan-step-status-payload-step-index payload)
                          (plan-step-status-payload-status payload)))))

(defun %chat-plan-presentation-steps (plan-state visible-steps execution-state &optional chat-state)
  (let ((execution-step-table
          (when (%chat-plan-execution-surface-active-p execution-state)
            (let ((table (make-hash-table :test #'eql)))
              (dolist (execution-step (plan-execution-state-steps execution-state))
                (let ((step-index (plan-execution-step-index execution-step)))
                  (when (integerp step-index)
                    (setf (gethash step-index table) execution-step))))
              table)))
        (event-status-table (%chat-plan-step-status-event-table chat-state execution-state)))
    (loop for step in visible-steps
          for step-index = (or (plan-step-index step) 0)
          for execution-step = (and execution-step-table
                                    (gethash step-index execution-step-table))
          for approved-p = (if execution-step
                               (plan-execution-step-approved-p execution-step)
                               (member step-index
                                       (plan-mode-state-approved-step-indexes plan-state)
                                       :test #'=))
          for fallback-status = (%chat-plan-step-status-from-execution-step
                                 execution-step
                                 approved-p)
          for event-status = (and event-status-table
                                  (gethash step-index event-status-table))
          for status = (or event-status fallback-status)
          collect
          (ptui.components.plan-presentation:make-plan-presentation-step
           :index step-index
           :description (%chat-plan-presentation-safe-string
                         (plan-step-description step)
                         "Describe this step.")
           :approved-p (not (null approved-p))
           :file-paths (%chat-plan-normalize-path-list (plan-step-file-paths step))
           :rationale-snippet (%chat-plan-rationale-snippet step)
           :risk (or (plan-step-risk step) :medium)
           :status status))))

(defun %chat-plan-presentation-output-lines (plan-state selected-step-index)
  (let* ((steps (or (plan-mode-state-steps plan-state) '()))
         (total (length steps))
         (approved (length (plan-mode-state-approved-step-indexes plan-state)))
         (selected-step (%chat-plan-step-by-index steps selected-step-index))
         (selected-file-path
           (car (%chat-plan-normalize-path-list
                 (and selected-step (plan-step-file-paths selected-step)))))
         (selected-step-line
           (when (integerp selected-step-index)
             (if selected-file-path
                 (format nil
                         "Selected step: ~D (Ctrl-N/Ctrl-P to change) | file ~A"
                         selected-step-index
                         selected-file-path)
                 (format nil "Selected step: ~D (Ctrl-N/Ctrl-P to change)"
                         selected-step-index))))
         (decision-text
           (string-downcase
            (symbol-name (or (plan-mode-state-review-decision plan-state)
                             :pending))))
         (pending-p (plan-mode-state-review-pending-p plan-state))
         (command-previews (%chat-plan-command-preview-lines plan-state
                                                             selected-step-index)))
    (if command-previews
        (if selected-step-line
            (cons selected-step-line command-previews)
            command-previews)
        (append
         (when selected-step-line
           (list selected-step-line))
         (list "Plan mode active. Mutating tools remain blocked."
              (format nil "Review decision: ~A~:[~; (pending)~]" decision-text pending-p)
              (format nil "Step approvals: ~D/~D" approved total)
              "DRY-RUN> [non-executed] No command snippets detected in proposed steps yet.")))))

(defun %chat-plan-output-stdin-capture-policy ()
  (if (or (%chat-plan-mode-enabled-p)
          (%chat-plan-execution-surface-active-p))
      :disabled
      :enabled))

(defun %chat-plan-execution-output-line-entries (execution-state)
  (when (%chat-plan-execution-surface-active-p execution-state)
    (loop for entry in (plan-execution-state-continuity-output execution-state)
          collect (list :text (%chat-plan-presentation-safe-string
                               (plan-execution-output-entry-line entry)
                               "")
                        :step-index (plan-execution-output-entry-step-index entry)
                        :severity (or (plan-execution-output-entry-severity entry) :info)
                        :style (or (plan-execution-output-entry-style entry) :plain)
                        :recovery-actions
                        (copy-list (or (plan-execution-output-entry-recovery-actions entry)
                                       '()))))))

(defun %chat-plan-format-elapsed-seconds (elapsed-seconds)
  (let* ((total-seconds (max 0 (or elapsed-seconds 0)))
         (hours (truncate total-seconds 3600))
         (remaining (mod total-seconds 3600))
         (minutes (truncate remaining 60))
         (seconds (mod remaining 60)))
    (cond
      ((> hours 0)
       (format nil "~Dh ~Dm ~Ds" hours minutes seconds))
      ((> minutes 0)
       (format nil "~Dm ~Ds" minutes seconds))
      (t
       (format nil "~Ds" seconds)))))

(defun %chat-plan-execution-elapsed-seconds (execution-state)
  (let* ((started-at (plan-execution-state-started-at execution-state))
         (finished-at (plan-execution-state-finished-at execution-state))
         (status (plan-execution-state-status execution-state))
         (end-time (if (member status '(:completed :failed :aborted) :test #'eq)
                       finished-at
                       (get-universal-time))))
    (if (and (integerp started-at)
             (integerp end-time))
        (max 0 (- end-time started-at))
        0)))

(defun %chat-plan-execution-progress-line (execution-state)
  (let* ((approved-indexes (or (plan-execution-state-approved-step-indexes execution-state) '()))
         (total (length approved-indexes)))
    (unless (plusp total)
      (return-from %chat-plan-execution-progress-line
        "Execution progress: step 0 of 0 (elapsed 0s)"))
    (let* ((current-index (plan-execution-state-current-step-index execution-state))
           (current-position (and (integerp current-index)
                                  (position current-index approved-indexes :test #'=)))
           (completed (length (plan-execution-state-completed-step-indexes execution-state)))
           (status (plan-execution-state-status execution-state))
           (step-number
             (cond
               ((integerp current-position)
                (1+ current-position))
               ((eq status :completed)
                total)
               ((plusp completed)
                (min total (1+ completed)))
               (t
                1)))
           (elapsed-seconds (%chat-plan-execution-elapsed-seconds execution-state)))
      (format nil "Execution progress: step ~D of ~D (elapsed ~A)"
              step-number
              total
              (%chat-plan-format-elapsed-seconds elapsed-seconds)))))

(defun %chat-plan-presentation-context-lines (plan-state selected-step visible-steps execution-state)
  (let* ((steps (or (plan-mode-state-steps plan-state) '()))
         (high-risk-count
           (count-if (lambda (step)
                       (eq (plan-step-risk step) :high))
                     steps))
         (flattened-file-paths
           (remove-duplicates
            (loop for step in steps
                  append (or (plan-step-file-paths step) '()))
            :test #'string=))
         (notes (plan-mode-state-review-notes plan-state))
         (extra-step-count
           (max 0 (- (length steps) +chat-plan-presentation-max-steps+))))
    (append
     (list (format nil "Captured steps: ~D~:[~; (showing first ~D)~]"
                   (length steps)
                   (> extra-step-count 0)
                   +chat-plan-presentation-max-steps+)
           (format nil "Visible steps: ~D" (length visible-steps))
           (format nil "High-risk steps: ~D" high-risk-count)
           (if flattened-file-paths
               (format nil "Referenced files: ~{~A~^, ~}"
                       (subseq flattened-file-paths
                               0
                               (min 3 (length flattened-file-paths))))
               "Referenced files: none")
           (format nil "Review notes: ~A"
                   (%chat-plan-presentation-safe-string notes "none"))
           "Selection controls: Ctrl-N next, Ctrl-P previous.")
     (when selected-step
       (list (format nil "Selected rationale chars: ~D"
                     (length (%chat-plan-rationale-snippet selected-step)))))
     (when (%chat-plan-execution-surface-active-p execution-state)
       (list (format nil "Execution run: ~A"
                     (%chat-plan-presentation-safe-string
                      (plan-execution-state-run-id execution-state)
                      "none"))
             (format nil "Run status: ~A"
                     (string-downcase
                      (symbol-name (or (plan-execution-state-status execution-state)
                                       :idle))))
             (%chat-plan-execution-progress-line execution-state)
             (format nil "Execution progress: done ~D / pending ~D"
                     (length (plan-execution-state-completed-step-indexes execution-state))
                     (length (plan-execution-state-pending-step-indexes execution-state))))))))

(defun %chat-plan-presentation-widget (plan-state chat-state)
  (let ((execution-state (current-plan-execution-state)))
    (when (%chat-plan-workspace-visible-p plan-state execution-state)
    (let* ((visible-steps (%chat-plan-visible-steps plan-state))
           (selected-step-index (%chat-plan-resolve-selected-step-index
                                 chat-state
                                 plan-state
                                 visible-steps))
           (selected-step (%chat-plan-step-by-index visible-steps
                                                    selected-step-index))
           (output-line-entries (%chat-plan-execution-output-line-entries execution-state))
           (output-lines (if output-line-entries
                             nil
                             (%chat-plan-presentation-output-lines plan-state
                                                                  selected-step-index))))
      (ptui.components.plan-presentation:make-plan-mode-presentation-widget
       :id :chat-plan-presentation
       :steps (%chat-plan-presentation-steps plan-state
                                             visible-steps
                                             execution-state
                                             chat-state)
       :selected-step-index selected-step-index
       :output-lines output-lines
       :output-line-entries output-line-entries
       :output-stdin-capture-policy (%chat-plan-output-stdin-capture-policy)
       :context-lines (%chat-plan-presentation-context-lines plan-state
                                                             selected-step
                                                             visible-steps
                                                             execution-state)
       :output-viewport-height +chat-plan-presentation-output-viewport-height+)))))

(defun %chat-plan-workspace-tree-key (chat-state)
  (let* ((plan-state (current-plan-mode-state))
         (execution-state (current-plan-execution-state)))
    (list (%chat-plan-mode-enabled-p)
          (chat-ui-state-plan-selected-step-index chat-state)
          (loop for step in (or (plan-mode-state-steps plan-state) '())
                collect (list (plan-step-index step)
                              (plan-step-description step)
                              (copy-list (or (plan-step-file-paths step) '()))
                              (plan-step-risk step)))
          (copy-list (or (plan-mode-state-approved-step-indexes plan-state) '()))
          (plan-mode-state-review-decision plan-state)
          (plan-mode-state-review-pending-p plan-state)
          (%chat-plan-execution-surface-active-p execution-state)
          (plan-execution-state-run-id execution-state)
          (plan-execution-state-status execution-state)
          (plan-execution-state-current-step-index execution-state)
          (and (%chat-plan-execution-surface-active-p execution-state)
               (%chat-plan-execution-elapsed-seconds execution-state))
          (%chat-plan-step-status-event-signature chat-state execution-state)
          (copy-list (or (plan-execution-state-pending-step-indexes execution-state) '()))
          (copy-list (or (plan-execution-state-completed-step-indexes execution-state) '()))
          (loop for entry in (or (plan-execution-state-continuity-output execution-state) '())
                collect (list (plan-execution-output-entry-line entry)
                              (plan-execution-output-entry-step-index entry)
                              (plan-execution-output-entry-style entry)
                              (plan-execution-output-entry-severity entry)
                              (plan-execution-output-entry-phase entry)
                              (plan-execution-output-entry-timestamp entry))))))

(defun %chat-template-cell (&key (fg :default) (bg :default) (boldp nil))
  (ptui.core.types:make-cell
   " "
   fg
   bg
   (ptui.core.types:make-attrs :boldp boldp)))

(defun %chat-cell-with-attrs (cell
                              &key
                                boldp
                                italicp
                                underlinep
                                invertp
                                dimp
                                strikep)
  (let ((attrs (ptui.core.types:cell-attrs cell)))
    (ptui.core.types:make-cell
     (ptui.core.types:cell-glyph cell)
     (ptui.core.types:cell-fg cell)
     (ptui.core.types:cell-bg cell)
     (ptui.core.types:make-attrs
      :boldp (or (ptui.core.types:attrs-boldp attrs) (not (null boldp)))
      :italicp (or (ptui.core.types:attrs-italicp attrs) (not (null italicp)))
      :underlinep (or (ptui.core.types:attrs-underlinep attrs) (not (null underlinep)))
      :invertp (or (ptui.core.types:attrs-invertp attrs) (not (null invertp)))
      :dimp (or (ptui.core.types:attrs-dimp attrs) (not (null dimp)))
      :strikep (or (ptui.core.types:attrs-strikep attrs) (not (null strikep)))))))

(defun chat-role-cell (role
                       &key
                         (focusp nil)
                         boldp
                         italicp
                         underlinep
                         invertp
                         dimp
                         strikep)
  (let* ((role-key (intern (string-upcase (princ-to-string role)) :keyword))
         (theme ptui.core.theme:*active-theme*)
         (base (if theme
                   (ptui.core.theme:theme-role-cell theme role-key)
                   ;; fallback when no theme is active
                   (%chat-template-cell :fg (ptui.core.color:make-color-rgb 175 175 175))))
         (styled (%chat-cell-with-attrs base
                                        :boldp boldp
                                        :italicp italicp
                                        :underlinep underlinep
                                        :invertp invertp
                                        :dimp dimp
                                        :strikep strikep)))
    (if focusp
        (%chat-cell-with-attrs styled :boldp t :invertp t)
        styled)))

(defun %fit-line-width (text width)
  (ptui.text.layout:truncate-to-width text (max 0 width)))

(defun %prompt-wrapped-lines (value width)
  (if (<= width 0)
      (list "")
      (ptui.text.layout:wrap-by-width value (max 1 width)
                                       :preserve-spaces t)))

(defun %cursor-to-line-col (cursor-pos lines)
  "Given a grapheme-offset CURSOR-POS and list of wrapped LINES, return
\(values line-index col-offset) as display coordinates."
  (let ((remaining cursor-pos))
    (loop for line in lines
          for line-idx from 0
          for line-len = (length line)
          do (if (<= remaining line-len)
                 ;; Cursor is on this line. If it's exactly at line-len and
                 ;; there are more lines, it wraps to next line col 0.
                 (if (and (= remaining line-len)
                          (< (1+ line-idx) (length lines)))
                     ;; Wrap to next line
                     (return-from %cursor-to-line-col
                       (values (1+ line-idx) 0))
                     (return-from %cursor-to-line-col
                       (values line-idx remaining)))
                 (decf remaining line-len)))
    ;; Past end — cursor on last line at end
    (values (max 0 (1- (length lines)))
            (if lines (length (car (last lines))) 0))))

(defun %line-col-to-cursor-pos (line-index col lines)
  "Convert display coordinates (LINE-INDEX, COL) back to a grapheme offset."
  (let ((pos 0))
    (loop for i from 0 below (min line-index (length lines))
          do (incf pos (length (nth i lines))))
    (+ pos (min col (if (< line-index (length lines))
                        (length (nth line-index lines))
                        0)))))

(defun %prompt-visible-lines (lines visible-rows scroll-offset)
  (let* ((row-count (max 0 visible-rows))
         (total (length lines))
         (max-offset (max 0 (- total row-count)))
         (desired (if (null scroll-offset)
                      max-offset
                      scroll-offset))
         (offset (min max-offset (max 0 desired)))
         (end (min total (+ offset row-count))))
    (values (subseq lines offset end) offset max-offset)))

(defun %styled-text-segments (segments &key (focusp nil))
  (let ((result '()))
    (dolist (segment segments)
      (let* ((plist-segment (and (listp segment)
                                 (keywordp (first segment))))
             (text
               (cond
                 (plist-segment
                  (or (getf segment :text) ""))
                 ((and (consp segment) (stringp (car segment)))
                  (car segment))
                 ((stringp segment)
                  segment)
                 (t
                  (princ-to-string segment))))
             (role
               (cond
                 (plist-segment
                  (or (getf segment :role) :meta))
                 ((and (consp segment) (cdr segment))
                  (cdr segment))
                 (t
                  :meta))))
        (when (plusp (length text))
          (push
           (list text
                 (chat-role-cell role
                                 :focusp focusp
                                 :boldp (and plist-segment (getf segment :boldp))
                                 :italicp (and plist-segment (getf segment :italicp))
                                 :underlinep (and plist-segment (getf segment :underlinep))
                                 :invertp (and plist-segment (getf segment :invertp))
                                 :dimp (and plist-segment (getf segment :dimp))
                                 :strikep (and plist-segment (getf segment :strikep))))
           result))))
    (nreverse result)))

(defun %pop-last-grapheme (text)
  (let ((clusters (ptui.text.grapheme:split-graphemes text)))
    (if (null clusters)
        ""
        (with-output-to-string (out)
          (dolist (cluster (butlast clusters))
            (write-string cluster out))))))

(defun %delete-word-backward (text)
  "Delete the last word from TEXT, following readline Ctrl+W semantics:
skip trailing whitespace, then delete back to the next whitespace boundary."
  (when (or (null text) (zerop (length text)))
    (return-from %delete-word-backward ""))
  (let ((end (length text))
        (pos (1- (length text))))
    ;; Skip trailing whitespace
    (loop while (and (>= pos 0)
                     (member (char text pos) '(#\Space #\Tab #\Newline #\Return)
                             :test #'char=))
          do (decf pos))
    ;; Delete back through non-whitespace (the word)
    (loop while (and (>= pos 0)
                     (not (member (char text pos) '(#\Space #\Tab #\Newline #\Return)
                                  :test #'char=)))
          do (decf pos))
    (if (< pos 0)
        ""
        (subseq text 0 (1+ pos)))))

;;; ---------------------------------------------------------------------------
;;; Cursor-aware grapheme helpers
;;; ---------------------------------------------------------------------------

(defun %grapheme-length (text)
  "Return the number of grapheme clusters in TEXT."
  (if (or (null text) (zerop (length text)))
      0
      (length (ptui.text.grapheme:split-graphemes text))))

(defun %word-boundary-p (cluster)
  "Return T if CLUSTER is a word-boundary character (whitespace or punctuation)."
  (and (= (length cluster) 1)
       (let ((ch (char cluster 0)))
         (or (member ch '(#\Space #\Tab #\Newline #\Return) :test #'char=)
             (member ch '(#\( #\) #\[ #\] #\{ #\} #\< #\>
                          #\. #\, #\; #\: #\! #\? #\/ #\\
                          #\- #\+ #\= #\* #\& #\| #\^ #\~
                          #\' #\" #\` #\@ #\# #\$ #\%)
                     :test #'char=)))))

(defun %word-boundary-backward (text cursor-pos)
  "Return the grapheme offset for the word boundary to the left of CURSOR-POS."
  (let* ((clusters (ptui.text.grapheme:split-graphemes text))
         (pos (min (max 0 cursor-pos) (length clusters))))
    (when (<= pos 0)
      (return-from %word-boundary-backward 0))
    ;; Skip whitespace/punctuation backward
    (loop while (and (> pos 0)
                     (%word-boundary-p (nth (1- pos) clusters)))
          do (decf pos))
    ;; Skip word characters backward
    (loop while (and (> pos 0)
                     (not (%word-boundary-p (nth (1- pos) clusters))))
          do (decf pos))
    pos))

(defun %word-boundary-forward (text cursor-pos)
  "Return the grapheme offset for the word boundary to the right of CURSOR-POS."
  (let* ((clusters (ptui.text.grapheme:split-graphemes text))
         (len (length clusters))
         (pos (min (max 0 cursor-pos) len)))
    (when (>= pos len)
      (return-from %word-boundary-forward len))
    ;; Skip word characters forward
    (loop while (and (< pos len)
                     (not (%word-boundary-p (nth pos clusters))))
          do (incf pos))
    ;; Skip whitespace/punctuation forward
    (loop while (and (< pos len)
                     (%word-boundary-p (nth pos clusters)))
          do (incf pos))
    pos))

(defun %grapheme-insert-at (text cursor-pos new-text)
  "Insert NEW-TEXT at grapheme position CURSOR-POS in TEXT."
  (let* ((clusters (ptui.text.grapheme:split-graphemes text))
         (pos (min (max 0 cursor-pos) (length clusters))))
    (with-output-to-string (out)
      (loop for cluster in (subseq clusters 0 pos)
            do (write-string cluster out))
      (write-string new-text out)
      (loop for cluster in (nthcdr pos clusters)
            do (write-string cluster out)))))

(defun %grapheme-delete-before (text cursor-pos)
  "Delete grapheme before CURSOR-POS. Returns (values new-text new-cursor)."
  (if (or (<= cursor-pos 0) (zerop (length text)))
      (values text 0)
      (let* ((clusters (ptui.text.grapheme:split-graphemes text))
             (pos (min cursor-pos (length clusters))))
        (values
         (with-output-to-string (out)
           (loop for cluster in (subseq clusters 0 (1- pos))
                 do (write-string cluster out))
           (loop for cluster in (nthcdr pos clusters)
                 do (write-string cluster out)))
         (1- pos)))))

(defun %grapheme-delete-at (text cursor-pos)
  "Delete grapheme at CURSOR-POS (Delete key). Returns new text."
  (let* ((clusters (ptui.text.grapheme:split-graphemes text))
         (pos (min cursor-pos (length clusters))))
    (if (>= pos (length clusters))
        text
        (with-output-to-string (out)
          (loop for cluster in (subseq clusters 0 pos)
                do (write-string cluster out))
          (loop for cluster in (nthcdr (1+ pos) clusters)
                do (write-string cluster out))))))

(defun %delete-word-backward-at (text cursor-pos)
  "Delete word backward from CURSOR-POS. Returns (values new-text new-cursor)."
  (if (or (<= cursor-pos 0) (zerop (length text)))
      (values text 0)
      (let* ((clusters (ptui.text.grapheme:split-graphemes text))
             (pos (min cursor-pos (length clusters)))
             (before (with-output-to-string (out)
                       (loop for cluster in (subseq clusters 0 pos)
                             do (write-string cluster out))))
             (after-delete (%delete-word-backward before))
             (new-cursor (%grapheme-length after-delete)))
        (values
         (with-output-to-string (out)
           (write-string after-delete out)
           (loop for cluster in (nthcdr pos clusters)
                 do (write-string cluster out)))
         new-cursor))))

(defvar *chat-cursor-blink-start* nil)

(defun chat-ui-cursor-visible-p (chat-state)
  "Return T if the input cursor should be visible based on blink phase."
  (declare (ignore chat-state))
  ;; Initialize blink start time on first call
  (unless *chat-cursor-blink-start*
    (setf *chat-cursor-blink-start* (get-internal-real-time)))
  ;; Always visible if we can't determine time
  (let ((now (get-internal-real-time))
        (start *chat-cursor-blink-start*))
    (if (or (null now) (null start) (<= now start))
        t
        (let* ((units-per-sec internal-time-units-per-second)
               ;; Blink every 1060ms (530ms on, 530ms off)
               (blink-period-ms 1060)
               (half-period-ms 530)
               ;; Calculate elapsed time in milliseconds safely
               (elapsed-units (- now start))
               (elapsed-ms (floor (* elapsed-units blink-period-ms)
                                  (max 1 units-per-sec)))
               ;; Get position in current blink cycle
               (phase (mod elapsed-ms blink-period-ms)))
          ;; Visible during first half of cycle
          (< phase half-period-ms)))))

(defun %ensure-cursor-pos (text cursor-pos)
  "Coerce NIL cursor-position to end-of-text grapheme count."
  (if (null cursor-pos)
      (%grapheme-length text)
      cursor-pos))

(defun %format-compression-output (compression)
  (if (getf compression :compressed-p)
      (format nil
              "Compacted context: ~D -> ~D tokens (saved ~D). Summarized ~D messages; kept last ~D turns."
              (getf compression :before-tokens 0)
              (getf compression :after-tokens 0)
              (getf compression :saved-tokens 0)
              (getf compression :summarized-messages 0)
              (getf compression :keep-last-turns +context-compression-default-keep-last-turns+))
      "Compaction skipped: not enough older context to summarize."))

(defun %apply-slash-command-action (chat-state result)
  (let ((action-output nil)
        (conversation (%ensure-chat-conversation-state chat-state)))
  (case (slash-command-result-action result)
    (:clear-chat
     (conversation-reset! conversation)
     (%chat-deactivate-history-search! chat-state)
     (setf (chat-ui-state-messages chat-state)
           (conversation-state-messages conversation)
           (chat-ui-state-message-scrollback-lines chat-state) 0
           (chat-ui-state-max-message-scrollback-lines chat-state) 0)
     (%invalidate-styled-lines-cache))
    (:compact-chat
     (let ((compression
             (%compress-chat-history!
              chat-state
              :keep-last-turns (slash-command-result-payload result)
              :trigger :manual)))
       (setf action-output (%format-compression-output compression))))
    (:toggle-provider-dashboard
     (let* ((payload (slash-command-result-payload result))
            (current (chat-ui-state-provider-dashboard-visible-p chat-state))
            (next
              (case payload
                ((:on t) t)
                ((:off nil) nil)
                (otherwise (not current)))))
       (setf (chat-ui-state-provider-dashboard-visible-p chat-state) next
             action-output (format nil "Provider dashboard ~:[hidden~;visible~]." next))
       (provider-health-refresh! :force t)))
    (otherwise nil))
    (%sync-chat-context-usage! chat-state :allow-auto-compress-p nil)
    action-output))

(defun %normalize-command-result (result)
  (cond
    ((typep result 'slash-command-result)
     result)
    ((stringp result)
     (make-slash-command-result :output result))
    (t
     (make-slash-command-result :output nil))))

(defun %handle-slash-command-input (chat-state input)
  (multiple-value-bind (handledp raw-result)
      (dispatch-slash-command input
                              :config (current-config)
                              :memory-backend (current-memory-backend)
                              :chat-state chat-state)
    (when handledp
      (let ((result (%normalize-command-result raw-result)))
        (let ((action-output (%apply-slash-command-action chat-state result)))
        (when (slash-command-result-echo-input-p result)
          (chat-ui-add-message chat-state "user" input))
          (let ((output (or action-output
                            (slash-command-result-output result))))
          (when (and (stringp output)
                     (plusp (length (%slash-trim output))))
            (chat-ui-add-message chat-state "system" output)))
          (setf (chat-ui-state-input-text chat-state) ""
                (chat-ui-state-prompt-scroll-offset chat-state) nil))
        t))))

(defun %handle-command-tab-completion (chat-state)
  (let ((input (chat-ui-state-input-text chat-state)))
    (unless (slash-command-input-p input)
      (return-from %handle-command-tab-completion nil))
    (multiple-value-bind (replacement suggestions)
        (complete-slash-command-input input)
      (cond
        ((and (stringp replacement)
              (not (string= replacement input)))
         (chat-ui-set-input chat-state replacement)
         t)
        ((and (listp suggestions)
              (> (length suggestions) 1))
         (chat-ui-add-message
          chat-state
          "system"
          (format nil "Completions: ~{~A~^, ~}" suggestions))
         t)
        (t
         nil)))))

(defun %handle-memory-candidate (chat-state submitted-message)
  (let* ((user-text (%message-content->text submitted-message))
         (candidate (extract-durable-memory-candidate user-text)))
    (when candidate
      (multiple-value-bind (status payload)
          (apply-memory-candidate candidate :backend (current-memory-backend))
        (case status
          (:stored
           (chat-ui-add-message
            chat-state
            "system"
            (format nil "Memory saved: [~A] ~A"
                    (memory-entry-key payload)
                    (memory-entry-value payload)))
           t)
          (:deleted
           (chat-ui-add-message
            chat-state
            "system"
            (format nil "Memory removed: ~A" payload))
           t)
          (:not-found
           (chat-ui-add-message
            chat-state
            "system"
            (format nil "No existing memory matched ~A." payload))
           t)
          (:candidate
           (chat-ui-add-message
            chat-state
            "system"
            (format nil "Detected durable preference candidate: \"~A\". Use /memory remember <text> to persist."
                    (memory-candidate-text candidate)))
           nil)
          (otherwise nil))))))

(defun %chat-trim-text (text)
  (if (stringp text)
      (string-trim '(#\Space #\Tab #\Newline #\Return) text)
      ""))

(defun %normalize-instruction-text (text)
  (let ((trimmed (%chat-trim-text text)))
    (with-output-to-string (out)
      (loop with previous-space-p = t
            for char across (string-downcase trimmed) do
              (if (or (alphanumericp char) (char= char #\Space))
                  (progn
                    (write-char char out)
                    (setf previous-space-p (char= char #\Space)))
                  (unless previous-space-p
                    (write-char #\Space out)
                    (setf previous-space-p t)))))))

(defun %plan-mode-entry-instruction-p (input)
  (let* ((normalized (%normalize-instruction-text input))
         (padded (format nil " ~A " normalized)))
    (or (search " enter plan mode " padded :test #'char=)
        (search " enable plan mode " padded :test #'char=)
        (search " switch to plan mode " padded :test #'char=)
        (search " go into plan mode " padded :test #'char=)
        (search " turn on plan mode " padded :test #'char=)
        (search " plan mode on " padded :test #'char=))))

(defun %handle-plan-mode-entry-instruction (chat-state input)
  (when (%plan-mode-entry-instruction-p input)
    (chat-ui-add-message chat-state "user" input)
    (when (%handle-slash-command-input chat-state "/plan on")
      (setf (chat-ui-state-input-text chat-state) ""
            (chat-ui-state-prompt-scroll-offset chat-state) nil)
      t)))

(defun %agent-completion-summary (completion)
  (let* ((result (getf completion :result))
         (output (%chat-trim-text (getf completion :stdout)))
         (stderr (%chat-trim-text (getf completion :stderr)))
         (error-message (%chat-trim-text (getf completion :error-message))))
    (cond
      ((and result (not (null result)))
       (princ-to-string result))
      ((plusp (length output))
       output)
      ((plusp (length stderr))
       (format nil "stderr: ~A" stderr))
      ((plusp (length error-message))
       (format nil "error: ~A" error-message))
      (t
       "no output"))))

(defun %inject-agent-completions (chat-state)
  (let ((count 0))
    (dolist (completion (drain-agent-completions))
      (incf count)
      (let ((agent-id (getf completion :id))
            (agent-type (or (getf completion :type) :task))
            (status (or (getf completion :status) :completed)))
        (chat-ui-add-message
         chat-state
         "tool"
         (format nil "subagent ~A (~A) ~A: ~A"
                 agent-id
                 (string-downcase (symbol-name agent-type))
                 (string-downcase (symbol-name status))
                 (%agent-completion-summary completion)))))
    count))

(defun %voice-transcription-text (payload)
  (cond
    ((stringp payload) payload)
    ((listp payload) (or (getf payload :text) ""))
    (t "")))

(defun %inject-voice-transcriptions (chat-state)
  (let ((count 0))
    (dolist (payload (drain-voice-transcriptions))
      (let ((text (%chat-trim-text (%voice-transcription-text payload))))
        (when (plusp (length text))
          (incf count)
          (let ((conversation (%ensure-chat-conversation-state chat-state)))
            (unless (eq (conversation-state-state conversation) :user-input)
              (conversation-transition! conversation :user-input :save-p nil)))
          (let ((submitted (chat-ui-add-message chat-state "user" text)))
            (unless (%handle-memory-candidate chat-state submitted)
              (unless (token-stream-active-p (chat-ui-state-stream-state chat-state))
                (%start-streaming-assistant-response chat-state submitted)))))))
    count))

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

(defun %handle-approval-ui-error! (chat-state stage condition)
  (handler-case
      (let ((pending-tool nil)
            (pending-decision-id nil)
            (crash-log-text (or (ignore-errors (namestring (crash-log-path)))
                                "the crash log"))
            (message-text nil))
        (ignore-errors
          (bt:with-lock-held (*pending-approval-lock*)
            (let ((pending *pending-approval*))
              (setf pending-tool (and pending
                                      (pending-approval-tool-name pending))
                    pending-decision-id (and pending
                                             (pending-approval-decision-id pending))))))
        (setf message-text
              (format nil "Approval dialog failed during ~A. The pending tool request was denied safely. See ~A for details."
                      stage
                      crash-log-text))
        (ignore-errors
          (log-runtime-condition condition
                                 :kind "approval-ui-error"
                                 :source :chat-ui
                                 :message (format nil "Approval dialog failed during ~A." stage)
                                 :details (list :stage stage
                                                :pending-tool pending-tool
                                                :pending-decision-id pending-decision-id)
                                 :path (crash-log-path)))
        (handler-case
            (chat-ui-add-message chat-state "system" message-text)
          (error ()
            (setf (chat-ui-state-messages chat-state)
                  (append (chat-ui-state-messages chat-state)
                          (list (make-chat-message "system" message-text))))))
        (ignore-errors (submit-pending-approval :deny :source :ui-error))
        (ignore-errors
          (approval-dialog-deactivate!
           (chat-ui-state-approval-dialog-state chat-state)))
        nil)
    (error ()
      (ignore-errors (submit-pending-approval :deny :source :ui-error))
      (ignore-errors
        (approval-dialog-deactivate!
         (chat-ui-state-approval-dialog-state chat-state)))
      nil)))

(defun %chat-approval-dialog-widget (chat-state approval-state)
  (handler-case
      (make-approval-dialog-widget
       (list :tool-name (approval-dialog-state-tool-name approval-state)
             :command (approval-dialog-state-command approval-state)
             :path (approval-dialog-state-path approval-state)
             :reason (approval-dialog-state-reason approval-state)
             :selected-option (approval-dialog-state-selected-option approval-state)))
    (error (condition)
      (%handle-approval-ui-error! chat-state :render condition)
      (%chat-text-widget
       "Approval dialog unavailable. Pending tool request denied safely."
       :approval-dialog-error
       :error))))

(defun %approval-recovery-active-p (chat-state)
  (or (approval-dialog-state-active-p
       (chat-ui-state-approval-dialog-state chat-state))
      (bt:with-lock-held (*pending-approval-lock*)
        (not (null *pending-approval*)))))

(defun %sync-pending-approval-dialog! (chat-state)
  "Poll *pending-approval* and activate the dialog if a new approval is waiting."
  (handler-case
      (let ((dialog (chat-ui-state-approval-dialog-state chat-state)))
        (bt:with-lock-held (*pending-approval-lock*)
          (let ((pa *pending-approval*))
            (cond
              ;; A pending approval exists but dialog is not active yet — activate it
              ((and pa (not (approval-dialog-state-active-p dialog)))
               (approval-dialog-activate! dialog
                                          (pending-approval-tool-name pa)
                                          :command (pending-approval-command pa)
                                          :path (pending-approval-path pa)
                                          :reason (pending-approval-reason pa)
                                          :decision-id (pending-approval-decision-id pa)))
              ;; No pending approval but dialog is still active — deactivate
              ((and (null pa) (approval-dialog-state-active-p dialog))
               (approval-dialog-deactivate! dialog))))))
    (error (condition)
      (%handle-approval-ui-error! chat-state :sync condition))))

(defvar *%style-resolve-cache* (make-hash-table :test #'eql)
  "Per-frame cache: style-id → resolved cell. Cleared each frame.")

(defun %resolve-style-id-to-cell (style-id)
  "Resolve a style-id to a ptui cell, caching per frame."
  (or (gethash style-id *%style-resolve-cache*)
      (let* ((entry (lookup-style style-id))
             (cell (chat-role-cell (style-entry-role entry)
                                   :boldp (style-entry-boldp entry)
                                   :italicp (style-entry-italicp entry)
                                   :underlinep (style-entry-underlinep entry)
                                   :invertp (style-entry-invertp entry)
                                   :dimp (style-entry-dimp entry)
                                   :strikep (style-entry-strikep entry))))
        (setf (gethash style-id *%style-resolve-cache*) cell)
        cell)))

(defun %styled-segment->render-segment (segment default-role)
  (cond
    ;; Compact segment: (text . style-id)
    ((compact-segment-p segment)
     (let ((text (compact-segment-text segment)))
       (when (plusp (length text))
         (list text (%resolve-style-id-to-cell (compact-segment-style-id segment))))))
    ((and (consp segment)
          (stringp (first segment))
          (typep (ignore-errors (second segment)) 'ptui.core.types:cell))
     (list (first segment) (second segment)))
    ((and (listp segment)
          (keywordp (first segment)))
     (let* ((text (or (getf segment :text) ""))
            (cell (getf segment :cell)))
       (when (plusp (length text))
         (list text
               (if (typep cell 'ptui.core.types:cell)
                   cell
                   (chat-role-cell (or (getf segment :role) default-role :meta)
                                   :boldp (getf segment :boldp)
                                   :italicp (getf segment :italicp)
                                   :underlinep (getf segment :underlinep)
                                   :invertp (getf segment :invertp)
                                   :dimp (getf segment :dimp)
                                   :strikep (getf segment :strikep)))))))
    ((and (consp segment)
          (stringp (car segment)))
     (let* ((role (if (listp (cdr segment))
                      (second segment)
                      (cdr segment)))
            (text (car segment)))
       (when (plusp (length text))
         (list text (chat-role-cell (or role default-role :meta))))))
    ((stringp segment)
     (when (plusp (length segment))
       (list segment (chat-role-cell (or default-role :meta)))))
    (t
     nil)))

(defun %normalize-tree-styled-segments! (node)
  (when (eq (ptui.ui.elements:ui-element-type node) :text)
    (let* ((props (copy-list (ptui.ui.elements:ui-element-props node)))
           (segments (getf props :styled-segments))
           (default-role (getf props :role :meta)))
      (when segments
        (let* ((segment-list (if (listp segments)
                                 segments
                                 (list segments)))
               (normalized
                 (remove nil
                         (loop for segment in segment-list
                               collect (%styled-segment->render-segment
                                        segment
                                        default-role)))))
          (when normalized
            (setf (getf props :styled-segments) normalized
                  (ptui.ui.elements:ui-element-props node) props)))))
    )
  (dolist (child (ptui.ui.elements:ui-element-children node))
    (%normalize-tree-styled-segments! child))
  node)

(defun chat-ui-build-tree (state cols rows)
  (clrhash *%style-resolve-cache*)
  (let* ((chat-state (ensure-chat-ui-state state))
         (runtime (chat-ui-state-runtime chat-state))
         (tree (let ((ptui.ui.runtime:*current-runtime* runtime))
                 (render-chat-panel chat-state cols rows)))
         (id-remaps '()))
    (labels
        ((record-id-remap (old-id new-id)
           (let ((existing (assoc old-id id-remaps :test #'eq)))
             (if existing
                 (setf (cdr existing) new-id)
                 (push (cons old-id new-id) id-remaps))))
         (tree-has-id-p (node target-id)
           (or (equal (ptui.ui.elements:ui-element-id node) target-id)
               (loop for child in (ptui.ui.elements:ui-element-children node)
                     thereis (tree-has-id-p child target-id))))
         (tree-has-id-prefix-p (node target-id)
           (let ((node-id (ptui.ui.elements:ui-element-id node)))
             (or (and (consp node-id)
                      (equal (first node-id) target-id))
                 (loop for child in (ptui.ui.elements:ui-element-children node)
                       thereis (tree-has-id-prefix-p child target-id)))))
         (normalize-ids! (node)
           (let ((node-id (ptui.ui.elements:ui-element-id node)))
             (when (and (symbolp node-id)
                        (string= (symbol-name node-id) "TREE")
                        (tree-has-id-p node :tree-browser-header))
               (record-id-remap node-id :tree-browser)
               (setf (ptui.ui.elements:ui-element-id node) :tree-browser))
             (when (and (symbolp node-id)
                        (string= (symbol-name node-id) "PLAN")
                        (tree-has-id-prefix-p node :chat-plan-presentation))
               (record-id-remap node-id :chat-plan-presentation)
               (setf (ptui.ui.elements:ui-element-id node) :chat-plan-presentation))
             (when (and (symbolp node-id)
                        (string= (symbol-name node-id) "INPUT")
                        (eql (ptui.ui.elements:ui-element-type node) :prompt-box))
               (record-id-remap node-id :chat-input)
               (setf (ptui.ui.elements:ui-element-id node) :chat-input)))
           (dolist (child (ptui.ui.elements:ui-element-children node))
             (normalize-ids! child)))
         (apply-constraint-id-remaps! (node)
           (when (eq (ptui.ui.elements:ui-element-type node) :constraint-layout)
             (let ((constraints
                     (getf (ptui.ui.elements:ui-element-props node) :constraints)))
               (dolist (spec constraints)
                 (let* ((spec-id (ptui.layout.constraints:constraint-spec-id spec))
                        (remap (cdr (assoc spec-id id-remaps :test #'eq))))
                   (when remap
                     (setf (ptui.layout.constraints:constraint-spec-id spec) remap))))))
           (dolist (child (ptui.ui.elements:ui-element-children node))
             (apply-constraint-id-remaps! child))))
      (normalize-ids! tree)
      (when id-remaps
        (apply-constraint-id-remaps! tree)))
    (ptui.ui.runtime:update-runtime runtime tree)
    tree))

(defvar *%render-prev-fingerprint* nil)
(defvar *%render-prev-buffer* nil)

(defun %chat-render-fingerprint (chat-state cols rows)
  "Cheap fingerprint of UI-visible state. Returns a list for EQUAL comparison."
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (renderer (chat-ui-state-stream-markdown-renderer chat-state)))
    (list (length (chat-ui-state-messages chat-state))
          (chat-ui-state-input-text chat-state)
          (chat-ui-state-message-scrollback-lines chat-state)
          (chat-ui-state-prompt-scroll-offset chat-state)
          (token-stream-state-status stream-state)
          (streaming-markdown-renderer-pending-line renderer)
          (length (streaming-markdown-renderer-wrapped-lines renderer))
          (stream-cursor-visible-p stream-state)
          (chat-ui-state-provider-dashboard-visible-p chat-state)
          (chat-ui-state-history-search-active-p chat-state)
          (chat-ui-state-plan-selected-step-index chat-state)
          (chat-ui-state-cursor-position chat-state)
          cols rows
          *%styled-lines-cache-generation*)))

(defun render-chat-ui-buffer (state size)
  (labels ((render-once ()
             (let* ((chat-state (ensure-chat-ui-state state))
                    (cols (ptui.core.types:size-cols size))
                    (rows (ptui.core.types:size-rows size))
                    (frame-start-ms (%usdt-now-ms)))
               (%sync-all-state! chat-state)
               (maybe-auto-checkpoint
                :conversation (%ensure-chat-conversation-state chat-state)
                :config (%chat-config)
                :busy-p (token-stream-active-p (chat-ui-state-stream-state chat-state)))
               ;; Fingerprint cache disabled — buffer reuse can mutate cached buffer.

               (incf (chat-ui-state-frame-count chat-state))
               (let* ((frame-index (chat-ui-state-frame-count chat-state))
                      (tree (chat-ui-build-tree chat-state cols rows))
                      (buffer nil))
                 (%normalize-tree-styled-segments! tree)
                 (setf buffer (ptui.ui.app::%render-tree-to-buffer tree size))
                 (usdt-probe-render-frame frame-index
                                          (max 0 (- (%usdt-now-ms) frame-start-ms))
                                          cols
                                          rows)
                 buffer))))
    (let* ((chat-state (ensure-chat-ui-state state))
           (approval-recovery-p (%approval-recovery-active-p chat-state)))
      (handler-case
          (render-once)
        (error (condition)
          (if approval-recovery-p
              (progn
                (%handle-approval-ui-error! chat-state :render-cycle condition)
                (render-once))
              (error condition)))))))

(defun %handle-chat-ui-unrouted-key-event (chat-state event)
  "Fallback key handler for smoke tests that dispatch keys before first render
or before runtime routing has a focused target."
  (let* ((key (ptui.core.events:key-event-key event))
         (text (ptui.core.events:key-event-text? event))
         (input-text (chat-ui-state-input-text chat-state))
         (tree-state (%ensure-chat-tree-browser-state chat-state)))
    (cond
      ((and (chat-ui-state-history-search-active-p chat-state)
            (eql key :escape))
       (%chat-deactivate-history-search! chat-state :restore-input-p t))
      ((and (zerop (length input-text))
            (typep tree-state 'tree-browser-state)
            (member key '(:up :down :left :right :enter :return :escape) :test #'eq))
       (chat-panel-handle-tree-browser-key chat-state key))
      ((or (eql key :pgup) (eql key :pgdn))
       (chat-ui-scroll-history chat-state (if (eql key :pgup) 5 -5)))
      ;; Up/Down with empty input → scroll history (not input cursor movement)
      ((and (member key '(:up :down) :test #'eq)
            (zerop (length input-text)))
       (chat-ui-scroll-history chat-state (if (eql key :up) 3 -3)))
      ((member key '(:text :enter :return :backspace :delete
                      :ctrl-j :tab :ctrl-p :ctrl-n :ctrl-r
                      :ctrl-a :ctrl-e :left :right
                      :ctrl-left :ctrl-right :home :end
                      :ctrl-w :ctrl-u :ctrl-k
                      :up :down :escape)
               :test #'eq)
       (chat-panel-handle-input-key chat-state
                                    (if (eql key :return) :enter key)
                                    text
                                    80))
      (t
       nil))))

(defun %make-approval-dialog-key-handler-table ()
  (let ((table (make-hash-table :test #'eq)))
    (dolist (key '(:up :down :left :right :enter :return :escape))
      (setf (gethash key table) '%approval-dialog-handle-navigation-key))
    (setf (gethash :text table) '%approval-dialog-handle-text-key)
    table))

(defparameter *approval-dialog-key-handlers*
  (%make-approval-dialog-key-handler-table))

(defun %approval-dialog-handle-navigation-key (approval-state key text)
  (declare (ignore text))
  (approval-dialog-handle-key! approval-state key))

(defun %approval-dialog-handle-text-key (approval-state key text)
  (declare (ignore key))
  (approval-dialog-handle-text! approval-state text))

(defun %make-chat-ui-key-handler-table ()
  (let ((table (make-hash-table :test #'eq)))
    (setf (gethash :ctrl-c table) '%chat-ui-handle-ctrl-c-key)
    (setf (gethash :escape table) '%chat-ui-handle-escape-key)
    table))

(defparameter *chat-ui-key-handlers* (%make-chat-ui-key-handler-table))

(defun %chat-ui-route (runtime event)
  (if (typep event 'ptui.core.events:key-event)
      (ptui.ui.runtime:route-event runtime event)
      (list :kind :unhandled :event event)))

(defun %chat-ui-unhandled-route-p (runtime route)
  (or (null (ptui.ui.runtime:runtime-root runtime))
      (eq (getf route :kind) :unhandled)))

(defun %chat-ui-key-context (chat-state event)
  (list :chat-state chat-state
        :event event
        :stream-state (chat-ui-state-stream-state chat-state)
        :approval-state (chat-ui-state-approval-dialog-state chat-state)))

(defun %chat-ui-disarm-quit-unless-ctrl-c! (chat-state key)
  (unless (eq key :ctrl-c)
    (%chat-disarm-ctrl-c-quit! chat-state)))

(defun %chat-ui-handle-approval-key-event (context key text)
  (let ((approval-state (getf context :approval-state))
        (stream-state (getf context :stream-state)))
    (when (approval-dialog-state-active-p approval-state)
      (handler-case
              (if (eq key :ctrl-c)
              (if (token-stream-active-p stream-state)
                  (progn
                    (token-stream-request-cancel stream-state)
                    t)
                  (approval-dialog-handle-key! approval-state :escape))
              (let* ((handler-name (gethash key *approval-dialog-key-handlers*))
                     (handler (and handler-name (symbol-function handler-name))))
                (when handler
                  (funcall handler approval-state key text))))
        (error (condition)
          (%handle-approval-ui-error! (getf context :chat-state) :event condition)))
      t)))

(defun %chat-ui-handle-escape-key (context key)
  (declare (ignore key))
  (let ((stream-state (getf context :stream-state)))
    (when (token-stream-active-p stream-state)
      (token-stream-request-cancel stream-state)
      :consume)))

(defun %chat-ui-handle-ctrl-c-key (context key)
  (declare (ignore key))
  (let ((chat-state (getf context :chat-state))
        (stream-state (getf context :stream-state)))
    (cond
      ((token-stream-active-p stream-state)
       (token-stream-request-cancel stream-state)
       :consume)
      ((%chat-ctrl-c-quit-armed-p chat-state)
       (%chat-disarm-ctrl-c-quit! chat-state)
       :quit)
      (t
       (%chat-arm-ctrl-c-quit! chat-state)
       :consume))))

(defun %chat-ui-handle-key-event (chat-state runtime event route)
  (checkpoint-mark-activity)
  (%chat-mark-activity)
  (let* ((key (ptui.core.events:key-event-key event))
         (text (ptui.core.events:key-event-text? event))
         (context (%chat-ui-key-context chat-state event)))
    (%chat-ui-disarm-quit-unless-ctrl-c! chat-state key)
    (cond
      ((%chat-ui-handle-approval-key-event context key text)
       :consume)
      (t
       (let* ((handler-name (gethash key *chat-ui-key-handlers*))
              (handler (and handler-name (symbol-function handler-name))))
         (or (and handler (funcall handler context key))
             (when (%chat-ui-unhandled-route-p runtime route)
               (and (%handle-chat-ui-unrouted-key-event chat-state event)
                    :consume))))))))

(defun %chat-ui-dispatch-routed-event (chat-state runtime route)
  (when (and (ptui.ui.runtime:runtime-root runtime)
             (listp route))
    (handler-case
        (ptui.widgets.core:dispatch-widget-event
         (ptui.ui.runtime:runtime-root runtime)
         route
         :bubble t)
      (error (condition)
        (if (%approval-recovery-active-p chat-state)
            (%handle-approval-ui-error! chat-state :dispatch condition)
            (error condition))))))

(defun %chat-ui-text-q-consumed-p (event outcome)
  (and (typep event 'ptui.core.events:key-event)
       (eql (ptui.core.events:key-event-key event) :text)
       (string-equal (or (ptui.core.events:key-event-text? event) "") "q")
       (not (eq outcome :quit))))

(defun handle-chat-ui-event (state event)
  (let* ((chat-state (ensure-chat-ui-state state))
         (runtime (chat-ui-state-runtime chat-state))
         (route (%chat-ui-route runtime event))
         (outcome nil))
    ;; Sync approval dialog state so key routing can intercept for active dialogs.
    ;; All other sync happens in %sync-all-state! during render.
    (%sync-pending-approval-dialog! chat-state)
    (when (typep event 'ptui.core.events:key-event)
      (setf outcome (%chat-ui-handle-key-event chat-state runtime event route)))
    (unless outcome
      (%chat-ui-dispatch-routed-event chat-state runtime route))
    (when (%chat-ui-text-q-consumed-p event outcome)
      ;; Amoebum owns quit semantics now; a bare "q" should route normally and
      ;; never fall through to PTUI's legacy default quit binding.
      (setf outcome :consume))
    (values chat-state
            outcome)))

(defun run-chat-ui (&key (backend :auto) (fps 20) initial-state demo)
  (let ((*session-persistence-enabled* (and *session-persistence-enabled*
                                            (not demo))))
    (let ((resolved-state
            (if initial-state
                initial-state
                (if demo
                    (make-chat-ui-state :stream-runner #'demo-stream-runner
                                        :stream-client nil)
                    (chat-ui-restore-latest-session (make-chat-ui-state))))))
      (checkpoint-mark-activity)
      (%chat-mark-activity)
      (load-user-extensions)
      (setf *approval-ui-active-p* t)
      (let ((chat-state nil)
            (log-stream nil)
            (heap-monitor-stop-p nil)
            (heap-monitor-thread nil))
        (unwind-protect
            (progn
              ;; Redirect ptui log output to a file so it doesn't corrupt the TUI
              (let ((log-path (merge-pathnames "runtime/ptui.log"
                                               (ensure-directories-exist
                                                (merge-pathnames ".amoebum/"
                                                                 (user-homedir-pathname))))))
                (ignore-errors
                  (ensure-directories-exist log-path)
                  (setf log-stream (open log-path
                                         :direction :output
                                         :if-exists :append
                                         :if-does-not-exist :create))
                  (setf ptui.util.log:*log-output* log-stream)))
              ;; Start periodic heap monitor (every 30s → runtime.log)
              (enable-gc-telemetry)
              (setf heap-monitor-thread
                    (bt:make-thread
                     (lambda ()
                       (loop until heap-monitor-stop-p
                             do (ignore-errors
                                  (let ((mem (memory-statistics)))
                                    (log-runtime-event
                                     :level :info
                                     :kind "heap-snapshot"
                                     :source :profiler
                                     :message "Periodic heap snapshot."
                                     :details
                                     (list :dynamic-usage-mb
                                           (round (getf mem :dynamic-usage-mb) 0.1)
                                           :gc-run-time-s
                                           (getf mem :gc-run-time)
                                           :message-count
                                           (if chat-state
                                               (length (chat-ui-state-messages chat-state))
                                               0)))))
                                (%chat-sleep-until-stop
                                 (lambda ()
                                   heap-monitor-stop-p)
                                 :seconds +heap-monitor-snapshot-interval-seconds+
                                 :poll-seconds +heap-monitor-stop-poll-seconds+)))
                     :name "heap-monitor"))
              (setf chat-state (ensure-chat-ui-state resolved-state))
              (ptui.engine.loop:run #'render-chat-ui-buffer
                                    :backend backend
                                    :fps fps
                                    :initial-state chat-state
                                    :event-bus (current-event-bus)
                                    :on-event #'handle-chat-ui-event))
          (setf heap-monitor-stop-p t)
          (when heap-monitor-thread
            (ignore-errors (bt:join-thread heap-monitor-thread)))
          (disable-gc-telemetry)
          (setf *approval-ui-active-p* nil)
          (setf ptui.util.log:*log-output* nil)
          (when log-stream
            (ignore-errors (close log-stream)))
          ;; Checkpoint session on exit so next launch can restore full state.
          (when (and chat-state (not demo))
            (ignore-errors
              (let ((conversation (chat-ui-state-conversation chat-state)))
                (when conversation
                  (checkpoint-session :conversation conversation
                                      :trigger :exit
                                      :auto-p t))))))))))
