(in-package :amoebum)

;;; NXT-278: chat-ui-state struct, constants, and low-level state helpers
;;; extracted from chat.lisp. Both files share the :amoebum package, so
;;; call sites in chat.lisp (and elsewhere) continue to resolve unchanged.

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
  (demo-mode-p nil :type boolean)
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
correct order. Replaces the previous scattered sync calls in
render-chat-ui-buffer, handle-chat-ui-event, and defpanel effects."
  (%inject-agent-completions chat-state)
  (%inject-voice-transcriptions chat-state)
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
    (unless (typep (chat-ui-state-demo-mode-p chat-state) 'boolean)
      (setf (chat-ui-state-demo-mode-p chat-state) nil))
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

(defun %tool-display-label (key)
  (string-downcase (substitute #\- #\_ key)))

(defun %normalize-tool-json-key (key)
  (let ((text (cond
                ((stringp key) key)
                ((symbolp key) (symbol-name key))
                (t (princ-to-string key)))))
    (string-downcase
     (substitute #\_ #\-
                 (string-trim '(#\Space #\Tab #\Newline #\Return) text)))))

(defun %tool-json-field (object key)
  (when (hash-table-p object)
    (let ((normalized-target (%normalize-tool-json-key key)))
      (multiple-value-bind (value presentp) (gethash key object)
        (if presentp
            (values value t)
            (loop for existing-key being the hash-keys of object
                  for normalized-key = (%normalize-tool-json-key existing-key)
                  when (string= normalized-key normalized-target)
                    do (return (values (gethash existing-key object) t))
                  finally (return (values nil nil))))))))

(defun %trim-chat-error-text (text)
  (if (stringp text)
      (string-trim '(#\Space #\Tab #\Newline #\Return) text)
      ""))

(defun %parse-json-object-substring (text)
  (when (stringp text)
    (let ((start (position #\{ text)))
      (when start
        (ignore-errors
          (jonathan:parse (subseq text start) :as :hash-table))))))

(defun %stream-failure-provider-message (error-message)
  (let ((payload (%parse-json-object-substring error-message)))
    (when (hash-table-p payload)
      (multiple-value-bind (error-object presentp) (gethash "error" payload)
        (when (and presentp (hash-table-p error-object))
          (multiple-value-bind (message message-present-p)
              (gethash "message" error-object)
            (when (and message-present-p
                       (stringp message)
                       (plusp (length (%trim-chat-error-text message))))
              (%trim-chat-error-text message))))))))

(defun %stream-failure-summary-line (error-message)
  (let ((trimmed (%trim-chat-error-text error-message)))
    (cond
      ((zerop (length trimmed))
       "The provider request failed.")
      ((search "(status=429)" trimmed :test #'char=)
       "Provider request failed with HTTP 429.")
      (t
       trimmed))))

(defun %format-stream-failure-message (error-message)
  (let* ((trimmed (%trim-chat-error-text error-message))
         (provider-message (%stream-failure-provider-message trimmed))
         (summary-line (%stream-failure-summary-line trimmed))
         (retry-guidance
           (if (search "(status=429)" trimmed :test #'char=)
               "Retry your last message in a moment."
               "Review the error and retry when ready.")))
    (with-output-to-string (out)
      (write-string "[Stream failed]" out)
      (terpri out)
      (write-string summary-line out)
      (when (and (stringp provider-message)
                 (plusp (length provider-message))
                 (not (string= provider-message summary-line)))
        (terpri out)
        (write-string provider-message out))
      (terpri out)
      (write-string retry-guidance out))))

(defun %tool-json-scalar-string (value)
  (cond
    ((stringp value) value)
    ((numberp value) (princ-to-string value))
    ((or (eq value t) (eq value :true)) "true")
    ((or (null value) (eq value :false)) "false")
    ((symbolp value) (string-downcase (symbol-name value)))
    (t nil)))

(defun %tool-primary-display-label-p (label)
  (member label
          '("stdout" "output" "text" "content" "message" "summary" "result" "body")
          :test #'string=))

(defun %tool-json-blank-string-p (value)
  (or (null value)
      (and (stringp value)
           (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) value))))))

(defun %tool-json-true-p (value)
  (or (eq value t)
      (eq value :true)
      (and (stringp value)
           (member (string-downcase
                    (string-trim '(#\Space #\Tab #\Newline #\Return) value))
                   '("true" "t" "yes" "y" "1")
                   :test #'string=))))

(defun %tool-json-zero-p (value)
  (cond
    ((null value) t)
    ((integerp value) (zerop value))
    ((and (realp value) (not (complexp value))) (zerop value))
    ((stringp value)
     (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
       (or (string= trimmed "")
           (string= trimmed "0"))))
    (t nil)))

(defun %tool-json-success-status-p (value)
  (or (null value)
      (and (stringp value)
           (member (string-downcase
                    (string-trim '(#\Space #\Tab #\Newline #\Return) value))
                   '("" "completed" "complete" "success" "succeeded" "ok" "done" "finished")
                   :test #'string=))
      (and (symbolp value)
           (member (string-downcase (symbol-name value))
                   '("completed" "complete" "success" "succeeded" "ok" "done" "finished")
                   :test #'string=))))

(defun %tool-json-clean-success-p (value primary-block-p)
  (and primary-block-p
       (multiple-value-bind (stderr stderr-present-p) (%tool-json-field value "stderr")
         (declare (ignore stderr-present-p))
         (%tool-json-blank-string-p stderr))
       (multiple-value-bind (error error-present-p) (%tool-json-field value "error")
         (declare (ignore error-present-p))
         (%tool-json-blank-string-p error))
       (multiple-value-bind (signal signal-present-p) (%tool-json-field value "signal")
         (declare (ignore signal-present-p))
         (%tool-json-zero-p signal))
       (multiple-value-bind (exit-code exit-code-present-p) (%tool-json-field value "exit_code")
         (declare (ignore exit-code-present-p))
         (%tool-json-zero-p exit-code))
       (multiple-value-bind (status status-present-p) (%tool-json-field value "status")
         (declare (ignore status-present-p))
         (%tool-json-success-status-p status))
       (multiple-value-bind (stdout-truncated truncated-present-p)
           (%tool-json-field value "stdout_truncated_p")
         (declare (ignore truncated-present-p))
         (not (%tool-json-true-p stdout-truncated)))
       (multiple-value-bind (stderr-truncated truncated-present-p)
           (%tool-json-field value "stderr_truncated_p")
         (declare (ignore truncated-present-p))
         (not (%tool-json-true-p stderr-truncated)))
       (multiple-value-bind (stdout-omitted omitted-present-p)
           (%tool-json-field value "stdout_omitted_chars")
         (declare (ignore omitted-present-p))
         (%tool-json-zero-p stdout-omitted))
       (multiple-value-bind (stderr-omitted omitted-present-p)
           (%tool-json-field value "stderr_omitted_chars")
         (declare (ignore omitted-present-p))
         (%tool-json-zero-p stderr-omitted))))

(defun %tool-json-display-text (value)
  (cond
    ((and (stringp value) (plusp (length value)))
     value)
    ((vectorp value)
     (let ((items (coerce value 'list)))
       (when (and items (every #'stringp items))
         (format nil "~{~A~^~%~}" items))))
    ((hash-table-p value)
     (let ((blocks '())
           (details '())
           (nested-text nil))
       (labels ((add-block (key)
                  (multiple-value-bind (field presentp) (%tool-json-field value key)
                    (when (and presentp (stringp field) (plusp (length field)))
                      (push (cons (%tool-display-label key) field) blocks))))
                (add-detail (key)
                  (multiple-value-bind (field presentp) (%tool-json-field value key)
                    (when presentp
                      (let ((text (%tool-json-scalar-string field)))
                        (when (and (stringp text) (plusp (length text)))
                          (push (format nil "~A: ~A"
                                        (%tool-display-label key)
                                        text)
                                details)))))))
         (dolist (key '("stdout" "output" "text" "content" "message" "summary" "result" "body"))
           (add-block key))
         (dolist (key '("stderr" "error"))
           (add-block key))
         (let ((clean-success-p (%tool-json-clean-success-p value (not (null blocks)))))
           (unless clean-success-p
             (dolist (key '("exit_code" "status" "signal"))
               (add-detail key)))
           (dolist (key '("stdout_truncated_p" "stderr_truncated_p"
                          "stdout_omitted_chars" "stderr_omitted_chars"))
             (add-detail key)))
         (when (and (null blocks) (null details))
           (dolist (nested-key '("payload" "result" "data"))
             (multiple-value-bind (nested presentp) (%tool-json-field value nested-key)
               (when (and presentp (null nested-text))
                 (setf nested-text (%tool-json-display-text nested)))))))
       (cond
         ((and (stringp nested-text) (plusp (length nested-text)))
          nested-text)
         ((or blocks details)
          (setf blocks (nreverse blocks)
                details (nreverse details))
          (if (and (= (length blocks) 1)
                   (null details)
                   (%tool-primary-display-label-p (caar blocks)))
              (cdar blocks)
              (with-output-to-string (out)
                (loop for (label . block) in blocks
                      for index from 0 do
                        (when (> index 0)
                          (terpri out)
                          (terpri out))
                        (format out "~A:~%~A" label block))
                (when details
                  (when blocks
                    (terpri out)
                    (terpri out))
                  (format out "~{~A~^~%~}" details)))))
         (t nil))))
    (t nil)))

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
