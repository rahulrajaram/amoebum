(in-package :amoebum)

(defparameter +chat-role-order+ '("system" "user" "assistant" "tool"))
(defparameter +max-agentic-iterations+ 25)
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
                      (status-bar-state (make-status-bar-state))
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
                      (stream-status-publish-key nil)
                      (frame-count 0)
                      (agentic-iteration-count 0)
                      (plan-selected-step-index nil))))
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
  (status-bar-state (make-status-bar-state) :type status-bar-state)
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
  (stream-status-publish-key nil)
  (frame-count 0 :type fixnum)
  (agentic-iteration-count 0 :type fixnum)
  plan-selected-step-index
  (cached-tree-key nil)
  (cached-tree nil)
  (cached-layout nil)
  (cached-render-key nil)
  (cached-buffer nil))

(defun %chat-config ()
  (ignore-errors (current-config)))

(defun %chat-project-root ()
  (let ((cfg (%chat-config)))
    (or (and (config-p cfg)
             (config-project-root cfg))
        (ignore-errors (uiop:getcwd))
        *default-pathname-defaults*)))

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
    (setf (fuzzy-picker-state-context-label picker) "@ file"
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
          (setf (fuzzy-picker-state-context-label picker) "@ file"
                (fuzzy-picker-state-empty-message picker) "  [none] no matching files")
          (fuzzy-picker-sync-input! picker
                                    (chat-ui-state-input-text chat-state)
                                    :root (%chat-project-root))
          (when step-p
            (fuzzy-picker-step! picker))))
    picker))

(defun %ensure-chat-tree-browser-state (chat-state)
  (let* ((resolved-root (%resolve-search-root (%chat-project-root)))
         (resolved-root-key (%path-text resolved-root))
         (tree-state (chat-ui-state-tree-browser-state chat-state))
         (current-root-key
           (and (typep tree-state 'tree-browser-state)
                (tree-browser-state-root-path tree-state)
                (%path-text (tree-browser-state-root-path tree-state)))))
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

(defun ensure-chat-ui-state (state)
  (let ((chat-state
          (if (and state (typep state 'chat-ui-state))
              state
              (make-chat-ui-state :runtime (ptui.ui.runtime:make-runtime)))))
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
    (setf (chat-ui-state-status-bar-state chat-state)
          (ensure-status-bar-state
           (chat-ui-state-status-bar-state chat-state)
           :event-bus (current-event-bus)))
    (%ensure-chat-fuzzy-picker-state chat-state)
    (%ensure-chat-tree-browser-state chat-state)
    (%chat-sync-fuzzy-picker! chat-state :step-p nil)
    (%ensure-chat-conversation-state chat-state)
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
    (setf (chat-ui-state-message-scrollback-lines chat-state) 0)
    (%sync-chat-context-usage! chat-state)
    message))

(defun chat-ui-add-message (state role content &key name tool-calls partial)
  (chat-ui-append-message
   state
   (make-chat-message role content
                      :name name
                      :tool-calls tool-calls
                      :partial partial)))

(defun chat-ui-set-input (state text)
  (let ((chat-state (ensure-chat-ui-state state)))
    (setf (chat-ui-state-input-text chat-state)
          (if (stringp text)
              text
              (princ-to-string text))
          (chat-ui-state-prompt-scroll-offset chat-state) nil)
    (%chat-sync-fuzzy-picker! chat-state :step-p nil)
    (chat-ui-state-input-text chat-state)))

(defun %blank-string-p (text)
  (every (lambda (char)
           (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=))
         text))

(defun chat-ui-submit-input (state)
  (let* ((chat-state (ensure-chat-ui-state state))
         (conversation (%ensure-chat-conversation-state chat-state))
         (input (chat-ui-state-input-text chat-state)))
    (setf (chat-ui-state-agentic-iteration-count chat-state) 0)
    (if (or (null input) (zerop (length input)) (%blank-string-p input))
        nil
        (prog1
            (progn
              (conversation-transition! conversation :user-input)
              (chat-ui-add-message chat-state "user" input))
          (setf (chat-ui-state-input-text chat-state) ""
                (chat-ui-state-prompt-scroll-offset chat-state) nil)
          (fuzzy-picker-deactivate! (%ensure-chat-fuzzy-picker-state chat-state))))))

(defun chat-ui-scroll-history (state delta-lines)
  (let* ((chat-state (ensure-chat-ui-state state))
         (stream-active-p (token-stream-active-p (chat-ui-state-stream-state chat-state)))
         (max-scrollback (max 0 (chat-ui-state-max-message-scrollback-lines chat-state)))
         (next-scrollback (+ (chat-ui-state-message-scrollback-lines chat-state)
                             (or delta-lines 0))))
    (setf (chat-ui-state-message-scrollback-lines chat-state)
          (max 0 (min max-scrollback next-scrollback)))
    (when stream-active-p
      (cond
        ((> (chat-ui-state-message-scrollback-lines chat-state) 0)
         (setf (chat-ui-state-stream-scroll-follow-p chat-state) nil))
        (t
         (setf (chat-ui-state-stream-scroll-follow-p chat-state) t))))
    (chat-ui-state-message-scrollback-lines chat-state)))

(defun chat-role-prefix (role)
  (case (intern (string-upcase (%normalize-chat-role role)) :keyword)
    (:system "SYSTEM>")
    (:user "YOU>")
    (:assistant "ASSISTANT>")
    (:tool "TOOL>")
    (otherwise "ASSISTANT>")))

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
    ((integerp index) index)
    ((and (stringp tool-call-id) (plusp (length tool-call-id)))
     (concatenate 'string "id:" tool-call-id))
    ((and (stringp tool-name) (plusp (length tool-name)))
     (concatenate 'string "name:" tool-name))
    ((and (stringp arguments) (plusp (length arguments)))
     (concatenate 'string "args:" arguments))
    (t
     :unknown)))

(defun %ensure-stream-tool-call-preview (chat-state key)
  (let* ((table (chat-ui-state-stream-tool-calls chat-state))
         (entry (and (hash-table-p table) (gethash key table))))
    (or entry
        (let ((fresh (list :key key
                           :index nil
                           :tool-name nil
                           :tool-call-id nil
                           :arguments nil
                           :started-p nil
                           :arguments-complete-p nil
                           :executed-p nil
                          :execution-error nil
                          :result nil
                          :malformed-p nil)))
          (setf (gethash key table) fresh)
          fresh))))

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
         (entry (%ensure-stream-tool-call-preview chat-state key))
         (kind (getf event :kind)))
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
              (eq kind :tool-call-argument-complete)
              (and (stringp tool-name) (plusp (length tool-name)))))
    (setf (getf entry :arguments-complete-p)
          (or (getf entry :arguments-complete-p)
              (eq kind :tool-call-argument-complete)
              (not (null (getf event :arguments-complete-p)))))
    entry))

(defun %clear-stream-tool-tracking! (chat-state)
  (let ((tool-calls (chat-ui-state-stream-tool-calls chat-state))
        (executed (chat-ui-state-stream-executed-tool-call-keys chat-state)))
    (when (hash-table-p tool-calls)
      (clrhash tool-calls))
    (when (hash-table-p executed)
      (clrhash executed))
    chat-state))

(defun %set-stream-tool-call-execution-status! (chat-state preview-key
                                                &key executed-p execution-error
                                                     result malformed-p)
  (let* ((table (chat-ui-state-stream-tool-calls chat-state))
         (entry (and (hash-table-p table)
                     (gethash preview-key table))))
    (when entry
      (when executed-p
        (setf (getf entry :executed-p) t))
      (when execution-error
        (setf (getf entry :execution-error) execution-error))
      (when result
        (setf (getf entry :result) result))
      (when malformed-p
        (setf (getf entry :malformed-p) t)))
    entry))

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
                        (or (pseudopod:tool-call-arguments tool-call) "")))
      preview-key))

(defun %tool-call-has-id-p (tool-call)
  "Return T if TOOL-CALL has a non-empty tool-call-id."
  (and (pseudopod:tool-call-p tool-call)
       (stringp (pseudopod:tool-call-id tool-call))
       (plusp (length (pseudopod:tool-call-id tool-call)))))

(defun %execute-stream-tool-call! (chat-state event)
  (let* ((tool-call (%stream-tool-call-from-event event))
         (preview-entry (%update-stream-tool-call-preview! chat-state event))
         (preview-key (and (listp preview-entry) (getf preview-entry :key)))
         (execution-key (%stream-tool-call-execution-key tool-call preview-key))
         (executed-table (chat-ui-state-stream-executed-tool-call-keys chat-state)))
    (unless (and (pseudopod:tool-call-p tool-call)
                 execution-key)
      (return-from %execute-stream-tool-call! nil))
    (when (gethash execution-key executed-table)
      (return-from %execute-stream-tool-call! nil))
    ;; If tool call is missing an ID, mark as malformed and skip execution.
    ;; The :complete handler will ask the LLM to re-issue with proper IDs.
    (unless (%tool-call-has-id-p tool-call)
      (setf (gethash execution-key executed-table) t)
      (%set-stream-tool-call-execution-status!
       chat-state preview-key :malformed-p t)
      (return-from %execute-stream-tool-call! nil))
    (setf (gethash execution-key executed-table) t)
    (%set-stream-tool-call-execution-status! chat-state preview-key :executed-p t)
    (let* ((toolset (or (chat-ui-state-stream-tools chat-state) *toolset*))
           (config (%chat-config))
           (permission-mode (and (config-p config)
                                 (config-permission-mode config))))
      (let ((result-text nil))
      (handler-case
          (if (pseudopod:find-tool toolset (pseudopod:tool-call-name tool-call))
              (let ((result (execute-tool tool-call
                                          (make-amoebum-context
                                           :toolset toolset
                                           :permission-mode permission-mode
                                           :event-bus (%context-event-bus chat-state)))))
                (setf result-text (if (stringp result)
                                      result
                                      (princ-to-string (or result ""))))
                (%set-stream-tool-call-execution-status!
                 chat-state preview-key :result result-text))
              (let ((err-msg (format nil "Unregistered tool ~A."
                                     (or (pseudopod:tool-call-name tool-call)
                                         "<unknown>"))))
                (setf result-text err-msg)
                (%set-stream-tool-call-execution-status!
                 chat-state preview-key
                 :execution-error err-msg :result err-msg)))
        (error (condition)
          (let ((err-msg (princ-to-string condition)))
            (setf result-text err-msg)
            (%set-stream-tool-call-execution-status!
             chat-state preview-key
             :execution-error err-msg :result err-msg))))))
    t))

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

(defun %stream-tool-call-preview-lines (chat-state width)
  (let ((safe-width (max 24 width))
        (entries '()))
    (maphash
     (lambda (key entry)
       (declare (ignore key))
       (when (listp entry)
         (let* ((tool-name (or (getf entry :tool-name) "<tool>"))
                (arguments (or (getf entry :arguments) ""))
                (status (cond
                          ((getf entry :execution-error) "error")
                          ((getf entry :executed-p) "running")
                          ((getf entry :arguments-complete-p) "args-ready")
                          (t "streaming")))
                (arguments-limit (max 6 (- safe-width 26)))
                (snippet (%truncate-inline-text arguments arguments-limit))
                (detail (if (plusp (length snippet))
                            (format nil " ~A" snippet)
                            ""))
                (error-fragment
                  (if (getf entry :execution-error)
                      (format nil
                              " ! ~A"
                              (%truncate-inline-text (getf entry :execution-error)
                                                     (max 8 (- safe-width 40))))
                      ""))
                (text (format nil "TOOL> [~A] ~A~A~A"
                              status
                              tool-name
                              detail
                              error-fragment)))
           (push (list :id (list :stream-tool-call (or (getf entry :key) tool-name))
                       :text (%truncate-inline-text text safe-width)
                       :role :tool)
                 entries))))
     (chat-ui-state-stream-tool-calls chat-state))
    (sort entries #'string<
          :key (lambda (entry) (princ-to-string (getf entry :id))))))

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
        (%replace-message-at-index! messages target-index assistant-message))
      (when (or (not (token-stream-active-p (chat-ui-state-stream-state chat-state)))
                (chat-ui-state-stream-scroll-follow-p chat-state))
        (setf (chat-ui-state-message-scrollback-lines chat-state) 0))
      (%sync-chat-context-usage! chat-state)
      t)))

(defun %append-streaming-assistant-chunk (chat-state chunk)
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (target-index (token-stream-state-target-message-index stream-state))
         (messages (chat-ui-state-messages chat-state)))
    (when (and (integerp target-index)
               (>= target-index 0)
               (< target-index (length messages)))
      (let* ((message (nth target-index messages))
             (current-text (%message-content->text message))
             (next-text (concatenate 'string current-text (or chunk ""))))
        (%set-streaming-assistant-message chat-state target-index next-text :partialp t)))))

(defun %finalize-streaming-assistant-message (chat-state &key partialp)
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (target-index (token-stream-state-target-message-index stream-state))
         (messages (chat-ui-state-messages chat-state)))
    (when (and (integerp target-index)
               (>= target-index 0)
               (< target-index (length messages)))
      (let* ((message (nth target-index messages))
             (text (%message-content->text message)))
        (%set-streaming-assistant-message chat-state target-index text :partialp partialp)
        (let* ((updated-messages (chat-ui-state-messages chat-state))
               (updated-message (and (integerp target-index)
                                     (>= target-index 0)
                                     (< target-index (length updated-messages))
                                     (nth target-index updated-messages))))
          (when (pseudopod:message-p updated-message)
            (conversation-state-update-entry (%ensure-chat-conversation-state chat-state)
                                             target-index
                                             updated-message)))))
    (setf (chat-ui-state-stream-scroll-follow-p chat-state) t)))

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

(defun %collect-stream-tool-calls (chat-state)
  "Collect pseudopod:tool-call structs from the stream preview table."
  (let ((calls '()))
    (maphash
     (lambda (key entry)
       (declare (ignore key))
       (when (and (listp entry) (getf entry :executed-p))
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
  "Append tool-result messages to the conversation for each executed tool call."
  (dolist (entry tool-call-entries)
    (let* ((tc (getf entry :tool-call))
           (result (getf entry :result))
           (tool-call-id (and (pseudopod:tool-call-p tc)
                              (pseudopod:tool-call-id tc)))
           (tool-name (and (pseudopod:tool-call-p tc)
                           (pseudopod:tool-call-name tc)))
           (message (pseudopod:make-message
                     :role "tool"
                     :content (or result "")
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
        (conversation-transition! (%ensure-chat-conversation-state chat-state)
                                  :streaming)
        (chat-ui-add-message chat-state "assistant" "" :partial t)
        (%clear-stream-tool-tracking! chat-state)
        (token-stream-start
         stream-state
         (lambda (active-stream-state)
           (funcall runner
                    active-stream-state
                    ""
                    history
                    :system-prompt system-prompt
                    :client (chat-ui-state-stream-client chat-state)
                    :tools (%resolve-chat-tools chat-state)))
         :target-message-index target-index)))))

(defun %drain-stream-events (chat-state)
  (let ((conversation (%ensure-chat-conversation-state chat-state)))
  (token-stream-drain-events
   (chat-ui-state-stream-state chat-state)
   (lambda (event)
     (case (getf event :kind)
       (:text-delta
        (%append-streaming-assistant-chunk chat-state (getf event :text))
        (%emit-stream-budget-warning-if-needed chat-state))
       (:chunk
        (%append-streaming-assistant-chunk chat-state (getf event :text))
        (%emit-stream-budget-warning-if-needed chat-state))
       (:tool-call-delta
        (%update-stream-tool-call-preview! chat-state event))
       (:tool-call-started
        (let* ((entry (%update-stream-tool-call-preview! chat-state event))
               (tool-name (or (getf event :tool-name)
                              (and (listp entry) (getf entry :tool-name))))
               (tool-call-id (or (getf event :tool-call-id)
                                 (and (listp entry) (getf entry :tool-call-id))))
               (arguments (or (getf event :arguments)
                              (and (listp entry) (getf entry :arguments))))
               (index (or (getf event :index)
                          (and (listp entry) (getf entry :index)))))
          (publish (%context-event-bus chat-state)
                   (make-tool-call-started-event
                    :tool-name tool-name
                    :tool-call-id tool-call-id
                    :arguments arguments
                    :index index))))
       (:tool-call-argument-complete
        (let* ((entry (%update-stream-tool-call-preview! chat-state event))
               (tool-name (or (getf event :tool-name)
                              (and (listp entry) (getf entry :tool-name))))
               (tool-call-id (or (getf event :tool-call-id)
                                 (and (listp entry) (getf entry :tool-call-id))))
               (arguments (or (getf event :arguments)
                              (and (listp entry) (getf entry :arguments))))
               (index (or (getf event :index)
                          (and (listp entry) (getf entry :index)))))
          (publish (%context-event-bus chat-state)
                   (make-tool-call-argument-complete-event
                    :tool-name tool-name
                    :tool-call-id tool-call-id
                    :arguments arguments
                    :index index))
          (%execute-stream-tool-call! chat-state event)))
       (:complete
        (let ((tool-call-entries (%collect-stream-tool-calls chat-state))
              (malformed-names (%collect-malformed-tool-calls chat-state)))
          (when tool-call-entries
            (%set-assistant-message-tool-calls! chat-state tool-call-entries))
          (%finalize-streaming-assistant-message chat-state :partialp nil)
          (cond
            ;; Malformed tool calls (missing tool_call_id) — ask LLM to retry
            ((and malformed-names
                  (< (chat-ui-state-agentic-iteration-count chat-state)
                     +max-agentic-iterations+))
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
                     +max-agentic-iterations+))
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
             (conversation-transition! conversation :idle))
            ;; Normal text-only response
            (t
             (%clear-stream-tool-tracking! chat-state)
             (conversation-transition! conversation :idle)))))
       (:cancelled
        (%finalize-streaming-assistant-message chat-state :partialp t)
        (%clear-stream-tool-tracking! chat-state)
        (conversation-transition! conversation :idle))
       (:failed
        (%finalize-streaming-assistant-message chat-state :partialp t)
        (%clear-stream-tool-tracking! chat-state)
        (conversation-transition! conversation :error-recovery))
       (otherwise nil))))))

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
       (format nil "stream cancelled (~D tok, ~,1fs)"
               tokens
               (/ elapsed-ms 1000.0d0)))
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

(defun %start-streaming-assistant-response (chat-state user-message)
  (when (and (pseudopod:message-p user-message)
             (not (token-stream-active-p (chat-ui-state-stream-state chat-state))))
    (let ((runner (chat-ui-state-stream-runner chat-state)))
      (when (functionp runner)
        (let* ((prompt (%message-content->text user-message))
               (history (copy-list (chat-ui-state-messages chat-state)))
               (target-index (length history))
               (stream-state (chat-ui-state-stream-state chat-state))
               (system-prompt (%resolve-chat-system-prompt chat-state)))
          (setf (chat-ui-state-stream-system-prompt chat-state) system-prompt)
          (setf (chat-ui-state-stream-scroll-follow-p chat-state) t)
          (conversation-transition! (%ensure-chat-conversation-state chat-state)
                                    :streaming)
          (chat-ui-add-message chat-state "assistant" "" :partial t)
          (%clear-stream-tool-tracking! chat-state)
          (token-stream-start
           stream-state
           (lambda (active-stream-state)
             (funcall runner
                      active-stream-state
                      prompt
                      history
                      :system-prompt system-prompt
                      :client (chat-ui-state-stream-client chat-state)
                      :tools (%resolve-chat-tools chat-state)))
           :target-message-index target-index))))))

(defun %styled-segments->text (segments)
  (with-output-to-string (out)
    (dolist (segment segments)
      (write-string (or (getf segment :text) "") out))))

(defun %assistant-message-styled-lines (chat-state message message-index content-width)
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (stream-active-p (token-stream-active-p stream-state))
         (target-index (token-stream-state-target-message-index stream-state))
         (partialp (not (null (pseudopod:message-partial message))))
         (cursor-visible-p
           (and partialp
                stream-active-p
                (integerp target-index)
                (= target-index message-index)
                (stream-cursor-visible-p stream-state))))
    (stream-markdown-styled-lines (%message-content->text message)
                                  content-width
                                  :partialp partialp
                                  :cursor-visible-p cursor-visible-p)))

(defun %message-line-entries (chat-state messages width)
  (let ((entries '())
        (safe-width (max 1 width)))
    (loop for message in messages
          for index from 0 do
            (let* ((role (%normalize-chat-role (pseudopod:message-role message)))
                   (prefix (chat-role-prefix role))
                   (prefix-width (ptui.text.width:string-width prefix))
                   (content-width (max 1 (- safe-width (+ prefix-width 1))))
                   (indent (make-string (+ prefix-width 1) :initial-element #\Space)))
              (if (string= role "assistant")
                  (let ((styled-lines
                          (%assistant-message-styled-lines
                           chat-state
                           message
                           index
                           content-width)))
                    (loop for styled-line in styled-lines
                          for line-index from 0 do
                            (let* ((prefix-text (if (zerop line-index)
                                                    (concatenate 'string prefix " ")
                                                    indent))
                                   (prefix-segment (list :text prefix-text
                                                         :role role))
                                   (content-segments (or styled-line
                                                         (list (list :text ""
                                                                     :role role))))
                                   (segments (append (list prefix-segment)
                                                     content-segments)))
                              (push (list :id (list :chat-message index line-index)
                                          :text (%styled-segments->text segments)
                                          :role role
                                          :styled-segments segments)
                                    entries))))
                  (let* ((body (%message-content->text message))
                         (wrapped (ptui.text.layout:wrap-by-width body content-width))
                         (wrapped (if (null wrapped) (list "") wrapped)))
                    (loop for line in wrapped
                          for line-index from 0 do
                            (push (list :id (list :chat-message index line-index)
                                        :text (if (zerop line-index)
                                                  (format nil "~A ~A" prefix line)
                                                  (concatenate 'string indent line))
                                        :role role)
                                  entries))))
              (unless (= index (1- (length messages)))
                (push (list :id (list :chat-gap index)
                            :text ""
                            :role :meta)
                      entries))))
    (let ((preview-lines (%stream-tool-call-preview-lines chat-state safe-width)))
      (when (and entries preview-lines)
        (push (list :id :chat-stream-tool-gap :text "" :role :meta) entries))
      (dolist (preview preview-lines)
        (push preview entries)))
    (if entries
        (nreverse entries)
        (list (list :id :chat-empty
                    :text "No conversation yet. Type below and press Enter."
                    :role :system)))))

(defun %chat-text-widget (text id role &key styled-segments)
  (ptui.ui.elements:make-element
   :text
   :id id
   :props (list :text text :role role :styled-segments styled-segments)
   :children '()))

(defun %clamp-layout-size (size avail-width avail-height)
  (let ((w (ptui.layout:layout-size-width size))
        (h (ptui.layout:layout-size-height size)))
    (ptui.layout:make-layout-size
     (if avail-width
         (min w (max 0 avail-width))
         w)
     (if avail-height
         (min h (max 0 avail-height))
         h))))

(defun %element-prop (element key &optional default)
  (getf (ptui.ui.elements:ui-element-props element) key default))

(defun %element-id (element)
  (or (ptui.ui.elements:ui-element-id element)
      (ptui.ui.elements:ui-element-key element)))

(defun %ui-tree-node (element)
  (let ((id (%element-id element))
        (type (ptui.ui.elements:ui-element-type element))
        (children (mapcar #'%ui-tree-node
                          (ptui.ui.elements:ui-element-children element))))
    (ptui.layout:make-layout-node
     :id id
     :direction (if (eql type :stack)
                    (%element-prop element :direction :column)
                    :column)
     :gap (if (eql type :stack)
              (%element-prop element :gap 0)
              0)
     :measure (lambda (avail-width avail-height)
                (%clamp-layout-size
                 (ptui.widgets.core:widget-measure element)
                 avail-width
                 avail-height))
     :children children)))

(defun %chat-tree-signature (messages)
  (mapcar (lambda (message)
            (list (%normalize-chat-role (pseudopod:message-role message))
                  (%message-content->text message)))
          messages))

(defun %compute-scroll-offset (total-lines viewport-height scrollback-lines)
  (let* ((max-scrollback (max 0 (- total-lines viewport-height)))
         (bounded-scrollback (max 0 (min max-scrollback scrollback-lines)))
         (offset (- max-scrollback bounded-scrollback)))
    (values offset bounded-scrollback max-scrollback)))

(defun %chat-plan-mode-enabled-p ()
  (let ((config (%chat-config)))
    (and (config-p config)
         (not (null (config-value :plan-mode config))))))

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

(defun %chat-plan-presentation-steps (plan-state visible-steps execution-state)
  (let ((execution-step-table
          (when (%chat-plan-execution-surface-active-p execution-state)
            (let ((table (make-hash-table :test #'eql)))
              (dolist (execution-step (plan-execution-state-steps execution-state))
                (let ((step-index (plan-execution-step-index execution-step)))
                  (when (integerp step-index)
                    (setf (gethash step-index table) execution-step))))
              table))))
  (loop for step in visible-steps
          for step-index = (or (plan-step-index step) 0)
          for execution-step = (and execution-step-table
                                    (gethash step-index execution-step-table))
          for approved-p = (if execution-step
                               (plan-execution-step-approved-p execution-step)
                               (member step-index
                                       (plan-mode-state-approved-step-indexes plan-state)
                                       :test #'=))
          for status = (if execution-step
                           (case (plan-execution-step-status execution-step)
                             (:running :running)
                             (:completed :done)
                             (:pending (if approved-p :approved :pending))
                             (:failed :blocked)
                             (:aborted :blocked)
                             (otherwise (if approved-p :approved :pending)))
                           (if approved-p :approved :pending))
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
         (decision-text
           (string-downcase
            (symbol-name (or (plan-mode-state-review-decision plan-state)
                             :pending))))
         (pending-p (plan-mode-state-review-pending-p plan-state))
         (command-previews (%chat-plan-command-preview-lines plan-state
                                                             selected-step-index)))
    (if command-previews
        (if (integerp selected-step-index)
            (cons (format nil "Selected step: ~D (Ctrl-N/Ctrl-P to change)"
                          selected-step-index)
                  command-previews)
            command-previews)
        (append
         (when (integerp selected-step-index)
           (list (format nil "Selected step: ~D (Ctrl-N/Ctrl-P to change)"
                         selected-step-index)))
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
       :steps (%chat-plan-presentation-steps plan-state visible-steps execution-state)
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
          (copy-list (or (plan-execution-state-pending-step-indexes execution-state) '()))
          (copy-list (or (plan-execution-state-completed-step-indexes execution-state) '()))
          (loop for entry in (or (plan-execution-state-continuity-output execution-state) '())
                collect (list (plan-execution-output-entry-line entry)
                              (plan-execution-output-entry-step-index entry)
                              (plan-execution-output-entry-style entry)
                              (plan-execution-output-entry-severity entry)
                              (plan-execution-output-entry-phase entry)
                              (plan-execution-output-entry-timestamp entry))))))

(defun chat-ui-build-tree (state cols rows)
  (let* ((chat-state (ensure-chat-ui-state state))
         (plan-state (current-plan-mode-state))
         (picker-state (%chat-sync-fuzzy-picker! chat-state))
         (tree-state (%ensure-chat-tree-browser-state chat-state))
         (picker-widget
           (when (fuzzy-picker-state-active-p picker-state)
             (make-fuzzy-picker-widget picker-state)))
         (tree-widget
           (when (and (typep tree-state 'tree-browser-state)
                      (tree-browser-state-active-p tree-state))
             (make-tree-browser-widget tree-state)))
         (inner-width (max 20 (- cols 4)))
         (inner-height (max 8 (- rows 4)))
         (input (ptui.components.prompt-box:make-prompt-box-widget
                 (chat-ui-state-input-text chat-state)
                 :id :chat-input
                 :min-width 18
                 :max-width inner-width
                 :min-rows 1
                 :max-rows 4
                 :scroll-offset (chat-ui-state-prompt-scroll-offset chat-state)
                 :border-style :rounded))
         (input-height (ptui.layout:layout-size-height
                        (ptui.widgets.core:widget-measure input)))
         (header-height 2)
         (picker-height (if picker-widget
                            (ptui.layout:layout-size-height
                             (ptui.widgets.core:widget-measure picker-widget))
                            0))
         (tree-height (if tree-widget
                          (ptui.layout:layout-size-height
                           (ptui.widgets.core:widget-measure tree-widget))
                          0))
         (stream-stop-hint-widget
           (when (token-stream-active-p (chat-ui-state-stream-state chat-state))
             (%chat-text-widget "Streaming... Press Ctrl-C to stop early."
                                :chat-stream-stop-hint
                                :meta)))
         (plan-presentation-widget (%chat-plan-presentation-widget plan-state chat-state))
         (stream-stop-hint-height (if stream-stop-hint-widget 1 0))
         (plan-presentation-height
           (if plan-presentation-widget
               (ptui.layout:layout-size-height
                (ptui.widgets.core:widget-measure plan-presentation-widget))
               0))
         (history-height (max 1 (- inner-height
                                   input-height
                                   header-height
                                   picker-height
                                   tree-height
                                   plan-presentation-height
                                   stream-stop-hint-height
                                   1)))
         (message-lines (%message-line-entries chat-state
                                               (chat-ui-state-messages chat-state)
                                               inner-width))
         (message-widgets
           (mapcar (lambda (entry)
                     (%chat-text-widget (getf entry :text)
                                        (getf entry :id)
                                        (getf entry :role)
                                        :styled-segments (getf entry :styled-segments)))
                   message-lines))
         (history-stack (ptui.widgets.core:make-stack-widget
                         message-widgets
                         :id :chat-history-stack
                         :direction :column
                         :gap 0))
         (history-total-lines
           (ptui.layout:layout-size-height
            (ptui.widgets.core:widget-measure history-stack)))
         (history-offset 0)
         (scrollback 0)
         (max-scrollback 0))
    (multiple-value-setq (history-offset scrollback max-scrollback)
      (%compute-scroll-offset history-total-lines
                              history-height
                              (chat-ui-state-message-scrollback-lines chat-state)))
    (setf (chat-ui-state-message-scrollback-lines chat-state) scrollback
          (chat-ui-state-max-message-scrollback-lines chat-state) max-scrollback)
    (when (token-stream-active-p (chat-ui-state-stream-state chat-state))
      (setf (chat-ui-state-stream-scroll-follow-p chat-state)
            (zerop scrollback)))
    (let* ((history-scroll
             (ptui.widgets.core:make-scroll-widget
              history-stack
              :id :chat-history-scroll
              :viewport-width inner-width
              :viewport-height history-height
              :offset history-offset))
           (status-widget
             (make-status-bar-widget
              (chat-ui-state-status-bar-state chat-state)
              :id :chat-status-bar
              :width inner-width))
           (content
             (ptui.widgets.core:make-stack-widget
              (append
               (list
               (%chat-text-widget "amoebum chat" :chat-title :meta)
                status-widget)
               (if tree-widget
                   (list tree-widget)
                   '())
               (if plan-presentation-widget
                   (list plan-presentation-widget)
                   '())
               (list history-scroll)
               (if picker-widget
                   (list picker-widget)
                   '())
               (if stream-stop-hint-widget
                   (list stream-stop-hint-widget)
                   '())
               (list input))
              :id :chat-content
              :direction :column
              :gap 0)))
      (ptui.widgets.core:make-box-widget
       content
       :id :chat-root
       :padding 0
       :borderp t))))

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
  (let ((base
          (case (intern (string-upcase (princ-to-string role)) :keyword)
            (:system
             (%chat-template-cell :fg (ptui.core.color:make-color-rgb 255 205 120) :boldp t))
            (:user
             (%chat-template-cell :fg (ptui.core.color:make-color-rgb 130 210 255) :boldp t))
            (:assistant
             (%chat-template-cell :fg (ptui.core.color:make-color-rgb 150 235 170)))
            (:assistant-heading
             (%chat-template-cell :fg (ptui.core.color:make-color-rgb 195 235 255) :boldp t))
            (:assistant-code
             (%chat-template-cell :fg (ptui.core.color:make-color-rgb 175 215 255)))
            (:assistant-code-keyword
             (%chat-template-cell :fg (ptui.core.color:make-color-rgb 255 210 140) :boldp t))
            (:assistant-code-fence
             (%chat-template-cell :fg (ptui.core.color:make-color-rgb 145 165 185) :boldp t))
            (:tool
             (%chat-template-cell :fg (ptui.core.color:make-color-rgb 230 185 255)))
            (:prompt
             (%chat-template-cell :fg (ptui.core.color:make-color-rgb 210 210 210)))
            (:context-green
             (%chat-template-cell :fg (ptui.core.color:make-color-rgb 140 230 150) :boldp t))
            (:context-yellow
             (%chat-template-cell :fg (ptui.core.color:make-color-rgb 245 210 120) :boldp t))
            (:context-red
             (%chat-template-cell :fg (ptui.core.color:make-color-rgb 255 135 135) :boldp t))
            (otherwise
             (%chat-template-cell :fg (ptui.core.color:make-color-rgb 175 175 175)))))
        (styled nil))
    (setf styled (%chat-cell-with-attrs base
                                        :boldp boldp
                                        :italicp italicp
                                        :underlinep underlinep
                                        :invertp invertp
                                        :dimp dimp
                                        :strikep strikep))
    (if focusp
        (%chat-cell-with-attrs styled :boldp t :invertp t)
        styled)))

(defun %fit-line-width (text width)
  (ptui.text.layout:truncate-to-width text (max 0 width)))

(defun %prompt-wrapped-lines (value width)
  (if (<= width 0)
      (list "")
      (ptui.text.layout:wrap-by-width value (max 1 width))))

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

(defun %render-ui-element (buf element layout focus-id &key (dx 0) (dy 0))
  (let* ((id (%element-id element))
         (bounds (and id (ptui.layout:layout-bound layout id))))
    (when bounds
      (let* ((kind (ptui.ui.elements:ui-element-type element))
             (x (+ dx (ptui.layout:layout-bounds-x bounds)))
             (y (+ dy (ptui.layout:layout-bounds-y bounds)))
             (w (ptui.layout:layout-bounds-width bounds))
             (h (ptui.layout:layout-bounds-height bounds))
             (rect (ptui.core.types:make-rect x y w h)))
        (flet ((render-children (&key (child-dx dx)
                                      (child-dy dy)
                                      (clip-rect rect))
                 (ptui.render.buffer:with-clip (buf clip-rect)
                   (dolist (child (ptui.ui.elements:ui-element-children element))
                     (%render-ui-element buf child layout focus-id
                                         :dx child-dx
                                         :dy child-dy)))))
          (cond
            ((eq kind :text)
             (let* ((text (%element-prop element :text ""))
                    (role (%element-prop element :role :meta))
                    (styled-segments (%element-prop element :styled-segments nil))
                    (line (%fit-line-width text w))
                    (focusp (eql id focus-id))
                    (cell (chat-role-cell role :focusp focusp)))
               (ptui.render.buffer:buffer-draw-text
                buf
                x
                y
                (if (and (listp styled-segments) styled-segments)
                    (%styled-text-segments styled-segments :focusp focusp)
                    (list (list line cell)))
                :max-width w)))
            ((eq kind :input)
             (let* ((value (%element-prop element :value ""))
                    (line (%fit-line-width value w))
                    (cell (chat-role-cell :prompt :focusp (eql id focus-id))))
               (ptui.render.buffer:buffer-draw-text
                buf x y (list (list line cell)) :max-width w)))
            ((eq kind :prompt-box)
             (let* ((value (%element-prop element :value ""))
                    (border-style (%element-prop element :border-style :rounded))
                    (scroll-offset (%element-prop element :scroll-offset nil))
                    (inner-x (+ x 1))
                    (inner-y (+ y 1))
                    (inner-w (max 0 (- w 2)))
                    (inner-h (max 0 (- h 2)))
                    (line-cell (chat-role-cell :prompt :focusp (eql id focus-id)))
                    (lines (%prompt-wrapped-lines value inner-w)))
               (ptui.render.buffer:buffer-draw-border
                buf rect :style line-cell :border-style border-style)
               (multiple-value-bind (visible-lines effective-offset max-offset)
                   (%prompt-visible-lines lines inner-h scroll-offset)
                 (declare (ignore effective-offset max-offset))
                 (loop for line in visible-lines
                       for row from 0 do
                         (ptui.render.buffer:buffer-draw-text
                          buf
                          inner-x
                          (+ inner-y row)
                          (list (list (%fit-line-width line inner-w) line-cell))
                          :max-width inner-w)))))
            ((eq kind :spacer)
             nil)
            ((eq kind :box)
             (let* ((padding (%element-prop element :padding 0))
                    (borderp (%element-prop element :borderp nil))
                    (border (if borderp 1 0))
                    (inset (+ border padding))
                    (inner-rect (ptui.core.types:make-rect
                                 (+ x inset)
                                 (+ y inset)
                                 (max 0 (- w (* 2 inset)))
                                 (max 0 (- h (* 2 inset)))))
                    (child (first (ptui.ui.elements:ui-element-children element))))
               (when borderp
                 (ptui.render.buffer:buffer-draw-border buf rect))
               (when child
                 (let* ((child-id (%element-id child))
                        (child-bounds
                          (and child-id (ptui.layout:layout-bound layout child-id))))
                   (when child-bounds
                     (let ((delta-x (- (ptui.core.types:rect-x inner-rect)
                                       (ptui.layout:layout-bounds-x child-bounds)))
                           (delta-y (- (ptui.core.types:rect-y inner-rect)
                                       (ptui.layout:layout-bounds-y child-bounds))))
                       (ptui.render.buffer:with-clip (buf inner-rect)
                         (%render-ui-element buf child layout focus-id
                                             :dx delta-x
                                             :dy delta-y))))))))
            ((eq kind :stack)
             (render-children))
            ((eq kind :scroll)
             (let* ((offset (%element-prop element :offset 0))
                    (child (first (ptui.ui.elements:ui-element-children element))))
               (when child
                 (ptui.render.buffer:with-clip (buf rect)
                   (%render-ui-element buf child layout focus-id
                                       :dx dx
                                       :dy (- dy offset))))))
            (t
             (render-children))))))))

(defun %chat-tree-key (state cols rows)
  (list cols
        rows
        (chat-ui-state-input-text state)
        (chat-ui-state-message-scrollback-lines state)
        (chat-ui-state-prompt-scroll-offset state)
        (%chat-tree-signature (chat-ui-state-messages state))
        (tree-browser-render-key
         (%ensure-chat-tree-browser-state state))
        (fuzzy-picker-render-key
         (%ensure-chat-fuzzy-picker-state state))
        (%chat-plan-workspace-tree-key state)
        (%stream-tree-key state)))

(defun render-chat-ui-buffer (state size)
  (let* ((chat-state (ensure-chat-ui-state state))
         (runtime (chat-ui-state-runtime chat-state))
         (cols (ptui.core.types:size-cols size))
         (rows (ptui.core.types:size-rows size))
         (agent-completion-count (%inject-agent-completions chat-state))
         (drained-event-count (%drain-stream-events chat-state))
         (stream-summary (%publish-status-bar-stream-summary-if-needed chat-state))
         (picker-state (%chat-sync-fuzzy-picker! chat-state))
         (tree-key (%chat-tree-key chat-state cols rows))
         (tree (chat-ui-state-cached-tree chat-state))
         (layout (chat-ui-state-cached-layout chat-state))
         (focus-id nil))
    (declare (ignore agent-completion-count drained-event-count stream-summary picker-state))
    (%sync-chat-context-usage! chat-state)
    (%emit-stream-budget-warning-if-needed chat-state)
    (let ((checkpoint
            (maybe-auto-checkpoint
             :conversation (%ensure-chat-conversation-state chat-state)
             :config (%chat-config)
             :busy-p (token-stream-active-p (chat-ui-state-stream-state chat-state)))))
      (declare (ignore checkpoint)))
    (incf (chat-ui-state-frame-count chat-state))
    (unless (and tree layout
                 (equal tree-key (chat-ui-state-cached-tree-key chat-state)))
      (setf tree (chat-ui-build-tree chat-state cols rows))
      (setf layout (ptui.layout:compute-layout
                    (%ui-tree-node tree)
                    :x 2
                    :y 1
                    :width (max 4 (- cols 4))
                    :height (max 4 (- rows 2))))
      (ptui.ui.runtime:update-runtime runtime tree)
      (setf (chat-ui-state-cached-tree-key chat-state) tree-key
            (chat-ui-state-cached-tree chat-state) tree
            (chat-ui-state-cached-layout chat-state) layout
            (chat-ui-state-cached-render-key chat-state) nil
            (chat-ui-state-cached-buffer chat-state) nil))
    (setf focus-id (ptui.ui.runtime:runtime-focus-id runtime))
    (let* ((render-key (list tree-key focus-id))
           (cached-render-key (chat-ui-state-cached-render-key chat-state))
           (cached-buffer (chat-ui-state-cached-buffer chat-state)))
      (if (and cached-buffer (equal render-key cached-render-key))
          cached-buffer
          (let ((buf (ptui.render.buffer:make-buffer cols rows)))
            (%render-ui-element buf tree layout focus-id)
            (setf (chat-ui-state-cached-render-key chat-state) render-key
                  (chat-ui-state-cached-buffer chat-state) buf)
            buf)))))

(defun %pop-last-grapheme (text)
  (let ((clusters (ptui.text.grapheme:split-graphemes text)))
    (if (null clusters)
        ""
        (with-output-to-string (out)
          (dolist (cluster (butlast clusters))
            (write-string cluster out))))))

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
           (chat-ui-state-max-message-scrollback-lines chat-state) 0))
    (:compact-chat
     (let ((compression
             (%compress-chat-history!
              chat-state
              :keep-last-turns (slash-command-result-payload result)
              :trigger :manual)))
       (setf action-output (%format-compression-output compression))))
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

(defun %handle-fuzzy-picker-key (chat-state key)
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

(defun %handle-tree-browser-key (chat-state key)
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

(defun %handle-input-key (state key text)
  (cond
    ((eql key :ctrl-p)
     (%chat-plan-move-selection! state -1))
    ((eql key :ctrl-n)
     (%chat-plan-move-selection! state 1))
    ((eql key :ctrl-r)
     (if (chat-ui-state-history-search-active-p state)
         (%chat-deactivate-history-search! state :restore-input-p t)
         (%chat-activate-history-search! state))
     t)
    ((and (eql key :text) (stringp text))
     (chat-ui-set-input state
                        (concatenate 'string
                                     (chat-ui-state-input-text state)
                                     text))
     t)
    ((eql key :ctrl-j)
     (chat-ui-set-input state
                        (concatenate 'string
                                     (chat-ui-state-input-text state)
                                     (string #\Newline)))
     t)
    ((eql key :tab)
     (%handle-command-tab-completion state))
    ((or (eql key :enter) (eql key :return))
     (let ((input (chat-ui-state-input-text state)))
       (if (%handle-slash-command-input state input)
           t
           (if (%handle-plan-mode-entry-instruction state input)
               t
               (let ((submitted (chat-ui-submit-input state)))
                 (when submitted
                   (if (%handle-memory-candidate state submitted)
                       (conversation-transition! (%ensure-chat-conversation-state state)
                                                 :idle)
                       (%start-streaming-assistant-response state submitted)))))))
     t)
    ((eql key :backspace)
     (chat-ui-set-input state
                        (%pop-last-grapheme (chat-ui-state-input-text state)))
     t)
    (t
     nil)))

(defun %handle-scroll-key (state key)
  (case key
    (:up (chat-ui-scroll-history state 1))
    (:down (chat-ui-scroll-history state -1))
    (:pgup (chat-ui-scroll-history state 5))
    ((:pgdn :pgdown) (chat-ui-scroll-history state -5))
    (otherwise nil)))

(defun handle-chat-ui-event (state event)
  (let* ((chat-state (ensure-chat-ui-state state))
         (runtime (chat-ui-state-runtime chat-state))
         (agent-completion-count (%inject-agent-completions chat-state))
         (drained-event-count (%drain-stream-events chat-state))
         (route (if (typep event 'ptui.core.events:key-event)
                    (ptui.ui.runtime:route-event runtime event)
                    (list :kind :unhandled :event event)))
         (kind (getf route :kind))
         (target (getf route :target)))
    (declare (ignore agent-completion-count drained-event-count))
    (when (typep event 'ptui.core.events:key-event)
      (checkpoint-mark-activity)
      (let ((key (ptui.core.events:key-event-key event))
            (text (ptui.core.events:key-event-text? event)))
        (when (and (member key '(:escape :ctrl-c))
                   (token-stream-active-p (chat-ui-state-stream-state chat-state)))
          (token-stream-request-cancel (chat-ui-state-stream-state chat-state)))
        (unless (%handle-fuzzy-picker-key chat-state key)
          (unless (%handle-tree-browser-key chat-state key)
            (%handle-scroll-key chat-state key)
            (when (or (eql target :chat-input)
                      (eql kind :unhandled)
                      (null target))
              (%handle-input-key chat-state key text))))))
    (when (and (ptui.ui.runtime:runtime-root runtime)
               (listp route))
      (ptui.widgets.core:dispatch-widget-event
       (ptui.ui.runtime:runtime-root runtime)
       route))
    (%drain-stream-events chat-state)
    (%publish-status-bar-stream-summary-if-needed chat-state)
    (%sync-chat-context-usage! chat-state)
    (%emit-stream-budget-warning-if-needed chat-state)
    chat-state))

(defun run-chat-ui (&key (backend :auto) (fps 20) initial-state)
  (let ((resolved-state
          (if initial-state
              initial-state
              (chat-ui-restore-latest-session (make-chat-ui-state)))))
    (checkpoint-mark-activity)
    (load-user-extensions)
    (ptui.engine.loop:run #'render-chat-ui-buffer
                          :backend backend
                          :fps fps
                          :initial-state (ensure-chat-ui-state resolved-state)
                          :on-event #'handle-chat-ui-event)))
