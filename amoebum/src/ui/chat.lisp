(in-package :amoebum)

(defparameter +chat-role-order+ '("system" "user" "assistant" "tool"))
(defparameter +context-compression-default-keep-last-turns+ 6)
(defparameter +context-compression-min-summarized-messages+ 2)
(defparameter +context-compression-max-summary-points+ 4)
(defparameter +context-compression-snippet-chars+ 96)

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
                      (status-bar-state (make-status-bar-state))
                      (conversation nil)
                      (context-used-tokens 0)
                      (context-window-limit +default-context-window-limit+)
                      (stream-status-publish-key nil)
                      (frame-count 0))))
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
  (status-bar-state (make-status-bar-state) :type status-bar-state)
  (conversation nil)
  (context-used-tokens 0 :type integer)
  (context-window-limit +default-context-window-limit+ :type integer)
  (stream-status-publish-key nil)
  (frame-count 0 :type fixnum)
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
    (setf (chat-ui-state-status-bar-state chat-state)
          (ensure-status-bar-state
           (chat-ui-state-status-bar-state chat-state)
           :event-bus (current-event-bus)))
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
    (chat-ui-state-input-text chat-state)))

(defun %blank-string-p (text)
  (every (lambda (char)
           (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=))
         text))

(defun chat-ui-submit-input (state)
  (let* ((chat-state (ensure-chat-ui-state state))
         (conversation (%ensure-chat-conversation-state chat-state))
         (input (chat-ui-state-input-text chat-state)))
    (if (or (null input) (zerop (length input)) (%blank-string-p input))
        nil
        (prog1
            (progn
              (conversation-transition! conversation :user-input)
              (chat-ui-add-message chat-state "user" input))
          (setf (chat-ui-state-input-text chat-state) ""
                (chat-ui-state-prompt-scroll-offset chat-state) nil)))))

(defun chat-ui-scroll-history (state delta-lines)
  (let* ((chat-state (ensure-chat-ui-state state))
         (max-scrollback (max 0 (chat-ui-state-max-message-scrollback-lines chat-state)))
         (next-scrollback (+ (chat-ui-state-message-scrollback-lines chat-state)
                             (or delta-lines 0))))
    (setf (chat-ui-state-message-scrollback-lines chat-state)
          (max 0 (min max-scrollback next-scrollback)))
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

(defun %stream-status-summary (chat-state)
  (token-stream-progress-summary (chat-ui-state-stream-state chat-state)))

(defun %stream-summary-publish-key (summary)
  (let ((status (or (getf summary :status) :idle))
        (tokens (or (getf summary :tokens) 0))
        (chunks (or (getf summary :chunks) 0))
        (cancel-requested-p (not (null (getf summary :cancel-requested-p))))
        (tokens-per-second (or (getf summary :tokens-per-second) 0.0d0))
        (elapsed-ms (or (getf summary :elapsed-ms) 0)))
    (list status
          tokens
          chunks
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
          (if (eq status :running)
              (truncate elapsed-ms 100)
              elapsed-ms)
          (getf summary :cancel-requested-p)
          (getf summary :error-message))))

(defun %set-streaming-assistant-message (chat-state target-index text &key partialp)
  (let ((messages (chat-ui-state-messages chat-state)))
    (when (and (integerp target-index)
               (>= target-index 0)
               (< target-index (length messages)))
      (%replace-message-at-index!
       messages
       target-index
       (make-chat-message "assistant"
                          (or text "")
                          :partial partialp))
      (setf (chat-ui-state-message-scrollback-lines chat-state) 0)
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
                                             updated-message)))))))

(defun %drain-stream-events (chat-state)
  (let ((conversation (%ensure-chat-conversation-state chat-state)))
  (token-stream-drain-events
   (chat-ui-state-stream-state chat-state)
   (lambda (event)
     (case (getf event :kind)
       (:chunk
        (%append-streaming-assistant-chunk chat-state (getf event :text)))
       (:complete
        (%finalize-streaming-assistant-message chat-state :partialp nil)
        (conversation-transition! conversation :idle))
       (:cancelled
        (%finalize-streaming-assistant-message chat-state :partialp t)
        (conversation-transition! conversation :idle))
       (:failed
        (%finalize-streaming-assistant-message chat-state :partialp t)
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
          (conversation-transition! (%ensure-chat-conversation-state chat-state)
                                    :streaming)
          (chat-ui-add-message chat-state "assistant" "" :partial t)
          (token-stream-start
           stream-state
           (lambda (active-stream-state)
             (funcall runner
                      active-stream-state
                      prompt
                      history
                      :system-prompt system-prompt
                      :client (chat-ui-state-stream-client chat-state)
                      :tools (chat-ui-state-stream-tools chat-state)))
           :target-message-index target-index))))))

(defun %message-line-entries (messages width)
  (let ((entries '())
        (safe-width (max 1 width)))
    (loop for message in messages
          for index from 0 do
            (let* ((role (%normalize-chat-role (pseudopod:message-role message)))
                   (prefix (chat-role-prefix role))
                   (body (%message-content->text message))
                   (prefix-width (ptui.text.width:string-width prefix))
                   (content-width (max 1 (- safe-width (+ prefix-width 1))))
                   (wrapped (ptui.text.layout:wrap-by-width body content-width))
                   (wrapped (if (null wrapped) (list "") wrapped))
                   (indent (make-string (+ prefix-width 1) :initial-element #\Space)))
              (loop for line in wrapped
                    for line-index from 0 do
                      (push (list :id (list :chat-message index line-index)
                                  :text (if (zerop line-index)
                                            (format nil "~A ~A" prefix line)
                                            (concatenate 'string indent line))
                                  :role role)
                            entries))
              (unless (= index (1- (length messages)))
                (push (list :id (list :chat-gap index)
                            :text ""
                            :role :meta)
                      entries))))
    (if entries
        (nreverse entries)
        (list (list :id :chat-empty
                    :text "No conversation yet. Type below and press Enter."
                    :role :system)))))

(defun %chat-text-widget (text id role)
  (ptui.ui.elements:make-element
   :text
   :id id
   :props (list :text text :role role)
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

(defun chat-ui-build-tree (state cols rows)
  (let* ((chat-state (ensure-chat-ui-state state))
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
         (history-height (max 1 (- inner-height input-height header-height 1)))
         (message-lines (%message-line-entries (chat-ui-state-messages chat-state)
                                               inner-width))
         (message-widgets
           (mapcar (lambda (entry)
                     (%chat-text-widget (getf entry :text)
                                        (getf entry :id)
                                        (getf entry :role)))
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
              (list
               (%chat-text-widget "amoebum chat" :chat-title :meta)
               status-widget
               history-scroll
               input)
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

(defun chat-role-cell (role &key (focusp nil))
  (let ((base
          (case (intern (string-upcase (princ-to-string role)) :keyword)
            (:system
             (%chat-template-cell :fg (ptui.core.color:make-color-rgb 255 205 120) :boldp t))
            (:user
             (%chat-template-cell :fg (ptui.core.color:make-color-rgb 130 210 255) :boldp t))
            (:assistant
             (%chat-template-cell :fg (ptui.core.color:make-color-rgb 150 235 170)))
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
             (%chat-template-cell :fg (ptui.core.color:make-color-rgb 175 175 175))))))
    (if focusp
        (ptui.core.types:make-cell
         " "
         (ptui.core.types:cell-fg base)
         (ptui.core.types:cell-bg base)
         (ptui.core.types:make-attrs :boldp t :invertp t))
        base)))

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
      (let* ((text
               (cond
                 ((and (consp segment) (stringp (car segment)))
                  (car segment))
                 ((stringp segment)
                  segment)
                 (t
                  (princ-to-string segment))))
             (role
               (cond
                 ((and (consp segment) (cdr segment))
                  (cdr segment))
                 ((and (listp segment) (getf segment :role))
                  (getf segment :role))
                 (t
                  :meta))))
        (when (plusp (length text))
          (push (list text (chat-role-cell role :focusp focusp)) result))))
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
        (%stream-tree-key state)))

(defun render-chat-ui-buffer (state size)
  (let* ((chat-state (ensure-chat-ui-state state))
         (runtime (chat-ui-state-runtime chat-state))
         (cols (ptui.core.types:size-cols size))
         (rows (ptui.core.types:size-rows size))
         (agent-completion-count (%inject-agent-completions chat-state))
         (drained-event-count (%drain-stream-events chat-state))
         (stream-summary (%publish-status-bar-stream-summary-if-needed chat-state))
         (tree-key (%chat-tree-key chat-state cols rows))
         (tree (chat-ui-state-cached-tree chat-state))
         (layout (chat-ui-state-cached-layout chat-state))
         (focus-id nil))
    (declare (ignore agent-completion-count drained-event-count stream-summary))
    (%sync-chat-context-usage! chat-state)
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
     (setf (chat-ui-state-messages chat-state) '()
           (chat-ui-state-message-scrollback-lines chat-state) 0
           (chat-ui-state-max-message-scrollback-lines chat-state) 0)
     (conversation-transition! conversation :idle))
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

(defun %handle-input-key (state key text)
  (cond
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
           (let ((submitted (chat-ui-submit-input state)))
             (when submitted
               (if (%handle-memory-candidate state submitted)
                   (conversation-transition! (%ensure-chat-conversation-state state)
                                             :idle)
                   (%start-streaming-assistant-response state submitted))))))
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
    (:pgdn (chat-ui-scroll-history state -5))
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
      (let ((key (ptui.core.events:key-event-key event))
            (text (ptui.core.events:key-event-text? event)))
        (when (and (member key '(:escape :ctrl-c))
                   (token-stream-active-p (chat-ui-state-stream-state chat-state)))
          (token-stream-request-cancel (chat-ui-state-stream-state chat-state)))
        (%handle-scroll-key chat-state key)
        (when (or (eql target :chat-input)
                  (eql kind :unhandled)
                  (null target))
          (%handle-input-key chat-state key text))))
    (when (and (ptui.ui.runtime:runtime-root runtime)
               (listp route))
      (ptui.widgets.core:dispatch-widget-event
       (ptui.ui.runtime:runtime-root runtime)
       route))
    (%drain-stream-events chat-state)
    (%publish-status-bar-stream-summary-if-needed chat-state)
    (%sync-chat-context-usage! chat-state)
    chat-state))

(defun run-chat-ui (&key (backend :auto) (fps 20) initial-state)
  (let ((resolved-state
          (if initial-state
              initial-state
              (chat-ui-restore-latest-session (make-chat-ui-state)))))
  (ptui.engine.loop:run #'render-chat-ui-buffer
                        :backend backend
                        :fps fps
                        :initial-state (ensure-chat-ui-state resolved-state)
                        :on-event #'handle-chat-ui-event)))
