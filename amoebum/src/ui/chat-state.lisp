(in-package :amoebum)

;;; NXT-431: state defaults, conversation-entry coercion, slash-descriptor
;;; parsing, and snapshot-metadata formatting now live in dedicated
;;; ui/chat-state/* submodules. This residual file keeps the coordinating
;;; context/message/state behavior that downstream UI modules call directly.

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

(defun chat-ui-restore-latest-session (&optional state)
  (let* ((chat-state (ensure-chat-ui-state state))
         (project-root (%chat-project-root))
         (restored (conversation-load-latest :project-root project-root)))
    (%apply-chat-conversation! chat-state restored)
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
