(in-package :amoebum)

;;; NXT-543: slash-command actions, plan-mode entry instructions, key
;;; handler tables, page-scroll routing, approval dialog interception, and
;;; the vertical cursor-move helper extracted from ui/chat-input.lisp. The
;;; residual chat-input.lisp keeps only the top-level event facade
;;; (handle-chat-ui-event) and very small helpers it inlines.

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

(defun %slash-command-clear-chat-action (result &key chat-state)
  (declare (ignore result))
  (let ((conversation (%ensure-chat-conversation-state chat-state)))
    (conversation-reset! conversation)
    (%chat-deactivate-history-search! chat-state)
    (setf (chat-ui-state-messages chat-state)
          (conversation-state-messages conversation)
          (chat-ui-state-message-scrollback-lines chat-state) 0
          (chat-ui-state-max-message-scrollback-lines chat-state) 0)
    (%invalidate-styled-lines-cache)
    (%sync-chat-context-usage! chat-state :allow-auto-compress-p nil)
    nil))

(defun %slash-command-compact-chat-action (result &key chat-state)
  (let ((compression
          (%compress-chat-history!
           chat-state
           :keep-last-turns (slash-command-result-payload result)
           :trigger :manual)))
    (%sync-chat-context-usage! chat-state :allow-auto-compress-p nil)
    (%format-compression-output compression)))

(defun %slash-command-toggle-provider-dashboard-action (result &key chat-state)
  (let* ((payload (slash-command-result-payload result))
         (current (chat-ui-state-provider-dashboard-visible-p chat-state))
         (next
           (case payload
             ((:on t) t)
             ((:off nil) nil)
             (otherwise (not current)))))
    (setf (chat-ui-state-provider-dashboard-visible-p chat-state) next)
    (provider-health-refresh! :force t)
    (%sync-chat-context-usage! chat-state :allow-auto-compress-p nil)
    (format nil "Provider dashboard ~:[hidden~;visible~]." next)))

(eval-when (:load-toplevel :execute)
  (register-slash-command-action-handler :clear-chat #'%slash-command-clear-chat-action)
  (register-slash-command-action-handler :compact-chat #'%slash-command-compact-chat-action)
  (register-slash-command-action-handler :toggle-provider-dashboard
                                         #'%slash-command-toggle-provider-dashboard-action))

(defun %handle-slash-command-input (chat-state input)
  (multiple-value-bind (handledp result)
      (resolve-slash-command input
                             :config (current-config)
                             :memory-backend (current-memory-backend)
                             :chat-state chat-state)
    (when handledp
      (let ((action-output (apply-slash-command-result-action result :chat-state chat-state)))
        (when (slash-command-result-echo-input-p result)
          (chat-ui-add-message chat-state "user" input))
        (let ((output (or action-output
                          (slash-command-result-output result))))
          (when (and (stringp output)
                     (plusp (length (%slash-trim output))))
            (chat-ui-add-message chat-state "system" output)))
        (setf (chat-ui-state-input-text chat-state) ""
              (chat-ui-state-prompt-scroll-offset chat-state) nil)
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

(defun %chat-input-route-submitted-message (chat-state submitted-message)
  (when submitted-message
    (if (%handle-memory-candidate chat-state submitted-message)
        (conversation-transition! (%ensure-chat-conversation-state chat-state)
                                  :idle)
        (%start-streaming-assistant-response chat-state submitted-message))
    t))

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

(defun %chat-input-handle-submit-routing (chat-state input)
  (cond
    ((and (plan-step-awaiting-approval-p)
          (zerop (length input)))
     (approve-next-plan-step)
     t)
    ((%handle-slash-command-input chat-state input)
     t)
    ((%handle-plan-mode-entry-instruction chat-state input)
     t)
    (t
     (%chat-input-route-submitted-message
      chat-state
      (chat-ui-submit-input chat-state)))))

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

(defun %chat-input-handle-vertical-cursor-move (chat-state cursor-pos input-width delta)
  (let* ((input-text (chat-ui-state-input-text chat-state))
         (pos (%ensure-cursor-pos input-text cursor-pos))
         (lines (%prompt-wrapped-lines input-text input-width))
         (max-rows 4))
    (multiple-value-bind (cur-line cur-col)
        (%cursor-to-line-col pos lines)
      (let ((target-line (+ cur-line delta)))
        (when (and (>= target-line 0)
                   (< target-line (length lines)))
          (let* ((line (nth target-line lines))
                 (target-col (min cur-col (length line)))
                 (new-pos (%line-col-to-cursor-pos target-line target-col lines))
                 (last-line-p (= target-line (1- (length lines)))))
            (setf (chat-ui-state-cursor-position chat-state)
                  (if (and last-line-p
                           (= target-col (length line)))
                      nil
                      new-pos))
            (let* ((current-scroll (or (chat-ui-state-prompt-scroll-offset chat-state) 0))
                   (total-lines (length lines))
                   (visible-rows (min max-rows total-lines))
                   (max-scroll (max 0 (- total-lines visible-rows)))
                   (new-scroll
                     (cond
                       ((< target-line current-scroll)
                        target-line)
                       ((>= target-line (+ current-scroll visible-rows))
                        (max 0 (- target-line visible-rows -1)))
                       (t current-scroll))))
              (setf (chat-ui-state-prompt-scroll-offset chat-state)
                    (min new-scroll max-scroll)))
            t))))))

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
      ;; Up/Down with empty input -> scroll history (not input cursor movement)
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
                    (ignore-errors
                      (token-stream-force-reset-if-stuck stream-state))
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
      (ignore-errors
        (token-stream-force-reset-if-stuck stream-state))
      :consume)))

(defun %chat-ui-handle-ctrl-c-key (context key)
  (declare (ignore key))
  (let ((chat-state (getf context :chat-state))
        (stream-state (getf context :stream-state)))
    (cond
      ((token-stream-active-p stream-state)
       (token-stream-request-cancel stream-state)
       (ignore-errors
         (token-stream-force-reset-if-stuck stream-state))
       :consume)
      ((%chat-ctrl-c-quit-armed-p chat-state)
       (%chat-disarm-ctrl-c-quit! chat-state)
       :quit)
      (t
       (%chat-arm-ctrl-c-quit! chat-state)
       :consume))))

(defun %chat-ui-scroll-page-lines (chat-state)
  (max 5
       (floor (max 10
                   (1+ (chat-ui-state-max-message-scrollback-lines chat-state)))
              3)))

(defun %chat-ui-handle-page-scroll-key (context key)
  (let ((chat-state (getf context :chat-state)))
    (case key
      (:pgup
       (chat-ui-scroll-history chat-state (%chat-ui-scroll-page-lines chat-state))
       :consume)
      (:pgdn
       (chat-ui-scroll-history chat-state (- (%chat-ui-scroll-page-lines chat-state)))
       :consume)
      (:home
       (setf (chat-ui-state-message-scrollback-lines chat-state)
             (max 0 (chat-ui-state-max-message-scrollback-lines chat-state))
             (chat-ui-state-stream-scroll-follow-p chat-state) nil)
       :consume)
      (:end
       (setf (chat-ui-state-message-scrollback-lines chat-state) 0
             (chat-ui-state-stream-scroll-follow-p chat-state) t)
       :consume)
      (otherwise
       nil))))

(defun %chat-ui-yaml-reload-modal-active-p (chat-state)
  "Return T when any modal mode is active that should block the YAML reload key.
Mirrors the modal predicates used by chat-panel's defpanel `(:mode ...)` clauses."
  (let* ((approval-state (chat-ui-state-approval-dialog-state chat-state))
         (picker-state (chat-ui-state-fuzzy-picker-state chat-state))
         (tree-state (chat-ui-state-tree-browser-state chat-state)))
    (or (and approval-state (approval-dialog-state-active-p approval-state))
        (and picker-state (fuzzy-picker-state-active-p picker-state))
        (and (typep tree-state 'tree-browser-state)
             (tree-browser-state-active-p tree-state))
        (chat-ui-state-history-search-active-p chat-state))))

(defun %chat-ui-yaml-reload-key-active-p (chat-state key text)
  "Return T when KEY is a single-char :text event matching the configured
yaml-theme-reload-key, the prompt is empty, and no modal mode is active.
This mirrors the existing default-mode pattern that only consumes a key on
empty input (see chat-panel.lisp :up/:down handling)."
  (and (eq key :text)
       (stringp text)
       (= 1 (length text))
       (string= text (yaml-theme-reload-key))
       (zerop (length (chat-ui-state-input-text chat-state)))
       (not (%chat-ui-yaml-reload-modal-active-p chat-state))))

(defun %chat-ui-handle-yaml-reload-key (context key text)
  "Intercept the YAML reload key. Returns :consume on a successful intercept,
NIL otherwise so the caller falls through to the normal text-insertion path."
  (let ((chat-state (getf context :chat-state)))
    (when (%chat-ui-yaml-reload-key-active-p chat-state key text)
      (%chat-handle-yaml-reload-key! chat-state)
      :consume)))

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
      ((%chat-ui-handle-page-scroll-key context key)
       :consume)
      ((%chat-ui-handle-yaml-reload-key context key text))
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
