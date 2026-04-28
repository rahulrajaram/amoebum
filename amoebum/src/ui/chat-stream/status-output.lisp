(in-package :amoebum)

;;; NXT-428: status publishing, stream-budget enforcement, and stream-output
;;; helpers extracted from ui/chat-stream.lisp. Loaded before the residual
;;; chat-stream coordinator so the root file can stay focused on turn setup.

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

(defun %stream-status-fragment (chat-state)
  (let* ((summary (%stream-status-summary chat-state))
         (status (getf summary :status))
         (tokens (or (getf summary :tokens) 0))
         (elapsed-ms (or (getf summary :elapsed-ms) 0))
         (tps (or (getf summary :tokens-per-second) 0.0d0))
         (error-message (getf summary :error-message)))
    (if (null status)
        nil
        (amoebum.fp:match status
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
               "stream failed"))))))
