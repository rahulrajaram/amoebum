(in-package :amoebum)

(defparameter +chat-stream-default-system-prompt+
  "You are a helpful assistant.")

(define-condition token-stream-cancelled (condition)
  ())

(defstruct (token-stream-state
            (:constructor make-token-stream-state
                (&key
                   (status :idle)
                   (events (ptui.runtime.queue:make-event-queue))
                   (started-ms 0)
                   (ended-ms 0)
                   (token-count 0)
                   (chunk-count 0)
                   (target-message-index nil)
                   (cancel-requested-p nil)
                   (error-message nil))))
  (status :idle)
  (events (ptui.runtime.queue:make-event-queue)
          :type ptui.runtime.queue:event-queue)
  (started-ms 0 :type integer)
  (ended-ms 0 :type integer)
  (token-count 0 :type fixnum)
  (chunk-count 0 :type fixnum)
  target-message-index
  (cancel-requested-p nil :type boolean)
  (error-message nil)
  (worker-thread nil)
  (lock (bordeaux-threads:make-lock "amoebum-token-stream-lock")))

(defmacro %with-token-stream-lock ((stream-state) &body body)
  `(bordeaux-threads:with-lock-held ((token-stream-state-lock ,stream-state))
     ,@body))

(defun %token-stream-now-ms ()
  (ptui.util.time:monotonic-ms))

(defun %token-stream-blank-string-p (value)
  (or (null value)
      (zerop (length value))
      (every (lambda (char)
               (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=))
             value)))

(defun %token-stream-estimate-token-count (text)
  (if (%token-stream-blank-string-p text)
      0
      (let ((count 0)
            (inside-token-p nil))
        (loop for char across text do
          (if (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=)
              (setf inside-token-p nil)
              (unless inside-token-p
                (incf count)
                (setf inside-token-p t))))
        count)))

(defun %token-stream-reset! (stream-state)
  (%with-token-stream-lock (stream-state)
    (setf (token-stream-state-status stream-state) :idle
          (token-stream-state-started-ms stream-state) 0
          (token-stream-state-ended-ms stream-state) 0
          (token-stream-state-token-count stream-state) 0
          (token-stream-state-chunk-count stream-state) 0
          (token-stream-state-target-message-index stream-state) nil
          (token-stream-state-cancel-requested-p stream-state) nil
          (token-stream-state-error-message stream-state) nil
          (token-stream-state-worker-thread stream-state) nil))
  (ptui.runtime.queue:queue-pop-all (token-stream-state-events stream-state))
  stream-state)

(defun token-stream-active-p (stream-state)
  (and (typep stream-state 'token-stream-state)
       (%with-token-stream-lock (stream-state)
         (eq (token-stream-state-status stream-state) :running))))

(defun token-stream-cancel-requested-p (stream-state)
  (%with-token-stream-lock (stream-state)
    (token-stream-state-cancel-requested-p stream-state)))

(defun token-stream-request-cancel (stream-state)
  (%with-token-stream-lock (stream-state)
    (setf (token-stream-state-cancel-requested-p stream-state) t))
  t)

(defun token-stream-check-cancel (stream-state)
  (when (token-stream-cancel-requested-p stream-state)
    (error 'token-stream-cancelled))
  nil)

(defun token-stream-emit-chunk (stream-state chunk)
  (token-stream-check-cancel stream-state)
  (when (and (stringp chunk) (plusp (length chunk)))
    (ptui.runtime.queue:queue-push (token-stream-state-events stream-state)
                                   (list :kind :chunk
                                         :text chunk
                                         :token-count (%token-stream-estimate-token-count chunk))))
  nil)

(defun token-stream-mark-complete (stream-state)
  (ptui.runtime.queue:queue-push (token-stream-state-events stream-state)
                                 (list :kind :complete))
  nil)

(defun token-stream-mark-cancelled (stream-state)
  (ptui.runtime.queue:queue-push (token-stream-state-events stream-state)
                                 (list :kind :cancelled))
  nil)

(defun token-stream-mark-failed (stream-state condition)
  (ptui.runtime.queue:queue-push
   (token-stream-state-events stream-state)
   (list :kind :failed
         :error-message (handler-case
                            (princ-to-string condition)
                          (error ()
                            "Streaming request failed."))))
  nil)

(defun %token-stream-run-worker (stream-state worker-fn)
  (handler-case
      (progn
        (funcall worker-fn stream-state)
        (if (token-stream-cancel-requested-p stream-state)
            (token-stream-mark-cancelled stream-state)
            (token-stream-mark-complete stream-state)))
    (token-stream-cancelled ()
      (token-stream-mark-cancelled stream-state))
    (error (condition)
      (if (token-stream-cancel-requested-p stream-state)
          (token-stream-mark-cancelled stream-state)
          (token-stream-mark-failed stream-state condition)))))

(defun token-stream-start (stream-state worker-fn &key target-message-index)
  (check-type stream-state token-stream-state)
  (check-type worker-fn function)
  (%token-stream-reset! stream-state)
  (%with-token-stream-lock (stream-state)
    (setf (token-stream-state-status stream-state) :running
          (token-stream-state-started-ms stream-state) (%token-stream-now-ms)
          (token-stream-state-target-message-index stream-state) target-message-index))
  #+sb-thread
  (let ((thread
          (sb-thread:make-thread
           (lambda ()
             (%token-stream-run-worker stream-state worker-fn))
           :name "amoebum-token-stream")))
    (%with-token-stream-lock (stream-state)
      (setf (token-stream-state-worker-thread stream-state) thread)))
  #-sb-thread
  (%token-stream-run-worker stream-state worker-fn)
  stream-state)

(defun token-stream-drain-events (stream-state on-event)
  (check-type stream-state token-stream-state)
  (multiple-value-bind (events count)
      (ptui.runtime.queue:queue-pop-all (token-stream-state-events stream-state))
    (dolist (event events)
      (let ((kind (getf event :kind)))
        (case kind
          (:chunk
           (%with-token-stream-lock (stream-state)
             (incf (token-stream-state-token-count stream-state)
                   (or (getf event :token-count) 0))
             (incf (token-stream-state-chunk-count stream-state))))
          (:complete
           (%with-token-stream-lock (stream-state)
             (setf (token-stream-state-status stream-state) :completed
                   (token-stream-state-ended-ms stream-state) (%token-stream-now-ms))))
          (:cancelled
           (%with-token-stream-lock (stream-state)
             (setf (token-stream-state-status stream-state) :cancelled
                   (token-stream-state-ended-ms stream-state) (%token-stream-now-ms))))
          (:failed
           (%with-token-stream-lock (stream-state)
             (setf (token-stream-state-status stream-state) :failed
                   (token-stream-state-ended-ms stream-state) (%token-stream-now-ms)
                   (token-stream-state-error-message stream-state)
                   (getf event :error-message))))))
      (when on-event
        (funcall on-event event)))
    count))

(defun token-stream-elapsed-ms (stream-state)
  (%with-token-stream-lock (stream-state)
    (let ((started (token-stream-state-started-ms stream-state))
          (ended (token-stream-state-ended-ms stream-state)))
      (if (<= started 0)
          0
          (max 0 (- (if (> ended 0)
                        ended
                        (%token-stream-now-ms))
                    started))))))

(defun token-stream-tokens-per-second (stream-state)
  (%with-token-stream-lock (stream-state)
    (let* ((started (token-stream-state-started-ms stream-state))
           (ended (token-stream-state-ended-ms stream-state))
           (elapsed-ms
             (if (<= started 0)
                 0
                 (max 0 (- (if (> ended 0)
                               ended
                               (%token-stream-now-ms))
                           started))))
           (elapsed-seconds (/ (max 1 elapsed-ms) 1000.0d0))
           (tokens (token-stream-state-token-count stream-state)))
      (/ tokens elapsed-seconds))))

(defun token-stream-progress-summary (stream-state)
  (%with-token-stream-lock (stream-state)
    (let* ((status (token-stream-state-status stream-state))
          (tokens (token-stream-state-token-count stream-state))
          (chunks (token-stream-state-chunk-count stream-state))
          (cancel-requested-p (token-stream-state-cancel-requested-p stream-state))
          (error-message (token-stream-state-error-message stream-state))
          (target-index (token-stream-state-target-message-index stream-state))
          (started (token-stream-state-started-ms stream-state))
          (ended (token-stream-state-ended-ms stream-state))
          (elapsed-ms
            (if (<= started 0)
                0
                (max 0 (- (if (> ended 0)
                              ended
                              (%token-stream-now-ms))
                          started))))
          (elapsed-seconds (/ (max 1 elapsed-ms) 1000.0d0)))
      (list :status status
            :activep (eq status :running)
            :tokens tokens
            :chunks chunks
            :target-message-index target-index
            :cancel-requested-p cancel-requested-p
            :elapsed-ms elapsed-ms
            :tokens-per-second (/ tokens elapsed-seconds)
            :error-message error-message))))

(defun stream-pseudopod-chat (stream-state prompt messages
                              &key
                                (system-prompt +chat-stream-default-system-prompt+)
                                client
                                tools)
  (let ((resolved-client (or client (pseudopod:make-client))))
    (pseudopod:stream-chat-completion*
     resolved-client
     prompt
     :system-prompt system-prompt
     :messages messages
     :tools tools
     :on-content (lambda (chunk)
                   (token-stream-emit-chunk stream-state chunk)))))
