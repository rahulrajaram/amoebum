(in-package :amoebum)

(defparameter +stream-budget-warning-threshold-percent+ 90)
(defparameter +stream-budget-abort-threshold-percent+ 80)

(define-condition token-stream-cancelled (condition)
  ())

(defstruct (stream-stats
            (:constructor make-stream-stats
                (&key
                   (tokens-received 0)
                   (chunks-processed 0)
                   (elapsed-ms 0)
                   (aborted-p nil)
                   (abort-reason nil))))
  (tokens-received 0 :type fixnum)
  (chunks-processed 0 :type fixnum)
  (elapsed-ms 0 :type integer)
  (aborted-p nil :type boolean)
  abort-reason)

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
                   (error-message nil)
                   (budget-warning-threshold-percent
                    +stream-budget-warning-threshold-percent+)
                   (budget-warning-emitted-p nil)
                   (budget-abort-threshold-percent
                    +stream-budget-abort-threshold-percent+)
                   (aborted-p nil)
                   (abort-reason nil))))
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
  (budget-warning-threshold-percent
   +stream-budget-warning-threshold-percent+
   :type integer)
  (budget-warning-emitted-p nil :type boolean)
  (budget-abort-threshold-percent
   +stream-budget-abort-threshold-percent+
   :type integer)
  (aborted-p nil :type boolean)
  abort-reason
  (stream-turn-snapshot nil)
  (worker-thread nil)
  (lock (bordeaux-threads:make-lock "amoebum-token-stream-lock")))

(defmacro %with-token-stream-lock ((stream-state) &body body)
  `(bordeaux-threads:with-lock-held ((token-stream-state-lock ,stream-state))
     ,@body))

(defparameter +stream-stuck-timeout-ms+ 300000
  "Maximum time a stream can be 'running' without progress before considered stuck (5 minutes).")

(defparameter +token-stream-managed-keys+
  '(:status
    :started-ms
    :ended-ms
    :token-count
    :chunk-count
    :target-message-index
    :cancel-requested-p
    :error-message
    :budget-warning-threshold-percent
    :budget-warning-emitted-p
    :budget-abort-threshold-percent
    :aborted-p
    :abort-reason
    :stream-turn-snapshot
    :worker-thread))

(defun %token-stream-now-ms ()
  (ptui.util.time:monotonic-ms))

(defun %token-stream-blank-string-p (value)
  (or (null value)
      (zerop (length value))
      (every (lambda (char)
               (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=))
             value)))

(defun %token-stream-empty-string-p (value)
  (or (null value)
      (zerop (length value))))

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

(defun %token-stream-valid-threshold-percent-p (value)
  (and (integerp value)
       (>= value 1)
       (<= value 100)))

(defun %token-stream-normalize-threshold-percent (value)
  (if (%token-stream-valid-threshold-percent-p value)
      value
      +stream-budget-warning-threshold-percent+))

(defun %token-stream-event-kind (event)
  (cond
    ((keywordp event) event)
    ((listp event) (or (getf event :type) (getf event :event)))
    (t nil)))

(defun %token-stream-event-data (event key &optional default)
  (if (listp event)
      (getf event key default)
      default))

(defun %ts-transition-start (stream-state &key target-message-index
                                               budget-warning-threshold-percent
                                               budget-abort-threshold-percent)
  (declare (ignore stream-state))
  (list :status :running
        :started-ms (%token-stream-now-ms)
        :ended-ms 0
        :token-count 0
        :chunk-count 0
        :target-message-index target-message-index
        :cancel-requested-p nil
        :error-message nil
        :budget-warning-threshold-percent
        (%token-stream-normalize-threshold-percent budget-warning-threshold-percent)
        :budget-abort-threshold-percent
        (%token-stream-normalize-threshold-percent budget-abort-threshold-percent)
        :budget-warning-emitted-p nil
        :aborted-p nil
        :abort-reason nil
        :stream-turn-snapshot nil
        :worker-thread nil))

(defun %ts-transition-complete (stream-state)
  (declare (ignore stream-state))
  (list :status :completed
        :ended-ms (%token-stream-now-ms)))

(defun %ts-transition-cancelled (stream-state)
  (declare (ignore stream-state))
  (list :status :cancelled
        :ended-ms (%token-stream-now-ms)))

(defun %ts-transition-failed (stream-state &key error-message)
  (declare (ignore stream-state))
  (list :status :failed
        :ended-ms (%token-stream-now-ms)
        :error-message error-message))

(defun %ts-transition-timeout (stream-state &key elapsed)
  (declare (ignore stream-state))
  (list :status :error
        :ended-ms (%token-stream-now-ms)
        :error-message (format nil "Stream timed out after ~D ms" elapsed)))

(defun %ts-transition-force-reset (stream-state &key elapsed)
  (declare (ignore stream-state))
  (list :status :idle
        :ended-ms (%token-stream-now-ms)
        :error-message (format nil "Stream force-reset after being stuck for ~D ms" elapsed)
        :cancel-requested-p nil
        :aborted-p nil
        :worker-thread nil))

(defparameter +token-stream-transitions+
  '(;; Normal lifecycle
    ((:idle :start) . (:running %ts-transition-start))
    ((:running :complete) . (:completed %ts-transition-complete))
    ((:running :cancelled) . (:cancelled %ts-transition-cancelled))
    ((:running :failed) . (:failed %ts-transition-failed))
    ((:running :timeout) . (:error %ts-transition-timeout))
    ((:running :force-reset) . (:idle %ts-transition-force-reset))
    ;; Start from terminal states (implicit reset)
    ((:completed :start) . (:running %ts-transition-start))
    ((:cancelled :start) . (:running %ts-transition-start))
    ((:failed :start) . (:running %ts-transition-start))
    ((:error :start) . (:running %ts-transition-start)))
  "Declarative token-stream state machine: ((from-status event) . (to-status handler-fn)).")

(defun %token-stream-state->updates (stream-state)
  (list :status (token-stream-state-status stream-state)
        :started-ms (token-stream-state-started-ms stream-state)
        :ended-ms (token-stream-state-ended-ms stream-state)
        :token-count (token-stream-state-token-count stream-state)
        :chunk-count (token-stream-state-chunk-count stream-state)
        :target-message-index (token-stream-state-target-message-index stream-state)
        :cancel-requested-p (token-stream-state-cancel-requested-p stream-state)
        :error-message (token-stream-state-error-message stream-state)
        :budget-warning-threshold-percent
        (token-stream-state-budget-warning-threshold-percent stream-state)
        :budget-warning-emitted-p
        (token-stream-state-budget-warning-emitted-p stream-state)
        :budget-abort-threshold-percent
        (token-stream-state-budget-abort-threshold-percent stream-state)
        :aborted-p (token-stream-state-aborted-p stream-state)
        :abort-reason (token-stream-state-abort-reason stream-state)
        :stream-turn-snapshot (token-stream-state-stream-turn-snapshot stream-state)
        :worker-thread (token-stream-state-worker-thread stream-state)))

(defun %apply-token-stream-updates!-unlocked (stream-state updates)
  "Apply slot update plist to STREAM-STATE. Caller must hold the lock."
  (loop for (key value) on updates by #'cddr
        do (case key
             (:status (setf (token-stream-state-status stream-state) value))
             (:started-ms (setf (token-stream-state-started-ms stream-state) value))
             (:ended-ms (setf (token-stream-state-ended-ms stream-state) value))
             (:token-count (setf (token-stream-state-token-count stream-state) value))
             (:chunk-count (setf (token-stream-state-chunk-count stream-state) value))
             (:target-message-index
              (setf (token-stream-state-target-message-index stream-state) value))
             (:cancel-requested-p
              (setf (token-stream-state-cancel-requested-p stream-state) value))
             (:error-message
              (setf (token-stream-state-error-message stream-state) value))
             (:budget-warning-threshold-percent
              (setf (token-stream-state-budget-warning-threshold-percent stream-state) value))
             (:budget-abort-threshold-percent
              (setf (token-stream-state-budget-abort-threshold-percent stream-state) value))
             (:budget-warning-emitted-p
              (setf (token-stream-state-budget-warning-emitted-p stream-state) value))
             (:aborted-p (setf (token-stream-state-aborted-p stream-state) value))
             (:abort-reason (setf (token-stream-state-abort-reason stream-state) value))
             (:stream-turn-snapshot
              (setf (token-stream-state-stream-turn-snapshot stream-state) value))
             (:worker-thread
              (setf (token-stream-state-worker-thread stream-state) value))
             (otherwise nil)))
  stream-state)

(defun %apply-token-stream-updates! (stream-state updates)
  "Apply slot update plist to STREAM-STATE under lock. Single mutation point."
  (%with-token-stream-lock (stream-state)
    (%apply-token-stream-updates!-unlocked stream-state updates))
  stream-state)

(defun %token-stream-valid-transition-p (from-status event)
  (not (null (assoc (list from-status event) +token-stream-transitions+ :test #'equal))))

(defun %token-stream-next-lifecycle-updates (stream-state event-kind event)
  (let* ((status (token-stream-state-status stream-state))
         (entry (assoc (list status event-kind) +token-stream-transitions+ :test #'equal)))
    (unless entry
      (return-from %token-stream-next-lifecycle-updates
        (amoebum.fp:make-err
         :value (format nil "Invalid token-stream transition from ~S on event ~S."
                        status
                        event-kind))))
    (handler-case
        (amoebum.fp:make-ok
         :value (apply (symbol-function (second (cdr entry)))
                       stream-state
                       (case event-kind
                         (:start
                          (list :target-message-index
                                (%token-stream-event-data event :target-message-index)
                                :budget-warning-threshold-percent
                                (%token-stream-event-data
                                 event
                                 :budget-warning-threshold-percent
                                 +stream-budget-warning-threshold-percent+)
                                :budget-abort-threshold-percent
                                (%token-stream-event-data
                                 event
                                 :budget-abort-threshold-percent
                                 +stream-budget-abort-threshold-percent+)))
                         (:failed
                          (list :error-message (%token-stream-event-data event :error-message)))
                         ((:timeout :force-reset)
                          (list :elapsed (%token-stream-event-data event :elapsed 0)))
                         (otherwise nil))))
      (error (condition)
        (amoebum.fp:make-err :value (princ-to-string condition))))))

(defun token-stream-transition (stream-state event)
  "Pure reducer returning Result[new-state, error] for a token-stream event."
  (unless (typep stream-state 'token-stream-state)
    (return-from token-stream-transition
      (amoebum.fp:make-err
       :value (format nil "TOKEN-STREAM-TRANSITION expected TOKEN-STREAM-STATE, got ~S."
                      stream-state))))
  (let* ((event-kind (%token-stream-event-kind event))
         (copied-state (copy-token-stream-state stream-state)))
    (unless event-kind
      (return-from token-stream-transition
        (amoebum.fp:make-err
         :value (format nil "TOKEN-STREAM-TRANSITION requires an event kind, got ~S." event))))
    (let ((transition-result
            (case event-kind
              ((:start :complete :cancelled :failed :timeout :force-reset)
               (%token-stream-next-lifecycle-updates copied-state event-kind event))
              (:reset
               (amoebum.fp:make-ok
                :value (list :status :idle
                             :started-ms 0
                             :ended-ms 0
                             :token-count 0
                             :chunk-count 0
                             :target-message-index nil
                             :cancel-requested-p nil
                             :error-message nil
                             :budget-warning-threshold-percent
                             (token-stream-state-budget-warning-threshold-percent copied-state)
                             :budget-warning-emitted-p nil
                             :budget-abort-threshold-percent +stream-budget-abort-threshold-percent+
                             :aborted-p nil
                             :abort-reason nil
                             :stream-turn-snapshot nil
                             :worker-thread nil)))
              (:request-cancel
               (amoebum.fp:make-ok :value (list :cancel-requested-p t)))
              (:set-budget-warning-threshold
               (let ((threshold (%token-stream-event-data event :threshold-percent)))
                 (if (%token-stream-valid-threshold-percent-p threshold)
                     (amoebum.fp:make-ok
                      :value (list :budget-warning-threshold-percent threshold
                                   :budget-warning-emitted-p nil))
                     (amoebum.fp:make-err
                      :value (format nil "THRESHOLD-PERCENT must be an integer in [1, 100], got ~S."
                                     threshold)))))
              (:set-budget-abort-threshold
               (let ((threshold (%token-stream-event-data event :threshold-percent)))
                 (if (%token-stream-valid-threshold-percent-p threshold)
                     (amoebum.fp:make-ok
                      :value (list :budget-abort-threshold-percent threshold))
                     (amoebum.fp:make-err
                      :value (format nil "THRESHOLD-PERCENT must be an integer in [1, 100], got ~S."
                                     threshold)))))
              (:abort
               (amoebum.fp:make-ok
                :value (list :cancel-requested-p t
                             :aborted-p t
                             :abort-reason (%token-stream-event-data event :abort-reason))))
              (:budget-warning-emitted
               (amoebum.fp:make-ok
                :value (append
                        (if (%token-stream-event-data event :threshold-supplied-p)
                            (list :budget-warning-threshold-percent
                                  (%token-stream-normalize-threshold-percent
                                   (%token-stream-event-data event :threshold-percent)))
                            nil)
                        (list :budget-warning-emitted-p t))))
              (:text-delta
               (amoebum.fp:make-ok
                :value (list :token-count (+ (token-stream-state-token-count copied-state)
                                             (%token-stream-event-data event :token-count 0))
                             :chunk-count (1+ (token-stream-state-chunk-count copied-state)))))
              (:set-worker-thread
               (amoebum.fp:make-ok
                :value (list :worker-thread (%token-stream-event-data event :worker-thread))))
              (:set-target-message-index
               (amoebum.fp:make-ok
                :value (list :target-message-index
                             (%token-stream-event-data event :target-message-index))))
              (otherwise
               (amoebum.fp:make-err
                :value (format nil "Unsupported token-stream event ~S." event-kind))))))
      (typecase transition-result
        (amoebum.fp:ok
         (%apply-token-stream-updates!-unlocked copied-state
                                                (amoebum.fp:ok-value transition-result))
         (amoebum.fp:make-ok :value copied-state))
        (amoebum.fp:err transition-result)
        (t
         (amoebum.fp:make-err
          :value (format nil "Expected Result, got ~S." transition-result)))))))

(defun %token-stream-result-state-or-signal (result)
  (typecase result
    (amoebum.fp:ok (amoebum.fp:ok-value result))
    (amoebum.fp:err (error "~A" (amoebum.fp:err-value result)))
    (t (error "Expected Result, got ~S." result))))

(defun %token-stream-updates-from-result (result)
  (%token-stream-state->updates (%token-stream-result-state-or-signal result)))

(defun %compute-token-stream-transition (stream-state event &rest args)
  "Compatibility bridge: compute updates plist from the new pure reducer."
  (%token-stream-updates-from-result
   (apply #'token-stream-transition
          stream-state
          (list (list* :type event args)))))

(defun %token-stream-reset! (stream-state)
  (let* ((result (token-stream-transition stream-state '(:type :reset)))
         (next-state (%token-stream-result-state-or-signal result)))
    (%apply-token-stream-updates! stream-state (%token-stream-state->updates next-state))
    (ptui.runtime.queue:queue-pop-all (token-stream-state-events stream-state))
    stream-state))

(defun token-stream-active-p (stream-state)
  "Return T if the stream is actively running.
Also checks for 'stuck' streams that have been running too long without progress."
  (and (typep stream-state 'token-stream-state)
       (%with-token-stream-lock (stream-state)
         (when (eq (token-stream-state-status stream-state) :running)
           (let* ((now (ptui.util.time:monotonic-ms))
                  (started (token-stream-state-started-ms stream-state))
                  (elapsed (- now started)))
             (if (> elapsed +stream-stuck-timeout-ms+)
                 (progn
                   (%apply-token-stream-updates!-unlocked
                    stream-state
                    (%token-stream-updates-from-result
                     (token-stream-transition
                      stream-state
                      (list :type :timeout :elapsed elapsed))))
                   nil)
                 t))))))

(defun token-stream-cancel-requested-p (stream-state)
  (%with-token-stream-lock (stream-state)
    (token-stream-state-cancel-requested-p stream-state)))

(defun token-stream-request-cancel (stream-state)
  (%apply-token-stream-updates!
   stream-state
   (%token-stream-updates-from-result
    (token-stream-transition stream-state '(:type :request-cancel))))
  t)

(defun token-stream-force-reset-if-stuck (stream-state)
  "Force reset the stream state if it appears stuck.
Returns T if reset was performed, NIL otherwise."
  (check-type stream-state token-stream-state)
  (%with-token-stream-lock (stream-state)
    (when (eq (token-stream-state-status stream-state) :running)
      (let* ((now (ptui.util.time:monotonic-ms))
             (started (token-stream-state-started-ms stream-state))
             (elapsed (if (> now started) (- now started) 0)))
        (when (> elapsed +stream-stuck-timeout-ms+)
          (%apply-token-stream-updates!-unlocked
           stream-state
           (%token-stream-updates-from-result
            (token-stream-transition stream-state
                                     (list :type :force-reset :elapsed elapsed))))
          (ignore-errors
            (ptui.runtime.queue:queue-pop-all (token-stream-state-events stream-state)))
          t)))))

(defun token-stream-check-cancel (stream-state)
  (when (token-stream-cancel-requested-p stream-state)
    (error 'token-stream-cancelled))
  nil)

(defun token-stream-set-budget-warning-threshold (stream-state threshold-percent)
  (check-type stream-state token-stream-state)
  (%apply-token-stream-updates!
   stream-state
   (%token-stream-updates-from-result
    (token-stream-transition
     stream-state
     (list :type :set-budget-warning-threshold
           :threshold-percent threshold-percent))))
  threshold-percent)

(defun token-stream-set-budget-abort-threshold (stream-state threshold-percent)
  (check-type stream-state token-stream-state)
  (%apply-token-stream-updates!
   stream-state
   (%token-stream-updates-from-result
    (token-stream-transition
     stream-state
     (list :type :set-budget-abort-threshold
           :threshold-percent threshold-percent))))
  threshold-percent)

(defun token-stream-abort (stream-state abort-reason)
  (check-type stream-state token-stream-state)
  (%apply-token-stream-updates!
   stream-state
   (%token-stream-updates-from-result
    (token-stream-transition
     stream-state
     (list :type :abort :abort-reason abort-reason))))
  t)

(defun token-stream-set-target-message-index (stream-state target-message-index)
  (check-type stream-state token-stream-state)
  (%apply-token-stream-updates!
   stream-state
   (%token-stream-updates-from-result
    (token-stream-transition
     stream-state
     (list :type :set-target-message-index
           :target-message-index target-message-index))))
  target-message-index)

(defun token-stream-maybe-budget-warning (stream-state used-tokens limit-tokens
                                          &key threshold-percent)
  (check-type stream-state token-stream-state)
  (unless (and (integerp used-tokens) (>= used-tokens 0))
    (error "USED-TOKENS must be a non-negative integer, got ~S." used-tokens))
  (unless (and (integerp limit-tokens) (> limit-tokens 0))
    (error "LIMIT-TOKENS must be a positive integer, got ~S." limit-tokens))
  (let ((warning nil))
    (%with-token-stream-lock (stream-state)
      (let* ((threshold-supplied-p (not (null threshold-percent)))
             (prepared-state
               (if threshold-supplied-p
                   (%token-stream-result-state-or-signal
                    (token-stream-transition
                     stream-state
                     (list :type :set-budget-warning-threshold
                           :threshold-percent threshold-percent)))
                   stream-state))
             (status (token-stream-state-status prepared-state))
             (threshold
               (token-stream-state-budget-warning-threshold-percent prepared-state))
             (emitted-p
               (token-stream-state-budget-warning-emitted-p prepared-state))
             (usage-percent (truncate (/ (* used-tokens 100.0d0)
                                        (max 1 limit-tokens)))))
        (when threshold-supplied-p
          (%apply-token-stream-updates!-unlocked
           stream-state
           (%token-stream-state->updates prepared-state)))
        (when (and (eq status :running)
                   (not emitted-p)
                   (>= usage-percent threshold))
          (%apply-token-stream-updates!-unlocked
           stream-state
           (%token-stream-updates-from-result
            (token-stream-transition
             prepared-state
             (list :type :budget-warning-emitted
                   :threshold-supplied-p threshold-supplied-p
                   :threshold-percent threshold))))
          (setf warning
                (list :used-tokens used-tokens
                      :limit-tokens limit-tokens
                      :usage-percent usage-percent
                      :threshold-percent threshold)))))
    warning))

(defun stream-cursor-visible-p (stream-state
                                &key
                                  (now-ms (%token-stream-now-ms))
                                  (blink-ms +stream-cursor-blink-ms+))
  (check-type stream-state token-stream-state)
  (let* ((safe-blink-ms (max 80 (if (integerp blink-ms)
                                    blink-ms
                                    +stream-cursor-blink-ms+)))
         (cycle (* 2 safe-blink-ms)))
    (%with-token-stream-lock (stream-state)
      (and (eq (token-stream-state-status stream-state) :running)
           (> cycle 0)
           (let* ((started (token-stream-state-started-ms stream-state))
                  (elapsed (max 0 (- (if (and (integerp now-ms) (>= now-ms 0))
                                         now-ms
                                         (%token-stream-now-ms))
                                     started)))
                  (phase (mod elapsed cycle)))
             (< phase safe-blink-ms))))))

(defun token-stream-emit-chunk (stream-state chunk)
  (token-stream-check-cancel stream-state)
  (when (and (stringp chunk) (plusp (length chunk)))
    (ptui.runtime.queue:queue-push (token-stream-state-events stream-state)
                                   (list :type :text-delta
                                         :text chunk
                                         :token-count (%token-stream-estimate-token-count chunk))))
  nil)

(defun token-stream-emit-tool-call-delta (stream-state chunk)
  (token-stream-check-cancel stream-state)
  (let* ((type (and (listp chunk) (getf chunk :type)))
         (tool-call (and (listp chunk) (getf chunk :tool-call)))
         (tool-call-id
           (or (and (pseudopod:tool-call-p tool-call)
                    (pseudopod:tool-call-id tool-call))
               (and (listp chunk) (getf chunk :tool-call-id))
               (and (listp chunk) (getf chunk :id))))
         (tool-name
           (or (and (pseudopod:tool-call-p tool-call)
                    (pseudopod:tool-call-name tool-call))
               (and (listp chunk) (getf chunk :name))))
         (arguments
           (or (and (pseudopod:tool-call-p tool-call)
                    (pseudopod:tool-call-arguments tool-call))
               (and (listp chunk) (getf chunk :arguments))))
         (index (and (listp chunk) (getf chunk :index)))
         (arguments-complete-p
           (and (listp chunk) (not (null (getf chunk :arguments-complete-p))))))
    (when (and (eq type :tool-call-delta)
               (or (and (stringp tool-name) (plusp (length tool-name)))
                   (and (stringp arguments) (plusp (length arguments)))))
      (ptui.runtime.queue:queue-push
       (token-stream-state-events stream-state)
       (list :type :tool-call-delta
             :index index
             :tool-call tool-call
             :tool-call-id tool-call-id
             :tool-name tool-name
             :arguments arguments
             :chunk chunk)))
    (when (and arguments-complete-p
               (or (pseudopod:tool-call-p tool-call)
                   (and (stringp tool-name) (plusp (length tool-name)))))
      (ptui.runtime.queue:queue-push
       (token-stream-state-events stream-state)
       (list :type :tool-call-argument-complete
             :index index
             :tool-call tool-call
             :tool-call-id tool-call-id
             :tool-name tool-name
             :arguments arguments
             :chunk chunk)))
    nil))

(defun token-stream-emit-tool-call-started (stream-state tool-call)
  (token-stream-check-cancel stream-state)
  (when (pseudopod:tool-call-p tool-call)
    (usdt-probe-tool-call :started
                          (pseudopod:tool-call-name tool-call)
                          (pseudopod:tool-call-id tool-call))
    (ptui.runtime.queue:queue-push
     (token-stream-state-events stream-state)
     (list :type :tool-call-started
           :tool-call tool-call
           :tool-name (pseudopod:tool-call-name tool-call)
           :arguments (pseudopod:tool-call-arguments tool-call)
           :tool-call-id (pseudopod:tool-call-id tool-call))))
  nil)

(defun token-stream-emit-tool-call-argument-complete (stream-state tool-call)
  (token-stream-check-cancel stream-state)
  (when (pseudopod:tool-call-p tool-call)
    (usdt-probe-tool-call :argument-complete
                          (pseudopod:tool-call-name tool-call)
                          (pseudopod:tool-call-id tool-call))
    (ptui.runtime.queue:queue-push
     (token-stream-state-events stream-state)
     (list :type :tool-call-argument-complete
           :tool-call tool-call
           :tool-name (pseudopod:tool-call-name tool-call)
           :arguments (pseudopod:tool-call-arguments tool-call)
           :tool-call-id (pseudopod:tool-call-id tool-call))))
  nil)

(defun token-stream-emit-tool-call-result (stream-state
                                           &key
                                             tool-call
                                             preview-key
                                             execution-key
                                             result
                                             execution-error)
  "Publish a completed tool call execution result for async tool workers."
  (when (pseudopod:tool-call-p tool-call)
    (usdt-probe-tool-call :result
                          (pseudopod:tool-call-name tool-call)
                          (pseudopod:tool-call-id tool-call)
                          :status (if execution-error :error :ok))
    (ptui.runtime.queue:queue-push
     (token-stream-state-events stream-state)
     (list :type :tool-call-result
           :tool-call tool-call
           :preview-key preview-key
           :execution-key execution-key
           :result result
           :execution-error execution-error)))
  nil)

(defun token-stream-mark-complete (stream-state)
  (ptui.runtime.queue:queue-push (token-stream-state-events stream-state)
                                 (list :type :complete))
  nil)

(defun token-stream-mark-cancelled (stream-state)
  (ptui.runtime.queue:queue-push (token-stream-state-events stream-state)
                                 (list :type :cancelled))
  nil)

(defun token-stream-mark-failed (stream-state condition)
  (ptui.runtime.queue:queue-push
   (token-stream-state-events stream-state)
   (list :type :failed
         :error-message (handler-case
                            (princ-to-string condition)
                          (error ()
                            "Streaming request failed."))))
  nil)

(defun %token-stream-drain-event-starts-stream-p (kind)
  (member kind
          '(:text-delta
            :chunk
            :reasoning
            :tool-call-delta
            :tool-call-started
            :tool-call-argument-complete
            :tool-call-result)
          :test #'eq))

(defun %token-stream-ensure-running-for-drain-event! (stream-state kind)
  (when (and (%token-stream-drain-event-starts-stream-p kind)
             (eq (token-stream-state-status stream-state) :idle))
    (%apply-token-stream-updates!
     stream-state
     (%compute-token-stream-transition stream-state
                                       :start
                                       :target-message-index nil
                                       :budget-warning-threshold-percent
                                       +stream-budget-warning-threshold-percent+
                                       :budget-abort-threshold-percent
                                       +stream-budget-abort-threshold-percent+))))

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

(defun token-stream-start (stream-state worker-fn
                           &key
                             target-message-index
                             budget-warning-threshold-percent
                             budget-abort-threshold-percent)
  (check-type stream-state token-stream-state)
  (check-type worker-fn function)
  (%token-stream-reset! stream-state)
  (%apply-token-stream-updates!
   stream-state
   (%token-stream-updates-from-result
    (token-stream-transition
     stream-state
     (list :type :start
           :target-message-index target-message-index
           :budget-warning-threshold-percent
           (or budget-warning-threshold-percent
               +stream-budget-warning-threshold-percent+)
           :budget-abort-threshold-percent
           (or budget-abort-threshold-percent
               +stream-budget-abort-threshold-percent+)))))
  #+sb-thread
  (let ((thread
          (sb-thread:make-thread
           (lambda ()
             (%token-stream-run-worker stream-state worker-fn))
           :name "amoebum-token-stream")))
    (%apply-token-stream-updates!
     stream-state
     (%token-stream-updates-from-result
      (token-stream-transition stream-state
                               (list :type :set-worker-thread
                                     :worker-thread thread)))))
  #-sb-thread
  (%token-stream-run-worker stream-state worker-fn)
  stream-state)

(defun token-stream-drain-events (stream-state on-event)
  (check-type stream-state token-stream-state)
  (multiple-value-bind (events count)
      (ptui.runtime.queue:queue-pop-all (token-stream-state-events stream-state))
    (dolist (event events)
      (let ((kind (or (getf event :type) (getf event :kind))))
        (%token-stream-ensure-running-for-drain-event! stream-state kind)
        (case kind
          ((:text-delta :chunk)
           (%apply-token-stream-updates!
            stream-state
            (%token-stream-updates-from-result
             (token-stream-transition
              stream-state
              (list :type :text-delta
                    :token-count (or (getf event :token-count) 0))))))
          (:complete
           (%apply-token-stream-updates!
            stream-state (%compute-token-stream-transition stream-state :complete)))
          (:cancelled
           (%apply-token-stream-updates!
            stream-state (%compute-token-stream-transition stream-state :cancelled)))
          (:failed
           (%apply-token-stream-updates!
            stream-state
            (%compute-token-stream-transition stream-state
                                              :failed
                                              :error-message (getf event :error-message))))))
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
           (budget-warning-threshold
             (token-stream-state-budget-warning-threshold-percent stream-state))
           (budget-warning-emitted-p
             (token-stream-state-budget-warning-emitted-p stream-state))
           (budget-abort-threshold
             (token-stream-state-budget-abort-threshold-percent stream-state))
           (aborted-p (token-stream-state-aborted-p stream-state))
           (abort-reason (token-stream-state-abort-reason stream-state))
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
            :budget-warning-threshold-percent budget-warning-threshold
            :budget-warning-emitted-p budget-warning-emitted-p
            :budget-abort-threshold-percent budget-abort-threshold
            :aborted-p aborted-p
            :abort-reason abort-reason
            :error-message error-message))))

(defun token-stream-stats (stream-state)
  (check-type stream-state token-stream-state)
  (let ((summary (token-stream-progress-summary stream-state)))
    (make-stream-stats
     :tokens-received (or (getf summary :tokens) 0)
     :chunks-processed (or (getf summary :chunks) 0)
     :elapsed-ms (or (getf summary :elapsed-ms) 0)
     :aborted-p (not (null (getf summary :aborted-p)))
     :abort-reason (getf summary :abort-reason))))
