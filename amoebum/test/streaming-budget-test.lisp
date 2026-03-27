(in-package :amoebum/test)

(def-suite streaming-budget-suite
  :description "I226 per-token chunk events and streaming budget enforcement."
  :in amoebum-suite)

(in-suite streaming-budget-suite)

(test streaming-budget-enforcement-emits-token-events-and-stats
  (let* ((bus (amoebum:make-event-bus :capacity 256))
         (chunk-payloads '())
         (original-context-limit (amoebum.config:config-value :context-window-limit
                                                       (amoebum.config:current-config)))
         (original-threshold (amoebum.config:config-value :stream-budget-abort-threshold-percent
                                                   (amoebum.config:current-config))))
    (unwind-protect
        (progn
          (amoebum.config:setconfig :context-window-limit 5)
          (amoebum.config:setconfig :stream-budget-abort-threshold-percent 40)
          (let ((amoebum::*event-bus* bus))
            (amoebum:subscribe bus
                               amoebum:+event-type-llm-stream-chunk+
                               (lambda (event)
                                 (push (amoebum:event-payload event) chunk-payloads)))
            (let* ((runner
                     (lambda (stream-state prompt messages &key system-prompt client tools)
                       (declare (ignore prompt messages system-prompt client tools))
                       (sleep 0.02d0)
                       (amoebum:token-stream-emit-chunk stream-state "one two three four five")
                       (sleep 0.06d0)
                       (amoebum:token-stream-emit-chunk stream-state "six seven")
                       (sleep 0.02d0)))
                   (state
                     (amoebum:ensure-chat-ui-state
                      (amoebum.ui:make-chat-ui-state
                       :stream-runner runner
                       :status-bar-state (amoebum.ui:make-status-bar-state))))
                   (stream-state (amoebum.ui:chat-ui-state-stream-state state)))
              (setf (amoebum.ui:chat-ui-state-context-window-limit state) 5)
              (setf (amoebum.ui:chat-ui-state-context-used-tokens state) 0)
              (amoebum:chat-ui-set-input state "trigger")
              (let ((user-message (amoebum:chat-ui-submit-input state)))
                (amoebum::%start-streaming-assistant-response state user-message))
              (loop repeat 120
                    while (amoebum:token-stream-active-p stream-state) do
                      (sleep 0.01d0)
                      (amoebum::%drain-stream-events state))
              (amoebum::%drain-stream-events state)
              (let* ((summary (amoebum:token-stream-progress-summary stream-state))
                     (stats (amoebum:token-stream-stats stream-state))
                     (ordered (reverse chunk-payloads)))
                (is (eq :cancelled (getf summary :status)))
                (is-true (getf summary :aborted-p))
                (is (eql :budget-exceeded (getf summary :abort-reason)))
                (is (typep stats 'amoebum:stream-stats))
                (is (amoebum:stream-stats-aborted-p stats))
                (is (eql :budget-exceeded (amoebum:stream-stats-abort-reason stats)))
                (is (> (amoebum:stream-stats-tokens-received stats) 0))
                (is (> (amoebum:stream-stats-chunks-processed stats) 0))
                (is (>= (amoebum:stream-stats-elapsed-ms stats) 0))
                (is-true ordered)
                (loop for payload in ordered
                      for expected from 1 do
                        (is (= expected
                               (amoebum:llm-stream-chunk-payload-total-tokens payload))))
                (is (string= "one"
                             (amoebum:llm-stream-chunk-payload-token (first ordered))))
                (is (= 1
                       (amoebum:llm-stream-chunk-payload-token-index (first ordered))))))))
      (amoebum.config:setconfig :context-window-limit original-context-limit)
      (amoebum.config:setconfig :stream-budget-abort-threshold-percent original-threshold))))

(test streaming-budget-smoke-sentinel
  (is-true t)
  (format t "STREAMING_BUDGET_SMOKE_OK~%"))
