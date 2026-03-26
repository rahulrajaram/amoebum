(in-package :pseudopod/test)

(def-suite stream-turn-suite :in pseudopod-suite
  :description "Shared streamed-turn reducer contract.")

(in-suite stream-turn-suite)

(test stream-turn-snapshot-collects-answer-lifecycle
  (let ((snapshot (pseudopod:make-stream-turn-snapshot)))
    (pseudopod:stream-turn-apply-event! snapshot '(:type :role :role "assistant"))
    (pseudopod:stream-turn-apply-event! snapshot '(:type :text-delta :text "Hello "))
    (pseudopod:stream-turn-apply-event! snapshot '(:type :text-delta :text "world"))
    (pseudopod:stream-turn-apply-event! snapshot
                                        '(:type :usage-delta
                                          :usage #.(let ((h (make-hash-table :test #'equal)))
                                                     (setf (gethash "total_tokens" h) 12)
                                                     h)))
    (pseudopod:stream-turn-apply-event! snapshot '(:type :answer-finalized))
    (pseudopod:stream-turn-apply-event! snapshot
                                        '(:type :done
                                          :stream-id "stream-1"
                                          :status :completed
                                          :parse-error-count 0))
    (is (string= "assistant" (pseudopod:stream-turn-snapshot-role snapshot)))
    (is (string= "Hello world" (pseudopod:stream-turn-snapshot-content snapshot)))
    (is (eq :completed (pseudopod:stream-turn-snapshot-status snapshot)))
    (is (string= "stream-1"
                 (or (pseudopod:stream-turn-snapshot-stream-id snapshot) "")))
    (is (eq :answer
            (pseudopod:stream-turn-snapshot-terminal-outcome snapshot)))))

(test stream-turn-snapshot-merges-tool-call-partials
  (let ((snapshot (pseudopod:make-stream-turn-snapshot)))
    (pseudopod:stream-turn-apply-event! snapshot
                                        '(:type :tool-call-delta
                                          :index 0
                                          :tool-call-id "tc-1"
                                          :name "lookup"
                                          :arguments "{\"query\":\"stream"))
    (pseudopod:stream-turn-apply-event! snapshot
                                        '(:type :tool-call-delta
                                          :index 0
                                          :tool-call-id "tc-1"
                                          :arguments "ing\"}"
                                          :arguments-complete-p t))
    (pseudopod:stream-turn-apply-event! snapshot '(:type :done :status :completed))
    (let ((tool-calls (pseudopod:stream-turn-snapshot-tool-calls snapshot)))
      (is (= 1 (length tool-calls)))
      (is (string= "lookup"
                   (or (pseudopod:tool-call-name (first tool-calls)) "")))
      (is (string= "{\"query\":\"streaming\"}"
                   (or (pseudopod:tool-call-arguments (first tool-calls)) "")))
      (is (eq :tool-continuation
              (pseudopod:stream-turn-snapshot-terminal-outcome snapshot))))))

(test stream-turn-reducer-accepts-amoebum-chat-event-shapes
  (let ((snapshot (pseudopod:make-stream-turn-snapshot)))
    (dolist (event '((:kind :text-delta :text "Thinking through it. ")
                     (:kind :assistant-final :text "Done.")
                     (:kind :stream-progress :status :completed)))
      (pseudopod:stream-turn-apply-event! snapshot event))
    (is (string= "Thinking through it. Done."
                 (pseudopod:stream-turn-snapshot-content snapshot)))
    (is (eq :completed
            (pseudopod:stream-turn-snapshot-status snapshot)))
    (is (eq :answer
            (pseudopod:stream-turn-snapshot-terminal-outcome snapshot)))))

(test finalize-stream-turn-snapshot-seals-answer
  (let ((snapshot (pseudopod:make-stream-turn-snapshot)))
    (pseudopod:stream-turn-apply-event! snapshot '(:type :text-delta :text "Hello"))
    (pseudopod:stream-turn-apply-event! snapshot '(:type :text-delta :text " world"))
    (let ((result (pseudopod:finalize-stream-turn-snapshot! snapshot)))
      (is (eq result snapshot))
      (is (eq :completed (pseudopod:stream-turn-snapshot-status snapshot)))
      (is (eq :answer (pseudopod:stream-turn-snapshot-terminal-outcome snapshot))))))

(test stream-turn-snapshot-values-returns-correct-tuple
  (let ((snapshot (pseudopod:make-stream-turn-snapshot)))
    (pseudopod:stream-turn-apply-event! snapshot '(:type :text-delta :text "Reply"))
    (pseudopod:stream-turn-apply-event! snapshot '(:type :reasoning-delta :text "think"))
    (pseudopod:finalize-stream-turn-snapshot! snapshot)
    (multiple-value-bind (role content tool-calls usage)
        (pseudopod:stream-turn-snapshot-values snapshot)
      (is (string= "assistant" role))
      (is (string= "Reply" content))
      (is (null tool-calls))
      (is (null usage)))
    (multiple-value-bind (role content tool-calls usage reasoning)
        (pseudopod:stream-turn-snapshot-values snapshot :include-reasoning-p t)
      (declare (ignore role content tool-calls usage))
      (is (string= "think" reasoning)))))

(test normalize-stream-turn-event-coerces-chat-tool-result-shapes
  (let ((normalized
          (pseudopod:normalize-stream-turn-event
           '(:kind :tool-call-result
             :tool-call-id "tc-2"
             :tool-name "lookup"
             :arguments "{\"query\":\"lisp\"}"
             :execution-error "tool failed"))))
    (is (eq :tool-call-result (getf normalized :type)))
    (is (string= "lookup" (or (getf normalized :name) "")))
    (is (string= "tool failed" (or (getf normalized :error-message) "")))))

(test compute-stream-turn-snapshot-delta-captures-appended-content
  (let ((snapshot (pseudopod:make-stream-turn-snapshot)))
    (pseudopod:stream-turn-apply-event! snapshot '(:type :text-delta :text "Hello"))
    (let ((before-content (pseudopod:stream-turn-snapshot-content snapshot))
          (before-reasoning (pseudopod:stream-turn-snapshot-reasoning-content snapshot))
          (before-status (pseudopod:stream-turn-snapshot-status snapshot)))
      (pseudopod:stream-turn-apply-event! snapshot '(:type :text-delta :text " world"))
      (let ((delta (pseudopod:compute-stream-turn-snapshot-delta
                    before-content before-reasoning before-status snapshot)))
        (is (string= " world" (or (pseudopod:stream-turn-snapshot-delta-content-appended delta) "")))
        (is (null (pseudopod:stream-turn-snapshot-delta-status-changed delta)))))))

(test stream-turn-snapshot-round-trips-through-alist
  (let ((snapshot (pseudopod:make-stream-turn-snapshot)))
    (pseudopod:stream-turn-apply-event! snapshot '(:type :text-delta :text "Hello"))
    (pseudopod:stream-turn-apply-event! snapshot '(:type :reasoning-delta :text "think"))
    (pseudopod:finalize-stream-turn-snapshot! snapshot)
    (let* ((alist (pseudopod:stream-turn-snapshot-to-alist snapshot))
           (restored (pseudopod:stream-turn-snapshot-from-alist alist)))
      (is (string= "Hello" (pseudopod:stream-turn-snapshot-content restored)))
      (is (string= "think" (pseudopod:stream-turn-snapshot-reasoning-content restored)))
      (is (eq :completed (pseudopod:stream-turn-snapshot-status restored)))
      (is (eq :answer (pseudopod:stream-turn-snapshot-terminal-outcome restored))))))

(test parse-sse-data-lines-extracts-payloads
  (let ((payloads '()))
    (with-input-from-string (s (format nil "data: {\"text\":\"hello\"}~%~%data: {\"text\":\"world\"}~%~%data: [DONE]~%"))
      (pseudopod:parse-sse-data-lines s
        :on-payload (lambda (p) (push p payloads))))
    (is (= 2 (length payloads)))
    (is (string= "{\"text\":\"world\"}" (first payloads)))
    (is (string= "{\"text\":\"hello\"}" (second payloads)))))
