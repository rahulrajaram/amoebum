(in-package :amoebum/test)

(def-suite event-types-suite
  :description "Typed event struct definitions for Phase 9 I217."
  :in amoebum-suite)

(in-suite event-types-suite)

(test event-type-p-recognizes-i217-keywords
  (is-true (amoebum:event-type-p :tool-started))
  (is-true (amoebum:event-type-p :tool-completed))
  (is-true (amoebum:event-type-p :tool-error))
  (is-true (amoebum:event-type-p :llm-request))
  (is-true (amoebum:event-type-p :llm-response))
  (is-true (amoebum:event-type-p :conversation-step))
  (is-false (amoebum:event-type-p :unknown-event-type)))

(test tool-started-event-constructor-and-slots
  (let ((event (amoebum:make-tool-started-event
                :tool-name "search"
                :arguments '(:q "event types")
                :timestamp 123456789)))
    (is (string= "search" (amoebum:tool-started-event-tool-name event)))
    (is (equal '(:q "event types") (amoebum:tool-started-event-arguments event)))
    (is (= 123456789 (amoebum:tool-started-event-timestamp event)))
    (is (eq :tool-started (amoebum:tool-started-event-event-type event)))))

(test tool-completed-event-constructor-and-slots
  (let ((event (amoebum:make-tool-completed-event-type
                :tool-name "read_file"
                :result "ok"
                :elapsed-ms 44
                :timestamp 100)))
    (is (string= "read_file" (amoebum:tool-completed-event-tool-name event)))
    (is (string= "ok" (amoebum:tool-completed-event-result event)))
    (is (= 44 (amoebum:tool-completed-event-elapsed-ms event)))
    (is (= 100 (amoebum:tool-completed-event-timestamp event)))
    (is (eq :tool-completed (amoebum:tool-completed-event-event-type event)))))

(test tool-error-event-constructor-and-slots
  (let ((event (amoebum:make-tool-error-event-type
                :tool-name "shell"
                :condition "permission denied"
                :restarts '(:retry :abort)
                :timestamp 999)))
    (is (string= "shell" (amoebum:tool-error-event-tool-name event)))
    (is (string= "permission denied" (amoebum:tool-error-event-condition event)))
    (is (equal '(:retry :abort) (amoebum:tool-error-event-restarts event)))
    (is (= 999 (amoebum:tool-error-event-timestamp event)))
    (is (eq :tool-error (amoebum:tool-error-event-event-type event)))))

(test llm-request-event-constructor-and-slots
  (let ((event (amoebum:make-llm-request-event
                :provider :moonshot
                :model "moonshot-v1-8k"
                :message-count 6
                :estimated-tokens 1024
                :timestamp 777)))
    (is (eq :moonshot (amoebum:llm-request-event-provider event)))
    (is (string= "moonshot-v1-8k" (amoebum:llm-request-event-model event)))
    (is (= 6 (amoebum:llm-request-event-message-count event)))
    (is (= 1024 (amoebum:llm-request-event-estimated-tokens event)))
    (is (= 777 (amoebum:llm-request-event-timestamp event)))
    (is (eq :llm-request (amoebum:llm-request-event-event-type event)))))

(test llm-response-event-constructor-and-slots
  (let ((event (amoebum:make-llm-response-event
                :provider :moonshot
                :model "moonshot-v1-8k"
                :usage '(:input 120 :output 30)
                :latency-ms 187
                :timestamp 888)))
    (is (eq :moonshot (amoebum:llm-response-event-provider event)))
    (is (string= "moonshot-v1-8k" (amoebum:llm-response-event-model event)))
    (is (equal '(:input 120 :output 30) (amoebum:llm-response-event-usage event)))
    (is (= 187 (amoebum:llm-response-event-latency-ms event)))
    (is (= 888 (amoebum:llm-response-event-timestamp event)))
    (is (eq :llm-response (amoebum:llm-response-event-event-type event)))))

(test conversation-step-event-constructor-and-slots
  (let ((event (amoebum:make-conversation-step-event
                :step-number 3
                :role "assistant"
                :content-length 241
                :timestamp 111)))
    (is (= 3 (amoebum:conversation-step-event-step-number event)))
    (is (string= "assistant" (amoebum:conversation-step-event-role event)))
    (is (= 241 (amoebum:conversation-step-event-content-length event)))
    (is (= 111 (amoebum:conversation-step-event-timestamp event)))
    (is (eq :conversation-step (amoebum:conversation-step-event-event-type event)))))
