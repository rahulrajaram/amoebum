(in-package :amoebum/test)

;;; ============================================================
;;; I246: Streaming Conversation Step — Token Stream State
;;; ============================================================

(def-suite streaming-step-suite :in amoebum-suite)
(in-suite streaming-step-suite)

;;; --- Token stream state lifecycle ---

(test token-stream-state-creation
  "make-token-stream-state should create idle state."
  (let ((state (amoebum:make-token-stream-state)))
    (is (amoebum::token-stream-state-p state))
    (is (eq :idle (amoebum::token-stream-state-status state)))
    (is (= 0 (amoebum::token-stream-state-token-count state)))
    (is (= 0 (amoebum::token-stream-state-chunk-count state)))
    (is (not (amoebum:token-stream-active-p state)))))

(test token-stream-reset
  "Reset should clear all state."
  (let ((state (amoebum:make-token-stream-state)))
    (setf (amoebum::token-stream-state-status state) :running)
    (amoebum::%token-stream-reset! state)
    (is (eq :idle (amoebum::token-stream-state-status state)))
    (is (= 0 (amoebum::token-stream-state-token-count state)))))

;;; --- Cancellation ---

(test token-stream-cancel-mechanism
  "Cancellation should set flag and raise condition on check."
  (let ((state (amoebum:make-token-stream-state)))
    (is (not (amoebum:token-stream-cancel-requested-p state)))
    (amoebum:token-stream-request-cancel state)
    (is (amoebum:token-stream-cancel-requested-p state))
    (signals amoebum::token-stream-cancelled
      (amoebum:token-stream-check-cancel state))))

;;; --- Budget warning ---

(test token-stream-budget-warning-emits
  "Budget warning should fire when usage exceeds threshold."
  (let ((state (amoebum:make-token-stream-state)))
    (setf (amoebum::token-stream-state-status state) :running)
    ;; Set threshold to 50%
    (amoebum:token-stream-set-budget-warning-threshold state 50)
    ;; 60% usage should trigger
    (let ((warning (amoebum:token-stream-maybe-budget-warning state 60 100)))
      (is (not (null warning)))
      (is (= 60 (getf warning :usage-percent))))
    ;; Second call should not re-emit (already emitted)
    (let ((warning2 (amoebum:token-stream-maybe-budget-warning state 70 100)))
      (is (null warning2)))))

(test token-stream-budget-warning-below-threshold
  "Budget warning should not fire below threshold."
  (let ((state (amoebum:make-token-stream-state)))
    (setf (amoebum::token-stream-state-status state) :running)
    (amoebum:token-stream-set-budget-warning-threshold state 90)
    (let ((warning (amoebum:token-stream-maybe-budget-warning state 50 100)))
      (is (null warning)))))

;;; --- Abort ---

(test token-stream-abort-sets-flags
  "token-stream-abort should set abort flags."
  (let ((state (amoebum:make-token-stream-state)))
    (amoebum:token-stream-abort state "budget exceeded")
    (is (amoebum::token-stream-state-aborted-p state))
    (is (string= "budget exceeded" (amoebum::token-stream-state-abort-reason state)))
    (is (amoebum:token-stream-cancel-requested-p state))))

;;; --- Budget threshold validation ---

(test token-stream-threshold-validation
  "Budget thresholds must be in [1, 100]."
  (let ((state (amoebum:make-token-stream-state)))
    (signals error (amoebum:token-stream-set-budget-warning-threshold state 0))
    (signals error (amoebum:token-stream-set-budget-warning-threshold state 101))
    (finishes (amoebum:token-stream-set-budget-warning-threshold state 1))
    (finishes (amoebum:token-stream-set-budget-warning-threshold state 100))))

;;; --- Stream stats ---

(test token-stream-stats-returns-struct
  "token-stream-stats should return a stream-stats struct."
  (let ((state (amoebum:make-token-stream-state)))
    (let ((stats (amoebum:token-stream-stats state)))
      (is (amoebum::stream-stats-p stats))
      (is (= 0 (amoebum::stream-stats-tokens-received stats)))
      (is (= 0 (amoebum::stream-stats-chunks-processed stats))))))

;;; --- Progress summary ---

(test token-stream-progress-summary-structure
  "Progress summary should return a plist with expected keys."
  (let ((state (amoebum:make-token-stream-state)))
    (let ((summary (amoebum:token-stream-progress-summary state)))
      (is (listp summary))
      (is (eq :idle (getf summary :status)))
      (is (not (getf summary :activep)))
      (is (integerp (getf summary :tokens)))
      (is (integerp (getf summary :chunks))))))

;;; --- Emit chunk and drain ---

(test token-stream-emit-and-drain
  "Emitting chunks should be retrievable via drain."
  (let ((state (amoebum:make-token-stream-state))
        (events '()))
    (setf (amoebum::token-stream-state-status state) :running)
    (amoebum:token-stream-emit-chunk state "hello ")
    (amoebum:token-stream-emit-chunk state "world")
    (amoebum:token-stream-drain-events state
      (lambda (event) (push event events)))
    (is (= 2 (length events)))
    ;; Token and chunk counts should be updated
    (is (plusp (amoebum::token-stream-state-token-count state)))
    (is (= 2 (amoebum::token-stream-state-chunk-count state)))))

;;; --- Streaming markdown renderer ---

(test streaming-markdown-renderer-basic
  "Streaming markdown renderer should produce styled lines."
  (let ((renderer (amoebum:make-streaming-markdown-renderer)))
    (amoebum:streaming-markdown-renderer-append-chunk renderer "Hello World")
    (let ((lines (amoebum:streaming-markdown-renderer-render-lines renderer 80)))
      (is (listp lines))
      (is (>= (length lines) 1)))))

(test chat-stream-turn-snapshot-tracks-answer-lifecycle
  "The live chat stream snapshot should accumulate normalized streamed-turn events."
  (let* ((state (amoebum:ensure-chat-ui-state
                 (amoebum.ui:make-chat-ui-state :stream-runner nil)))
         (snapshot (amoebum::chat-ui-state-stream-turn-snapshot state)))
    (amoebum::%record-chat-stream-event! state '(:kind :text-delta :text "hello "))
    (amoebum::%record-chat-stream-event! state '(:kind :text-delta :text "world"))
    (amoebum::%record-chat-stream-event! state '(:kind :answer-finalized))
    (amoebum::%record-chat-stream-event! state '(:kind :stream-progress :status :completed))
    (is (string= "hello world"
                 (pseudopod:stream-turn-snapshot-content snapshot)))
    (is (eq :completed
            (pseudopod:stream-turn-snapshot-status snapshot)))
    (is (eq :answer
            (pseudopod:stream-turn-snapshot-terminal-outcome snapshot)))))

(test chat-stream-turn-snapshot-tracks-tool-continuation-lifecycle
  "Tool-call previews should register as tool continuation signals in the shared snapshot."
  (let* ((state (amoebum:ensure-chat-ui-state
                 (amoebum.ui:make-chat-ui-state :stream-runner nil)))
         (snapshot (amoebum::chat-ui-state-stream-turn-snapshot state))
         (tool-call (pseudopod:make-tool-call
                     :id "tc-1"
                     :name "lookup"
                     :arguments "{\"query\":\"lisp\"}")))
    (amoebum::%record-chat-stream-event! state
                                         (list :kind :tool-call-delta
                                               :index 0
                                               :tool-call tool-call
                                               :tool-call-id "tc-1"
                                               :tool-name "lookup"
                                               :arguments "{\"query\":\"lisp\"}"
                                               :arguments-complete-p t))
    (amoebum::%record-chat-stream-event! state '(:kind :stream-progress :status :completed))
    (is (= 1 (length (pseudopod:stream-turn-snapshot-tool-calls snapshot))))
    (is (eq :tool-continuation
            (pseudopod:stream-turn-snapshot-terminal-outcome snapshot)))))

(test chat-stream-terminal-outcome-phase-classification
  "Terminal outcome phase selection should stay explicit across retry, continuation, limit, and answer."
  (let ((state (amoebum:ensure-chat-ui-state
                (amoebum.ui:make-chat-ui-state :stream-runner nil))))
    (flet ((classify (tool-call-entries malformed-names)
             (amoebum::%stream-terminal-outcome-kind
              (amoebum::%stream-terminal-phase-context
               state
               :conversation
               :assistant-response
               tool-call-entries
               malformed-names))))
      (setf (amoebum::chat-ui-state-agentic-iteration-count state) 0)
      (is (eq :retry (classify '((:name "tool")) '("broken-tool"))))
      (is (eq :tool-continuation (classify '((:name "tool")) nil)))
      (setf (amoebum::chat-ui-state-agentic-iteration-count state)
            (amoebum::%chat-effective-max-iterations state))
      (is (eq :max-iterations (classify '((:name "tool")) nil)))
      (is (eq :answer (classify nil nil))))))

(test chat-stream-terminal-finalization-shares-focus-reset
  "Cancellation and failure finalization should both restore input focus and clear stream tracking."
  (let* ((state (amoebum:ensure-chat-ui-state
                 (amoebum.ui:make-chat-ui-state :stream-runner nil)))
         (runtime (ptui.ui.runtime:make-runtime))
         (conversation (amoebum::%ensure-chat-conversation-state state)))
    (setf (amoebum::chat-ui-state-runtime state) runtime
          (ptui.ui.runtime:runtime-focus-id runtime) :overlay
          (amoebum::chat-ui-state-stream-completion-pending-p state) t)
    (setf (gethash "id:tc-1" (amoebum::chat-ui-state-stream-tool-calls state))
          (list :key "id:tc-1" :executed-p t))
    (amoebum::%finalize-streaming-terminal! state conversation
                                            :partialp t
                                            :next-state :idle)
    (is (eq :chat-input (ptui.ui.runtime:runtime-focus-id runtime)))
    (is (eq :idle (amoebum::conversation-state-state conversation)))
    (is (zerop (hash-table-count (amoebum::chat-ui-state-stream-tool-calls state))))
    (setf (ptui.ui.runtime:runtime-focus-id runtime) :overlay)
    (setf (amoebum::token-stream-state-error-message
           (amoebum::chat-ui-state-stream-state state))
          "Provider timeout while streaming.")
    (amoebum::%finalize-streaming-terminal-failure! state conversation)
    (is (eq :chat-input (ptui.ui.runtime:runtime-focus-id runtime)))
    (is (eq :error-recovery (amoebum::conversation-state-state conversation)))
    (is (search "Provider timeout while streaming."
                (amoebum::%message-content->text
                 (car (last (amoebum::chat-ui-state-messages state))))
                :test #'char-equal))))

;;; --- NXT-126: Token stream emitters use canonical :type key ---

(test token-stream-emitters-use-type-key
  "Emitted events should carry :type, not :kind."
  (let ((state (amoebum:make-token-stream-state)))
    (setf (amoebum::token-stream-state-status state) :running)
    (amoebum:token-stream-emit-chunk state "hello")
    (let ((events '()))
      (ptui.runtime.queue:queue-pop-all (amoebum::token-stream-state-events state)
                                        )
      ;; Re-emit after clearing stale pop
      (amoebum:token-stream-emit-chunk state "world")
      (multiple-value-bind (popped count)
          (ptui.runtime.queue:queue-pop-all (amoebum::token-stream-state-events state))
        (declare (ignore count))
        (setf events popped))
      (let ((event (first events)))
        (is (eq :text-delta (getf event :type)))
        (is (null (getf event :kind)))
        (is (string= "world" (getf event :text)))))))

(test token-stream-drain-dispatches-on-type-key
  "Drain should update counts from :type :text-delta events."
  (let ((state (amoebum:make-token-stream-state))
        (dispatched '()))
    (setf (amoebum::token-stream-state-status state) :running)
    (amoebum:token-stream-emit-chunk state "hello ")
    (amoebum:token-stream-emit-chunk state "world")
    (amoebum:token-stream-drain-events state
      (lambda (event) (push event dispatched)))
    (is (= 2 (length dispatched)))
    (is (= 2 (amoebum::token-stream-state-chunk-count state)))))

;;; --- NXT-129: Stream event journal ---

(test stream-event-journal-append-and-count
  "Journal should track events and count correctly."
  (let ((journal (amoebum:make-stream-event-journal :capacity 100)))
    (amoebum:stream-event-journal-append! journal '(:type :text-delta :text "hello"))
    (amoebum:stream-event-journal-append! journal '(:type :text-delta :text "world"))
    (is (= 2 (amoebum:stream-event-journal-count journal)))
    (amoebum:stream-event-journal-clear! journal)
    (is (= 0 (amoebum:stream-event-journal-count journal)))))

(test stream-event-journal-drops-oldest-on-overflow
  "Journal should drop oldest quarter when capacity exceeded."
  (let ((journal (amoebum:make-stream-event-journal :capacity 8)))
    (dotimes (i 9)
      (amoebum:stream-event-journal-append! journal (list :type :text-delta :text (format nil "~D" i))))
    (is (<= (amoebum:stream-event-journal-count journal) 8))))

(test chat-stream-event-journal-captures-live-stream-events
  "The live chat reducer should also append normalized stream journal entries."
  (let* ((state (amoebum:ensure-chat-ui-state
                 (amoebum.ui:make-chat-ui-state :stream-runner nil)))
         (journal (amoebum:chat-ui-state-stream-event-journal state)))
    (amoebum::%record-chat-stream-event! state '(:kind :text-delta :text "hello"))
    (amoebum::%record-chat-stream-event! state '(:type :answer-finalized))
    (let ((entries (amoebum:stream-event-journal-entries-list journal)))
      (is (= 2 (length entries)))
      (is (equal '(:stream-event :stream-event)
                 (mapcar (lambda (entry) (getf entry :kind)) entries)))
      (is (eq :text-delta (getf (first entries) :event-type)))
      (is (eq :answer-finalized (getf (second entries) :event-type))))))

(test stream-event-journal-combines-stream-events-and-policy-traces
  "A single journal should preserve both stream and policy events in append order."
  (let* ((state (amoebum:ensure-chat-ui-state
                 (amoebum.ui:make-chat-ui-state :stream-runner nil)))
         (journal (amoebum:chat-ui-state-stream-event-journal state))
         (trace (list (amoebum:make-policy-trace-entry
                       :phase :input
                       :source :permission-check
                       :data '(:tool-name "read-file")
                       :timestamp 1000)
                      (amoebum:make-policy-trace-entry
                       :phase :evaluate
                       :source :rule-match
                       :decision :allow
                       :reason-code :explicit-allow
                       :reason "matched allow rule"
                       :timestamp 1001))))
    (amoebum::%record-chat-stream-event! state '(:kind :text-delta :text "hello"))
    (amoebum:stream-event-journal-append-policy-trace! journal trace)
    (let ((entries (amoebum:stream-event-journal-entries-list journal)))
      (is (= 3 (length entries)))
      (is (equal '(:stream-event :policy-trace :policy-trace)
                 (mapcar (lambda (entry) (getf entry :kind)) entries)))
      (is (eq :input (getf (second entries) :phase)))
      (is (eq :evaluate (getf (third entries) :phase)))
      (is (eq :allow (getf (third entries) :decision))))))

(test clear-stream-tool-tracking-clears-unified-event-journal
  "Resetting per-turn stream tracking should clear the shared journal too."
  (let* ((state (amoebum:ensure-chat-ui-state
                 (amoebum.ui:make-chat-ui-state :stream-runner nil)))
         (journal (amoebum:chat-ui-state-stream-event-journal state)))
    (amoebum::%record-chat-stream-event! state '(:kind :text-delta :text "hello"))
    (amoebum:stream-event-journal-append-policy-trace!
     journal
     (list (amoebum:make-policy-trace-entry
            :phase :materialize
            :source :final
            :decision :allow
            :timestamp 1002)))
    (is (= 2 (amoebum:stream-event-journal-count journal)))
    (amoebum::%clear-stream-tool-tracking! state)
    (is (= 0 (amoebum:stream-event-journal-count journal)))))

(test stream-tool-call-result-uses-preview-key-to-complete-pending-entry
  "Tool results should settle the preview entry selected during execution."
  (let* ((chat-state (amoebum.ui:make-chat-ui-state
                      :stream-runner nil
                      :stream-client (pseudopod:make-client :api-key "stub")))
         (preview-key "preview:glob-files:0")
         (table (amoebum.ui:chat-ui-state-stream-tool-calls chat-state))
         (entry (list :key preview-key
                      :tool-name "glob-files"
                      :tool-call-id "glob-files:0"
                      :arguments "{\"pattern\":\"*\"}"
                      :executed-p t
                      :completed-p nil)))
    (setf (gethash preview-key table) entry)
    (amoebum::%set-tool-call-result!
     chat-state
     (list :kind :tool-call-result
           :tool-call (pseudopod:make-tool-call
                       :id "glob-files:0"
                       :name "glob-files"
                       :arguments "{}")
           :preview-key preview-key
           :result "{\"count\":3}"))
    (let ((stored (gethash preview-key table)))
      (is (getf stored :completed-p))
      (is (string= "{\"count\":3}" (or (getf stored :result) ""))))
    (is-false (amoebum::%stream-tool-call-completion-pending-p chat-state))))

(test stream-tool-call-transition-events-publish-once
  "Started/argument-complete signals should publish once even if the provider duplicates them."
  (let* ((bus (amoebum:make-event-bus :capacity 32))
         (stream-state (amoebum:make-token-stream-state))
         (chat-state (amoebum.ui:make-chat-ui-state
                      :stream-runner nil
                      :stream-client (pseudopod:make-client :api-key "stub")
                      :stream-state stream-state
                      :status-bar-state (amoebum.ui:make-status-bar-state
                                         :event-bus bus
                                         :model-name "stub-model"
                                         :branch-name "master")))
         (tool-call (pseudopod:make-tool-call
                     :id "glob-files:0"
                     :name "glob-files"
                     :arguments "{\"pattern\":\"*\"}"))
         (started-events 0)
         (argument-events 0)
         (execute-calls 0)
         (original-execute-fn (symbol-function 'amoebum::%execute-stream-tool-call!)))
    (unwind-protect
        (progn
          (amoebum:subscribe bus
                             amoebum:+event-type-tool-call-started+
                             (lambda (_event)
                               (declare (ignore _event))
                               (incf started-events)))
          (amoebum:subscribe bus
                             amoebum:+event-type-tool-call-argument-complete+
                             (lambda (_event)
                               (declare (ignore _event))
                               (incf argument-events)))
          (setf (symbol-function 'amoebum::%execute-stream-tool-call!)
                (lambda (_chat-state _event)
                  (declare (ignore _chat-state _event))
                  (incf execute-calls)
                  nil))
          (amoebum:token-stream-emit-tool-call-started stream-state tool-call)
          (amoebum:token-stream-emit-tool-call-started stream-state tool-call)
          (amoebum:token-stream-emit-tool-call-argument-complete stream-state tool-call)
          (amoebum:token-stream-emit-tool-call-argument-complete stream-state tool-call)
          (amoebum::%drain-stream-events chat-state)
          (is (= started-events 1))
          (is (= argument-events 1))
          (is (= execute-calls 1)))
      (setf (symbol-function 'amoebum::%execute-stream-tool-call!) original-execute-fn))))


;;; --- NXT-143: token-stream-state snapshot slot ---

(test token-stream-state-carries-snapshot-slot
  "Token stream state should have a stream-turn-snapshot slot."
  (let ((state (amoebum:make-token-stream-state))
        (snapshot (pseudopod:make-stream-turn-snapshot)))
    (setf (amoebum::token-stream-state-stream-turn-snapshot state) snapshot)
    (is (eq snapshot (amoebum::token-stream-state-stream-turn-snapshot state)))
    (amoebum::%token-stream-reset! state)
    (is (null (amoebum::token-stream-state-stream-turn-snapshot state)))))

;;; ----------------------------------------------------------------------------
;;; NXT-400: lock the provider-runtime surface inside STREAMING-STEP-SUITE so
;;; the headless-streaming-regression.sh per-submodule coverage gate can map
;;; provider-runtime -> STREAMING-STEP-SUITE without relying on the
;;; stream-hooks suite. Exercises `stream-pseudopod-chat` against a stubbed
;;; `pseudopod:stream-chat-completion*`, validating that provider-runtime
;;; orchestration funnels chunks into the token-stream state.
;;; ----------------------------------------------------------------------------

(def-suite streaming-provider-runtime-suite :in amoebum-suite)
(in-suite streaming-provider-runtime-suite)

(test streaming-step-provider-runtime-coverage-stream-pseudopod-chat
  "STREAMING-PROVIDER-RUNTIME-SUITE must exercise the provider-runtime stream-pseudopod-chat surface."
  (let ((stream-state (amoebum:make-token-stream-state))
        (events '()))
    (let ((original-stream-chat-completion
            (symbol-function 'pseudopod:stream-chat-completion*)))
      (unwind-protect
          (progn
            (setf (symbol-function 'pseudopod:stream-chat-completion*)
                  (lambda (client prompt &key on-content on-reasoning &allow-other-keys)
                    (declare (ignore client prompt on-reasoning))
                    (funcall on-content "alpha ")
                    (funcall on-content "beta")
                    nil))
            (amoebum.ui:stream-pseudopod-chat
             stream-state
             "prompt"
             '()
             :client (pseudopod::%make-client :api-key "test-key")))
        (setf (symbol-function 'pseudopod:stream-chat-completion*)
              original-stream-chat-completion)))
    (amoebum:token-stream-drain-events
     stream-state
     (lambda (event) (push event events)))
    ;; Provider-runtime must funnel chunks into the token-stream as :text-delta
    ;; events and bump the chunk count via token-stream-emit-chunk.
    (is (>= (length events) 2))
    (is (every (lambda (event)
                 (or (eq :text-delta (getf event :type))
                     (eq :text-delta (getf event :kind))))
               events))
    (is (>= (amoebum::token-stream-state-chunk-count stream-state) 2))))

(in-suite streaming-step-suite)

(test stream-cancel-recovery-restores-input-focus-and-status
  "Cancelling a live stream should return control to the chat input and publish a cancelled status."
  (let* ((checkpoint-dir (%make-temp-directory "stream-cancel-checkpoints"))
         (old-root-checkpoint-override amoebum::*checkpoint-directory-override*)
         (old-checkpoint-override amoebum.sessions:*checkpoint-directory-override*)
         (*default-pathname-defaults*
           (pathname "/home/rahul/Documents/amoebum/"))
         (amoebum::*current-config* nil))
    (unwind-protect
        (progn
          (setf amoebum::*checkpoint-directory-override* checkpoint-dir
                amoebum.sessions:*checkpoint-directory-override* checkpoint-dir)
          (let* ((state (%safe-make-chat-ui-state :branch-name "test/stream-cancel"))
                 (runtime (amoebum::chat-ui-state-runtime state))
                 (stream-state (amoebum::chat-ui-state-stream-state state))
                 (status-state (amoebum::chat-ui-state-status-bar-state state)))
            (amoebum:chat-ui-add-message state "user" "cancel this stream")
            (amoebum:chat-ui-add-message state "assistant" "" :partial t)
            (amoebum::%apply-token-stream-updates!
             stream-state
             (list :status :running
                   :started-ms (ptui.util.time:monotonic-ms)
                   :ended-ms 0
                   :token-count 0
                   :chunk-count 0
                   :target-message-index 1
                   :cancel-requested-p nil
                   :error-message nil
                   :budget-warning-emitted-p nil
                   :aborted-p nil
                   :abort-reason nil))
            (amoebum:token-stream-emit-chunk stream-state "partial answer")
            (amoebum:token-stream-request-cancel stream-state)
            (amoebum:token-stream-mark-cancelled stream-state)
            (amoebum::%drain-stream-events state)
            (amoebum::%publish-status-bar-stream-summary-if-needed state)
            (is-false (amoebum:token-stream-active-p stream-state))
            (is (eq :cancelled
                    (getf (amoebum:token-stream-progress-summary stream-state) :status)))
            (is-true (amoebum::chat-panel-handle-input-key state :text "x" 48)
                     "Expected text input handling to resume after stream cancellation.")
            (is (string= "x" (amoebum::chat-ui-state-input-text state)))
            (%safe-render-chat-ui state :cols 84 :rows 24)
            (let ((order (ptui.ui.runtime:runtime-focus-order runtime))
                  (focus (ptui.ui.runtime:runtime-focus-id runtime))
                  (status-line (amoebum.ui:status-bar-line status-state)))
              (is-true (member :chat-input order :test #'equal)
                       "Expected :chat-input in focus order after stream cancellation. Got: ~S"
                       order)
              (is (equal :chat-input focus)
                  "Expected focus to stabilize on :chat-input after cancellation. Got: ~S"
                  focus)
              (is (search "stream cancelled" status-line :test #'char-equal)
                  "Expected cancelled status-bar summary after cancellation. Got: ~S"
                  status-line)))))
      (setf amoebum::*checkpoint-directory-override* old-root-checkpoint-override
            amoebum.sessions:*checkpoint-directory-override* old-checkpoint-override)
      (%delete-directory-tree-safe checkpoint-dir)))
