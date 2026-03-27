(in-package :pseudopod/test)

;;; ---------------------------------------------------------------------------
;;; Anthropic block-state transition table tests (FP-Refine Phase 1)
;;; ---------------------------------------------------------------------------

(def-suite anthropic-block-state-suite :in pseudopod-suite
  :description "Anthropic block-state transition table and functional update tests.")

(in-suite anthropic-block-state-suite)

;;; --- Type inference from delta type ---

(test delta-type-infers-text
  (is (string= "text" (pseudopod::%anthropic-stream-infer-block-type "text_delta"))))

(test delta-type-infers-thinking
  (is (string= "thinking" (pseudopod::%anthropic-stream-infer-block-type "thinking_delta"))))

(test delta-type-infers-tool-use
  (is (string= "tool_use" (pseudopod::%anthropic-stream-infer-block-type "input_json_delta"))))

(test delta-type-unknown-returns-nil
  (is (null (pseudopod::%anthropic-stream-infer-block-type "unknown_delta")))
  (is (null (pseudopod::%anthropic-stream-infer-block-type ""))))

;;; --- Functional block-state update ---

(test update-block-state-type
  (let* ((old (pseudopod::%make-anthropic-stream-block-state :type nil :id "" :name "" :arguments ""))
         (new (pseudopod::%update-anthropic-block-state old :type "text")))
    (is (string= "text" (pseudopod::anthropic-stream-block-state-type new)))
    (is (null (pseudopod::anthropic-stream-block-state-type old)))))

(test update-block-state-preserves-fields
  (let* ((old (pseudopod::%make-anthropic-stream-block-state
               :type "tool_use" :id "tc-1" :name "search" :arguments "{\"q\":"))
         (new (pseudopod::%update-anthropic-block-state old :arguments-append "\"hi\"}")))
    (is (string= "tool_use" (pseudopod::anthropic-stream-block-state-type new)))
    (is (string= "tc-1" (pseudopod::anthropic-stream-block-state-id new)))
    (is (string= "search" (pseudopod::anthropic-stream-block-state-name new)))
    (is (string= "{\"q\":\"hi\"}" (pseudopod::anthropic-stream-block-state-arguments new)))
    ;; Old is unchanged
    (is (string= "{\"q\":" (pseudopod::anthropic-stream-block-state-arguments old)))))

(test update-block-state-id-and-name
  (let* ((old (pseudopod::%make-anthropic-stream-block-state :type "tool_use"))
         (new (pseudopod::%update-anthropic-block-state old :id "tc-99" :name "read_file")))
    (is (string= "tc-99" (pseudopod::anthropic-stream-block-state-id new)))
    (is (string= "read_file" (pseudopod::anthropic-stream-block-state-name new)))))

;;; --- Text block lifecycle: start → delta × N → stop ---

(test text-block-lifecycle
  (let* ((state (pseudopod::%make-anthropic-stream-state))
         (start-payload (%anthropic-make-table
                         "type" "content_block_start"
                         "index" 0
                         "content_block" (%anthropic-make-table "type" "text"))))
    (pseudopod::%handle-content-block-start state start-payload)
    (let ((block (gethash 0 (pseudopod::anthropic-stream-state-block-states state))))
      (is (pseudopod::anthropic-stream-block-state-p block))
      (is (string= "text" (pseudopod::anthropic-stream-block-state-type block))))
    ;; Two text deltas
    (pseudopod::%handle-content-block-delta state
      (%anthropic-make-table "index" 0
        "delta" (%anthropic-make-table "type" "text_delta" "text" "Hello ")))
    (pseudopod::%handle-content-block-delta state
      (%anthropic-make-table "index" 0
        "delta" (%anthropic-make-table "type" "text_delta" "text" "world")))
    ;; Stop
    (pseudopod::%handle-content-block-stop state
      (%anthropic-make-table "index" 0))
    ;; Block removed from hash after stop (text blocks don't finalize tool calls, so just removed)
    (is (null (gethash 0 (pseudopod::anthropic-stream-state-block-states state))))))

;;; --- Thinking block lifecycle ---

(test thinking-block-lifecycle
  (let ((state (pseudopod::%make-anthropic-stream-state)))
    (pseudopod::%handle-content-block-start state
      (%anthropic-make-table "index" 0
        "content_block" (%anthropic-make-table "type" "thinking")))
    (let ((block (gethash 0 (pseudopod::anthropic-stream-state-block-states state))))
      (is (string= "thinking" (pseudopod::anthropic-stream-block-state-type block))))
    (pseudopod::%handle-content-block-delta state
      (%anthropic-make-table "index" 0
        "delta" (%anthropic-make-table "type" "thinking_delta" "text" "let me think")))
    ;; Thinking text goes to thinking-stream
    (let ((thinking (get-output-stream-string
                     (pseudopod::anthropic-stream-state-thinking-stream state))))
      (is (string= "let me think" thinking)))))

;;; --- Tool_use block lifecycle: start → delta × N → stop ---

(test tool-use-block-lifecycle
  (let ((state (pseudopod::%make-anthropic-stream-state)))
    ;; Start tool_use block
    (pseudopod::%handle-content-block-start state
      (%anthropic-make-table "index" 0
        "content_block" (%anthropic-make-table
                         "type" "tool_use" "id" "tc-1" "name" "search")))
    (let ((block (gethash 0 (pseudopod::anthropic-stream-state-block-states state))))
      (is (string= "tool_use" (pseudopod::anthropic-stream-block-state-type block)))
      (is (string= "tc-1" (pseudopod::anthropic-stream-block-state-id block)))
      (is (string= "search" (pseudopod::anthropic-stream-block-state-name block))))
    ;; Two argument deltas
    (pseudopod::%handle-content-block-delta state
      (%anthropic-make-table "index" 0
        "delta" (%anthropic-make-table "type" "input_json_delta"
                                       "partial_json" "{\"q\":\"")))
    (pseudopod::%handle-content-block-delta state
      (%anthropic-make-table "index" 0
        "delta" (%anthropic-make-table "type" "input_json_delta"
                                       "partial_json" "hello\"}")))
    ;; Arguments accumulated
    (let ((block (gethash 0 (pseudopod::anthropic-stream-state-block-states state))))
      (is (string= "{\"q\":\"hello\"}"
                   (pseudopod::anthropic-stream-block-state-arguments block))))
    ;; Stop finalizes and removes
    (pseudopod::%handle-content-block-stop state
      (%anthropic-make-table "index" 0))
    (is (null (gethash 0 (pseudopod::anthropic-stream-state-block-states state))))))

;;; --- Delta dispatch table coverage ---

(test block-delta-handlers-table-has-three-entries
  (is (= 3 (length pseudopod::+anthropic-block-delta-handlers+))))

(test delta-type-to-block-type-table-has-three-entries
  (is (= 3 (length pseudopod::+anthropic-delta-type-to-block-type+))))

;;; --- Type inference from delta when block type is unknown ---

(test delta-infers-block-type-when-missing
  (let ((state (pseudopod::%make-anthropic-stream-state)))
    ;; Ensure a block exists but with no type
    (setf (gethash 0 (pseudopod::anthropic-stream-state-block-states state))
          (pseudopod::%make-anthropic-stream-block-state :type nil))
    ;; Delta arrives — should infer type from delta-type
    (pseudopod::%handle-content-block-delta state
      (%anthropic-make-table "index" 0
        "delta" (%anthropic-make-table "type" "text_delta" "text" "inferred")))
    (let ((block (gethash 0 (pseudopod::anthropic-stream-state-block-states state))))
      (is (string= "text" (pseudopod::anthropic-stream-block-state-type block))))))

;;; --- Invalid (block-type, delta-type) combinations ---

(test mismatched-block-delta-types-ignored
  (let ((state (pseudopod::%make-anthropic-stream-state)))
    (pseudopod::%handle-content-block-start state
      (%anthropic-make-table "index" 0
        "content_block" (%anthropic-make-table "type" "text")))
    ;; Send a thinking_delta to a text block — should be silently ignored
    (pseudopod::%handle-content-block-delta state
      (%anthropic-make-table "index" 0
        "delta" (%anthropic-make-table "type" "thinking_delta" "text" "should not appear")))
    ;; Snapshot should not contain the thinking text in the text stream
    (let ((thinking (get-output-stream-string
                     (pseudopod::anthropic-stream-state-thinking-stream state))))
      (is (string= "" thinking)))))

;;; --- Tool call argument accumulation across many deltas ---

(test tool-use-argument-accumulation-many-deltas
  (let ((state (pseudopod::%make-anthropic-stream-state)))
    (pseudopod::%handle-content-block-start state
      (%anthropic-make-table "index" 0
        "content_block" (%anthropic-make-table
                         "type" "tool_use" "id" "tc-big" "name" "write")))
    (loop for i from 1 to 10
          do (pseudopod::%handle-content-block-delta state
               (%anthropic-make-table "index" 0
                 "delta" (%anthropic-make-table
                          "type" "input_json_delta"
                          "partial_json" (format nil "chunk~D" i)))))
    (let ((block (gethash 0 (pseudopod::anthropic-stream-state-block-states state))))
      (is (string= "chunk1chunk2chunk3chunk4chunk5chunk6chunk7chunk8chunk9chunk10"
                   (pseudopod::anthropic-stream-block-state-arguments block))))))
