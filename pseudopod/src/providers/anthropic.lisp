(in-package :pseudopod)

;;; ---------------------------------------------------------------------------
;;; Anthropic Provider (I94)
;;;
;;; Claude Messages API provider. Handles content blocks, thinking blocks,
;;; and Anthropic-specific response format.
;;; ---------------------------------------------------------------------------

(defclass anthropic-provider (provider)
  ((api-version :initarg :api-version :accessor anthropic-provider-api-version
                :type string :initform "2023-06-01")
   (beta-features :initarg :beta-features :accessor anthropic-provider-beta-features
                  :type list :initform nil))
  (:default-initargs
   :name "anthropic"
   :base-url "https://api.anthropic.com"
   :default-model "claude-sonnet-4-5-20250929")
  (:documentation "Provider for the Anthropic Messages API (Claude)."))

(defun make-anthropic-provider (&key api-key
                                     (base-url "https://api.anthropic.com")
                                     (model "claude-sonnet-4-5-20250929")
                                     (timeout-seconds 180)
                                     (api-version "2023-06-01")
                                     beta-features)
  "Create an anthropic-provider."
  (let ((resolved-key (or api-key
                          (handler-case
                              (let ((env-key (uiop:getenv "ANTHROPIC_API_KEY")))
                                (if (and env-key (plusp (length env-key)))
                                    env-key
                                    ""))
                            (error () "")))))
    (make-instance 'anthropic-provider
                   :name "anthropic"
                   :api-key resolved-key
                   :base-url base-url
                   :default-model model
                   :timeout-seconds timeout-seconds
                   :api-version api-version
                   :beta-features beta-features)))

(defun %anthropic-headers (provider)
  "Build HTTP headers for Anthropic API."
  (let ((headers `((:x-api-key . ,(provider-api-key provider))
                   (:anthropic-version . ,(anthropic-provider-api-version provider))
                   (:content-type . "application/json"))))
    (when (anthropic-provider-beta-features provider)
      (push `(:anthropic-beta . ,(format nil "~{~A~^,~}"
                                          (anthropic-provider-beta-features provider)))
            headers))
    headers))

(defun %anthropic-endpoint (provider path)
  "Build full URL for Anthropic API."
  (format nil "~A~A"
          (string-right-trim "/" (provider-base-url provider))
          path))

(defun %anthropic-non-empty-string-p (value)
  (and (stringp value) (plusp (length value))))

(defun %anthropic-content-text (content)
  (labels ((emit-part (part stream)
             (cond
               ((stringp part)
                (write-string part stream))
               ((hash-table-p part)
                (let* ((type (string-downcase (or (gethash "type" part "") "")))
                       (text (or (and (string= type "text")
                                      (stringp (gethash "text" part))
                                      (gethash "text" part))
                                 (and (stringp (gethash "content" part))
                                      (gethash "content" part))
                                 (and (stringp (gethash "text" part))
                                      (gethash "text" part)))))
                  (when (stringp text)
                    (write-string text stream))))
               (t nil))))
    (with-output-to-string (stream)
      (cond
        ((null content) nil)
        ((stringp content) (write-string content stream))
        ((hash-table-p content) (emit-part content stream))
        ((vectorp content)
         (loop for part across content do (emit-part part stream)))
        ((listp content)
         (dolist (part content) (emit-part part stream)))
        (t (write-string (princ-to-string content) stream))))))

(defun %anthropic-join-strings (parts)
  (with-output-to-string (stream)
    (loop for part in (remove-if-not #'%anthropic-non-empty-string-p parts)
          for index from 0
          do (progn
               (when (plusp index)
                 (write-string (format nil "~%~%") stream))
               (write-string part stream)))))

(defun %anthropic-parse-tool-input (arguments)
  (cond
    ((hash-table-p arguments) arguments)
    ((null arguments) (make-hash-table :test #'equal))
    ((stringp arguments)
     (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline) arguments)))
       (if (zerop (length trimmed))
           (make-hash-table :test #'equal)
           (handler-case
               (let ((parsed (jonathan:parse trimmed :as :hash-table)))
                 (if (hash-table-p parsed)
                     parsed
                     (let ((fallback (make-hash-table :test #'equal)))
                       (setf (gethash "value" fallback) parsed)
                       fallback)))
             (error ()
               (let ((fallback (make-hash-table :test #'equal)))
                 (setf (gethash "raw" fallback) trimmed)
                 fallback))))))
    (t
     (let ((fallback (make-hash-table :test #'equal)))
       (setf (gethash "value" fallback) arguments)
       fallback))))

(defun %anthropic-make-text-block (text)
  (let ((block (make-hash-table :test #'equal)))
    (setf (gethash "type" block) "text")
    (setf (gethash "text" block) (or text ""))
    block))

(defun %anthropic-make-image-block (media-type data)
  (let ((block (make-hash-table :test #'equal))
        (source (make-hash-table :test #'equal)))
    (setf (gethash "type" block) "image"
          (gethash "type" source) "base64"
          (gethash "media_type" source) media-type
          (gethash "data" source) data
          (gethash "source" block) source)
    block))

(defun %anthropic-parse-data-uri (uri)
  (when (and (stringp uri)
             (uiop:string-prefix-p "data:" uri))
    (let* ((separator ";base64,")
           (marker (search separator uri :test #'char=))
           (start (length "data:"))
           (data-start (and marker (+ marker (length separator)))))
      (when (and marker data-start
                 (> marker start)
                 (< data-start (length uri)))
        (values (subseq uri start marker)
                (subseq uri data-start))))))

(defun %anthropic-image-block-from-part (part)
  (let* ((media-type (%trimmed-non-empty-string
                      (or (gethash "media_type" part)
                          (gethash "mime_type" part)
                          (gethash "mime-type" part))))
         (data (%trimmed-non-empty-string (gethash "data" part)))
         (image-url (%openai-image-url-value part)))
    (cond
      ((and media-type data)
       (%anthropic-make-image-block media-type data))
      (image-url
       (multiple-value-bind (data-uri-media data-uri-data)
           (%anthropic-parse-data-uri image-url)
         (when (and (%anthropic-non-empty-string-p data-uri-media)
                    (%anthropic-non-empty-string-p data-uri-data))
           (%anthropic-make-image-block data-uri-media data-uri-data))))
      (t
       nil))))

;;; --- Anthropic Content-Part Coercion Dispatch Table (FP-Refine Phase 2) ---

(defun %anthropic-coerce-text-block-part (hash)
  (%anthropic-make-text-block
   (or (%trimmed-non-empty-string (gethash "text" hash))
       (%trimmed-non-empty-string (gethash "content" hash))
       "")))

(defun %anthropic-coerce-image-block-part (hash)
  (or (%anthropic-image-block-from-part hash)
      (let ((fallback (%trimmed-non-empty-string (gethash "text" hash))))
        (and fallback (%anthropic-make-text-block fallback)))))

(defparameter +anthropic-content-part-coercers+
  '(("text"        . %anthropic-coerce-text-block-part)
    ("image"       . %anthropic-coerce-image-block-part)
    ("input_image" . %anthropic-coerce-image-block-part)
    ("image_url"   . %anthropic-coerce-image-block-part))
  "Dispatch table mapping content-part type strings to Anthropic coercion handlers.
Each handler: (hash-table) -> coerced hash-table | nil.")

(defun %anthropic-coerce-content-part-block (part)
  (let* ((hash (cond
                 ((content-part-p part) (content-part-to-hash part))
                 ((hash-table-p part) (%copy-hash-table part))
                 ((stringp part)
                  (let ((value (make-hash-table :test #'equal)))
                    (setf (gethash "type" value) "text"
                          (gethash "text" value) part)
                    value))
                 (t nil)))
         (type (and (hash-table-p hash)
                    (string-downcase (or (gethash "type" hash) "")))))
    (cond
      ((null hash) nil)
      (t (or (%dispatch-content-part-coercion type +anthropic-content-part-coercers+ hash)
             ;; Fallthrough: unknown type — extract text if available
             (let ((fallback (%trimmed-non-empty-string
                              (or (gethash "text" hash)
                                  (gethash "content" hash)))))
               (and fallback (%anthropic-make-text-block fallback))))))))

(defun %anthropic-coerce-content-blocks (content)
  (let ((parts (cond
                 ((null content) nil)
                 ((or (listp content) (vectorp content))
                  (%sequence->list content))
                 (t
                  (list content)))))
    (remove nil (mapcar #'%anthropic-coerce-content-part-block parts))))

(defun %anthropic-tool-call->block (tool-call)
  (when (hash-table-p tool-call)
    (let* ((function-body (and (hash-table-p (gethash "function" tool-call))
                               (gethash "function" tool-call)))
           (id (or (and (stringp (gethash "id" tool-call))
                        (gethash "id" tool-call))
                   ""))
           (name (or (and (hash-table-p function-body)
                          (stringp (gethash "name" function-body))
                          (gethash "name" function-body))
                     (and (stringp (gethash "name" tool-call))
                          (gethash "name" tool-call))
                     ""))
           (arguments (or (and (hash-table-p function-body)
                               (gethash "arguments" function-body))
                          (gethash "arguments" tool-call)))
           (block (make-hash-table :test #'equal)))
      (setf (gethash "type" block) "tool_use")
      (when (%anthropic-non-empty-string-p id)
        (setf (gethash "id" block) id))
      (when (%anthropic-non-empty-string-p name)
        (setf (gethash "name" block) name))
      (setf (gethash "input" block) (%anthropic-parse-tool-input arguments))
      block)))

(defun %anthropic-coerce-tool-calls (tool-calls)
  (let ((calls (cond
                 ((null tool-calls) '())
                 ((listp tool-calls) tool-calls)
                 ((vectorp tool-calls) (coerce tool-calls 'list))
                 ((hash-table-p tool-calls) (list tool-calls))
                 (t '()))))
    (remove nil (mapcar #'%anthropic-tool-call->block calls))))

(defun %anthropic-tool-result-message (tool-use-id content)
  (let* ((message (make-hash-table :test #'equal))
         (block (make-hash-table :test #'equal)))
    (setf (gethash "type" block) "tool_result")
    (setf (gethash "tool_use_id" block) (or tool-use-id ""))
    (setf (gethash "content" block) (or content ""))
    (setf (gethash "role" message) "user")
    (setf (gethash "content" message) (list block))
    message))

(defun %anthropic-coerce-message (m)
  "Coerce message M into Anthropic message format.
Returns two values:
1. Anthropic message hash-table or NIL (for system-role extraction).
2. System prompt fragment (string) or NIL."
  (let ((raw (cond
               ((hash-table-p m) m)
               ((message-p m) (message-to-hash m))
               ((stringp m)
                (let ((h (make-hash-table :test #'equal)))
                  (setf (gethash "role" h) "user"
                        (gethash "content" h) m)
                  h))
               (t nil))))
    (unless (hash-table-p raw)
      (return-from %anthropic-coerce-message (values nil nil)))
    (let* ((role (string-downcase (or (gethash "role" raw) "user")))
           (content (gethash "content" raw))
           (tool-calls (gethash "tool_calls" raw))
           (tool-call-id (and (stringp (gethash "tool_call_id" raw))
                              (gethash "tool_call_id" raw))))
      (cond
        ((string= role "system")
         (values nil (%anthropic-content-text content)))
        ((string= role "tool")
         (values (%anthropic-tool-result-message tool-call-id
                                                 (%anthropic-content-text content))
                 nil))
        ((or (string= role "user")
             (string= role "assistant"))
         (let ((out (make-hash-table :test #'equal))
               (blocks (%anthropic-coerce-content-blocks content)))
           (when (string= role "assistant")
             (setf blocks (nconc blocks (%anthropic-coerce-tool-calls tool-calls))))
           (setf (gethash "role" out) role)
           (setf (gethash "content" out)
                 (if (null blocks) "" blocks))
           (values out nil)))
        (t
         (let ((fallback (make-hash-table :test #'equal)))
           (setf (gethash "role" fallback) "user")
           (setf (gethash "content" fallback) (%anthropic-content-text content))
           (values fallback nil)))))))

(defun %anthropic-build-payload (provider messages &key model temperature max-tokens
                                                        top-p tools tool-choice
                                                        system-prompt stream-p)
  "Build JSON payload for the Anthropic Messages API."
  (let ((payload (make-hash-table :test #'equal)))
    (setf (gethash "model" payload) (or model (provider-default-model provider)))
    (setf (gethash "max_tokens" payload) (or max-tokens 4096))
    (let ((coerced-messages '())
          (system-fragments '()))
      (dolist (message messages)
        (multiple-value-bind (coerced system-fragment)
            (%anthropic-coerce-message message)
          (when coerced
            (push coerced coerced-messages))
          (when (%anthropic-non-empty-string-p system-fragment)
            (push system-fragment system-fragments))))
      (let ((resolved-system
              (%anthropic-join-strings
               (append (when (%anthropic-non-empty-string-p system-prompt)
                         (list system-prompt))
                       (nreverse system-fragments)))))
        (when (%anthropic-non-empty-string-p resolved-system)
          (setf (gethash "system" payload) resolved-system)))
      (setf (gethash "messages" payload)
            (nreverse coerced-messages)))
    (when temperature
      (setf (gethash "temperature" payload) (coerce temperature 'double-float)))
    (when top-p
      (setf (gethash "top_p" payload) (coerce top-p 'double-float)))
    (when tools
      (setf (gethash "tools" payload)
            (mapcar (lambda (td)
                      (if (tool-definition-p td)
                          (tool-definition-to-hash td)
                          td))
                    tools)))
    (when tool-choice
      (setf (gethash "tool_choice" payload) tool-choice))
    (when stream-p
      (setf (gethash "stream" payload) t))
    (jonathan:to-json payload)))

(defun %anthropic-parse-response (body)
  "Parse an Anthropic Messages API response into a hash-table."
  (cond
    ((stringp body)
     (jonathan:parse body :as :hash-table))
    ((streamp body)
     (let ((text (handler-case (uiop:slurp-stream-string body)
                   (error () nil))))
       (when text (jonathan:parse text :as :hash-table))))
    ((hash-table-p body) body)
    (t nil)))

(defun %anthropic-extract-text (response)
  "Extract text content from Anthropic response content blocks."
  (let* ((content (and (hash-table-p response) (gethash "content" response)))
         (blocks (cond ((listp content) content)
                       ((vectorp content) (coerce content 'list))
                       (t nil))))
    (with-output-to-string (out)
      (dolist (block blocks)
        (when (and (hash-table-p block)
                   (string= "text" (gethash "type" block "")))
          (write-string (or (gethash "text" block) "") out))))))

(defun %anthropic-parse-int (value)
  (cond
    ((integerp value) value)
    ((and (stringp value)
          (plusp (length value))
          (every #'digit-char-p value))
     (ignore-errors (parse-integer value)))
    (t nil)))

(defun %anthropic-normalize-block-type (type)
  (and (stringp type)
       (string-downcase (string-trim '(#\Space #\Tab #\Return #\Newline) type))))

(defun %anthropic-trim-sse-data (line)
  (cond
    ((uiop:string-prefix-p "data: " line) (subseq line 6))
    ((uiop:string-prefix-p "data:" line) (string-left-trim '(#\Space #\Tab) (subseq line 5)))
    (t "")))

(defun %anthropic-normalize-stream-result (role content tool-calls usage &optional thinking)
  (let ((response (make-hash-table :test #'equal)))
    (setf (gethash "role" response) (or role "assistant"))
    (setf (gethash "content" response) (or content ""))
    (setf (gethash "tool_calls" response)
          (coerce (or tool-calls '()) 'vector))
    (when usage
      (setf (gethash "usage" response) usage))
    (when (%anthropic-non-empty-string-p thinking)
      (setf (gethash "thinking" response) thinking))
    response))

(defstruct (anthropic-stream-block-state
             (:constructor %make-anthropic-stream-block-state
                 (&key type (id "") (name "") (arguments ""))))
  type
  (id "" :type string)
  (name "" :type string)
  (arguments "" :type string))

(defstruct (anthropic-stream-state
             (:constructor %make-anthropic-stream-state (&key callback)))
  (thinking-stream (make-string-output-stream))
  (block-states (make-hash-table :test #'eql))
  (snapshot (make-stream-turn-snapshot) :type stream-turn-snapshot)
  callback)

(defun %anthropic-stream-emit-text (state text)
  (when (%anthropic-non-empty-string-p text)
    (stream-turn-apply-event! (anthropic-stream-state-snapshot state)
                              (list :type :text-delta
                                    :text text))
    (when (anthropic-stream-state-callback state)
      (funcall (anthropic-stream-state-callback state) text))))

(defun %anthropic-stream-emit-thinking (state text)
  (when (%anthropic-non-empty-string-p text)
    (write-string text (anthropic-stream-state-thinking-stream state))
    (stream-turn-apply-event! (anthropic-stream-state-snapshot state)
                              (list :type :reasoning-delta
                                    :text text))))

(defun %anthropic-stream-ensure-block-state (state index)
  (or (gethash index (anthropic-stream-state-block-states state))
      (setf (gethash index (anthropic-stream-state-block-states state))
            (%make-anthropic-stream-block-state))))

(defun %anthropic-stream-block-type (block-state)
  (%anthropic-normalize-block-type
   (and (anthropic-stream-block-state-p block-state)
        (anthropic-stream-block-state-type block-state))))

;;; --- Block-state transition tables (FP-Refine Phase 1, Target 3) ---

(defparameter +anthropic-delta-type-to-block-type+
  '(("text_delta"        . "text")
    ("thinking_delta"    . "thinking")
    ("input_json_delta"  . "tool_use"))
  "Data table mapping SSE delta type strings to block type strings.")

(defun %anthropic-stream-infer-block-type (delta-type)
  (cdr (assoc delta-type +anthropic-delta-type-to-block-type+ :test #'string=)))

(defun %update-anthropic-block-state (old-state &key type id name arguments-append)
  "Return a new block-state with updated fields. Pure — no mutation."
  (%make-anthropic-stream-block-state
   :type (or type (anthropic-stream-block-state-type old-state))
   :id (or id (anthropic-stream-block-state-id old-state))
   :name (or name (anthropic-stream-block-state-name old-state))
   :arguments (if arguments-append
                  (concatenate 'string
                    (anthropic-stream-block-state-arguments old-state)
                    arguments-append)
                  (anthropic-stream-block-state-arguments old-state))))

(defun %anthropic-block-handle-text-delta (state block-state delta)
  "Handle text_delta: emit text to snapshot callback. Returns new block-state."
  (declare (ignore block-state))
  (%anthropic-stream-emit-text state
                               (and (hash-table-p delta) (gethash "text" delta)))
  nil)

(defun %anthropic-block-handle-thinking-delta (state block-state delta)
  "Handle thinking_delta: emit thinking text to snapshot. Returns new block-state."
  (declare (ignore block-state))
  (%anthropic-stream-emit-thinking state
                                   (and (hash-table-p delta) (gethash "text" delta)))
  nil)

(defun %anthropic-block-handle-tool-use-delta (state block-state delta)
  "Handle input_json_delta: accumulate arguments. Returns new block-state or NIL."
  (declare (ignore state))
  (let ((partial-json (and (hash-table-p delta) (gethash "partial_json" delta))))
    (when (stringp partial-json)
      (%update-anthropic-block-state block-state :arguments-append partial-json))))

(defparameter +anthropic-block-delta-handlers+
  '(("text"     "text_delta"        . %anthropic-block-handle-text-delta)
    ("thinking" "thinking_delta"    . %anthropic-block-handle-thinking-delta)
    ("tool_use" "input_json_delta"  . %anthropic-block-handle-tool-use-delta))
  "Dispatch table: (block-type delta-type . handler-fn).
Each handler receives (state block-state delta) and returns a new block-state or NIL.")

(defun %anthropic-stream-extract-usage (payload)
  (let ((payload-usage (and (hash-table-p payload) (gethash "usage" payload)))
        (message (and (hash-table-p payload) (gethash "message" payload))))
    (or (and (hash-table-p payload-usage) payload-usage)
        (let ((message-usage (and (hash-table-p message)
                                  (gethash "usage" message))))
          (and (hash-table-p message-usage) message-usage)))))

(defun %anthropic-stream-note-usage (state payload)
  (let ((next-usage (%anthropic-stream-extract-usage payload)))
    (when (hash-table-p next-usage)
      (stream-turn-apply-event! (anthropic-stream-state-snapshot state)
                                (%make-stream-usage-delta-chunk next-usage)))))

(defun %anthropic-stream-finalize-tool-call (state index)
  (let ((block-state (and (integerp index)
                          (gethash index (anthropic-stream-state-block-states state)))))
    (when (anthropic-stream-block-state-p block-state)
	      (when (string= (or (%anthropic-stream-block-type block-state) "") "tool_use")
        (let ((tool-call (make-hash-table :test #'equal))
              (function-body (make-hash-table :test #'equal)))
          (setf (gethash "id" tool-call) (anthropic-stream-block-state-id block-state)
                (gethash "type" tool-call) "function"
                (gethash "name" function-body) (anthropic-stream-block-state-name block-state)
                (gethash "arguments" function-body) (anthropic-stream-block-state-arguments block-state)
                (gethash "function" tool-call) function-body)
          (stream-turn-apply-event! (anthropic-stream-state-snapshot state)
                                    (list :type :tool-call-delta
                                          :index index
                                          :tool-call (hash-to-tool-call tool-call)
                                          :tool-call-id (gethash "id" tool-call)
                                          :name (anthropic-stream-block-state-name block-state)
	                                          :arguments (anthropic-stream-block-state-arguments block-state)
	                                          :arguments-complete-p t))))
	      (remhash index (anthropic-stream-state-block-states state)))))

(defun %handle-message-start (state payload)
  (let* ((message (and (hash-table-p payload) (gethash "message" payload)))
         (message-role (and (hash-table-p message) (gethash "role" message))))
    (when (stringp message-role)
      (stream-turn-apply-event! (anthropic-stream-state-snapshot state)
                                (list :type :role
                                      :role message-role)))
    (%anthropic-stream-note-usage state payload)))

(defun %handle-message-delta (state payload)
  (%anthropic-stream-note-usage state payload))

(defun %handle-message-stop (state payload)
  (%anthropic-stream-note-usage state payload))

(defun %handle-content-block-start (state payload)
  (let* ((index (%anthropic-parse-int (and (hash-table-p payload) (gethash "index" payload))))
         (content-block (and (hash-table-p payload) (gethash "content_block" payload)))
         (block-type (%anthropic-normalize-block-type
                      (and (hash-table-p content-block)
                           (gethash "type" content-block))))
         (block-id (and (hash-table-p content-block) (gethash "id" content-block)))
         (name (and (hash-table-p content-block) (gethash "name" content-block))))
    (when (integerp index)
      (let* ((old-block (%anthropic-stream-ensure-block-state state index))
             (new-block (if (string= (or block-type "") "tool_use")
                            (%update-anthropic-block-state old-block
                              :type block-type
                              :id (or block-id "")
                              :name (or name ""))
                            (%update-anthropic-block-state old-block
                              :type block-type))))
        (setf (gethash index (anthropic-stream-state-block-states state))
              new-block)))))

(defun %handle-content-block-delta (state payload)
  (let* ((index (%anthropic-parse-int (and (hash-table-p payload) (gethash "index" payload))))
         (block-state (and (integerp index)
                           (%anthropic-stream-ensure-block-state state index)))
         (delta (and (hash-table-p payload) (gethash "delta" payload)))
         (delta-type (%anthropic-normalize-block-type
                      (and (hash-table-p delta) (gethash "type" delta))))
         (block-type (or (%anthropic-stream-block-type block-state)
                         (%anthropic-stream-infer-block-type (or delta-type "")))))
    (when (and (anthropic-stream-block-state-p block-state) block-type
               (not (equal block-type (%anthropic-stream-block-type block-state))))
      (setf (gethash index (anthropic-stream-state-block-states state))
            (%update-anthropic-block-state block-state :type block-type))
      (setf block-state (gethash index (anthropic-stream-state-block-states state))))
    (let ((entry (find-if (lambda (e)
                            (and (string= (first e) (or block-type ""))
                                 (string= (second e) (or delta-type ""))))
                          +anthropic-block-delta-handlers+)))
      (when (and entry (anthropic-stream-block-state-p block-state))
        (let ((new-block (funcall (cddr entry) state block-state delta)))
          (when new-block
            (setf (gethash index (anthropic-stream-state-block-states state))
                  new-block)))))))

(defun %handle-content-block-stop (state payload)
  (let ((index (%anthropic-parse-int (and (hash-table-p payload) (gethash "index" payload)))))
    (when (integerp index)
      (%anthropic-stream-finalize-tool-call state index))))

(defparameter +anthropic-stream-event-handlers+
  '(("message_start" . %handle-message-start)
    ("message_delta" . %handle-message-delta)
    ("message_stop" . %handle-message-stop)
    ("content_block_start" . %handle-content-block-start)
    ("content_block_delta" . %handle-content-block-delta)
    ("content_block_stop" . %handle-content-block-stop)))

(defun %anthropic-stream-resolve-event (event-name event-data)
  (or (%anthropic-normalize-block-type event-name)
      (%anthropic-normalize-block-type
       (and (hash-table-p event-data)
            (gethash "type" event-data)))))

(defun %anthropic-stream-dispatch-event (state event-name event-data)
  (let* ((event-key (%anthropic-stream-resolve-event event-name event-data))
         (handler-name (cdr (assoc event-key +anthropic-stream-event-handlers+
                                   :test #'string=))))
    (when handler-name
      (funcall (symbol-function handler-name) state event-data))
    (%anthropic-stream-note-usage state event-data)))

(defun %anthropic-stream-consume-event (state event-name payload)
  (handler-case
      (let ((event-data (jonathan:parse payload :as :hash-table :junk-allowed t)))
        (%anthropic-stream-dispatch-event state event-name event-data))
    (error ()
      nil)))

(defun %anthropic-handle-sse-line (state line current-event)
  (let ((trimmed (string-right-trim '(#\Return #\Newline) line)))
    (cond
      ((string= trimmed "")
       nil)
      ((uiop:string-prefix-p "event:" trimmed)
       (%anthropic-normalize-block-type
        (if (>= (length trimmed) 6)
            (string-trim '(#\Space #\Tab) (subseq trimmed 6))
            "")))
      ((uiop:string-prefix-p "data:" trimmed)
       (let ((payload (%anthropic-trim-sse-data trimmed)))
         (unless (or (string= payload "")
                     (string= payload "[DONE]"))
           (%anthropic-stream-consume-event state current-event payload))
         current-event))
      (t
       current-event))))

(defun %anthropic-collect-stream (body-stream callback)
  (let ((state (%make-anthropic-stream-state :callback callback))
        (current-event nil))
    (loop for line = (read-line body-stream nil nil)
          while line
          do (setf current-event (%anthropic-handle-sse-line state line current-event)))
    (let ((snapshot (finalize-stream-turn-snapshot!
                     (anthropic-stream-state-snapshot state))))
      (multiple-value-bind (role content tool-calls usage reasoning)
          (stream-turn-snapshot-values snapshot :include-reasoning-p t)
        (values role content tool-calls usage reasoning snapshot)))))

(defmethod send-chat-completion ((provider anthropic-provider) messages
                                   &key model temperature max-tokens top-p
                                         tools tool-choice system-prompt extra-params)
  (declare (ignore extra-params))
  (%provider-timed-call provider
    (lambda ()
      (let* ((payload (%anthropic-build-payload provider messages
                                                 :model model
                                                 :temperature temperature
                                                 :max-tokens max-tokens
                                                 :top-p top-p
                                                 :tools tools
                                                 :tool-choice tool-choice
                                                 :system-prompt system-prompt))
             (url (%anthropic-endpoint provider "/v1/messages"))
             (headers (%anthropic-headers provider)))
        (multiple-value-bind (body status)
            (dex:request url
                         :method :post
                         :headers headers
                         :content payload
                         :read-timeout (provider-timeout-seconds provider)
                         :connect-timeout 30)
          (unless (<= 200 status 299)
            (error 'pseudopod-api-error
                   :message (format nil "Anthropic API error (status ~A)" status)
                   :status-code status
                   :body (if (stringp body) body (princ-to-string body))))
          (%anthropic-parse-response body))))))

(defmethod send-streaming-completion ((provider anthropic-provider) messages callback
                                      &key model temperature max-tokens top-p
                                           tools tool-choice system-prompt extra-params)
  (declare (ignore extra-params))
  (%provider-timed-call provider
    (lambda ()
      (let* ((payload (%anthropic-build-payload provider messages
                                                 :model model
                                                 :temperature temperature
                                                 :max-tokens max-tokens
                                                 :top-p top-p
                                                 :tools tools
                                                 :tool-choice tool-choice
                                                 :system-prompt system-prompt
                                                 :stream-p t))
             (url (%anthropic-endpoint provider "/v1/messages"))
             (headers (%anthropic-headers provider))
             (result nil))
        (multiple-value-bind (body-stream status)
            (dex:request url
                         :method :post
                         :headers headers
                         :content payload
                         :read-timeout (provider-timeout-seconds provider)
                         :connect-timeout 30
                         :want-stream t)
          (unless (<= 200 status 299)
            (let ((error-body (%coerce-response-body body-stream)))
              (handler-case (close body-stream)
                (error () nil))
              (%signal-http-status-error status error-body :streamp t)))
          (let ((snapshot nil))
            (unwind-protect
                 (multiple-value-bind (parsed-role parsed-content parsed-tool-calls parsed-usage thinking snap)
                     (%anthropic-collect-stream body-stream callback)
                   (setf result
                         (%anthropic-normalize-stream-result parsed-role
                                                           parsed-content
                                                           parsed-tool-calls
                                                           parsed-usage
                                                           thinking)
                         snapshot snap))
              (handler-case (close body-stream)
                (error () nil)))
            (values result snapshot)))))))

(defmethod list-provider-models ((provider anthropic-provider))
  "Anthropic does not have a public models endpoint; return known models."
  (list (%make-model-info :id "claude-opus-4-6"
                          :object "model" :owned-by "anthropic")
        (%make-model-info :id "claude-sonnet-4-5-20250929"
                          :object "model" :owned-by "anthropic")
        (%make-model-info :id "claude-haiku-4-5-20251001"
                          :object "model" :owned-by "anthropic")))

(defmethod estimate-provider-tokens ((provider anthropic-provider) text)
  "Anthropic ~4 chars per token heuristic (cl100k-like tokenizer)."
  (ceiling (length text) 4))
