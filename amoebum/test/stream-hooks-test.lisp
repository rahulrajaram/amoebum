(in-package :amoebum/test)

(def-suite stream-hooks-suite
  :description "I221 stream chunk hooks and hook-chain composition."
  :in amoebum-suite)

(in-suite stream-hooks-suite)

(defun %with-cleared-stream-hooks (thunk)
  (let ((existing (amoebum:list-hooks :on-stream-chunk)))
    (unwind-protect
        (progn
          (amoebum:clear-hooks :on-stream-chunk)
          (funcall thunk))
      (amoebum:clear-hooks :on-stream-chunk)
      (dolist (entry existing)
        (amoebum:register-hook
         (amoebum::hook-entry-hook-point entry)
         (amoebum::hook-entry-hook-id entry)
         (amoebum::hook-entry-handler entry)
         :priority (amoebum::hook-entry-priority entry)
         :async (amoebum::hook-entry-async-p entry)
         :max-ms (amoebum::hook-entry-max-ms entry)
         :on-error (amoebum::hook-entry-on-error entry)
         :failure-threshold (amoebum::hook-entry-failure-threshold entry)
         :docstring (amoebum::hook-entry-docstring entry)
         :source-file (amoebum::hook-entry-source-file entry)
         :source-line (amoebum::hook-entry-source-line entry))))))

(test stream-hook-chain-priority-order-and-block
  (%with-cleared-stream-hooks
   (lambda ()
     (let ((order '()))
       (amoebum:register-hook
        :on-stream-chunk
        'stream-hooks-order-low
        (lambda (chunk-text chunk-index total-tokens)
          (declare (ignore chunk-text chunk-index total-tokens))
          (push :low order)
          :ok)
        :priority -10)
       (amoebum:register-hook
        :on-stream-chunk
        'stream-hooks-order-mid
        (lambda (chunk-text chunk-index total-tokens)
          (declare (ignore chunk-text chunk-index total-tokens))
          (push :mid order)
          :block)
        :priority 0)
       (amoebum:register-hook
        :on-stream-chunk
        'stream-hooks-order-high
        (lambda (chunk-text chunk-index total-tokens)
          (declare (ignore chunk-text chunk-index total-tokens))
          (push :high order)
          :ok)
        :priority 20)
       (multiple-value-bind (decision results)
           (amoebum:hook-chain :on-stream-chunk "token" 1 1)
         (is (eq decision :block))
         (is (= (length results) 2))
         (is (equal (nreverse order) '(:low :mid))))))))

(test stream-hook-chain-default-priority-is-zero
  (%with-cleared-stream-hooks
   (lambda ()
     (let ((order '()))
       (amoebum:register-hook
        :on-stream-chunk
        'stream-hooks-priority-neg
        (lambda (chunk-text chunk-index total-tokens)
          (declare (ignore chunk-text chunk-index total-tokens))
          (push :neg order)
          :ok)
        :priority -1)
       (amoebum:register-hook
        :on-stream-chunk
        'stream-hooks-priority-default
        (lambda (chunk-text chunk-index total-tokens)
          (declare (ignore chunk-text chunk-index total-tokens))
          (push :zero order)
          :ok))
       (amoebum:register-hook
        :on-stream-chunk
        'stream-hooks-priority-pos
        (lambda (chunk-text chunk-index total-tokens)
          (declare (ignore chunk-text chunk-index total-tokens))
          (push :pos order)
          :ok)
        :priority 1)
       (multiple-value-bind (decision results)
           (amoebum:hook-chain :on-stream-chunk "chunk" 1 1)
         (is (eq decision :continue))
         (is (= (length results) 3))
         (is (equal (nreverse order) '(:neg :zero :pos))))))))

(test stream-chunk-hook-fires-with-index-and-total-tokens
  (%with-cleared-stream-hooks
   (lambda ()
     (let ((observed '()))
       (amoebum:register-hook
        :on-stream-chunk
        'stream-hooks-observer
        (lambda (chunk-text chunk-index total-tokens)
          (push (list chunk-text chunk-index total-tokens) observed)
          :ok))
       (let ((callback (amoebum::%make-stream-chunk-hook-callback)))
         (funcall callback "one")
         (funcall callback "two three")
         (funcall callback ""))
       (is (= (length observed) 2))
       (is (equal (reverse observed)
                  '(("one" 1 1)
                    ("two three" 2 3))))))))

(test stream-pseudopod-chat-invokes-stream-chunk-callback
  (%with-cleared-stream-hooks
   (lambda ()
     (let ((chunks '())
           (events '())
           (stream-state (amoebum:make-token-stream-state)))
       (let ((original-stream-chat-completion
               (symbol-function 'pseudopod:stream-chat-completion*)))
         (unwind-protect
             (progn
               (setf (symbol-function 'pseudopod:stream-chat-completion*)
                     (lambda (client prompt &key on-content on-reasoning &allow-other-keys)
                       (declare (ignore client prompt))
                       (funcall on-content "alpha")
                       (funcall on-reasoning "beta gamma")
                       nil))
               (let ((amoebum::*stream-chunk-hook-callback*
                       (lambda (chunk)
                         (push chunk chunks)
                         nil)))
                 (amoebum:stream-pseudopod-chat
                  stream-state
                  "prompt"
                  '()
                  :client :fake)))
           (setf (symbol-function 'pseudopod:stream-chat-completion*)
                 original-stream-chat-completion)))
       (amoebum:token-stream-drain-events
        stream-state
        (lambda (event)
          (push event events)))
       (is (equal (reverse chunks) '("alpha" "beta gamma")))
       (is (= (count :text-delta events :key (lambda (event) (getf event :kind)))
              2))))))

(test stream-hooks-smoke-sentinel
  (is-true t)
  (format t "STREAM_HOOKS_SMOKE_OK~%"))
