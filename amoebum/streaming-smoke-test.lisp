(let* ((smoke-file (or *load-truename* *compile-file-truename*))
       (amoebum-dir (and smoke-file (make-pathname :name nil :type nil :defaults smoke-file)))
       (repo-root (and amoebum-dir (truename (merge-pathnames #P"../" amoebum-dir)))))
  (unless repo-root
    (error "Unable to resolve repository root from ~S" smoke-file))

  (load (merge-pathnames #P"ptui/.tools/quicklisp/setup.lisp" repo-root))
  (require :asdf)

  (let* ((asdf-pkg (or (find-package "ASDF")
                       (error "Missing package ASDF")))
         (load-asd-sym (or (find-symbol "LOAD-ASD" asdf-pkg)
                           (error "Missing symbol LOAD-ASD in ASDF package")))
         (load-system-sym (or (find-symbol "LOAD-SYSTEM" asdf-pkg)
                              (error "Missing symbol LOAD-SYSTEM in ASDF package")))
         (load-asd-fn (symbol-function load-asd-sym))
         (load-system-fn (symbol-function load-system-sym)))
    (funcall load-asd-fn (merge-pathnames #P"pseudopod/pseudopod.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"sw4rm-sdk/sw4rm-sdk.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"ptui/ptui.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"amoebum/amoebum.asd" repo-root))
    (funcall load-system-fn "amoebum"))

  (let* ((amoebum-pkg (or (find-package "AMOEBUM")
                          (error "Missing package AMOEBUM after load.")))
         (events-pkg (or (find-package "PTUI.CORE.EVENTS")
                         (error "Missing package PTUI.CORE.EVENTS after load.")))
         (types-pkg (or (find-package "PTUI.CORE.TYPES")
                        (error "Missing package PTUI.CORE.TYPES after load.")))
         (pseudopod-pkg (or (find-package "PSEUDOPOD")
                            (error "Missing package PSEUDOPOD after load.")))
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn-in
           (lambda (name package)
             (symbol-function (funcall symbol-in name package))))
         (make-chat-ui-state-fn (funcall fn-in "MAKE-CHAT-UI-STATE" amoebum-pkg))
         (render-chat-ui-buffer-fn (funcall fn-in "RENDER-CHAT-UI-BUFFER" amoebum-pkg))
         (handle-chat-ui-event-fn (funcall fn-in "HANDLE-CHAT-UI-EVENT" amoebum-pkg))
         (chat-ui-scroll-history-fn (funcall fn-in "CHAT-UI-SCROLL-HISTORY" amoebum-pkg))
         (chat-ui-state-messages-fn (funcall fn-in "CHAT-UI-STATE-MESSAGES" amoebum-pkg))
         (chat-ui-state-stream-state-fn (funcall fn-in "CHAT-UI-STATE-STREAM-STATE" amoebum-pkg))
         (chat-ui-state-message-scrollback-lines-fn
           (funcall fn-in "CHAT-UI-STATE-MESSAGE-SCROLLBACK-LINES" amoebum-pkg))
         (chat-ui-state-stream-scroll-follow-p-fn
           (funcall fn-in "CHAT-UI-STATE-STREAM-SCROLL-FOLLOW-P" amoebum-pkg))
         (token-stream-emit-chunk-fn (funcall fn-in "TOKEN-STREAM-EMIT-CHUNK" amoebum-pkg))
         (token-stream-emit-tool-call-delta-fn
           (funcall fn-in "TOKEN-STREAM-EMIT-TOOL-CALL-DELTA" amoebum-pkg))
         (token-stream-emit-tool-call-started-fn
           (funcall fn-in "TOKEN-STREAM-EMIT-TOOL-CALL-STARTED" amoebum-pkg))
         (token-stream-emit-tool-call-argument-complete-fn
           (funcall fn-in "TOKEN-STREAM-EMIT-TOOL-CALL-ARGUMENT-COMPLETE" amoebum-pkg))
         (token-stream-start-fn (funcall fn-in "TOKEN-STREAM-START" amoebum-pkg))
         (make-token-stream-state-fn (funcall fn-in "MAKE-TOKEN-STREAM-STATE" amoebum-pkg))
         (token-stream-state-started-ms-fn (funcall fn-in "TOKEN-STREAM-STATE-STARTED-MS" amoebum-pkg))
         (token-stream-progress-summary-fn (funcall fn-in "TOKEN-STREAM-PROGRESS-SUMMARY" amoebum-pkg))
         (stream-markdown-styled-lines-fn
           (funcall fn-in "STREAM-MARKDOWN-STYLED-LINES" amoebum-pkg))
         (stream-cursor-visible-p-fn
           (funcall fn-in "STREAM-CURSOR-VISIBLE-P" amoebum-pkg))
         (current-event-bus-fn (funcall fn-in "CURRENT-EVENT-BUS" amoebum-pkg))
         (subscribe-fn (funcall fn-in "SUBSCRIBE" amoebum-pkg))
         (event-payload-fn (funcall fn-in "EVENT-PAYLOAD" amoebum-pkg))
         (stream-budget-warning-payload-usage-percent-fn
           (funcall fn-in "STREAM-BUDGET-WARNING-PAYLOAD-USAGE-PERCENT" amoebum-pkg))
         (stream-budget-warning-payload-threshold-percent-fn
           (funcall fn-in "STREAM-BUDGET-WARNING-PAYLOAD-THRESHOLD-PERCENT" amoebum-pkg))
         (tool-call-started-payload-tool-name-fn
           (funcall fn-in "TOOL-CALL-STARTED-PAYLOAD-TOOL-NAME" amoebum-pkg))
         (tool-call-argument-complete-payload-tool-name-fn
           (funcall fn-in "TOOL-CALL-ARGUMENT-COMPLETE-PAYLOAD-TOOL-NAME" amoebum-pkg))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (current-config-fn (funcall fn-in "CURRENT-CONFIG" amoebum-pkg))
         (config-value-fn (funcall fn-in "CONFIG-VALUE" amoebum-pkg))
         (stream-budget-warning-event-type
           (symbol-value (funcall symbol-in "+EVENT-TYPE-STREAM-BUDGET-WARNING+" amoebum-pkg)))
         (tool-call-started-event-type
           (symbol-value (funcall symbol-in "+EVENT-TYPE-TOOL-CALL-STARTED+" amoebum-pkg)))
         (tool-call-argument-complete-event-type
           (symbol-value (funcall symbol-in "+EVENT-TYPE-TOOL-CALL-ARGUMENT-COMPLETE+" amoebum-pkg)))
         (toolset-symbol (funcall symbol-in "*TOOLSET*" amoebum-pkg))
         (toolset (symbol-value toolset-symbol))
         (register-tool-function-fn (funcall fn-in "REGISTER-TOOL-FUNCTION" pseudopod-pkg))
         (make-tool-call-fn (funcall fn-in "MAKE-TOOL-CALL" pseudopod-pkg))
         (make-key-event-fn (funcall fn-in "MAKE-KEY-EVENT" events-pkg))
         (make-size-fn (funcall fn-in "MAKE-SIZE" types-pkg))
         (buffer-cols-fn (funcall fn-in "CELL-BUFFER-COLS" types-pkg))
         (buffer-rows-fn (funcall fn-in "CELL-BUFFER-ROWS" types-pkg))
         (buffer-cells-fn (funcall fn-in "CELL-BUFFER-CELLS" types-pkg))
         (cell-glyph-fn (funcall fn-in "CELL-GLYPH" types-pkg))
         (message-role-fn (funcall fn-in "MESSAGE-ROLE" pseudopod-pkg))
         (message-content-fn (funcall fn-in "MESSAGE-CONTENT" pseudopod-pkg))
         (content-part-text-fn (funcall fn-in "CONTENT-PART-TEXT" pseudopod-pkg)))
    (labels
        ((assert-true (condition format-string &rest format-args)
           (unless condition
             (error (apply #'format nil format-string format-args))))
         (buffer-cell-at (buffer col row)
           (let* ((cols (funcall buffer-cols-fn buffer))
                  (cells (funcall buffer-cells-fn buffer))
                  (index (+ col (* row cols))))
             (svref cells index)))
         (buffer-lines (buffer)
           (let* ((cols (funcall buffer-cols-fn buffer))
                  (rows (funcall buffer-rows-fn buffer)))
             (loop for row from 0 below rows
                   collect
                   (with-output-to-string (out)
                     (loop for col from 0 below cols do
                       (let ((glyph (funcall cell-glyph-fn
                                             (buffer-cell-at buffer col row))))
                         (when (> (length glyph) 0)
                           (write-string glyph out))))))))
         (rows-contain-p (rows text)
           (loop for row in rows
                 thereis (search text row :test #'char-equal)))
         (message-text (message)
           (with-output-to-string (out)
             (loop for part in (funcall message-content-fn message)
                   for index from 0 do
                     (when (> index 0)
                       (write-char #\Newline out))
                     (write-string (or (funcall content-part-text-fn part) "") out))))
         (first-non-empty-segment (styled-lines)
           (loop for line in styled-lines do
             (loop for segment in line
                   for text = (getf segment :text "")
                   when (plusp (length text))
                     do (return-from first-non-empty-segment segment)))
           nil)
         (last-assistant-text (state)
           (let ((assistant-message
                   (find-if (lambda (message)
                              (string= (funcall message-role-fn message) "assistant"))
                            (funcall chat-ui-state-messages-fn state)
                            :from-end t)))
             (and assistant-message
                  (message-text assistant-message))))
         (word-burst (count)
           (with-output-to-string (out)
             (loop for i from 1 to count do
               (when (> i 1)
                 (write-char #\Space out))
               (format out "w~D" i))))
         (make-tool-schema ()
           (let ((schema (make-hash-table :test #'equal))
                 (properties (make-hash-table :test #'equal)))
             (setf (gethash "type" schema) "object")
             (setf (gethash "properties" schema) properties)
             schema))
         (make-text-event (text)
           (funcall make-key-event-fn :text :text? text)))
      (let* ((runner
               (lambda (stream-state prompt messages &key system-prompt client tools)
                 (declare (ignore prompt messages system-prompt client tools))
                 (funcall token-stream-emit-chunk-fn stream-state "Hello ")
                 (sleep 0.2d0)
                 (funcall token-stream-emit-chunk-fn stream-state "world")))
             (state (funcall make-chat-ui-state-fn :stream-runner runner))
             (size (funcall make-size-fn 100 20)))
        (funcall render-chat-ui-buffer-fn state size)
        (setf state (funcall handle-chat-ui-event-fn state (make-text-event "h")))
        (setf state (funcall handle-chat-ui-event-fn state (make-text-event "i")))
        (setf state (funcall handle-chat-ui-event-fn state (funcall make-key-event-fn :enter)))
        (sleep 0.05d0)
        (let* ((buffer (funcall render-chat-ui-buffer-fn state size))
               (rows (buffer-lines buffer))
               (assistant-text (or (last-assistant-text state) "")))
          (assert-true (search "Hello " assistant-text :test #'char-equal)
                       "Expected first streamed chunk to appear incrementally.")
          (assert-true (rows-contain-p rows "tok/s")
                       "Expected status line to include tok/s during active stream."))
        (sleep 0.25d0)
        (funcall render-chat-ui-buffer-fn state size)
        (let* ((assistant-text (or (last-assistant-text state) ""))
               (summary (funcall token-stream-progress-summary-fn
                                 (funcall chat-ui-state-stream-state-fn state))))
          (assert-true (string= assistant-text "Hello world")
                       "Expected assistant text to include all streamed chunks, got ~S."
                       assistant-text)
          (assert-true (eq (getf summary :status) :completed)
                       "Expected stream status :COMPLETED, got ~S."
                       (getf summary :status))))

      (let* ((tool-name "stream-smoke-i72-tool")
             (tool-call-id "stream-smoke-i72-call-1")
             (partial-arguments "{\"value\":")
             (complete-arguments "{\"value\":1}")
             (tool-started-payloads '())
             (tool-argument-complete-payloads '())
             (tool-executions '())
             (bus (funcall current-event-bus-fn))
             (original-permission-mode
               (funcall config-value-fn
                        :permission-mode
                        (funcall current-config-fn))))
        (unwind-protect
            (progn
              (funcall setconfig-fn :permission-mode :full-auto)
              (funcall register-tool-function-fn
                       toolset
                       :name tool-name
                       :description "I72 streaming smoke tool"
                       :parameters (make-tool-schema)
                       :fn (lambda (arguments &optional tool-call)
                             (declare (ignore tool-call))
                             (push (list :value (gethash "value" arguments))
                                   tool-executions)
                             "ok"))
              (funcall subscribe-fn
                       bus
                       tool-call-started-event-type
                       (lambda (event)
                         (push (funcall event-payload-fn event) tool-started-payloads)))
              (funcall subscribe-fn
                       bus
                       tool-call-argument-complete-event-type
                       (lambda (event)
                         (push (funcall event-payload-fn event) tool-argument-complete-payloads)))
              (let* ((runner
                       (lambda (stream-state prompt messages &key system-prompt client tools)
                         (declare (ignore prompt messages system-prompt client tools))
                         (let ((partial-call
                                 (funcall make-tool-call-fn
                                          :id tool-call-id
                                          :name tool-name
                                          :arguments partial-arguments))
                               (complete-call
                                 (funcall make-tool-call-fn
                                          :id tool-call-id
                                          :name tool-name
                                          :arguments complete-arguments)))
                           (funcall token-stream-emit-tool-call-delta-fn
                                    stream-state
                                    (list :type :tool-call-delta
                                          :index 0
                                          :name tool-name
                                          :arguments partial-arguments
                                          :tool-call partial-call))
                           (funcall token-stream-emit-tool-call-started-fn
                                    stream-state
                                    partial-call)
                           (sleep 0.15d0)
                           (funcall token-stream-emit-tool-call-delta-fn
                                    stream-state
                                    (list :type :tool-call-delta
                                          :index 0
                                          :name tool-name
                                          :arguments complete-arguments
                                          :arguments-complete-p t
                                          :tool-call complete-call))
                           (funcall token-stream-emit-tool-call-argument-complete-fn
                                    stream-state
                                    complete-call)
                           (sleep 0.40d0))))
                     (state (funcall make-chat-ui-state-fn :stream-runner runner))
                     (size (funcall make-size-fn 100 20)))
                (funcall render-chat-ui-buffer-fn state size)
                (setf state (funcall handle-chat-ui-event-fn state (make-text-event "i")))
                (setf state (funcall handle-chat-ui-event-fn state (make-text-event "7")))
                (setf state (funcall handle-chat-ui-event-fn state (funcall make-key-event-fn :enter)))
                (sleep 0.06d0)
                (let* ((buffer (funcall render-chat-ui-buffer-fn state size))
                       (rows (buffer-lines buffer)))
                  (assert-true (rows-contain-p rows tool-name)
                               "Expected streaming UI to show tool name before arguments complete.")
                  (assert-true (rows-contain-p rows partial-arguments)
                               "Expected streaming UI to show partial arguments while streaming.")
                  (assert-true (null tool-executions)
                               "Expected tool not to execute before arguments complete."))
                (sleep 0.20d0)
                (let* ((buffer (funcall render-chat-ui-buffer-fn state size))
                       (rows (buffer-lines buffer))
                       (summary (funcall token-stream-progress-summary-fn
                                         (funcall chat-ui-state-stream-state-fn state))))
                  (assert-true (rows-contain-p rows tool-name)
                               "Expected tool preview row to remain visible during streaming.")
                  (assert-true (= 1 (length tool-executions))
                               "Expected tool execution to start immediately after args completion.")
                  (assert-true (= 1 (or (getf (first tool-executions) :value) -1))
                               "Expected parsed tool arguments to reach execution pipeline.")
                  (assert-true (eq (getf summary :status) :running)
                               "Expected stream to remain active while tool executes early."))
                (assert-true (= 1 (length tool-started-payloads))
                             "Expected exactly one tool-call-started payload, got ~D."
                             (length tool-started-payloads))
                (assert-true (= 1 (length tool-argument-complete-payloads))
                             "Expected exactly one tool-call-argument-complete payload, got ~D."
                             (length tool-argument-complete-payloads))
                (assert-true (string= tool-name
                                      (funcall tool-call-started-payload-tool-name-fn
                                               (first tool-started-payloads)))
                             "Expected started payload tool name ~S, got ~S."
                             tool-name
                             (funcall tool-call-started-payload-tool-name-fn
                                      (first tool-started-payloads)))
                (assert-true (string= tool-name
                                      (funcall tool-call-argument-complete-payload-tool-name-fn
                                               (first tool-argument-complete-payloads)))
                             "Expected argument-complete payload tool name ~S, got ~S."
                             tool-name
                             (funcall tool-call-argument-complete-payload-tool-name-fn
                                      (first tool-argument-complete-payloads)))
                (sleep 0.45d0)
                (funcall render-chat-ui-buffer-fn state size)))
          (funcall setconfig-fn :permission-mode original-permission-mode)))

      (let* ((bold-lines
               (funcall stream-markdown-styled-lines-fn
                        "**bold"
                        40
                        :partialp t
                        :cursor-visible-p nil))
             (italic-lines
               (funcall stream-markdown-styled-lines-fn
                        "*ital"
                        40
                        :partialp t
                        :cursor-visible-p nil))
             (code-lines
               (funcall stream-markdown-styled-lines-fn
                        "`code"
                        40
                        :partialp t
                        :cursor-visible-p nil))
             (heading-lines
               (funcall stream-markdown-styled-lines-fn
                        "# Heading"
                        40
                        :partialp t
                        :cursor-visible-p nil))
             (bold-segment (first-non-empty-segment bold-lines))
             (italic-segment (first-non-empty-segment italic-lines))
             (code-segment (first-non-empty-segment code-lines))
             (heading-segment (first-non-empty-segment heading-lines)))
        (assert-true (and bold-segment (string= (getf bold-segment :text) "bold"))
                     "Expected partial bold markdown text to strip marker, got ~S."
                     bold-segment)
        (assert-true (getf bold-segment :boldp)
                     "Expected partial bold markdown to keep :BOLDP style.")
        (assert-true (and italic-segment (string= (getf italic-segment :text) "ital"))
                     "Expected partial italic markdown text to strip marker, got ~S."
                     italic-segment)
        (assert-true (getf italic-segment :italicp)
                     "Expected partial italic markdown to keep :ITALICP style.")
        (assert-true (and code-segment (string= (getf code-segment :text) "code"))
                     "Expected partial code markdown text to strip marker, got ~S."
                     code-segment)
        (assert-true (getf code-segment :invertp)
                     "Expected partial code markdown to set :INVERTP style.")
        (assert-true (and heading-segment
                          (string= (getf heading-segment :text) "Heading"))
                     "Expected markdown heading text to strip prefix, got ~S."
                     heading-segment)
        (assert-true (eq (getf heading-segment :role) :assistant-heading)
                     "Expected heading line role :ASSISTANT-HEADING, got ~S."
                     heading-segment))

      (let* ((stream-state (funcall make-token-stream-state-fn))
             (worker (lambda (active-stream-state)
                       (declare (ignore active-stream-state))
                       (sleep 0.25d0))))
        (funcall token-stream-start-fn stream-state worker)
        (sleep 0.02d0)
        (let ((started-ms (funcall token-stream-state-started-ms-fn stream-state)))
          (assert-true (funcall stream-cursor-visible-p-fn
                                stream-state
                                :now-ms (+ started-ms 10)
                                :blink-ms 100)
                       "Expected cursor visible during first blink phase.")
          (assert-true (not (funcall stream-cursor-visible-p-fn
                                     stream-state
                                     :now-ms (+ started-ms 150)
                                     :blink-ms 100))
                       "Expected cursor hidden during second blink phase."))
        (sleep 0.30d0))

      (let* ((runner
               (lambda (stream-state prompt messages &key system-prompt client tools)
                 (declare (ignore prompt messages system-prompt client tools))
                 (funcall token-stream-emit-chunk-fn
                          stream-state
                          (with-output-to-string (out)
                            (loop for i from 1 to 28 do
                              (format out "line ~D~%" i))))
                 (sleep 0.35d0)
                 (funcall token-stream-emit-chunk-fn
                          stream-state
                          (with-output-to-string (out)
                            (loop for i from 29 to 40 do
                              (format out "line ~D~%" i))))))
             (state (funcall make-chat-ui-state-fn :stream-runner runner))
             (size (funcall make-size-fn 80 14)))
        (funcall render-chat-ui-buffer-fn state size)
        (setf state (funcall handle-chat-ui-event-fn state (make-text-event "s")))
        (setf state (funcall handle-chat-ui-event-fn state (make-text-event "f")))
        (setf state (funcall handle-chat-ui-event-fn state (funcall make-key-event-fn :enter)))
        (sleep 0.08d0)
        (funcall render-chat-ui-buffer-fn state size)
        (funcall chat-ui-scroll-history-fn state 10)
        (assert-true (> (funcall chat-ui-state-message-scrollback-lines-fn state) 0)
                     "Expected manual scroll-up to move off bottom during stream.")
        (assert-true (not (funcall chat-ui-state-stream-scroll-follow-p-fn state))
                     "Expected scroll-follow to pause after manual scroll-up.")
        (sleep 0.20d0)
        (funcall render-chat-ui-buffer-fn state size)
        (assert-true (> (funcall chat-ui-state-message-scrollback-lines-fn state) 0)
                     "Expected paused scroll-follow to preserve non-zero scrollback.")
        (funcall chat-ui-scroll-history-fn state -1000)
        (funcall render-chat-ui-buffer-fn state size)
        (assert-true (= (funcall chat-ui-state-message-scrollback-lines-fn state) 0)
                     "Expected scroll-to-bottom to reset scrollback to zero.")
        (assert-true (funcall chat-ui-state-stream-scroll-follow-p-fn state)
                     "Expected scroll-follow to resume after returning to bottom.")
        (sleep 0.40d0)
        (funcall render-chat-ui-buffer-fn state size))

      (let ((warning-payloads '())
            (original-context-limit
              (funcall config-value-fn
                       :context-window-limit
                       (funcall current-config-fn))))
        (unwind-protect
            (progn
              (funcall setconfig-fn :context-window-limit 20)
              (let* ((bus (funcall current-event-bus-fn))
                     (runner
                       (lambda (stream-state prompt messages &key system-prompt client tools)
                         (declare (ignore prompt messages system-prompt client tools))
                         (funcall token-stream-emit-chunk-fn
                                  stream-state
                                  (word-burst 40))))
                     (state (funcall make-chat-ui-state-fn
                                     :stream-runner runner))
                     (size (funcall make-size-fn 100 20)))
                (funcall subscribe-fn
                         bus
                         stream-budget-warning-event-type
                         (lambda (event)
                           (push (funcall event-payload-fn event) warning-payloads)))
                (funcall render-chat-ui-buffer-fn state size)
                (setf state (funcall handle-chat-ui-event-fn state (make-text-event "b")))
                (setf state (funcall handle-chat-ui-event-fn state (make-text-event "g")))
                (setf state (funcall handle-chat-ui-event-fn state (funcall make-key-event-fn :enter)))
                (sleep 0.10d0)
                (funcall render-chat-ui-buffer-fn state size)
                (sleep 0.15d0)
                (funcall render-chat-ui-buffer-fn state size)
                (assert-true (= (length warning-payloads) 1)
                             "Expected exactly one stream budget warning event, got ~D."
                             (length warning-payloads))
                (let ((payload (first warning-payloads)))
                  (assert-true (>= (funcall stream-budget-warning-payload-usage-percent-fn payload)
                                   90)
                               "Expected warning usage percent >= 90, got ~S."
                               (funcall stream-budget-warning-payload-usage-percent-fn payload))
                  (assert-true (= (funcall stream-budget-warning-payload-threshold-percent-fn payload)
                                  90)
                               "Expected warning threshold percent 90, got ~S."
                               (funcall stream-budget-warning-payload-threshold-percent-fn payload)))))
          (funcall setconfig-fn :context-window-limit original-context-limit)))

      (let* ((runner
               (lambda (stream-state prompt messages &key system-prompt client tools)
                 (declare (ignore prompt messages system-prompt client tools))
                 (funcall token-stream-emit-chunk-fn stream-state "partial ")
                 (sleep 0.30d0)
                 (funcall token-stream-emit-chunk-fn stream-state "tail")))
             (state (funcall make-chat-ui-state-fn :stream-runner runner))
             (size (funcall make-size-fn 100 20)))
        (funcall render-chat-ui-buffer-fn state size)
        (setf state (funcall handle-chat-ui-event-fn state (make-text-event "c")))
        (setf state (funcall handle-chat-ui-event-fn state (make-text-event "x")))
        (setf state (funcall handle-chat-ui-event-fn state (funcall make-key-event-fn :enter)))
        (sleep 0.05d0)
        (funcall render-chat-ui-buffer-fn state size)
        (setf state (funcall handle-chat-ui-event-fn state (funcall make-key-event-fn :ctrl-c :ctrlp t)))
        (sleep 0.35d0)
        (funcall render-chat-ui-buffer-fn state size)
        (let* ((assistant-text (or (last-assistant-text state) ""))
               (summary (funcall token-stream-progress-summary-fn
                                 (funcall chat-ui-state-stream-state-fn state))))
          (assert-true (string= assistant-text "partial ")
                       "Expected cancellation to preserve partial assistant output, got ~S."
                       assistant-text)
          (assert-true (eq (getf summary :status) :cancelled)
                       "Expected stream status :CANCELLED, got ~S."
                       (getf summary :status))))))

  (format t "AMOEBUM_STREAMING_SMOKE_OK~%"))
