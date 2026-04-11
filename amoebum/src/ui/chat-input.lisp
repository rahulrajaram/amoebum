;;; NXT-281: input-oriented chat helpers extracted mechanically from chat.lisp.
;;; Keep original behavior and call sites stable while narrowing file ownership.
(in-package :amoebum)

(defun chat-ui-set-input (state text &key cursor-position)
  "Set the input text. CURSOR-POSITION: nil = end, integer = grapheme index."
  (let ((chat-state (ensure-chat-ui-state state)))
    (setf (chat-ui-state-input-text chat-state)
          (if (stringp text)
              text
              (princ-to-string text))
          (chat-ui-state-prompt-scroll-offset chat-state) nil
          (chat-ui-state-cursor-position chat-state) cursor-position)
    (%chat-sync-fuzzy-picker! chat-state :step-p nil)
    (chat-ui-state-input-text chat-state)))

(defun %blank-string-p (text)
  (every (lambda (char)
           (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=))
         text))

(defun %chat-push-input-text-part (parts text)
  (if (and (stringp text) (plusp (length text)))
      (cons (pseudopod:make-text-part text) parts)
      parts))

(defun %chat-parse-input-content-parts (input)
  (let ((source (if (stringp input) input (princ-to-string input)))
        (cursor 0)
        (parts '()))
    (labels ((emit-text (from to)
               (when (< from to)
                 (setf parts (%chat-push-input-text-part parts (subseq source from to))))))
      (loop
        for marker = (search "![" source :start2 cursor)
        while marker do
          (let* ((alt-end (position #\] source :start (+ marker 2)))
                 (open-paren (and alt-end
                                  (< (1+ alt-end) (length source))
                                  (char= (char source (1+ alt-end)) #\()
                                  (1+ alt-end)))
                 (close-paren (and open-paren
                                   (position #\) source :start (1+ open-paren)))))
            (unless (and alt-end open-paren close-paren)
              (loop-finish))
            (emit-text cursor marker)
            (let ((path (string-trim '(#\Space #\Tab #\Newline #\Return)
                                     (subseq source (+ open-paren 1) close-paren))))
              (if (plusp (length path))
                  (push (%make-image-content-part path) parts)
                  (emit-text marker (1+ close-paren))))
            (setf cursor (1+ close-paren))))
      (emit-text cursor (length source)))
    (nreverse parts)))

(defun chat-ui-submit-input (state)
  (let* ((chat-state (ensure-chat-ui-state state))
         (conversation (%ensure-chat-conversation-state chat-state))
         (input (chat-ui-state-input-text chat-state)))
    (setf (chat-ui-state-agentic-iteration-count chat-state) 0)
    (if (or (null input) (zerop (length input)) (%blank-string-p input))
        nil
        (handler-case
            (let ((content (%chat-parse-input-content-parts input)))
              (if (null content)
                  nil
                  (prog1
                      (progn
                        (conversation-transition! conversation :user-input)
                        (chat-ui-add-message chat-state "user" content))
                    (setf (chat-ui-state-input-text chat-state) ""
                          (chat-ui-state-prompt-scroll-offset chat-state) nil)
                    (fuzzy-picker-deactivate! (%ensure-chat-fuzzy-picker-state chat-state)))))
          (error (condition)
            (chat-ui-add-message
             chat-state
             "system"
             (format nil "Unable to attach image input: ~A" condition))
            nil)))))

(defun %fit-line-width (text width)
  (ptui.text.layout:truncate-to-width text (max 0 width)))

(defun %prompt-wrapped-lines (value width)
  (if (<= width 0)
      (list "")
      (ptui.text.layout:wrap-by-width value (max 1 width)
                                      :preserve-spaces t)))

(defun %cursor-to-line-col (cursor-pos lines)
  "Given a grapheme-offset CURSOR-POS and list of wrapped LINES, return
\(values line-index col-offset) as display coordinates."
  (let ((remaining cursor-pos))
    (loop for line in lines
          for line-idx from 0
          for line-len = (length line)
          do (if (<= remaining line-len)
                 ;; Cursor is on this line. If it's exactly at line-len and
                 ;; there are more lines, it wraps to next line col 0.
                 (if (and (= remaining line-len)
                          (< (1+ line-idx) (length lines)))
                     ;; Wrap to next line
                     (return-from %cursor-to-line-col
                       (values (1+ line-idx) 0))
                     (return-from %cursor-to-line-col
                       (values line-idx remaining)))
                 (decf remaining line-len)))
    ;; Past end - cursor on last line at end
    (values (max 0 (1- (length lines)))
            (if lines (length (car (last lines))) 0))))

(defun %line-col-to-cursor-pos (line-index col lines)
  "Convert display coordinates (LINE-INDEX, COL) back to a grapheme offset."
  (let ((pos 0))
    (loop for i from 0 below (min line-index (length lines))
          do (incf pos (length (nth i lines))))
    (+ pos (min col (if (< line-index (length lines))
                        (length (nth line-index lines))
                        0)))))

(defun %prompt-visible-lines (lines visible-rows scroll-offset)
  (let* ((row-count (max 0 visible-rows))
         (total (length lines))
         (max-offset (max 0 (- total row-count)))
         (desired (if (null scroll-offset)
                      max-offset
                      scroll-offset))
         (offset (min max-offset (max 0 desired)))
         (end (min total (+ offset row-count))))
    (values (subseq lines offset end) offset max-offset)))

(defun %pop-last-grapheme (text)
  (let ((clusters (ptui.text.grapheme:split-graphemes text)))
    (if (null clusters)
        ""
        (with-output-to-string (out)
          (dolist (cluster (butlast clusters))
            (write-string cluster out))))))

(defun %delete-word-backward (text)
  "Delete the last word from TEXT, following readline Ctrl+W semantics:
skip trailing whitespace, then delete back to the next whitespace boundary."
  (when (or (null text) (zerop (length text)))
    (return-from %delete-word-backward ""))
  (let ((end (length text))
        (pos (1- (length text))))
    ;; Skip trailing whitespace
    (loop while (and (>= pos 0)
                     (member (char text pos) '(#\Space #\Tab #\Newline #\Return)
                             :test #'char=))
          do (decf pos))
    ;; Delete back through non-whitespace (the word)
    (loop while (and (>= pos 0)
                     (not (member (char text pos) '(#\Space #\Tab #\Newline #\Return)
                                  :test #'char=)))
          do (decf pos))
    (if (< pos 0)
        ""
        (subseq text 0 (1+ pos)))))

;;; ---------------------------------------------------------------------------
;;; Cursor-aware grapheme helpers
;;; ---------------------------------------------------------------------------

(defun %grapheme-length (text)
  "Return the number of grapheme clusters in TEXT."
  (if (or (null text) (zerop (length text)))
      0
      (length (ptui.text.grapheme:split-graphemes text))))

(defun %word-boundary-p (cluster)
  "Return T if CLUSTER is a word-boundary character (whitespace or punctuation)."
  (and (= (length cluster) 1)
       (let ((ch (char cluster 0)))
         (or (member ch '(#\Space #\Tab #\Newline #\Return) :test #'char=)
             (member ch '(#\( #\) #\[ #\] #\{ #\} #\< #\>
                          #\. #\, #\; #\: #\! #\? #\/ #\\
                          #\- #\+ #\= #\* #\& #\| #\^ #\~
                          #\' #\" #\` #\@ #\# #\$ #\%)
                     :test #'char=)))))

(defun %word-boundary-backward (text cursor-pos)
  "Return the grapheme offset for the word boundary to the left of CURSOR-POS."
  (let* ((clusters (ptui.text.grapheme:split-graphemes text))
         (pos (min (max 0 cursor-pos) (length clusters))))
    (when (<= pos 0)
      (return-from %word-boundary-backward 0))
    ;; Skip whitespace/punctuation backward
    (loop while (and (> pos 0)
                     (%word-boundary-p (nth (1- pos) clusters)))
          do (decf pos))
    ;; Skip word characters backward
    (loop while (and (> pos 0)
                     (not (%word-boundary-p (nth (1- pos) clusters))))
          do (decf pos))
    pos))

(defun %word-boundary-forward (text cursor-pos)
  "Return the grapheme offset for the word boundary to the right of CURSOR-POS."
  (let* ((clusters (ptui.text.grapheme:split-graphemes text))
         (len (length clusters))
         (pos (min (max 0 cursor-pos) len)))
    (when (>= pos len)
      (return-from %word-boundary-forward len))
    ;; Skip word characters forward
    (loop while (and (< pos len)
                     (not (%word-boundary-p (nth pos clusters))))
          do (incf pos))
    ;; Skip whitespace/punctuation forward
    (loop while (and (< pos len)
                     (%word-boundary-p (nth pos clusters)))
          do (incf pos))
    pos))

(defun %grapheme-insert-at (text cursor-pos new-text)
  "Insert NEW-TEXT at grapheme position CURSOR-POS in TEXT."
  (let* ((clusters (ptui.text.grapheme:split-graphemes text))
         (pos (min (max 0 cursor-pos) (length clusters))))
    (with-output-to-string (out)
      (loop for cluster in (subseq clusters 0 pos)
            do (write-string cluster out))
      (write-string new-text out)
      (loop for cluster in (nthcdr pos clusters)
            do (write-string cluster out)))))

(defun %grapheme-delete-before (text cursor-pos)
  "Delete grapheme before CURSOR-POS. Returns (values new-text new-cursor)."
  (if (or (<= cursor-pos 0) (zerop (length text)))
      (values text 0)
      (let* ((clusters (ptui.text.grapheme:split-graphemes text))
             (pos (min cursor-pos (length clusters))))
        (values
         (with-output-to-string (out)
           (loop for cluster in (subseq clusters 0 (1- pos))
                 do (write-string cluster out))
           (loop for cluster in (nthcdr pos clusters)
                 do (write-string cluster out)))
         (1- pos)))))

(defun %grapheme-delete-at (text cursor-pos)
  "Delete grapheme at CURSOR-POS (Delete key). Returns new text."
  (let* ((clusters (ptui.text.grapheme:split-graphemes text))
         (pos (min cursor-pos (length clusters))))
    (if (>= pos (length clusters))
        text
        (with-output-to-string (out)
          (loop for cluster in (subseq clusters 0 pos)
                do (write-string cluster out))
          (loop for cluster in (nthcdr (1+ pos) clusters)
                do (write-string cluster out))))))

(defun %delete-word-backward-at (text cursor-pos)
  "Delete word backward from CURSOR-POS. Returns (values new-text new-cursor)."
  (if (or (<= cursor-pos 0) (zerop (length text)))
      (values text 0)
      (let* ((clusters (ptui.text.grapheme:split-graphemes text))
             (pos (min cursor-pos (length clusters)))
             (before (with-output-to-string (out)
                       (loop for cluster in (subseq clusters 0 pos)
                             do (write-string cluster out))))
             (after-delete (%delete-word-backward before))
             (new-cursor (%grapheme-length after-delete)))
        (values
         (with-output-to-string (out)
           (write-string after-delete out)
           (loop for cluster in (nthcdr pos clusters)
                 do (write-string cluster out)))
         new-cursor))))

(defvar *chat-cursor-blink-start* nil)

(defun chat-ui-cursor-visible-p (chat-state)
  "Return T if the input cursor should be visible based on blink phase."
  (declare (ignore chat-state))
  ;; Initialize blink start time on first call
  (unless *chat-cursor-blink-start*
    (setf *chat-cursor-blink-start* (get-internal-real-time)))
  ;; Always visible if we can't determine time
  (let ((now (get-internal-real-time))
        (start *chat-cursor-blink-start*))
    (if (or (null now) (null start) (<= now start))
        t
        (let* ((units-per-sec internal-time-units-per-second)
               ;; Blink every 1060ms (530ms on, 530ms off)
               (blink-period-ms 1060)
               (half-period-ms 530)
               ;; Calculate elapsed time in milliseconds safely
               (elapsed-units (- now start))
               (elapsed-ms (floor (* elapsed-units blink-period-ms)
                                  (max 1 units-per-sec)))
               ;; Get position in current blink cycle
               (phase (mod elapsed-ms blink-period-ms)))
          ;; Visible during first half of cycle
          (< phase half-period-ms)))))

(defun %ensure-cursor-pos (text cursor-pos)
  "Coerce NIL cursor-position to end-of-text grapheme count."
  (if (null cursor-pos)
      (%grapheme-length text)
      cursor-pos))

(defun %format-compression-output (compression)
  (if (getf compression :compressed-p)
      (format nil
              "Compacted context: ~D -> ~D tokens (saved ~D). Summarized ~D messages; kept last ~D turns."
              (getf compression :before-tokens 0)
              (getf compression :after-tokens 0)
              (getf compression :saved-tokens 0)
              (getf compression :summarized-messages 0)
              (getf compression :keep-last-turns +context-compression-default-keep-last-turns+))
      "Compaction skipped: not enough older context to summarize."))

(defun %slash-command-clear-chat-action (result &key chat-state)
  (declare (ignore result))
  (let ((conversation (%ensure-chat-conversation-state chat-state)))
    (conversation-reset! conversation)
    (%chat-deactivate-history-search! chat-state)
    (setf (chat-ui-state-messages chat-state)
          (conversation-state-messages conversation)
          (chat-ui-state-message-scrollback-lines chat-state) 0
          (chat-ui-state-max-message-scrollback-lines chat-state) 0)
    (%invalidate-styled-lines-cache)
    (%sync-chat-context-usage! chat-state :allow-auto-compress-p nil)
    nil))

(defun %slash-command-compact-chat-action (result &key chat-state)
  (let ((compression
          (%compress-chat-history!
           chat-state
           :keep-last-turns (slash-command-result-payload result)
           :trigger :manual)))
    (%sync-chat-context-usage! chat-state :allow-auto-compress-p nil)
    (%format-compression-output compression)))

(defun %slash-command-toggle-provider-dashboard-action (result &key chat-state)
  (let* ((payload (slash-command-result-payload result))
         (current (chat-ui-state-provider-dashboard-visible-p chat-state))
         (next
           (case payload
             ((:on t) t)
             ((:off nil) nil)
             (otherwise (not current)))))
    (setf (chat-ui-state-provider-dashboard-visible-p chat-state) next)
    (provider-health-refresh! :force t)
    (%sync-chat-context-usage! chat-state :allow-auto-compress-p nil)
    (format nil "Provider dashboard ~:[hidden~;visible~]." next)))

(eval-when (:load-toplevel :execute)
  (register-slash-command-action-handler :clear-chat #'%slash-command-clear-chat-action)
  (register-slash-command-action-handler :compact-chat #'%slash-command-compact-chat-action)
  (register-slash-command-action-handler :toggle-provider-dashboard
                                         #'%slash-command-toggle-provider-dashboard-action))

(defun %handle-slash-command-input (chat-state input)
  (multiple-value-bind (handledp result)
      (resolve-slash-command input
                             :config (current-config)
                             :memory-backend (current-memory-backend)
                             :chat-state chat-state)
    (when handledp
      (let ((action-output (apply-slash-command-result-action result :chat-state chat-state)))
        (when (slash-command-result-echo-input-p result)
          (chat-ui-add-message chat-state "user" input))
        (let ((output (or action-output
                          (slash-command-result-output result))))
          (when (and (stringp output)
                     (plusp (length (%slash-trim output))))
            (chat-ui-add-message chat-state "system" output)))
        (setf (chat-ui-state-input-text chat-state) ""
              (chat-ui-state-prompt-scroll-offset chat-state) nil)
        t))))

(defun %handle-command-tab-completion (chat-state)
  (let ((input (chat-ui-state-input-text chat-state)))
    (unless (slash-command-input-p input)
      (return-from %handle-command-tab-completion nil))
    (multiple-value-bind (replacement suggestions)
        (complete-slash-command-input input)
      (cond
        ((and (stringp replacement)
              (not (string= replacement input)))
         (chat-ui-set-input chat-state replacement)
         t)
        ((and (listp suggestions)
              (> (length suggestions) 1))
         (chat-ui-add-message
          chat-state
          "system"
          (format nil "Completions: ~{~A~^, ~}" suggestions))
         t)
        (t
         nil)))))

(defun %handle-memory-candidate (chat-state submitted-message)
  (let* ((user-text (%message-content->text submitted-message))
         (candidate (extract-durable-memory-candidate user-text)))
    (when candidate
      (multiple-value-bind (status payload)
          (apply-memory-candidate candidate :backend (current-memory-backend))
        (case status
          (:stored
           (chat-ui-add-message
            chat-state
            "system"
            (format nil "Memory saved: [~A] ~A"
                    (memory-entry-key payload)
                    (memory-entry-value payload)))
           t)
          (:deleted
           (chat-ui-add-message
            chat-state
            "system"
            (format nil "Memory removed: ~A" payload))
           t)
          (:not-found
           (chat-ui-add-message
            chat-state
            "system"
            (format nil "No existing memory matched ~A." payload))
           t)
          (:candidate
           (chat-ui-add-message
            chat-state
            "system"
            (format nil "Detected durable preference candidate: \"~A\". Use /memory remember <text> to persist."
                    (memory-candidate-text candidate)))
           nil)
          (otherwise nil))))))

(defun %chat-input-route-submitted-message (chat-state submitted-message)
  (when submitted-message
    (if (%handle-memory-candidate chat-state submitted-message)
        (conversation-transition! (%ensure-chat-conversation-state chat-state)
                                  :idle)
        (%start-streaming-assistant-response chat-state submitted-message))
    t))

(defun %chat-trim-text (text)
  (if (stringp text)
      (string-trim '(#\Space #\Tab #\Newline #\Return) text)
      ""))

(defun %normalize-instruction-text (text)
  (let ((trimmed (%chat-trim-text text)))
    (with-output-to-string (out)
      (loop with previous-space-p = t
            for char across (string-downcase trimmed) do
              (if (or (alphanumericp char) (char= char #\Space))
                  (progn
                    (write-char char out)
                    (setf previous-space-p (char= char #\Space)))
                  (unless previous-space-p
                    (write-char #\Space out)
                    (setf previous-space-p t)))))))

(defun %plan-mode-entry-instruction-p (input)
  (let* ((normalized (%normalize-instruction-text input))
         (padded (format nil " ~A " normalized)))
    (or (search " enter plan mode " padded :test #'char=)
        (search " enable plan mode " padded :test #'char=)
        (search " switch to plan mode " padded :test #'char=)
        (search " go into plan mode " padded :test #'char=)
        (search " turn on plan mode " padded :test #'char=)
        (search " plan mode on " padded :test #'char=))))

(defun %handle-plan-mode-entry-instruction (chat-state input)
  (when (%plan-mode-entry-instruction-p input)
    (chat-ui-add-message chat-state "user" input)
    (when (%handle-slash-command-input chat-state "/plan on")
      (setf (chat-ui-state-input-text chat-state) ""
            (chat-ui-state-prompt-scroll-offset chat-state) nil)
      t)))

(defun %chat-input-handle-submit-routing (chat-state input)
  (cond
    ((and (plan-step-awaiting-approval-p)
          (zerop (length input)))
     (approve-next-plan-step)
     t)
    ((%handle-slash-command-input chat-state input)
     t)
    ((%handle-plan-mode-entry-instruction chat-state input)
     t)
    (t
     (%chat-input-route-submitted-message
      chat-state
      (chat-ui-submit-input chat-state)))))

(defun %chat-plan-move-selection! (chat-state delta)
  (let* ((plan-state (current-plan-mode-state))
         (execution-state (current-plan-execution-state)))
    (unless (and (%chat-plan-workspace-visible-p plan-state execution-state)
                 (plan-mode-state-p plan-state)
                 (plan-mode-state-steps plan-state)
                 (integerp delta)
                 (/= delta 0))
      (return-from %chat-plan-move-selection! nil))
    (let* ((visible-steps (%chat-plan-visible-steps plan-state))
           (visible-indexes
             (loop for step in visible-steps
                   for index = (plan-step-index step)
                   when (integerp index)
                     collect index)))
      (unless visible-indexes
        (setf (chat-ui-state-plan-selected-step-index chat-state) nil)
        (return-from %chat-plan-move-selection! nil))
      (let* ((current-index
               (%chat-plan-resolve-selected-step-index chat-state
                                                      plan-state
                                                      visible-steps))
             (current-position
               (or (position current-index visible-indexes :test #'=)
                   0))
             (next-position
               (min (1- (length visible-indexes))
                    (max 0 (+ current-position delta))))
             (next-index (nth next-position visible-indexes)))
        (setf (chat-ui-state-plan-selected-step-index chat-state) next-index)
        t))))

(defun %chat-input-handle-vertical-cursor-move (chat-state cursor-pos input-width delta)
  (let* ((input-text (chat-ui-state-input-text chat-state))
         (pos (%ensure-cursor-pos input-text cursor-pos))
         (lines (%prompt-wrapped-lines input-text input-width))
         (max-rows 4))
    (multiple-value-bind (cur-line cur-col)
        (%cursor-to-line-col pos lines)
      (let ((target-line (+ cur-line delta)))
        (when (and (>= target-line 0)
                   (< target-line (length lines)))
          (let* ((line (nth target-line lines))
                 (target-col (min cur-col (length line)))
                 (new-pos (%line-col-to-cursor-pos target-line target-col lines))
                 (last-line-p (= target-line (1- (length lines)))))
            (setf (chat-ui-state-cursor-position chat-state)
                  (if (and last-line-p
                           (= target-col (length line)))
                      nil
                      new-pos))
            (let* ((current-scroll (or (chat-ui-state-prompt-scroll-offset chat-state) 0))
                   (total-lines (length lines))
                   (visible-rows (min max-rows total-lines))
                   (max-scroll (max 0 (- total-lines visible-rows)))
                   (new-scroll
                     (cond
                       ((< target-line current-scroll)
                        target-line)
                       ((>= target-line (+ current-scroll visible-rows))
                        (max 0 (- target-line visible-rows -1)))
                       (t current-scroll))))
              (setf (chat-ui-state-prompt-scroll-offset chat-state)
                    (min new-scroll max-scroll)))
            t))))))

(defun %agent-completion-summary (completion)
  (let* ((result (getf completion :result))
         (output (%chat-trim-text (getf completion :stdout)))
         (stderr (%chat-trim-text (getf completion :stderr)))
         (error-message (%chat-trim-text (getf completion :error-message))))
    (cond
      ((and result (not (null result)))
       (princ-to-string result))
      ((plusp (length output))
       output)
      ((plusp (length stderr))
       (format nil "stderr: ~A" stderr))
      ((plusp (length error-message))
       (format nil "error: ~A" error-message))
      (t
       "no output"))))

(defun %inject-agent-completions (chat-state)
  (let ((count 0))
    (dolist (completion (drain-agent-completions))
      (incf count)
      (let ((agent-id (getf completion :id))
            (agent-type (or (getf completion :type) :task))
            (status (or (getf completion :status) :completed)))
        (chat-ui-add-message
         chat-state
         "tool"
         (format nil "subagent ~A (~A) ~A: ~A"
                 agent-id
                 (string-downcase (symbol-name agent-type))
                 (string-downcase (symbol-name status))
                 (%agent-completion-summary completion)))))
    count))

(defun %voice-transcription-text (payload)
  (cond
    ((stringp payload) payload)
    ((listp payload) (or (getf payload :text) ""))
    (t "")))

(defun %inject-voice-transcriptions (chat-state)
  (let ((count 0))
    (dolist (payload (drain-voice-transcriptions))
      (let ((text (%chat-trim-text (%voice-transcription-text payload))))
        (when (plusp (length text))
          (incf count)
          (let ((conversation (%ensure-chat-conversation-state chat-state)))
            (unless (eq (conversation-state-state conversation) :user-input)
              (conversation-transition! conversation :user-input :save-p nil)))
          (let ((submitted (chat-ui-add-message chat-state "user" text)))
            (unless (%handle-memory-candidate chat-state submitted)
              (unless (token-stream-active-p (chat-ui-state-stream-state chat-state))
                (%start-streaming-assistant-response chat-state submitted)))))))
    count))

(defun %chat-apply-fuzzy-picker-selection! (chat-state)
  (let* ((picker (%ensure-chat-fuzzy-picker-state chat-state))
         (selected-path (fuzzy-picker-selected-path picker)))
    (if (and (stringp selected-path)
             (plusp (length selected-path)))
        (let* ((raw-input (chat-ui-state-input-text chat-state))
               (replaced (fuzzy-picker-apply-selection raw-input picker selected-path))
               (next-input (if (and (plusp (length replaced))
                                    (%whitespace-char-p
                                     (char replaced (1- (length replaced)))))
                               replaced
                               (concatenate 'string replaced " "))))
          (setf (chat-ui-state-input-text chat-state) next-input
                (chat-ui-state-prompt-scroll-offset chat-state) nil)
          (fuzzy-picker-deactivate! picker)
          t)
        (progn
          (fuzzy-picker-deactivate! picker)
          t))))

(defun %chat-apply-history-picker-selection! (chat-state)
  (let* ((picker (%ensure-chat-fuzzy-picker-state chat-state))
         (selected-path (fuzzy-picker-selected-path picker))
         (table (chat-ui-state-history-selection-map chat-state))
         (replacement (and (hash-table-p table)
                           (stringp selected-path)
                           (gethash selected-path table))))
    (setf (chat-ui-state-input-text chat-state) (or replacement "")
          (chat-ui-state-prompt-scroll-offset chat-state) nil)
    (%chat-deactivate-history-search! chat-state)
    t))

(defun %handle-chat-ui-unrouted-key-event (chat-state event)
  "Fallback key handler for smoke tests that dispatch keys before first render
or before runtime routing has a focused target."
  (let* ((key (ptui.core.events:key-event-key event))
         (text (ptui.core.events:key-event-text? event))
         (input-text (chat-ui-state-input-text chat-state))
         (tree-state (%ensure-chat-tree-browser-state chat-state)))
    (cond
      ((and (chat-ui-state-history-search-active-p chat-state)
            (eql key :escape))
       (%chat-deactivate-history-search! chat-state :restore-input-p t))
      ((and (zerop (length input-text))
            (typep tree-state 'tree-browser-state)
            (member key '(:up :down :left :right :enter :return :escape) :test #'eq))
       (chat-panel-handle-tree-browser-key chat-state key))
      ((or (eql key :pgup) (eql key :pgdn))
       (chat-ui-scroll-history chat-state (if (eql key :pgup) 5 -5)))
      ;; Up/Down with empty input -> scroll history (not input cursor movement)
      ((and (member key '(:up :down) :test #'eq)
            (zerop (length input-text)))
       (chat-ui-scroll-history chat-state (if (eql key :up) 3 -3)))
      ((member key '(:text :enter :return :backspace :delete
                      :ctrl-j :tab :ctrl-p :ctrl-n :ctrl-r
                      :ctrl-a :ctrl-e :left :right
                      :ctrl-left :ctrl-right :home :end
                      :ctrl-w :ctrl-u :ctrl-k
                      :up :down :escape)
               :test #'eq)
       (chat-panel-handle-input-key chat-state
                                    (if (eql key :return) :enter key)
                                    text
                                    80))
      (t
       nil))))

(defun %make-approval-dialog-key-handler-table ()
  (let ((table (make-hash-table :test #'eq)))
    (dolist (key '(:up :down :left :right :enter :return :escape))
      (setf (gethash key table) '%approval-dialog-handle-navigation-key))
    (setf (gethash :text table) '%approval-dialog-handle-text-key)
    table))

(defparameter *approval-dialog-key-handlers*
  (%make-approval-dialog-key-handler-table))

(defun %approval-dialog-handle-navigation-key (approval-state key text)
  (declare (ignore text))
  (approval-dialog-handle-key! approval-state key))

(defun %approval-dialog-handle-text-key (approval-state key text)
  (declare (ignore key))
  (approval-dialog-handle-text! approval-state text))

(defun %make-chat-ui-key-handler-table ()
  (let ((table (make-hash-table :test #'eq)))
    (setf (gethash :ctrl-c table) '%chat-ui-handle-ctrl-c-key)
    (setf (gethash :escape table) '%chat-ui-handle-escape-key)
    table))

(defparameter *chat-ui-key-handlers* (%make-chat-ui-key-handler-table))

(defun %chat-ui-route (runtime event)
  (if (typep event 'ptui.core.events:key-event)
      (ptui.ui.runtime:route-event runtime event)
      (list :kind :unhandled :event event)))

(defun %chat-ui-unhandled-route-p (runtime route)
  (or (null (ptui.ui.runtime:runtime-root runtime))
      (eq (getf route :kind) :unhandled)))

(defun %chat-ui-key-context (chat-state event)
  (list :chat-state chat-state
        :event event
        :stream-state (chat-ui-state-stream-state chat-state)
        :approval-state (chat-ui-state-approval-dialog-state chat-state)))

(defun %chat-ui-disarm-quit-unless-ctrl-c! (chat-state key)
  (unless (eq key :ctrl-c)
    (%chat-disarm-ctrl-c-quit! chat-state)))

(defun %chat-ui-handle-approval-key-event (context key text)
  (let ((approval-state (getf context :approval-state))
        (stream-state (getf context :stream-state)))
    (when (approval-dialog-state-active-p approval-state)
      (handler-case
          (if (eq key :ctrl-c)
              (if (token-stream-active-p stream-state)
                  (progn
                    (token-stream-request-cancel stream-state)
                    (ignore-errors
                      (token-stream-force-reset-if-stuck stream-state))
                    t)
                  (approval-dialog-handle-key! approval-state :escape))
              (let* ((handler-name (gethash key *approval-dialog-key-handlers*))
                     (handler (and handler-name (symbol-function handler-name))))
                (when handler
                  (funcall handler approval-state key text))))
        (error (condition)
          (%handle-approval-ui-error! (getf context :chat-state) :event condition)))
      t)))

(defun %chat-ui-handle-escape-key (context key)
  (declare (ignore key))
  (let ((stream-state (getf context :stream-state)))
    (when (token-stream-active-p stream-state)
      (token-stream-request-cancel stream-state)
      (ignore-errors
        (token-stream-force-reset-if-stuck stream-state))
      :consume)))

(defun %chat-ui-handle-ctrl-c-key (context key)
  (declare (ignore key))
  (let ((chat-state (getf context :chat-state))
        (stream-state (getf context :stream-state)))
    (cond
      ((token-stream-active-p stream-state)
       (token-stream-request-cancel stream-state)
       (ignore-errors
         (token-stream-force-reset-if-stuck stream-state))
       :consume)
      ((%chat-ctrl-c-quit-armed-p chat-state)
       (%chat-disarm-ctrl-c-quit! chat-state)
       :quit)
      (t
       (%chat-arm-ctrl-c-quit! chat-state)
       :consume))))

(defun %chat-ui-scroll-page-lines (chat-state)
  (max 5
       (floor (max 10
                   (1+ (chat-ui-state-max-message-scrollback-lines chat-state)))
              3)))

(defun %chat-ui-handle-page-scroll-key (context key)
  (let ((chat-state (getf context :chat-state)))
    (case key
      (:pgup
       (chat-ui-scroll-history chat-state (%chat-ui-scroll-page-lines chat-state))
       :consume)
      (:pgdn
       (chat-ui-scroll-history chat-state (- (%chat-ui-scroll-page-lines chat-state)))
       :consume)
      (:home
       (setf (chat-ui-state-message-scrollback-lines chat-state)
             (max 0 (chat-ui-state-max-message-scrollback-lines chat-state))
             (chat-ui-state-stream-scroll-follow-p chat-state) nil)
       :consume)
      (:end
       (setf (chat-ui-state-message-scrollback-lines chat-state) 0
             (chat-ui-state-stream-scroll-follow-p chat-state) t)
       :consume)
      (otherwise
       nil))))

(defun %chat-ui-handle-key-event (chat-state runtime event route)
  (checkpoint-mark-activity)
  (%chat-mark-activity)
  (let* ((key (ptui.core.events:key-event-key event))
         (text (ptui.core.events:key-event-text? event))
         (context (%chat-ui-key-context chat-state event)))
    (%chat-ui-disarm-quit-unless-ctrl-c! chat-state key)
    (cond
      ((%chat-ui-handle-approval-key-event context key text)
       :consume)
      ((%chat-ui-handle-page-scroll-key context key)
       :consume)
      (t
       (let* ((handler-name (gethash key *chat-ui-key-handlers*))
              (handler (and handler-name (symbol-function handler-name))))
         (or (and handler (funcall handler context key))
             (when (%chat-ui-unhandled-route-p runtime route)
               (and (%handle-chat-ui-unrouted-key-event chat-state event)
                    :consume))))))))

(defun %chat-ui-dispatch-routed-event (chat-state runtime route)
  (when (and (ptui.ui.runtime:runtime-root runtime)
             (listp route))
    (handler-case
        (ptui.widgets.core:dispatch-widget-event
         (ptui.ui.runtime:runtime-root runtime)
         route
         :bubble t)
      (error (condition)
        (if (%approval-recovery-active-p chat-state)
            (%handle-approval-ui-error! chat-state :dispatch condition)
            (error condition))))))

(defun %chat-ui-text-q-consumed-p (event outcome)
  (and (typep event 'ptui.core.events:key-event)
       (eql (ptui.core.events:key-event-key event) :text)
       (string-equal (or (ptui.core.events:key-event-text? event) "") "q")
       (not (eq outcome :quit))))

(defun handle-chat-ui-event (state event)
  (let* ((chat-state (ensure-chat-ui-state state))
         (runtime (chat-ui-state-runtime chat-state))
         (route (%chat-ui-route runtime event))
         (outcome nil))
    ;; Sync approval dialog state so key routing can intercept for active dialogs.
    ;; All other sync happens in %sync-all-state! during render.
    (%sync-pending-approval-dialog! chat-state)
    (when (typep event 'ptui.core.events:key-event)
      (setf outcome (%chat-ui-handle-key-event chat-state runtime event route)))
    (unless outcome
      (%chat-ui-dispatch-routed-event chat-state runtime route))
    (when (%chat-ui-text-q-consumed-p event outcome)
      ;; Amoebum owns quit semantics now; a bare "q" should route normally and
      ;; never fall through to PTUI's legacy default quit binding.
      (setf outcome :consume))
    (values chat-state
            outcome)))
