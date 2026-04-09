(in-package :amoebum)

;;; NXT-280: render-oriented chat helpers extracted mechanically from
;;; chat.lisp. Keep original form order; avoid behavior changes here.

(defun %message-display-text (message)
  (let ((body (%message-content->text message)))
    (if (string= (%normalize-chat-role (pseudopod:message-role message)) "tool")
        (or (ignore-errors
              (%tool-json-display-text (jonathan:parse body :as :hash-table)))
            (ignore-errors
              (%tool-json-display-text (jonathan:parse body)))
            body)
        body)))

(defun %replace-message-at-index! (messages index message)
  (let ((cell (nthcdr index messages)))
    (when cell
      (setf (car cell) message)
      t)))

;;; Kimi K2.5 Thinking Overlay
;;; ---------------------------------------------------------------------------

(defparameter +thinking-overlay-max-lines+ 10
  "Maximum number of lines to display in the thinking overlay.")

(defparameter +thinking-overlay-border-chars+
  '((:top-left . "╭") (:top-right . "╮")
    (:bottom-left . "╰") (:bottom-right . "╯")
    (:horizontal . "─") (:vertical . "│")))


;;; ---------------------------------------------------------------------------



(defun chat-ui-append-thinking-chunk (chat-state chunk)
  "Append a chunk of thinking/reasoning content to the overlay."
  (when (and (stringp chunk) (plusp (length chunk)))
    (setf (chat-ui-state-stream-thinking-content chat-state)
          (concatenate 'string
                       (chat-ui-state-stream-thinking-content chat-state)
                       chunk))))

(defun chat-ui-clear-thinking-content (chat-state)
  "Clear the thinking content buffer."
  (setf (chat-ui-state-stream-thinking-content chat-state) "")
  chat-state)

(defun chat-ui-toggle-thinking-visible (chat-state)
  "Toggle the visibility of the thinking overlay."
  (setf (chat-ui-state-stream-thinking-visible-p chat-state)
        (not (chat-ui-state-stream-thinking-visible-p chat-state)))
  chat-state)

(defun %format-thinking-content (content max-lines max-width)
  "Format thinking content for display, truncating to max-lines."
  (let* ((lines (split-sequence:split-sequence #\Newline content))
         (truncated (if (> (length lines) max-lines)
                        (subseq lines 0 max-lines)
                        lines))
         (padded (append truncated
                        (make-list (max 0 (- max-lines (length truncated)))
                                   :initial-element "")))
         (wrapped-lines
           (loop for line in padded
                 append (or (ptui.text.layout:wrap-by-width line max-width)
                            (list ""))))
         (final-lines (if (> (length wrapped-lines) max-lines)
                          (subseq wrapped-lines 0 max-lines)
                          wrapped-lines)))
    (mapcar (lambda (line)
              (let ((len (length line)))
                (if (< len max-width)
                    (concatenate 'string line
                                 (make-string (- max-width len)
                                              :initial-element #\Space))
                    (subseq line 0 max-width))))
            final-lines)))

(defun %make-thinking-overlay-widget (chat-state width)
  "Create the thinking overlay widget with border and content."
  (let* ((content (chat-ui-state-stream-thinking-content chat-state))
         (visible-p (chat-ui-state-stream-thinking-visible-p chat-state))
         (stream-active-p (token-stream-active-p (chat-ui-state-stream-state chat-state)))
         (max-content-width (max 10 (- width 4)))  ; 2 chars for border + padding
         (lines (when (and visible-p
                           (or stream-active-p (plusp (length content))))
                  (%format-thinking-content content +thinking-overlay-max-lines+ max-content-width)))
         (bc +thinking-overlay-border-chars+))
    (if (null lines)
        ;; Empty/invisible: return a zero-height spacer
        (ptui.widgets.core:make-spacer-widget 0 0 :key :thinking-overlay-empty)
        ;; Visible overlay with border
        (let* ((top-border (concatenate 'string
                                        (cdr (assoc :top-left bc))
                                        (make-string max-content-width :initial-element (char (cdr (assoc :horizontal bc)) 0))
                                        (cdr (assoc :top-right bc))))
               (bottom-border (concatenate 'string
                                           (cdr (assoc :bottom-left bc))
                                           (make-string max-content-width :initial-element (char (cdr (assoc :horizontal bc)) 0))
                                           (cdr (assoc :bottom-right bc))))
               (title " 💭 thinking ")
               (title-len (length title))
               (title-padded (concatenate 'string
                                          (subseq top-border 0 2)
                                          title
                                          (subseq top-border (+ 2 title-len))))
               (content-lines
                 (loop for line in lines
                       collect (concatenate 'string
                                            (cdr (assoc :vertical bc))
                                            " " line " "
                                            (cdr (assoc :vertical bc)))))
               (all-lines (cons title-padded
                                (append content-lines (list bottom-border))))
               (text-widgets
                 (loop for line in all-lines
                       for idx from 0
                       collect (ptui.widgets.core:make-text-widget
                                line
                                :id (list :thinking idx)
                                :role :thinking-overlay))))
          (ptui.widgets.core:make-stack-widget
           text-widgets
           :id :thinking-overlay-stack
           :direction :column
           :gap 0)))))





(defun %parse-tool-arguments (arguments-string)
  "Parse a JSON arguments string into an alist of (key . value-string) pairs.
Falls back to ((\"args\" . original-string)) on parse failure."
  (if (or (null arguments-string) (string= arguments-string ""))
      '()
      (handler-case
          (let ((parsed (jonathan:parse arguments-string :as :alist)))
            (if (listp parsed)
                (loop for (key . val) in parsed
                      collect (cons (if (stringp key) key (princ-to-string key))
                                    (if (stringp val)
                                        val
                                        (%normalize-inline-text (princ-to-string val)))))
                (list (cons "args" arguments-string))))
        (error () (list (cons "args" arguments-string))))))

(defun %format-tool-argument-lines (arguments-alist indent-width content-width)
  "Format parsed tool arguments into indented display lines.
INDENT-WIDTH is the left padding for continuation lines.
CONTENT-WIDTH is the max width for value text.
Returns a list of strings, one per display line."
  (let ((indent (make-string indent-width :initial-element #\Space))
        (lines '()))
    (dolist (pair arguments-alist)
      (let* ((key (car pair))
             (value (cdr pair))
             (prefix (format nil "~A~A: " indent key))
             (prefix-width (length prefix))
             (value-width (max 10 (- content-width prefix-width)))
             (wrapped (ptui.text.layout:wrap-by-width value (max 10 value-width))))
        (when (null wrapped) (setf wrapped (list "")))
        (let ((cont-indent (make-string prefix-width :initial-element #\Space)))
          (loop for wline in wrapped
                for i from 0 do
                  (push (if (zerop i)
                            (concatenate 'string prefix wline)
                            (concatenate 'string cont-indent wline))
                        lines)))))
    (nreverse lines)))

(defparameter +tool-spinner-frames+
  #("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"))

(defun %tool-call-spinner-glyph (frame-count)
  "Return the current spinner glyph based on the frame counter."
  (aref +tool-spinner-frames+
        (mod (floor frame-count 3) (length +tool-spinner-frames+))))

(defun %stream-tool-call-preview-lines (chat-state width)
  (let ((safe-width (max 24 width))
        (entries '())
        (frame-count (chat-ui-state-frame-count chat-state))
        (stream-active-p (token-stream-active-p
                          (chat-ui-state-stream-state chat-state))))
    (flet ((record-entry (id text role styled-segments)
             (push (list :id id
                         :text text
                         :role role
                         :styled-segments styled-segments)
                   entries)))
      (maphash
       (lambda (key entry)
         (declare (ignore key))
         (when (listp entry)
           (let* ((tool-name (or (getf entry :tool-name) "<tool>"))
                  (arguments (or (getf entry :arguments) ""))
                  (normalized-arguments (%normalize-inline-text arguments))
                  (status (cond
                            ((getf entry :execution-error) "error")
                            ((getf entry :executed-p) "running")
                            ((getf entry :arguments-complete-p) "args-ready")
                            (t "streaming")))
                  (entry-key (or (getf entry :key) tool-name))
                  (header (format nil "tool: ~A [~A]" tool-name status))
                  (arg-indent 6)
                  (arg-body-width (max 12 (- safe-width (+ 2 arg-indent))))
                  (error-text (getf entry :execution-error))
                  (show-spinner (and stream-active-p
                                     (member status '("streaming" "args-ready" "running")
                                             :test #'string=)))
                  (line-idx 0))
             (record-entry
              (list :stream-tool-call entry-key 0)
              header
              :tool-preview-header
              (list
               (list :text "tool: " :role :tool-preview-keyword :dimp t)
               (list :text tool-name :role :tool-preview-tool)
               (list :text (format nil " [~A]" status)
                     :role (cond
                             ((string= status "error") :tool-preview-status-error)
                             ((string= status "running") :tool-preview-status-running)
                             (t :tool-preview-status-streaming)))))
             (incf line-idx)
             (when (and (stringp error-text) (plusp (length error-text)))
               (let ((err-line (format nil "~A! ~A"
                                       (make-string arg-indent :initial-element #\Space)
                                       (%truncate-inline-text error-text
                                                              (max 8
                                                                   (- safe-width arg-indent 2))))))
                 (record-entry
                  (list :stream-tool-call entry-key line-idx)
                  err-line
                  :tool-preview-error
                  (list (list :text err-line :role :tool-preview-error-text :boldp t)))
                 (incf line-idx)))
             (cond
               ((and (null error-text)
                     (plusp (length normalized-arguments))
                     (member status '("streaming" "args-ready" "running")
                             :test #'string=))
                (let* ((line-text
                         (%truncate-inline-text
                          (format nil " TOOL> [~A] ~A ~A"
                                  status
                                  tool-name
                                  normalized-arguments)
                          safe-width))
                       (styled-segments
                         (list
                          (list :text " TOOL> " :role :tool-preview-args)
                          (list :text (format nil "[~A] " status)
                                :role :tool-preview-args)
                          (list :text tool-name :role :tool-preview-args)
                          (list :text " " :role :tool-preview-args)
                          (list :text normalized-arguments :role :tool-preview-args))))
                  (setf entries (remove (list :stream-tool-call entry-key 0)
                                        entries
                                        :key (lambda (entry) (getf entry :id))
                                        :test #'equal))
                  (record-entry
                   (list :stream-tool-call entry-key 0)
                   line-text
                   :tool-preview-header
                   styled-segments)))
               ((plusp (length normalized-arguments))
                (let ((wrapped-args
                        (or (ptui.text.layout:wrap-by-width
                             normalized-arguments arg-body-width)
                            (list ""))))
                  (loop for arg-line in wrapped-args
                        for arg-line-idx from 0 do
                        (let* ((label (if (zerop arg-line-idx) "  args: " "         "))
                               (line-text (concatenate 'string label arg-line))
                               (styled-segments (list (list :text label :role :tool-preview-args-label :dimp t)
                                                      (list :text arg-line :role :tool-preview-args))))
                          (record-entry (list :stream-tool-call-arg entry-key line-idx)
                                        line-text
                                        :tool-preview-args
                                        styled-segments)
                          (incf line-idx))))))
             (when (and show-spinner
                        (null error-text)
                        (not (plusp (length normalized-arguments))))
               (let ((spinner-line
                       (format nil "~A~A"
                               (make-string arg-indent :initial-element #\Space)
                               (%tool-call-spinner-glyph frame-count))))
                 (record-entry (list :stream-tool-call entry-key line-idx)
                               spinner-line
                               :tool-preview-spinner
                               (list (list :text spinner-line
                                           :role :tool-preview-spinner)))
                 (incf line-idx))))))
       (chat-ui-state-stream-tool-calls chat-state)))
    (setf entries (sort entries #'string<
                        :key (lambda (entry) (princ-to-string (getf entry :id)))))
    (nreverse entries)))

(defun %stream-response-text (chat-state)
  (let ((chunks (chat-ui-state-stream-response-chunks chat-state)))
    (if (null chunks)
        ""
        (with-output-to-string (out)
          (dolist (chunk (nreverse (copy-list chunks)))
            (when (stringp chunk)
              (write-string chunk out)))))))

(defun %prime-finalized-streaming-assistant-cache! (chat-state target-index)
  (let* ((renderer (chat-ui-state-stream-markdown-renderer chat-state))
         (content-width (and (typep renderer 'streaming-markdown-renderer)
                             (streaming-markdown-renderer-width renderer))))
    (when (and (integerp target-index)
               (integerp content-width)
               (> content-width 0))
      (setf (gethash (cons target-index content-width)
                     *%styled-lines-cache*)
            (streaming-markdown-renderer-render-lines renderer
                                                      content-width
                                                      :partialp nil
                                                      :cursor-visible-p nil)))))

(defun %set-streaming-assistant-message (chat-state target-index text &key partialp)
  (let ((messages (chat-ui-state-messages chat-state)))
    (when (and (integerp target-index)
               (>= target-index 0)
               (< target-index (length messages)))
      (let* ((existing (nth target-index messages))
             (assistant-message (make-chat-message "assistant"
                                                   (or text "")
                                                   :partial partialp))
             (existing-tool-calls (and existing
                                       (typep existing 'pseudopod:message)
                                       (pseudopod:message-tool-calls existing))))
        (when existing-tool-calls
          (setf (pseudopod:message-tool-calls assistant-message)
                existing-tool-calls))
        (%replace-message-at-index! messages target-index assistant-message)
        (%sync-chat-context-usage-replacement! chat-state existing assistant-message))
      (when (chat-ui-state-stream-scroll-follow-p chat-state)
        (setf (chat-ui-state-message-scrollback-lines chat-state) 0))
      t)))

(defun %materialize-streaming-assistant-message! (chat-state &key partialp)
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (target-index (token-stream-state-target-message-index stream-state)))
    (when (integerp target-index)
      (%set-streaming-assistant-message chat-state
                                        target-index
                                        (%stream-response-text chat-state)
                                        :partialp partialp))))

(defun %append-streaming-assistant-chunk (chat-state chunk)
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (target-index (token-stream-state-target-message-index stream-state))
         (messages (chat-ui-state-messages chat-state)))
    (when (and (integerp target-index)
               (>= target-index 0)
               (< target-index (length messages)))
      (when (and (stringp chunk)
                 (plusp (length chunk)))
        (push chunk (chat-ui-state-stream-response-chunks chat-state)))
      (when (chat-ui-state-stream-scroll-follow-p chat-state)
        (setf (chat-ui-state-message-scrollback-lines chat-state) 0))
      (when (stringp chunk)
        (streaming-markdown-renderer-append-chunk
         (chat-ui-state-stream-markdown-renderer chat-state)
         chunk)))))

(defun %finalize-streaming-assistant-message (chat-state &key partialp)
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (target-index (token-stream-state-target-message-index stream-state))
         (messages (chat-ui-state-messages chat-state)))
    (when (and (integerp target-index)
               (>= target-index 0)
               (< target-index (length messages)))
      (let ((text (%stream-response-text chat-state)))
        (%set-streaming-assistant-message chat-state target-index text :partialp partialp)
        (unless partialp
          (%prime-finalized-streaming-assistant-cache! chat-state target-index))
        (when (not partialp)
          (%record-chat-stream-event! chat-state '(:kind :answer-finalized)))
        (let* ((updated-messages (chat-ui-state-messages chat-state))
               (updated-message (and (integerp target-index)
                                     (>= target-index 0)
                                     (< target-index (length updated-messages))
                                     (nth target-index updated-messages))))
          (when (pseudopod:message-p updated-message)
            (conversation-state-update-entry (%ensure-chat-conversation-state chat-state)
                                             target-index
                                             updated-message)))))
    (unless partialp
      (%maybe-trim-demo-transcript! chat-state :target-index target-index))
    (setf (chat-ui-state-stream-response-chunks chat-state) '())
    (setf (chat-ui-state-stream-scroll-follow-p chat-state) t)))

(defun %stream-target-assistant-response (chat-state)
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (target-index (token-stream-state-target-message-index stream-state))
         (messages (chat-ui-state-messages chat-state)))
    (and (integerp target-index)
         (>= target-index 0)
         (< target-index (length messages))
         (nth target-index messages))))

(defun %styled-segments->text (segments)
  (with-output-to-string (out)
    (dolist (segment segments)
      (write-string (if (compact-segment-p segment)
                        (compact-segment-text segment)
                        (or (getf segment :text) ""))
                    out))))

(defvar *%styled-lines-cache* (make-hash-table :test #'equal)
  "Cache of rendered styled lines for completed assistant messages.
Key: (message-index . content-width), Value: styled-lines list.")

(defvar *%styled-lines-cache-generation* 0
  "Incremented when the message list changes, invalidating the cache.")

(defvar *%message-wrap-cache* (make-hash-table :test #'equal)
  "Cache of wrapped message lines to avoid re-wrapping every frame.
Key: (message-content-hash . width), Value: wrapped-lines list.")

(defvar *%message-wrap-cache-size* 0
  "Track cache size to prevent unbounded growth.")

(defparameter +max-message-wrap-cache-entries+ 500
  "Maximum number of entries in the message wrap cache.")

(defun %invalidate-styled-lines-cache ()
  "Call when messages are added or modified."
  (incf *%styled-lines-cache-generation*)
  (clrhash *%styled-lines-cache*)
  ;; Also clear wrap cache when messages change
  (clrhash *%message-wrap-cache*)
  (setf *%message-wrap-cache-size* 0)
  ;; Clear per-message entry cache
  (when (boundp '*%message-entry-cache*)
    (%invalidate-message-entry-cache)))

(defun %message-content-hash (message)
  "Generate a simple hash for message content caching."
  (let ((content (%message-content->text message))
        (role (pseudopod:message-role message)))
    (sxhash (concatenate 'string (or role "") "|" (or content "")))))

(defun %get-cached-wrapped-lines (message width)
  "Get cached wrapped lines for a message, or nil if not cached."
  (when (> *%message-wrap-cache-size* +max-message-wrap-cache-entries+)
    ;; Simple LRU eviction: clear half the cache when it gets too big
    (clrhash *%message-wrap-cache*)
    (setf *%message-wrap-cache-size* 0)
    (return-from %get-cached-wrapped-lines nil))
  (let* ((hash (%message-content-hash message))
         (key (cons hash width)))
    (gethash key *%message-wrap-cache*)))

(defun %set-cached-wrapped-lines (message width lines)
  "Cache wrapped lines for a message."
  (when (< *%message-wrap-cache-size* +max-message-wrap-cache-entries+)
    (let* ((hash (%message-content-hash message))
           (key (cons hash width)))
      (unless (gethash key *%message-wrap-cache*)
        (incf *%message-wrap-cache-size*))
      (setf (gethash key *%message-wrap-cache*) lines))))

;;; --- Per-message line-entry cache ---
;;; During streaming, %message-line-entries is called every render.  Completed
;;; messages produce identical entries each time, so caching them avoids O(n)
;;; allocation per frame where n = total lines across all completed messages.
(defvar *%message-entry-cache* (make-hash-table :test #'equal))

(defun %invalidate-message-entry-cache ()
  (clrhash *%message-entry-cache*))

(defun %make-message-entry-block (entries)
  (cons (length entries) entries))

(defun %message-entry-block-count (block)
  (car block))

(defun %message-entry-block-entries (block)
  (cdr block))

(defun %assistant-message-styled-lines (chat-state message message-index content-width)
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (stream-active-p (token-stream-active-p stream-state))
         (target-index (token-stream-state-target-message-index stream-state))
         (partialp (not (null (pseudopod:message-partial message))))
         (streaming-target-p
           (and partialp
                stream-active-p
                (integerp target-index)
                (= target-index message-index)))
         (cursor-visible-p
           (and streaming-target-p
                (stream-cursor-visible-p stream-state))))
    (if streaming-target-p
        ;; Active streaming — cache until content changes
        (let* ((renderer (chat-ui-state-stream-markdown-renderer chat-state))
               (pending-len (length (streaming-markdown-renderer-pending-line renderer)))
               (line-count (length (streaming-markdown-renderer-wrapped-lines renderer)))
               (cache-key (list :streaming message-index content-width
                                line-count pending-len cursor-visible-p)))
          (or (gethash cache-key *%styled-lines-cache*)
              (let ((result (streaming-markdown-renderer-render-lines
                             renderer content-width
                             :partialp partialp
                             :cursor-visible-p cursor-visible-p)))
                ;; Clear old streaming entries for this message before caching new one
                (let ((stale '()))
                  (maphash (lambda (k v)
                             (declare (ignore v))
                             (when (and (consp k) (eq (car k) :streaming)
                                        (eql (second k) message-index)
                                        (not (equal k cache-key)))
                               (push k stale)))
                           *%styled-lines-cache*)
                  (dolist (k stale) (remhash k *%styled-lines-cache*)))
                (setf (gethash cache-key *%styled-lines-cache*) result))))
        ;; Completed message — cache the styled lines
        (let ((cache-key (cons message-index content-width)))
          (or (gethash cache-key *%styled-lines-cache*)
              (setf (gethash cache-key *%styled-lines-cache*)
                    (stream-markdown-styled-lines (%message-content->text message)
                                                  content-width
                                                  :partialp partialp
                                                  :cursor-visible-p cursor-visible-p)))))))

(defun %lerp-rgb (from-r from-g from-b to-r to-g to-b t-val)
  "Linear interpolation between two RGB colors. Returns 3 values (r g b)."
  (let ((clamped (max 0.0 (min 1.0 (coerce t-val 'single-float)))))
    (values (round (+ from-r (* clamped (- to-r from-r))))
            (round (+ from-g (* clamped (- to-g from-g))))
            (round (+ from-b (* clamped (- to-b from-b)))))))

(defun %gradient-styled-segments (text from-rgb to-rgb &key boldp)
  "Return a list of styled segments with per-character gradient color.
FROM-RGB and TO-RGB are lists of (r g b)."
  (let* ((len (length text))
         (segments '()))
    (loop for i from 0 below len
          for ch = (char text i)
          do (let ((t-val (if (<= len 1) 0.0 (/ (float i) (float (1- len))))))
               (multiple-value-bind (r g b)
                   (%lerp-rgb (first from-rgb) (second from-rgb) (third from-rgb)
                              (first to-rgb) (second to-rgb) (third to-rgb)
                              t-val)
                 (push (list (string ch)
                             (%chat-template-cell
                              :fg (ptui.core.color:make-color-rgb r g b)
                              :boldp boldp))
                       segments))))
    (nreverse segments)))

;;; ---------------------------------------------------------------------------
;;; Welcome screen ASCII art with truecolor gradient
;;; ---------------------------------------------------------------------------

;; Sky blue to orange gradient colors (RGB)
(defparameter +welcome-gradient-start+ '(135 206 250))  ; Light sky blue
(defparameter +welcome-gradient-end+ '(255 165 0))      ; Orange

(defun %lerp-color (start end t-val)
  "Linearly interpolate between two RGB colors."
  (flet ((lerp-channel (a b)
           (round (+ a (* (- b a) t-val)))))
    (list (lerp-channel (first start) (first end))
          (lerp-channel (second start) (second end))
          (lerp-channel (third start) (third end)))))

(defun %line-gradient-segments (line start-rgb end-rgb &key bg-rgb)
  "Create styled segments for a line with horizontal gradient coloring.
If BG-RGB is provided, uses gradient background fills."
  (let* ((len (length line))
         (segments '()))
    (loop for i from 0 below len
          for ch = (char line i)
          for t-val = (if (<= len 1) 0.0 (/ (float i) (float (1- len))))
          for rgb = (%lerp-color start-rgb end-rgb t-val)
          for bg = (when bg-rgb (%lerp-color bg-rgb bg-rgb t-val))
          do (push (list (string ch)
                         (%chat-template-cell
                          :fg (ptui.core.color:make-color-rgb
                               (first rgb) (second rgb) (third rgb))
                          :bg (when bg
                                (ptui.core.color:make-color-rgb
                                 (first bg) (second bg) (third bg)))
                          :boldp t))
                   segments))
    (nreverse segments)))

(defun %welcome-filled-block-line (width start-rgb end-rgb char)
  "Create a line filled entirely with a character using gradient colors."
  (let ((line (make-string width :initial-element char)))
    (%line-gradient-segments line start-rgb end-rgb)))

(defun %welcome-screen-entries (safe-width)
  "Return snapshot-stable empty-state content when there are no messages.
Displays a truecolor gradient ASCII art logo for amoebum with color fills."
  (declare (ignore safe-width))
  (let* ((entries '())
         (art-width 68)
         ;; Gradient positions for visual effect
         (light-start '(100 180 255))    ; Deeper sky blue
         (light-end '(255 200 100))      ; Light orange
         (deep-start '(70 130 200))      ; Deep blue
         (deep-end '(255 140 50)))       ; Deep orange

    ;; Top decorative border with half blocks
    (push (list :id :chat-welcome-top-border
                :text (make-string art-width :initial-element #\━)
                :role :system
                :styled-segments (%welcome-filled-block-line
                                  art-width deep-start deep-end #\━))
          entries)

    ;; Amoeba-inspired filled shapes with gradient backgrounds
    ;; AMOEBUM text with filled blocks inside letters
    (let ((text-lines
            '(" █████╗ ███╗   ███╗ ██████╗ ███████╗██████╗ ██╗   ██╗███╗   ███╗ "
              "██╔══██╗████╗ ████║██╔═══██╗██╔════╝██╔══██╗██║   ██║████╗ ████║ "
              "███████║██╔████╔██║██║   ██║█████╗  ██████╔╝██║   ██║██╔████╔██║ "
              "██╔══██║██║╚██╔╝██║██║   ██║██╔══╝  ██╔══██╗██║   ██║██║╚██╔╝██║ "
              "██║  ██║██║ ╚═╝ ██║╚██████╔╝███████╗██████╔╝╚██████╔╝██║ ╚═╝ ██║ "
              "╚═╝  ╚═╝╚═╝     ╚═╝ ╚═════╝ ╚══════╝╚═════╝  ╚═════╝ ╚═╝     ╚═╝ ")))
      (loop for line in text-lines
            for idx from 0
            do (push (list :id (list :chat-welcome-text idx)
                           :text line
                           :role :system
                           :styled-segments (%line-gradient-segments
                                             line
                                             +welcome-gradient-start+
                                             +welcome-gradient-end+))
                     entries)))

    ;; Bottom border
    (push (list :id :chat-welcome-bottom-border
                :text (make-string art-width :initial-element #\━)
                :role :system
                :styled-segments (%welcome-filled-block-line
                                  art-width deep-end deep-start #\━))
          entries)

    ;; Tagline with subtle styling
    (push (list :id :chat-welcome-tagline
                :text ""
                :role :meta)
          entries)
    (push (list :id :chat-welcome-tagline-text
                :text "              🧬 Single-celled coding assistant 🧬"
                :role :system
                :styled-segments
                (list (list "              " (%chat-template-cell))
                      (list "🧬" (%chat-template-cell
                                 :fg (ptui.core.color:make-color-rgb 135 206 250)
                                 :boldp t))
                      (list " Single-celled coding assistant "
                            (%chat-template-cell))
                      (list "🧬" (%chat-template-cell
                                 :fg (ptui.core.color:make-color-rgb 255 165 0)
                                 :boldp t))))
          entries)

    ;; Helper text
    (push (list :id :chat-empty-fallback
                :text ""
                :role :meta)
          entries)
    (push (list :id :chat-empty-hint
                :text ""
                :role :meta)
          entries)
    (push (list :id :chat-empty-hint-text
                :text "              Type below and press Enter to start"
                :role :meta)
          entries)

    (nreverse entries)))

(defun %with-left-gutter (text)
  (concatenate 'string " " (or text "")))

(defparameter +max-ui-render-messages+ 100
  "Maximum number of messages to render in the UI.
   Older messages are skipped to maintain performance.
   The full history is still kept for LLM context.")

(defun %chat-ui-render-message-limit (chat-state)
  (if (chat-ui-state-demo-mode-p chat-state)
      +demo-max-ui-render-messages+
      +max-ui-render-messages+))

(defun %maybe-trim-demo-transcript! (chat-state &key keep-last-messages target-index)
  (let* ((message-limit (or keep-last-messages
                            (%chat-ui-render-message-limit chat-state)))
         (messages (chat-ui-state-messages chat-state))
         (total-messages (length messages)))
    (when (and (chat-ui-state-demo-mode-p chat-state)
               (> total-messages message-limit))
      (let* ((keep-count (max 1 message-limit))
             (trim-count (- total-messages keep-count))
             (trimmed-messages (copy-list (nthcdr trim-count messages)))
             (conversation (chat-ui-state-conversation chat-state))
             (shifted-target-index
               (and (integerp target-index)
                    (max 0 (- target-index trim-count)))))
        (setf (chat-ui-state-messages chat-state) trimmed-messages
              (chat-ui-state-message-scrollback-lines chat-state) 0
              (chat-ui-state-max-message-scrollback-lines chat-state) 0)
        (when (typep conversation 'conversation-state)
          (setf (conversation-state-entries conversation)
                (copy-list (nthcdr trim-count
                                   (conversation-state-entries conversation)))))
        (%invalidate-styled-lines-cache)
        (%sync-chat-context-usage! chat-state :allow-auto-compress-p nil)
        (when shifted-target-index
          (setf (token-stream-state-target-message-index
                 (chat-ui-state-stream-state chat-state))
                shifted-target-index))
        shifted-target-index))))

(defun %message-line-entries-for-one (chat-state message index safe-width is-last-p)
  "Generate line entries for a single message in display order."
  (declare (ignore is-last-p))
  (let ((entries '()))
    (let* ((role (%normalize-chat-role (pseudopod:message-role message)))
           (role-key (intern (string-upcase role) :keyword))
           (prefix (let ((badge (chat-role-prefix role)))
                     (if (plusp (length badge))
                         (format nil " [~A] " badge)
                         " > ")))
           (prefix-width (ptui.text.width:string-width prefix))
           (content-width (max 1 (- safe-width prefix-width)))
           (indent (make-string prefix-width :initial-element #\Space))
           (label-role role-key))
      (if (string= role "assistant")
          (let ((styled-lines
                  (%assistant-message-styled-lines
                   chat-state message index content-width)))
            (loop for styled-line in styled-lines
                  for line-index from 0
                  do
                     (let* ((prefix-text (if (zerop line-index) prefix indent))
                            (prefix-segment
                              (cons prefix-text
                                    (intern-style
                                     (if (zerop line-index) label-role role-key)
                                     :boldp (and (zerop line-index)
                                                 (not (string= role "system"))))))
                            (content-segments
                              (or styled-line
                                  (list (cons "" (intern-style role-key)))))
                            (segments (append (list prefix-segment)
                                              content-segments)))
                       (push (list :id (list :chat-message index line-index)
                                   :text (%styled-segments->text segments)
                                   :role role-key
                                   :styled-segments segments)
                             entries))))
          ;; Use cache for wrapped lines to avoid re-wrapping every frame
          (let* ((body (%message-display-text message))
                 (cached (%get-cached-wrapped-lines message content-width))
                 (wrapped (or cached
                              (let ((lines (ptui.text.layout:wrap-by-width body content-width)))
                                (setf lines (if (null lines) (list "") lines))
                                (%set-cached-wrapped-lines message content-width lines)
                                lines))))
            (loop for line in wrapped
                  for line-index from 0
                  do
                     (let* ((prefix-part (if (zerop line-index) prefix indent))
                            (line-with-gutter (concatenate 'string prefix-part line))
                            (prefix-segment
                              (cons prefix-part
                                    (intern-style
                                     (if (zerop line-index) label-role role-key)
                                     :boldp (and (zerop line-index)
                                                 (not (string= role "system"))))))
                            (body-seg (cons line (intern-style role-key)))
                            (segments (list prefix-segment body-seg)))
                       (push (list :id (list :chat-message index line-index)
                                   :text line-with-gutter
                                   :role role-key
                                   :styled-segments segments)
                             entries)))))
    (nreverse entries))))

(defun %message-entry-cacheable-p (chat-state message)
  "A message is cacheable if it is NOT the active streaming target."
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (partialp (pseudopod:message-partial message)))
    (not (and partialp (token-stream-active-p stream-state)))))

(defun %get-cached-message-entries (index width)
  (when (boundp '*%message-entry-cache*)
    (gethash (cons index width)
             *%message-entry-cache*)))

(defun %set-cached-message-entries (index width block)
  (when (boundp '*%message-entry-cache*)
    (setf (gethash (cons index width)
                   *%message-entry-cache*)
          block)))

(defun %message-gap-block (index)
  (%make-message-entry-block
   (list (list :id (list :chat-gap index)
               :text ""
               :role :meta))))

(defun %message-preview-entry (preview)
  (let ((entry (copy-list preview)))
    (setf (getf entry :text)
          (%with-left-gutter (getf preview :text "")))
    entry))

(defun %message-entry-blocks (chat-state messages width)
  (let* ((safe-width (max 1 (1- width)))
         (message-limit (%chat-ui-render-message-limit chat-state))
         (start-index (max 0 (- (length messages) message-limit)))
         (total-messages (length messages))
         (blocks '())
         (total-lines 0))
    (flet ((push-block (block)
             (push block blocks)
             (incf total-lines (%message-entry-block-count block))))
      (when (> total-messages message-limit)
        (let* ((skipped (- total-messages message-limit))
               (entries (list (list :id :chat-skipped-messages-indicator
                                    :text (format nil " ... (~D older messages not shown) ..." skipped)
                                    :role :system)))
               (block (%make-message-entry-block entries)))
          (push-block block)))
      (loop for index from start-index below total-messages
            for message in (nthcdr start-index messages)
            do
               (let* ((is-last-p (= index (1- total-messages)))
                      (cacheable (%message-entry-cacheable-p chat-state message))
                      (cached-block (and cacheable
                                         (%get-cached-message-entries
                                          index
                                          safe-width)))
                      (msg-block
                        (or cached-block
                            (let* ((fresh-entries (%message-line-entries-for-one
                                                   chat-state message index safe-width is-last-p))
                                   (fresh-block (%make-message-entry-block fresh-entries)))
                              (when cacheable
                                (%set-cached-message-entries
                                 index
                                 safe-width
                                 fresh-block))
                              fresh-block))))
                 (push-block msg-block)
                 (unless is-last-p
                   (push-block (%message-gap-block index)))))
      (let ((preview-lines (%stream-tool-call-preview-lines chat-state safe-width)))
        (when preview-lines
          (when (> total-lines 0)
            (let ((gap-block (%make-message-entry-block
                              (list (list :id :chat-stream-tool-gap :text "" :role :meta)))))
              (push-block gap-block)))
          (let* ((preview-entries (mapcar #'%message-preview-entry preview-lines))
                 (preview-block (%make-message-entry-block preview-entries)))
            (push-block preview-block))))
      (unless blocks
        (let* ((welcome-entries (%welcome-screen-entries safe-width))
               (welcome-block (%make-message-entry-block welcome-entries)))
          (push-block welcome-block)))
      (values (nreverse blocks) total-lines))))

(defun %entry-list-slice (entries start count)
  (let ((tail (nthcdr start entries))
        (slice '()))
    (loop repeat count
          while tail
          do (push (car tail) slice)
             (setf tail (cdr tail)))
    (nreverse slice)))

(defun %message-line-window (chat-state messages width viewport-height scrollback-lines)
  (multiple-value-bind (blocks total-lines)
      (%message-entry-blocks chat-state messages width)
    (multiple-value-bind (offset new-scrollback max-scrollback)
        (%compute-scroll-offset total-lines viewport-height scrollback-lines)
      (let ((window-end (+ offset viewport-height))
            (cursor 0)
            (visible '()))
        (dolist (block blocks)
          (let* ((block-count (%message-entry-block-count block))
                 (block-end (+ cursor block-count))
                 (slice-start (max offset cursor))
                 (slice-end (min window-end block-end)))
            (when (< slice-start slice-end)
              (setf visible
                    (nconc visible
                           (%entry-list-slice (%message-entry-block-entries block)
                                              (- slice-start cursor)
                                              (- slice-end slice-start)))))
            (setf cursor block-end)
            (when (>= cursor window-end)
              (return))))
        (values visible total-lines offset new-scrollback max-scrollback)))))

(defun %message-line-entries (chat-state messages width)
  (multiple-value-bind (blocks total-lines)
      (%message-entry-blocks chat-state messages width)
    (declare (ignore total-lines))
    (let ((entries '()))
      (dolist (block blocks entries)
        (setf entries
              (nconc entries
                     (%entry-list-slice (%message-entry-block-entries block)
                                        0
                                        (%message-entry-block-count block))))))))

(defun %chat-text-widget (text id role &key styled-segments)
  (ptui.ui.elements:make-element
   :text
   :id id
   :props (list :text text :role role :styled-segments styled-segments)
   :children '()))

(defun %compute-scroll-offset (total-lines viewport-height scrollback-lines)
  (let* ((max-scrollback (max 0 (- total-lines viewport-height)))
         (bounded-scrollback (max 0 (min max-scrollback scrollback-lines)))
         (offset (- max-scrollback bounded-scrollback)))
    (values offset bounded-scrollback max-scrollback)))

(defun %handle-approval-ui-error! (chat-state stage condition)
  (handler-case
      (let ((pending-tool nil)
            (pending-decision-id nil)
            (crash-log-text (or (ignore-errors (namestring (crash-log-path)))
                                "the crash log"))
            (message-text nil))
        (ignore-errors
          (bt:with-lock-held (*pending-approval-lock*)
            (let ((pending *pending-approval*))
              (setf pending-tool (and pending
                                      (pending-approval-tool-name pending))
                    pending-decision-id (and pending
                                             (pending-approval-decision-id pending))))))
        (setf message-text
              (format nil "Approval dialog failed during ~A. The pending tool request was denied safely. See ~A for details."
                      stage
                      crash-log-text))
        (ignore-errors
          (log-runtime-condition condition
                                 :kind "approval-ui-error"
                                 :source :chat-ui
                                 :message (format nil "Approval dialog failed during ~A." stage)
                                 :details (list :stage stage
                                                :pending-tool pending-tool
                                                :pending-decision-id pending-decision-id)
                                 :path (crash-log-path)))
        (handler-case
            (chat-ui-add-message chat-state "system" message-text)
          (error ()
            (setf (chat-ui-state-messages chat-state)
                  (append (chat-ui-state-messages chat-state)
                          (list (make-chat-message "system" message-text))))))
        (ignore-errors (submit-pending-approval :deny :source :ui-error))
        (ignore-errors
          (approval-dialog-deactivate!
           (chat-ui-state-approval-dialog-state chat-state)))
        nil)
    (error ()
      (ignore-errors (submit-pending-approval :deny :source :ui-error))
      (ignore-errors
        (approval-dialog-deactivate!
         (chat-ui-state-approval-dialog-state chat-state)))
      nil)))

(defun %chat-approval-dialog-widget (chat-state approval-state)
  (handler-case
      (make-approval-dialog-widget
       (list :tool-name (approval-dialog-state-tool-name approval-state)
             :command (approval-dialog-state-command approval-state)
             :path (approval-dialog-state-path approval-state)
             :reason (approval-dialog-state-reason approval-state)
             :selected-option (approval-dialog-state-selected-option approval-state)))
    (error (condition)
      (%handle-approval-ui-error! chat-state :render condition)
      (%chat-text-widget
       "Approval dialog unavailable. Pending tool request denied safely."
       :approval-dialog-error
       :error))))

(defun %approval-recovery-active-p (chat-state)
  (or (approval-dialog-state-active-p
       (chat-ui-state-approval-dialog-state chat-state))
      (bt:with-lock-held (*pending-approval-lock*)
        (not (null *pending-approval*)))))

(defun %sync-pending-approval-dialog! (chat-state)
  "Poll *pending-approval* and activate the dialog if a new approval is waiting."
  (handler-case
      (let ((dialog (chat-ui-state-approval-dialog-state chat-state)))
        (bt:with-lock-held (*pending-approval-lock*)
          (let ((pa *pending-approval*))
            (cond
              ;; A pending approval exists but dialog is not active yet — activate it
              ((and pa (not (approval-dialog-state-active-p dialog)))
               (approval-dialog-activate! dialog
                                          (pending-approval-tool-name pa)
                                          :command (pending-approval-command pa)
                                          :path (pending-approval-path pa)
                                          :reason (pending-approval-reason pa)
                                          :decision-id (pending-approval-decision-id pa)))
              ;; No pending approval but dialog is still active — deactivate
              ((and (null pa) (approval-dialog-state-active-p dialog))
               (approval-dialog-deactivate! dialog))))))
    (error (condition)
      (%handle-approval-ui-error! chat-state :sync condition))))

(defvar *%style-resolve-cache* (make-hash-table :test #'eql)
  "Per-frame cache: style-id → resolved cell. Cleared each frame.")

(defun %resolve-style-id-to-cell (style-id)
  "Resolve a style-id to a ptui cell, caching per frame."
  (or (gethash style-id *%style-resolve-cache*)
      (let* ((entry (lookup-style style-id))
             (cell (chat-role-cell (style-entry-role entry)
                                   :boldp (style-entry-boldp entry)
                                   :italicp (style-entry-italicp entry)
                                   :underlinep (style-entry-underlinep entry)
                                   :invertp (style-entry-invertp entry)
                                   :dimp (style-entry-dimp entry)
                                   :strikep (style-entry-strikep entry))))
        (setf (gethash style-id *%style-resolve-cache*) cell)
        cell)))

(defun %styled-segment->render-segment (segment default-role)
  (cond
    ;; Compact segment: (text . style-id)
    ((compact-segment-p segment)
     (let ((text (compact-segment-text segment)))
       (when (plusp (length text))
         (list text (%resolve-style-id-to-cell (compact-segment-style-id segment))))))
    ((and (consp segment)
          (stringp (first segment))
          (typep (ignore-errors (second segment)) 'ptui.core.types:cell))
     (list (first segment) (second segment)))
    ((and (listp segment)
          (keywordp (first segment)))
     (let* ((text (or (getf segment :text) ""))
            (cell (getf segment :cell)))
       (when (plusp (length text))
         (list text
               (if (typep cell 'ptui.core.types:cell)
                   cell
                   (chat-role-cell (or (getf segment :role) default-role :meta)
                                   :boldp (getf segment :boldp)
                                   :italicp (getf segment :italicp)
                                   :underlinep (getf segment :underlinep)
                                   :invertp (getf segment :invertp)
                                   :dimp (getf segment :dimp)
                                   :strikep (getf segment :strikep)))))))
    ((and (consp segment)
          (stringp (car segment)))
     (let* ((role (if (listp (cdr segment))
                      (second segment)
                      (cdr segment)))
            (text (car segment)))
       (when (plusp (length text))
         (list text (chat-role-cell (or role default-role :meta))))))
    ((stringp segment)
     (when (plusp (length segment))
       (list segment (chat-role-cell (or default-role :meta)))))
    (t
     nil)))

(defun %normalize-tree-styled-segments! (node)
  (when (eq (ptui.ui.elements:ui-element-type node) :text)
    (let* ((props (copy-list (ptui.ui.elements:ui-element-props node)))
           (segments (getf props :styled-segments))
           (default-role (getf props :role :meta)))
      (when segments
        (let* ((segment-list (if (listp segments)
                                 segments
                                 (list segments)))
               (normalized
                 (remove nil
                         (loop for segment in segment-list
                               collect (%styled-segment->render-segment
                                        segment
                                        default-role)))))
          (when normalized
            (setf (getf props :styled-segments) normalized
                  (ptui.ui.elements:ui-element-props node) props)))))
    )
  (dolist (child (ptui.ui.elements:ui-element-children node))
    (%normalize-tree-styled-segments! child))
  node)

(defun %chat-plan-mode-enabled-p ()
  (not (null (cfg :plan-mode))))

(defun %chat-plan-execution-surface-active-p (&optional (execution-state (current-plan-execution-state)))
  (and (plan-execution-state-p execution-state)
       (let ((run-id (plan-execution-state-run-id execution-state))
             (status (plan-execution-state-status execution-state))
             (continuity (plan-execution-state-continuity-output execution-state))
             (steps (plan-execution-state-steps execution-state)))
         (or (and (stringp run-id)
                  (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) run-id))))
             (and (keywordp status)
                  (not (eq status :idle)))
             continuity
             steps))))

(defun %chat-plan-workspace-visible-p (plan-state execution-state)
  (and (plan-mode-state-p plan-state)
       (plan-mode-state-steps plan-state)
       (or (%chat-plan-mode-enabled-p)
           (%chat-plan-execution-surface-active-p execution-state))))

(defun %chat-plan-presentation-safe-string (value &optional (fallback ""))
  (cond
    ((and (stringp value)
          (plusp (length value)))
     value)
    ((null value)
     fallback)
    (t
     (princ-to-string value))))

(defun %chat-plan-inline-code-spans (text)
  (let* ((source (%chat-plan-presentation-safe-string text ""))
         (length (length source))
         (index 0)
         (spans '()))
    (loop while (< index length) do
      (let ((start (position #\` source :start index)))
        (if (null start)
            (setf index length)
            (let ((end (position #\` source :start (1+ start))))
              (if (null end)
                  (setf index length)
                  (let ((snippet
                          (string-trim '(#\Space #\Tab #\Newline #\Return)
                                       (subseq source (1+ start) end))))
                    (when (plusp (length snippet))
                      (push snippet spans))
                    (setf index (1+ end))))))))
    (nreverse spans)))

(defun %chat-plan-leading-token (text)
  (let* ((trimmed
           (string-trim '(#\Space #\Tab #\Newline #\Return)
                        (%chat-plan-presentation-safe-string text "")))
         (length (length trimmed)))
    (if (zerop length)
        ""
        (let ((end
                (or (position-if #'%whitespace-char-p trimmed)
                    length)))
          (string-downcase (subseq trimmed 0 end))))))

(defun %chat-plan-commandish-p (text)
  (let* ((trimmed
           (string-trim '(#\Space #\Tab #\Newline #\Return)
                        (%chat-plan-presentation-safe-string text "")))
         (length (length trimmed))
         (token (%chat-plan-leading-token trimmed)))
    (and (plusp length)
         (or (member token +chat-plan-command-heads+ :test #'string=)
             (and (>= length 2)
                  (string= (subseq trimmed 0 2) "./"))
             (and (>= length 2)
                  (string= (subseq trimmed 0 2) "~/"))
             (char= (char trimmed 0) #\/)
             (search "&&" trimmed :test #'char=)
             (search "||" trimmed :test #'char=)
             (search "|" trimmed :test #'char=)
             (search ";" trimmed :test #'char=)
             (search ">" trimmed :test #'char=)
             (search "<" trimmed :test #'char=)
             (and (>= length 3)
                  (string= (subseq trimmed (- length 3)) ".sh"))))))

(defun %chat-plan-step-command-previews (step)
  (check-type step plan-step)
  (let* ((description (%chat-plan-presentation-safe-string
                       (plan-step-description step)
                       ""))
         (inline-spans (%chat-plan-inline-code-spans description)))
    (remove-duplicates
     (loop for span in inline-spans
           for normalized = (string-trim '(#\Space #\Tab #\Newline #\Return)
                                         (%normalize-inline-text span))
           when (%chat-plan-commandish-p normalized)
             collect normalized)
     :test #'string=)))

(defun %chat-plan-sorted-steps (plan-state)
  (sort (copy-list (or (plan-mode-state-steps plan-state) '()))
        #'<
        :key #'plan-step-index))

(defun %chat-plan-visible-steps (plan-state)
  (let* ((sorted-steps (%chat-plan-sorted-steps plan-state))
         (visible-count (min (length sorted-steps) +chat-plan-presentation-max-steps+)))
    (subseq sorted-steps 0 visible-count)))

(defun %chat-plan-normalize-path-list (paths)
  (remove nil
          (loop for path in (or paths '())
                for text = (%chat-plan-presentation-safe-string path "")
                for trimmed = (string-trim '(#\Space #\Tab #\Newline #\Return) text)
                when (plusp (length trimmed))
                  collect trimmed)))

(defun %chat-plan-rationale-snippet (step)
  (check-type step plan-step)
  (%truncate-inline-text
   (%chat-plan-presentation-safe-string (plan-step-description step) "")
   +chat-plan-rationale-snippet-chars+))

(defun %chat-plan-step-by-index (steps step-index)
  (when (integerp step-index)
    (find step-index (or steps '()) :key #'plan-step-index :test #'=)))

(defun %chat-plan-resolve-selected-step-index (chat-state plan-state visible-steps)
  (let* ((visible-indexes
           (loop for step in (or visible-steps '())
                 for index = (plan-step-index step)
                 when (integerp index)
                   collect index))
         (approved-visible-indexes
           (loop for index in (plan-mode-state-approved-step-indexes plan-state)
                 when (member index visible-indexes :test #'=)
                   collect index))
         (current-selection (chat-ui-state-plan-selected-step-index chat-state))
         (resolved-selection
           (cond
             ((and (integerp current-selection)
                   (member current-selection visible-indexes :test #'=))
              current-selection)
             (approved-visible-indexes
              (first approved-visible-indexes))
             (visible-indexes
              (first visible-indexes))
             (t
              nil))))
    (setf (chat-ui-state-plan-selected-step-index chat-state) resolved-selection)
    resolved-selection))

(defun %chat-template-cell (&key (fg :default) (bg :default) (boldp nil))
  (ptui.core.types:make-cell
   " "
   fg
   bg
   (ptui.core.types:make-attrs :boldp boldp)))

(defun %chat-cell-with-attrs (cell
                              &key
                                boldp
                                italicp
                                underlinep
                                invertp
                                dimp
                                strikep)
  (let ((attrs (ptui.core.types:cell-attrs cell)))
    (ptui.core.types:make-cell
     (ptui.core.types:cell-glyph cell)
     (ptui.core.types:cell-fg cell)
     (ptui.core.types:cell-bg cell)
     (ptui.core.types:make-attrs
      :boldp (or (ptui.core.types:attrs-boldp attrs) (not (null boldp)))
      :italicp (or (ptui.core.types:attrs-italicp attrs) (not (null italicp)))
      :underlinep (or (ptui.core.types:attrs-underlinep attrs) (not (null underlinep)))
      :invertp (or (ptui.core.types:attrs-invertp attrs) (not (null invertp)))
      :dimp (or (ptui.core.types:attrs-dimp attrs) (not (null dimp)))
      :strikep (or (ptui.core.types:attrs-strikep attrs) (not (null strikep)))))))

(defun chat-role-cell (role
                       &key
                         (focusp nil)
                         boldp
                         italicp
                         underlinep
                         invertp
                         dimp
                         strikep)
  (let* ((role-key (intern (string-upcase (princ-to-string role)) :keyword))
         (theme ptui.core.theme:*active-theme*)
         (base (if theme
                   (ptui.core.theme:theme-role-cell theme role-key)
                   ;; fallback when no theme is active
                   (%chat-template-cell :fg (ptui.core.color:make-color-rgb 175 175 175))))
         (styled (%chat-cell-with-attrs base
                                        :boldp boldp
                                        :italicp italicp
                                        :underlinep underlinep
                                        :invertp invertp
                                        :dimp dimp
                                        :strikep strikep)))
    (if focusp
        (%chat-cell-with-attrs styled :boldp t :invertp t)
        styled)))

(defun %styled-text-segments (segments &key (focusp nil))
  (let ((result '()))
    (dolist (segment segments)
      (let* ((plist-segment (and (listp segment)
                                 (keywordp (first segment))))
             (text
               (cond
                 (plist-segment
                  (or (getf segment :text) ""))
                 ((and (consp segment) (stringp (car segment)))
                  (car segment))
                 ((stringp segment)
                  segment)
                 (t
                  (princ-to-string segment))))
             (role
               (cond
                 (plist-segment
                  (or (getf segment :role) :meta))
                 ((and (consp segment) (cdr segment))
                  (cdr segment))
                 (t
                  :meta))))
        (when (plusp (length text))
          (push
           (list text
                 (chat-role-cell role
                                 :focusp focusp
                                 :boldp (and plist-segment (getf segment :boldp))
                                 :italicp (and plist-segment (getf segment :italicp))
                                 :underlinep (and plist-segment (getf segment :underlinep))
                                 :invertp (and plist-segment (getf segment :invertp))
                                 :dimp (and plist-segment (getf segment :dimp))
                                 :strikep (and plist-segment (getf segment :strikep))))
           result))))
    (nreverse result)))

(defun %chat-plan-command-preview-lines (plan-state selected-step-index)
  (let* ((steps (%chat-plan-sorted-steps plan-state))
         (approved-indexes (plan-mode-state-approved-step-indexes plan-state))
         (entries '()))
    (dolist (step steps)
      (let* ((step-index (or (plan-step-index step) 0))
             (approved-p (member step-index approved-indexes :test #'=)))
        (dolist (command (%chat-plan-step-command-previews step))
          (push (format nil
                        "DRY-RUN> [step ~D ~A~:[~; | selected~] | non-executed] ~A"
                        step-index
                        (if approved-p "approved" "pending")
                        (and (integerp selected-step-index)
                             (= step-index selected-step-index))
                        command)
                entries))))
    (let ((ordered (nreverse entries)))
      (subseq ordered
              0
              (min (length ordered)
                   +chat-plan-command-preview-max-lines+)))))

(defun %chat-plan-step-status-from-execution-step (execution-step approved-p)
  (if execution-step
      (case (plan-execution-step-status execution-step)
        (:running :running)
        (:completed :done)
        (:done :done)
        (:blocked :blocked)
        (:failed :blocked)
        (:aborted :blocked)
        (otherwise :pending))
      (if approved-p :approved :pending)))

(defun %chat-plan-step-status-event-table (chat-state execution-state)
  (let* ((run-id (and (plan-execution-state-p execution-state)
                      (plan-execution-state-run-id execution-state)))
         (bus (and chat-state (%context-event-bus chat-state))))
    (unless (and (%chat-plan-execution-surface-active-p execution-state)
                 (event-bus-p bus)
                 (stringp run-id)
                 (plusp (length run-id)))
      (return-from %chat-plan-step-status-event-table nil))
    (let ((table (make-hash-table :test #'eql)))
      (dolist (event (event-history bus))
        (when (eq (event-type event) +event-type-plan-step-status+)
          (let ((payload (event-payload event)))
            (when (and (plan-step-status-payload-p payload)
                       (integerp (plan-step-status-payload-step-index payload))
                       (stringp (plan-step-status-payload-run-id payload))
                       (string= (plan-step-status-payload-run-id payload) run-id))
              (setf (gethash (plan-step-status-payload-step-index payload) table)
                    (case (plan-step-status-payload-status payload)
                      (:running :running)
                      (:blocked :blocked)
                      (:done :done)
                      (otherwise :pending)))))))
      table)))

(defun %chat-plan-step-status-event-signature (chat-state execution-state)
  (let* ((run-id (and (plan-execution-state-p execution-state)
                      (plan-execution-state-run-id execution-state)))
         (bus (and chat-state (%context-event-bus chat-state))))
    (unless (and (%chat-plan-execution-surface-active-p execution-state)
                 (event-bus-p bus)
                 (stringp run-id)
                 (plusp (length run-id)))
      (return-from %chat-plan-step-status-event-signature nil))
    (loop for event in (event-history bus)
          for payload = (event-payload event)
          when (and (eq (event-type event) +event-type-plan-step-status+)
                    (plan-step-status-payload-p payload)
                    (integerp (plan-step-status-payload-step-index payload))
                    (stringp (plan-step-status-payload-run-id payload))
                    (string= (plan-step-status-payload-run-id payload) run-id))
            collect (list (event-seq event)
                          (plan-step-status-payload-step-index payload)
                          (plan-step-status-payload-status payload)))))

(defun %chat-plan-presentation-steps (plan-state visible-steps execution-state &optional chat-state)
  (let ((execution-step-table
          (when (%chat-plan-execution-surface-active-p execution-state)
            (let ((table (make-hash-table :test #'eql)))
              (dolist (execution-step (plan-execution-state-steps execution-state))
                (let ((step-index (plan-execution-step-index execution-step)))
                  (when (integerp step-index)
                    (setf (gethash step-index table) execution-step))))
              table)))
        (event-status-table (%chat-plan-step-status-event-table chat-state execution-state)))
    (loop for step in visible-steps
          for step-index = (or (plan-step-index step) 0)
          for execution-step = (and execution-step-table
                                    (gethash step-index execution-step-table))
          for approved-p = (if execution-step
                               (plan-execution-step-approved-p execution-step)
                               (member step-index
                                       (plan-mode-state-approved-step-indexes plan-state)
                                       :test #'=))
          for fallback-status = (%chat-plan-step-status-from-execution-step
                                 execution-step
                                 approved-p)
          for event-status = (and event-status-table
                                  (gethash step-index event-status-table))
          for status = (or event-status fallback-status)
          collect
          (ptui.components.plan-presentation:make-plan-presentation-step
           :index step-index
           :description (%chat-plan-presentation-safe-string
                         (plan-step-description step)
                         "Describe this step.")
           :approved-p (not (null approved-p))
           :file-paths (%chat-plan-normalize-path-list (plan-step-file-paths step))
           :rationale-snippet (%chat-plan-rationale-snippet step)
           :risk (or (plan-step-risk step) :medium)
           :status status))))

(defun %chat-plan-presentation-output-lines (plan-state selected-step-index)
  (let* ((steps (or (plan-mode-state-steps plan-state) '()))
         (total (length steps))
         (approved (length (plan-mode-state-approved-step-indexes plan-state)))
         (selected-step (%chat-plan-step-by-index steps selected-step-index))
         (selected-file-path
           (car (%chat-plan-normalize-path-list
                 (and selected-step (plan-step-file-paths selected-step)))))
         (selected-step-line
           (when (integerp selected-step-index)
             (if selected-file-path
                 (format nil
                         "Selected step: ~D (Ctrl-N/Ctrl-P to change) | file ~A"
                         selected-step-index
                         selected-file-path)
                 (format nil "Selected step: ~D (Ctrl-N/Ctrl-P to change)"
                         selected-step-index))))
         (decision-text
           (string-downcase
            (symbol-name (or (plan-mode-state-review-decision plan-state)
                             :pending))))
         (pending-p (plan-mode-state-review-pending-p plan-state))
         (command-previews (%chat-plan-command-preview-lines plan-state
                                                             selected-step-index)))
    (if command-previews
        (if selected-step-line
            (cons selected-step-line command-previews)
            command-previews)
        (append
         (when selected-step-line
           (list selected-step-line))
         (list "Plan mode active. Mutating tools remain blocked."
               (format nil "Review decision: ~A~:[~; (pending)~]" decision-text pending-p)
               (format nil "Step approvals: ~D/~D" approved total)
               "DRY-RUN> [non-executed] No command snippets detected in proposed steps yet.")))))

(defun %chat-plan-output-stdin-capture-policy ()
  (if (or (%chat-plan-mode-enabled-p)
          (%chat-plan-execution-surface-active-p))
      :disabled
      :enabled))

(defun %chat-plan-execution-output-line-entries (execution-state)
  (when (%chat-plan-execution-surface-active-p execution-state)
    (loop for entry in (plan-execution-state-continuity-output execution-state)
          collect (list :text (%chat-plan-presentation-safe-string
                               (plan-execution-output-entry-line entry)
                               "")
                        :step-index (plan-execution-output-entry-step-index entry)
                        :severity (or (plan-execution-output-entry-severity entry) :info)
                        :style (or (plan-execution-output-entry-style entry) :plain)
                        :recovery-actions
                        (copy-list (or (plan-execution-output-entry-recovery-actions entry)
                                       '()))))))

(defun %chat-plan-format-elapsed-seconds (elapsed-seconds)
  (let* ((total-seconds (max 0 (or elapsed-seconds 0)))
         (hours (truncate total-seconds 3600))
         (remaining (mod total-seconds 3600))
         (minutes (truncate remaining 60))
         (seconds (mod remaining 60)))
    (cond
      ((> hours 0)
       (format nil "~Dh ~Dm ~Ds" hours minutes seconds))
      ((> minutes 0)
       (format nil "~Dm ~Ds" minutes seconds))
      (t
       (format nil "~Ds" seconds)))))

(defun %chat-plan-execution-elapsed-seconds (execution-state)
  (let* ((started-at (plan-execution-state-started-at execution-state))
         (finished-at (plan-execution-state-finished-at execution-state))
         (status (plan-execution-state-status execution-state))
         (end-time (if (member status '(:completed :failed :aborted) :test #'eq)
                       finished-at
                       (get-universal-time))))
    (if (and (integerp started-at)
             (integerp end-time))
        (max 0 (- end-time started-at))
        0)))

(defun %chat-plan-execution-progress-line (execution-state)
  (let* ((approved-indexes (or (plan-execution-state-approved-step-indexes execution-state) '()))
         (total (length approved-indexes)))
    (unless (plusp total)
      (return-from %chat-plan-execution-progress-line
        "Execution progress: step 0 of 0 (elapsed 0s)"))
    (let* ((current-index (plan-execution-state-current-step-index execution-state))
           (current-position (and (integerp current-index)
                                  (position current-index approved-indexes :test #'=)))
           (completed (length (plan-execution-state-completed-step-indexes execution-state)))
           (status (plan-execution-state-status execution-state))
           (step-number
             (cond
               ((integerp current-position)
                (1+ current-position))
               ((eq status :completed)
                total)
               ((plusp completed)
                (min total (1+ completed)))
               (t
                1)))
           (elapsed-seconds (%chat-plan-execution-elapsed-seconds execution-state)))
      (format nil "Execution progress: step ~D of ~D (elapsed ~A)"
              step-number
              total
              (%chat-plan-format-elapsed-seconds elapsed-seconds)))))

(defun %chat-plan-presentation-context-lines (plan-state selected-step visible-steps execution-state)
  (let* ((steps (or (plan-mode-state-steps plan-state) '()))
         (high-risk-count
           (count-if (lambda (step)
                       (eq (plan-step-risk step) :high))
                     steps))
         (flattened-file-paths
           (remove-duplicates
            (loop for step in steps
                  append (or (plan-step-file-paths step) '()))
            :test #'string=))
         (notes (plan-mode-state-review-notes plan-state))
         (extra-step-count
           (max 0 (- (length steps) +chat-plan-presentation-max-steps+))))
    (append
     (list (format nil "Captured steps: ~D~:[~; (showing first ~D)~]"
                   (length steps)
                   (> extra-step-count 0)
                   +chat-plan-presentation-max-steps+)
           (format nil "Visible steps: ~D" (length visible-steps))
           (format nil "High-risk steps: ~D" high-risk-count)
           (if flattened-file-paths
               (format nil "Referenced files: ~{~A~^, ~}"
                       (subseq flattened-file-paths
                               0
                               (min 3 (length flattened-file-paths))))
               "Referenced files: none")
           (format nil "Review notes: ~A"
                   (%chat-plan-presentation-safe-string notes "none"))
           "Selection controls: Ctrl-N next, Ctrl-P previous.")
     (when selected-step
       (list (format nil "Selected rationale chars: ~D"
                     (length (%chat-plan-rationale-snippet selected-step)))))
     (when (%chat-plan-execution-surface-active-p execution-state)
       (list (format nil "Execution run: ~A"
                     (%chat-plan-presentation-safe-string
                      (plan-execution-state-run-id execution-state)
                      "none"))
             (format nil "Run status: ~A"
                     (string-downcase
                      (symbol-name (or (plan-execution-state-status execution-state)
                                       :idle))))
             (%chat-plan-execution-progress-line execution-state)
             (format nil "Execution progress: done ~D / pending ~D"
                     (length (plan-execution-state-completed-step-indexes execution-state))
                     (length (plan-execution-state-pending-step-indexes execution-state))))))))

(defun %chat-plan-presentation-widget (plan-state chat-state)
  (let ((execution-state (current-plan-execution-state)))
    (when (%chat-plan-workspace-visible-p plan-state execution-state)
      (let* ((visible-steps (%chat-plan-visible-steps plan-state))
             (selected-step-index (%chat-plan-resolve-selected-step-index
                                   chat-state
                                   plan-state
                                   visible-steps))
             (selected-step (%chat-plan-step-by-index visible-steps
                                                      selected-step-index))
             (output-line-entries (%chat-plan-execution-output-line-entries execution-state))
             (output-lines (if output-line-entries
                               nil
                               (%chat-plan-presentation-output-lines plan-state
                                                                    selected-step-index))))
        (ptui.components.plan-presentation:make-plan-mode-presentation-widget
         :id :chat-plan-presentation
         :steps (%chat-plan-presentation-steps plan-state
                                               visible-steps
                                               execution-state
                                               chat-state)
         :selected-step-index selected-step-index
         :output-lines output-lines
         :output-line-entries output-line-entries
         :output-stdin-capture-policy (%chat-plan-output-stdin-capture-policy)
         :context-lines (%chat-plan-presentation-context-lines plan-state
                                                               selected-step
                                                               visible-steps
                                                               execution-state)
         :output-viewport-height +chat-plan-presentation-output-viewport-height+)))))

(defun %chat-plan-workspace-tree-key (chat-state)
  (let* ((plan-state (current-plan-mode-state))
         (execution-state (current-plan-execution-state)))
    (list (%chat-plan-mode-enabled-p)
          (chat-ui-state-plan-selected-step-index chat-state)
          (loop for step in (or (plan-mode-state-steps plan-state) '())
                collect (list (plan-step-index step)
                              (plan-step-description step)
                              (copy-list (or (plan-step-file-paths step) '()))
                              (plan-step-risk step)))
          (copy-list (or (plan-mode-state-approved-step-indexes plan-state) '()))
          (plan-mode-state-review-decision plan-state)
          (plan-mode-state-review-pending-p plan-state)
          (%chat-plan-execution-surface-active-p execution-state)
          (plan-execution-state-run-id execution-state)
          (plan-execution-state-status execution-state)
          (plan-execution-state-current-step-index execution-state)
          (and (%chat-plan-execution-surface-active-p execution-state)
               (%chat-plan-execution-elapsed-seconds execution-state))
          (%chat-plan-step-status-event-signature chat-state execution-state)
          (copy-list (or (plan-execution-state-pending-step-indexes execution-state) '()))
          (copy-list (or (plan-execution-state-completed-step-indexes execution-state) '()))
          (loop for entry in (or (plan-execution-state-continuity-output execution-state) '())
                collect (list (plan-execution-output-entry-line entry)
                              (plan-execution-output-entry-step-index entry)
                              (plan-execution-output-entry-style entry)
                              (plan-execution-output-entry-severity entry)
                              (plan-execution-output-entry-phase entry)
                              (plan-execution-output-entry-timestamp entry))))))

(defun chat-ui-build-tree (state cols rows)
  (clrhash *%style-resolve-cache*)
  (let* ((chat-state (ensure-chat-ui-state state))
         (runtime (chat-ui-state-runtime chat-state))
         (tree (let ((ptui.ui.runtime:*current-runtime* runtime))
                 (render-chat-panel chat-state cols rows)))
         (id-remaps '()))
    (labels
        ((record-id-remap (old-id new-id)
           (let ((existing (assoc old-id id-remaps :test #'eq)))
             (if existing
                 (setf (cdr existing) new-id)
                 (push (cons old-id new-id) id-remaps))))
         (tree-has-id-p (node target-id)
           (or (equal (ptui.ui.elements:ui-element-id node) target-id)
               (loop for child in (ptui.ui.elements:ui-element-children node)
                     thereis (tree-has-id-p child target-id))))
         (tree-has-id-prefix-p (node target-id)
           (let ((node-id (ptui.ui.elements:ui-element-id node)))
             (or (and (consp node-id)
                      (equal (first node-id) target-id))
                 (loop for child in (ptui.ui.elements:ui-element-children node)
                       thereis (tree-has-id-prefix-p child target-id)))))
         (normalize-ids! (node)
           (let ((node-id (ptui.ui.elements:ui-element-id node)))
             (when (and (symbolp node-id)
                        (string= (symbol-name node-id) "TREE")
                        (tree-has-id-p node :tree-browser-header))
               (record-id-remap node-id :tree-browser)
               (setf (ptui.ui.elements:ui-element-id node) :tree-browser))
             (when (and (symbolp node-id)
                        (string= (symbol-name node-id) "PLAN")
                        (tree-has-id-prefix-p node :chat-plan-presentation))
               (record-id-remap node-id :chat-plan-presentation)
               (setf (ptui.ui.elements:ui-element-id node) :chat-plan-presentation))
             (when (and (symbolp node-id)
                        (string= (symbol-name node-id) "INPUT")
                        (eql (ptui.ui.elements:ui-element-type node) :prompt-box))
               (record-id-remap node-id :chat-input)
               (setf (ptui.ui.elements:ui-element-id node) :chat-input)))
           (dolist (child (ptui.ui.elements:ui-element-children node))
             (normalize-ids! child)))
         (apply-constraint-id-remaps! (node)
           (when (eq (ptui.ui.elements:ui-element-type node) :constraint-layout)
             (let ((constraints
                     (getf (ptui.ui.elements:ui-element-props node) :constraints)))
               (dolist (spec constraints)
                 (let* ((spec-id (ptui.layout.constraints:constraint-spec-id spec))
                        (remap (cdr (assoc spec-id id-remaps :test #'eq))))
                   (when remap
                     (setf (ptui.layout.constraints:constraint-spec-id spec) remap))))))
           (dolist (child (ptui.ui.elements:ui-element-children node))
             (apply-constraint-id-remaps! child))))
      (normalize-ids! tree)
      (when id-remaps
        (apply-constraint-id-remaps! tree)))
    (ptui.ui.runtime:update-runtime runtime tree)
    tree))

(defvar *%render-prev-fingerprint* nil)
(defvar *%render-prev-buffer* nil)

(defun %chat-render-fingerprint (chat-state cols rows)
  "Cheap fingerprint of UI-visible state. Returns a list for EQUAL comparison."
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (renderer (chat-ui-state-stream-markdown-renderer chat-state)))
    (list (length (chat-ui-state-messages chat-state))
          (chat-ui-state-input-text chat-state)
          (chat-ui-state-message-scrollback-lines chat-state)
          (chat-ui-state-prompt-scroll-offset chat-state)
          (token-stream-state-status stream-state)
          (streaming-markdown-renderer-pending-line renderer)
          (length (streaming-markdown-renderer-wrapped-lines renderer))
          (stream-cursor-visible-p stream-state)
          (chat-ui-state-provider-dashboard-visible-p chat-state)
          (chat-ui-state-history-search-active-p chat-state)
          (chat-ui-state-plan-selected-step-index chat-state)
          (chat-ui-state-cursor-position chat-state)
          cols rows
          *%styled-lines-cache-generation*)))

(defun render-chat-ui-buffer (state size)
  (labels ((render-once ()
             (let* ((chat-state (ensure-chat-ui-state state))
                    (cols (ptui.core.types:size-cols size))
                    (rows (ptui.core.types:size-rows size))
                    (frame-start-ms (%usdt-now-ms)))
               (%sync-all-state! chat-state)
               (maybe-auto-checkpoint
                :conversation (%ensure-chat-conversation-state chat-state)
                :config (%chat-config)
                :busy-p (token-stream-active-p (chat-ui-state-stream-state chat-state)))
               ;; Fingerprint cache disabled — buffer reuse can mutate cached buffer.

               (incf (chat-ui-state-frame-count chat-state))
               (let* ((frame-index (chat-ui-state-frame-count chat-state))
                      (tree (chat-ui-build-tree chat-state cols rows))
                      (buffer nil))
                 (%normalize-tree-styled-segments! tree)
                 (setf buffer (ptui.ui.app::%render-tree-to-buffer tree size))
                 (usdt-probe-render-frame frame-index
                                          (max 0 (- (%usdt-now-ms) frame-start-ms))
                                          cols
                                          rows)
                 buffer))))
    (let* ((chat-state (ensure-chat-ui-state state))
           (approval-recovery-p (%approval-recovery-active-p chat-state)))
      (handler-case
          (render-once)
        (error (condition)
          (if approval-recovery-p
              (progn
                (%handle-approval-ui-error! chat-state :render-cycle condition)
                (render-once))
              (error condition)))))))
