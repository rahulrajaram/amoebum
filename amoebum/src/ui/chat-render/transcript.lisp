(in-package :amoebum)

;;; Transcript/message-entry cache ownership extracted mechanically from
;;; ui/chat-render.lisp for NXT-384. Preserve snapshot and render ordering.

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

(defun %reset-chat-render-caches! ()
  (clrhash *%styled-lines-cache*)
  (clrhash *%message-wrap-cache*)
  (setf *%message-wrap-cache-size* 0)
  (when (boundp '*%message-entry-cache*)
    (%invalidate-message-entry-cache)))

(defun %invalidate-styled-lines-cache ()
  "Call when messages are added or modified."
  (incf *%styled-lines-cache-generation*)
  (%reset-chat-render-caches!))

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
          (token-stream-set-target-message-index
           (chat-ui-state-stream-state chat-state)
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
