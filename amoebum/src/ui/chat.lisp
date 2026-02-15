(in-package :amoebum)

(defparameter +chat-role-order+ '("system" "user" "assistant" "tool"))

(defstruct (chat-ui-state
            (:constructor make-chat-ui-state
                (&key (runtime (ptui.ui.runtime:make-runtime))
                      (messages '())
                      (input-text "")
                      (message-scrollback-lines 0)
                      (max-message-scrollback-lines 0)
                      (prompt-scroll-offset nil)
                      (stream-state (make-token-stream-state))
                      (stream-runner #'stream-pseudopod-chat)
                      (stream-client nil)
                      (stream-system-prompt +chat-stream-default-system-prompt+)
                      (stream-tools nil)
                      (status-bar-state (make-status-bar-state))
                      (stream-status-publish-key nil)
                      (frame-count 0))))
  runtime
  (messages '() :type list)
  (input-text "" :type string)
  (message-scrollback-lines 0 :type fixnum)
  (max-message-scrollback-lines 0 :type fixnum)
  (prompt-scroll-offset nil)
  (stream-state (make-token-stream-state) :type token-stream-state)
  (stream-runner #'stream-pseudopod-chat :type (or null function))
  (stream-client nil)
  (stream-system-prompt +chat-stream-default-system-prompt+ :type string)
  (stream-tools nil)
  (status-bar-state (make-status-bar-state) :type status-bar-state)
  (stream-status-publish-key nil)
  (frame-count 0 :type fixnum)
  (cached-tree-key nil)
  (cached-tree nil)
  (cached-layout nil)
  (cached-render-key nil)
  (cached-buffer nil))

(defun ensure-chat-ui-state (state)
  (let ((chat-state
          (if (and state (typep state 'chat-ui-state))
              state
              (make-chat-ui-state :runtime (ptui.ui.runtime:make-runtime)))))
    (when (null (chat-ui-state-runtime chat-state))
      (setf (chat-ui-state-runtime chat-state) (ptui.ui.runtime:make-runtime)))
    (unless (typep (chat-ui-state-stream-state chat-state) 'token-stream-state)
      (setf (chat-ui-state-stream-state chat-state) (make-token-stream-state)))
    (unless (or (null (chat-ui-state-stream-runner chat-state))
                (functionp (chat-ui-state-stream-runner chat-state)))
      (setf (chat-ui-state-stream-runner chat-state) #'stream-pseudopod-chat))
    (unless (and (stringp (chat-ui-state-stream-system-prompt chat-state))
                 (plusp (length (chat-ui-state-stream-system-prompt chat-state))))
      (setf (chat-ui-state-stream-system-prompt chat-state)
            +chat-stream-default-system-prompt+))
    (setf (chat-ui-state-status-bar-state chat-state)
          (ensure-status-bar-state
           (chat-ui-state-status-bar-state chat-state)
           :event-bus (current-event-bus)))
    chat-state))

(defun %normalize-chat-role (role)
  (let ((normalized
          (string-downcase
           (cond
             ((stringp role) role)
             ((symbolp role) (symbol-name role))
             (t "assistant")))))
    (if (member normalized +chat-role-order+ :test #'string=)
        normalized
        "assistant")))

(defun make-chat-message (role content &key name tool-calls partial)
  (pseudopod:make-message
   :role (%normalize-chat-role role)
   :content content
   :name name
   :tool-calls tool-calls
   :partial partial))

(defun chat-ui-append-message (state message)
  (check-type message pseudopod:message)
  (let ((chat-state (ensure-chat-ui-state state)))
    (setf (chat-ui-state-messages chat-state)
          (append (chat-ui-state-messages chat-state)
                  (list message)))
    (setf (chat-ui-state-message-scrollback-lines chat-state) 0)
    message))

(defun chat-ui-add-message (state role content &key name tool-calls partial)
  (chat-ui-append-message
   state
   (make-chat-message role content
                      :name name
                      :tool-calls tool-calls
                      :partial partial)))

(defun chat-ui-set-input (state text)
  (let ((chat-state (ensure-chat-ui-state state)))
    (setf (chat-ui-state-input-text chat-state)
          (if (stringp text)
              text
              (princ-to-string text))
          (chat-ui-state-prompt-scroll-offset chat-state) nil)
    (chat-ui-state-input-text chat-state)))

(defun %blank-string-p (text)
  (every (lambda (char)
           (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=))
         text))

(defun chat-ui-submit-input (state)
  (let* ((chat-state (ensure-chat-ui-state state))
         (input (chat-ui-state-input-text chat-state)))
    (if (or (null input) (zerop (length input)) (%blank-string-p input))
        nil
        (prog1
            (chat-ui-add-message chat-state "user" input)
          (setf (chat-ui-state-input-text chat-state) ""
                (chat-ui-state-prompt-scroll-offset chat-state) nil)))))

(defun chat-ui-scroll-history (state delta-lines)
  (let* ((chat-state (ensure-chat-ui-state state))
         (max-scrollback (max 0 (chat-ui-state-max-message-scrollback-lines chat-state)))
         (next-scrollback (+ (chat-ui-state-message-scrollback-lines chat-state)
                             (or delta-lines 0))))
    (setf (chat-ui-state-message-scrollback-lines chat-state)
          (max 0 (min max-scrollback next-scrollback)))
    (chat-ui-state-message-scrollback-lines chat-state)))

(defun chat-role-prefix (role)
  (case (intern (string-upcase (%normalize-chat-role role)) :keyword)
    (:system "SYSTEM>")
    (:user "YOU>")
    (:assistant "ASSISTANT>")
    (:tool "TOOL>")
    (otherwise "ASSISTANT>")))

(defun %content-part-text (part)
  (let ((type (string-downcase (or (pseudopod:content-part-type part) "text"))))
    (cond
      ((string= type "text")
       (or (pseudopod:content-part-text part) ""))
      ((string= type "think")
       (or (pseudopod:content-part-think part) ""))
      (t
       (or (pseudopod:content-part-text part)
           (pseudopod:content-part-think part)
           "")))))

(defun %message-content->text (message)
  (let ((parts (pseudopod:message-content message)))
    (if (null parts)
        ""
        (with-output-to-string (out)
          (loop for part in parts
                for index from 0 do
                  (when (> index 0)
                    (write-char #\Newline out))
                  (write-string (%content-part-text part) out))))))

(defun %replace-message-at-index! (messages index message)
  (let ((cell (nthcdr index messages)))
    (when cell
      (setf (car cell) message)
      t)))

(defun %stream-status-summary (chat-state)
  (token-stream-progress-summary (chat-ui-state-stream-state chat-state)))

(defun %stream-summary-publish-key (summary)
  (let ((status (or (getf summary :status) :idle))
        (tokens (or (getf summary :tokens) 0))
        (chunks (or (getf summary :chunks) 0))
        (cancel-requested-p (not (null (getf summary :cancel-requested-p))))
        (tokens-per-second (or (getf summary :tokens-per-second) 0.0d0))
        (elapsed-ms (or (getf summary :elapsed-ms) 0)))
    (list status
          tokens
          chunks
          cancel-requested-p
          (if (eq status :running)
              (truncate (* 10 tokens-per-second))
              elapsed-ms))))

(defun %publish-status-bar-stream-summary-if-needed (chat-state)
  (let* ((summary (%stream-status-summary chat-state))
         (publish-key (%stream-summary-publish-key summary)))
    (unless (equal publish-key (chat-ui-state-stream-status-publish-key chat-state))
      (publish-status-bar-stream-summary
       summary
       :event-bus (status-bar-state-event-bus
                   (chat-ui-state-status-bar-state chat-state)))
      (setf (chat-ui-state-stream-status-publish-key chat-state) publish-key))
    summary))

(defun %stream-tree-key (chat-state)
  (let* ((summary (%stream-status-summary chat-state))
         (status (getf summary :status))
         (elapsed-ms (or (getf summary :elapsed-ms) 0)))
    (list (status-bar-render-key (chat-ui-state-status-bar-state chat-state))
          status
          (getf summary :tokens)
          (getf summary :chunks)
          (if (eq status :running)
              (truncate elapsed-ms 100)
              elapsed-ms)
          (getf summary :cancel-requested-p)
          (getf summary :error-message))))

(defun %set-streaming-assistant-message (chat-state target-index text &key partialp)
  (let ((messages (chat-ui-state-messages chat-state)))
    (when (and (integerp target-index)
               (>= target-index 0)
               (< target-index (length messages)))
      (%replace-message-at-index!
       messages
       target-index
       (make-chat-message "assistant"
                          (or text "")
                          :partial partialp))
      (setf (chat-ui-state-message-scrollback-lines chat-state) 0)
      t)))

(defun %append-streaming-assistant-chunk (chat-state chunk)
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (target-index (token-stream-state-target-message-index stream-state))
         (messages (chat-ui-state-messages chat-state)))
    (when (and (integerp target-index)
               (>= target-index 0)
               (< target-index (length messages)))
      (let* ((message (nth target-index messages))
             (current-text (%message-content->text message))
             (next-text (concatenate 'string current-text (or chunk ""))))
        (%set-streaming-assistant-message chat-state target-index next-text :partialp t)))))

(defun %finalize-streaming-assistant-message (chat-state &key partialp)
  (let* ((stream-state (chat-ui-state-stream-state chat-state))
         (target-index (token-stream-state-target-message-index stream-state))
         (messages (chat-ui-state-messages chat-state)))
    (when (and (integerp target-index)
               (>= target-index 0)
               (< target-index (length messages)))
      (let* ((message (nth target-index messages))
             (text (%message-content->text message)))
        (%set-streaming-assistant-message chat-state target-index text :partialp partialp)))))

(defun %drain-stream-events (chat-state)
  (token-stream-drain-events
   (chat-ui-state-stream-state chat-state)
   (lambda (event)
     (case (getf event :kind)
       (:chunk
        (%append-streaming-assistant-chunk chat-state (getf event :text)))
       (:complete
        (%finalize-streaming-assistant-message chat-state :partialp nil))
       (:cancelled
        (%finalize-streaming-assistant-message chat-state :partialp t))
       (:failed
        (%finalize-streaming-assistant-message chat-state :partialp t))
       (otherwise nil)))))

(defun %stream-status-fragment (chat-state)
  (let* ((summary (%stream-status-summary chat-state))
         (status (getf summary :status))
         (tokens (or (getf summary :tokens) 0))
         (elapsed-ms (or (getf summary :elapsed-ms) 0))
         (tps (or (getf summary :tokens-per-second) 0.0d0))
         (error-message (getf summary :error-message)))
    (case status
      (:running
       (format nil "stream ~D tok @ ~,2f tok/s ~,1fs"
               tokens
               tps
               (/ elapsed-ms 1000.0d0)))
      (:cancelled
       (format nil "stream cancelled (~D tok, ~,1fs)"
               tokens
               (/ elapsed-ms 1000.0d0)))
      (:completed
       (format nil "stream complete (~D tok, ~,1fs)"
               tokens
               (/ elapsed-ms 1000.0d0)))
      (:failed
       (if (and (stringp error-message) (plusp (length error-message)))
           (format nil "stream failed: ~A" error-message)
           "stream failed"))
      (otherwise
       nil))))

(defun %start-streaming-assistant-response (chat-state user-message)
  (when (and (pseudopod:message-p user-message)
             (not (token-stream-active-p (chat-ui-state-stream-state chat-state))))
    (let ((runner (chat-ui-state-stream-runner chat-state)))
      (when (functionp runner)
        (let* ((prompt (%message-content->text user-message))
               (history (copy-list (chat-ui-state-messages chat-state)))
               (target-index (length history))
               (stream-state (chat-ui-state-stream-state chat-state)))
          (chat-ui-add-message chat-state "assistant" "" :partial t)
          (token-stream-start
           stream-state
           (lambda (active-stream-state)
             (funcall runner
                      active-stream-state
                      prompt
                      history
                      :system-prompt (chat-ui-state-stream-system-prompt chat-state)
                      :client (chat-ui-state-stream-client chat-state)
                      :tools (chat-ui-state-stream-tools chat-state)))
           :target-message-index target-index))))))

(defun %message-line-entries (messages width)
  (let ((entries '())
        (safe-width (max 1 width)))
    (loop for message in messages
          for index from 0 do
            (let* ((role (%normalize-chat-role (pseudopod:message-role message)))
                   (prefix (chat-role-prefix role))
                   (body (%message-content->text message))
                   (prefix-width (ptui.text.width:string-width prefix))
                   (content-width (max 1 (- safe-width (+ prefix-width 1))))
                   (wrapped (ptui.text.layout:wrap-by-width body content-width))
                   (wrapped (if (null wrapped) (list "") wrapped))
                   (indent (make-string (+ prefix-width 1) :initial-element #\Space)))
              (loop for line in wrapped
                    for line-index from 0 do
                      (push (list :id (list :chat-message index line-index)
                                  :text (if (zerop line-index)
                                            (format nil "~A ~A" prefix line)
                                            (concatenate 'string indent line))
                                  :role role)
                            entries))
              (unless (= index (1- (length messages)))
                (push (list :id (list :chat-gap index)
                            :text ""
                            :role :meta)
                      entries))))
    (if entries
        (nreverse entries)
        (list (list :id :chat-empty
                    :text "No conversation yet. Type below and press Enter."
                    :role :system)))))

(defun %chat-text-widget (text id role)
  (ptui.ui.elements:make-element
   :text
   :id id
   :props (list :text text :role role)
   :children '()))

(defun %clamp-layout-size (size avail-width avail-height)
  (let ((w (ptui.layout:layout-size-width size))
        (h (ptui.layout:layout-size-height size)))
    (ptui.layout:make-layout-size
     (if avail-width
         (min w (max 0 avail-width))
         w)
     (if avail-height
         (min h (max 0 avail-height))
         h))))

(defun %element-prop (element key &optional default)
  (getf (ptui.ui.elements:ui-element-props element) key default))

(defun %element-id (element)
  (or (ptui.ui.elements:ui-element-id element)
      (ptui.ui.elements:ui-element-key element)))

(defun %ui-tree-node (element)
  (let ((id (%element-id element))
        (type (ptui.ui.elements:ui-element-type element))
        (children (mapcar #'%ui-tree-node
                          (ptui.ui.elements:ui-element-children element))))
    (ptui.layout:make-layout-node
     :id id
     :direction (if (eql type :stack)
                    (%element-prop element :direction :column)
                    :column)
     :gap (if (eql type :stack)
              (%element-prop element :gap 0)
              0)
     :measure (lambda (avail-width avail-height)
                (%clamp-layout-size
                 (ptui.widgets.core:widget-measure element)
                 avail-width
                 avail-height))
     :children children)))

(defun %chat-tree-signature (messages)
  (mapcar (lambda (message)
            (list (%normalize-chat-role (pseudopod:message-role message))
                  (%message-content->text message)))
          messages))

(defun %compute-scroll-offset (total-lines viewport-height scrollback-lines)
  (let* ((max-scrollback (max 0 (- total-lines viewport-height)))
         (bounded-scrollback (max 0 (min max-scrollback scrollback-lines)))
         (offset (- max-scrollback bounded-scrollback)))
    (values offset bounded-scrollback max-scrollback)))

(defun chat-ui-build-tree (state cols rows)
  (let* ((chat-state (ensure-chat-ui-state state))
         (inner-width (max 20 (- cols 4)))
         (inner-height (max 8 (- rows 4)))
         (input (ptui.components.prompt-box:make-prompt-box-widget
                 (chat-ui-state-input-text chat-state)
                 :id :chat-input
                 :min-width 18
                 :max-width inner-width
                 :min-rows 1
                 :max-rows 4
                 :scroll-offset (chat-ui-state-prompt-scroll-offset chat-state)
                 :border-style :rounded))
         (input-height (ptui.layout:layout-size-height
                        (ptui.widgets.core:widget-measure input)))
         (header-height 2)
         (history-height (max 1 (- inner-height input-height header-height 1)))
         (message-lines (%message-line-entries (chat-ui-state-messages chat-state)
                                               inner-width))
         (message-widgets
           (mapcar (lambda (entry)
                     (%chat-text-widget (getf entry :text)
                                        (getf entry :id)
                                        (getf entry :role)))
                   message-lines))
         (history-stack (ptui.widgets.core:make-stack-widget
                         message-widgets
                         :id :chat-history-stack
                         :direction :column
                         :gap 0))
         (history-total-lines
           (ptui.layout:layout-size-height
            (ptui.widgets.core:widget-measure history-stack)))
         (history-offset 0)
         (scrollback 0)
         (max-scrollback 0))
    (multiple-value-setq (history-offset scrollback max-scrollback)
      (%compute-scroll-offset history-total-lines
                              history-height
                              (chat-ui-state-message-scrollback-lines chat-state)))
    (setf (chat-ui-state-message-scrollback-lines chat-state) scrollback
          (chat-ui-state-max-message-scrollback-lines chat-state) max-scrollback)
    (let* ((history-scroll
             (ptui.widgets.core:make-scroll-widget
              history-stack
              :id :chat-history-scroll
              :viewport-width inner-width
              :viewport-height history-height
              :offset history-offset))
           (status-widget
             (make-status-bar-widget
              (chat-ui-state-status-bar-state chat-state)
              :id :chat-status-bar
              :width inner-width))
           (content
             (ptui.widgets.core:make-stack-widget
              (list
               (%chat-text-widget "amoebum chat" :chat-title :meta)
               status-widget
               history-scroll
               input)
              :id :chat-content
              :direction :column
              :gap 0)))
      (ptui.widgets.core:make-box-widget
       content
       :id :chat-root
       :padding 0
       :borderp t))))

(defun %chat-template-cell (&key (fg :default) (bg :default) (boldp nil))
  (ptui.core.types:make-cell
   " "
   fg
   bg
   (ptui.core.types:make-attrs :boldp boldp)))

(defun chat-role-cell (role &key (focusp nil))
  (let ((base
          (case (intern (string-upcase (princ-to-string role)) :keyword)
            (:system
             (%chat-template-cell :fg (ptui.core.color:make-color-rgb 255 205 120) :boldp t))
            (:user
             (%chat-template-cell :fg (ptui.core.color:make-color-rgb 130 210 255) :boldp t))
            (:assistant
             (%chat-template-cell :fg (ptui.core.color:make-color-rgb 150 235 170)))
            (:tool
             (%chat-template-cell :fg (ptui.core.color:make-color-rgb 230 185 255)))
            (:prompt
             (%chat-template-cell :fg (ptui.core.color:make-color-rgb 210 210 210)))
            (otherwise
             (%chat-template-cell :fg (ptui.core.color:make-color-rgb 175 175 175))))))
    (if focusp
        (ptui.core.types:make-cell
         " "
         (ptui.core.types:cell-fg base)
         (ptui.core.types:cell-bg base)
         (ptui.core.types:make-attrs :boldp t :invertp t))
        base)))

(defun %fit-line-width (text width)
  (ptui.text.layout:truncate-to-width text (max 0 width)))

(defun %prompt-wrapped-lines (value width)
  (if (<= width 0)
      (list "")
      (ptui.text.layout:wrap-by-width value (max 1 width))))

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

(defun %render-ui-element (buf element layout focus-id &key (dx 0) (dy 0))
  (let* ((id (%element-id element))
         (bounds (and id (ptui.layout:layout-bound layout id))))
    (when bounds
      (let* ((kind (ptui.ui.elements:ui-element-type element))
             (x (+ dx (ptui.layout:layout-bounds-x bounds)))
             (y (+ dy (ptui.layout:layout-bounds-y bounds)))
             (w (ptui.layout:layout-bounds-width bounds))
             (h (ptui.layout:layout-bounds-height bounds))
             (rect (ptui.core.types:make-rect x y w h)))
        (flet ((render-children (&key (child-dx dx)
                                      (child-dy dy)
                                      (clip-rect rect))
                 (ptui.render.buffer:with-clip (buf clip-rect)
                   (dolist (child (ptui.ui.elements:ui-element-children element))
                     (%render-ui-element buf child layout focus-id
                                         :dx child-dx
                                         :dy child-dy)))))
          (cond
            ((eq kind :text)
             (let* ((text (%element-prop element :text ""))
                    (role (%element-prop element :role :meta))
                    (line (%fit-line-width text w))
                    (cell (chat-role-cell role :focusp (eql id focus-id))))
               (ptui.render.buffer:buffer-draw-text
                buf x y (list (list line cell)) :max-width w)))
            ((eq kind :input)
             (let* ((value (%element-prop element :value ""))
                    (line (%fit-line-width value w))
                    (cell (chat-role-cell :prompt :focusp (eql id focus-id))))
               (ptui.render.buffer:buffer-draw-text
                buf x y (list (list line cell)) :max-width w)))
            ((eq kind :prompt-box)
             (let* ((value (%element-prop element :value ""))
                    (border-style (%element-prop element :border-style :rounded))
                    (scroll-offset (%element-prop element :scroll-offset nil))
                    (inner-x (+ x 1))
                    (inner-y (+ y 1))
                    (inner-w (max 0 (- w 2)))
                    (inner-h (max 0 (- h 2)))
                    (line-cell (chat-role-cell :prompt :focusp (eql id focus-id)))
                    (lines (%prompt-wrapped-lines value inner-w)))
               (ptui.render.buffer:buffer-draw-border
                buf rect :style line-cell :border-style border-style)
               (multiple-value-bind (visible-lines effective-offset max-offset)
                   (%prompt-visible-lines lines inner-h scroll-offset)
                 (declare (ignore effective-offset max-offset))
                 (loop for line in visible-lines
                       for row from 0 do
                         (ptui.render.buffer:buffer-draw-text
                          buf
                          inner-x
                          (+ inner-y row)
                          (list (list (%fit-line-width line inner-w) line-cell))
                          :max-width inner-w)))))
            ((eq kind :spacer)
             nil)
            ((eq kind :box)
             (let* ((padding (%element-prop element :padding 0))
                    (borderp (%element-prop element :borderp nil))
                    (border (if borderp 1 0))
                    (inset (+ border padding))
                    (inner-rect (ptui.core.types:make-rect
                                 (+ x inset)
                                 (+ y inset)
                                 (max 0 (- w (* 2 inset)))
                                 (max 0 (- h (* 2 inset)))))
                    (child (first (ptui.ui.elements:ui-element-children element))))
               (when borderp
                 (ptui.render.buffer:buffer-draw-border buf rect))
               (when child
                 (let* ((child-id (%element-id child))
                        (child-bounds
                          (and child-id (ptui.layout:layout-bound layout child-id))))
                   (when child-bounds
                     (let ((delta-x (- (ptui.core.types:rect-x inner-rect)
                                       (ptui.layout:layout-bounds-x child-bounds)))
                           (delta-y (- (ptui.core.types:rect-y inner-rect)
                                       (ptui.layout:layout-bounds-y child-bounds))))
                       (ptui.render.buffer:with-clip (buf inner-rect)
                         (%render-ui-element buf child layout focus-id
                                             :dx delta-x
                                             :dy delta-y))))))))
            ((eq kind :stack)
             (render-children))
            ((eq kind :scroll)
             (let* ((offset (%element-prop element :offset 0))
                    (child (first (ptui.ui.elements:ui-element-children element))))
               (when child
                 (ptui.render.buffer:with-clip (buf rect)
                   (%render-ui-element buf child layout focus-id
                                       :dx dx
                                       :dy (- dy offset))))))
            (t
             (render-children))))))))

(defun %chat-tree-key (state cols rows)
  (list cols
        rows
        (chat-ui-state-input-text state)
        (chat-ui-state-message-scrollback-lines state)
        (chat-ui-state-prompt-scroll-offset state)
        (%chat-tree-signature (chat-ui-state-messages state))
        (%stream-tree-key state)))

(defun render-chat-ui-buffer (state size)
  (let* ((chat-state (ensure-chat-ui-state state))
         (runtime (chat-ui-state-runtime chat-state))
         (cols (ptui.core.types:size-cols size))
         (rows (ptui.core.types:size-rows size))
         (agent-completion-count (%inject-agent-completions chat-state))
         (drained-event-count (%drain-stream-events chat-state))
         (stream-summary (%publish-status-bar-stream-summary-if-needed chat-state))
         (tree-key (%chat-tree-key chat-state cols rows))
         (tree (chat-ui-state-cached-tree chat-state))
         (layout (chat-ui-state-cached-layout chat-state))
         (focus-id nil))
    (declare (ignore agent-completion-count drained-event-count stream-summary))
    (incf (chat-ui-state-frame-count chat-state))
    (unless (and tree layout
                 (equal tree-key (chat-ui-state-cached-tree-key chat-state)))
      (setf tree (chat-ui-build-tree chat-state cols rows))
      (setf layout (ptui.layout:compute-layout
                    (%ui-tree-node tree)
                    :x 2
                    :y 1
                    :width (max 4 (- cols 4))
                    :height (max 4 (- rows 2))))
      (ptui.ui.runtime:update-runtime runtime tree)
      (setf (chat-ui-state-cached-tree-key chat-state) tree-key
            (chat-ui-state-cached-tree chat-state) tree
            (chat-ui-state-cached-layout chat-state) layout
            (chat-ui-state-cached-render-key chat-state) nil
            (chat-ui-state-cached-buffer chat-state) nil))
    (setf focus-id (ptui.ui.runtime:runtime-focus-id runtime))
    (let* ((render-key (list tree-key focus-id))
           (cached-render-key (chat-ui-state-cached-render-key chat-state))
           (cached-buffer (chat-ui-state-cached-buffer chat-state)))
      (if (and cached-buffer (equal render-key cached-render-key))
          cached-buffer
          (let ((buf (ptui.render.buffer:make-buffer cols rows)))
            (%render-ui-element buf tree layout focus-id)
            (setf (chat-ui-state-cached-render-key chat-state) render-key
                  (chat-ui-state-cached-buffer chat-state) buf)
            buf)))))

(defun %pop-last-grapheme (text)
  (let ((clusters (ptui.text.grapheme:split-graphemes text)))
    (if (null clusters)
        ""
        (with-output-to-string (out)
          (dolist (cluster (butlast clusters))
            (write-string cluster out))))))

(defun %compact-chat-history (chat-state keep-last)
  (let* ((messages (chat-ui-state-messages chat-state))
         (count (length messages))
         (keep (if (and (integerp keep-last) (> keep-last 0))
                   keep-last
                   12)))
    (if (<= count keep)
        0
        (let* ((drop-count (- count keep))
               (tail (nthcdr drop-count messages)))
          (setf (chat-ui-state-messages chat-state) (copy-list tail)
                (chat-ui-state-message-scrollback-lines chat-state) 0)
          drop-count))))

(defun %apply-slash-command-action (chat-state result)
  (case (slash-command-result-action result)
    (:clear-chat
     (setf (chat-ui-state-messages chat-state) '()
           (chat-ui-state-message-scrollback-lines chat-state) 0
           (chat-ui-state-max-message-scrollback-lines chat-state) 0))
    (:compact-chat
     (%compact-chat-history chat-state (slash-command-result-payload result)))
    (otherwise nil)))

(defun %normalize-command-result (result)
  (cond
    ((typep result 'slash-command-result)
     result)
    ((stringp result)
     (make-slash-command-result :output result))
    (t
     (make-slash-command-result :output nil))))

(defun %handle-slash-command-input (chat-state input)
  (multiple-value-bind (handledp raw-result)
      (dispatch-slash-command input
                              :config (current-config)
                              :memory-backend (current-memory-backend)
                              :chat-state chat-state)
    (when handledp
      (let ((result (%normalize-command-result raw-result)))
        (%apply-slash-command-action chat-state result)
        (when (slash-command-result-echo-input-p result)
          (chat-ui-add-message chat-state "user" input))
        (let ((output (slash-command-result-output result)))
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

(defun %chat-trim-text (text)
  (if (stringp text)
      (string-trim '(#\Space #\Tab #\Newline #\Return) text)
      ""))

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

(defun %handle-input-key (state key text)
  (cond
    ((and (eql key :text) (stringp text))
     (chat-ui-set-input state
                        (concatenate 'string
                                     (chat-ui-state-input-text state)
                                     text))
     t)
    ((eql key :ctrl-j)
     (chat-ui-set-input state
                        (concatenate 'string
                                     (chat-ui-state-input-text state)
                                     (string #\Newline)))
     t)
    ((eql key :tab)
     (%handle-command-tab-completion state))
    ((or (eql key :enter) (eql key :return))
     (let ((input (chat-ui-state-input-text state)))
       (if (%handle-slash-command-input state input)
           t
           (let ((submitted (chat-ui-submit-input state)))
             (when submitted
               (unless (%handle-memory-candidate state submitted)
                 (%start-streaming-assistant-response state submitted))))))
     t)
    ((eql key :backspace)
     (chat-ui-set-input state
                        (%pop-last-grapheme (chat-ui-state-input-text state)))
     t)
    (t
     nil)))

(defun %handle-scroll-key (state key)
  (case key
    (:up (chat-ui-scroll-history state 1))
    (:down (chat-ui-scroll-history state -1))
    (:pgup (chat-ui-scroll-history state 5))
    (:pgdn (chat-ui-scroll-history state -5))
    (otherwise nil)))

(defun handle-chat-ui-event (state event)
  (let* ((chat-state (ensure-chat-ui-state state))
         (runtime (chat-ui-state-runtime chat-state))
         (agent-completion-count (%inject-agent-completions chat-state))
         (drained-event-count (%drain-stream-events chat-state))
         (route (if (typep event 'ptui.core.events:key-event)
                    (ptui.ui.runtime:route-event runtime event)
                    (list :kind :unhandled :event event)))
         (kind (getf route :kind))
         (target (getf route :target)))
    (declare (ignore agent-completion-count drained-event-count))
    (when (typep event 'ptui.core.events:key-event)
      (let ((key (ptui.core.events:key-event-key event))
            (text (ptui.core.events:key-event-text? event)))
        (when (and (member key '(:escape :ctrl-c))
                   (token-stream-active-p (chat-ui-state-stream-state chat-state)))
          (token-stream-request-cancel (chat-ui-state-stream-state chat-state)))
        (%handle-scroll-key chat-state key)
        (when (or (eql target :chat-input)
                  (eql kind :unhandled)
                  (null target))
          (%handle-input-key chat-state key text))))
    (when (and (ptui.ui.runtime:runtime-root runtime)
               (listp route))
      (ptui.widgets.core:dispatch-widget-event
       (ptui.ui.runtime:runtime-root runtime)
       route))
    (%drain-stream-events chat-state)
    (%publish-status-bar-stream-summary-if-needed chat-state)
    chat-state))

(defun run-chat-ui (&key (backend :auto) (fps 20) initial-state)
  (ptui.engine.loop:run #'render-chat-ui-buffer
                        :backend backend
                        :fps fps
                        :initial-state (ensure-chat-ui-state initial-state)
                        :on-event #'handle-chat-ui-event))
