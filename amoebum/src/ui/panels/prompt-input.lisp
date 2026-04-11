;;; I298: prompt-input-panel
;;; Extracts prompt box from chat-ui-build-tree.
(in-package :amoebum)

;;; Forward declarations - defined in chat.lisp which is loaded after this file
(declaim (ftype function %prompt-wrapped-lines %cursor-to-line-col %line-col-to-cursor-pos))

(defstruct (chat-panel-input-key-context
            (:constructor %make-chat-panel-input-key-context
                (&key state key text inner-width input-text cur-pos pos input-width)))
  state
  key
  text
  inner-width
  input-text
  cur-pos
  pos
  input-width)

(defun %chat-panel-input-key-context (state key text inner-width)
  (let* ((input-text (chat-ui-state-input-text state))
         (cur-pos (chat-ui-state-cursor-position state))
         (pos (%ensure-cursor-pos input-text cur-pos))
         (input-width (max 1 (- (max 1 inner-width) 2))))
    (%make-chat-panel-input-key-context
     :state state
     :key key
     :text text
     :inner-width inner-width
     :input-text input-text
     :cur-pos cur-pos
     :pos pos
     :input-width input-width)))

(defun %chat-panel-input-handle-text (context)
  (let ((text (chat-panel-input-key-context-text context))
        (state (chat-panel-input-key-context-state context))
        (input-text (chat-panel-input-key-context-input-text context))
        (pos (chat-panel-input-key-context-pos context)))
    (if (stringp text)
        (let ((new-text (%grapheme-insert-at input-text pos text))
              (advance (%grapheme-length text)))
          (chat-ui-set-input state new-text :cursor-position (+ pos advance)))
        (chat-ui-set-input state input-text))))

(defun %chat-panel-input-handle-plan-selection (context delta)
  (%chat-plan-move-selection! (chat-panel-input-key-context-state context) delta))

(defun %chat-panel-input-handle-history-search-toggle (context)
  (let ((state (chat-panel-input-key-context-state context)))
    (if (chat-ui-state-history-search-active-p state)
        (%chat-deactivate-history-search! state :restore-input-p t)
        (%chat-activate-history-search! state))))

(defun %chat-panel-input-handle-escape (context)
  (let ((state (chat-panel-input-key-context-state context)))
    (when (chat-ui-state-history-search-active-p state)
      (%chat-deactivate-history-search! state :restore-input-p t))
    t))

(defun %chat-panel-input-handle-newline (context)
  (let* ((state (chat-panel-input-key-context-state context))
         (input-text (chat-panel-input-key-context-input-text context))
         (pos (chat-panel-input-key-context-pos context))
         (new-text (%grapheme-insert-at input-text pos (string #\Newline))))
    (chat-ui-set-input state new-text :cursor-position (1+ pos))))

(defun %chat-panel-input-handle-submit (context)
  (let ((state (chat-panel-input-key-context-state context))
        (input-text (chat-panel-input-key-context-input-text context)))
    (%chat-input-handle-submit-routing state input-text))
  t)

(defun %chat-panel-input-handle-backspace (context)
  (let ((state (chat-panel-input-key-context-state context))
        (input-text (chat-panel-input-key-context-input-text context))
        (cur-pos (chat-panel-input-key-context-cur-pos context))
        (pos (chat-panel-input-key-context-pos context)))
    (if (null cur-pos)
        (chat-ui-set-input state (%pop-last-grapheme input-text))
        (multiple-value-bind (new-text new-pos)
            (%grapheme-delete-before input-text pos)
          (chat-ui-set-input state new-text :cursor-position new-pos)))
    t))

(defun %chat-panel-input-handle-delete (context)
  (let ((state (chat-panel-input-key-context-state context))
        (input-text (chat-panel-input-key-context-input-text context))
        (pos (chat-panel-input-key-context-pos context)))
    (chat-ui-set-input state (%grapheme-delete-at input-text pos)
                       :cursor-position pos)
    t))

(defun %chat-panel-input-handle-delete-word-backward (context)
  (let ((state (chat-panel-input-key-context-state context))
        (input-text (chat-panel-input-key-context-input-text context))
        (cur-pos (chat-panel-input-key-context-cur-pos context))
        (pos (chat-panel-input-key-context-pos context)))
    (if (null cur-pos)
        (chat-ui-set-input state (%delete-word-backward input-text))
        (multiple-value-bind (new-text new-pos)
            (%delete-word-backward-at input-text pos)
          (chat-ui-set-input state new-text :cursor-position new-pos)))
    t))

(defun %chat-panel-input-handle-kill-before-cursor (context)
  (let* ((state (chat-panel-input-key-context-state context))
         (input-text (chat-panel-input-key-context-input-text context))
         (pos (chat-panel-input-key-context-pos context))
         (clusters (ptui.text.grapheme:split-graphemes input-text))
         (after (with-output-to-string (out)
                  (loop for cluster in (nthcdr pos clusters)
                        do (write-string cluster out)))))
    (chat-ui-set-input state after :cursor-position 0)
    t))

(defun %chat-panel-input-handle-kill-after-cursor (context)
  (let* ((state (chat-panel-input-key-context-state context))
         (input-text (chat-panel-input-key-context-input-text context))
         (pos (chat-panel-input-key-context-pos context))
         (clusters (ptui.text.grapheme:split-graphemes input-text))
         (before (with-output-to-string (out)
                   (loop for cluster in (subseq clusters 0 (min pos (length clusters)))
                         do (write-string cluster out)))))
    (chat-ui-set-input state before :cursor-position pos)
    t))

(defun %chat-panel-input-set-cursor! (context position)
  (setf (chat-ui-state-cursor-position (chat-panel-input-key-context-state context))
        position)
  t)

(defun %chat-panel-input-handle-right (context)
  (let* ((state (chat-panel-input-key-context-state context))
         (input-text (chat-panel-input-key-context-input-text context))
         (pos (chat-panel-input-key-context-pos context))
         (len (%grapheme-length input-text)))
    (if (< pos len)
        (let ((new-pos (1+ pos)))
          (setf (chat-ui-state-cursor-position state)
                (if (= new-pos len) nil new-pos)))
        (setf (chat-ui-state-cursor-position state) nil))
    t))

(defun %chat-panel-input-handle-ctrl-right (context)
  (let* ((state (chat-panel-input-key-context-state context))
         (input-text (chat-panel-input-key-context-input-text context))
         (pos (chat-panel-input-key-context-pos context))
         (new-pos (%word-boundary-forward input-text pos))
         (len (%grapheme-length input-text)))
    (setf (chat-ui-state-cursor-position state)
          (if (>= new-pos len) nil new-pos))
    t))

(defun %chat-panel-input-handle-vertical-move (context delta)
  (let* ((state (chat-panel-input-key-context-state context))
         (input-width (chat-panel-input-key-context-input-width context))
         (cur-pos (chat-panel-input-key-context-cur-pos context)))
    (%chat-input-handle-vertical-cursor-move state cur-pos input-width delta))
  t)

(defparameter +chat-panel-input-key-handlers+
  (list (cons :text #'%chat-panel-input-handle-text)
        (cons :ctrl-p (lambda (context)
                        (%chat-panel-input-handle-plan-selection context -1)))
        (cons :ctrl-n (lambda (context)
                        (%chat-panel-input-handle-plan-selection context 1)))
        (cons :ctrl-r #'%chat-panel-input-handle-history-search-toggle)
        (cons :escape #'%chat-panel-input-handle-escape)
        (cons :ctrl-j #'%chat-panel-input-handle-newline)
        (cons :tab (lambda (context)
                     (%handle-command-tab-completion
                      (chat-panel-input-key-context-state context))))
        (cons :enter #'%chat-panel-input-handle-submit)
        (cons :return #'%chat-panel-input-handle-submit)
        (cons :backspace #'%chat-panel-input-handle-backspace)
        (cons :delete #'%chat-panel-input-handle-delete)
        (cons :ctrl-w #'%chat-panel-input-handle-delete-word-backward)
        (cons :ctrl-u #'%chat-panel-input-handle-kill-before-cursor)
        (cons :ctrl-k #'%chat-panel-input-handle-kill-after-cursor)
        (cons :ctrl-a (lambda (context)
                        (%chat-panel-input-set-cursor! context 0)))
        (cons :ctrl-e (lambda (context)
                        (%chat-panel-input-set-cursor! context nil)))
        (cons :left (lambda (context)
                      (let ((pos (chat-panel-input-key-context-pos context)))
                        (when (> pos 0)
                          (%chat-panel-input-set-cursor! context (1- pos)))
                        t)))
        (cons :right #'%chat-panel-input-handle-right)
        (cons :ctrl-left (lambda (context)
                           (%chat-panel-input-set-cursor!
                            context
                            (%word-boundary-backward
                             (chat-panel-input-key-context-input-text context)
                             (chat-panel-input-key-context-pos context)))))
        (cons :ctrl-right #'%chat-panel-input-handle-ctrl-right)
        (cons :home (lambda (context)
                      (%chat-panel-input-set-cursor! context 0)))
        (cons :end (lambda (context)
                     (%chat-panel-input-set-cursor! context nil)))
        (cons :up (lambda (context)
                    (%chat-panel-input-handle-vertical-move context -1)))
        (cons :down (lambda (context)
                      (%chat-panel-input-handle-vertical-move context 1))))
  "Key dispatch table for prompt-input editing actions.")

(defun %chat-panel-input-key-handler (key)
  (cdr (assoc key +chat-panel-input-key-handlers+ :test #'eq)))

(defun chat-panel-handle-input-key (state key text inner-width)
  (let ((handler (%chat-panel-input-key-handler key)))
    (when handler
      (funcall handler (%chat-panel-input-key-context state key text inner-width)))))

(ptui.ui.panel:defpanel prompt-input-panel (chat-state inner-width)
  (:layout
    (:column
      (input :fixed 4
        (ptui.components.prompt-box:make-prompt-box-widget
         (chat-ui-state-input-text chat-state)
         :id :chat-input
         :min-width 18
         :max-width inner-width
         :min-rows 1
         :max-rows 4
         :scroll-offset (chat-ui-state-prompt-scroll-offset chat-state)
         :cursor-position (chat-ui-state-cursor-position chat-state)
         :cursor-visible-p t
         :border-style :rounded))))
  (:keys
    (:text (chat-panel-handle-input-key
             chat-state
             :text
             (ptui.core.events:key-event-text? ptui.ui.panel::event)
             inner-width))
    (:enter (chat-panel-handle-input-key chat-state :enter nil inner-width))
    (:backspace (chat-panel-handle-input-key chat-state :backspace nil inner-width))
    (:delete (chat-panel-handle-input-key chat-state :delete nil inner-width))
    (:ctrl-j (chat-panel-handle-input-key chat-state :ctrl-j nil inner-width))
    (:tab (chat-panel-handle-input-key chat-state :tab nil inner-width))
    (:up (chat-panel-handle-input-key chat-state :up nil inner-width))
    (:down (chat-panel-handle-input-key chat-state :down nil inner-width))
    (:ctrl-p (chat-panel-handle-input-key chat-state :ctrl-p nil inner-width))
    (:ctrl-n (chat-panel-handle-input-key chat-state :ctrl-n nil inner-width))
    (:ctrl-r (chat-panel-handle-input-key chat-state :ctrl-r nil inner-width))
    (:ctrl-a (chat-panel-handle-input-key chat-state :ctrl-a nil inner-width))
    (:ctrl-e (chat-panel-handle-input-key chat-state :ctrl-e nil inner-width))
    (:left (chat-panel-handle-input-key chat-state :left nil inner-width))
    (:right (chat-panel-handle-input-key chat-state :right nil inner-width))
    (:ctrl-left (chat-panel-handle-input-key chat-state :ctrl-left nil inner-width))
    (:ctrl-right (chat-panel-handle-input-key chat-state :ctrl-right nil inner-width))
    (:home (chat-panel-handle-input-key chat-state :home nil inner-width))
    (:end (chat-panel-handle-input-key chat-state :end nil inner-width))
    (:ctrl-w (chat-panel-handle-input-key chat-state :ctrl-w nil inner-width))
    (:ctrl-u (chat-panel-handle-input-key chat-state :ctrl-u nil inner-width))
    (:ctrl-k (chat-panel-handle-input-key chat-state :ctrl-k nil inner-width))))


(defun amoebum::%handle-input-key (chat-state key text &optional (inner-width 80))
  "Compatibility shim for smoke tests that still call the legacy helper."
  (chat-panel-handle-input-key chat-state key text inner-width))
