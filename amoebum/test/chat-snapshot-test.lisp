(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Chat UI Snapshot Tests (I140)
;;; ---------------------------------------------------------------------------

(def-suite chat-snapshot-suite :in amoebum-suite
  :description "Chat UI snapshot coverage for message area, tool preview, status, and empty states.")

(in-suite chat-snapshot-suite)

(defparameter +chat-snapshot-dir*
  (asdf:system-relative-pathname "amoebum" "test/snapshots/"))

(defun chat-snapshot-path (name)
  (merge-pathnames (pathname (format nil "~A.snap" name)) +chat-snapshot-dir*))

(defun %assert-chat-snapshot (buffer name)
  (multiple-value-bind (pass diff)
      (ptui.test-support.harness:assert-snapshot
       buffer
       (chat-snapshot-path name)
       :test-name (string-capitalize (princ-to-string name)))
    (is (eq t pass))
    (is (null diff))))

(defun %snapshot-status-bar-state (&key
                                   (branch-name "main")
                                   (model-name "gpt-4o-mini")
                                   (context-window-limit 12000)
                                   stream-summary)
  (let* ((event-bus (amoebum:make-event-bus :capacity 16))
         (state (amoebum:make-status-bar-state
                 :branch-name branch-name
                 :model-name model-name
                 :permission-mode :full-auto
                 :context-window-limit context-window-limit
                 :event-bus event-bus)))
    (when stream-summary
      (amoebum:publish-status-bar-stream-summary stream-summary
                                                :event-bus event-bus))
    state))

(defun %snapshot-chat-state (&key
                              messages
                              status-bar-state)
  (let ((*default-pathname-defaults*
          (pathname "/home/rahul/Documents/amoebum/"))
        (amoebum::*current-config* nil))
    (ignore-errors (amoebum::drain-voice-transcriptions))
    (let ((state (amoebum:make-chat-ui-state
                  :status-bar-state (or status-bar-state
                                        (%snapshot-status-bar-state)))))
      (dolist (message messages)
        (amoebum:chat-ui-add-message state
                                     (first message)
                                     (second message)))
      state)))

(defun %render-chat-ui (state &key (cols 84) (rows 20))
  (amoebum:render-chat-ui-buffer
   state
   (ptui.core.types:make-size cols rows)))

(defun %status-line-buffer (status-state &key (cols 84))
  (let ((buffer (ptui.render.buffer:make-buffer cols 1)))
    (ptui.render.buffer:buffer-draw-text
     buffer
     0
     0
     (amoebum:status-bar-line status-state))
    buffer))

(test chat-snapshot-message-area
  (let* ((state (%snapshot-chat-state
                 :messages '(("system" "System: respond concisely and cite sources only.")
                             ("user" "Can you summarize the module layout for the chat UI?")
                             ("assistant" "The chat UI uses a box container, status bar, message history, and prompt input."))
                 :status-bar-state (%snapshot-status-bar-state :branch-name "feat/chat-snapshot")))
         (buffer (%render-chat-ui state :cols 84 :rows 20)))
    (%assert-chat-snapshot buffer "message-area")))

(test chat-snapshot-tool-call-preview
  (let* ((state (%snapshot-chat-state
                 :messages '(("assistant" "I will check symbols in the current tree."))
                 :status-bar-state (%snapshot-status-bar-state :branch-name "feat/chat-snapshot")))
         (tool-calls (amoebum:chat-ui-state-stream-tool-calls state)))
    (setf (gethash :preview tool-calls)
          (list :key :preview
                :tool-name "search_symbols"
                :arguments "{\"query\":\"chat\", \"limit\": 3}"))
    (%assert-chat-snapshot (%render-chat-ui state :cols 84 :rows 20)
                           "tool-call-preview")))

(test chat-snapshot-status-bar
  (let* ((status-state (%snapshot-status-bar-state
                       :branch-name "feat/chat-snapshot"
                       :model-name "gpt-4o"
                       :stream-summary (list :status :running
                                            :tokens 512
                                            :tokens-per-second 6.25d0
                                            :activep t)))
         (buffer (%status-line-buffer status-state :cols 84)))
    (%assert-chat-snapshot buffer "status-bar")))

(test chat-snapshot-empty-state
  (let* ((state (%snapshot-chat-state
                 :status-bar-state (%snapshot-status-bar-state :branch-name "feat/chat-snapshot")))
         (buffer (%render-chat-ui state :cols 84 :rows 20)))
    (%assert-chat-snapshot buffer "empty-state")))
