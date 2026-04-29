(in-package :amoebum)

;;; NXT-575: REPL panel widget — chat overlay that evaluates Common Lisp via
;;; `sandboxed-eval`. State and eval logic live in ui/repl-panel-state.lisp;
;;; this file is render + key-routing only.

(defun %repl-panel-history-row (entry index width)
  (declare (ignore width))
  (let* ((input (or (getf entry :input) ""))
         (output (getf entry :output))
         (error-text (getf entry :error))
         (text (cond
                 (error-text
                  (format nil "> ~A~%!! ~A" input error-text))
                 (t
                  (format nil "> ~A~%=> ~A" input (or output "nil"))))))
    (ptui.ui.elements:make-element
     :text
     :id (list :repl-history-row index)
     :props (list :text text
                  :role (if error-text :error :assistant))
     :children '())))

(defun %repl-panel-history-children (state width)
  (let ((entries (reverse (repl-state-history state))))
    (if (null entries)
        (list (ptui.ui.elements:make-element
               :text
               :id :repl-history-empty
               :props (list :text "  (REPL ready — type a Lisp form and press Enter)"
                            :role :meta)
               :children '()))
        (loop for entry in entries
              for index from 0
              collect (%repl-panel-history-row entry index width)))))

(defun %repl-panel-prompt-text (state)
  (format nil "lisp> ~A" (repl-state-input-text state)))

(defun chat-panel-handle-repl-key (chat-state key value inner-width)
  "Route a key event to the REPL panel state. Returns T when the key was
handled by the REPL (so the caller can suppress further dispatch)."
  (declare (ignore inner-width))
  (let ((state (chat-ui-state-repl-panel-state chat-state)))
    (when (repl-state-active-p state)
      (case key
        (:enter
         (repl-state-submit-input! state)
         t)
        (:escape
         (setf (repl-state-active-p state) nil)
         t)
        (:backspace
         (repl-state-backspace! state)
         t)
        (:ctrl-u
         (repl-state-clear-input! state)
         t)
        (:text
         (repl-state-append-text! state value)
         t)
        (otherwise
         nil)))))

(ptui.widgets.defwidget:defwidget make-repl-panel-widget (state)
  (:memoize nil)
  (let* ((history (vstack
                   (map-widget #'identity
                               (%repl-panel-history-children state 60))))
         (prompt (ptui.ui.elements:make-element
                  :text
                  :id :repl-panel-prompt
                  :props (list :text (%repl-panel-prompt-text state)
                               :role :user)
                  :children '())))
    (ptui.ui.elements:make-element
     :box
     :id :repl-panel
     :props (list :border :rounded)
     :children (list (vstack history prompt)))))

(ptui.ui.panel:defpanel repl-panel (chat-state)
  (:data
    (state (chat-ui-state-repl-panel-state chat-state) :deps (chat-state))
    (active-p (repl-state-active-p state)
      :deps ((repl-state-active-p state))))
  (:layout
    (:column
      (repl :flex 1 :when active-p
        (make-repl-panel-widget state)))))
