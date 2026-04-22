(in-package :amoebum)

;;; Streaming overlay ownership extracted mechanically from ui/chat-render.lisp
;;; for NXT-384. Keep render output semantics unchanged while the facade keeps
;;; approval/plan/tree assembly local.

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
