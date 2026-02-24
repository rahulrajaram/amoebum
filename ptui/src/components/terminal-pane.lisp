(defpackage :ptui.components.terminal-pane
  (:use :cl)
  (:export
   #:terminal-pane-state
   #:make-terminal-pane-state
   #:terminal-pane-title
   #:terminal-pane-lines
   #:terminal-pane-pending-output
   #:terminal-pane-max-lines
   #:terminal-pane-scroll-offset
   #:terminal-pane-status
   #:terminal-pane-empty-message
   #:terminal-pane-append-line
   #:terminal-pane-append-output
   #:terminal-pane-clear
   #:terminal-pane-scroll
   #:terminal-pane-scroll-home
   #:terminal-pane-scroll-end
   #:terminal-pane-visible-lines
   #:terminal-pane-handle-event
   #:make-terminal-pane-widget))

(in-package :ptui.components.terminal-pane)

(defstruct (terminal-pane-state
            (:constructor %make-terminal-pane-state
                (&key
                  (title "terminal")
                  (lines '())
                  (pending-output "")
                  (max-lines 2000)
                  (scroll-offset 0)
                  (status :idle)
                  (empty-message "[no output]"))))
  (title "terminal" :type string)
  (lines '() :type list)
  (pending-output "" :type string)
  (max-lines 2000 :type fixnum)
  (scroll-offset 0 :type fixnum)
  (status :idle :type keyword)
  (empty-message "[no output]" :type string))

(defun %normalize-line-text (value)
  (let* ((text (typecase value
                 (string value)
                 (pathname (namestring value))
                 (t (princ-to-string value))))
         (length (length text)))
    (if (and (> length 0)
             (char= (char text (1- length)) #\Return))
        (subseq text 0 (1- length))
        text)))

(defun %trim-lines (lines max-lines)
  (let ((overflow (- (length lines) max-lines)))
    (if (> overflow 0)
        (nthcdr overflow lines)
        lines)))

(defun make-terminal-pane-state (&key
                                   (title "terminal")
                                   (lines '())
                                   (pending-output "")
                                   (max-lines 2000)
                                   (scroll-offset 0)
                                   (status :idle)
                                   (empty-message "[no output]"))
  (check-type title string)
  (check-type pending-output string)
  (check-type max-lines (integer 1 *))
  (check-type scroll-offset (integer 0 *))
  (check-type empty-message string)
  (let* ((normalized-lines (mapcar #'%normalize-line-text (or lines '())))
         (trimmed-lines (%trim-lines normalized-lines max-lines)))
    (%make-terminal-pane-state :title title
                               :lines trimmed-lines
                               :pending-output pending-output
                               :max-lines max-lines
                               :scroll-offset scroll-offset
                               :status status
                               :empty-message empty-message)))

(defun terminal-pane-title (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-title state))

(defun terminal-pane-lines (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-lines state))

(defun terminal-pane-pending-output (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-pending-output state))

(defun terminal-pane-max-lines (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-max-lines state))

(defun terminal-pane-scroll-offset (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-scroll-offset state))

(defun terminal-pane-status (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-status state))

(defun terminal-pane-empty-message (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-empty-message state))

(defun %display-lines (state &key (include-pending t))
  (check-type state terminal-pane-state)
  (let ((lines (copy-list (or (terminal-pane-state-lines state) '())))
        (pending (terminal-pane-state-pending-output state)))
    (if (and include-pending
             (stringp pending)
             (> (length pending) 0))
        (nconc lines (list pending))
        lines)))

(defun %max-scroll-offset (state viewport-height)
  (let ((line-count (length (%display-lines state))))
    (max 0 (- line-count viewport-height))))

(defun %clamp-scroll-offset! (state &key viewport-height)
  (let* ((height (or viewport-height (length (%display-lines state))))
         (max-offset (%max-scroll-offset state (max 1 height))))
    (setf (terminal-pane-state-scroll-offset state)
          (max 0
               (min (terminal-pane-state-scroll-offset state)
                    max-offset))))
  state)

(defun %refresh-status! (state)
  (setf (terminal-pane-state-status state)
        (if (or (terminal-pane-state-lines state)
                (> (length (terminal-pane-state-pending-output state)) 0))
            :active
            :idle))
  state)

(defun %split-output-lines (text)
  (let ((start 0)
        (length (length text))
        (complete-lines '()))
    (loop for index from 0 below length do
      (when (char= (char text index) #\Newline)
        (push (%normalize-line-text (subseq text start index)) complete-lines)
        (setf start (1+ index))))
    (values (nreverse complete-lines)
            (if (< start length)
                (subseq text start)
                ""))))

(defun terminal-pane-append-line (state line)
  "Append one completed output line."
  (check-type state terminal-pane-state)
  (let* ((normalized (%normalize-line-text line))
         (next-lines (nconc (terminal-pane-state-lines state)
                            (list normalized))))
    (setf (terminal-pane-state-lines state)
          (%trim-lines next-lines (terminal-pane-state-max-lines state)))
    (%clamp-scroll-offset! state)
    (%refresh-status! state))
  state)

(defun terminal-pane-append-output (state output)
  "Append raw output chunk, preserving trailing partial line state."
  (check-type state terminal-pane-state)
  (check-type output string)
  (unless (zerop (length output))
    (let* ((combined (if (zerop (length (terminal-pane-state-pending-output state)))
                         output
                         (concatenate 'string
                                      (terminal-pane-state-pending-output state)
                                      output))))
      (multiple-value-bind (complete pending)
          (%split-output-lines combined)
        (dolist (line complete)
          (terminal-pane-append-line state line))
        (setf (terminal-pane-state-pending-output state) pending))))
  (%refresh-status! state)
  state)

(defun terminal-pane-clear (state)
  "Clear all buffered terminal content."
  (check-type state terminal-pane-state)
  (setf (terminal-pane-state-lines state) '()
        (terminal-pane-state-pending-output state) ""
        (terminal-pane-state-scroll-offset state) 0)
  (%refresh-status! state)
  state)

(defun terminal-pane-visible-lines (state &key (viewport-height 12) (include-pending t))
  "Return visible lines for the current scroll offset."
  (check-type state terminal-pane-state)
  (check-type viewport-height (integer 1 *))
  (let* ((lines (%display-lines state :include-pending include-pending))
         (count (length lines))
         (max-offset (max 0 (- count viewport-height)))
         (offset (max 0 (min (terminal-pane-state-scroll-offset state)
                             max-offset)))
         (end (max 0 (- count offset)))
         (start (max 0 (- end viewport-height))))
    (setf (terminal-pane-state-scroll-offset state) offset)
    (if (>= start end)
        '()
        (subseq lines start end))))

(defun terminal-pane-scroll (state delta &key (viewport-height 12))
  "Adjust scroll offset by DELTA; positive values scroll back in history."
  (check-type state terminal-pane-state)
  (check-type delta integer)
  (check-type viewport-height (integer 1 *))
  (let* ((max-offset (%max-scroll-offset state viewport-height))
         (next-offset (+ (terminal-pane-state-scroll-offset state) delta)))
    (setf (terminal-pane-state-scroll-offset state)
          (max 0 (min max-offset next-offset))))
  state)

(defun terminal-pane-scroll-home (state &key (viewport-height 12))
  "Jump to the oldest visible window."
  (check-type state terminal-pane-state)
  (check-type viewport-height (integer 1 *))
  (setf (terminal-pane-state-scroll-offset state)
        (%max-scroll-offset state viewport-height))
  state)

(defun terminal-pane-scroll-end (state)
  "Jump to latest output."
  (check-type state terminal-pane-state)
  (setf (terminal-pane-state-scroll-offset state) 0)
  state)

(defun terminal-pane-handle-event (state event &key (viewport-height 12))
  "Apply key navigation event to STATE and return action metadata."
  (check-type state terminal-pane-state)
  (check-type viewport-height (integer 1 *))
  (unless (typep event 'ptui.core.events:key-event)
    (return-from terminal-pane-handle-event
      (list :action :ignored :state state)))
  (let* ((key (ptui.core.events:key-event-key event))
         (page-step (max 1 (1- viewport-height))))
    (cond
      ((eq key :up)
       (terminal-pane-scroll state 1 :viewport-height viewport-height)
       (list :action :scrolled :delta 1 :state state))
      ((eq key :down)
       (terminal-pane-scroll state -1 :viewport-height viewport-height)
       (list :action :scrolled :delta -1 :state state))
      ((eq key :page-up)
       (terminal-pane-scroll state page-step :viewport-height viewport-height)
       (list :action :scrolled :delta page-step :state state))
      ((eq key :page-down)
       (terminal-pane-scroll state (- page-step) :viewport-height viewport-height)
       (list :action :scrolled :delta (- page-step) :state state))
      ((eq key :home)
       (terminal-pane-scroll-home state :viewport-height viewport-height)
       (list :action :scrolled-home :state state))
      ((eq key :end)
       (terminal-pane-scroll-end state)
       (list :action :scrolled-end :state state))
      (t
       (list :action :ignored :state state)))))

(defun %status-line (state)
  (let* ((line-count (length (%display-lines state)))
         (offset (terminal-pane-state-scroll-offset state))
         (partialp (> (length (terminal-pane-state-pending-output state)) 0)))
    (format nil "~A | ~A | ~D line~:P~@[ | +~D~]~:[~; | partial~]"
            (terminal-pane-state-title state)
            (terminal-pane-state-status state)
            line-count
            (and (> offset 0) offset)
            partialp)))

(defun make-terminal-pane-widget (state &key id key (borderp t) (padding 0) (viewport-height 12))
  "Build a reusable terminal pane element tree from STATE."
  (check-type state terminal-pane-state)
  (check-type viewport-height (integer 1 *))
  (let* ((status-widget (ptui.widgets.core:make-text-widget (%status-line state)))
         (line-widgets
           (let ((visible (terminal-pane-visible-lines state :viewport-height viewport-height)))
             (if visible
                 (loop for line in visible
                       collect (ptui.widgets.core:make-text-widget line))
                 (list (ptui.widgets.core:make-text-widget
                        (terminal-pane-state-empty-message state))))))
         (content (ptui.widgets.core:make-stack-widget
                   (append (list status-widget) line-widgets)
                   :direction :column
                   :gap 0)))
    (ptui.widgets.core:make-box-widget content
                                       :id id
                                       :key key
                                       :padding padding
                                       :borderp borderp)))
