(defpackage :ptui.components.glob-widget
  (:use :cl)
  (:export
   #:glob-widget-stream
   #:make-glob-widget-stream
   #:make-sequence-glob-stream
   #:glob-widget-state
   #:make-glob-widget-state
   #:glob-widget-pattern
   #:glob-widget-status
   #:glob-widget-matches
   #:glob-widget-selected-index
   #:glob-widget-visible-count
   #:glob-widget-batch-size
   #:glob-widget-start
   #:glob-widget-step
   #:glob-widget-cancel
   #:glob-widget-handle-event
   #:glob-widget-selected-match
   #:glob-widget-visible-matches
   #:make-glob-widget))

(in-package :ptui.components.glob-widget)

(defstruct (glob-widget-stream
            (:constructor %make-glob-widget-stream
                (&key next-fn cancel-fn)))
  (next-fn (lambda () (values nil t)) :type function)
  (cancel-fn (lambda () nil) :type function)
  (done-p nil :type boolean)
  (cancelled-p nil :type boolean))

(defstruct (glob-widget-state
            (:constructor %make-glob-widget-state
                (&key
                  (pattern "")
                  (matcher #'%default-glob-match-p)
                  (candidate->string #'princ-to-string)
                  stream
                  (candidates (make-array 0 :element-type 'string :adjustable t :fill-pointer 0))
                  (matches '())
                  (selected-index 0)
                  (visible-count 10)
                  (batch-size 64)
                  (status :idle)
                  (prompt "glob> ")
                  (empty-message "[no matches]")
                  on-select)))
  (pattern "" :type string)
  (matcher #'%default-glob-match-p :type function)
  (candidate->string #'princ-to-string :type function)
  (stream nil :type (or null glob-widget-stream))
  (candidates (make-array 0 :element-type 'string :adjustable t :fill-pointer 0) :type vector)
  (matches '() :type list)
  (selected-index 0 :type fixnum)
  (visible-count 10 :type fixnum)
  (batch-size 64 :type fixnum)
  (status :idle :type keyword)
  (prompt "glob> " :type string)
  (empty-message "[no matches]" :type string)
  (on-select nil :type (or null function)))

(defun glob-widget-pattern (state)
  (glob-widget-state-pattern state))

(defun (setf glob-widget-pattern) (value state)
  (check-type value string)
  (setf (glob-widget-state-pattern state) value))

(defun glob-widget-status (state)
  (glob-widget-state-status state))

(defun glob-widget-matches (state)
  (glob-widget-state-matches state))

(defun glob-widget-selected-index (state)
  (glob-widget-state-selected-index state))

(defun (setf glob-widget-selected-index) (value state)
  (check-type value (integer 0 *))
  (setf (glob-widget-state-selected-index state) value))

(defun glob-widget-visible-count (state)
  (glob-widget-state-visible-count state))

(defun glob-widget-batch-size (state)
  (glob-widget-state-batch-size state))

(defun make-glob-widget-stream (&key next cancel)
  "Create a streaming candidate source for a glob widget.
NEXT must return two values: CANDIDATE and DONE-P."
  (check-type next function)
  (let ((cancel-fn (or cancel (lambda () nil))))
    (check-type cancel-fn function)
    (%make-glob-widget-stream :next-fn next
                              :cancel-fn cancel-fn)))

(defun make-sequence-glob-stream (candidates)
  "Create a deterministic stream backed by CANDIDATES."
  (let* ((vector (coerce candidates 'vector))
         (cursor 0))
    (make-glob-widget-stream
     :next (lambda ()
             (if (>= cursor (length vector))
                 (values nil t)
                 (prog1
                     (values (aref vector cursor)
                             (= (1+ cursor) (length vector)))
                   (incf cursor))))
     :cancel (lambda ()
               (setf cursor (length vector))
               t))))

(defun %glob-match-at-p (pattern candidate pattern-index candidate-index)
  (loop
    with pattern-length = (length pattern)
    with candidate-length = (length candidate)
    with p = pattern-index
    with c = candidate-index
    with star = nil
    with match = 0
    do (cond
         ((and (< p pattern-length)
               (char= (char pattern p) #\*))
          (setf star p
                match c)
          (incf p))
         ((and (< p pattern-length)
               (< c candidate-length)
               (or (char= (char pattern p) #\?)
                   (char-equal (char pattern p) (char candidate c))))
          (incf p)
          (incf c))
         (star
          (setf p (1+ star))
          (incf match)
          (setf c match))
         (t
          (return nil)))
       (when (>= c candidate-length)
         (loop while (and (< p pattern-length)
                          (char= (char pattern p) #\*))
               do (incf p))
         (return (= p pattern-length)))))

(defun %default-glob-match-p (pattern candidate)
  "Lightweight fallback glob matcher supporting * and ?."
  (let ((glob (or pattern ""))
        (text (or candidate "")))
    (%glob-match-at-p glob text 0 0)))

(defun %normalize-candidate (state candidate)
  (let ((value (funcall (glob-widget-state-candidate->string state) candidate)))
    (if (stringp value)
        value
        (princ-to-string value))))

(defun %clamp-selected-index! (state)
  (let ((count (length (glob-widget-state-matches state))))
    (setf (glob-widget-state-selected-index state)
          (if (zerop count)
              0
              (max 0 (min (glob-widget-state-selected-index state)
                          (1- count)))))))

(defun %recompute-matches! (state)
  (let ((pattern (glob-widget-state-pattern state))
        (matcher (glob-widget-state-matcher state))
        (matches '()))
    (loop for candidate across (glob-widget-state-candidates state) do
      (when (funcall matcher pattern candidate)
        (push candidate matches)))
    (setf (glob-widget-state-matches state) (nreverse matches))
    (%clamp-selected-index! state)))

(defun make-glob-widget-state (&key
                                 (pattern "")
                                 (matcher #'%default-glob-match-p)
                                 (candidate->string #'princ-to-string)
                                 stream
                                 (visible-count 10)
                                 (batch-size 64)
                                 (prompt "glob> ")
                                 (empty-message "[no matches]")
                                 on-select)
  "Create mutable state for the glob widget."
  (check-type pattern string)
  (check-type matcher function)
  (check-type candidate->string function)
  (check-type visible-count (integer 1 *))
  (check-type batch-size (integer 1 *))
  (check-type prompt string)
  (check-type empty-message string)
  (when stream
    (check-type stream glob-widget-stream))
  (when on-select
    (check-type on-select function))
  (%make-glob-widget-state :pattern pattern
                           :matcher matcher
                           :candidate->string candidate->string
                           :stream stream
                           :visible-count visible-count
                           :batch-size batch-size
                           :status (if stream :streaming :idle)
                           :prompt prompt
                           :empty-message empty-message
                           :on-select on-select))

(defun glob-widget-start (state stream &key pattern)
  "Reset STATE and attach STREAM for a fresh traversal."
  (check-type state glob-widget-state)
  (check-type stream glob-widget-stream)
  (when pattern
    (check-type pattern string))
  (setf (glob-widget-state-pattern state) (or pattern (glob-widget-state-pattern state))
        (glob-widget-state-stream state) stream
        (glob-widget-state-candidates state) (make-array 0 :element-type 'string :adjustable t :fill-pointer 0)
        (glob-widget-state-matches state) '()
        (glob-widget-state-selected-index state) 0
        (glob-widget-state-status state) :streaming)
  state)

(defun glob-widget-step (state &key max-items)
  "Advance streaming traversal and apply current glob pattern."
  (check-type state glob-widget-state)
  (let* ((limit (or max-items (glob-widget-state-batch-size state)))
         (stream (glob-widget-state-stream state))
         (consumed 0)
         (matched 0))
    (check-type limit (integer 1 *))
    (cond
      ((null stream)
       (values state consumed matched))
      ((member (glob-widget-state-status state) '(:cancelled :done) :test #'eq)
       (values state consumed matched))
      (t
       (loop while (< consumed limit) do
         (multiple-value-bind (candidate done-p)
             (funcall (glob-widget-stream-next-fn stream))
           (when candidate
             (incf consumed)
             (let ((text (%normalize-candidate state candidate)))
               (vector-push-extend text (glob-widget-state-candidates state))
               (when (funcall (glob-widget-state-matcher state)
                              (glob-widget-state-pattern state)
                              text)
                 (setf (glob-widget-state-matches state)
                       (nconc (glob-widget-state-matches state) (list text)))
                 (incf matched))))
           (when done-p
             (setf (glob-widget-stream-done-p stream) t
                   (glob-widget-state-status state) :done
                   (glob-widget-state-stream state) nil)
             (return))
           (when (null candidate)
             ;; No candidate available in this tick; caller can poll again.
             (return))))
       (when (and (glob-widget-state-stream state)
                  (not (eq (glob-widget-state-status state) :cancelled)))
         (setf (glob-widget-state-status state) :streaming))
       (%clamp-selected-index! state)
       (values state consumed matched)))))

(defun glob-widget-cancel (state)
  "Cancel streaming traversal."
  (check-type state glob-widget-state)
  (let ((stream (glob-widget-state-stream state)))
    (when stream
      (setf (glob-widget-stream-cancelled-p stream) t)
      (funcall (glob-widget-stream-cancel-fn stream))))
  (setf (glob-widget-state-stream state) nil
        (glob-widget-state-status state) :cancelled)
  state)

(defun glob-widget-selected-match (state)
  "Return the currently selected match string or NIL."
  (check-type state glob-widget-state)
  (nth (glob-widget-state-selected-index state)
       (glob-widget-state-matches state)))

(defun %visible-window (state)
  (let* ((matches (glob-widget-state-matches state))
         (count (length matches))
         (visible (glob-widget-state-visible-count state))
         (selected (glob-widget-state-selected-index state))
         (start (if (<= count visible)
                    0
                    (min (max 0 (- selected (1- visible)))
                         (- count visible))))
         (end (min count (+ start visible))))
    (values (subseq matches start end) start)))

(defun glob-widget-visible-matches (state)
  "Return currently visible matches."
  (check-type state glob-widget-state)
  (nth-value 0 (%visible-window state)))

(defun %set-pattern! (state pattern)
  (setf (glob-widget-state-pattern state) pattern)
  (%recompute-matches! state))

(defun %move-selection! (state key)
  (let ((count (length (glob-widget-state-matches state))))
    (cond
      ((zerop count)
       (setf (glob-widget-state-selected-index state) 0))
      ((eq key :up)
       (setf (glob-widget-state-selected-index state)
             (max 0 (1- (glob-widget-state-selected-index state)))))
      ((eq key :down)
       (setf (glob-widget-state-selected-index state)
             (min (1- count) (1+ (glob-widget-state-selected-index state)))))
      ((eq key :home)
       (setf (glob-widget-state-selected-index state) 0))
      ((eq key :end)
       (setf (glob-widget-state-selected-index state) (1- count)))))
  state)

(defun glob-widget-handle-event (state event)
  "Apply EVENT to STATE and return a plist describing the action."
  (check-type state glob-widget-state)
  (unless (typep event 'ptui.core.events:key-event)
    (return-from glob-widget-handle-event
      (list :action :ignored :state state)))
  (let* ((key (ptui.core.events:key-event-key event))
         (text (ptui.core.events:key-event-text? event)))
    (cond
      ((and (stringp text) (> (length text) 0))
       (%set-pattern! state (concatenate 'string (glob-widget-state-pattern state) text))
       (list :action :pattern-updated :state state))
      ((eq key :backspace)
       (let ((pattern (glob-widget-state-pattern state)))
         (when (> (length pattern) 0)
           (%set-pattern! state (subseq pattern 0 (1- (length pattern))))))
       (list :action :pattern-updated :state state))
      ((member key '(:up :down :home :end) :test #'eq)
       (%move-selection! state key)
       (list :action :selection-moved :state state))
      ((member key '(:escape :ctrl-c) :test #'eq)
       (glob-widget-cancel state)
       (list :action :cancelled :state state))
      ((eq key :enter)
       (let ((selected (glob-widget-selected-match state)))
         (when (and selected (glob-widget-state-on-select state))
           (funcall (glob-widget-state-on-select state) selected state))
         (list :action :selected
               :state state
               :match selected)))
      (t
       (list :action :ignored :state state)))))

(defun %status-line (state)
  (let ((count (length (glob-widget-state-matches state))))
    (format nil "~A (~D match~:P)"
            (case (glob-widget-state-status state)
              (:streaming "scanning")
              (:done "done")
              (:cancelled "cancelled")
              (t "idle"))
            count)))

(defun make-glob-widget (state &key id key (input-id :glob-input) (borderp t) (padding 0))
  "Build a composable PTUI element tree for glob interaction."
  (check-type state glob-widget-state)
  (let* ((input (ptui.widgets.core:make-input-widget
                 (glob-widget-state-pattern state)
                 :id input-id
                 :min-width 1
                 :on-event (lambda (event node)
                             (declare (ignore node))
                             (glob-widget-handle-event state event))))
         (status (ptui.widgets.core:make-text-widget (%status-line state)))
         (rows
           (multiple-value-bind (visible start) (%visible-window state)
             (if visible
                 (loop for match in visible
                       for offset from 0
                       for absolute = (+ start offset)
                       collect (ptui.widgets.core:make-text-widget
                                (format nil "~A ~A"
                                        (if (= absolute (glob-widget-state-selected-index state))
                                            ">"
                                            " ")
                                        match)))
                 (list (ptui.widgets.core:make-text-widget
                        (glob-widget-state-empty-message state))))))
         (content (ptui.widgets.core:make-stack-widget
                   (append (list input status) rows)
                   :direction :column
                   :gap 0)))
    (ptui.widgets.core:make-box-widget content
                                       :id id
                                       :key key
                                       :padding padding
                                       :borderp borderp)))
