(defpackage :ptui.components.streaming-widget
  (:use :cl)
  (:export
   #:streaming-widget-state
   #:make-streaming-widget-state
   #:streaming-widget-text
   #:streaming-widget-lines
   #:streaming-widget-scroll-offset
   #:streaming-widget-scroll-follow-p
   #:streaming-widget-status
   #:streaming-widget-full-relayout-count
   #:streaming-widget-incremental-layout-count
   #:streaming-widget-cursor-visible-p
   #:streaming-widget-set-width
   #:streaming-widget-start
   #:streaming-widget-stop
   #:streaming-widget-clear
   #:streaming-widget-append-chunk
   #:streaming-widget-visible-lines
   #:streaming-widget-scroll
   #:streaming-widget-scroll-home
   #:streaming-widget-scroll-end
   #:streaming-widget-handle-event
   #:make-streaming-widget))

(in-package :ptui.components.streaming-widget)

(defun %make-string-vector ()
  (make-array 0 :element-type 'string :adjustable t :fill-pointer 0))

(defun %make-int-vector ()
  (make-array 0 :element-type 'integer :adjustable t :fill-pointer 0))

;; Monotonic time delegated to ptui.util.time:monotonic-ms

(defstruct (streaming-widget-state
            (:constructor %make-streaming-widget-state
                (&key
                  (raw-text "")
                  wrapped-lines
                  wrapped-widths
                  (viewport-width 80)
                  (viewport-height 12)
                  (scroll-offset 0)
                  (scroll-follow-p t)
                  (status :idle)
                  (cursor-glyph "|")
                  (cursor-blink-ms 450)
                  (empty-message "[no stream output]")
                  (full-relayout-count 0)
                  (incremental-layout-count 0))))
  (raw-text "" :type string)
  (wrapped-lines (%make-string-vector) :type vector)
  (wrapped-widths (%make-int-vector) :type vector)
  (viewport-width 80 :type fixnum)
  (viewport-height 12 :type fixnum)
  (scroll-offset 0 :type fixnum)
  (scroll-follow-p t :type boolean)
  (status :idle :type keyword)
  (cursor-glyph "|" :type string)
  (cursor-blink-ms 450 :type fixnum)
  (empty-message "[no stream output]" :type string)
  (full-relayout-count 0 :type fixnum)
  (incremental-layout-count 0 :type fixnum))

(defun %safe-width (state)
  (max 1 (streaming-widget-state-viewport-width state)))

(defun %safe-height (state)
  (max 1 (streaming-widget-state-viewport-height state)))

(defun %vector->list (vector)
  (loop for index from 0 below (length vector)
        collect (aref vector index)))

(defun %ensure-lines-seeded! (state)
  (let ((lines (streaming-widget-state-wrapped-lines state))
        (widths (streaming-widget-state-wrapped-widths state)))
    (when (zerop (length lines))
      (vector-push-extend "" lines)
      (vector-push-extend 0 widths)))
  state)

(defun %reset-wrapped-lines! (state)
  (setf (streaming-widget-state-wrapped-lines state) (%make-string-vector)
        (streaming-widget-state-wrapped-widths state) (%make-int-vector))
  (%ensure-lines-seeded! state))

(defun %append-char-to-wrapped! (state char)
  (%ensure-lines-seeded! state)
  (let* ((lines (streaming-widget-state-wrapped-lines state))
         (widths (streaming-widget-state-wrapped-widths state))
         (index (max 0 (1- (length lines))))
         (width-limit (%safe-width state)))
    (cond
      ((char= char #\Return)
       state)
      ((char= char #\Newline)
       (vector-push-extend "" lines)
       (vector-push-extend 0 widths)
       state)
      (t
       (let* ((glyph (string char))
              (glyph-width (ptui.text.width:string-width glyph))
              (current-line (aref lines index))
              (current-width (aref widths index)))
         (if (and (> current-width 0)
                  (> (+ current-width glyph-width) width-limit))
             (progn
               (vector-push-extend glyph lines)
               (vector-push-extend glyph-width widths))
             (setf (aref lines index) (concatenate 'string current-line glyph)
                   (aref widths index) (+ current-width glyph-width))))
         state))))

(defun %rewrap-from-raw-text! (state)
  (%reset-wrapped-lines! state)
  (let ((text (streaming-widget-state-raw-text state)))
    (loop for char across text do
      (%append-char-to-wrapped! state char)))
  (incf (streaming-widget-state-full-relayout-count state))
  state)

(defun %line-count (state)
  (max 1 (length (streaming-widget-state-wrapped-lines state))))

(defun %max-scroll-offset (state viewport-height)
  (ptui.util.scroll:max-scroll-offset (%line-count state) viewport-height))

(defun %clamp-scroll-offset! (state &optional viewport-height)
  (let* ((height (or viewport-height (%safe-height state)))
         (offset (streaming-widget-state-scroll-offset state)))
    (setf (streaming-widget-state-scroll-offset state)
          (ptui.util.scroll:clamp-scroll-offset offset (%line-count state) height)))
  state)

(defun make-streaming-widget-state (&key
                                      (text "")
                                      (viewport-width 80)
                                      (viewport-height 12)
                                      (scroll-offset 0)
                                      (scroll-follow-p t)
                                      (status :idle)
                                      (cursor-glyph "|")
                                      (cursor-blink-ms 450)
                                      (empty-message "[no stream output]"))
  "Create mutable state for streaming text rendering."
  (check-type text string)
  (check-type viewport-width (integer 1 *))
  (check-type viewport-height (integer 1 *))
  (check-type scroll-offset (integer 0 *))
  (check-type cursor-glyph string)
  (check-type cursor-blink-ms (integer 80 *))
  (check-type empty-message string)
  (let ((state (%make-streaming-widget-state
                :raw-text text
                :wrapped-lines (%make-string-vector)
                :wrapped-widths (%make-int-vector)
                :viewport-width viewport-width
                :viewport-height viewport-height
                :scroll-offset scroll-offset
                :scroll-follow-p (not (null scroll-follow-p))
                :status status
                :cursor-glyph cursor-glyph
                :cursor-blink-ms cursor-blink-ms
                :empty-message empty-message)))
    (%rewrap-from-raw-text! state)
    (%clamp-scroll-offset! state viewport-height)
    state))

(defun streaming-widget-text (state)
  (check-type state streaming-widget-state)
  (streaming-widget-state-raw-text state))

(defun streaming-widget-lines (state)
  (check-type state streaming-widget-state)
  (%vector->list (streaming-widget-state-wrapped-lines state)))

(defun streaming-widget-scroll-offset (state)
  (check-type state streaming-widget-state)
  (streaming-widget-state-scroll-offset state))

(defun streaming-widget-scroll-follow-p (state)
  (check-type state streaming-widget-state)
  (not (null (streaming-widget-state-scroll-follow-p state))))

(defun streaming-widget-status (state)
  (check-type state streaming-widget-state)
  (streaming-widget-state-status state))

(defun streaming-widget-full-relayout-count (state)
  (check-type state streaming-widget-state)
  (streaming-widget-state-full-relayout-count state))

(defun streaming-widget-incremental-layout-count (state)
  (check-type state streaming-widget-state)
  (streaming-widget-state-incremental-layout-count state))

(defun streaming-widget-cursor-visible-p (state &key now-ms)
  (check-type state streaming-widget-state)
  (let* ((status (streaming-widget-state-status state)))
    (if (not (eq status :streaming))
        nil
        (let* ((blink-ms (max 80 (streaming-widget-state-cursor-blink-ms state)))
               (cycle (* 2 blink-ms))
               (phase (mod (or now-ms (ptui.util.time:monotonic-ms)) cycle)))
          (< phase blink-ms)))))

(defun streaming-widget-set-width (state width)
  (check-type state streaming-widget-state)
  (check-type width (integer 1 *))
  (unless (= width (streaming-widget-state-viewport-width state))
    (setf (streaming-widget-state-viewport-width state) width)
    (%rewrap-from-raw-text! state)
    (%clamp-scroll-offset! state))
  state)

(defun streaming-widget-start (state)
  (check-type state streaming-widget-state)
  (setf (streaming-widget-state-status state) :streaming)
  state)

(defun streaming-widget-stop (state)
  (check-type state streaming-widget-state)
  (setf (streaming-widget-state-status state) :done)
  state)

(defun streaming-widget-clear (state)
  (check-type state streaming-widget-state)
  (setf (streaming-widget-state-raw-text state) ""
        (streaming-widget-state-scroll-offset state) 0
        (streaming-widget-state-scroll-follow-p state) t
        (streaming-widget-state-status state) :idle
        (streaming-widget-state-incremental-layout-count state) 0)
  (%rewrap-from-raw-text! state)
  state)

(defun streaming-widget-append-chunk (state chunk)
  "Append CHUNK incrementally without forcing full text reflow."
  (check-type state streaming-widget-state)
  (let ((text (if (stringp chunk) chunk (princ-to-string (or chunk "")))))
    (when (plusp (length text))
      (setf (streaming-widget-state-status state) :streaming
            (streaming-widget-state-raw-text state)
            (concatenate 'string
                         (streaming-widget-state-raw-text state)
                         text))
      (loop for char across text do
        (%append-char-to-wrapped! state char))
      (incf (streaming-widget-state-incremental-layout-count state)
            (length text))
      (if (streaming-widget-state-scroll-follow-p state)
          (setf (streaming-widget-state-scroll-offset state) 0)
          (%clamp-scroll-offset! state))))
  state)

(defun %append-cursor-if-visible (state lines &key cursor-visible-p)
  (if (not cursor-visible-p)
      lines
      (let* ((cursor (streaming-widget-state-cursor-glyph state))
             (safe-lines (if lines (copy-list lines) (list "")))
             (cursor-width (ptui.text.width:string-width cursor))
             (last-index (1- (length safe-lines)))
             (last-line (nth last-index safe-lines))
             (last-width (ptui.text.width:string-width last-line))
             (max-width (%safe-width state)))
        (if (and (> last-width 0)
                 (> (+ last-width cursor-width) max-width))
            (append safe-lines (list cursor))
            (progn
              (setf (nth last-index safe-lines)
                    (concatenate 'string last-line cursor))
              safe-lines)))))

(defun streaming-widget-visible-lines (state
                                       &key
                                         (viewport-height (%safe-height state))
                                         now-ms
                                         (include-cursor-p t))
  "Return visible wrapped lines honoring scroll-offset and optional cursor."
  (check-type state streaming-widget-state)
  (check-type viewport-height (integer 1 *))
  (setf (streaming-widget-state-viewport-height state) viewport-height)
  (%clamp-scroll-offset! state viewport-height)
  (let* ((base-lines (streaming-widget-lines state))
         (cursor-visible-p (and include-cursor-p
                                (streaming-widget-cursor-visible-p state :now-ms now-ms)))
         (all-lines
           (if (and (zerop (length (streaming-widget-state-raw-text state)))
                    (not cursor-visible-p))
               (list (streaming-widget-state-empty-message state))
               (%append-cursor-if-visible state base-lines
                                          :cursor-visible-p cursor-visible-p)))
         (count (length all-lines))
         (offset (streaming-widget-state-scroll-offset state))
         (max-offset (max 0 (- count viewport-height)))
         (clamped-offset (max 0 (min max-offset offset)))
         (end (max 0 (- count clamped-offset)))
         (start (max 0 (- end viewport-height))))
    (setf (streaming-widget-state-scroll-offset state) clamped-offset)
    (if (>= start end)
        (list (streaming-widget-state-empty-message state))
        (subseq all-lines start end))))

(defun streaming-widget-scroll (state delta &key (viewport-height (%safe-height state)))
  "Adjust scroll offset by DELTA. Positive scrolls up into history."
  (check-type state streaming-widget-state)
  (check-type delta integer)
  (check-type viewport-height (integer 1 *))
  (setf (streaming-widget-state-viewport-height state) viewport-height)
  (let* ((clamped-offset
           (ptui.util.scroll:apply-scroll-delta
            (streaming-widget-state-scroll-offset state)
            delta
            (%line-count state)
            viewport-height)))
    (setf (streaming-widget-state-scroll-offset state) clamped-offset)
    (cond
      ((> clamped-offset 0)
       (setf (streaming-widget-state-scroll-follow-p state) nil))
      ((= clamped-offset 0)
       (setf (streaming-widget-state-scroll-follow-p state) t))))
  state)

(defun streaming-widget-scroll-home (state &key (viewport-height (%safe-height state)))
  (check-type state streaming-widget-state)
  (check-type viewport-height (integer 1 *))
  (setf (streaming-widget-state-viewport-height state) viewport-height
        (streaming-widget-state-scroll-offset state) (%max-scroll-offset state viewport-height)
        (streaming-widget-state-scroll-follow-p state) nil)
  state)

(defun streaming-widget-scroll-end (state)
  (check-type state streaming-widget-state)
  (setf (streaming-widget-state-scroll-offset state) 0
        (streaming-widget-state-scroll-follow-p state) t)
  state)

(defun streaming-widget-handle-event (state event &key (viewport-height (%safe-height state)))
  "Handle key events for scrolling and follow-mode behavior."
  (check-type state streaming-widget-state)
  (check-type viewport-height (integer 1 *))
  (unless (typep event 'ptui.core.events:key-event)
    (return-from streaming-widget-handle-event
      (list :action :ignored :state state)))
  (let ((key (ptui.core.events:key-event-key event)))
    (multiple-value-bind (action delta)
        (ptui.util.scroll:key-scroll-action key :viewport-height viewport-height)
      (case action
        (:delta
         (streaming-widget-scroll state delta :viewport-height viewport-height)
         (list :action :scrolled :delta delta :state state))
        (:home
         (streaming-widget-scroll-home state :viewport-height viewport-height)
         (list :action :scrolled-home :state state))
        (:end
         (streaming-widget-scroll-end state)
         (list :action :scrolled-end :state state))
        (otherwise
         (list :action :ignored :state state))))))

(defun %status-line (state)
  (format nil "stream ~A | follow ~:[off~;on~] | +~D"
          (streaming-widget-state-status state)
          (streaming-widget-state-scroll-follow-p state)
          (streaming-widget-state-scroll-offset state)))

(defun make-streaming-widget (state
                              &key
                                id
                                key
                                (viewport-width (%safe-width state))
                                (viewport-height (%safe-height state))
                                (borderp t)
                                (padding 0)
                                now-ms)
  "Create a composable widget tree for a streaming text buffer."
  (check-type state streaming-widget-state)
  (check-type viewport-width (integer 1 *))
  (check-type viewport-height (integer 1 *))
  (streaming-widget-set-width state viewport-width)
  (setf (streaming-widget-state-viewport-height state) viewport-height)
  (let* ((status (ptui.widgets.core:make-text-widget
                  (%status-line state)
                  :id (list id :status)))
         (rows
           (loop for line in (streaming-widget-visible-lines
                              state
                              :viewport-height viewport-height
                              :include-cursor-p t
                              :now-ms now-ms)
                 for index from 0
                 collect (ptui.widgets.core:make-text-widget
                          line
                          :id (list id :line index))))
         (content (ptui.widgets.core:make-stack-widget
                   (append (list status) rows)
                   :id (list id :content)
                   :direction :column
                   :gap 0)))
    (ptui.widgets.core:make-box-widget
     content
     :id id
     :key key
     :padding padding
     :borderp borderp)))
