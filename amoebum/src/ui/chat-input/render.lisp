(in-package :amoebum)

;;; NXT-543: layout/render-side helpers extracted from ui/chat-input.lisp.
;;; Owns wrapped-line geometry, cursor display coordinates, prompt scroll
;;; window selection, and the cursor-blink phase oracle. These are pure
;;; functions of state — no mutation of chat-state and no event dispatch.

(defun %fit-line-width (text width)
  (ptui.text.layout:truncate-to-width text (max 0 width)))

(defun %prompt-wrapped-lines (value width)
  (if (<= width 0)
      (list "")
      (ptui.text.layout:wrap-by-width value (max 1 width)
                                      :preserve-spaces t)))

(defun %cursor-to-line-col (cursor-pos lines)
  "Given a grapheme-offset CURSOR-POS and list of wrapped LINES, return
\(values line-index col-offset) as display coordinates."
  (let ((remaining cursor-pos))
    (loop for line in lines
          for line-idx from 0
          for line-len = (length line)
          do (if (<= remaining line-len)
                 ;; Cursor is on this line. If it's exactly at line-len and
                 ;; there are more lines, it wraps to next line col 0.
                 (if (and (= remaining line-len)
                          (< (1+ line-idx) (length lines)))
                     ;; Wrap to next line
                     (return-from %cursor-to-line-col
                       (values (1+ line-idx) 0))
                     (return-from %cursor-to-line-col
                       (values line-idx remaining)))
                 (decf remaining line-len)))
    ;; Past end - cursor on last line at end
    (values (max 0 (1- (length lines)))
            (if lines (length (car (last lines))) 0))))

(defun %line-col-to-cursor-pos (line-index col lines)
  "Convert display coordinates (LINE-INDEX, COL) back to a grapheme offset."
  (let ((pos 0))
    (loop for i from 0 below (min line-index (length lines))
          do (incf pos (length (nth i lines))))
    (+ pos (min col (if (< line-index (length lines))
                        (length (nth line-index lines))
                        0)))))

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

(defvar *chat-cursor-blink-start* nil)

(defun chat-ui-cursor-visible-p (chat-state)
  "Return T if the input cursor should be visible based on blink phase."
  (declare (ignore chat-state))
  ;; Initialize blink start time on first call
  (unless *chat-cursor-blink-start*
    (setf *chat-cursor-blink-start* (get-internal-real-time)))
  ;; Always visible if we can't determine time
  (let ((now (get-internal-real-time))
        (start *chat-cursor-blink-start*))
    (if (or (null now) (null start) (<= now start))
        t
        (let* ((units-per-sec internal-time-units-per-second)
               ;; Blink every 1060ms (530ms on, 530ms off)
               (blink-period-ms 1060)
               (half-period-ms 530)
               ;; Calculate elapsed time in milliseconds safely
               (elapsed-units (- now start))
               (elapsed-ms (floor (* elapsed-units blink-period-ms)
                                  (max 1 units-per-sec)))
               ;; Get position in current blink cycle
               (phase (mod elapsed-ms blink-period-ms)))
          ;; Visible during first half of cycle
          (< phase half-period-ms)))))
