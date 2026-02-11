(defpackage :ptui.text.adapter.fallback
  (:use :cl)
  (:export
   #:split-graphemes
   #:codepoint-width
   #:combining-codepoint-p
   #:variation-selector-codepoint-p
   #:emoji-codepoint-p
   #:regional-indicator-codepoint-p
   #:joiner-codepoint-p
   #:keycap-codepoint-p
   #:east-asian-wide-codepoint-p))

(in-package :ptui.text.adapter.fallback)

(defun %codepoint-in-range-p (cp start end)
  (and (<= start cp) (<= cp end)))

(defun combining-codepoint-p (cp)
  (or (%codepoint-in-range-p cp #x0300 #x036F)
      (%codepoint-in-range-p cp #x1AB0 #x1AFF)
      (%codepoint-in-range-p cp #x1DC0 #x1DFF)
      (%codepoint-in-range-p cp #x20D0 #x20FF)
      (%codepoint-in-range-p cp #xFE20 #xFE2F)))

(defun variation-selector-codepoint-p (cp)
  (or (%codepoint-in-range-p cp #xFE00 #xFE0F)
      (%codepoint-in-range-p cp #xE0100 #xE01EF)))

(defun %emoji-modifier-codepoint-p (cp)
  (%codepoint-in-range-p cp #x1F3FB #x1F3FF))

(defun regional-indicator-codepoint-p (cp)
  (%codepoint-in-range-p cp #x1F1E6 #x1F1FF))

(defun joiner-codepoint-p (cp)
  (= cp #x200D))

(defun keycap-codepoint-p (cp)
  (= cp #x20E3))

(defun %control-codepoint-p (cp)
  (or (%codepoint-in-range-p cp #x0000 #x001F)
      (%codepoint-in-range-p cp #x007F #x009F)))

(defun emoji-codepoint-p (cp)
  ;; Keep this conservative: only ranges that are consistently emoji-style
  ;; across modern terminals. Do not classify the entire Dingbats block
  ;; as width-2 because many symbols render as width-1 text glyphs.
  (or (%codepoint-in-range-p cp #x1F000 #x1FAFF)
      (%codepoint-in-range-p cp #x2600 #x26FF)))

(defun east-asian-wide-codepoint-p (cp)
  (or (%codepoint-in-range-p cp #x1100 #x115F)
      (%codepoint-in-range-p cp #x2329 #x232A)
      (%codepoint-in-range-p cp #x2E80 #xA4CF)
      (%codepoint-in-range-p cp #xAC00 #xD7A3)
      (%codepoint-in-range-p cp #xF900 #xFAFF)
      (%codepoint-in-range-p cp #xFE10 #xFE19)
      (%codepoint-in-range-p cp #xFE30 #xFE6F)
      (%codepoint-in-range-p cp #xFF00 #xFF60)
      (%codepoint-in-range-p cp #xFFE0 #xFFE6)
      (%codepoint-in-range-p cp #x20000 #x2FFFD)
      (%codepoint-in-range-p cp #x30000 #x3FFFD)))

(defun %extend-codepoint-p (cp)
  (or (combining-codepoint-p cp)
      (variation-selector-codepoint-p cp)
      (%emoji-modifier-codepoint-p cp)
      (joiner-codepoint-p cp)
      (keycap-codepoint-p cp)))

(defun %append-char (builder ch)
  (vector-push-extend ch builder))

(defun %builder->string (builder)
  (coerce builder 'string))

(defun %make-builder ()
  (make-array 8 :element-type 'character :adjustable t :fill-pointer 0))

(defun split-graphemes (text)
  (check-type text string)
  (let ((clusters '())
        (builder (%make-builder))
        (prev-was-joiner nil)
        (ri-pending nil))
    (labels ((flush-builder ()
               (when (> (fill-pointer builder) 0)
                 (push (%builder->string builder) clusters)
                 (setf builder (%make-builder)
                       prev-was-joiner nil
                       ri-pending nil)))
             (append-codepoint (ch cp)
               (%append-char builder ch)
               (setf prev-was-joiner (joiner-codepoint-p cp))
               (setf ri-pending
                     (cond
                       ((regional-indicator-codepoint-p cp)
                        (not ri-pending))
                       (t nil)))))
      (loop for ch across text do
        (let ((cp (char-code ch)))
          (cond
            ((= (fill-pointer builder) 0)
             (append-codepoint ch cp))
            ((or prev-was-joiner
                 (%extend-codepoint-p cp)
                 (and ri-pending (regional-indicator-codepoint-p cp)))
             (append-codepoint ch cp))
            (t
             (flush-builder)
             (append-codepoint ch cp)))))
      (flush-builder))
    (nreverse clusters)))

(defun codepoint-width (ch)
  (check-type ch character)
  (let ((cp (char-code ch)))
    (cond
      ((%control-codepoint-p cp) 0)
      ((or (combining-codepoint-p cp)
           (variation-selector-codepoint-p cp)
           (joiner-codepoint-p cp)
           (keycap-codepoint-p cp))
       0)
      ((or (east-asian-wide-codepoint-p cp)
           (emoji-codepoint-p cp))
       2)
      (t
       1))))

(eval-when (:load-toplevel :execute)
  (ptui.text.engine:register-text-engine
   (ptui.text.engine:make-text-engine-adapter
    :fallback
    #'split-graphemes
    #'codepoint-width)))
