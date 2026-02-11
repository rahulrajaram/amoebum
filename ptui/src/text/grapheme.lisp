(defpackage :ptui.text.grapheme
  (:use :cl)
  (:export
   #:split-graphemes
   #:map-graphemes
   #:do-graphemes
   #:grapheme-engine))

(in-package :ptui.text.grapheme)

(defparameter *native-grapheme-support-p* nil
  "Native UAX#29 grapheme boundary support.
Current PTUI text pipeline always uses deterministic fallback segmentation.")

(defun grapheme-engine ()
  "Return :native when available, otherwise :fallback."
  (if *native-grapheme-support-p* :native :fallback))

(defun %codepoint-in-range-p (cp start end)
  (and (<= start cp) (<= cp end)))

(defun %combining-mark-codepoint-p (cp)
  (or (%codepoint-in-range-p cp #x0300 #x036F)
      (%codepoint-in-range-p cp #x1AB0 #x1AFF)
      (%codepoint-in-range-p cp #x1DC0 #x1DFF)
      (%codepoint-in-range-p cp #x20D0 #x20FF)
      (%codepoint-in-range-p cp #xFE20 #xFE2F)))

(defun %variation-selector-codepoint-p (cp)
  (or (%codepoint-in-range-p cp #xFE00 #xFE0F)
      (%codepoint-in-range-p cp #xE0100 #xE01EF)))

(defun %emoji-modifier-codepoint-p (cp)
  (%codepoint-in-range-p cp #x1F3FB #x1F3FF))

(defun %regional-indicator-codepoint-p (cp)
  (%codepoint-in-range-p cp #x1F1E6 #x1F1FF))

(defun %joiner-codepoint-p (cp)
  (= cp #x200D))

(defun %keycap-codepoint-p (cp)
  (= cp #x20E3))

(defun %extend-codepoint-p (cp)
  (or (%combining-mark-codepoint-p cp)
      (%variation-selector-codepoint-p cp)
      (%emoji-modifier-codepoint-p cp)
      (%joiner-codepoint-p cp)
      (%keycap-codepoint-p cp)))

(defun %append-char (builder ch)
  (vector-push-extend ch builder))

(defun %builder->string (builder)
  (coerce builder 'string))

(defun %make-builder ()
  (make-array 8 :element-type 'character :adjustable t :fill-pointer 0))

(defun %split-graphemes-fallback (text)
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
               (setf prev-was-joiner (%joiner-codepoint-p cp))
               (setf ri-pending
                     (cond
                       ((%regional-indicator-codepoint-p cp)
                        (not ri-pending))
                       (t nil)))))
      (loop for ch across text do
        (let ((cp (char-code ch)))
          (cond
            ((= (fill-pointer builder) 0)
             (append-codepoint ch cp))
            ((or prev-was-joiner
                 (%extend-codepoint-p cp)
                 (and ri-pending (%regional-indicator-codepoint-p cp)))
             (append-codepoint ch cp))
            (t
             (flush-builder)
             (append-codepoint ch cp)))))
      (flush-builder))
    (nreverse clusters)))

(defun %resolve-engine (engine)
  (ecase engine
    (:auto (grapheme-engine))
    (:native (if *native-grapheme-support-p* :native :fallback))
    (:fallback :fallback)))

(defun split-graphemes (text &key (engine :auto))
  "Split TEXT into grapheme-like clusters.
Current implementation uses deterministic fallback segmentation for all engines.
When native segmentation is introduced, :auto/:native can diverge."
  (check-type text string)
  (case (%resolve-engine engine)
    (:native (%split-graphemes-fallback text))
    (:fallback (%split-graphemes-fallback text))
    (t (%split-graphemes-fallback text))))

(defun map-graphemes (function text &key (engine :auto))
  "Map FUNCTION across grapheme clusters in TEXT."
  (mapcar function (split-graphemes text :engine engine)))

(defmacro do-graphemes ((var text &key (engine :auto) result) &body body)
  "Iterate VAR over grapheme clusters in TEXT."
  `(dolist (,var (split-graphemes ,text :engine ,engine) ,result)
     ,@body))
