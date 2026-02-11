(defpackage :ptui.text.adapter.native
  (:use :cl)
  (:export
   #:*native-grapheme-support-p*
   #:*native-width-support-p*
   #:native-engine-available-p
   #:split-graphemes
   #:codepoint-width))

(in-package :ptui.text.adapter.native)

(defparameter *native-grapheme-support-p* nil
  "Set true when a native grapheme segmenter is wired.")

(defparameter *native-width-support-p* nil
  "Set true when a native width engine is wired.")

(defun native-engine-available-p ()
  (and *native-grapheme-support-p*
       *native-width-support-p*))

(defun split-graphemes (text)
  "Temporary native adapter placeholder: delegates to fallback behavior."
  (ptui.text.adapter.fallback:split-graphemes text))

(defun codepoint-width (ch)
  "Temporary native adapter placeholder: delegates to fallback behavior."
  (ptui.text.adapter.fallback:codepoint-width ch))

(eval-when (:load-toplevel :execute)
  (ptui.text.engine:register-text-engine
   (ptui.text.engine:make-text-engine-adapter
    :native
    #'split-graphemes
    #'codepoint-width
    :available-p-fn #'native-engine-available-p)))
