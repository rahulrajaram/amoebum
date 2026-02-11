(defpackage :ptui.text.grapheme
  (:use :cl)
  (:export
   #:split-graphemes
   #:map-graphemes
   #:do-graphemes
   #:grapheme-engine))

(in-package :ptui.text.grapheme)

(defun grapheme-engine ()
  "Return the active grapheme engine keyword for :auto selection."
  (ptui.text.engine:resolve-text-engine :auto))

(defun split-graphemes (text &key (engine :auto))
  "Split TEXT into grapheme-like clusters using the selected engine adapter."
  (check-type text string)
  (ptui.text.engine:call-split-graphemes text :engine engine))

(defun map-graphemes (function text &key (engine :auto))
  "Map FUNCTION across grapheme clusters in TEXT."
  (mapcar function (split-graphemes text :engine engine)))

(defmacro do-graphemes ((var text &key (engine :auto) result) &body body)
  "Iterate VAR over grapheme clusters in TEXT."
  `(dolist (,var (split-graphemes ,text :engine ,engine) ,result)
     ,@body))
