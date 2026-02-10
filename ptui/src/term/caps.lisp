(defpackage :ptui.term.caps
  (:use :cl)
  (:export #:terminal-caps
           #:probe-terminal-caps
           #:terminal-caps-term
           #:terminal-caps-truecolorp
           #:terminal-caps-256colorp
           #:terminal-caps-mousep
           #:terminal-caps-alt-screenp))

(in-package :ptui.term.caps)

(defstruct (terminal-caps
            (:constructor make-terminal-caps
                (&key term truecolorp 256colorp mousep alt-screenp)))
  (term "dumb" :type string)
  (truecolorp nil :type boolean)
  (256colorp nil :type boolean)
  (mousep nil :type boolean)
  (alt-screenp nil :type boolean))

(defun %contains-ci (haystack needle)
  (and haystack
       (search needle haystack :test #'char-equal)))

(defun %env-get (env name)
  (cond
    ((functionp env)
     (funcall env name))
    ((hash-table-p env)
     (gethash name env))
    ((listp env)
     (cdr (assoc name env :test #'string=)))
    (t
     nil)))

(defun probe-terminal-caps (&key (env #'uiop:getenv))
  (let* ((term (or (%env-get env "TERM") "dumb"))
         (colorterm (%env-get env "COLORTERM"))
         (term-program (%env-get env "TERM_PROGRAM"))
         (truecolorp (or (%contains-ci colorterm "truecolor")
                         (%contains-ci colorterm "24bit")))
         (256colorp (or truecolorp (%contains-ci term "256color")))
         (interactive-term-p (not (string-equal term "dumb")))
         (mousep interactive-term-p)
         (alt-screenp interactive-term-p))
    (declare (ignore term-program))
    (make-terminal-caps
     :term term
     :truecolorp (not (null truecolorp))
     :256colorp (not (null 256colorp))
     :mousep mousep
     :alt-screenp alt-screenp)))
