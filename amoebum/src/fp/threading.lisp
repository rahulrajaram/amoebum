;;;; amoebum/src/fp/threading.lisp
;;;;
;;;; Clojure-style threading macros. `->` threads the value as the FIRST
;;;; argument of each successive form; `->>` threads it as the LAST
;;;; argument. Atom forms are treated as one-argument functions, e.g.
;;;;   (-> 3 1+ (* 2))  ==  (* (1+ 3) 2)
;;;;   (->> '(1 2 3) (mapcar #'1+) (reduce #'+))

(in-package #:amoebum.fp)

(defun %thread-first-step (value form)
  "Rewrite a single `->` step so VALUE becomes the first argument of FORM."
  (cond
    ((and (consp form) (not (eq (first form) 'quote)))
     `(,(first form) ,value ,@(rest form)))
    ((symbolp form)
     `(,form ,value))
    (t
     (error "Cannot thread value into form ~S" form))))

(defun %thread-last-step (value form)
  "Rewrite a single `->>` step so VALUE becomes the last argument of FORM."
  (cond
    ((and (consp form) (not (eq (first form) 'quote)))
     `(,(first form) ,@(rest form) ,value))
    ((symbolp form)
     `(,form ,value))
    (t
     (error "Cannot thread value into form ~S" form))))

(defmacro -> (initial-value &rest forms)
  "Thread INITIAL-VALUE through FORMS, inserting it as the FIRST argument
of each form. Atom forms are treated as single-argument function calls."
  (reduce (lambda (acc form) (%thread-first-step acc form))
          forms
          :initial-value initial-value))

(defmacro ->> (initial-value &rest forms)
  "Thread INITIAL-VALUE through FORMS, inserting it as the LAST argument
of each form. Atom forms are treated as single-argument function calls."
  (reduce (lambda (acc form) (%thread-last-step acc form))
          forms
          :initial-value initial-value))
