;;;; amoebum/src/fp/result.lisp
;;;;
;;;; A tiny `Result[T, E]` type for modelling success/failure without
;;;; exceptions. Two distinct structs are used (rather than a tagged
;;;; single struct) so that the SBCL type system and `typecase` can
;;;; dispatch statically, and so `ok-p` / `err-p` become plain predicates.

(in-package #:amoebum.fp)

(defstruct ok
  "Successful result carrying a VALUE."
  value)

(defstruct err
  "Failed result carrying an error VALUE (typically a string or condition)."
  value)

(defun result-map (fn res)
  "Apply FN to the inner value of an `ok` result, returning a new `ok`.
If RES is an `err`, return it unchanged. Signals an error if RES is
neither an `ok` nor an `err`."
  (typecase res
    (ok  (make-ok :value (funcall fn (ok-value res))))
    (err res)
    (t   (error "result-map: not a result: ~S" res))))

(defun result-bind (res fn)
  "Monadic bind. If RES is an `ok`, apply FN to its value; FN must
return a result (either an `ok` or an `err`). If RES is an `err`,
short-circuit and return it unchanged."
  (typecase res
    (ok  (let ((next (funcall fn (ok-value res))))
           (typecase next
             (ok  next)
             (err next)
             (t   (error "result-bind: fn must return a result, got ~S" next)))))
    (err res)
    (t   (error "result-bind: not a result: ~S" res))))
