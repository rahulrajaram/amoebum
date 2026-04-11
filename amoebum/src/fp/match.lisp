;;;; amoebum/src/fp/match.lisp
;;;;
;;;; Sealed-enum dispatch for amoebum. The `match` macro expands to an
;;;; ordinary `case` whose otherwise branch raises through
;;;; `assert-exhaustive`, so adding a new variant without updating every
;;;; dispatch site fails loudly at runtime rather than silently falling
;;;; through.
;;;;
;;;; Typical usage:
;;;;
;;;;   (match status
;;;;     (:pending (start))
;;;;     (:running (tick))
;;;;     (:done    (finish)))
;;;;
;;;; If a new `:cancelled` variant is introduced and this site is not
;;;; updated, the call will signal a clear error naming the offending
;;;; value rather than returning NIL.

(in-package #:amoebum.fp)

(define-condition non-exhaustive-match (error)
  ((value :initarg :value :reader non-exhaustive-match-value))
  (:report (lambda (c stream)
             (format stream "Non-exhaustive match: no branch for variant ~S"
                     (non-exhaustive-match-value c)))))

(defun assert-exhaustive (value)
  "Signal a `non-exhaustive-match` error naming VALUE. Intended as the
fall-through for a sealed dispatch: if control reaches this call, a new
variant was introduced without updating the dispatch site."
  (error 'non-exhaustive-match :value value))

(defmacro match (expr &body clauses)
  "Dispatch on EXPR by matching against the literal head of each clause.
Each CLAUSE has the shape `(KEY BODY...)` where KEY is either a single
literal (typically a keyword) or a list of literals sharing a body.
Expands to a `case` whose otherwise branch calls `assert-exhaustive` on
the evaluated value, so unhandled variants fail loudly at runtime.

Example:
  (match state
    (:a 1)
    ((:b :c) 2))

Evaluates EXPR exactly once."
  (let ((value-sym (gensym "MATCH-VALUE")))
    (dolist (clause clauses)
      (unless (and (consp clause) (not (null (rest clause))))
        (error "match: expected (KEY BODY...) clause, got ~S" clause))
      (let ((key (first clause)))
        (when (or (eq key t) (eq key 'otherwise))
          (error "match: `otherwise` clauses are not allowed; ~
                  matches must be exhaustive (got key ~S)" key))))
    `(let ((,value-sym ,expr))
       (case ,value-sym
         ,@(mapcar (lambda (clause)
                     `(,(first clause) ,@(rest clause)))
                   clauses)
         (otherwise (assert-exhaustive ,value-sym))))))
