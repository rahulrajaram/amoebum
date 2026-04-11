;;;; amoebum/test/fp-match-test.lisp
;;;;
;;;; FiveAM tests for the sealed-enum dispatch helpers in `amoebum.fp`.

(in-package :amoebum/test)

(test fp-match-returns-correct-branch
  "`match` should evaluate the body of the clause matching the value."
  (flet ((classify (x)
           (amoebum.fp:match x
             (:a 1)
             (:b 2)
             (:c 3))))
    (is (= 1 (classify :a)))
    (is (= 2 (classify :b)))
    (is (= 3 (classify :c)))))

(test fp-match-signals-on-unknown-variant
  "`match` should signal `non-exhaustive-match` when no clause matches,
and the condition should carry the offending variant."
  (flet ((classify (x)
           (amoebum.fp:match x
             (:a 1)
             (:b 2))))
    (signals amoebum.fp:non-exhaustive-match (classify :z))
    (handler-case (classify :z)
      (amoebum.fp:non-exhaustive-match (c)
        (is (eq :z (amoebum.fp:non-exhaustive-match-value c)))))))

(test fp-match-supports-nested-matches
  "`match` forms should compose: one match in the body of another."
  (flet ((describe-pair (outer inner)
           (amoebum.fp:match outer
             (:num (amoebum.fp:match inner
                     (:small 10)
                     (:large 20)))
             (:str (amoebum.fp:match inner
                     (:small "s")
                     (:large "L"))))))
    (is (= 10 (describe-pair :num :small)))
    (is (= 20 (describe-pair :num :large)))
    (is (equal "s" (describe-pair :str :small)))
    (is (equal "L" (describe-pair :str :large)))))

(test fp-assert-exhaustive-signals-useful-error
  "`assert-exhaustive` should signal a `non-exhaustive-match` that
carries the offending value, independent of `match`."
  (signals amoebum.fp:non-exhaustive-match
    (amoebum.fp:assert-exhaustive :unknown-variant))
  (handler-case (amoebum.fp:assert-exhaustive :unknown-variant)
    (amoebum.fp:non-exhaustive-match (c)
      (is (eq :unknown-variant
              (amoebum.fp:non-exhaustive-match-value c)))
      (let ((msg (princ-to-string c)))
        (is (not (null (search "UNKNOWN-VARIANT" msg))))))))
