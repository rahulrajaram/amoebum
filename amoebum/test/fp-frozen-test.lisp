;;;; amoebum/test/fp-frozen-test.lisp
;;;;
;;;; FiveAM tests for the `defrozen` macro in `amoebum.fp`
;;;; (see `amoebum/src/fp/frozen.lisp`). A frozen record must:
;;;;   - construct via a &key `make-NAME`,
;;;;   - expose readers,
;;;;   - return a fresh instance via `with-NAME`,
;;;;   - leave the original instance untouched,
;;;;   - reject any attempt to `setf` a slot accessor.

(in-package :amoebum/test)

(amoebum.fp:defrozen test-point (x y))

(test fp-frozen-constructs-and-reads-via-accessors
  "A `defrozen` record should construct via the generated &key
`make-NAME` helper and expose the slot values via readers."
  (let ((pt (make-test-point :x 1 :y 2)))
    (is (equal 1 (test-point-x pt)))
    (is (equal 2 (test-point-y pt)))))

(test fp-frozen-with-returns-updated-instance
  "`with-NAME` should return a fresh instance whose overridden slots
carry the new values while other slots are inherited from the
original."
  (let* ((pt  (make-test-point :x 1 :y 2))
         (pt2 (with-test-point pt :x 99)))
    (is (equal 99 (test-point-x pt2)))
    (is (equal 2  (test-point-y pt2)))))

(test fp-frozen-with-does-not-mutate-original
  "`with-NAME` must be pure: the original instance's slots must be
unchanged after the call and the returned instance must be a
distinct object."
  (let* ((pt  (make-test-point :x 1 :y 2))
         (pt2 (with-test-point pt :x 99)))
    (is (equal 1 (test-point-x pt)))
    (is (equal 2 (test-point-y pt)))
    (is (not (eq pt pt2)))))

(test fp-frozen-with-no-overrides-copies-all-slots
  "`with-NAME` with no override arguments should return a new
instance whose slots exactly match the original."
  (let* ((pt  (make-test-point :x 7 :y 8))
         (pt2 (with-test-point pt)))
    (is (equal 7 (test-point-x pt2)))
    (is (equal 8 (test-point-y pt2)))
    (is (not (eq pt pt2)))))

(test fp-frozen-slot-setf-signals-error
  "Every slot of a `defrozen` record is declared `:read-only t`, so
attempting to `setf` any slot accessor must signal an error (either
at compile time inside the inner `eval` or at run time)."
  (let ((pt (make-test-point :x 1 :y 2)))
    (signals error
      (eval `(setf (test-point-x ,pt) 99)))))
