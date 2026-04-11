;;;; amoebum/test/fp-transition-test.lisp
;;;;
;;;; FiveAM tests for the declarative transition-table helper in
;;;; `amoebum.fp` (see `amoebum/src/fp/transition.lisp`). Every
;;;; operation on a transition table must be pure: the underlying
;;;; hash-table must never be mutated by lookups or transitions.

(in-package :amoebum/test)

(5am:def-suite fp-transition-suite
  :description "Pure declarative transition-table helper (NXT-290).")

(5am:in-suite fp-transition-suite)

(defun %make-traffic-light-table ()
  "Build a tiny 3-state traffic-light machine for tests.

States: :red :green :yellow. Events: :tick. Routing:
  :red    + :tick -> :green
  :green  + :tick -> :yellow
  :yellow + :tick -> :red

Plus a `:reset` event from every state back to `:red`."
  (amoebum.fp:make-transition-table
   '(((:red    :tick)  . :green)
     ((:green  :tick)  . :yellow)
     ((:yellow :tick)  . :red)
     ((:red    :reset) . :red)
     ((:green  :reset) . :red)
     ((:yellow :reset) . :red))))

(5am:test fp-transition-builds-three-state-machine
  "`make-transition-table` should accept a list of pair forms and
produce an opaque table that can be inspected via
`transition-table-lookup` for each configured edge."
  (let ((table (%make-traffic-light-table)))
    (multiple-value-bind (to-state foundp)
        (amoebum.fp:transition-table-lookup table :red :tick)
      (5am:is (eq :green to-state))
      (5am:is-true foundp))
    (multiple-value-bind (to-state foundp)
        (amoebum.fp:transition-table-lookup table :green :tick)
      (5am:is (eq :yellow to-state))
      (5am:is-true foundp))
    (multiple-value-bind (to-state foundp)
        (amoebum.fp:transition-table-lookup table :yellow :tick)
      (5am:is (eq :red to-state))
      (5am:is-true foundp))))

(5am:test fp-transition-ok-on-legal-edge
  "`transition` should return an `ok` carrying the new state when
the `(from-state event)` pair is present in the table."
  (let* ((table  (%make-traffic-light-table))
         (result (amoebum.fp:transition table :red :tick)))
    (5am:is-true (amoebum.fp:ok-p result))
    (5am:is (eq :green (amoebum.fp:ok-value result)))))

(5am:test fp-transition-err-on-unknown-event
  "`transition` should return an `err` tagged
`:unknown-transition` when no rule matches the pair, carrying
the offending from-state and event for diagnostics."
  (let* ((table  (%make-traffic-light-table))
         (result (amoebum.fp:transition table :red :blink)))
    (5am:is-true (amoebum.fp:err-p result))
    (let ((err-value (amoebum.fp:err-value result)))
      (5am:is (eq :unknown-transition (first  err-value)))
      (5am:is (eq :red                (second err-value)))
      (5am:is (eq :blink              (third  err-value))))))

(5am:test fp-transition-same-from-routes-by-event
  "The same from-state with different events must route to the
corresponding distinct target states."
  (let ((table (%make-traffic-light-table)))
    (let ((tick-result  (amoebum.fp:transition table :green :tick))
          (reset-result (amoebum.fp:transition table :green :reset)))
      (5am:is-true (amoebum.fp:ok-p tick-result))
      (5am:is-true (amoebum.fp:ok-p reset-result))
      (5am:is (eq :yellow (amoebum.fp:ok-value tick-result)))
      (5am:is (eq :red    (amoebum.fp:ok-value reset-result))))))

(5am:test fp-transition-events-for-returns-legal-events
  "`transition-table-events-for` should return every event legal
from the given from-state, regardless of order."
  (let* ((table  (%make-traffic-light-table))
         (events (amoebum.fp:transition-table-events-for table :red)))
    (5am:is (= 2 (length events)))
    (5am:is-true (find :tick  events))
    (5am:is-true (find :reset events))
    ;; States with no outgoing edges should return NIL.
    (5am:is (null (amoebum.fp:transition-table-events-for
                   (amoebum.fp:make-transition-table
                    '(((:a :go) . :b)))
                   :b)))))

(5am:test fp-transition-lookups-do-not-mutate-table
  "Pure guarantee: neither `transition-table-lookup` nor
`transition` may mutate the underlying table. A structurally
identical twin built beforehand should remain `equalp` to the
queried table after a mix of successful and failing operations."
  (let* ((pairs '(((:red    :tick)  . :green)
                  ((:green  :tick)  . :yellow)
                  ((:yellow :tick)  . :red)))
         (table (amoebum.fp:make-transition-table pairs))
         (twin  (amoebum.fp:make-transition-table pairs)))
    ;; A mix of hits and misses.
    (amoebum.fp:transition-table-lookup table :red    :tick)
    (amoebum.fp:transition-table-lookup table :red    :nope)
    (amoebum.fp:transition              table :green  :tick)
    (amoebum.fp:transition              table :yellow :nope)
    (amoebum.fp:transition-table-events-for table :red)
    ;; Hash-table identity: same count, same key->value mapping.
    (let ((a (amoebum.fp::transition-table-entries table))
          (b (amoebum.fp::transition-table-entries twin)))
      (5am:is (= (hash-table-count a) (hash-table-count b)))
      (maphash (lambda (k v)
                 (5am:is (equal v (gethash k a))))
               b)
      (5am:is (equalp a b)))))

(5am:in-suite amoebum-suite)
