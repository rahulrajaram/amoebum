;;;; amoebum/src/fp/transition.lisp
;;;;
;;;; Pure declarative transition-table helper for amoebum's functional
;;;; kernel.
;;;;
;;;; A transition table encodes a finite-state machine as data:
;;;; a set of `((from-state event) . to-state)` pairs. Once built
;;;; via `make-transition-table`, the table is opaque and treated
;;;; as immutable — all accessors read without mutating, and no
;;;; exported operation ever calls `setf` on a live table. The
;;;; backing hash-table lives inside a struct with a read-only
;;;; slot and no copier, so callers cannot reach in and mutate it
;;;; through the public API.
;;;;
;;;; The core `transition` function returns a `result` (see
;;;; `result.lisp`): `ok` with the new state on a legal
;;;; transition, or `err` carrying `(:unknown-transition from-state
;;;; event)` when no rule matches. This lets FSM drivers compose
;;;; transitions through `result-bind` instead of raising.

(in-package #:amoebum.fp)

(defstruct (transition-table
            (:copier nil)
            (:predicate transition-table-p)
            (:constructor %make-transition-table (entries)))
  "Opaque immutable transition table. ENTRIES is a frozen
hash-table keyed by `(from-state . event)` cons cells (compared
with `equal`) mapping to target states. The slot is read-only:
after `make-transition-table` returns, no exported operation
ever rebinds it."
  (entries (error "transition-table entries must be supplied")
           :type hash-table
           :read-only t))

(defun make-transition-table (pairs)
  "Build a frozen transition table from PAIRS.

PAIRS is a list of `((from-state event) . to-state)` forms.
Keys are compared structurally (`equal`), so any atoms that
`equal` can compare — keywords, symbols, numbers, strings — are
valid state and event values.

If the same `(from-state event)` pair appears more than once,
later entries win. The resulting table is opaque; callers should
only interact with it through `transition`,
`transition-table-lookup`, and `transition-table-events-for`."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (pair pairs)
      (let* ((key      (car pair))
             (to-state (cdr pair))
             (from     (first key))
             (event    (second key)))
        (setf (gethash (cons from event) table) to-state)))
    (%make-transition-table table)))

(defun transition-table-lookup (table from-state event)
  "Look up the target state for `(FROM-STATE EVENT)` in TABLE.

Returns two values: the target state (or NIL) and a generalized
boolean indicating whether the entry was present. Mirrors the
`gethash` calling convention so callers can distinguish `nil`
target states from missing entries. Pure: never mutates TABLE."
  (gethash (cons from-state event) (transition-table-entries table)))

(defun transition (table from-state event)
  "Apply EVENT to FROM-STATE using TABLE.

Returns a `result`: `ok` wrapping the new state on a legal
transition, or `err` wrapping `(:unknown-transition from-state
event)` when no rule matches. Pure: never mutates TABLE."
  (multiple-value-bind (to-state foundp)
      (transition-table-lookup table from-state event)
    (if foundp
        (make-ok :value to-state)
        (make-err :value (list :unknown-transition from-state event)))))

(defun transition-table-events-for (table from-state)
  "Return the list of events that are legal from FROM-STATE in TABLE.

Order is unspecified — callers that need a stable order should
sort the result. Pure: never mutates TABLE. Intended for error
messages and exhaustiveness checks."
  (let ((events '()))
    (maphash (lambda (key value)
               (declare (ignore value))
               (when (equal (car key) from-state)
                 (push (cdr key) events)))
             (transition-table-entries table))
    events))
