;;;; amoebum/src/fp/plist.lisp
;;;;
;;;; Pure persistent plist helpers for amoebum's functional kernel.
;;;;
;;;; Every function in this file returns a freshly-consed plist and
;;;; never mutates its inputs. No `setf`, no destructive operations on
;;;; user data. These helpers are the functional-kernel counterpart to
;;;; the internal `%plist-assoc` helper in `update.lisp` (which is kept
;;;; private, uses `setf` on a private copy, and is only referenced by
;;;; the `update` macro expansion).
;;;;
;;;; Keys are compared with `eql`, which is the normal plist
;;;; convention (see `getf`). Keys are typically keywords.

(in-package #:amoebum.fp)

(defun plist-assoc (plist key value)
  "Return a new plist derived from PLIST with KEY mapped to VALUE.

If KEY is already present, its value is replaced; otherwise KEY/VALUE
are appended to the end. The original PLIST is not mutated and the
result is freshly consed. Runs in O(n) where n is the length of PLIST."
  (labels ((walk (tail)
             (cond
               ((null tail)
                (list key value))
               ((eql (first tail) key)
                (cons key (cons value (copy-list (cddr tail)))))
               (t
                (cons (first tail)
                      (cons (second tail)
                            (walk (cddr tail))))))))
    (walk plist)))

(defun plist-dissoc (plist key)
  "Return a new plist derived from PLIST with KEY removed.

If KEY is not present, the result is a fresh copy of PLIST. The
original PLIST is not mutated. Runs in O(n)."
  (labels ((walk (tail)
             (cond
               ((null tail) nil)
               ((eql (first tail) key)
                (copy-list (cddr tail)))
               (t
                (cons (first tail)
                      (cons (second tail)
                            (walk (cddr tail))))))))
    (walk plist)))

(defun plist-merge (plist1 plist2)
  "Return a new plist consisting of the entries of PLIST1 with every
entry of PLIST2 layered on top. When a key appears in both, PLIST2's
value wins (right-biased). Neither input is mutated."
  (labels ((merge-step (acc tail)
             (if (null tail)
                 acc
                 (merge-step (plist-assoc acc (first tail) (second tail))
                             (cddr tail)))))
    (merge-step (copy-list plist1) plist2)))

(defun plist-get-in (plist path)
  "Thread a nested lookup through PLIST. PATH is a list of keys; each
step expects the current value to be another plist (or nil). Returns
NIL as soon as any key is missing or the intermediate value is not a
cons.

Example:
  (plist-get-in '(:a (:b (:c 42))) '(:a :b :c))
  => 42"
  (cond
    ((null path) plist)
    ((not (consp plist)) nil)
    (t (plist-get-in (getf plist (first path)) (rest path)))))

(defun plist-select-keys (plist keys)
  "Return a new plist containing only the entries of PLIST whose key
appears in KEYS. Result order matches KEYS; keys not present in PLIST
are omitted (no NIL stand-in). Runs in O(k*n) where k = (length KEYS)
and n = (length PLIST)."
  (labels ((lookup (tail key)
             (cond
               ((null tail) (values nil nil))
               ((eql (first tail) key) (values (second tail) t))
               (t (lookup (cddr tail) key))))
           (walk (remaining)
             (if (null remaining)
                 nil
                 (multiple-value-bind (v present)
                     (lookup plist (first remaining))
                   (if present
                       (cons (first remaining)
                             (cons v (walk (rest remaining))))
                       (walk (rest remaining)))))))
    (walk keys)))
