;;;; amoebum/src/fp/update.lisp
;;;;
;;;; Functional (non-destructive) update helpers for the small set of
;;;; associative containers used throughout amoebum: property lists,
;;;; association lists, and hash-tables.
;;;;
;;;; All functions in this file return a NEW container; the input is
;;;; never mutated (hash-tables are copied).
;;;;
;;;; Supported container shapes:
;;;;   - plist:      (:a 1 :b 2)                 — detected when car is a keyword/symbol
;;;;                                               and every other element is a key
;;;;   - alist:      ((:a . 1) (:b . 2))         — detected when every element is a cons
;;;;                                               whose car is a key
;;;;   - hash-table: #<HASH-TABLE ...>
;;;;
;;;; For nested structures, `assoc-in` / `update-in` recurse on each
;;;; level of PATH, copying-and-replacing at that level.

(in-package #:amoebum.fp)

;;; ----------------------------------------------------------------
;;; Container shape detection
;;; ----------------------------------------------------------------

(defun %alist-p (obj)
  "Return T if OBJ looks like an alist (a list of conses)."
  (and (listp obj)
       (not (null obj))
       (every #'consp obj)))

(defun %plist-p (obj)
  "Return T if OBJ looks like a plist (even-length list of
key/value pairs, with keys that are symbols)."
  (and (listp obj)
       (evenp (length obj))
       (loop for tail on obj by #'cddr
             always (symbolp (car tail)))))

;;; ----------------------------------------------------------------
;;; Single-level functional get / set
;;; ----------------------------------------------------------------

(defun %assoc-get (obj key)
  "Fetch KEY from OBJ (plist, alist, or hash-table). Returns the
value or NIL if missing."
  (etypecase obj
    (hash-table (gethash key obj))
    (list
     (cond
       ((%alist-p obj) (cdr (assoc key obj :test #'equal)))
       ((%plist-p obj) (getf obj key))
       (t nil)))))

(defun %plist-assoc (plist key value)
  "Return a copy of PLIST with KEY set to VALUE (appending if absent)."
  (let ((copy (copy-list plist)))
    (setf (getf copy key) value)
    copy))

(defun %alist-assoc (alist key value)
  "Return a copy of ALIST with KEY set to VALUE (appending if absent)."
  (if (assoc key alist :test #'equal)
      (mapcar (lambda (cell)
                (if (equal (car cell) key)
                    (cons key value)
                    cell))
              alist)
      (append alist (list (cons key value)))))

(defun %hash-assoc (ht key value)
  "Return a copy of hash-table HT with KEY set to VALUE."
  (let ((copy (make-hash-table :test (hash-table-test ht)
                               :size (max 16 (hash-table-count ht)))))
    (maphash (lambda (k v) (setf (gethash k copy) v)) ht)
    (setf (gethash key copy) value)
    copy))

(defun %assoc-set (obj key value)
  "Return a new container like OBJ with KEY mapped to VALUE. Supports
plist, alist, and hash-table. If OBJ is nil, returns a fresh plist."
  (etypecase obj
    (hash-table (%hash-assoc obj key value))
    (null       (list key value))
    (list       (cond
                  ((%alist-p obj) (%alist-assoc obj key value))
                  ((%plist-p obj) (%plist-assoc obj key value))
                  (t              (%plist-assoc obj key value))))))

;;; ----------------------------------------------------------------
;;; Nested variants
;;; ----------------------------------------------------------------

(defun assoc-in (obj path value)
  "Return a new container like OBJ with the nested location specified
by PATH (a list of keys) set to VALUE. Supports plists, alists, and
hash-tables at any level of nesting.

Example:
  (assoc-in '(:a (:b (:c 1))) '(:a :b :c) 42)
  => (:A (:B (:C 42)))"
  (cond
    ((null path)       value)
    ((null (rest path)) (%assoc-set obj (first path) value))
    (t
     (let* ((key   (first path))
            (child (%assoc-get obj key))
            (new-child (assoc-in child (rest path) value)))
       (%assoc-set obj key new-child)))))

(defun update-in (obj path fn)
  "Return a new container like OBJ with the nested location at PATH
replaced by `(funcall FN current-value)`. Missing intermediate keys
are treated as NIL, which allows FN to initialise counters etc.

Example:
  (update-in '(:a (:n 0)) '(:a :n) #'1+)
  => (:A (:N 1))"
  (let ((current (reduce #'%assoc-get path :initial-value obj)))
    (assoc-in obj path (funcall fn current))))

;;; ----------------------------------------------------------------
;;; `update` macro
;;; ----------------------------------------------------------------
;;;
;;; For plists, `(update obj (:slot value) ...)` returns a copy with
;;; each slot set. Struct support is deliberately deferred: in CL there
;;; is no portable `copy-and-set` protocol for arbitrary defstructs
;;; without reflection; the correct pattern is either
;;;   (a) a generated `(copy-foo foo :slot value)` initarg-aware
;;;       constructor, or
;;;   (b) moving the data into a plist / persistent map.
;;; See TODO below.

(defmacro update (object &rest slot-value-forms)
  "Return a new plist derived from OBJECT with each `(:slot value)` form
applied in order. OBJECT is evaluated once.

Example:
  (let ((p (list :a 1 :b 2)))
    (update p (:a 10) (:c 3)))
  => (:A 10 :B 2 :C 3)

TODO: struct support. A future iteration may detect structs via
`typep` and dispatch to a `copy-<name>` constructor, or convert
struct slot updates into a known copying-constructor table. For
now, callers with struct values should use `copy-structure` +
setters explicitly, or convert to a plist."
  (let ((obj-sym (gensym "OBJ")))
    `(let ((,obj-sym ,object))
       ,(reduce
         (lambda (acc form)
           (unless (and (consp form) (= (length form) 2))
             (error "update: expected (:slot value) form, got ~S" form))
           (let ((slot (first form))
                 (val  (second form)))
             `(amoebum.fp::%plist-assoc ,acc ',slot ,val)))
         slot-value-forms
         :initial-value obj-sym))))
