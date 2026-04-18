;;;; amoebum/src/fp/collections.lisp
;;;;
;;;; Small pure collection helpers for `amoebum.fp`.
;;;;
;;;; These helpers intentionally stay on plain Common Lisp lists/alists so
;;;; they can act as a lightweight substrate for later state/policy refactors
;;;; without pulling in a heavier collection dependency first.

(in-package #:amoebum.fp)

(defun filter-map (fn list)
  "Apply FN to each element of LIST and collect the non-NIL results.

Result order matches LIST. The input list is never mutated."
  (let ((results '()))
    (dolist (item list (nreverse results))
      (let ((mapped (funcall fn item)))
        (when mapped
          (push mapped results))))))

(defun first-some (fn list)
  "Return the first non-NIL value produced by applying FN to LIST.

Returns NIL when FN yields NIL for every element."
  (dolist (item list nil)
    (let ((value (funcall fn item)))
      (when value
        (return value)))))

(defun %alist-put (alist key value test)
  "Return a fresh ALIST with KEY mapped to VALUE using TEST."
  (labels ((walk (tail)
             (cond
               ((null tail)
                (list (cons key value)))
               ((funcall test (caar tail) key)
                (cons (cons key value) (copy-list (cdr tail))))
               (t
                (cons (car tail) (walk (cdr tail)))))))
    (walk alist)))

(defun group-by (key-fn list &key (test #'equal))
  "Group LIST into an alist keyed by `(funcall KEY-FN item)`.

Returns an alist of `(key . items)` pairs. Key encounter order matches the
first appearance in LIST, and item order inside each group matches LIST."
  (let ((groups '()))
    (dolist (item list)
      (let* ((key (funcall key-fn item))
             (existing (assoc key groups :test test))
             (bucket (if existing (append (cdr existing) (list item)) (list item))))
        (setf groups (%alist-put groups key bucket test))))
    groups))

(defun index-by (key-fn list &key (test #'equal))
  "Index LIST into an alist keyed by `(funcall KEY-FN item)`.

Later items for the same key replace earlier ones. Key order reflects first
encounter order."
  (let ((index '()))
    (dolist (item list index)
      (setf index (%alist-put index (funcall key-fn item) item test)))))

(defun partition (predicate list)
  "Split LIST into elements that satisfy PREDICATE and those that do not.

Returns two values: the matching elements, then the non-matching elements.
Both preserve the input order."
  (let ((matching '())
        (rest '()))
    (dolist (item list (values (nreverse matching) (nreverse rest)))
      (if (funcall predicate item)
          (push item matching)
          (push item rest)))))

(defun map-values (fn alist)
  "Return a fresh alist with FN applied to each value in ALIST.

Keys are preserved as-is and the input alist is never mutated."
  (mapcar (lambda (entry)
            (cons (car entry) (funcall fn (cdr entry))))
          alist))
