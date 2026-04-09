;;;; amoebum/test/fp-plist-test.lisp
;;;;
;;;; FiveAM tests for the persistent plist helpers in `amoebum.fp`
;;;; (see `amoebum/src/fp/plist.lisp`). Every helper must leave its
;;;; inputs untouched and return a freshly-consed plist.

(in-package :amoebum/test)

(test fp-plist-assoc-adds-new-key
  "`plist-assoc` should append a new key/value pair when the key is
absent from the original plist."
  (let* ((original '(:a 1 :b 2))
         (result   (amoebum.fp:plist-assoc original :c 3)))
    (is (equal 3 (getf result :c)))
    (is (equal 1 (getf result :a)))
    (is (equal 2 (getf result :b)))))

(test fp-plist-assoc-updates-existing-key
  "`plist-assoc` should replace the value of an existing key without
leaving a stale entry behind."
  (let* ((original '(:a 1 :b 2))
         (result   (amoebum.fp:plist-assoc original :a 99)))
    (is (equal 99 (getf result :a)))
    (is (equal 2  (getf result :b)))
    ;; Exactly one entry for :a — no duplicate tail.
    (is (equal 1 (count :a result)))))

(test fp-plist-assoc-does-not-mutate-original
  "`plist-assoc` must be pure: the caller's plist must be unchanged
after the call, and the result must not share structure that would
let a later mutation leak back."
  (let* ((original '(:a 1 :b 2))
         (snapshot (copy-list original))
         (result   (amoebum.fp:plist-assoc original :a 99)))
    (is (equal snapshot original))
    (is (not (eq original result)))
    (is (equal 1 (getf original :a)))))

(test fp-plist-dissoc-removes-existing-key
  "`plist-dissoc` should drop the requested key and its value from
the returned plist while preserving every other entry."
  (let* ((original '(:a 1 :b 2 :c 3))
         (result   (amoebum.fp:plist-dissoc original :b)))
    (is (null (getf result :b)))
    (is (equal 1 (getf result :a)))
    (is (equal 3 (getf result :c)))
    ;; Original is untouched.
    (is (equal 2 (getf original :b)))))

(test fp-plist-dissoc-missing-key-returns-copy
  "`plist-dissoc` on an absent key should be a no-op semantically and
still return a fresh list (not the same cons cell as the original)."
  (let* ((original '(:a 1 :b 2))
         (result   (amoebum.fp:plist-dissoc original :zzz)))
    (is (equal original result))
    (is (not (eq original result)))))

(test fp-plist-merge-right-biased
  "`plist-merge` should be right-biased: when a key is present in both
plists, the value from PLIST2 wins in the result."
  (let* ((p1 '(:a 1 :b 2 :c 3))
         (p2 '(:b 20 :d 40))
         (result (amoebum.fp:plist-merge p1 p2)))
    (is (equal 1  (getf result :a)))
    (is (equal 20 (getf result :b)))
    (is (equal 3  (getf result :c)))
    (is (equal 40 (getf result :d)))
    ;; Originals are unchanged.
    (is (equal 2  (getf p1 :b)))
    (is (equal 20 (getf p2 :b)))))

(test fp-plist-get-in-threads-nested-lookup
  "`plist-get-in` should thread successive keys through nested plists
and return the innermost value (or nil when any key is missing)."
  (let ((nested '(:a (:b (:c 42)))))
    (is (equal 42  (amoebum.fp:plist-get-in nested '(:a :b :c))))
    (is (equal '(:c 42) (amoebum.fp:plist-get-in nested '(:a :b))))
    (is (null (amoebum.fp:plist-get-in nested '(:a :b :missing))))
    (is (null (amoebum.fp:plist-get-in nested '(:x :y :z))))))

(test fp-plist-select-keys-returns-requested-subset
  "`plist-select-keys` should return a plist containing only the
requested keys, in the requested order, skipping keys that are not
present in the source."
  (let* ((original '(:a 1 :b 2 :c 3 :d 4))
         (result   (amoebum.fp:plist-select-keys original '(:c :a :missing))))
    (is (equal '(:c 3 :a 1) result))
    ;; Original plist is untouched.
    (is (equal '(:a 1 :b 2 :c 3 :d 4) original))))
