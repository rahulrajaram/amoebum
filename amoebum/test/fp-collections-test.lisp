;;;; amoebum/test/fp-collections-test.lisp
;;;;
;;;; Focused regression coverage for the collection helpers in
;;;; `amoebum/src/fp/collections.lisp`.

(in-package :amoebum/test)

(def-suite fp-collections-suite :in amoebum-suite
  :description "Focused tests for amoebum.fp collection helpers.")

(in-suite fp-collections-suite)

(test fp-filter-map-collects-non-nil-results
  (let* ((source '(1 2 3 4 5))
         (result (amoebum.fp:filter-map
                  (lambda (n)
                    (when (evenp n)
                      (* n 10)))
                  source)))
    (is (equal '(20 40) result))
    (is (equal '(1 2 3 4 5) source))))

(test fp-group-by-preserves-key-and-item-order
  (let* ((source '("ant" "ape" "bat" "bear" "cat"))
         (result (amoebum.fp:group-by
                  (lambda (word) (char word 0))
                  source)))
    (is (equal
         (list (cons #\a '("ant" "ape"))
               (cons #\b '("bat" "bear"))
               (cons #\c '("cat")))
         result))
    (is (equal '("ant" "ape" "bat" "bear" "cat") source))))

(test fp-index-by-keeps-latest-value-per-key
  (let* ((source '((:id . "a") (:id . "b") (:name . "first") (:name . "second")))
         (result (amoebum.fp:index-by #'car source)))
    (is (equal
         (list (cons :id '(:id . "b"))
               (cons :name '(:name . "second")))
         result))
    (is (equal '((:id . "a") (:id . "b") (:name . "first") (:name . "second"))
               source))))

(test fp-first-some-returns-first-successful-value
  (is (equal "BEAR"
             (amoebum.fp:first-some
              (lambda (word)
                (when (> (length word) 3)
                  (string-upcase word)))
              '("ant" "bee" "bear" "cat"))))
  (is (null (amoebum.fp:first-some #'identity '(nil nil nil)))))

(test fp-partition-splits-matching-and-rest
  (multiple-value-bind (evens odds)
      (amoebum.fp:partition #'evenp '(1 2 3 4 5 6))
    (is (equal '(2 4 6) evens))
    (is (equal '(1 3 5) odds))))

(test fp-map-values-transforms-alist-values-purely
  (let* ((source '((:queued . 1) (:running . 2) (:done . 3)))
         (result (amoebum.fp:map-values #'1+ source)))
    (is (equal '((:queued . 2) (:running . 3) (:done . 4)) result))
    (is (equal '((:queued . 1) (:running . 2) (:done . 3)) source))))

(in-suite amoebum-suite)
