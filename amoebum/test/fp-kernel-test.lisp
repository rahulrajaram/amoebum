;;;; amoebum/test/fp-kernel-test.lisp
;;;;
;;;; Smoke tests for the minimal functional/immutable kernel in
;;;; `amoebum.fp`. One positive case per export, plus a couple of
;;;; short-circuit / composition cases.

(in-package :amoebum/test)

(test fp-thread-first-composes-left-to-right
  "`->` should compose forms in first-argument position."
  (is (= 8 (amoebum.fp:-> 3 1+ (* 2))))
  (is (= 5 (amoebum.fp:-> 2 1+ 1+ 1+))))

(test fp-thread-last-composes-with-last-argument
  "`->>` should compose forms in last-argument position."
  (is (equal '(2 3 4)
             (amoebum.fp:->> '(1 2 3) (mapcar #'1+))))
  (is (= 9
         (amoebum.fp:->> '(1 2 3) (mapcar #'1+) (reduce #'+)))))

(test fp-ok-err-round-trip
  "ok/err constructors and accessors preserve values."
  (let ((o (amoebum.fp:make-ok :value 42))
        (e (amoebum.fp:make-err :value "boom")))
    (is (amoebum.fp:ok-p o))
    (is (amoebum.fp:err-p e))
    (is (= 42 (amoebum.fp:ok-value o)))
    (is (equal "boom" (amoebum.fp:err-value e)))))

(test fp-result-map-applies-fn-to-ok
  "result-map transforms ok values and leaves err alone."
  (let ((o (amoebum.fp:make-ok :value 10))
        (e (amoebum.fp:make-err :value :bad)))
    (is (= 20 (amoebum.fp:ok-value
               (amoebum.fp:result-map (lambda (x) (* x 2)) o))))
    (is (eq :bad (amoebum.fp:err-value
                  (amoebum.fp:result-map (lambda (x) (* x 2)) e))))))

(test fp-result-bind-short-circuits-on-err
  "result-bind runs fn on ok and short-circuits on err."
  (let* ((ok-in  (amoebum.fp:make-ok :value 5))
         (err-in (amoebum.fp:make-err :value :nope))
         (step (lambda (x) (amoebum.fp:make-ok :value (* x 10))))
         (via-ok  (amoebum.fp:result-bind ok-in step))
         (via-err (amoebum.fp:result-bind err-in step)))
    (is (amoebum.fp:ok-p via-ok))
    (is (= 50 (amoebum.fp:ok-value via-ok)))
    (is (amoebum.fp:err-p via-err))
    (is (eq :nope (amoebum.fp:err-value via-err)))))

(test fp-assoc-in-updates-3-deep-plist
  "assoc-in writes into a 3-level nested plist without mutation."
  (let* ((original '(:a (:b (:c 1))))
         (updated  (amoebum.fp:assoc-in original '(:a :b :c) 42)))
    (is (equal '(:a (:b (:c 42))) updated))
    ;; original is untouched
    (is (equal '(:a (:b (:c 1))) original))))

(test fp-update-in-increments-counter
  "update-in applies fn to the existing value at the nested path."
  (let* ((original '(:a (:n 0)))
         (updated  (amoebum.fp:update-in original '(:a :n) #'1+)))
    (is (equal '(:a (:n 1)) updated))))

(test fp-update-macro-sets-two-plist-slots
  "`update` returns a new plist with each (:slot value) applied."
  (let* ((p (list :a 1 :b 2))
         (p2 (amoebum.fp:update p (:a 10) (:c 3))))
    (is (equal 10 (getf p2 :a)))
    (is (equal 2  (getf p2 :b)))
    (is (equal 3  (getf p2 :c)))
    ;; original untouched
    (is (equal 1 (getf p :a)))
    (is (null (getf p :c)))))
