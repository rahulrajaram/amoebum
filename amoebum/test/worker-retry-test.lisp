(in-package :amoebum/test)

;;; ============================================================
;;; I260: Worker Retry and Supervision — Smoke Tests
;;; ============================================================

(def-suite worker-retry-suite :in amoebum-suite)
(in-suite worker-retry-suite)

;;; --- Policy structure ---

(test supervision-policy-struct-exists
  "worker-supervision-policy struct is defined."
  (is (fboundp 'amoebum:make-worker-supervision-policy))
  (is (fboundp 'amoebum:worker-supervision-policy-p))
  (is (fboundp 'amoebum:worker-supervision-policy-max-retries))
  (is (fboundp 'amoebum:worker-supervision-policy-backoff-strategy))
  (is (fboundp 'amoebum:worker-supervision-policy-backoff-base-seconds)))

(test default-supervision-policy-exists
  "Default supervision policy is bound."
  (is (boundp 'amoebum:*default-supervision-policy*))
  (is (amoebum:worker-supervision-policy-p amoebum:*default-supervision-policy*)))

(test make-supervision-policy-defaults
  "Default policy has 0 retries and :none backoff."
  (let ((policy (amoebum:make-worker-supervision-policy)))
    (is (= 0 (amoebum:worker-supervision-policy-max-retries policy)))
    (is (eq :none (amoebum:worker-supervision-policy-backoff-strategy policy)))
    (is (= 5 (amoebum:worker-supervision-policy-backoff-base-seconds policy)))))

(test make-supervision-policy-custom
  "Custom policy accepts all parameters."
  (let ((policy (amoebum:make-worker-supervision-policy
                 :max-retries 3
                 :backoff-strategy :exponential
                 :backoff-base-seconds 2
                 :transient-exit-codes '(1 75)
                 :permanent-exit-codes '(127))))
    (is (= 3 (amoebum:worker-supervision-policy-max-retries policy)))
    (is (eq :exponential (amoebum:worker-supervision-policy-backoff-strategy policy)))
    (is (= 2 (amoebum:worker-supervision-policy-backoff-base-seconds policy)))
    (is (member 75 (amoebum:worker-supervision-policy-transient-exit-codes policy)))))

;;; --- Failure classification (using synthetic worker records) ---

(test classify-worker-failure-exists
  "classify-worker-failure is a bound function."
  (is (fboundp 'amoebum:classify-worker-failure)))

(test classify-permanent-exit-code
  "Exit code 127 is classified as permanent."
  (let* ((old-sup amoebum:*worker-supervisor*)
         (worker (progn
                   (setf amoebum:*worker-supervisor* nil)
                   (amoebum:clear-workers)
                   ;; Build a synthetic worker record
                   (let ((w (amoebum::%make-worker-record
                             :id "w-test-perm"
                             :type :shell
                             :label "perm-test"
                             :status :failed
                             :exit-code 127)))
                     (setf amoebum:*worker-supervisor* old-sup)
                     w))))
    (let ((classification (amoebum:classify-worker-failure worker)))
      (is (eq :permanent classification)))))

(test classify-transient-exit-code
  "Exit code 75 (EX_TEMPFAIL) is classified as transient."
  (let ((worker (amoebum::%make-worker-record
                 :id "w-test-trans"
                 :type :shell
                 :label "trans-test"
                 :status :failed
                 :exit-code 75)))
    (let ((classification (amoebum:classify-worker-failure worker)))
      (is (eq :transient classification)))))

(test classify-timeout-as-transient
  "Timeout status is classified as transient."
  (let ((worker (amoebum::%make-worker-record
                 :id "w-test-timeout"
                 :type :shell
                 :label "timeout-test"
                 :status :timeout
                 :exit-code nil)))
    (let ((classification (amoebum:classify-worker-failure worker)))
      (is (eq :transient classification)))))

;;; --- Retry eligibility ---

(test worker-retry-eligible-p-exists
  "worker-retry-eligible-p is a bound function."
  (is (fboundp 'amoebum:worker-retry-eligible-p)))

(test retry-eligible-with-transient-failure
  "Worker with transient failure and retries remaining is eligible."
  (let* ((policy (amoebum:make-worker-supervision-policy
                  :max-retries 3
                  :transient-exit-codes '(1 75)))
         (worker (amoebum::%make-worker-record
                  :id "w-retry-elig"
                  :type :shell
                  :label "retry-elig"
                  :status :failed
                  :exit-code 75
                  :retry-count 0
                  :max-retries 3)))
    (is (amoebum:worker-retry-eligible-p worker policy))))

(test retry-not-eligible-permanent
  "Worker with permanent failure is not retry eligible."
  (let* ((policy (amoebum:make-worker-supervision-policy
                  :max-retries 3))
         (worker (amoebum::%make-worker-record
                  :id "w-no-retry"
                  :type :shell
                  :label "no-retry"
                  :status :failed
                  :exit-code 127
                  :retry-count 0)))
    (is (not (amoebum:worker-retry-eligible-p worker policy)))))

(test retry-not-eligible-max-retries-exhausted
  "Worker with max retries exhausted is not eligible."
  (let* ((policy (amoebum:make-worker-supervision-policy
                  :max-retries 2
                  :transient-exit-codes '(75)))
         (worker (amoebum::%make-worker-record
                  :id "w-exhausted"
                  :type :shell
                  :label "exhausted"
                  :status :failed
                  :exit-code 75
                  :retry-count 2
                  :max-retries 2)))
    (is (not (amoebum:worker-retry-eligible-p worker policy)))))

;;; --- Supervised spawn ---

(test spawn-worker-supervised-exists
  "spawn-worker-supervised is a bound function."
  (is (fboundp 'amoebum:spawn-worker-supervised)))

;;; --- Child supervision ---

(test child-worker-failed-condition-exists
  "child-worker-failed condition class exists."
  (is (find-class 'amoebum:child-worker-failed nil)))

(test supervise-child-worker-exists
  "supervise-child-worker is a bound function."
  (is (fboundp 'amoebum:supervise-child-worker)))
