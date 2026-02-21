(defpackage :ptui.test.app
  (:use :cl :fiveam)
  (:export #:run-all #:app-suite))

(in-package :ptui.test.app)

(def-suite app-suite
  :description "PTUI app shell: defapp, lifecycle, interceptors (I275-I277, I280).")

(in-suite app-suite)

(test app-config-creation
  (let ((config (ptui.ui.app:make-app-config
                 'test-app
                 :backend :ansi
                 :fps 30
                 :initial-state '(:count 0)
                 :interceptors '())))
    (is (eq (ptui.ui.app:app-config-name config) 'test-app))
    (is (eq (ptui.ui.app:app-config-backend config) :ansi))
    (is (= (ptui.ui.app:app-config-fps config) 30))))

(test populate-initial-state-works
  (let ((rt (ptui.ui.runtime:make-runtime)))
    (ptui.ui.app::%populate-initial-state rt '(:count 0 :name "test"))
    (is (= (ptui.ui.runtime:runtime-state rt :count) 0))
    (is (string= (ptui.ui.runtime:runtime-state rt :name) "test"))))

(test interceptors-priority-ordering
  (let ((log '()))
    (let ((interceptors
            (list (list 10 (constantly t) (lambda (e) (declare (ignore e)) (push :second log) nil))
                  (list 1 (constantly t) (lambda (e) (declare (ignore e)) (push :first log) :consumed)))))
      (let ((result (ptui.ui.app::%run-interceptors
                     (ptui.ui.app::%sorted-interceptors interceptors)
                     :test-event)))
        (is (eq result :consumed))
        ;; Only first interceptor should run (it consumed)
        (is (equal log '(:first)))))))

(test interceptors-pass-through-when-not-consumed
  (let ((log '()))
    (let ((interceptors
            (list (list 1 (constantly t) (lambda (e) (declare (ignore e)) (push :a log) nil))
                  (list 2 (constantly t) (lambda (e) (declare (ignore e)) (push :b log) nil)))))
      (let ((result (ptui.ui.app::%run-interceptors
                     (ptui.ui.app::%sorted-interceptors interceptors)
                     :test-event)))
        (is (null result))
        ;; Both should have run
        (is (= (length log) 2))))))

(test state-version-caching
  (let ((rt (ptui.ui.runtime:make-runtime)))
    (let ((v0 (ptui.ui.runtime:runtime-state-version rt)))
      (is (= v0 0))
      (ptui.ui.runtime:set-runtime-state rt :key1 "val1")
      (is (= (ptui.ui.runtime:runtime-state-version rt) 1))
      (ptui.ui.runtime:set-runtime-state rt :key2 "val2")
      (is (= (ptui.ui.runtime:runtime-state-version rt) 2)))))

(defun run-all ()
  (run! 'app-suite))
