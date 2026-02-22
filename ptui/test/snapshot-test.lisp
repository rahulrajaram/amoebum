(defpackage :ptui.test.snapshot
  (:use :cl :fiveam)
  (:export #:run-all #:snapshot-suite))

(in-package :ptui.test.snapshot)

(def-suite snapshot-suite
  :description "PTUI snapshot testing framework coverage.")

(in-suite snapshot-suite)

(defparameter *snapshot-dir*
  (asdf:system-relative-pathname "ptui/tests" "test/snapshots/"))

(defun snapshot-path (name)
  (merge-pathnames (pathname (format nil "~A.snap" name)) *snapshot-dir*))

;;; --- Buffer to snapshot round-trip ---

(test buffer-to-snapshot-basic
  (let* ((buf (ptui.render.buffer:make-buffer 10 3))
         (snap (ptui.test-support.snapshot:buffer-to-snapshot buf)))
    (is (stringp snap))
    (is (search "ptui-snapshot v1 10x3" snap))))

(test buffer-to-snapshot-with-text
  (let ((buf (ptui.render.buffer:make-buffer 10 3)))
    (ptui.render.buffer:buffer-draw-text buf 0 0 "Hello")
    (let ((snap (ptui.test-support.snapshot:buffer-to-snapshot buf)))
      (is (search "Hello" snap)))))

;;; --- Snapshot diff ---

(test snapshot-diff-identical
  (let* ((buf (ptui.render.buffer:make-buffer 5 2))
         (snap (ptui.test-support.snapshot:buffer-to-snapshot buf)))
    (is (null (ptui.test-support.snapshot:snapshot-diff snap snap)))))

(test snapshot-diff-detects-change
  (let* ((buf1 (ptui.render.buffer:make-buffer 5 2))
         (buf2 (ptui.render.buffer:make-buffer 5 2)))
    (ptui.render.buffer:buffer-draw-text buf2 0 0 "X")
    (let ((snap1 (ptui.test-support.snapshot:buffer-to-snapshot buf1))
          (snap2 (ptui.test-support.snapshot:buffer-to-snapshot buf2)))
      (is (stringp (ptui.test-support.snapshot:snapshot-diff snap1 snap2))))))

;;; --- Test backend ---

(test test-backend-creation
  (ptui.test-support.harness:with-test-terminal
      (:cols 40 :rows 10 :backend-var backend :buffer-var buffer)
    (is (not (null backend)))
    (is (not (null buffer)))
    (let ((size (ptui.backend.protocol:backend-size backend)))
      (is (= 40 (ptui.core.types:size-cols size)))
      (is (= 10 (ptui.core.types:size-rows size))))))

(test test-backend-event-injection
  (ptui.test-support.harness:with-test-terminal
      (:cols 20 :rows 5 :backend-var backend :buffer-var buffer)
    (is (null (ptui.backend.protocol:backend-poll-events backend)))
    (ptui.backend.test:test-backend-inject-events
     backend
     (list (ptui.core.events:make-key-event #\a)))
    (let ((events (ptui.backend.protocol:backend-poll-events backend)))
      (is (= 1 (length events)))
      (is (ptui.core.events:key-event-p (first events))))))

;;; --- Golden file assert-snapshot ---

(test assert-snapshot-creates-golden-on-first-run
  (let* ((tmp-path (merge-pathnames
                    (pathname (format nil "tmp-~A.snap" (get-universal-time)))
                    *snapshot-dir*))
         (buf (ptui.render.buffer:make-buffer 10 2)))
    (ptui.render.buffer:buffer-draw-text buf 0 0 "Test")
    (unwind-protect
         (progn
           (multiple-value-bind (pass diff)
               (handler-bind ((warning #'muffle-warning))
                 (ptui.test-support.harness:assert-snapshot buf tmp-path
                                                            :test-name "tmp-test"))
             (is (eq t pass))
             (is (null diff)))
           ;; Second run should match
           (multiple-value-bind (pass diff)
               (ptui.test-support.harness:assert-snapshot buf tmp-path
                                                          :test-name "tmp-test")
             (is (eq t pass))
             (is (null diff))))
      (when (probe-file tmp-path)
        (delete-file tmp-path)))))

;;; --- render-to-buffer helper ---

(test render-to-buffer-basic
  (let ((buf (ptui.test-support.harness:render-to-buffer
              (lambda (buf state cols rows)
                (declare (ignore state rows))
                (ptui.render.buffer:buffer-draw-text buf 0 0
                  (format nil "~Dx~D" cols 24)))
              nil
              :cols 20 :rows 5)))
    (let ((snap (ptui.test-support.snapshot:buffer-to-snapshot buf)))
      (is (search "20x24" snap)))))

(defun run-all ()
  (let ((results (run 'snapshot-suite)))
    (explain! results)
    (results-status results)))
