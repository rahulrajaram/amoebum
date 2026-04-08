(defpackage :ptui.test-support.harness
  (:use :cl)
  (:export #:render-to-buffer
           #:assert-snapshot
           #:with-test-terminal))

(in-package :ptui.test-support.harness)

(defun render-to-buffer (render-fn state &key (cols 80) (rows 24))
  "Call RENDER-FN with STATE and a fresh buffer, return the buffer.
RENDER-FN should accept (buffer state cols rows)."
  (let ((buf (ptui.render.buffer:make-buffer cols rows)))
    (funcall render-fn buf state cols rows)
    buf))

(defun assert-snapshot (buffer golden-path &key (test-name "snapshot"))
  "Compare BUFFER against golden file at GOLDEN-PATH.
Returns T on match. On first run (no golden file), creates it and warns.
When PTUI_UPDATE_SNAPSHOTS=1, always overwrites the golden file.
Returns (values pass-p diff-message)."
  (let* ((actual (ptui.test-support.snapshot:buffer-to-snapshot buffer))
         (golden (ptui.test-support.snapshot:load-golden-file golden-path)))
    (cond
      ;; Update mode: always overwrite
      (ptui.test-support.snapshot:*update-snapshots-p*
       (ptui.test-support.snapshot:save-golden-file golden-path actual)
       (values t nil))
      ;; First run: create golden file, warn
      ((null golden)
       (ptui.test-support.snapshot:save-golden-file golden-path actual)
       (warn "Snapshot ~A: golden file created at ~A (first run)"
              test-name (namestring golden-path))
       (values t nil))
      ;; Normal comparison
      (t
       (let ((diff (ptui.test-support.snapshot:snapshot-diff golden actual)))
         (if diff
             (values nil (format nil "Snapshot ~A mismatch:~%~A" test-name diff))
             (values t nil)))))))

(defmacro with-test-terminal ((&key (cols 80) (rows 24)
                                    (backend-var 'backend)
                                    (buffer-var 'buffer))
                               &body body)
  "Create a test backend and bind it along with its buffer for testing."
  `(let* ((,backend-var (ptui.backend.test:make-test-backend :cols ,cols :rows ,rows))
          (,buffer-var (ptui.backend.test:test-backend-buffer ,backend-var)))
     (declare (ignorable ,buffer-var))
     ,@body))
