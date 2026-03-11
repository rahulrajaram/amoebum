(in-package :amoebum/test)

(def-suite runtime-log-suite
  :description "Runtime and crash logging tests."
  :in amoebum-suite)

(in-suite runtime-log-suite)

(defmacro with-runtime-log-environment ((runtime-log crash-log) &body body)
  `(let ((original-runtime-log (uiop:getenv "AMOEBUM_RUNTIME_LOG_FILE"))
         (original-crash-log (uiop:getenv "AMOEBUM_CRASH_LOG_FILE")))
     (unwind-protect
          (progn
            (setf (uiop:getenv "AMOEBUM_RUNTIME_LOG_FILE")
                  (and ,runtime-log (namestring ,runtime-log)))
            (setf (uiop:getenv "AMOEBUM_CRASH_LOG_FILE")
                  (and ,crash-log (namestring ,crash-log)))
            ,@body)
       (setf (uiop:getenv "AMOEBUM_RUNTIME_LOG_FILE") (or original-runtime-log ""))
       (setf (uiop:getenv "AMOEBUM_CRASH_LOG_FILE") (or original-crash-log "")))))

(defun %decode-runtime-log-lines (path)
  (with-open-file (stream path :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect (jonathan:parse line :as :hash-table))))

(test runtime-log-default-paths-live-under-amoebum-home
  (let* ((fake-home (merge-pathnames #P"fake-home/"
                                     (%make-temp-directory "amoebum-runtime-home")))
         (runtime-path (amoebum::default-runtime-log-path fake-home))
         (crash-path (amoebum::default-crash-log-path fake-home)))
    (unwind-protect
        (progn
          (is-true (search ".amoebum/runtime/runtime.log"
                           (namestring runtime-path)
                           :test #'char-equal))
          (is-true (search ".amoebum/runtime/crash.log"
                           (namestring crash-path)
                           :test #'char-equal)))
      (%delete-directory-tree-safe fake-home))))

(test runtime-log-writes-to-env-override
  (let* ((tmp-root (%make-temp-directory "amoebum-runtime-log"))
         (runtime-path (merge-pathnames #P"runtime/runtime.log" tmp-root))
         (crash-path (merge-pathnames #P"runtime/crash.log" tmp-root)))
    (unwind-protect
        (with-runtime-log-environment (runtime-path crash-path)
          (is-true
           (amoebum::log-runtime-event
            :level :info
            :kind "unit-runtime"
            :source :test
            :message "runtime log ok"
            :details '(:mode :test)))
          (let* ((entries (%decode-runtime-log-lines runtime-path))
                 (entry (first entries))
                 (details (gethash "details" entry)))
            (is (= 1 (length entries)))
            (is (string= "unit-runtime" (gethash "kind" entry)))
            (is (string= "runtime log ok" (gethash "message" entry)))
            (is (hash-table-p details))
            (is (string= "test" (gethash "mode" details)))))
      (%delete-directory-tree-safe tmp-root))))

(test crash-log-includes-condition-and-backtrace
  (let* ((tmp-root (%make-temp-directory "amoebum-crash-log"))
         (runtime-path (merge-pathnames #P"runtime/runtime.log" tmp-root))
         (crash-path (merge-pathnames #P"runtime/crash.log" tmp-root)))
    (unwind-protect
        (with-runtime-log-environment (runtime-path crash-path)
          (is-true
           (amoebum::log-runtime-condition
            (make-condition 'simple-error :format-control "boom")
            :kind "unit-crash"
            :source :test
            :message "crash log ok"
            :details '(:mode :test)
            :path crash-path))
          (let* ((entries (%decode-runtime-log-lines crash-path))
                 (entry (first entries)))
            (is (= 1 (length entries)))
            (is (string= "unit-crash" (gethash "kind" entry)))
            (is (string= "boom" (gethash "condition" entry)))
            (is-true (stringp (gethash "backtrace" entry)))
            (is-true (plusp (length (gethash "backtrace" entry))))))
      (%delete-directory-tree-safe tmp-root))))
