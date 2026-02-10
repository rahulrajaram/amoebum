(defpackage :ptui.util.log
  (:use :cl)
  (:export #:*log-level* #:log-debug #:log-info #:log-warn #:log-error
           #:with-log-context #:log-kv
           #:resolve-log-level
           #:render-stats #:make-render-stats
           #:render-stats-frame-count #:render-stats-last-frame-ms
           #:render-stats-last-commit-bytes #:render-stats-last-diff-ops))

(in-package :ptui.util.log)

(defun resolve-log-level ()
  (let ((raw (uiop:getenv "PTUI_LOG_LEVEL")))
    (if raw
        (let* ((name (string-upcase raw))
               (sym (find-symbol name :keyword)))
          (if (assoc sym '((:debug . t) (:info . t) (:warn . t) (:error . t))
                     :test #'eq)
              sym
              :info))
        :info)))

(defparameter *log-level* :info)
(defparameter *log-context* nil)

(defparameter +level-order+
  '((:debug . 10)
    (:info . 20)
    (:warn . 30)
    (:error . 40)))

(defun level-enabled-p (level)
  (let ((current (or (cdr (assoc *log-level* +level-order+)) 20))
        (target (or (cdr (assoc level +level-order+)) 20)))
    (>= target current)))

(defun format-log-context ()
  (with-output-to-string (out)
    (dolist (entry *log-context*)
      (destructuring-bind (k . v) entry
        (format out " ~A=~S" k v)))))

(defun %log (level message &rest args)
  (when (level-enabled-p level)
    (let ((rendered (if args
                        (apply #'format nil message args)
                        message)))
      (format *error-output* "~&[~A] ~A~A~%"
              (string-upcase (symbol-name level))
              rendered
              (format-log-context))
      (finish-output *error-output*)))
  nil)

(defmacro log-debug (message &rest args)
  `(%log :debug ,message ,@args))

(defmacro log-info (message &rest args)
  `(%log :info ,message ,@args))

(defmacro log-warn (message &rest args)
  `(%log :warn ,message ,@args))

(defmacro log-error (message &rest args)
  `(%log :error ,message ,@args))

(defmacro with-log-context ((&rest kv-pairs) &body body)
  `(let ((*log-context* (append (list ,@kv-pairs) *log-context*)))
     ,@body))

(defun log-kv (&rest kv-pairs)
  (with-output-to-string (out)
    (let ((firstp t))
    (loop for (k v) on kv-pairs by #'cddr
          do (progn
               (unless firstp
                 (write-char #\Space out))
               (setf firstp nil)
               (format out "~A=~S" k v))))))

(defstruct (render-stats (:constructor make-render-stats ()))
  (frame-count 0 :type fixnum)
  (last-frame-ms 0 :type fixnum)
  (last-commit-bytes 0 :type fixnum)
  (last-diff-ops 0 :type fixnum))
