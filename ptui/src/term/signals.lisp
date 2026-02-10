(defpackage :ptui.term.signals
  (:use :cl)
  (:export #:signals-init #:signals-poll))

(in-package :ptui.term.signals)

(cffi:defcfun ("ptui_signals_init" %ptui-signals-init) :int)
(cffi:defcfun ("ptui_signals_fd" %ptui-signals-fd) :int)
(cffi:defcfun ("ptui_signals_read" %ptui-signals-read) :int
  (signo :pointer))

(defun %signo->keyword (signo)
  (case signo
    (2 :int)
    (15 :term)
    (28 :winch)
    (otherwise (intern (format nil "SIG-~D" signo) :keyword))))

(defun signals-init ()
  (ptui.term.tty::%ensure-native-loaded)
  (unless (zerop (%ptui-signals-init))
    (error "ptui_signals_init failed"))
  (unless (>= (%ptui-signals-fd) 0)
    (error "ptui_signals_fd failed"))
  nil)

(defun signals-poll ()
  (ptui.term.tty::%ensure-native-loaded)
  (let ((signals '()))
    (cffi:with-foreign-object (signo :int)
      (loop
        for status = (%ptui-signals-read signo)
        do (cond
             ((= status 1)
              (push (%signo->keyword (cffi:mem-ref signo :int)) signals))
             ((= status 0)
              (return))
             (t
              (error "ptui_signals_read failed")))))
    (nreverse signals)))
