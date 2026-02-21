(defpackage :ptui.term.tty
  (:use :cl)
  (:export #:with-raw-tty #:tty-get-size #:tty-read-bytes
           #:tty-stdin-fd #:tty-stdout-fd))

(in-package :ptui.term.tty)

(defparameter +native-library-name+ "libptui_native.so")
(defparameter +tty-state-bytes+ 1024)
(defvar *native-library-loaded-p* nil)

(cffi:defcfun ("ptui_tty_enable_raw" %ptui-tty-enable-raw) :int
  (fd :int)
  (state :pointer))

(cffi:defcfun ("ptui_tty_restore" %ptui-tty-restore) :int
  (fd :int)
  (state :pointer))

(cffi:defcfun ("ptui_tty_get_winsz" %ptui-tty-get-winsz) :int
  (fd :int)
  (rows :pointer)
  (cols :pointer))

(cffi:defcfun ("ptui_fd_set_nonblocking" %ptui-fd-set-nonblocking) :int
  (fd :int)
  (nonblocking :int))

(cffi:defcfun ("ptui_fd_read" %ptui-fd-read) :long
  (fd :int)
  (buf :pointer)
  (cap :size))

(defun tty-stdin-fd ()
  0)

(defun tty-stdout-fd ()
  1)

(defun %directory-of (pathspec)
  (and pathspec
       (handler-case
           (uiop:pathname-directory-pathname (uiop:parse-native-namestring pathspec))
         (error ()
           nil))))

(defun %candidate-library-paths ()
  (remove-duplicates
   (remove nil
           (list
            (uiop:getenv "PTUI_NATIVE_LIB")
            (let ((src-dir (or (and *load-truename* (uiop:pathname-directory-pathname *load-truename*))
                               (and *compile-file-truename*
                                    (uiop:pathname-directory-pathname *compile-file-truename*)))))
              (and src-dir
                   (uiop:native-namestring
                    (merge-pathnames #P"../../native/libptui_native.so" src-dir))))
            (uiop:native-namestring
             (merge-pathnames #P"native/libptui_native.so" (uiop:getcwd)))
            (uiop:native-namestring
             (merge-pathnames #P"dist/libptui_native.so" (uiop:getcwd)))
            (uiop:native-namestring
             (merge-pathnames #P"ptui/native/libptui_native.so" (uiop:getcwd)))
            (uiop:native-namestring
             (merge-pathnames #P"ptui/dist/libptui_native.so" (uiop:getcwd)))
            (let ((argv-dir (%directory-of (uiop:argv0))))
              (and argv-dir
                   (uiop:native-namestring
                    (merge-pathnames +native-library-name+ argv-dir))))
            ;; On Linux, resolve the actual binary via /proc/self/exe
            ;; (handles wrapper scripts that exec the real binary)
            (handler-case
                (let ((exe (uiop:native-namestring (truename #P"/proc/self/exe"))))
                  (and exe
                       (uiop:native-namestring
                        (merge-pathnames +native-library-name+
                                         (uiop:pathname-directory-pathname
                                          (uiop:parse-native-namestring exe))))))
              (error () nil))
            +native-library-name+))
   :test #'string=))

(defun %ensure-native-loaded ()
  (unless *native-library-loaded-p*
    (let ((loaded nil)
          (last-error nil))
      (dolist (candidate (%candidate-library-paths))
        (unless loaded
          (handler-case
              (progn
                (cffi:load-foreign-library candidate)
                (setf loaded t))
            (error (err)
              (setf last-error err)))))
      (unless loaded
        (error "Failed to load ~A from candidates ~S (~A)"
               +native-library-name+
               (%candidate-library-paths)
               last-error))
      (setf *native-library-loaded-p* t))))

(defun %tty-enable-raw ()
  (%ensure-native-loaded)
  (let* ((stdin-fd (tty-stdin-fd))
         (state-ptr (cffi:foreign-alloc :uchar :count +tty-state-bytes+)))
    (if (not (zerop (%ptui-tty-enable-raw stdin-fd state-ptr)))
        (progn
          (cffi:foreign-free state-ptr)
          nil)
      (if (not (zerop (%ptui-fd-set-nonblocking stdin-fd 1)))
          (progn
            (ignore-errors
              (%ptui-tty-restore stdin-fd state-ptr))
            (cffi:foreign-free state-ptr)
            nil)
        state-ptr))))

(defun %tty-restore-raw (state-ptr)
  (when state-ptr
    (let ((stdin-fd (tty-stdin-fd)))
      (unwind-protect
           (progn
             (ignore-errors
               (%ptui-fd-set-nonblocking stdin-fd 0))
             (ignore-errors
               (%ptui-tty-restore stdin-fd state-ptr)))
        (cffi:foreign-free state-ptr))))
  nil)

(defmacro with-raw-tty (() &body body)
  `(let ((state-ptr nil))
     (unwind-protect
          (progn
            (setf state-ptr (%tty-enable-raw))
            ,@body)
       (ignore-errors
         (%tty-restore-raw state-ptr)))))

(defun tty-get-size ()
  (%ensure-native-loaded)
  (cffi:with-foreign-objects ((rows :int) (cols :int))
    (if (zerop (%ptui-tty-get-winsz (tty-stdout-fd) rows cols))
        (ptui.core.types:make-size
         (max 1 (cffi:mem-ref cols :int))
         (max 1 (cffi:mem-ref rows :int)))
      (ptui.core.types:make-size
       (or (ignore-errors (parse-integer (or (uiop:getenv "COLUMNS") ""))) 80)
       (or (ignore-errors (parse-integer (or (uiop:getenv "LINES") ""))) 24)))))

(defun tty-read-bytes (&key (max 4096))
  (%ensure-native-loaded)
  (let* ((cap (max 1 max))
         (empty (make-array 0 :element-type '(unsigned-byte 8))))
    (cffi:with-foreign-object (buf :uchar cap)
      (let ((count (%ptui-fd-read (tty-stdin-fd) buf cap)))
        (if (<= count 0)
            empty
          (let ((bytes (make-array count :element-type '(unsigned-byte 8))))
            (dotimes (i count bytes)
              (setf (aref bytes i) (cffi:mem-aref buf :uchar i)))))))))
