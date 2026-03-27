;;; ===================================================================
;;; PTUI YAML Preview — CLI Entry Point
;;;
;;; Usage (from project root):
;;;   sbcl --load ptui/src/preview/loader.lisp -- path/to/file.tui-spec.yaml
;;; ===================================================================

(in-package :cl)
(require :asdf)

;; Compute the ptui/ directory from this file's location (ptui/src/preview/loader.lisp)
(let* ((this-dir (if *load-pathname*
                     (make-pathname :directory (pathname-directory *load-pathname*))
                     (uiop:getcwd)))
       ;; Go up from src/preview/ to ptui/
       (ptui-dir (merge-pathnames "../../" this-dir)))
  (pushnew (truename ptui-dir) asdf:*central-registry* :test #'equal))

;; Also add CWD in case user runs from ptui/ directory
(pushnew (truename (uiop:getcwd)) asdf:*central-registry* :test #'equal)

;; Load quicklisp if not already loaded
(unless (find-package :ql)
  (let ((ql-setup (merge-pathnames "quicklisp/setup.lisp"
                    (user-homedir-pathname))))
    (when (probe-file ql-setup)
      (load ql-setup))))

;; Also try the local ptui quicklisp
(unless (find-package :ql)
  (let* ((this-dir (if *load-pathname*
                       (make-pathname :directory (pathname-directory *load-pathname*))
                       (uiop:getcwd)))
         (ptui-ql (merge-pathnames "../../.tools/quicklisp/setup.lisp" this-dir)))
    (when (probe-file ptui-ql)
      (load ptui-ql))))

;; Load cl-yaml via quicklisp
(ql:quickload :cl-yaml :silent t)

;; Load the ptui-preview system
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "ptui-preview"))

;; Extract command-line argument (skip "--" separator if present)
(let* ((args (uiop:command-line-arguments))
       (args (if (and args (string= (first args) "--"))
                 (rest args)
                 args))
       (yaml-path (first args)))
  (unless yaml-path
    (format *error-output*
            "Usage: sbcl --load ptui/src/preview/loader.lisp -- <file.tui-spec.yaml>~%")
    (uiop:quit 1))
  (ptui.preview.app:run-preview yaml-path))
