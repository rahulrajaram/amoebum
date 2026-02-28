;;; check-parens.lisp — Pre-merge paren balance checker
;;;
;;; Uses SBCL's reader with *READ-SUPPRESS* to verify that every .lisp file
;;; has balanced parentheses without evaluating any code.
;;;
;;; Usage:
;;;   sbcl --script scripts/check-parens.lisp [directory ...]
;;;
;;; Defaults to checking ptui/src/ ptui/test/ amoebum/src/ sw4rm-sdk/src/
;;; if no arguments are given.

(defun check-file-parens (path)
  "Read PATH with *READ-SUPPRESS* T. Returns T if balanced, NIL + condition on error."
  (handler-case
      (with-open-file (stream path :direction :input :external-format :utf-8)
        (let ((*read-suppress* t)
              (*readtable* (copy-readtable nil))
              (*package* (find-package :cl-user)))
          (loop (let ((form (read stream nil :eof)))
                  (when (eq form :eof)
                    (return t))))))
    (end-of-file (c)
      (values nil (format nil "unmatched open paren: ~A" c)))
    (reader-error (c)
      (values nil (format nil "reader error: ~A" c)))
    (error (c)
      (values nil (format nil "unexpected error: ~A" c)))))

(defun collect-lisp-files (directory)
  "Recursively collect all .lisp files under DIRECTORY."
  (let ((files '()))
    (labels ((walk (dir)
               (dolist (entry (directory (merge-pathnames "*.*" dir)))
                 (cond
                   ((and (pathname-name entry)
                         (string-equal "lisp" (pathname-type entry)))
                    (push entry files))
                   ((and (not (pathname-name entry))
                         (not (pathname-type entry)))
                    ;; It's a directory
                    nil)))
               ;; Recurse into subdirectories
               (dolist (subdir (directory (merge-pathnames "*/" dir)))
                 (walk subdir))))
      (walk (truename directory)))
    (sort files #'string< :key #'namestring)))

(defun main ()
  (let* ((args (rest sb-ext:*posix-argv*))
         (dirs (or args '("ptui/src/" "ptui/test/" "amoebum/src/" "sw4rm-sdk/src/")))
         (fail-count 0)
         (check-count 0))
    (dolist (dir dirs)
      (let ((dir-path (merge-pathnames dir (truename "."))))
        (unless (probe-file dir-path)
          (format *error-output* "SKIP ~A (not found)~%" dir)
          (go-on))
        (dolist (file (collect-lisp-files dir-path))
          (incf check-count)
          (multiple-value-bind (ok msg) (check-file-parens file)
            (if ok
                (format t "  OK ~A~%" (enough-namestring file))
                (progn
                  (incf fail-count)
                  (format *error-output* "FAIL ~A~%     ~A~%" (enough-namestring file) msg)))))))
    (format t "~%Checked ~D files. ~D failures.~%" check-count fail-count)
    (sb-ext:exit :code (if (zerop fail-count) 0 1))))

;; CL doesn't have tagbody GO across functions; use restart instead.
(defun go-on () nil)

(main)
