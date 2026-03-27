(in-package :cl-user)

;;; ============================================================================
;;; Early Compile-Time Validation
;;; ============================================================================
;;;
;;; This file is loaded BEFORE amoebum compilation to validate source files
;;; for syntax errors. It catches issues like unbalanced parens that would
;;; otherwise cause silent failures.
;;;
;;; Usage: (asdf:load-system :amoebum) will automatically run these checks
;;;        if *validate-before-compile* is non-nil.
;;;
;;; ============================================================================

(defparameter *amoebum-validate-before-compile* t
  "When non-nil, validate source files before compilation.")

(defun %amoebum-count-chars (string char)
  "Count occurrences of CHAR in STRING."
  (loop for c across string
        when (char= c char)
        sum 1))

(defun %amoebum-file-has-balanced-parens-p (pathname)
  "Quick check if a file has balanced parens/brackets/braces."
  (let ((content (uiop:read-file-string pathname)))
    (and (= (%amoebum-count-chars content #\() 
            (%amoebum-count-chars content #\)))
         (= (%amoebum-count-chars content #\[)
            (%amoebum-count-chars content #\]))
         (= (%amoebum-count-chars content #\{)
            (%amoebum-count-chars content #\})))))

(defun %amoebum-validate-source-for-compile (pathname)
  "Validate a source file before compilation.
Signals an error if validation fails and *amoebum-validate-before-compile* is strict."
  (when (and *amoebum-validate-before-compile*
             (string-equal (pathname-type pathname) "lisp"))
    (unless (%amoebum-file-has-balanced-parens-p pathname)
      (let ((content (uiop:read-file-string pathname)))
        (error "Syntax validation failed for ~A:~%  Parens: ~D open, ~D close~%  Brackets: ~D open, ~D close~%  Braces: ~D open, ~D close~%~%Fix syntax errors before compiling."
               pathname
               (%amoebum-count-chars content #\()
               (%amoebum-count-chars content #\))
               (%amoebum-count-chars content #\[)
               (%amoebum-count-chars content #\])
               (%amoebum-count-chars content #\{)
               (%amoebum-count-chars content #\}))))))

;;; Hook into ASDF compile operation
(defmethod asdf:perform :around ((op asdf:compile-op) (c asdf:cl-source-file))
  "Validate source files before compilation."
  (when *amoebum-validate-before-compile*
    (let ((pathname (asdf:component-pathname c)))
      ;; Only validate amoebum's own files
      (when (search "amoebum" (namestring pathname))
        (%amoebum-validate-source-for-compile pathname))))
  (call-next-method))

(format t "~&[amoebum] Compile-time validation enabled.~%")
