;;; -----------------------------------------------------------------------------
;;; Test for String Builder (P1 #5 Memory Thrashing Fix)
;;; -----------------------------------------------------------------------------

;; Load required packages
(in-package :cl-user)

;; Simple test runner
(defparameter *test-passed* 0)
(defparameter *test-failed* 0)

(defmacro test (name &body body)
  `(handler-case
       (progn
         (format t "Testing ~a... " ,name)
         ,@body
         (incf *test-passed*)
         (format t "PASSED~%"))
     (error (e)
       (incf *test-failed*)
       (format t "FAILED: ~a~%" e))))

(defun test-string-builder ()
  "Test string-builder functionality."
  (let ((*test-passed* 0)
        (*test-failed* 0))
    
    (format t "~%~a~%" (make-string 60 :initial-element #\=))
    (format t "String Builder Tests~%")
    (format t "~a~%~%" (make-string 60 :initial-element #\=))
    
    ;; Test 1: Create empty string builder
    (test "make-string-builder creates empty builder"
      (let ((sb (amoebum::make-string-builder)))
        (assert (typep sb 'amoebum::string-builder))
        (assert (= 0 (amoebum::string-builder-length sb)))))
    
    ;; Test 2: Append to empty builder
    (test "string-builder-append on empty"
      (let ((sb (amoebum::make-string-builder)))
        (amoebum::string-builder-append sb "hello")
        (assert (= 5 (amoebum::string-builder-length sb)))
        (assert (string= "hello" (amoebum::string-builder-get sb)))))
    
    ;; Test 3: Multiple appends
    (test "string-builder-append multiple times"
      (let ((sb (amoebum::make-string-builder)))
        (amoebum::string-builder-append sb "Hello ")
        (amoebum::string-builder-append sb "World")
        (amoebum::string-builder-append sb "!")
        (assert (= 11 (amoebum::string-builder-length sb)))
        (assert (string= "Hello World!" (amoebum::string-builder-get sb)))))
    
    ;; Test 4: Clear and reuse
    (test "string-builder-clear resets state"
      (let ((sb (amoebum::make-string-builder)))
        (amoebum::string-builder-append sb "temporary")
        (amoebum::string-builder-clear sb)
        (assert (= 0 (amoebum::string-builder-length sb)))
        (amoebum::string-builder-append sb "new")
        (assert (string= "new" (amoebum::string-builder-get sb)))))
    
    ;; Test 5: Large append triggers resize
    (test "string-builder auto-resizes when needed"
      (let ((sb (amoebum::make-string-builder 10)))  ; small initial size
        (amoebum::string-builder-append sb "this is a longer string than initial capacity")
        (assert (> (amoebum::string-builder-length sb) 10))
        (assert (string= "this is a longer string than initial capacity"
                        (amoebum::string-builder-get sb)))))
    
    ;; Test 6: Empty string append
    (test "string-builder-append handles empty string"
      (let ((sb (amoebum::make-string-builder)))
        (amoebum::string-builder-append sb "")
        (assert (= 0 (amoebum::string-builder-length sb)))
        (amoebum::string-builder-append sb "x")
        (assert (string= "x" (amoebum::string-builder-get sb)))))
    
    ;; Test 7: Chaining (returns sb)
    (test "string-builder-append returns sb for chaining"
      (let ((sb (amoebum::make-string-builder)))
        (assert (eq sb (amoebum::string-builder-append sb "x")))
        ;; Test chaining pattern
        (amoebum::string-builder-append
         (amoebum::string-builder-append sb "y") "z")
        (assert (string= "xyz" (amoebum::string-builder-get sb)))))
    
    ;; Summary
    (format t "~%~a~%" (make-string 60 :initial-element #\=))
    (format t "Results: ~a passed, ~a failed~%" *test-passed* *test-failed*)
    (format t "~a~%" (make-string 60 :initial-element #\=))
    
    (zerop *test-failed*)))

(defun test-streaming-integration ()
  "Test that streaming markdown renderer still works with string builder."
  (let ((*test-passed* 0)
        (*test-failed* 0))
    
    (format t "~%~a~%" (make-string 60 :initial-element #\=))
    (format t "Streaming Integration Tests~%")
    (format t "~a~%~%" (make-string 60 :initial-element #\=))
    
    ;; Test 1: Basic chunk append
    (test "streaming-markdown-renderer-append-chunk works"
      (let ((renderer (amoebum::make-streaming-markdown-renderer)))
        (setf (amoebum::streaming-markdown-renderer-width renderer) 80)
        (amoebum::streaming-markdown-renderer-append-chunk renderer "Hello")
        (amoebum::streaming-markdown-renderer-append-chunk renderer " World")
        (let ((lines (amoebum::streaming-markdown-renderer-render-lines renderer 80)))
          (assert (listp lines))
          (assert (> (length lines) 0)))))
    
    ;; Test 2: Multi-line chunks
    (test "streaming-markdown-renderer handles newlines"
      (let ((renderer (amoebum::make-streaming-markdown-renderer)))
        (setf (amoebum::streaming-markdown-renderer-width renderer) 80)
        (amoebum::streaming-markdown-renderer-append-chunk renderer "Line 1\nLine 2")
        (let ((lines (amoebum::streaming-markdown-renderer-render-lines renderer 80)))
          (assert (>= (length lines) 2)))))
    
    ;; Test 3: Multiple chunks accumulate
    (test "streaming-markdown-renderer accumulates chunks"
      (let ((renderer (amoebum::make-streaming-markdown-renderer)))
        (setf (amoebum::streaming-markdown-renderer-width renderer) 80)
        (dotimes (i 10)
          (amoebum::streaming-markdown-renderer-append-chunk 
           renderer (format nil "chunk ~a " i)))
        (let ((lines (amoebum::streaming-markdown-renderer-render-lines renderer 80)))
          (assert (listp lines)))))
    
    ;; Test 4: Empty chunks
    (test "streaming-markdown-renderer handles empty chunks"
      (let ((renderer (amoebum::make-streaming-markdown-renderer)))
        (setf (amoebum::streaming-markdown-renderer-width renderer) 80)
        (amoebum::streaming-markdown-renderer-append-chunk renderer "")
        (amoebum::streaming-markdown-renderer-append-chunk renderer "valid")
        (let ((lines (amoebum::streaming-markdown-renderer-render-lines renderer 80)))
          (assert (listp lines)))))
    
    ;; Test 5: Reset works
    (test "streaming-markdown-renderer-reset clears state"
      (let ((renderer (amoebum::make-streaming-markdown-renderer)))
        (setf (amoebum::streaming-markdown-renderer-width renderer) 80)
        (amoebum::streaming-markdown-renderer-append-chunk renderer "content")
        (amoebum::streaming-markdown-renderer-reset renderer)
        (assert (string= "" (amoebum::streaming-markdown-renderer-pending-line renderer)))
        (assert (null (amoebum::streaming-markdown-renderer-logical-lines renderer)))))
    
    ;; Summary
    (format t "~%~a~%" (make-string 60 :initial-element #\=))
    (format t "Results: ~a passed, ~a failed~%" *test-passed* *test-failed*)
    (format t "~a~%" (make-string 60 :initial-element #\=))
    
    (zerop *test-failed*)))

(defun run-all-tests ()
  "Run all string builder tests."
  (let ((sb-ok (test-string-builder))
        (integration-ok (test-streaming-integration)))
    (format t "~%~a~%" (make-string 60 :initial-element #\*))
    (if (and sb-ok integration-ok)
        (format t "ALL TESTS PASSED~%")
        (format t "SOME TESTS FAILED~%"))
    (format t "~a~%" (make-string 60 :initial-element #\*))
    (and sb-ok integration-ok)))

;; Run tests if this file is loaded directly
(when (find-package :amoebum)
  (run-all-tests))
