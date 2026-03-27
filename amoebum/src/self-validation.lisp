(in-package :amoebum)

;;; ============================================================================
;;; Amoebum Self-Validation System
;;; ============================================================================
;;;
;;; Prevents silent compilation failures by validating source integrity
;;; before and during system load. Catches issues like:
;;;   - Unbalanced parentheses
;;;   - Missing function definitions  
;;;   - Syntax errors that compile but break functionality
;;;
;;; This addresses the March 23, 2025 incident where yaml-theme-loader.lisp
;;; compiled with syntax errors, causing load-yaml-theme to be undefined.
;;;
;;; ============================================================================

(defparameter *self-validation-enabled* t
  "When non-nil, amoebum validates its own source during load.")

(defparameter *self-validation-strict* nil
  "When non-nil, validation failures are hard errors (not warnings).")

(defvar *source-definition-registry* (make-hash-table :test #'equal)
  "Registry tracking expected vs actual definitions.
Maps file-path -> list of (:name symbol :type :function/:macro/:class :defined-p boolean)")

;;; ---------------------------------------------------------------------------
;;; Paren Balance Validation
;;; ---------------------------------------------------------------------------

(defun %count-chars-in-string (string char)
  "Count occurrences of CHAR in STRING."
  (loop for c across string
        when (char= c char)
        sum 1))

(defun %paren-balance-in-form (form-string)
  "Check paren balance in a string.
Returns (:balanced-p t/nil :open N :close N :line N :column N)"
  (let ((open-count 0)
        (close-count 0)
        (line 1)
        (col 0)
        (in-string nil)
        (last-open-pos nil))
    (loop for char across form-string
          for i from 0
          do (case char
               (#\Newline (incf line) (setf col 0))
               (#\" (setf in-string (not in-string)))
               (#\( (unless in-string
                      (incf open-count)
                      (setf last-open-pos (cons line col))))
               (#\) (unless in-string
                      (incf close-count)
                      (when (< close-count open-count)
                        ;; Track imbalance
                        ))))
          do (incf col))
    (list :balanced-p (= open-count close-count)
          :open open-count
          :close close-count
          :line (car last-open-pos)
          :column (cdr last-open-pos))))

(defun %file-paren-balance (pathname)
  "Check paren balance in a Lisp source file.
Returns (:ok t/nil :issues (list...))"
  (let ((content (uiop:read-file-string pathname))
        (issues '()))
    ;; Check parens
    (let ((paren-count (%count-chars-in-string content #\())
          (close-count (%count-chars-in-string content #\))))
      (unless (= paren-count close-count)
        (push (list :type :unbalanced-parens
                    :file pathname
                    :open paren-count
                    :close close-count
                    :message (format nil "Unbalanced parens: ~D open, ~D close"
                                     paren-count close-count))
              issues)))
    ;; Check brackets
    (let ((open-bracket (%count-chars-in-string content #\[))
          (close-bracket (%count-chars-in-string content #\])))
      (unless (= open-bracket close-bracket)
        (push (list :type :unbalanced-brackets
                    :file pathname
                    :open open-bracket
                    :close close-bracket
                    :message (format nil "Unbalanced brackets: ~D open, ~D close"
                                     open-bracket close-bracket))
              issues)))
    ;; Check braces
    (let ((open-brace (%count-chars-in-string content #\{))
          (close-brace (%count-chars-in-string content #\})))
      (unless (= open-brace close-brace)
        (push (list :type :unbalanced-braces
                    :file pathname
                    :open open-brace
                    :close close-brace
                    :message (format nil "Unbalanced braces: ~D open, ~D close"
                                     open-brace close-brace))
              issues)))
    ;; Check unclosed strings (rough)
    (let ((quote-count (%count-chars-in-string content #\")))
      (when (oddp quote-count)
        (push (list :type :unclosed-string
                    :file pathname
                    :count quote-count
                    :message "Odd number of double quotes - possible unclosed string")
              issues)))
    
    (list :ok (null issues)
          :issues (nreverse issues))))

;;; ---------------------------------------------------------------------------
;;; Definition Tracking
;;; ---------------------------------------------------------------------------

(defun %extract-top-level-definitions (pathname)
  "Scan a Lisp file for expected top-level definitions.
Returns list of (:name symbol :type keyword :line integer)"
  (let ((definitions '())
        (current-package (or (find-package :amoebum) *package*))
        (line 0))
    (handler-case
        (with-open-file (stream pathname :direction :input :external-format :utf-8)
          (loop
            (let ((form (handler-case
                            (read stream nil :eof)
                          (error (e)
                            (push (list :name :parse-error
                                        :type :error
                                        :line line
                                        :error (princ-to-string e))
                                  definitions)
                            :eof))))
              (incf line)
              (when (eq form :eof)
                (return))
              
              ;; Track in-package changes
              (when (and (consp form)
                         (eq (first form) 'in-package))
                (let ((pkg-designator (second form)))
                  (setf current-package
                        (or (and (packagep pkg-designator) pkg-designator)
                            (find-package pkg-designator)
                            current-package))))
              
              ;; Track definitions
              (when (consp form)
                (let ((head (first form)))
                  (case head
                    (defun (push (list :name (second form)
                                       :type :function
                                       :package (package-name current-package)
                                       :line line)
                                 definitions))
                    (defmacro (push (list :name (second form)
                                          :type :macro
                                          :package (package-name current-package)
                                          :line line)
                                    definitions))
                    (defmethod (push (list :name (second form)
                                           :type :method
                                           :package (package-name current-package)
                                           :line line)
                                     definitions))
                    (defclass (push (list :name (second form)
                                          :type :class
                                          :package (package-name current-package)
                                          :line line)
                                    definitions))
                    (defstruct (let ((name (if (consp (second form))
                                               (first (second form))
                                               (second form))))
                                 (push (list :name name
                                             :type :struct
                                             :package (package-name current-package)
                                             :line line)
                                       definitions))))
                  )))))
      (error (e)
        (push (list :name :file-read-error
                    :type :error
                    :line 0
                    :error (princ-to-string e))
              definitions)))
    (nreverse definitions)))

;;; ---------------------------------------------------------------------------
;;; Critical Function Verification
;;; ---------------------------------------------------------------------------

(defparameter *critical-amoebum-functions*
  '(;; Core UI functions
    load-yaml-theme
    reload-yaml-theme-if-changed
    ;; Tool system
    deftool
    find-tool-metadata
    execute-tool
    ;; Event system
    emit-event
    subscribe-event
    ;; Config
    current-config
    load-config
    ;; Main entry
    main
    %cli-handle-command)
  "Functions that MUST be defined for amoebum to work correctly.")

(defun %verify-critical-functions-exist ()  
  "Verify that all critical functions are actually defined.
Returns (:ok t/nil :missing (list...))"
  (let ((missing '()))
    (dolist (fn-name *critical-amoebum-functions*)
      (let ((symbol (find-symbol (symbol-name fn-name) :amoebum)))
        (unless (and symbol (fboundp symbol))
          (push fn-name missing))))
    (list :ok (null missing)
          :missing (nreverse missing))))

;;; ---------------------------------------------------------------------------
;;; Public API
;;; ---------------------------------------------------------------------------

(define-condition self-validation-error (error)
  ((issues :initarg :issues
           :reader self-validation-error-issues))
  (:report (lambda (condition stream)
             (format stream "Amoebum self-validation failed with ~D issue(s):~%"
                     (length (self-validation-error-issues condition)))
             (dolist (issue (self-validation-error-issues condition))
               (format stream "  - ~A: ~A~%" 
                       (getf issue :type)
                       (getf issue :message))))))

(define-condition self-validation-warning (warning)
  ((message :initarg :message
            :reader self-validation-warning-message)))

(defun validate-amoebum-source-file (pathname)
  "Validate a single amoebum source file.
Returns validation result plist."
  (let* ((balance-result (%file-paren-balance pathname))
         (definitions (%extract-top-level-definitions pathname))
         (parse-errors (remove-if-not (lambda (d) (eq (getf d :type) :error))
                                      definitions))
         (all-issues (append (getf balance-result :issues)
                             parse-errors)))
    (list :file pathname
          :ok (and (getf balance-result :ok) (null parse-errors))
          :paren-balance balance-result
          :definitions definitions
          :issues all-issues)))

(defun validate-all-amoebum-sources (&key (verbose nil))
  "Validate all amoebum source files.
Returns (:ok t/nil :results (list...) :total-files N :failed N)"
  (let* ((source-dir (asdf:system-source-directory :amoebum))
         (src-dir (merge-pathnames #P"src/" source-dir))
         (files '()))
    ;; Collect all .lisp files
    (uiop:collect-sub*directories src-dir t t
                                  (lambda (dir)
                                    (dolist (file (uiop:directory-files dir "*.lisp"))
                                      (push file files))))
    (setf files (nreverse files))
    
    (let ((results '())
          (failed 0))
      (dolist (file files)
        (when verbose
          (format *error-output* "Validating ~A...~%" (file-namestring file)))
        (let ((result (validate-amoebum-source-file file)))
          (push result results)
          (unless (getf result :ok)
            (incf failed))))
      
      ;; Also verify critical functions
      (let ((critical-result (%verify-critical-functions-exist)))
        (unless (getf critical-result :ok)
          (push (list :file :critical-functions
                      :ok nil
                      :issues (mapcar (lambda (fn)
                                        (list :type :missing-critical-function
                                              :name fn
                                              :message (format nil "Critical function ~A not defined" fn)))
                                      (getf critical-result :missing)))
                results)
          (incf failed)))
      
      (let ((result (list :ok (zerop failed)
                          :results (nreverse results)
                          :total-files (length files)
                          :failed failed)))
        (unless (getf result :ok)
          (warn 'self-validation-warning
                :message (format nil "~D source file(s) failed validation" failed)))
        result))))

(defun run-amoebum-self-validation (&key (error-on-failure *self-validation-strict*)
                                         (verbose t))
  "Run full self-validation and optionally signal error on failure.
Returns t if validation passes, nil otherwise."
  (when verbose
    (format *error-output* "~%Running amoebum self-validation...~%"))
  
  (let ((result (validate-all-amoebum-sources :verbose verbose)))
    (cond
      ((getf result :ok)
       (when verbose
         (format *error-output* "✓ Self-validation passed (~D files checked)~%"
                 (getf result :total-files)))
       t)
      (error-on-failure
       (error 'self-validation-error
              :issues (loop for r in (getf result :results)
                            when (getf r :issues)
                            append (getf r :issues))))
      (t
       (when verbose
         (format *error-output* "✗ Self-validation failed (~D file(s) with issues)~%"
                 (getf result :failed))
         (dolist (r (getf result :results))
           (when (getf r :issues)
             (format *error-output* "  ~A:~%" (getf r :file))
             (dolist (issue (getf r :issues))
               (format *error-output* "    - ~A: ~A~%"
                       (getf issue :type)
                       (getf issue :message))))))
       nil))))

;;; ---------------------------------------------------------------------------
;;; Safe Load Wrapper
;;; ---------------------------------------------------------------------------

(defmacro with-self-validation (&body body)
  "Execute BODY with self-validation enabled.
If validation fails and *self-validation-strict* is set, signals error."
  `(progn
     (when *self-validation-enabled*
       (run-amoebum-self-validation :error-on-failure *self-validation-strict*))
     ,@body))

;;; ---------------------------------------------------------------------------
;;; Startup Hook
;;; ---------------------------------------------------------------------------

(defun maybe-run-self-validation-on-load ()  
  "Called during system initialization to validate source integrity.
Controlled by *self-validation-enabled*."
  (when *self-validation-enabled*
    (handler-case
        (run-amoebum-self-validation :error-on-failure *self-validation-strict*)
      (self-validation-error (e)
        (format *error-output* "~%FATAL: Amoebum source validation failed:~%~A~%" e)
        (format *error-output* "~%Run (amoebum::validate-all-amoebum-sources :verbose t) for details.~%")
        (uiop:quit 1))
      (error (e)
        (format *error-output* "~%Warning: Self-validation error: ~A~%" e)))))
