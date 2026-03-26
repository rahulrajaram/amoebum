;;;; lisp-paren-check.lisp — O(n) parenthesis balance scanner for Common Lisp source.
;;;;
;;;; Usage:
;;;;   sbcl --script bin/lisp-paren-check.lisp file1.lisp file2.lisp ...
;;;;   sbcl --script bin/lisp-paren-check.lisp --project   ; scans all .lisp under cwd
;;;;
;;;; Reports:
;;;;   - Files with non-zero final paren depth (unmatched open or close)
;;;;   - Exact line/column of first negative depth (extra close paren)
;;;;   - Which top-level form contains the imbalance
;;;;   - Per-file summary with exit code 0 (clean) or 1 (violations found)
;;;;
;;;; Handles: strings, #\x character literals (including #\( #\) #\Space etc.),
;;;; line comments (;), block comments (#|...|#), reader macros (#', #., #P, etc.)

(defpackage :paren-check
  (:use :cl))

(in-package :paren-check)

;;; ── Scanner state ──────────────────────────────────────────────────

(defstruct scanner
  (depth       0   :type fixnum)
  (in-string   nil :type boolean)
  (block-depth 0   :type fixnum)   ; nesting depth of #|...|# comments
  (escape-next nil :type boolean)  ; next char is escaped (in string: \x)
  (char-lit    nil :type boolean)  ; saw #\ , next char is the literal
  (prev-char   nil)               ; previous character (for #\ and #| detection)
  ;; per-file diagnostics
  (violations  '())
  (form-start-line 0 :type fixnum)
  (form-start-text ""))

(defstruct violation
  (file    "" :type string)
  (line    0  :type fixnum)
  (column  0  :type fixnum)
  (kind    :extra-close :type keyword)  ; :extra-close | :unclosed
  (depth   0  :type fixnum)
  (form-start-line 0 :type fixnum)
  (form-start-text "" :type string)
  (source-line "" :type string))

;;; ── Core scanner: process one character ────────────────────────────

(defun scan-char (s ch line-num col-num source-line)
  "Process a single character CH through scanner state S.
Returns S (mutated)."
  (let ((prev (scanner-prev-char s)))
    ;; Always update prev-char at the end
    (cond
      ;; ── Handle escape inside string ──
      ((scanner-escape-next s)
       (setf (scanner-escape-next s) nil))

      ;; ── Handle character literal: #\x ──
      ;; After seeing #\, consume exactly one character (the literal) and done.
      ((scanner-char-lit s)
       (setf (scanner-char-lit s) nil))

      ;; ── Inside block comment #|...|# ──
      ((plusp (scanner-block-depth s))
       (cond
         ;; Nested open: #|
         ((and (eql prev #\#) (eql ch #\|))
          (incf (scanner-block-depth s))
          ;; Clear prev so |# on next pair doesn't double-match
          (setf ch nil))
         ;; Close: |#
         ((and (eql prev #\|) (eql ch #\#))
          (decf (scanner-block-depth s))
          (setf ch nil))))

      ;; ── Inside string ──
      ((scanner-in-string s)
       (cond
         ((eql ch #\\)
          (setf (scanner-escape-next s) t))
         ((eql ch #\")
          (setf (scanner-in-string s) nil))))

      ;; ── Normal mode ──
      (t
       (cond
         ;; Line comment: ; to end of line — handled at line level, but
         ;; if we see ; we can skip. We handle this in scan-line instead.
         ;; (This branch won't fire because scan-line truncates at ;)

         ;; Start of block comment: #|
         ((and (eql prev #\#) (eql ch #\|))
          (incf (scanner-block-depth s))
          (setf ch nil))

         ;; Character literal: #\
         ((and (eql prev #\#) (eql ch #\\))
          (setf (scanner-char-lit s) t)
          (setf ch nil))

         ;; Reader macro: #' #. #( #S( #A( etc — for #( we need to count parens
         ;; #' and #. don't affect paren depth. #( opens a vector literal (still a paren).
         ;; So we just let ( and ) be counted normally, and # is ignored.

         ;; String open
         ((eql ch #\")
          (setf (scanner-in-string s) t))

         ;; Open paren
         ((eql ch #\()
          (when (zerop (scanner-depth s))
            ;; Starting a new top-level form
            (setf (scanner-form-start-line s) line-num
                  (scanner-form-start-text s) (string-trim '(#\Space #\Tab #\Newline) source-line)))
          (incf (scanner-depth s)))

         ;; Close paren
         ((eql ch #\))
          (decf (scanner-depth s))
          (when (minusp (scanner-depth s))
            (push (make-violation
                   :line line-num
                   :column col-num
                   :kind :extra-close
                   :depth (scanner-depth s)
                   :form-start-line (scanner-form-start-line s)
                   :form-start-text (scanner-form-start-text s)
                   :source-line (string-right-trim '(#\Newline #\Return) source-line))
                  (scanner-violations s))
            ;; Reset to 0 so we can continue scanning and find more issues
            (setf (scanner-depth s) 0))))))

    (setf (scanner-prev-char s) ch)
    s))

;;; ── Line-level scanner (handles ; comments) ───────────────────────

(defun scan-line (s line line-num)
  "Scan one source LINE through scanner S. Handles line comments."
  (loop with len = (length line)
        for col from 0 below len
        for ch = (char line col)
        do
    ;; If we're in normal mode (not string, not block comment, not char-lit, not escape),
    ;; and we see ;, the rest of the line is a comment.
    (when (and (eql ch #\;)
              (not (scanner-in-string s))
              (not (scanner-escape-next s))
              (not (scanner-char-lit s))
              (zerop (scanner-block-depth s)))
      (return))
    (scan-char s ch line-num (1+ col) line))
  ;; End of line: clear prev-char so cross-line #\ doesn't misfire
  ;; Actually #\ at end of line in CL is valid (space/newline char), but we
  ;; handle char-lit flag correctly — it consumes the NEXT char.
  s)

;;; ── File-level scanner ─────────────────────────────────────────────

(defun scan-file (filepath)
  "Scan FILEPATH for paren balance issues. Returns list of violations."
  (let ((s (make-scanner)))
    (with-open-file (stream filepath :direction :input :if-does-not-exist nil)
      (unless stream
        (return-from scan-file
          (list (make-violation :file (namestring filepath)
                               :kind :extra-close
                               :line 0
                               :source-line (format nil "Cannot open file: ~A" filepath)))))
      (loop for line-num from 1
            for line = (read-line stream nil nil)
            while line
            do (scan-line s line line-num)))
    ;; Check final depth
    (when (plusp (scanner-depth s))
      (push (make-violation
             :kind :unclosed
             :depth (scanner-depth s)
             :form-start-line (scanner-form-start-line s)
             :form-start-text (scanner-form-start-text s)
             :source-line (format nil "~D unclosed open paren(s) at EOF" (scanner-depth s)))
            (scanner-violations s)))
    ;; Set file on all violations
    (let ((file-str (namestring filepath)))
      (dolist (v (scanner-violations s))
        (setf (violation-file v) file-str)))
    (nreverse (scanner-violations s))))

;;; ── Collect project files ──────────────────────────────────────────

(defun collect-lisp-files (root)
  "Recursively collect .lisp files under ROOT, skipping vendored dirs.
Uses a simple shell find to avoid SBCL directory portability issues."
  (let* ((root-str (namestring (truename root)))
         (skip-dirs '(".tools" ".yore" ".yore-test" ".yore-audit" "quicklisp"
                      "node_modules" ".git" ".yarli" ".yarl" ".claude"
                      ".codex" ".cultivar" ".ralph" ".haake"))
         (prune-args (format nil "~{-name ~S -prune -o ~}" skip-dirs))
         (cmd (format nil "find ~A ~A -name '*.lisp' -print" root-str prune-args))
         (output (with-output-to-string (s)
                   (sb-ext:run-program "/bin/sh" (list "-c" cmd)
                                       :output s :error nil)))
         (files '()))
    (with-input-from-string (s output)
      (loop for line = (read-line s nil nil)
            while line
            when (plusp (length line))
            do (push (pathname line) files)))
    (sort files #'string< :key #'namestring)))

;;; ── Output formatting ─────────────────────────────────────────────

(defun format-violation (v &optional (stream *standard-output*))
  (ecase (violation-kind v)
    (:extra-close
     (format stream "  ~A:~D:~D  EXTRA )  depth=~D~%    ~A~%"
             (violation-file v)
             (violation-line v)
             (violation-column v)
             (violation-depth v)
             (violation-source-line v))
     (when (plusp (violation-form-start-line v))
       (format stream "    in form starting at line ~D: ~A~%"
               (violation-form-start-line v)
               (violation-form-start-text v))))
    (:unclosed
     (format stream "  ~A:EOF  UNCLOSED  ~D open paren(s) remaining~%"
             (violation-file v)
             (violation-depth v))
     (when (plusp (violation-form-start-line v))
       (format stream "    last form started at line ~D: ~A~%"
               (violation-form-start-line v)
               (violation-form-start-text v))))))

;;; ── Main ───────────────────────────────────────────────────────────

(defun main ()
  (let* ((args (rest sb-ext:*posix-argv*))  ; skip script name
         (project-mode (member "--project" args :test #'string-equal))
         (files (if project-mode
                    (collect-lisp-files (make-pathname :directory '(:relative ".")))
                    (mapcar #'pathname args)))
         (total-violations 0)
         (files-with-issues 0)
         (files-scanned 0))
    (when (null files)
      (format *error-output* "Usage: sbcl --script bin/lisp-paren-check.lisp [--project | file1.lisp ...]~%")
      (sb-ext:exit :code 2))
    (dolist (file files)
      (let ((violations (scan-file file)))
        (incf files-scanned)
        (when violations
          (incf files-with-issues)
          (incf total-violations (length violations))
          (format t "~%FAIL  ~A~%" (namestring file))
          (dolist (v violations)
            (format-violation v)))))
    (format t "~%──────────────────────────────────────~%")
    (format t "Scanned ~D files: ~D clean, ~D with issues (~D violations)~%"
            files-scanned
            (- files-scanned files-with-issues)
            files-with-issues
            total-violations)
    (sb-ext:exit :code (if (zerop total-violations) 0 1))))

(main)
