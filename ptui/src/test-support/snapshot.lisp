(defpackage :ptui.test-support.snapshot
  (:use :cl)
  (:export #:buffer-to-snapshot
           #:snapshot-diff
           #:load-golden-file
           #:save-golden-file
           #:*update-snapshots-p*))

(in-package :ptui.test-support.snapshot)

(defvar *update-snapshots-p*
  (let ((env (uiop:getenv "PTUI_UPDATE_SNAPSHOTS")))
    (and env (string= env "1")))
  "When true, golden files are overwritten instead of compared.")

(defun buffer-to-snapshot (buffer)
  "Serialize a cell-buffer to a deterministic text snapshot.
Format: header line, then |line content| per row, optional attrs section."
  (let* ((cols (ptui.core.types:cell-buffer-cols buffer))
         (rows (ptui.core.types:cell-buffer-rows buffer))
         (cells (ptui.core.types:cell-buffer-cells buffer))
         (lines '())
         (attr-lines '()))
    (push (format nil "--- ptui-snapshot v1 ~Dx~D" cols rows) lines)
    (dotimes (row rows)
      (let ((chars (make-string cols :initial-element #\Space)))
        (dotimes (col cols)
          (let* ((index (+ col (* row cols)))
                 (cell (and (< index (length cells)) (svref cells index)))
                 (glyph (if cell
                            (ptui.core.types:cell-glyph cell)
                            " ")))
            (when (and (stringp glyph) (plusp (length glyph)))
              (setf (char chars col) (char glyph 0)))
            (when cell
              (let ((attrs (ptui.core.types:cell-attrs cell)))
                (when (and attrs
                           (or (ptui.core.types:attrs-boldp attrs)
                               (ptui.core.types:attrs-italicp attrs)
                               (ptui.core.types:attrs-underlinep attrs)
                               (ptui.core.types:attrs-invertp attrs)
                               (ptui.core.types:attrs-dimp attrs)
                               (ptui.core.types:attrs-strikep attrs)))
                  (push (format nil "~D,~D:~{~A~^,~}"
                                row col
                                (remove nil
                                        (list
                                         (when (ptui.core.types:attrs-boldp attrs) "bold")
                                         (when (ptui.core.types:attrs-italicp attrs) "italic")
                                         (when (ptui.core.types:attrs-underlinep attrs) "underline")
                                         (when (ptui.core.types:attrs-invertp attrs) "invert")
                                         (when (ptui.core.types:attrs-dimp attrs) "dim")
                                         (when (ptui.core.types:attrs-strikep attrs) "strike"))))
                        attr-lines))))))
        (push (format nil "|~A|" chars) lines)))
    (when attr-lines
      (push "--- attrs" lines)
      (dolist (line (nreverse attr-lines))
        (push line lines)))
    (format nil "~{~A~%~}" (nreverse lines))))

(defun snapshot-diff (expected actual)
  "Compare two snapshot strings line-by-line. Returns NIL if equal,
or a string describing the first difference."
  (let ((expected-lines (uiop:split-string expected :separator '(#\Newline)))
        (actual-lines (uiop:split-string actual :separator '(#\Newline))))
    ;; Remove trailing empty lines
    (setf expected-lines (reverse (member-if (lambda (s) (plusp (length s)))
                                             (reverse expected-lines))))
    (setf actual-lines (reverse (member-if (lambda (s) (plusp (length s)))
                                           (reverse actual-lines))))
    (let ((max-lines (max (length expected-lines) (length actual-lines))))
      (dotimes (i max-lines)
        (let ((exp-line (if (< i (length expected-lines))
                            (nth i expected-lines)
                            ""))
              (act-line (if (< i (length actual-lines))
                            (nth i actual-lines)
                            "")))
          (unless (string= exp-line act-line)
            (return-from snapshot-diff
              (format nil "Line ~D differs:~%  expected: ~S~%  actual:   ~S"
                      (1+ i) exp-line act-line))))))
    nil))

(defun load-golden-file (path)
  "Load golden snapshot from PATH. Returns NIL if file doesn't exist."
  (let ((resolved (probe-file path)))
    (when resolved
      (uiop:read-file-string resolved))))

(defun save-golden-file (path content)
  "Write snapshot CONTENT to golden file at PATH."
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string content stream))
  path)
