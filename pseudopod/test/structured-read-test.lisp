(in-package :pseudopod/test)

;;; ---------------------------------------------------------------------------
;;; I104 Tests: Structured file read
;;; ---------------------------------------------------------------------------

(in-suite pseudopod-suite)

;;; ---- Helpers ----

(defun make-temp-file-i104 (content &optional (prefix "pseudopod-i104-"))
  "Write CONTENT to a temporary file and return its pathname."
  (let ((path (merge-pathnames
               (format nil "~A~D.txt" prefix (random 1000000))
               (uiop:temporary-directory))))
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string content stream))
    path))

(defmacro with-temp-file ((var content) &body body)
  "Create a temp file with CONTENT, bind its path to VAR, execute BODY, clean up."
  `(let ((,var (make-temp-file-i104 ,content)))
     (unwind-protect (progn ,@body)
       (when (uiop:file-exists-p ,var)
         (handler-case (delete-file ,var) (error () nil))))))

;;; ---- Result shape tests ----

(test structured-read-result-structure
  "structured-read-result has all expected fields."
  (with-temp-file (path "hello
world
")
    (let ((result (pseudopod:read-file-lines path)))
      (is-true (pseudopod:structured-read-result-p result))
      (is (stringp (pseudopod:structured-read-result-content result)))
      (is (stringp (pseudopod:structured-read-result-path result)))
      (is (integerp (pseudopod:structured-read-result-lines result)))
      (is (integerp (pseudopod:structured-read-result-bytes result)))
      ;; truncated is a boolean
      (is (or (eq t (pseudopod:structured-read-result-truncated result))
              (eq nil (pseudopod:structured-read-result-truncated result)))))))

;;; ---- Line range tests ----

(test read-file-lines-full
  "Reading without range returns all lines."
  (with-temp-file (path (format nil "line1~%line2~%line3~%"))
    (let ((r (pseudopod:read-file-lines path)))
      (is (= 3 (pseudopod:structured-read-result-lines r)))
      (is (string= (format nil "line1~%line2~%line3~%")
                    (pseudopod:structured-read-result-content r)))
      (is (eq nil (pseudopod:structured-read-result-truncated r))))))

(test read-file-lines-range
  "Reading a specific line range returns only those lines."
  (with-temp-file (path (format nil "a~%b~%c~%d~%e~%"))
    (let ((r (pseudopod:read-file-lines path :start-line 2 :end-line 4)))
      (is (= 3 (pseudopod:structured-read-result-lines r)))
      (is (string= (format nil "b~%c~%d~%")
                    (pseudopod:structured-read-result-content r))))))

(test read-file-lines-start-only
  "Omitting end-line reads to end of file."
  (with-temp-file (path (format nil "a~%b~%c~%"))
    (let ((r (pseudopod:read-file-lines path :start-line 2)))
      (is (= 2 (pseudopod:structured-read-result-lines r)))
      (is (string= (format nil "b~%c~%")
                    (pseudopod:structured-read-result-content r))))))

(test read-file-lines-end-only
  "Omitting start-line reads from line 1."
  (with-temp-file (path (format nil "a~%b~%c~%"))
    (let ((r (pseudopod:read-file-lines path :end-line 2)))
      (is (= 2 (pseudopod:structured-read-result-lines r)))
      (is (string= (format nil "a~%b~%")
                    (pseudopod:structured-read-result-content r))))))

(test read-file-lines-max-lines
  "max-lines truncates output and sets truncated flag."
  (with-temp-file (path (format nil "a~%b~%c~%d~%e~%"))
    (let ((r (pseudopod:read-file-lines path :max-lines 2)))
      (is (= 2 (pseudopod:structured-read-result-lines r)))
      (is (eq t (pseudopod:structured-read-result-truncated r)))
      (is (string= (format nil "a~%b~%")
                    (pseudopod:structured-read-result-content r))))))

(test read-file-lines-max-lines-with-range
  "max-lines caps a line range when more restrictive."
  (with-temp-file (path (format nil "a~%b~%c~%d~%e~%"))
    (let ((r (pseudopod:read-file-lines path :start-line 1 :end-line 5 :max-lines 2)))
      (is (= 2 (pseudopod:structured-read-result-lines r)))
      (is (eq t (pseudopod:structured-read-result-truncated r))))))

(test read-file-lines-past-eof
  "start-line past end of file returns empty content."
  (with-temp-file (path (format nil "a~%b~%"))
    (let ((r (pseudopod:read-file-lines path :start-line 100)))
      (is (= 0 (pseudopod:structured-read-result-lines r)))
      (is (string= "" (pseudopod:structured-read-result-content r))))))

(test read-file-lines-empty-file
  "Reading an empty file returns empty result."
  (with-temp-file (path "")
    (let ((r (pseudopod:read-file-lines path)))
      (is (= 0 (pseudopod:structured-read-result-lines r)))
      (is (= 0 (pseudopod:structured-read-result-bytes r)))
      (is (string= "" (pseudopod:structured-read-result-content r))))))

(test read-file-lines-unterminated
  "File without trailing newline still counts the last line."
  (with-temp-file (path "no-newline")
    (let ((r (pseudopod:read-file-lines path)))
      (is (= 1 (pseudopod:structured-read-result-lines r)))
      (is (string= "no-newline" (pseudopod:structured-read-result-content r))))))

;;; ---- Byte range tests ----

(test read-file-bytes-full
  "Reading without range returns all bytes."
  (with-temp-file (path "abcdef")
    (let ((r (pseudopod:read-file-bytes path)))
      (is (= 6 (pseudopod:structured-read-result-bytes r)))
      (is (string= "abcdef" (pseudopod:structured-read-result-content r)))
      (is (eq nil (pseudopod:structured-read-result-truncated r))))))

(test read-file-bytes-range
  "Reading specific byte range returns correct slice."
  (with-temp-file (path "abcdefghij")
    (let ((r (pseudopod:read-file-bytes path :start-byte 2 :end-byte 5)))
      ;; bytes 2,3,4,5 -> "cdef"
      (is (= 4 (pseudopod:structured-read-result-bytes r)))
      (is (string= "cdef" (pseudopod:structured-read-result-content r))))))

(test read-file-bytes-start-only
  "Omitting end-byte reads to end of file."
  (with-temp-file (path "abcdef")
    (let ((r (pseudopod:read-file-bytes path :start-byte 3)))
      (is (= 3 (pseudopod:structured-read-result-bytes r)))
      (is (string= "def" (pseudopod:structured-read-result-content r))))))

(test read-file-bytes-end-only
  "Omitting start-byte reads from byte 0."
  (with-temp-file (path "abcdef")
    (let ((r (pseudopod:read-file-bytes path :end-byte 2)))
      ;; bytes 0,1,2 -> "abc"
      (is (= 3 (pseudopod:structured-read-result-bytes r)))
      (is (string= "abc" (pseudopod:structured-read-result-content r))))))

(test read-file-bytes-max-bytes
  "max-bytes truncates and sets truncated flag."
  (with-temp-file (path "abcdefghij")
    (let ((r (pseudopod:read-file-bytes path :max-bytes 4)))
      (is (= 4 (pseudopod:structured-read-result-bytes r)))
      (is (eq t (pseudopod:structured-read-result-truncated r)))
      (is (string= "abcd" (pseudopod:structured-read-result-content r))))))

(test read-file-bytes-max-bytes-with-range
  "max-bytes caps a byte range when more restrictive."
  (with-temp-file (path "abcdefghij")
    (let ((r (pseudopod:read-file-bytes path :start-byte 1 :end-byte 8 :max-bytes 3)))
      (is (= 3 (pseudopod:structured-read-result-bytes r)))
      (is (eq t (pseudopod:structured-read-result-truncated r)))
      (is (string= "bcd" (pseudopod:structured-read-result-content r))))))

(test read-file-bytes-past-eof
  "start-byte past end of file returns empty content."
  (with-temp-file (path "abc")
    (let ((r (pseudopod:read-file-bytes path :start-byte 100)))
      (is (= 0 (pseudopod:structured-read-result-bytes r)))
      (is (string= "" (pseudopod:structured-read-result-content r))))))

(test read-file-bytes-empty-file
  "Reading an empty file returns empty result."
  (with-temp-file (path "")
    (let ((r (pseudopod:read-file-bytes path)))
      (is (= 0 (pseudopod:structured-read-result-bytes r)))
      (is (string= "" (pseudopod:structured-read-result-content r))))))

;;; ---- Bounded read tests ----

(test read-file-bounded-max-lines
  "Bounded read with max-lines only."
  (with-temp-file (path (format nil "a~%b~%c~%d~%"))
    (let ((r (pseudopod:read-file-bounded path :max-lines 2)))
      (is (= 2 (pseudopod:structured-read-result-lines r)))
      (is (eq t (pseudopod:structured-read-result-truncated r))))))

(test read-file-bounded-max-bytes
  "Bounded read with max-bytes only."
  (with-temp-file (path "abcdefghij")
    (let ((r (pseudopod:read-file-bounded path :max-bytes 5)))
      (is (= 5 (pseudopod:structured-read-result-bytes r)))
      (is (eq t (pseudopod:structured-read-result-truncated r)))
      (is (string= "abcde" (pseudopod:structured-read-result-content r))))))

(test read-file-bounded-both
  "Bounded read applies max-lines first, then max-bytes."
  (with-temp-file (path (format nil "aaaaaaaaaa~%bbbbbbbbbb~%cccccccccc~%"))
    ;; 3 lines of 11 chars each (10 + newline). max-lines=2 -> 22 chars.
    ;; then max-bytes=15 -> truncated to 15 chars.
    (let ((r (pseudopod:read-file-bounded path :max-lines 2 :max-bytes 15)))
      (is (= 15 (pseudopod:structured-read-result-bytes r)))
      (is (eq t (pseudopod:structured-read-result-truncated r))))))

(test read-file-bounded-no-truncation
  "Bounded read without truncation when limits exceed content."
  (with-temp-file (path "small")
    (let ((r (pseudopod:read-file-bounded path :max-lines 1000 :max-bytes 1000)))
      (is (= 5 (pseudopod:structured-read-result-bytes r)))
      (is (eq nil (pseudopod:structured-read-result-truncated r))))))

;;; ---- Error handling tests ----

(test read-file-lines-missing-file
  "Reading a nonexistent file signals pseudopod-error."
  (signals pseudopod:pseudopod-error
    (pseudopod:read-file-lines "/tmp/pseudopod-nonexistent-i104-test.txt")))

(test read-file-bytes-missing-file
  "Reading a nonexistent file signals pseudopod-error."
  (signals pseudopod:pseudopod-error
    (pseudopod:read-file-bytes "/tmp/pseudopod-nonexistent-i104-test.txt")))

(test read-file-bounded-missing-file
  "Reading a nonexistent file signals pseudopod-error."
  (signals pseudopod:pseudopod-error
    (pseudopod:read-file-bounded "/tmp/pseudopod-nonexistent-i104-test.txt")))
