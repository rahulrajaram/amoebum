(in-package :pseudopod)

;;; ---------------------------------------------------------------------------
;;; I104 - Structured file read
;;;
;;; Provides structured read support with line ranges, byte ranges, and
;;; bounded reads.  Returns a predictable result shape (structured-read-result).
;;; ---------------------------------------------------------------------------

;;; ---- Result structure ----

(defstruct structured-read-result
  "Predictable result shape for all file-read operations."
  (content ""   :type string)
  (path   ""   :type string)
  (lines   0   :type fixnum)
  (bytes   0   :type fixnum)
  (truncated nil :type boolean))

;;; ---- Internal helpers ----

(defun %read-file-string (path)
  "Read entire file at PATH as a string.  Signals pseudopod-error on failure."
  (handler-case
      (uiop:read-file-string path)
    (file-error (c)
      (error 'pseudopod-error
             :message (format nil "Cannot read file ~A: ~A" path c)
             :cause c))))

(defun %count-lines (string)
  "Return the number of newline-terminated lines in STRING.
   A final line without trailing newline still counts."
  (if (zerop (length string))
      0
      (let ((count 0))
        (loop for ch across string
              when (char= ch #\Newline)
                do (incf count))
        ;; If the last character is not a newline, there is an unterminated final line.
        (unless (char= (char string (1- (length string))) #\Newline)
          (incf count))
        count)))

(defun %split-lines (string)
  "Split STRING into a list of lines, preserving terminators."
  (let ((lines nil)
        (start 0)
        (len (length string)))
    (loop for i from 0 below len
          when (char= (char string i) #\Newline)
            do (push (subseq string start (1+ i)) lines)
               (setf start (1+ i)))
    ;; Remaining text after last newline (unterminated final line).
    (when (< start len)
      (push (subseq string start len) lines))
    (nreverse lines)))

;;; ---- Public API ----

(defun read-file-lines (path &key start-line end-line max-lines)
  "Read PATH and return lines in the range [START-LINE, END-LINE] (1-based, inclusive).
   MAX-LINES caps the number of lines returned.  Returns a FILE-READ-RESULT.

   Omitting START-LINE defaults to 1.  Omitting END-LINE defaults to the last line.
   If both END-LINE and MAX-LINES are given, MAX-LINES wins when it is more restrictive."
  (let* ((full-text (%read-file-string path))
         (all-lines (%split-lines full-text))
         (total (length all-lines))
         (start (max 1 (or start-line 1)))
         (end   (min total (or end-line total)))
         ;; Clamp invalid ranges.
         (start (min start (1+ total)))
         (end   (max end (1- start))))
    ;; Extract the 1-based inclusive range.
    (let* ((selected (if (> start total)
                         nil
                         (subseq all-lines (1- start) (min end total))))
           (truncated nil))
      ;; Apply max-lines cap.
      (when (and max-lines (plusp max-lines) (> (length selected) max-lines))
        (setf selected (subseq selected 0 max-lines))
        (setf truncated t))
      (let ((content (apply #'concatenate 'string (or selected '("")))))
        (make-structured-read-result
         :content content
         :path (namestring (truename path))
         :lines (length selected)
         :bytes (length (the string content))
         :truncated truncated)))))

(defun read-file-bytes (path &key start-byte end-byte max-bytes)
  "Read PATH and return bytes in the range [START-BYTE, END-BYTE] (0-based, inclusive end).
   MAX-BYTES caps the number of bytes returned.  Returns a FILE-READ-RESULT.

   Omitting START-BYTE defaults to 0.  Omitting END-BYTE defaults to end-of-file.
   If both END-BYTE and MAX-BYTES are given, MAX-BYTES wins when it is more restrictive."
  (let* ((full-text (%read-file-string path))
         (total (length full-text))
         (start (max 0 (or start-byte 0)))
         (end   (min total (if end-byte (1+ end-byte) total)))  ; end-byte is inclusive
         ;; Clamp.
         (start (min start total))
         (end   (max end start))
         (selected (subseq full-text start end))
         (truncated nil))
    ;; Apply max-bytes cap.
    (when (and max-bytes (plusp max-bytes) (> (length selected) max-bytes))
      (setf selected (subseq selected 0 max-bytes))
      (setf truncated t))
    (make-structured-read-result
     :content selected
     :path (namestring (truename path))
     :lines (%count-lines selected)
     :bytes (length selected)
     :truncated truncated)))

(defun read-file-bounded (path &key max-lines max-bytes)
  "Read PATH with bounded output.  Apply MAX-LINES first, then MAX-BYTES.
   Returns a FILE-READ-RESULT."
  (let* ((full-text (%read-file-string path))
         (all-lines (%split-lines full-text))
         (selected all-lines)
         (truncated nil))
    ;; Apply max-lines cap.
    (when (and max-lines (plusp max-lines) (> (length selected) max-lines))
      (setf selected (subseq selected 0 max-lines))
      (setf truncated t))
    (let ((content (apply #'concatenate 'string (or selected '("")))))
      ;; Apply max-bytes cap.
      (when (and max-bytes (plusp max-bytes) (> (length content) max-bytes))
        (setf content (subseq content 0 max-bytes))
        (setf truncated t))
      (make-structured-read-result
       :content content
       :path (namestring (truename path))
       :lines (%count-lines content)
       :bytes (length content)
       :truncated truncated))))
