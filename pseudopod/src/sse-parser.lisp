(in-package :pseudopod)

(defun %sse-trim-line (line)
  "Trim trailing whitespace from SSE LINE."
  (if (and (stringp line) (plusp (length line)))
      (string-right-trim '(#\Space #\Tab #\Return #\Newline) line)
      ""))

(defun %sse-line-data-payload (line &key (prefix "data: ") (prefix-bare "data:"))
  "Extract data payload from SSE LINE, or NIL if not a data line."
  (let ((trimmed (%sse-trim-line line)))
    (cond
      ((and (>= (length trimmed) (length prefix))
            (string= prefix trimmed :end2 (length prefix)))
       (subseq trimmed (length prefix)))
      ((and (>= (length trimmed) (length prefix-bare))
            (string= prefix-bare trimmed :end2 (length prefix-bare)))
       (string-left-trim '(#\Space #\Tab)
                         (subseq trimmed (length prefix-bare))))
      (t nil))))

(defun %sse-done-payload-p (payload)
  "Return T if PAYLOAD is the SSE termination marker."
  (and (stringp payload)
       (string= (string-right-trim '(#\Space #\Tab #\Return) payload) "[DONE]")))

(defun parse-sse-data-lines (body-stream &key on-payload on-done)
  "Read SSE data lines from BODY-STREAM, calling ON-PAYLOAD for each data payload.
Calls ON-DONE (if provided) when [DONE] encountered. Returns number of payloads processed."
  (let ((count 0))
    (loop for line = (read-line body-stream nil nil)
          while line do
            (let ((payload (%sse-line-data-payload line)))
              (when payload
                (let ((trimmed (string-right-trim '(#\Space #\Tab #\Return) payload)))
                  (cond
                    ((%sse-done-payload-p trimmed)
                     (when on-done (funcall on-done))
                     (return))
                    ((plusp (length trimmed))
                     (when on-payload
                       (funcall on-payload trimmed))
                     (incf count)))))))
    count))
