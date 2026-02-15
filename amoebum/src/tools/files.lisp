(in-package :amoebum)

(defparameter *read-file-default-tabular-rows* 10)

(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defparameter *pdf-text-extractor* nil)
(defparameter *file-read-snapshots* (make-hash-table :test #'equal))
(defparameter *edit-file-syntax-validator-runner* nil)

(defun %path-text (path)
  (typecase path
    (pathname (namestring path))
    (string path)
    (t (prin1-to-string path))))

(defun %ensure-tool-path-allowed (tool path)
  (let ((decision (check-permission :tool tool :path path)))
    (unless (eq decision :allow)
      (error "Permission decision ~S for tool ~A on ~A."
             decision
             tool
             (%path-text path)))))

(defun %canonical-path-key (path)
  (%path-text (or (ignore-errors (truename path))
                  (probe-file path)
                  path)))

(defun %file-size-bytes (path)
  (handler-case
      (with-open-file (stream path
                              :direction :input
                              :element-type '(unsigned-byte 8))
        (file-length stream))
    (error ()
      nil)))

(defun %capture-file-snapshot (path)
  (let ((existing (probe-file path)))
    (when existing
      (list :write-date (ignore-errors (file-write-date existing))
            :size-bytes (%file-size-bytes existing)))))

(defun %record-file-read-state (path)
  (let ((key (%canonical-path-key path)))
    (setf (gethash key *file-read-snapshots*)
          (list :path key
                :snapshot (%capture-file-snapshot path)
                :recorded-at (get-universal-time)))
    key))

(defun %file-read-state (path)
  (gethash (%canonical-path-key path) *file-read-snapshots*))

(defun %ensure-file-read-before-edit (path)
  (or (%file-read-state path)
      (error 'tool-argument-error
             :tool-name "edit-file"
             :argument-name "path"
             :message (format nil "File ~A must be read with read-file before edit-file."
                              (%path-text path))
             :reason "read-file is required before edit-file for file safety.")))

(defun %file-snapshot-equal-p (left right)
  (and (eql (getf left :write-date) (getf right :write-date))
       (eql (getf left :size-bytes) (getf right :size-bytes))))

(defun %edit-conflict-warning (path)
  (format nil "File ~A changed on disk since the last read in this session."
          (%path-text path)))

(defun %normalized-read-window (offset limit)
  (let ((start (or offset 0))
        (max-lines limit))
    (when (< start 0)
      (error "OFFSET must be non-negative, got ~S." offset))
    (when (and max-lines (< max-lines 0))
      (error "LIMIT must be non-negative, got ~S." limit))
    (values start max-lines)))

(defun %files-trim-whitespace (text)
  (string-trim '(#\Space #\Tab #\Newline #\Return) (or text "")))

(defun %path-extension (path)
  (let ((type (pathname-type path)))
    (and type (string-downcase (princ-to-string type)))))

(defun %pdf-file-p (path)
  (string= (or (%path-extension path) "") "pdf"))

(defun %image-mime-type (path)
  (let ((extension (%path-extension path)))
    (cond
      ((string= extension "png") "image/png")
      ((or (string= extension "jpg")
           (string= extension "jpeg"))
       "image/jpeg")
      ((string= extension "gif") "image/gif")
      ((string= extension "svg") "image/svg+xml")
      (t nil))))

(defun %notebook-file-p (path)
  (string= (or (%path-extension path) "") "ipynb"))

(defun %tabular-delimiter (path)
  (let ((extension (%path-extension path)))
    (cond
      ((string= extension "csv") #\,)
      ((string= extension "tsv") #\Tab)
      (t nil))))

(defun %read-lines (path)
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun %format-lines-with-numbers (lines start-index limit)
  (with-output-to-string (stream)
    (let ((written 0)
          (line-index 0))
      (dolist (line lines)
        (when (and (>= line-index start-index)
                   (or (null limit) (< written limit)))
          (format stream "~D~C~A~%" (1+ line-index) #\Tab line)
          (incf written))
        (incf line-index)
        (when (and limit (>= written limit))
          (return))))))

(defun %read-file-window (path offset limit)
  (multiple-value-bind (start max-lines)
      (%normalized-read-window offset limit)
    (%format-lines-with-numbers (%read-lines path) start max-lines)))

(defun %parse-positive-integer (text label)
  (let ((trimmed (%files-trim-whitespace text)))
    (when (zerop (length trimmed))
      (error "~A must not be empty." label))
    (let ((value (handler-case
                     (parse-integer trimmed)
                   (error ()
                     (error "~A must be an integer, got ~S." label text)))))
      (when (< value 1)
        (error "~A must be >= 1, got ~S." label value))
      value)))

(defun %parse-pdf-page-range (pages)
  (let ((range (%files-trim-whitespace pages)))
    (if (zerop (length range))
        (values nil nil)
        (let ((dash-position (position #\- range)))
          (if dash-position
              (let* ((start-text (subseq range 0 dash-position))
                     (end-text (subseq range (1+ dash-position)))
                     (start (%parse-positive-integer start-text "pages start"))
                     (end (%parse-positive-integer end-text "pages end")))
                (when (> start end)
                  (error "Invalid pages range ~S: start must be <= end." pages))
                (values start end))
              (let ((page (%parse-positive-integer range "page")))
                (values page page)))))))

(defun %run-pdftotext (path pages)
  (multiple-value-bind (start-page end-page)
      (%parse-pdf-page-range pages)
    (let ((arguments (append (list "pdftotext" "-enc" "UTF-8")
                             (when start-page
                               (list "-f" (write-to-string start-page)))
                             (when end-page
                               (list "-l" (write-to-string end-page)))
                             (list (%path-text path) "-"))))
      (handler-case
          (multiple-value-bind (stdout stderr exit-code)
              (uiop:run-program arguments
                                :ignore-error-status t
                                :output :string
                                :error-output :string)
            (unless (zerop (or exit-code 0))
              (error "pdftotext failed for ~A: ~A"
                     (%path-text path)
                     (%files-trim-whitespace
                      (if (plusp (length (or stderr "")))
                          stderr
                          stdout))))
            (or stdout ""))
        (error (condition)
          (error "Unable to extract PDF text for ~A. Ensure pdftotext is installed. Cause: ~A"
                 (%path-text path)
                 condition))))))

(unless *pdf-text-extractor*
  (setf *pdf-text-extractor* #'%run-pdftotext))

(defun %read-pdf-file (path pages)
  (funcall (or *pdf-text-extractor* #'%run-pdftotext) path pages))

(defun %read-binary-octets (path)
  (with-open-file (stream path
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (let* ((size (file-length stream))
           (octets (make-array size :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      octets)))

(defun %base64-encode-octets (octets)
  (let* ((length* (length octets))
         (full-groups (* 3 (floor length* 3)))
         (remainder (mod length* 3)))
    (with-output-to-string (stream)
      (loop for index from 0 below full-groups by 3 do
            (let* ((b1 (aref octets index))
                   (b2 (aref octets (1+ index)))
                   (b3 (aref octets (+ index 2)))
                   (triple (logior (ash b1 16)
                                   (ash b2 8)
                                   b3)))
              (write-char (char +base64-alphabet+ (ldb (byte 6 18) triple)) stream)
              (write-char (char +base64-alphabet+ (ldb (byte 6 12) triple)) stream)
              (write-char (char +base64-alphabet+ (ldb (byte 6 6) triple)) stream)
              (write-char (char +base64-alphabet+ (ldb (byte 6 0) triple)) stream)))
      (when (= remainder 1)
        (let* ((b1 (aref octets full-groups))
               (triple (ash b1 16)))
          (write-char (char +base64-alphabet+ (ldb (byte 6 18) triple)) stream)
          (write-char (char +base64-alphabet+ (ldb (byte 6 12) triple)) stream)
          (write-char #\= stream)
          (write-char #\= stream)))
      (when (= remainder 2)
        (let* ((b1 (aref octets full-groups))
               (b2 (aref octets (1+ full-groups)))
               (triple (logior (ash b1 16)
                               (ash b2 8))))
          (write-char (char +base64-alphabet+ (ldb (byte 6 18) triple)) stream)
          (write-char (char +base64-alphabet+ (ldb (byte 6 12) triple)) stream)
          (write-char (char +base64-alphabet+ (ldb (byte 6 6) triple)) stream)
          (write-char #\= stream))))))

(defun %read-image-file (path)
  (let ((mime-type (%image-mime-type path)))
    (unless mime-type
      (error "Unsupported image type for ~A." (%path-text path)))
    (let* ((octets (%read-binary-octets path))
           (base64 (%base64-encode-octets octets)))
      (list :kind :image
            :path (%path-text path)
            :mime-type mime-type
            :encoding "base64"
            :bytes (length octets)
            :data base64))))

(defun %parse-json-hash-table (json-text)
  (let* ((jonathan-package (find-package :jonathan))
         (parse-symbol (and jonathan-package
                            (find-symbol "PARSE" jonathan-package))))
    (unless (and parse-symbol (fboundp parse-symbol))
      (error "Notebook parsing requires the JONATHAN package."))
    (funcall (symbol-function parse-symbol) json-text :as :hash-table)))

(defun %json-object-value (object key &optional default)
  (if (hash-table-p object)
      (multiple-value-bind (value present-p)
          (gethash key object)
        (if present-p
            value
            default))
      default))

(defun %sequence-elements (value)
  (cond
    ((null value) nil)
    ((stringp value) (list value))
    ((typep value 'sequence)
     (coerce value 'list))
    (t nil)))

(defun %join-text-fragments (value)
  (with-output-to-string (stream)
    (dolist (fragment (%sequence-elements value))
      (write-string (if (stringp fragment)
                        fragment
                        (princ-to-string fragment))
                    stream))))

(defun %notebook-language (notebook)
  (let* ((metadata (%json-object-value notebook "metadata"))
         (language-info (and (hash-table-p metadata)
                             (%json-object-value metadata "language_info")))
         (name (and (hash-table-p language-info)
                    (%json-object-value language-info "name"))))
    (if (and (stringp name)
             (plusp (length (%files-trim-whitespace name))))
        name
        "")))

(defun %notebook-output-text (output)
  (let* ((text (%join-text-fragments (%json-object-value output "text")))
         (trimmed (%files-trim-whitespace text)))
    (if (plusp (length trimmed))
        text
        (let* ((data (%json-object-value output "data"))
               (plain (and (hash-table-p data)
                           (%join-text-fragments (%json-object-value data "text/plain"))))
               (plain-trimmed (%files-trim-whitespace plain)))
          (if (plusp (length plain-trimmed))
              plain
              (let ((traceback (%join-text-fragments (%json-object-value output "traceback"))))
                (if (plusp (length (%files-trim-whitespace traceback)))
                    traceback
                    "")))))))

(defun %render-notebook-cell (stream cell index language)
  (let* ((cell-type (or (%json-object-value cell "cell_type") "unknown"))
         (source (%join-text-fragments (%json-object-value cell "source")))
         (outputs (%sequence-elements (%json-object-value cell "outputs"))))
    (format stream "## Cell ~D (~A)~2%" index cell-type)
    (if (string= cell-type "markdown")
        (format stream "~A~2%" source)
        (progn
          (format stream "```~A~%~A~%```~2%" language source)
          (dolist (output outputs)
            (let ((output-text (%notebook-output-text output)))
              (when (plusp (length (%files-trim-whitespace output-text)))
                (format stream "Output:~%```text~%~A~%```~2%"
                        output-text))))))))

(defun %read-notebook-file (path)
  (let* ((payload (uiop:read-file-string path :external-format :utf-8))
         (notebook (%parse-json-hash-table payload))
         (cells (%sequence-elements (%json-object-value notebook "cells")))
         (language (%notebook-language notebook)))
    (with-output-to-string (stream)
      (format stream "# Notebook: ~A~2%" (file-namestring path))
      (if cells
          (loop for cell in cells
                for index from 1 do
                  (%render-notebook-cell stream cell index language))
          (format stream "_No cells found._~%")))))

(defun %parse-delimited-line (line delimiter)
  (let ((fields '())
        (buffer (make-string-output-stream))
        (in-quotes nil)
        (length* (length line))
        (index 0))
    (labels ((emit-field ()
               (push (get-output-stream-string buffer) fields)
               (setf buffer (make-string-output-stream))))
      (loop while (< index length*) do
            (let ((char (char line index)))
              (cond
                ((char= char #\")
                 (if in-quotes
                     (if (and (< (1+ index) length*)
                              (char= (char line (1+ index)) #\"))
                         (progn
                           (write-char #\" buffer)
                           (incf index))
                         (setf in-quotes nil))
                     (setf in-quotes t)))
                ((and (not in-quotes)
                      (char= char delimiter))
                 (emit-field))
                (t
                 (write-char char buffer))))
            (incf index))
      (emit-field))
    (nreverse fields)))

(defun %pad-row (row target-size)
  (if (>= (length row) target-size)
      row
      (append row (make-list (- target-size (length row)) :initial-element ""))))

(defun %column-widths (rows)
  (let ((column-count (if rows
                          (loop for row in rows maximize (length row))
                          0)))
    (loop for column from 0 below column-count
          collect (max 1 (loop for row in rows
                               maximize (length (or (nth column row) "")))))))

(defun %render-aligned-row (row widths)
  (with-output-to-string (stream)
    (loop for value in row
          for width in widths
          for index from 0 do
            (when (> index 0)
              (write-string " | " stream))
            (let* ((text (or value ""))
                   (padding (max 0 (- width (length text)))))
              (write-string text stream)
              (dotimes (_ padding)
                (declare (ignore _))
                (write-char #\Space stream))))))

(defun %render-aligned-separator (widths)
  (with-output-to-string (stream)
    (loop for width in widths
          for index from 0 do
            (when (> index 0)
              (write-string "-+-" stream))
            (dotimes (_ width)
              (declare (ignore _))
              (write-char #\- stream)))))

(defun %read-tabular-preview (path delimiter limit)
  (let ((row-limit (or limit *read-file-default-tabular-rows*)))
    (when (< row-limit 0)
      (error "LIMIT must be non-negative, got ~S." limit))
    (if (zerop row-limit)
        ""
        (let ((rows '()))
          (with-open-file (stream path :direction :input :external-format :utf-8)
            (loop for line = (read-line stream nil nil)
                  while line
                  while (< (length rows) row-limit) do
                    (push (%parse-delimited-line line delimiter) rows)))
          (setf rows (nreverse rows))
          (if (null rows)
              ""
              (let* ((column-count (loop for row in rows maximize (length row)))
                     (rows* (mapcar (lambda (row) (%pad-row row column-count)) rows))
                     (widths (%column-widths rows*))
                     (header (first rows*))
                     (body (rest rows*)))
                (with-output-to-string (stream)
                  (write-string (%render-aligned-row header widths) stream)
                  (terpri stream)
                  (write-string (%render-aligned-separator widths) stream)
                  (terpri stream)
                  (dolist (row body)
                    (write-string (%render-aligned-row row widths) stream)
                    (terpri stream)))))))))

(defun %read-file-content (path offset limit pages)
  (let ((delimiter (%tabular-delimiter path)))
    (when (and pages (not (%pdf-file-p path)))
      (error "PAGES is supported only for PDF files, got ~A." (%path-text path)))
    (cond
      ((%pdf-file-p path)
       (%read-pdf-file path pages))
      ((%image-mime-type path)
       (%read-image-file path))
      ((%notebook-file-p path)
       (%read-notebook-file path))
      (delimiter
       (%read-tabular-preview path delimiter limit))
      (t
       (%read-file-window path offset limit)))))

(defun %write-file-string (path content)
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string content stream)))

(defun %replace-all-literal (source old-string new-string)
  (let ((needle-length (length old-string)))
    (when (zerop needle-length)
      (error "OLD-STRING must not be empty."))
    (let ((cursor 0)
          (replacements 0))
      (values
       (with-output-to-string (stream)
         (loop for position = (search old-string source :start2 cursor)
               while position do
                 (write-string source stream :start cursor :end position)
                 (write-string new-string stream)
                 (setf cursor (+ position needle-length))
                 (incf replacements)
               finally (write-string source stream :start cursor)))
       replacements))))

(defun %default-edit-file-syntax-validator-runner (command)
  (uiop:run-program command
                    :ignore-error-status t
                    :output :string
                    :error-output :string))

(unless *edit-file-syntax-validator-runner*
  (setf *edit-file-syntax-validator-runner*
        #'%default-edit-file-syntax-validator-runner))

(defun %normalize-validator-key (key)
  (let* ((text (string-downcase
                (%files-trim-whitespace
                 (typecase key
                   (string key)
                   (symbol (symbol-name key))
                   (t (princ-to-string key)))))))
    (if (and (plusp (length text))
             (char= (char text 0) #\.))
        (subseq text 1)
        text)))

(defun %validator-spec-for-extension (validators extension)
  (cond
    ((hash-table-p validators)
     (loop for key being the hash-keys of validators
           using (hash-value value)
           when (string= (%normalize-validator-key key) extension)
             do (return value)))
    ((listp validators)
     (loop for entry in validators
           when (and (consp entry)
                     (string= (%normalize-validator-key (car entry)) extension))
             do (return (cdr entry))))
    (t
     nil)))

(defun %syntax-validator-spec-for-path (path)
  (let* ((extension (%path-extension path))
         (cfg (ignore-errors (current-config)))
         (validators (and cfg (config-value :edit-file-syntax-validators cfg))))
    (when (and extension validators)
      (%validator-spec-for-extension validators extension))))

(defun %replace-path-placeholder (text path)
  (nth-value 0 (%replace-all-literal text "{path}" (%path-text path))))

(defun %syntax-validator-command (spec path)
  (cond
    ((stringp spec)
     (list "sh" "-lc" (%replace-path-placeholder spec path)))
    ((and (listp spec) spec)
     (loop for token in spec
           collect (%replace-path-placeholder
                    (if (stringp token)
                        token
                        (princ-to-string token))
                    path)))
    (t
     nil)))

(defun %syntax-validator-output (stdout stderr)
  (let ((trimmed-stdout (%files-trim-whitespace stdout))
        (trimmed-stderr (%files-trim-whitespace stderr)))
    (cond
      ((and (plusp (length trimmed-stderr))
            (plusp (length trimmed-stdout)))
       (format nil "~A~%~A" trimmed-stderr trimmed-stdout))
      ((plusp (length trimmed-stderr))
       trimmed-stderr)
      (t
       trimmed-stdout))))

(defun %maybe-run-post-edit-syntax-validation (path)
  (let ((validator-spec (%syntax-validator-spec-for-path path)))
    (if (null validator-spec)
        (values nil nil)
        (let ((command (%syntax-validator-command validator-spec path)))
          (if (null command)
              (values (list :enabled t
                            :ok nil
                            :reason "invalid syntax validator command specification")
                      (format nil "Syntax validation configuration for ~A is invalid."
                              (%path-text path)))
              (handler-case
                  (multiple-value-bind (stdout stderr exit-code)
                      (funcall (or *edit-file-syntax-validator-runner*
                                   #'%default-edit-file-syntax-validator-runner)
                               command)
                    (let* ((status (or exit-code 0))
                           (ok (zerop status))
                           (output (%syntax-validator-output stdout stderr))
                           (result (list :enabled t
                                         :ok ok
                                         :exit-code status
                                         :command command
                                         :output output)))
                      (if ok
                          (values result nil)
                          (values result
                                  (format nil "Syntax validation failed for ~A (exit ~D)."
                                          (%path-text path)
                                          status)))))
                (error (condition)
                  (values (list :enabled t
                                :ok nil
                                :command command
                                :error (princ-to-string condition))
                          (format nil "Syntax validation failed for ~A: ~A"
                                  (%path-text path)
                                  (princ-to-string condition))))))))))

(deftool read-file ((path pathname :description "Absolute path to read" :required t)
                    (offset (or null integer) :description "Line offset" :default nil)
                    (limit (or null integer) :description "Max lines" :default nil)
                    (pages (or null string)
                     :description "PDF page range (for .pdf), e.g. 1-5"
                     :default nil))
  "Read file content with type-aware handling for text, PDF, images, notebooks, and CSV/TSV."
  (:permission :auto)
  (:dangerous nil)
  (:category :file-read)
  (:timeout 30)
  (%ensure-tool-path-allowed :read-file path)
  (let ((content (%read-file-content path offset limit pages)))
    (%record-file-read-state path)
    content))

(deftool write-file ((path pathname :description "Absolute path to write" :required t)
                     (content string :description "Complete file content" :required t))
  "Create or overwrite a file with the provided content."
  (:permission :auto)
  (:dangerous nil)
  (:category :file-write)
  (:timeout 30)
  (%ensure-tool-path-allowed :write-file path)
  (%write-file-string path content)
  (list :path (%path-text path)
        :bytes (length content)
        :written t))

(deftool edit-file ((path pathname :description "Absolute path to edit" :required t)
                    (old-string string :description "String to replace" :required t)
                    (new-string string :description "Replacement string" :required t))
  "Edit an existing file using exact string replacement."
  (:permission :auto)
  (:dangerous nil)
  (:category :file-edit)
  (:timeout 30)
  (%ensure-tool-path-allowed :edit-file path)
  (let* ((read-state (%ensure-file-read-before-edit path))
         (snapshot-at-read (getf read-state :snapshot))
         (current-snapshot (%capture-file-snapshot path))
         (conflict-detected (not (%file-snapshot-equal-p snapshot-at-read current-snapshot)))
         (warnings '()))
    (when conflict-detected
      (push (%edit-conflict-warning path) warnings))
    (let ((current (uiop:read-file-string path :external-format :utf-8)))
    (multiple-value-bind (updated replacements)
        (%replace-all-literal current old-string new-string)
      (when (zerop replacements)
        (error "EDIT-FILE found no match for OLD-STRING in ~A." (%path-text path)))
      (%write-file-string path updated)
      (%record-file-read-state path)
      (multiple-value-bind (syntax-validation syntax-warning)
          (%maybe-run-post-edit-syntax-validation path)
        (when syntax-warning
          (push syntax-warning warnings))
        (let ((result (list :path (%path-text path)
                            :replacements replacements
                            :conflict-detected conflict-detected)))
          (when warnings
            (setf result (append result (list :warnings (nreverse warnings)))))
          (when syntax-validation
            (setf result (append result (list :syntax-validation syntax-validation))))
          result))))))
