(in-package :amoebum)

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

(defun %normalized-read-window (offset limit)
  (let ((start (or offset 0))
        (max-lines limit))
    (when (< start 0)
      (error "OFFSET must be non-negative, got ~S." offset))
    (when (and max-lines (< max-lines 0))
      (error "LIMIT must be non-negative, got ~S." limit))
    (values start max-lines)))

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

(deftool read-file ((path pathname :description "Absolute path to read" :required t)
                    (offset (or null integer) :description "Line offset" :default nil)
                    (limit (or null integer) :description "Max lines" :default nil))
  "Read a file and return its contents with line numbers."
  (:permission :auto)
  (:dangerous nil)
  (:category :file-read)
  (:timeout 30)
  (%ensure-tool-path-allowed :read-file path)
  (%read-file-window path offset limit))

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
  (let ((current (uiop:read-file-string path :external-format :utf-8)))
    (multiple-value-bind (updated replacements)
        (%replace-all-literal current old-string new-string)
      (when (zerop replacements)
        (error "EDIT-FILE found no match for OLD-STRING in ~A." (%path-text path)))
      (%write-file-string path updated)
      (list :path (%path-text path)
            :replacements replacements))))
