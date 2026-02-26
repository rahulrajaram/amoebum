(in-package :amoebum)

;;; ============================================================
;;; I266: Conversation Export for Human Review
;;;
;;; Serialize conversations to markdown (human-readable)
;;; or JSON (machine-readable) format.
;;; ============================================================

;;; --- Export directory ---

(defparameter *conversation-export-directory* nil
  "Override directory for conversation exports. NIL uses default.")

(defun %export-default-directory ()
  (let ((dir (merge-pathnames ".amoebum/exports/"
                              (user-homedir-pathname))))
    (ensure-directories-exist dir)
    dir))

(defun %export-directory ()
  (or *conversation-export-directory* (%export-default-directory)))

;;; --- Export filename ---

(defun %export-filename (project-name format-type)
  "Generate an export filename."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (let ((ext (ecase format-type (:markdown "md") (:json "json"))))
      (format nil "~A-~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0D.~A"
              (or project-name "conversation")
              year month day hour min sec ext))))

;;; --- Markdown export ---

(defun %export-entry-to-markdown (entry stream)
  "Write a single conversation entry as markdown."
  (let ((role (conversation-history-entry-role entry))
        (content (conversation-history-entry-content entry))
        (tool-call-id (conversation-history-entry-tool-call-id entry))
        (name (conversation-history-entry-name entry)))
    (cond
      ;; User message -> blockquote
      ((string-equal role "user")
       (format stream "~%> **User:**~%")
       (dolist (line (uiop:split-string content :separator '(#\Newline)))
         (format stream "> ~A~%" line))
       (terpri stream))
      ;; Tool call -> fenced code block
      ((and name (string-equal role "assistant"))
       (format stream "~%**Tool Call: `~A`**~%" name)
       (format stream "```~%~A~%```~%~%" content))
      ;; Tool result -> collapsible details
      ((string-equal role "tool")
       (format stream "~%<details>~%<summary>Tool Result")
       (when tool-call-id
         (format stream " (~A)" tool-call-id))
       (format stream "</summary>~%~%```~%~A~%```~%</details>~%~%" content))
      ;; Assistant message -> plain text
      ((string-equal role "assistant")
       (format stream "~%**Assistant:**~%~%~A~%~%" content))
      ;; System message
      ((string-equal role "system")
       (format stream "~%*System:* ~A~%~%" content))
      ;; Other
      (t
       (format stream "~%**~A:** ~A~%~%" role content)))))

(defun export-conversation-markdown (conversation &key (stream nil) (project-name nil))
  "Export a conversation-state to markdown.
   If STREAM is nil, returns a string."
  (let ((entries (conversation-state-entries conversation))
        (session-id (conversation-state-session-id conversation)))
    (flet ((write-md (out)
             (format out "# Conversation Export~%~%")
             (format out "- **Session:** ~A~%" session-id)
             (when project-name
               (format out "- **Project:** ~A~%" project-name))
             (format out "- **Entries:** ~D~%" (length entries))
             (format out "- **Exported:** ~A~%~%"
                     (%format-export-timestamp (get-universal-time)))
             (format out "---~%")
             (dolist (entry entries)
               (%export-entry-to-markdown entry out))
             (format out "~%---~%*Exported by amoebum*~%")))
      (if stream
          (progn (write-md stream) nil)
          (with-output-to-string (s) (write-md s))))))

;;; --- JSON export ---

(defun %export-entry-to-json (entry stream &optional last-p)
  "Write a single conversation entry as a JSON object."
  (format stream "  {\"role\":\"~A\",\"content\":\"~A\",\"timestamp\":~D"
          (%journal-escape-string (conversation-history-entry-role entry))
          (%journal-escape-string (conversation-history-entry-content entry))
          (conversation-history-entry-timestamp entry))
  (when (conversation-history-entry-name entry)
    (format stream ",\"name\":\"~A\""
            (%journal-escape-string (conversation-history-entry-name entry))))
  (when (conversation-history-entry-tool-call-id entry)
    (format stream ",\"tool_call_id\":\"~A\""
            (%journal-escape-string
             (conversation-history-entry-tool-call-id entry))))
  (write-char #\} stream)
  (unless last-p
    (write-char #\, stream))
  (terpri stream))

(defun export-conversation-json (conversation &key (stream nil) (project-name nil))
  "Export a conversation-state to JSON.
   If STREAM is nil, returns a string."
  (let ((entries (conversation-state-entries conversation))
        (session-id (conversation-state-session-id conversation)))
    (flet ((write-json (out)
             (format out "{~%")
             (format out "  \"session_id\": \"~A\",~%"
                     (%journal-escape-string session-id))
             (when project-name
               (format out "  \"project\": \"~A\",~%"
                       (%journal-escape-string project-name)))
             (format out "  \"entry_count\": ~D,~%"
                     (length entries))
             (format out "  \"exported_at\": ~D,~%"
                     (get-universal-time))
             (format out "  \"messages\": [~%")
             (loop for (entry . rest) on entries
                   do (%export-entry-to-json entry out (null rest)))
             (format out "  ]~%")
             (format out "}~%")))
      (if stream
          (progn (write-json stream) nil)
          (with-output-to-string (s) (write-json s))))))

;;; --- Timestamp formatting ---

(defun %format-export-timestamp (universal-time)
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour min sec)))

;;; --- Unified export API ---

(defun export-conversation (conversation &key
                                           (format-type :markdown)
                                           (project-name nil)
                                           (output-path nil))
  "Export a conversation to file.
   FORMAT-TYPE is :markdown or :json.
   Returns the export file path."
  (let* ((fname (%export-filename project-name format-type))
         (path (or output-path
                   (merge-pathnames fname (%export-directory)))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create
                              :external-format :utf-8)
      (ecase format-type
        (:markdown (export-conversation-markdown conversation
                                                 :stream out
                                                 :project-name project-name))
        (:json (export-conversation-json conversation
                                         :stream out
                                         :project-name project-name))))
    path))
