(in-package :amoebum/test)

;;; ============================================================
;;; I266: Conversation Export — Smoke Tests
;;; ============================================================

(def-suite conversation-export-suite :in amoebum-suite)
(in-suite conversation-export-suite)

;;; --- API functions ---

(test export-api-functions-exist
  "Conversation export API functions are bound."
  (is (fboundp 'amoebum.sessions:export-conversation-markdown))
  (is (fboundp 'amoebum.sessions:export-conversation-json))
  (is (fboundp 'amoebum.sessions:export-conversation)))

;;; --- Helper: build a test conversation ---

(defun %make-test-conversation ()
  "Create a conversation-state with a few entries for testing."
  (amoebum::%make-conversation-state
   :session-id "test-session-001"
   :entries (list
             (amoebum.sessions:make-conversation-history-entry
              :role "user"
              :content "Hello, can you help me?"
              :timestamp 100)
             (amoebum.sessions:make-conversation-history-entry
              :role "assistant"
              :content "Of course! What do you need?"
              :timestamp 101)
             (amoebum.sessions:make-conversation-history-entry
              :role "assistant"
              :content "(list-files \"/home\")"
              :name "list-files"
              :timestamp 102)
             (amoebum.sessions:make-conversation-history-entry
              :role "tool"
              :content "file1.txt\nfile2.txt"
              :tool-call-id "tc-001"
              :timestamp 103)
             (amoebum.sessions:make-conversation-history-entry
              :role "assistant"
              :content "I found 2 files."
              :timestamp 104))))

;;; --- Markdown export ---

(test export-markdown-returns-string
  "export-conversation-markdown returns a string."
  (let* ((conv (%make-test-conversation))
         (md (amoebum.sessions:export-conversation-markdown conv)))
    (is (stringp md))
    (is (plusp (length md)))))

(test export-markdown-contains-header
  "Markdown export contains conversation header."
  (let* ((conv (%make-test-conversation))
         (md (amoebum.sessions:export-conversation-markdown conv)))
    (is (search "Conversation Export" md))
    (is (search "test-session-001" md))
    (is (search "Entries" md))))

(test export-markdown-user-blockquoted
  "User messages are blockquoted in markdown."
  (let* ((conv (%make-test-conversation))
         (md (amoebum.sessions:export-conversation-markdown conv)))
    (is (search "> **User:**" md))
    (is (search "Hello, can you help me?" md))))

(test export-markdown-tool-call-fenced
  "Tool calls are in fenced code blocks."
  (let* ((conv (%make-test-conversation))
         (md (amoebum.sessions:export-conversation-markdown conv)))
    (is (search "Tool Call: `list-files`" md))
    (is (search "```" md))))

(test export-markdown-tool-result-details
  "Tool results are in collapsible details."
  (let* ((conv (%make-test-conversation))
         (md (amoebum.sessions:export-conversation-markdown conv)))
    (is (search "<details>" md))
    (is (search "Tool Result" md))
    (is (search "tc-001" md))))

;;; --- JSON export ---

(test export-json-returns-string
  "export-conversation-json returns a string."
  (let* ((conv (%make-test-conversation))
         (json (amoebum.sessions:export-conversation-json conv)))
    (is (stringp json))
    (is (plusp (length json)))))

(test export-json-valid-structure
  "JSON export starts with { and ends with }."
  (let* ((conv (%make-test-conversation))
         (json (string-trim '(#\Space #\Newline #\Return #\Tab)
                            (amoebum.sessions:export-conversation-json conv))))
    (is (char= #\{ (char json 0)))
    (is (char= #\} (char json (1- (length json)))))))

(test export-json-contains-session-id
  "JSON export contains session ID."
  (let* ((conv (%make-test-conversation))
         (json (amoebum.sessions:export-conversation-json conv)))
    (is (search "test-session-001" json))))

(test export-json-contains-messages
  "JSON export contains messages array."
  (let* ((conv (%make-test-conversation))
         (json (amoebum.sessions:export-conversation-json conv)))
    (is (search "\"messages\"" json))
    (is (search "\"role\"" json))
    (is (search "\"content\"" json))))

(test export-json-entry-count
  "JSON export contains correct entry count."
  (let* ((conv (%make-test-conversation))
         (json (amoebum.sessions:export-conversation-json conv)))
    (is (search "\"entry_count\": 5" json))))

;;; --- File export ---

(test export-conversation-to-file
  "export-conversation writes to file and returns path."
  (let ((old-dir amoebum.sessions:*conversation-export-directory*)
        (tmp-dir (merge-pathnames
                  (format nil "amoebum-conv-export-~D/" (get-universal-time))
                  #P"/tmp/")))
    (unwind-protect
         (progn
           (ensure-directories-exist tmp-dir)
           (setf amoebum.sessions:*conversation-export-directory* tmp-dir)
           (let* ((conv (%make-test-conversation))
                  (path (amoebum.sessions:export-conversation conv
                                                     :format-type :markdown
                                                     :project-name "test-proj")))
             (is (pathnamep path))
             (is (probe-file path))
             ;; File should contain markdown
             (let ((content (uiop:read-file-string path)))
               (is (search "Conversation Export" content)))))
      (setf amoebum.sessions:*conversation-export-directory* old-dir)
      (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t)))))

(test export-conversation-json-to-file
  "export-conversation writes JSON to file."
  (let ((old-dir amoebum.sessions:*conversation-export-directory*)
        (tmp-dir (merge-pathnames
                  (format nil "amoebum-conv-json-~D/" (get-universal-time))
                  #P"/tmp/")))
    (unwind-protect
         (progn
           (ensure-directories-exist tmp-dir)
           (setf amoebum.sessions:*conversation-export-directory* tmp-dir)
           (let* ((conv (%make-test-conversation))
                  (path (amoebum.sessions:export-conversation conv
                                                     :format-type :json
                                                     :project-name "test-proj")))
             (is (pathnamep path))
             (is (probe-file path))
             (let ((content (uiop:read-file-string path)))
               (is (search "messages" content)))))
      (setf amoebum.sessions:*conversation-export-directory* old-dir)
      (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t)))))
