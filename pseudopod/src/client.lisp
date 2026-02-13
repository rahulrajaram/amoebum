(in-package :pseudopod)

(defparameter *default-base-url* "https://api.moonshot.ai/v1")
(defparameter *default-model* "kimi-k2.5")
(defparameter *default-api-key-file*
  (merge-pathnames #P".moonshotai" (user-homedir-pathname)))

(defstruct (client (:constructor %make-client))
  (api-key "" :type string)
  (base-url *default-base-url* :type string)
  (model *default-model* :type string)
  (temperature 1.0d0 :type real)
  (max-tokens 32768 :type integer)
  (top-p 0.95d0 :type real)
  (timeout-seconds 180 :type integer))

(defun %normalize-api-key (value)
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                              (or value ""))))
    (if (plusp (length trimmed))
        trimmed
        nil)))

(defun read-api-key (&key (env-var "MOONSHOT_API_KEY")
                       (path *default-api-key-file*))
  "Read API key from ENV-VAR first, then PATH (defaults to ~/.moonshotai)."
  (let ((from-env (uiop:getenv env-var)))
    (cond
      ((%normalize-api-key from-env)
       (%normalize-api-key from-env))
      ((uiop:file-exists-p path)
       (or (%normalize-api-key (uiop:read-file-string path))
           (error "Moonshot API key file exists but is empty: ~A" path)))
      (t
       (error "Moonshot API key not found. Set ~A or create ~A"
              env-var
              path)))))

(defun make-client (&key api-key
                      (api-key-file *default-api-key-file*)
                      (base-url *default-base-url*)
                      (model *default-model*)
                      (temperature 1.0d0)
                      (max-tokens 32768)
                      (top-p 0.95d0)
                      (timeout-seconds 180))
  "Create a Moonshot client configuration."
  (let ((normalized-api-key (%normalize-api-key api-key)))
    (when (and api-key (null normalized-api-key))
      (error "Provided Moonshot API key is empty."))
  (%make-client
   :api-key (or normalized-api-key
                (read-api-key :path api-key-file))
   :base-url base-url
   :model model
   :temperature temperature
   :max-tokens max-tokens
   :top-p top-p
   :timeout-seconds timeout-seconds)))

(defun %build-payload (client system-prompt user-prompt streamp)
  (let* ((system-message (let ((obj (make-hash-table :test #'equal)))
                           (setf (gethash "role" obj) "system")
                           (setf (gethash "content" obj) system-prompt)
                           obj))
         (user-message (let ((obj (make-hash-table :test #'equal)))
                         (setf (gethash "role" obj) "user")
                         (setf (gethash "content" obj) user-prompt)
                         obj))
         (payload (make-hash-table :test #'equal)))
    (setf (gethash "model" payload) (client-model client))
    (setf (gethash "messages" payload) (list system-message user-message))
    (setf (gethash "temperature" payload) (coerce (client-temperature client) 'double-float))
    (setf (gethash "max_tokens" payload) (client-max-tokens client))
    (setf (gethash "top_p" payload) (coerce (client-top-p client) 'double-float))
    (setf (gethash "stream" payload) (if streamp t :false))
    (jonathan:to-json payload)))

(defun %headers (client)
  `((:authorization . ,(format nil "Bearer ~A" (client-api-key client)))
    (:content-type . "application/json")))

(defun %endpoint (client)
  (format nil "~A/chat/completions" (client-base-url client)))

(defun chat-completion (client user-prompt &key (system-prompt "You are a helpful assistant."))
  "Run a non-streaming Moonshot chat completion and return parsed JSON object."
  (multiple-value-bind (body status)
      (dex:post (%endpoint client)
                :content (%build-payload client system-prompt user-prompt nil)
                :headers (%headers client)
                :connect-timeout (client-timeout-seconds client)
                :read-timeout (client-timeout-seconds client)
                :keep-alive nil)
    (unless (<= 200 status 299)
      (error "Moonshot request failed (status=~A): ~A" status body))
    (jonathan:parse body :as :hash-table)))

(defun %first-item (sequence)
  (cond
    ((null sequence) nil)
    ((listp sequence) (first sequence))
    ((vectorp sequence)
     (when (> (length sequence) 0)
       (aref sequence 0)))
    (t nil)))

(defun %consume-sse-line (line on-reasoning on-content)
  (let ((payload nil))
    (cond
      ((uiop:string-prefix-p "data: " line)
       (setf payload (subseq line 6)))
      ((uiop:string-prefix-p "data:" line)
       (setf payload (string-left-trim " " (subseq line 5)))))
    (when (and payload
               (plusp (length payload))
               (not (string= payload "[DONE]")))
      (handler-case
          (let* ((json (jonathan:parse payload :as :hash-table :junk-allowed t))
                 (choices (gethash "choices" json))
                 (choice (%first-item choices))
                 (delta (and (hash-table-p choice) (gethash "delta" choice)))
                 (reasoning (and (hash-table-p delta) (gethash "reasoning_content" delta)))
                 (content (and (hash-table-p delta) (gethash "content" delta))))
            (when (and on-reasoning (stringp reasoning) (> (length reasoning) 0))
              (funcall on-reasoning reasoning))
            (when (and on-content (stringp content) (> (length content) 0))
              (funcall on-content content)))
        (error ()
          ;; Ignore malformed or non-JSON SSE lines while streaming.
          nil)))))

(defun stream-chat-completion (client user-prompt
                               &key
                                 (system-prompt "You are a helpful assistant.")
                                 on-reasoning
                                 on-content)
  "Run a streaming Moonshot completion.
ON-REASONING and ON-CONTENT are callbacks that receive text chunks."
  (multiple-value-bind (body-stream status)
      (dex:post (%endpoint client)
                :content (%build-payload client system-prompt user-prompt t)
                :headers (%headers client)
                :want-stream t
                :connect-timeout (client-timeout-seconds client)
                :read-timeout (client-timeout-seconds client)
                :keep-alive nil)
    (unless (<= 200 status 299)
      (let ((error-body (ignore-errors (uiop:slurp-stream-string body-stream))))
        (ignore-errors (close body-stream))
        (error "Moonshot streaming request failed (status=~A): ~A"
               status
               (or error-body "<no-body>"))))
    (unwind-protect
        (loop for line = (read-line body-stream nil nil)
              while line do
                (%consume-sse-line line on-reasoning on-content))
      (ignore-errors (close body-stream)))))

(defun print-streamed-completion (client user-prompt
                                  &key
                                    (system-prompt "You are a helpful assistant.")
                                    (print-reasoning t))
  "Print a streaming completion directly to *STANDARD-OUTPUT*."
  (stream-chat-completion
   client
   user-prompt
   :system-prompt system-prompt
   :on-reasoning (when print-reasoning
                   (lambda (text) (write-string text)))
   :on-content (lambda (text) (write-string text)))
  (terpri))

(defun main ()
  "Simple interactive entrypoint for quick manual testing."
  (let ((client (make-client)))
    (format t "Prompt: ")
    (finish-output)
    (let ((prompt (read-line *standard-input* nil "")))
      (when (zerop (length prompt))
        (error "Prompt cannot be empty."))
      (print-streamed-completion client prompt))))
