(in-package :amoebum)

(defparameter +sandbox-max-output-size+ (* 100 1024))
(defparameter +sandbox-max-read-size+ (* 10 1024 1024))

(defparameter *sandbox-read-only-write-tools*
  '("write-file" "edit-file"))

(defparameter *sandbox-shell-tools*
  '("bash" "bash-exec" "shell" "sh"))

(define-condition sandbox-violation (error)
  ((operation :initarg :operation
              :reader sandbox-violation-operation)
   (reason :initarg :reason
           :reader sandbox-violation-reason)
   (details :initarg :details
            :initform nil
            :reader sandbox-violation-details))
  (:report (lambda (condition stream)
             (format stream "Sandbox violation for ~A: ~A~@[ (~S)~]"
                     (sandbox-violation-operation condition)
                     (sandbox-violation-reason condition)
                     (sandbox-violation-details condition)))))

(define-condition sandbox-read-eval-disabled (sandbox-violation)
  ((form :initarg :form
         :initform nil
         :reader sandbox-read-eval-disabled-form))
  (:report (lambda (condition stream)
             (format stream "Sandbox readtable blocks #. read-eval~@[ for form ~S~]."
                     (sandbox-read-eval-disabled-form condition)))))

(define-condition sandbox-read-size-exceeded (sandbox-violation)
  ((path :initarg :path
         :reader sandbox-read-size-exceeded-path)
   (size-bytes :initarg :size-bytes
               :reader sandbox-read-size-exceeded-size-bytes)
   (limit-bytes :initarg :limit-bytes
                :reader sandbox-read-size-exceeded-limit-bytes))
  (:report (lambda (condition stream)
             (format stream
                     "Sandbox read limit exceeded for ~A (~D bytes > ~D bytes)."
                     (sandbox-read-size-exceeded-path condition)
                     (sandbox-read-size-exceeded-size-bytes condition)
                     (sandbox-read-size-exceeded-limit-bytes condition)))))

(defun %sandbox-path-text (path)
  (typecase path
    (pathname (namestring path))
    (string path)
    (symbol (symbol-name path))
    (t (princ-to-string path))))

(defun %command->string (command)
  (cond
    ((null command) "")
    ((stringp command) command)
    ((pathnamep command) (namestring command))
    ((symbolp command) (symbol-name command))
    ((and (listp command) command)
     (with-output-to-string (stream)
       (loop for item in command
             for idx from 0 do
               (when (> idx 0)
                 (write-char #\Space stream))
               (write-string (%command->string item) stream))))
    (t
     (princ-to-string command))))

(defun %remove-plist-keys (plist keys)
  (loop for (key value) on plist by #'cddr
        unless (member key keys :test #'eq)
          append (list key value)))

(defun %input-direction-p (direction)
  (member direction '(:input :io :probe) :test #'eq))

(defun %open-direction-tool (direction)
  (if (%input-direction-p direction)
      :read-file
      :write-file))

(defun %sandbox-file-size-bytes (path)
  (handler-case
      (with-open-file (stream path
                              :direction :input
                              :element-type '(unsigned-byte 8))
        (file-length stream))
    (error ()
      nil)))

(defun %assert-permission-allowed (&key tool path command dangerous-p permission-mode rules)
  (let ((decision (check-permission :tool tool
                                    :path path
                                    :command command
                                    :dangerous-p dangerous-p
                                    :permission-mode permission-mode
                                    :rules rules)))
    (unless (eq decision :allow)
      (error 'sandbox-violation
             :operation (or tool :sandbox)
             :reason (format nil "permission decision ~A" decision)
             :details (or path command)))))

(defun %assert-max-read-size (path max-read-size)
  (let ((existing (probe-file path)))
    (when existing
      (let ((size-bytes (%sandbox-file-size-bytes existing)))
        (when (and (integerp size-bytes)
                   (> size-bytes max-read-size))
          (error 'sandbox-read-size-exceeded
                 :operation :safe-open
                 :reason "file exceeds sandbox max read size"
                 :path (%sandbox-path-text existing)
                 :size-bytes size-bytes
                 :limit-bytes max-read-size
                 :details (list :path (%sandbox-path-text existing)
                                :size-bytes size-bytes
                                :limit-bytes max-read-size)))))))

(defun normalize-sandbox-policy (value)
  (let ((normalized
          (cond
            ((keywordp value) value)
            ((symbolp value)
             (intern (string-upcase (symbol-name value)) :keyword))
            ((stringp value)
             (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value))
                    (spaced (substitute #\- #\_ (string-downcase trimmed))))
               (when (> (length spaced) 0)
                 (intern (string-upcase spaced) :keyword))))
            (t nil))))
    (if (member normalized *known-sandbox-policies* :test #'eq)
        normalized
        :strict)))

(defun sandbox-policy (&optional (cfg (ignore-errors (current-config))))
  (normalize-sandbox-policy
   (and cfg (ignore-errors (config-value :sandbox-policy cfg)))))

(defun normalize-sandbox-mode (value)
  (let ((normalized
          (cond
            ((keywordp value) value)
            ((symbolp value)
             (intern (string-upcase (symbol-name value)) :keyword))
            ((stringp value)
             (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value))
                    (spaced (substitute #\- #\_ (string-downcase trimmed))))
               (when (> (length spaced) 0)
                 (intern (string-upcase spaced) :keyword))))
            (t nil))))
    (case normalized
      (:READ_ONLY :read-only)
      (:READ-ONLY :read-only)
      (:WORKSPACE_WRITE :workspace-write)
      (:WORKSPACE-WRITE :workspace-write)
      (:DANGER_FULL_ACCESS :danger-full-access)
      (:DANGER-FULL-ACCESS :danger-full-access)
      (otherwise :workspace-write))))

(defun sandbox-mode (&optional (cfg (ignore-errors (current-config))))
  (normalize-sandbox-mode
   (and cfg (ignore-errors (config-value :sandbox-mode cfg)))))

(defun sandbox-danger-full-access-p (&optional (cfg (ignore-errors (current-config))))
  (eq (sandbox-mode cfg) :danger-full-access))

(defun sandbox-read-only-p (&optional (cfg (ignore-errors (current-config))))
  (eq (sandbox-mode cfg) :read-only))

(defun sandbox-policy-enabled-p (&optional (cfg (ignore-errors (current-config))))
  (and (not (sandbox-danger-full-access-p cfg))
       (not (eq (sandbox-policy cfg) :off))))

(defun %sandbox-argument-key-candidates (key-name)
  (let* ((raw (if (symbolp key-name)
                  (symbol-name key-name)
                  (princ-to-string key-name)))
         (lower (string-downcase raw))
         (upper (string-upcase raw)))
    (list lower upper raw (intern upper :keyword))))

(defun %sandbox-argument-value (arguments key-name)
  (when (hash-table-p arguments)
    (loop for candidate in (%sandbox-argument-key-candidates key-name)
          do (multiple-value-bind (value present-p)
                 (gethash candidate arguments)
               (when present-p
                 (return value)))
          finally (return nil))))

(defun %sandbox-path-argument (arguments)
  (or (%sandbox-argument-value arguments "path")
      (%sandbox-argument-value arguments "file")
      (%sandbox-argument-value arguments "target")))

(defun %sandbox-command-argument (arguments)
  (or (%sandbox-argument-value arguments "command")
      (%sandbox-argument-value arguments "cmd")))

(defun %sandbox-tool-name-string (tool-name)
  (string-downcase
   (string-trim '(#\Space #\Tab #\Newline #\Return)
                (if (symbolp tool-name)
                    (symbol-name tool-name)
                    (princ-to-string tool-name)))))

(defparameter *sandbox-read-guard-tools*
  '("read-file"))

(defun %sandbox-read-only-tool-blocked-p (tool-name command-text)
  (let ((tool (%sandbox-tool-name-string tool-name)))
    (or (member tool *sandbox-read-only-write-tools* :test #'string=)
        (member tool *sandbox-shell-tools* :test #'string=)
        (and (stringp command-text)
             (> (length command-text) 0)))))

(defun sandbox-check-tool-call (tool-name arguments &key permission-mode)
  (when (sandbox-policy-enabled-p)
    (let* ((path (%sandbox-path-argument arguments))
           (command (%sandbox-command-argument arguments))
           (path-text (and path (%sandbox-path-text path)))
           (command-text (%command->string command))
           (dangerous-p (dangerous-command-p command-text)))
      (when (and (sandbox-read-only-p)
                 (%sandbox-read-only-tool-blocked-p tool-name command-text))
        (error 'sandbox-violation
               :operation (or tool-name :sandbox)
               :reason "sandbox mode read-only denies mutating or shell tool call"
               :details (or command-text path-text)))
      (when path-text
        (%assert-permission-allowed :tool tool-name
                                    :path path-text
                                    :permission-mode permission-mode
                                    :rules *permission-rules*)
        (when (member (%sandbox-tool-name-string tool-name)
                      *sandbox-read-guard-tools*
                      :test #'string=)
          (%assert-max-read-size path-text +sandbox-max-read-size+)))
      (when (plusp (length command-text))
        (%assert-permission-allowed :tool tool-name
                                    :command command-text
                                    :dangerous-p dangerous-p
                                    :permission-mode permission-mode
                                    :rules *permission-rules*))))
  t)

(defun safe-open (path &rest options
                 &key (direction :input)
                   tool
                   permission-mode
                   (rules *permission-rules*)
                   (max-read-size +sandbox-max-read-size+)
                 &allow-other-keys)
  (let* ((path-text (%sandbox-path-text path))
         (tool-name (or tool (%open-direction-tool direction)))
         (open-options (%remove-plist-keys
                        options
                        '(:tool :permission-mode :rules :max-read-size))))
    (when (and (sandbox-read-only-p)
               (not (%input-direction-p direction)))
      (error 'sandbox-violation
             :operation tool-name
             :reason "sandbox mode read-only denies write access"
             :details path-text))
    (%assert-permission-allowed :tool tool-name
                                :path path-text
                                :permission-mode permission-mode
                                :rules rules)
    (when (%input-direction-p direction)
      (%assert-max-read-size path-text max-read-size))
    (apply #'open path open-options)))

(defun safe-run-program (command &rest options
                        &key
                          (tool :bash-exec)
                          permission-mode
                          (rules *permission-rules*)
                          (check-permission t)
                          (allow-dangerous nil)
                        &allow-other-keys)
  (let* ((command-text (%command->string command))
         (dangerous-p (dangerous-command-p command-text))
         (run-program-options (%remove-plist-keys
                               options
                               '(:tool :permission-mode :rules :check-permission :allow-dangerous))))
    (when (sandbox-read-only-p)
      (error 'sandbox-violation
             :operation tool
             :reason "sandbox mode read-only denies shell execution"
             :details command-text))
    (when check-permission
      (%assert-permission-allowed :tool tool
                                  :command command-text
                                  :dangerous-p (and dangerous-p (not allow-dangerous))
                                  :permission-mode permission-mode
                                  :rules rules))
    (apply #'uiop:run-program command run-program-options)))

(defun truncate-sandbox-output (value &key (max-output-size +sandbox-max-output-size+))
  (labels ((truncate-value (entry)
             (cond
               ((stringp entry)
                (if (> (length entry) max-output-size)
                    (values (subseq entry 0 max-output-size) t)
                    (values entry nil)))
               ((hash-table-p entry)
                (let ((copy (make-hash-table :test (hash-table-test entry)
                                             :size (max 1 (hash-table-count entry))))
                      (changed nil))
                  (maphash (lambda (key item)
                             (multiple-value-bind (truncated-item truncated-p)
                                 (truncate-value item)
                               (setf (gethash key copy) truncated-item)
                               (when truncated-p
                                 (setf changed t))))
                           entry)
                  (values copy changed)))
               ((listp entry)
                (let ((result '())
                      (changed nil))
                  (dolist (item entry (values (nreverse result) changed))
                    (multiple-value-bind (truncated-item truncated-p)
                        (truncate-value item)
                      (push truncated-item result)
                      (when truncated-p
                        (setf changed t))))))
               ((vectorp entry)
                (let ((copy (copy-seq entry))
                      (changed nil))
                  (dotimes (index (length copy))
                    (multiple-value-bind (truncated-item truncated-p)
                        (truncate-value (aref copy index))
                      (setf (aref copy index) truncated-item)
                      (when truncated-p
                        (setf changed t))))
                  (values copy changed)))
               (t
                (values entry nil)))))
    (truncate-value value)))

(defun apply-sandbox-output-guard (value)
  (if (sandbox-policy-enabled-p)
      (multiple-value-bind (truncated-value truncated-p)
          (truncate-sandbox-output value)
        (declare (ignore truncated-p))
        truncated-value)
      value))

(defun %sandbox-read-eval-dispatch (stream sub-char arg)
  (declare (ignore stream sub-char arg))
  (error 'sandbox-read-eval-disabled
         :operation :read
         :reason "#. read-eval is disabled in restricted sandbox readtable"))

(defun make-restricted-readtable ()
  (let ((table (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\. #'%sandbox-read-eval-dispatch table)
    table))

(defparameter *sandbox-restricted-readtable* (make-restricted-readtable))

(defun sandbox-read-from-string (text &key (start 0) end (eof-error-p t) eof-value)
  (let ((*readtable* *sandbox-restricted-readtable*)
        (*read-eval* nil))
    (read-from-string text eof-error-p eof-value :start start :end end)))
