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

;; Path coercion delegated to coerce-path-string in util.lisp

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
    ;; :allow  → permitted
    ;; :prompt → interactive approval is the pipeline's responsibility;
    ;;           the sandbox enforces hard boundaries only.
    ;; :deny   → hard block
    (when (eq decision :deny)
      (error 'sandbox-violation
             :operation (or tool :sandbox)
             :reason "permission denied by policy"
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
                 :path (coerce-path-string existing)
                 :size-bytes size-bytes
                 :limit-bytes max-read-size
                 :details (list :path (coerce-path-string existing)
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

;; Name normalization delegated to normalize-name in util.lisp

(defparameter *sandbox-read-guard-tools*
  '("read-file"))

;;; ── Declarative sandbox enforcement ────────────────────────────────────

(defparameter *sandbox-enforcement-rules*
  '((:read-only :write-tools  :deny       "sandbox mode read-only denies mutating or shell tool call")
    (:read-only :shell-tools  :deny       "sandbox mode read-only denies mutating or shell tool call")
    (:read-only :has-command   :deny       "sandbox mode read-only denies mutating or shell tool call")
    (:any       :read-guard-tools :check-size "file exceeds sandbox max read size"))
  "Declarative enforcement rules for sandbox-check-tool-call.
Each entry is (MODE TOOL-CLASS ACTION REASON) where:
  MODE       — :read-only (fires only in read-only sandbox) or :any (always fires)
  TOOL-CLASS — classification returned by %sandbox-classify-tool
  ACTION     — :deny signals sandbox-violation; :check-size runs %assert-max-read-size
  REASON     — human-readable explanation attached to errors")

(defun %sandbox-classify-tool (tool-name command-text path-text)
  "Return the tool-class keyword for TOOL-NAME given its resolved arguments.
Returns one of :write-tools, :shell-tools, :has-command, :read-guard-tools,
or NIL when the tool does not match any classified category."
  (let ((tool (normalize-name tool-name)))
    (cond
      ((member tool *sandbox-read-only-write-tools* :test #'string=)
       :write-tools)
      ((member tool *sandbox-shell-tools* :test #'string=)
       :shell-tools)
      ((and (stringp command-text) (> (length command-text) 0))
       :has-command)
      ((and path-text
            (member tool *sandbox-read-guard-tools* :test #'string=))
       :read-guard-tools)
      (t nil))))

(defun %sandbox-enforce-rule (action tool-name path-text command-text reason)
  "Execute the ACTION for a matched enforcement rule.
:deny signals a sandbox-violation; :check-size runs %assert-max-read-size."
  (ecase action
    (:deny
     (error 'sandbox-violation
            :operation (or tool-name :sandbox)
            :reason reason
            :details (or command-text path-text)))
    (:check-size
     (when path-text
       (%assert-max-read-size path-text +sandbox-max-read-size+)))))

(defun sandbox-check-tool-call (tool-name arguments &key permission-mode)
  "Enforce structural sandbox limits (read-only mode, file size).
Permission decisions are handled by the pipeline chokepoint; this function
does NOT re-check permissions.  Walks *sandbox-enforcement-rules* to decide."
  (declare (ignore permission-mode))
  (when (sandbox-policy-enabled-p)
    (let* ((path (%sandbox-path-argument arguments))
           (command (%sandbox-command-argument arguments))
           (path-text (and path (coerce-path-string path)))
           (command-text (%command->string command))
           (tool-class (%sandbox-classify-tool tool-name command-text path-text))
           (read-only (sandbox-read-only-p)))
      (when tool-class
        (dolist (rule *sandbox-enforcement-rules*)
          (destructuring-bind (mode rule-class action reason) rule
            (when (and (eq rule-class tool-class)
                       (or (eq mode :any)
                           (and (eq mode :read-only) read-only)))
              (%sandbox-enforce-rule action tool-name path-text command-text reason)
              (return)))))))
  t)

(defun safe-open (path &rest options
                 &key (direction :input)
                   tool
                   permission-mode
                   (rules *permission-rules*)
                   (max-read-size +sandbox-max-read-size+)
                 &allow-other-keys)
  (let* ((path-text (coerce-path-string path))
         (canonical-path (or (%normalize-path path-text) path-text))
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
    (when (not (%input-direction-p direction))
      (%assert-path-identity-stable-at-use-time
       :tool tool-name
       :path path-text))
    (%assert-permission-allowed :tool tool-name
                                :path canonical-path
                                :permission-mode permission-mode
                                :rules rules)
    (when (%input-direction-p direction)
      (%assert-max-read-size canonical-path max-read-size))
    (apply #'open canonical-path open-options)))

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
