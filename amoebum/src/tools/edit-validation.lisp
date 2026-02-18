(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; Edit Validation (I109)
;;;
;;; Guardrails for amoebum's edit-file tool: precondition checks before edits,
;;; content hash verification to detect concurrent modifications, and post-edit
;;; verification hooks (syntax validation, parse checks).  Wires into the
;;; amoebum pipeline through the existing deftool / execute-tool infrastructure.
;;; ---------------------------------------------------------------------------

;;; --- Configuration ---------------------------------------------------------

(defparameter *edit-validation-enabled-p* t
  "When non-nil, edit validation guardrails are active.")

(defparameter *edit-validation-post-hooks* '()
  "List of post-edit verification hook functions.
Each function receives (PATH NEW-CONTENT) and should return
 (VALUES OK-P MESSAGE) where OK-P is a boolean and MESSAGE is a string
describing the result.  A nil OK-P indicates verification failure.")

;;; --- Content hashing -------------------------------------------------------

(defun %edit-validation-content-hash (content)
  "Compute a simple content hash for edit conflict detection.
Uses a combination of length and character-based checksum for efficiency."
  (let ((length (length content))
        (checksum 0))
    (loop for i from 0 below (min length 8192)
          do (setf checksum
                   (logand #xFFFFFFFF
                           (+ (* checksum 31)
                              (char-code (char content i))))))
    (list :length length :checksum checksum)))

(defun %edit-validation-hash-equal-p (hash1 hash2)
  "Return T if two content hashes are equal."
  (and hash1 hash2
       (eql (getf hash1 :length) (getf hash2 :length))
       (eql (getf hash1 :checksum) (getf hash2 :checksum))))

;;; --- Content hash tracking -------------------------------------------------

(defparameter *edit-validation-content-hashes* (make-hash-table :test #'equal)
  "Maps canonical path keys to content hashes recorded at read time.")

(defun %edit-validation-record-content-hash (path content)
  "Record a content hash for PATH after a successful read."
  (let ((key (%canonical-path-key path)))
    (setf (gethash key *edit-validation-content-hashes*)
          (%edit-validation-content-hash content))
    key))

(defun %edit-validation-stored-hash (path)
  "Retrieve the stored content hash for PATH, or NIL if not recorded."
  (gethash (%canonical-path-key path) *edit-validation-content-hashes*))

;;; --- Precondition validation -----------------------------------------------

(define-condition edit-validation-error (tool-argument-error)
  ()
  (:documentation "Signalled when an edit validation precondition fails.")
  (:report (lambda (condition stream)
             (let ((reason (or (tool-error-reason condition)
                               (amoebum-error-message condition)
                               "edit validation failed")))
               (format stream "Edit validation error for tool ~S: ~A"
                       (tool-error-tool-name condition) reason)))))

(defun %edit-validation-signal (argument-name message &key reason)
  "Signal an EDIT-VALIDATION-ERROR with the given details."
  (error 'edit-validation-error
         :tool-name "edit-file"
         :argument-name argument-name
         :message message
         :reason (or reason message)))

(defun %validate-edit-path (path)
  "Validate that PATH exists and is readable."
  (when (null path)
    (%edit-validation-signal "path" "File path is required."
                             :reason "missing file path"))
  (let ((path-string (%path-text path)))
    (when (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                       path-string)))
      (%edit-validation-signal "path" "File path must not be empty."
                               :reason "empty path"))
    (unless (probe-file path-string)
      (%edit-validation-signal "path"
                               (format nil "File does not exist: ~A" path-string)
                               :reason "file not found"))
    ;; Check readability by attempting to open
    (handler-case
        (with-open-file (stream path-string :direction :input
                                            :if-does-not-exist nil)
          (unless stream
            (%edit-validation-signal "path"
                                     (format nil "File is not readable: ~A" path-string)
                                     :reason "file not readable")))
      (error (condition)
        (%edit-validation-signal "path"
                                 (format nil "Cannot read file ~A: ~A"
                                         path-string condition)
                                 :reason "file not readable")))
    path-string))

(defun %validate-edit-old-string (old-string)
  "Validate that OLD-STRING is a non-empty string."
  (when (null old-string)
    (%edit-validation-signal "old_string" "old_string is required."
                             :reason "missing old_string"))
  (unless (stringp old-string)
    (%edit-validation-signal "old_string"
                             (format nil "old_string must be a string, got ~S."
                                     (type-of old-string))
                             :reason "invalid old_string type"))
  (when (zerop (length old-string))
    (%edit-validation-signal "old_string" "old_string must not be empty."
                             :reason "empty old_string"))
  old-string)

(defun %validate-edit-content-hash (path)
  "Verify file content hasn't changed since last read by checking content hash.
Returns (VALUES OK-P CURRENT-HASH STORED-HASH)."
  (let* ((path-string (%path-text path))
         (stored-hash (%edit-validation-stored-hash path-string))
         (current-content (handler-case
                              (uiop:read-file-string path-string
                                                      :external-format :utf-8)
                            (error () nil)))
         (current-hash (when current-content
                         (%edit-validation-content-hash current-content))))
    (cond
      ((null stored-hash)
       ;; No stored hash -- rely on the existing snapshot check in edit-file
       (values t current-hash nil))
      ((null current-hash)
       ;; Cannot read current content
       (values nil nil stored-hash))
      ((%edit-validation-hash-equal-p stored-hash current-hash)
       (values t current-hash stored-hash))
      (t
       (values nil current-hash stored-hash)))))

;;; --- Post-edit verification hooks ------------------------------------------

(defun %run-post-edit-hooks (path new-content)
  "Run all registered post-edit verification hooks.
Returns a list of (HOOK-NAME OK-P MESSAGE) results."
  (let ((results '()))
    (dolist (hook-entry *edit-validation-post-hooks*)
      (destructuring-bind (hook-name . hook-fn) hook-entry
        (handler-case
            (multiple-value-bind (ok-p message)
                (funcall hook-fn path new-content)
              (push (list hook-name ok-p (or message "")) results))
          (error (condition)
            (push (list hook-name nil (princ-to-string condition)) results)))))
    (nreverse results)))

(defun register-post-edit-hook (name function)
  "Register a post-edit verification hook with NAME and FUNCTION.
FUNCTION receives (PATH NEW-CONTENT) and returns (VALUES OK-P MESSAGE)."
  (let ((existing (assoc name *edit-validation-post-hooks* :test #'equal)))
    (if existing
        (setf (cdr existing) function)
        (push (cons name function) *edit-validation-post-hooks*)))
  name)

(defun unregister-post-edit-hook (name)
  "Remove a post-edit verification hook by NAME."
  (setf *edit-validation-post-hooks*
        (remove name *edit-validation-post-hooks*
                :key #'car :test #'equal))
  name)

(defun clear-post-edit-hooks ()
  "Remove all post-edit verification hooks."
  (setf *edit-validation-post-hooks* '())
  t)

;;; --- Main validation entry points ------------------------------------------

(defun validate-edit-preconditions (path old-string new-string)
  "Validate all edit-file preconditions.
Checks: file exists, file readable, old-string non-empty, content hash match.
Returns a plist with :valid-p, :path, :warnings, :content-hash-ok-p."
  (declare (ignore new-string))
  (let ((warnings '())
        (valid-p t)
        (content-hash-ok-p t))
    ;; Validate path
    (let ((path-string (%validate-edit-path path)))
      ;; Validate old-string
      (%validate-edit-old-string old-string)
      ;; Content hash check (advisory, not blocking)
      (multiple-value-bind (hash-ok current-hash stored-hash)
          (%validate-edit-content-hash path)
        (declare (ignore current-hash stored-hash))
        (unless hash-ok
          (setf content-hash-ok-p nil)
          (push (format nil "Content of ~A has changed since last read (hash mismatch)."
                        path-string)
                warnings)))
      (list :valid-p valid-p
            :path path-string
            :warnings (nreverse warnings)
            :content-hash-ok-p content-hash-ok-p))))

(defun validate-edit-postconditions (path new-content)
  "Run post-edit verification hooks on the edited file.
Returns a plist with :valid-p, :hook-results, :warnings."
  (let ((hook-results (%run-post-edit-hooks path new-content))
        (warnings '())
        (all-ok t))
    (dolist (result hook-results)
      (destructuring-bind (hook-name ok-p message) result
        (unless ok-p
          (setf all-ok nil)
          (push (format nil "Post-edit hook ~A failed: ~A" hook-name message)
                warnings))))
    (list :valid-p all-ok
          :hook-results hook-results
          :warnings (nreverse warnings))))

;;; --- Pipeline integration via defhook --------------------------------------

(defun %edit-validation-pre-hook (tool-name arguments)
  "Pre-tool-use hook for edit validation.
Only activates for edit-file tool calls when validation is enabled."
  (when (and *edit-validation-enabled-p*
             (string= (%pipeline-normalize-tool-name tool-name) "edit-file"))
    (let* ((path (%argument-value arguments "path"))
           (old-string (%argument-value arguments "old_string"))
           (new-string (%argument-value arguments "new_string")))
      ;; Run precondition validation -- errors propagate as edit-validation-error
      (validate-edit-preconditions path old-string new-string)))
  :allow)

(defun %edit-validation-post-hook (tool-name result elapsed-ms)
  "Post-tool-use hook for edit validation.
Runs registered post-edit hooks after successful edit-file calls."
  (declare (ignore elapsed-ms))
  (when (and *edit-validation-enabled-p*
             (string= (%pipeline-normalize-tool-name tool-name) "edit-file")
             (listp result))
    (let ((path (getf result :path)))
      (when path
        (let ((new-content (handler-case
                               (uiop:read-file-string path :external-format :utf-8)
                             (error () nil))))
          (when new-content
            ;; Update content hash after successful edit
            (%edit-validation-record-content-hash path new-content)
            ;; Run post-edit hooks
            (let ((post-result (validate-edit-postconditions path new-content)))
              (unless (getf post-result :valid-p)
                ;; Log warnings but don't block -- edit already committed
                (let ((warnings (getf post-result :warnings)))
                  (when warnings
                    (setf (getf result :edit-validation-warnings) warnings))))))))))
  :ok)

;;; --- Wire hooks at load time -----------------------------------------------

(defun install-edit-validation-hooks ()
  "Install edit validation pre/post hooks into the hook registry."
  (register-hook :pre-tool-use
                 'edit-validation-pre-hook
                 #'%edit-validation-pre-hook)
  (register-hook :post-tool-use
                 'edit-validation-post-hook
                 #'%edit-validation-post-hook)
  t)

(defun uninstall-edit-validation-hooks ()
  "Remove edit validation hooks from the hook registry."
  (unregister-hook :pre-tool-use 'edit-validation-pre-hook)
  (unregister-hook :post-tool-use 'edit-validation-post-hook)
  t)

;;; --- Read-file integration: record content hash on read --------------------

(defun %edit-validation-after-read-hook (tool-name result elapsed-ms)
  "Post-tool-use hook that records content hash after read-file."
  (declare (ignore elapsed-ms))
  (when (and *edit-validation-enabled-p*
             (string= (%pipeline-normalize-tool-name tool-name) "read-file")
             (stringp result))
    ;; Extract path from pipeline context
    (let ((path (and *pipeline-current-arguments*
                     (%argument-value *pipeline-current-arguments* "path"))))
      (when path
        (%edit-validation-record-content-hash (%path-text path) result))))
  :ok)

;;; Install hooks
(install-edit-validation-hooks)
(register-hook :post-tool-use
               'edit-validation-read-hash-hook
               #'%edit-validation-after-read-hook)
