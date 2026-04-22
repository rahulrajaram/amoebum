(in-package :amoebum)

;;;; Extension permission preparation, sandbox capability negotiation, and
;;;; load-time form validation.
;;;;
;;;; Extracted from extensions/loader.lisp under NXT-386 to separate the
;;;; pre-load capability negotiation pipeline (permission normalization,
;;;; approval prompts, package isolation, denylist scanning, top-level form
;;;; validation) from runtime registry/loader orchestration.
;;;;
;;;; Public surface (re-exported via :amoebum.extensions facade):
;;;;   +extension-supported-permissions+
;;;;   *extension-safe-operations*
;;;;   *extension-permission-approvals*
;;;;   *extension-permission-prompt-function*
;;;;   clear-extension-permission-approvals
;;;;
;;;; Internal helpers used by extensions/loader.lisp:
;;;;   %ensure-extension-permissions-approved
;;;;   %ensure-extension-package
;;;;   %extension-package-name
;;;;   %metadata-extension-package
;;;;   %validate-extension-form

(defparameter +extension-supported-permissions+ '(:filesystem :network :shell))

(defparameter *extension-permission-approvals* (make-hash-table :test #'equal))
(defparameter *extension-permission-prompt-function* nil)

(defparameter *extension-safe-operations*
  '("deftool" "defhook"
    "defun" "defmacro" "defparameter" "defvar"
    "progn" "let" "let*" "setf" "setq" "incf" "decf"
    "if" "when" "unless" "cond" "case" "ecase" "typecase"
    "handler-case" "ignore-errors" "unwind-protect"
    "multiple-value-bind" "dolist" "dotimes" "loop"
    "values" "quote" "function" "lambda"
    "list" "append" "cons" "car" "cdr"
    "and" "or" "not"
    "+" "-" "*" "/" "1+" "1-" "=" "<" ">" "<=" ">="))

;;;; ---------------------------------------------------------------------
;;;; Permission normalization.

(defun %normalize-extension-permission (value &key (errorp t))
  (let ((normalized
          (cond
            ((keywordp value) value)
            ((symbolp value) (intern (string-upcase (symbol-name value)) :keyword))
            ((stringp value) (intern (string-upcase (%extension-trim value)) :keyword))
            (t nil))))
    (cond
      ((member normalized +extension-supported-permissions+ :test #'eq)
       normalized)
      (errorp
       (error "Unsupported extension permission ~S. Expected one of ~S."
              value
              +extension-supported-permissions+))
      (t nil))))

(defun %normalize-extension-permissions (value &key (errorp t))
  (let ((raw
          (cond
            ((null value) '())
            ((listp value) value)
            (t (list value)))))
    (remove-duplicates
     (loop for permission in raw
           for normalized = (%normalize-extension-permission permission :errorp errorp)
           when normalized collect normalized)
     :test #'eq)))

(defun clear-extension-permission-approvals ()
  (clrhash *extension-permission-approvals*)
  t)

(defun %normalize-extension-permission-decision (value)
  (cond
    ((or (eq value :allow)
         (eq value t)
         (and (stringp value)
              (member (string-downcase (%extension-trim value))
                      '("allow" "approved" "approve" "yes" "y")
                      :test #'string=)))
     :allow)
    (t :deny)))

(defun %default-extension-permission-prompt (extension-name permission scope _metadata)
  (declare (ignore _metadata))
  (let ((prompt
          (format nil
                  "Extension ~A requests ~A permission (~A scope). Approve? [y/N]: "
                  extension-name
                  permission
                  scope)))
    (if (and (streamp *query-io*)
             (interactive-stream-p *query-io*))
        (progn
          (format *query-io* "~A" prompt)
          (finish-output *query-io*)
          (%normalize-extension-permission-decision
           (read-line *query-io* nil "")))
        (progn
          (format *error-output* "~Adenied (non-interactive).~%" prompt)
          :deny))))

(unless *extension-permission-prompt-function*
  (setf *extension-permission-prompt-function*
        #'%default-extension-permission-prompt))

(defun %extension-permission-approval-key (extension-name permission)
  (format nil "~A::~A"
          (string-downcase (%extension-trim extension-name))
          (string-downcase (symbol-name permission))))

(defun %ensure-extension-permissions-approved (metadata scope)
  (let* ((extension-name (or (getf metadata :name) "unknown-extension"))
         (permissions (or (getf metadata :permissions) '()))
         (prompt-fn (or *extension-permission-prompt-function*
                        #'%default-extension-permission-prompt)))
    (dolist (permission permissions)
      (let ((key (%extension-permission-approval-key extension-name permission)))
        (unless (gethash key *extension-permission-approvals*)
          (let ((decision
                  (%normalize-extension-permission-decision
                   (funcall prompt-fn extension-name permission scope metadata))))
            (unless (eq decision :allow)
              (error "Extension ~A permission ~S denied by user."
                     extension-name
                     permission))
            (setf (gethash key *extension-permission-approvals*) t))))))
  t)

;;;; ---------------------------------------------------------------------
;;;; Package isolation (sandbox capability prep).

(defun %sanitize-extension-package-fragment (value)
  (let* ((raw (%extension-trim (or value "")))
         (upper (string-upcase raw)))
    (if (zerop (length upper))
        "UNNAMED"
        (with-output-to-string (stream)
          (loop for char across upper do
                (if (or (alphanumericp char)
                        (char= char #\-)
                        (char= char #\_))
                    (write-char char stream)
                    (write-char #\- stream)))))))

(defun %extension-package-name (metadata)
  (format nil "AMOEBUM.EXT.~A"
          (%sanitize-extension-package-fragment (getf metadata :name))))

(defun %ensure-extension-package (metadata)
  (let* ((package-name (%extension-package-name metadata))
         (package (or (find-package package-name)
                      (make-package package-name :use '(:cl :amoebum)))))
    (dolist (dependency '(:cl :amoebum))
      (let ((dep-package (find-package dependency)))
        (when (and dep-package
                   (not (member dep-package (package-use-list package))))
          (use-package dep-package package))))
    package))

(defun %metadata-extension-package (metadata)
  (and (eq (getf metadata :kind) :manifest)
       (%ensure-extension-package metadata)))

;;;; ---------------------------------------------------------------------
;;;; Symbol-walking helpers and the load-time form validator.

(defun %symbol-token (symbol)
  (and (symbolp symbol)
       (string-downcase (symbol-name symbol))))

(defun %symbol-qualified-token (symbol)
  (when (symbolp symbol)
    (let ((pkg (symbol-package symbol)))
      (if pkg
          (format nil "~A:~A"
                  (string-downcase (package-name pkg))
                  (string-downcase (symbol-name symbol)))
          (%symbol-token symbol)))))

(defun %walk-extension-form-symbols (form visitor)
  (cond
    ((symbolp form)
     (funcall visitor form))
    ((consp form)
     (let ((head (car form)))
       (if (and (symbolp head)
                (member (%symbol-token head)
                        '("quote" "function")
                        :test #'string=))
           nil
           (dolist (item form)
             (%walk-extension-form-symbols item visitor)))))
    (t nil)))

(defun %extension-denied-operation (symbol)
  (let* ((token (%symbol-token symbol))
         (qualified (%symbol-qualified-token symbol))
         (pkg-name (and (symbol-package symbol)
                        (string-downcase (package-name (symbol-package symbol))))))
    (cond
      ((or (string= qualified "sb-ext:run-program")
           (string= qualified "uiop:run-program")
           (string= token "run-program"))
       (list :operation (or qualified token)
             :permission :shell
             :message "Shell execution is blocked without :shell permission."))
      ((or (string= token "open")
           (string= token "with-open-file"))
       (list :operation (or qualified token)
             :permission :filesystem
             :message "Filesystem access is blocked without :filesystem permission."))
      ((member pkg-name '("sb-alien" "cffi" "cffi-sys") :test #'string=)
       (list :operation (or qualified token)
             :permission :ffi
             :message "FFI operations are not allowed in extensions."))
      (t nil))))

(defun %scan-extension-form-for-denylist (form)
  (let ((violations '())
        (seen (make-hash-table :test #'equal)))
    (%walk-extension-form-symbols
     form
     (lambda (symbol)
       (let ((violation (%extension-denied-operation symbol)))
         (when violation
           (let* ((operation (or (getf violation :operation) "unknown"))
                  (permission (or (getf violation :permission) :unknown))
                  (key (format nil "~A|~A" operation permission)))
             (unless (gethash key seen)
               (setf (gethash key seen) t)
               (push violation violations)))))))
    (nreverse violations)))

(defun %extension-top-level-allowlisted-p (operator extension-package)
  (cond
    ((null operator) t)
    ((not (symbolp operator)) nil)
    ((and extension-package
          (eq (symbol-package operator) extension-package))
     t)
    ((member (%symbol-token operator)
             *extension-safe-operations*
             :test #'string=)
     t)
    (t nil)))

(defun %extension-in-package-target (form)
  (when (and (consp form)
             (symbolp (car form))
             (string= (%symbol-token (car form)) "in-package")
             (consp (cdr form)))
    (let ((target (cadr form)))
      (cond
        ((symbolp target) (string-upcase (symbol-name target)))
        ((stringp target) (string-upcase target))
        (t nil)))))

(defun %extension-in-package-allowed-p (form extension-package)
  (let* ((target (%extension-in-package-target form))
         (target-package (and target (find-package target))))
    (or (null target)
        (and extension-package
             target-package
             (string-equal (package-name target-package)
                           (package-name extension-package)))
        ;; Test fixtures use AMOEBUM/TEST globals to assert hot-reload behavior.
        (and target-package
             (string-equal (package-name target-package) "AMOEBUM/TEST")))))

(defun %validate-extension-form (form metadata extension-package)
  (when (consp form)
    (let* ((operator (car form))
           (extension-name (or (getf metadata :name) "unknown-extension"))
           (permissions (or (getf metadata :permissions) '()))
           (violations (%scan-extension-form-for-denylist form)))
      (when (and (symbolp operator)
                 (string= (%symbol-token operator) "in-package")
                 (not (%extension-in-package-allowed-p form extension-package)))
        (error "Extension ~A cannot call IN-PACKAGE; it is isolated in package ~A."
               extension-name
               (package-name extension-package)))
      (unless (or (%extension-top-level-allowlisted-p operator extension-package)
                  (and (symbolp operator)
                       (string= (%symbol-token operator) "in-package")
                       (%extension-in-package-allowed-p form extension-package))
                  (plusp (length violations)))
        (error "Extension ~A uses non-allowlisted top-level operation ~S."
               extension-name
               operator))
      (dolist (violation violations)
        (let ((permission (getf violation :permission))
              (operation (getf violation :operation))
              (message (getf violation :message)))
          (cond
            ((eq permission :ffi)
             (error "Extension ~A blocked: ~A (~A)."
                    extension-name
                    operation
                    message))
            ((not (member permission permissions :test #'eq))
             (error "Extension ~A blocked: operation ~A requires permission ~S."
                    extension-name
                    operation
                    permission)))))))
  t)
