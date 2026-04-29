(in-package :amoebum)

;;;; NXT-577: /deftool slash command.
;;;;
;;;; Lets an operator define and register a new tool from inside the
;;;; running amoebum chat — closing the loop on VISION §6.2
;;;; "macros-as-language extensibility" as a real keystroke-driven
;;;; workflow.
;;;;
;;;; UX:
;;;;
;;;;   /deftool <name> "<description>" <body-form>
;;;;
;;;; <body-form> must evaluate to a function of one argument (the
;;;; arguments hash-table for the tool invocation). The simplest form
;;;; is `(lambda (args) ...)`. Example:
;;;;
;;;;   /deftool greet "Say hello" (lambda (args) (declare (ignore args)) "hello")
;;;;
;;;; Removal:
;;;;
;;;;   /deftool --undo <name>
;;;;
;;;; Permission gate:
;;;;
;;;; Because the body is arbitrary Lisp, we route the form through
;;;; `sandboxed-eval` (the precedent established by /self-modify) so the
;;;; same unsafe-symbol allowlist applies. Tools registered this way
;;;; are inserted into `*toolset*` and `*tool-metadata*` directly using
;;;; the public pseudopod registration API. A proper macro layer
;;;; (mirroring `deftool` in `src/macros/deftool/expansion.lisp`) would
;;;; replace this once it's wanted, but the existing macro requires
;;;; compile-time expansion and therefore can't be invoked usefully
;;;; from a chat slash command without an `eval` in any case.

(defparameter +deftool-usage+
  "Usage: /deftool <name> \"<description>\" <body-form>
       /deftool --undo <name>")

(defun %deftool-result (output)
  (make-slash-command-result :output output :echo-input-p t))

(defun %deftool-usage-result (&optional details)
  (%deftool-result
   (if details
       (format nil "~A~%~A" details +deftool-usage+)
       +deftool-usage+)))

(defun %deftool-skip-spaces (text index)
  (loop while (and (< index (length text))
                   (member (char text index) '(#\Space #\Tab #\Newline #\Return)
                           :test #'char=))
        do (incf index))
  index)

(defun %deftool-read-name (text index)
  "Read the first whitespace-delimited token starting at INDEX. Returns
\(values name new-index\)."
  (let ((start (%deftool-skip-spaces text index)))
    (loop with end = start
          while (and (< end (length text))
                     (not (member (char text end)
                                  '(#\Space #\Tab #\Newline #\Return)
                                  :test #'char=)))
          do (incf end)
          finally (return (values (subseq text start end) end)))))

(defun %deftool-read-description (text index)
  "Read a quoted description. Supports \"...\" with backslash escapes.
Returns (values description new-index) or (values nil nil) on parse
failure."
  (let ((start (%deftool-skip-spaces text index)))
    (cond
      ((>= start (length text))
       (values nil nil))
      ((not (char= (char text start) #\"))
       (values nil nil))
      (t
       (let ((index (1+ start))
             (out (make-string-output-stream)))
         (loop while (< index (length text)) do
           (let ((char (char text index)))
             (cond
               ((char= char #\")
                (return (values (get-output-stream-string out) (1+ index))))
               ((and (char= char #\\) (< (1+ index) (length text)))
                (write-char (char text (1+ index)) out)
                (incf index 2))
               (t
                (write-char char out)
                (incf index))))
               finally (return (values nil nil))))))))

(defun %deftool-read-body (text index)
  (let ((start (%deftool-skip-spaces text index)))
    (when (< start (length text))
      (string-trim '(#\Space #\Tab #\Newline #\Return)
                   (subseq text start)))))

(defun %deftool-parse-define (raw)
  "Parse `<name> \"<desc>\" <body>`. Returns (values name desc body) or
(values nil nil nil) on failure."
  (multiple-value-bind (name after-name) (%deftool-read-name raw 0)
    (when (or (null name) (zerop (length name))
              (and (>= (length name) 2)
                   (string= "--" (subseq name 0 2))))
      (return-from %deftool-parse-define (values nil nil nil)))
    (multiple-value-bind (description after-desc)
        (%deftool-read-description raw after-name)
      (unless description
        (return-from %deftool-parse-define (values nil nil nil)))
      (let ((body (%deftool-read-body raw after-desc)))
        (when (or (null body) (zerop (length body)))
          (return-from %deftool-parse-define (values nil nil nil)))
        (values name description body)))))

(defun %deftool-eval-body (body-text)
  "Evaluate BODY-TEXT through sandboxed-eval. Returns
(values function nil) on success or (values nil error-message) on
failure."
  (multiple-value-bind (result errorp error-message)
      (sandboxed-eval body-text)
    (cond
      (errorp (values nil error-message))
      ((not (functionp result))
       (values nil (format nil "Body must evaluate to a function, got ~S." result)))
      (t (values result nil)))))

(defun %deftool-register (name description fn)
  (let* ((toolset (%ensure-toolset))
         (normalized (%tool-name-string name))
         (definition (pseudopod:make-tool-definition
                      :name normalized
                      :description (or description "")
                      :parameters nil
                      :fn (lambda (arguments &optional tool-call)
                            (declare (ignore tool-call))
                            (funcall fn arguments)))))
    (pseudopod:register-tool toolset definition)
    (setf (gethash normalized *tool-metadata*)
          (make-tool-metadata
           :name normalized
           :permission :supervised
           :dangerous-p nil
           :category :user-defined
           :timeout-seconds 30
           :source-file nil
           :source-line nil
           :parameter-specs '()
           :defined-at (get-universal-time)
           :mcp-server nil))
    normalized))

(defun %deftool-unregister (name)
  "Remove NAME from the toolset and metadata. Returns T if removed,
NIL if no such tool was present."
  (let* ((toolset (%ensure-toolset))
         (normalized (%tool-name-string name))
         (table (pseudopod::toolset-table toolset))
         (existed (nth-value 1 (gethash normalized table))))
    (when existed
      (remhash normalized table)
      (when (boundp '*tool-metadata*)
        (remhash normalized *tool-metadata*)))
    existed))

(defun %deftool-handle-undo (raw)
  (let ((name (string-trim '(#\Space #\Tab #\Newline #\Return) raw)))
    (cond
      ((zerop (length name))
       (%deftool-usage-result "Specify a tool name to undo."))
      (t
       (handler-case
           (if (%deftool-unregister name)
               (%deftool-result (format nil "Tool ~A unregistered." name))
               (%deftool-result (format nil "No tool named ~A is registered." name)))
         (error (condition)
           (%deftool-result (format nil "/deftool --undo error: ~A" condition))))))))

(defun %deftool-handle-define (raw)
  (multiple-value-bind (name description body) (%deftool-parse-define raw)
    (cond
      ((null name)
       (%deftool-usage-result "Could not parse /deftool arguments."))
      (t
       (multiple-value-bind (fn error-message) (%deftool-eval-body body)
         (cond
           (error-message
            (%deftool-result
             (format nil "/deftool body rejected: ~A" error-message)))
           (t
            (handler-case
                (let ((normalized (%deftool-register name description fn)))
                  (%deftool-result
                   (format nil "Tool ~A registered." normalized)))
              (error (condition)
                (%deftool-result
                 (format nil "/deftool registration error: ~A" condition)))))))))))

(defun %deftool-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((raw (or (gethash :ARGS arguments) ""))
         (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) raw)))
    (cond
      ((zerop (length trimmed))
       (%deftool-usage-result))
      ((or (%starts-with-ci-p "--undo " trimmed)
           (string-equal trimmed "--undo"))
       (%deftool-handle-undo
        (if (string-equal trimmed "--undo")
            ""
            (subseq trimmed (length "--undo ")))))
      (t
       (%deftool-handle-define trimmed)))))

(defun register-deftool-slash-command ()
  (register-slash-command
   (make-slash-command
    :name "deftool"
    :description
    "Define a new tool from chat (body must evaluate to a function of args)."
    :usage +deftool-usage+
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p t
           :greedy-p t
           :description
           "<name> \"<description>\" <body-form>  |  --undo <name>"))
    :handler #'%deftool-handler))
  t)
