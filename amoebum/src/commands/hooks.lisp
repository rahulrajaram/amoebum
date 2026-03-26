(in-package :amoebum)

(defun %hooks-usage ()
  "/hooks [list [hook-point]] | /hooks trace [limit] [hook-point]")

(defun %hook-point-name (hook-point)
  (string-downcase (subseq (symbol-name hook-point) 1)))

(defun %hook-point-definitions ()
  (let ((symbol (find-symbol "+HOOK-POINT-DEFINITIONS+" :amoebum)))
    (if (and symbol (boundp symbol))
        (symbol-value symbol)
        '())))

(defun %parse-hook-point-token (token)
  (when (and token (plusp (length (%slash-trim token))))
    (let ((candidate (intern (string-upcase (%slash-trim token)) :keyword)))
      (and (assoc candidate (%hook-point-definitions) :test #'eq)
           candidate))))

(defun %hook-point-token-completions (fragment)
  (let ((prefix (%slash-trim fragment)))
    (loop for spec in (%hook-point-definitions)
          for point = (car spec)
          for text = (%hook-point-name point)
          when (%starts-with-ci-p prefix text)
            collect text)))

(defun %render-hooks-list (&optional hook-point)
  (let ((entries (if hook-point
                     (list-hooks hook-point)
                     (list-hooks))))
    (if (null entries)
        (if hook-point
            (format nil "No hooks registered for ~A." (%hook-point-name hook-point))
            "No hooks registered.")
        (with-output-to-string (out)
          (format out "Registered hooks (~D):~%" (length entries))
          (dolist (entry entries)
            (format out "- ~A (~S) point=~A priority=~D async=~:[no~;yes~] enabled=~:[no~;yes~] on-error=~A budget=~Dms failures=~D/~D calls=~D total=~Dms~%"
                    (hook-entry-hook-id entry)
                    (hook-entry-hook-id entry)
                    (%hook-point-name (hook-entry-hook-point entry))
                    (hook-entry-priority entry)
                    (hook-entry-async-p entry)
                    (not (hook-entry-disabled-p entry))
                    (hook-entry-on-error entry)
                    (hook-entry-max-ms entry)
                    (hook-entry-consecutive-failures entry)
                    (hook-entry-failure-threshold entry)
                    (hook-entry-call-count entry)
                    (hook-entry-total-time-ms entry)))))))

(defun %render-hook-trace (&key (limit 20) hook-point)
  (let ((entries (hook-trace :limit limit :hook-point hook-point)))
    (if (null entries)
        "Hook trace is empty."
        (with-output-to-string (out)
          (format out "Hook trace (~D, newest first):~%" (length entries))
          (dolist (entry entries)
            (format out "- t=~D point=~A hook=~S status=~A elapsed=~Dms result=~S~@[ detail=~A~]~%"
                    (or (getf entry :timestamp) 0)
                    (%hook-point-name (getf entry :hook-point))
                    (getf entry :hook-id)
                    (getf entry :status)
                    (or (getf entry :elapsed-ms) 0)
                    (getf entry :result)
                    (getf entry :detail)))))))

(defun %hooks-invalid-usage (&optional details)
  (make-slash-command-result
   :echo-input-p t
   :output (format nil "~@[~A~%~]Usage: ~A"
                   details
                   (%hooks-usage))))

(defun %hooks-parse-trace-args (tokens)
  (let ((limit 20)
        (hook-point nil))
    (dolist (token (rest tokens))
      (cond
        ((ignore-errors (parse-integer token))
         (setf limit (parse-integer token)))
        ((%parse-hook-point-token token)
         (setf hook-point (%parse-hook-point-token token)))
        (t
         (return-from %hooks-parse-trace-args
           (values nil nil (format nil "Unrecognized trace argument ~S." token))))))
    (if (<= limit 0)
        (values nil nil (format nil "Trace limit must be positive, got ~S." limit))
        (values limit hook-point nil))))

(defun %hooks-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((raw (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments raw))
         (action-token (and tokens (string-downcase (first tokens)))))
    (cond
      ((or (null action-token) (string= action-token "list"))
       (let* ((point-token (and (> (length tokens) 1) (second tokens)))
              (extra-token (and (> (length tokens) 2) (third tokens))))
         (cond
           (extra-token
            (%hooks-invalid-usage (format nil "Unexpected extra token ~S." extra-token)))
           ((and point-token (null (%parse-hook-point-token point-token)))
            (%hooks-invalid-usage (format nil "Unknown hook-point ~S." point-token)))
           (t
            (make-slash-command-result
             :echo-input-p t
             :output (%render-hooks-list (%parse-hook-point-token point-token)))))))
      ((string= action-token "trace")
       (multiple-value-bind (limit hook-point error-text)
           (%hooks-parse-trace-args tokens)
         (if error-text
             (%hooks-invalid-usage error-text)
             (make-slash-command-result
              :echo-input-p t
              :output (%render-hook-trace :limit limit :hook-point hook-point)))))
      ((%parse-hook-point-token action-token)
       (make-slash-command-result
        :echo-input-p t
        :output (%render-hooks-list (%parse-hook-point-token action-token))))
      (t
       (%hooks-invalid-usage (format nil "Unknown /hooks action ~S." action-token))))))

(defun %hooks-arg-completer (_command _invocation index fragment prefix-tokens)
  (declare (ignore _command _invocation))
  (let ((head (and prefix-tokens (string-downcase (first prefix-tokens)))))
    (cond
      ((= index 0)
       (append (loop for option in '("list" "trace")
                     when (%starts-with-ci-p (%slash-trim fragment) option)
                       collect option)
               (%hook-point-token-completions fragment)))
      ((and (string= head "trace") (= index 1))
       (append (loop for option in '("10" "20" "50")
                     when (%starts-with-ci-p (%slash-trim fragment) option)
                       collect option)
               (%hook-point-token-completions fragment)))
      ((and (string= head "trace") (= index 2))
       (%hook-point-token-completions fragment))
      ((and (string= head "list") (= index 1))
       (%hook-point-token-completions fragment))
      (t
       nil))))

(defun register-hook-slash-commands ()
  (register-slash-command
   (make-slash-command
    :name "hooks"
    :description "Inspect hook registration state and recent hook trace events."
    :usage (%hooks-usage)
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional subcommand and arguments."))
    :handler #'%hooks-handler
    :completer #'%hooks-arg-completer))
  t)
