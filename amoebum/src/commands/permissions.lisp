(in-package :amoebum.commands.permissions)

(defparameter +permissions-usage+
  "/permissions [stats|session [once|session|always]|reset [session|all]|log [limit]|explain [decision-id|latest]]")

(defparameter +permissions-session-scopes+
  '("once" "session" "always"))

(defparameter +permissions-actions+
  '("stats" "session" "reset" "log" "explain"))

(defstruct (permissions-command-context
            (:constructor make-permissions-command-context
                (tokens action)))
  tokens
  action)

(defun %permissions-invalid-usage (&optional detail)
  (make-slash-command-result
   :echo-input-p t
   :output (format nil "~@[~A~%~]Usage: ~A"
                   detail
                   +permissions-usage+)))

(defun %permissions-scope-keyword (token)
  (let ((normalized (and token (string-downcase token))))
    (cond
      ((null normalized) nil)
      ((string= normalized "once") :once)
      ((string= normalized "session") :session)
      ((string= normalized "always") :always)
      (t nil))))

(defun %permissions-format-path-approval-entry (entry)
  (let ((tool (or (amoebum::path-approval-entry-tool entry) "unknown"))
        (scope (or (amoebum::path-approval-entry-scope entry) :unknown))
        (path (or (amoebum::path-approval-entry-path entry) ""))
        (uses (amoebum::path-approval-entry-uses-remaining entry)))
    (format nil "- tool=~A scope=~(~A~) path=~A~@[ uses-remaining=~D~]"
            tool
            scope
            path
            uses)))

(defun %permissions-session-output (&optional scope)
  (let* ((entries (amoebum::list-path-approvals :scope scope))
         (scope-label (if scope
                          (string-downcase (symbol-name scope))
                          "all")))
    (if entries
        (with-output-to-string (out)
          (format out "Session path approvals (~A, ~D):~%" scope-label (length entries))
          (dolist (entry entries)
            (format out "~A~%" (%permissions-format-path-approval-entry entry))))
        (format nil "No session path approvals recorded (~A scope)." scope-label))))

(defun %permissions-format-trace (trace)
  (if (null trace)
      "No permission decision trace available."
      (with-output-to-string (out)
        (format out "decision-id=~A decision=~A mode=~A tool=~A~%"
                (getf trace :decision-id)
                (getf trace :decision)
                (getf trace :permission-mode)
                (getf trace :tool))
        (when (getf trace :path)
          (format out "  path: ~A~%" (getf trace :path)))
        (when (getf trace :command)
          (format out "  command: ~A~%" (getf trace :command)))
        (dolist (phase (getf trace :evaluation-trace))
          (format out "  phase=~A matched-rule-id=~A specificity=~A effect=~A cache=~A~%"
                  (getf phase :phase)
                  (or (getf phase :matched-rule-id) "none")
                  (or (getf phase :specificity) 0)
                  (or (getf phase :effect) :none)
                  (or (getf phase :cache) :n/a))))))

(defun %permissions-context-token (context position)
  (nth position (permissions-command-context-tokens context)))

(defun %permissions-handle-stats (_context)
  (let ((metrics (amoebum::permission-cache-metrics)))
    (make-slash-command-result
     :echo-input-p t
     :output (format nil "Permission cache: hits=~D misses=~D invalidations=~D rules-version=~D entries=~D"
                     (or (getf metrics :hits) 0)
                     (or (getf metrics :misses) 0)
                     (or (getf metrics :invalidations) 0)
                     (or (getf metrics :rules-version) 0)
                     (or (getf metrics :entries) 0)))))

(defun %permissions-handle-session (context)
  (let* ((scope-token (%permissions-context-token context 1))
         (scope (%permissions-scope-keyword scope-token)))
    (if (and scope-token (null scope))
        (%permissions-invalid-usage
         (format nil "Unknown /permissions session scope ~S. Expected once, session, or always."
                 scope-token))
        (make-slash-command-result
         :echo-input-p t
         :output (%permissions-session-output scope)))))

(defun %permissions-handle-reset (context)
  (let* ((target (string-downcase (or (%permissions-context-token context 1)
                                      "session")))
         (include-persistent (string= target "all")))
    (if (member target '("session" "all") :test #'string=)
        (let ((removed (amoebum::clear-path-approvals
                        :include-persistent include-persistent)))
          (make-slash-command-result
           :echo-input-p t
           :output (format nil
                           "Removed ~D path approval~:P (~A)."
                           removed
                           (if include-persistent
                               "session + persistent"
                               "session only"))))
        (%permissions-invalid-usage
         (format nil "Unknown /permissions reset target ~S. Expected session or all."
                 target)))))

(defun %permissions-log-limit (token)
  (if token
      (handler-case
          (max 1 (parse-integer token))
        (error ()
          nil))
      5))

(defun %permissions-handle-log (context)
  (let* ((limit-token (%permissions-context-token context 1))
         (limit (%permissions-log-limit limit-token))
         (entries (and limit (amoebum::permission-decision-history :limit limit))))
    (if (null limit)
        (%permissions-invalid-usage (format nil "Invalid log limit ~S." limit-token))
        (make-slash-command-result
         :echo-input-p t
         :output (if entries
                     (with-output-to-string (out)
                       (format out "Permission decisions (~D):~%" (length entries))
                       (dolist (entry entries)
                         (format out "- ~A~%" (%permissions-format-trace entry))))
                     "No permission decisions recorded yet.")))))

(defun %permissions-handle-explain (context)
  (let* ((decision-id (or (%permissions-context-token context 1) "latest"))
         (payload (amoebum::explain-permission-decision :decision-id decision-id)))
    (if payload
        (make-slash-command-result
         :echo-input-p t
         :output (with-output-to-string (out)
                   (format out "Historical:~%~A~%Replay:~%~A"
                           (%permissions-format-trace (getf payload :historical))
                           (%permissions-format-trace (getf payload :replay))))
         :payload payload)
        (make-slash-command-result
         :echo-input-p t
         :output (format nil "No decision trace found for ~A." decision-id)))))

(defparameter +permissions-command-dispatch+
  '(("stats" . %permissions-handle-stats)
    ("status" . %permissions-handle-stats)
    ("session" . %permissions-handle-session)
    ("list" . %permissions-handle-session)
    ("reset" . %permissions-handle-reset)
    ("clear" . %permissions-handle-reset)
    ("log" . %permissions-handle-log)
    ("explain" . %permissions-handle-explain)))

(defun %permissions-dispatch-handler (action)
  (cdr (assoc action +permissions-command-dispatch+ :test #'string=)))

(defun %permissions-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((tokens (amoebum::%tokenize-command-arguments
                  (or (gethash :ARGS arguments) "")))
         (action (if tokens (string-downcase (first tokens)) "stats"))
         (context (make-permissions-command-context tokens action))
         (handler (%permissions-dispatch-handler action)))
    (if handler
        (funcall handler context)
        (%permissions-invalid-usage
         (format nil "Unknown /permissions action ~S."
                 (permissions-command-context-action context))))))

(defun %permissions-arg-completer (_command _invocation index fragment prefix-tokens)
  (declare (ignore _command _invocation))
  (let ((prefix (amoebum::%slash-trim fragment))
        (action (and prefix-tokens (string-downcase (first prefix-tokens)))))
    (cond
      ((= index 0)
       (loop for option in +permissions-actions+
             when (amoebum::%starts-with-ci-p prefix option)
               collect option))
      ((and (string= action "session") (= index 1))
       (loop for option in +permissions-session-scopes+
             when (amoebum::%starts-with-ci-p prefix option)
               collect option))
      ((and (string= action "reset") (= index 1))
       (loop for option in '("session" "all")
             when (amoebum::%starts-with-ci-p prefix option)
               collect option))
      ((and (string= action "explain") (= index 1))
       (let* ((entries (amoebum::permission-decision-history :limit 20))
              (ids (cons "latest"
                         (remove nil
                                 (mapcar (lambda (entry)
                                           (getf entry :decision-id))
                                         entries)))))
         (loop for option in ids
               when (amoebum::%starts-with-ci-p prefix option)
                 collect option)))
      (t nil))))
