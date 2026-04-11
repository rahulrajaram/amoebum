(in-package :amoebum)

(defvar *model-router* nil
  "Global model router instance, set during configuration.")

(defparameter +cost-default-interaction-count+ 5)

(defun %models-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let ((filter (gethash :PROVIDER arguments)))
    (if *model-router*
        (let ((status (pseudopod:router-status *model-router*)))
          (with-output-to-string (out)
            (format out "Router strategy: ~A~%Providers: ~A healthy / ~A total~%"
                    (getf status :strategy)
                    (getf status :healthy-providers)
                    (getf status :total-providers))
            (dolist (p (getf status :providers))
              (when (or (null filter)
                        (string-equal filter (getf p :name)))
                (format out "  ~A (~A) ~:[UNHEALTHY~;OK~] — ~A reqs, ~A errs, ~Ams~%"
                        (getf p :name) (getf p :model) (getf p :healthy)
                        (getf p :requests) (getf p :errors) (getf p :last-latency-ms))))))
        (make-slash-command-result :output "No model router configured. Set up providers in config."))))

(defun %providers-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((raw-action (gethash :ACTION arguments))
         (action (and (stringp raw-action)
                      (string-downcase (%slash-trim raw-action)))))
    (make-slash-command-result
     :echo-input-p t
     :action :toggle-provider-dashboard
     :payload (cond
                ((string-equal action "on") :on)
                ((string-equal action "off") :off)
                (t :toggle)))))

(defun %slash-message-text (message)
  (cond
    ((typep message 'pseudopod:message)
     (with-output-to-string (out)
       (dolist (part (pseudopod:message-content message))
         (when (and (pseudopod:content-part-p part)
                    (stringp (pseudopod:content-part-text part)))
           (write-string (pseudopod:content-part-text part) out)
           (write-char #\Space out)))))
    ((stringp message)
     message)
    (t
     (princ-to-string message))))

(defun %cost-recent-interactions (messages count)
  (let ((all '())
        (pending-user nil))
    (dolist (message messages)
      (when (typep message 'pseudopod:message)
        (let ((role (string-downcase (or (pseudopod:message-role message) ""))))
          (cond
            ((string= role "user") (setf pending-user message))
            ((and (string= role "assistant") pending-user)
             (push (list :user pending-user :assistant message) all)
             (setf pending-user nil))))))
    (let ((ordered (nreverse all)))
      (if (<= (length ordered) count)
          ordered
          (subseq ordered (- (length ordered) count))))))

(defun %format-usd (amount)
  (format nil "$~,6F" (coerce amount 'double-float)))

(defun %cost-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((chat-state (slash-command-context-chat-state context))
         (raw-count (gethash :COUNT arguments))
         (count (max 1 (or raw-count +cost-default-interaction-count+))))
    (cond
      ((null *model-router*)
       (make-slash-command-result :output "No model router configured. Set up providers in config."))
      ((not (typep chat-state 'chat-ui-state))
       (make-slash-command-result :output "Cost estimation requires an active chat session."))
      (t
       (let* ((messages (chat-ui-state-messages chat-state))
              (interactions (%cost-recent-interactions messages count)))
         (if (null interactions)
             (make-slash-command-result :output "No completed user/assistant interactions to estimate yet.")
             (let ((rows '())
                   (total 0.0d0)
                   (index 0))
               (dolist (interaction interactions)
                 (incf index)
                 (let* ((user-message (getf interaction :user))
                        (assistant-message (getf interaction :assistant))
                        (input-messages (list user-message))
                        (provider (or (pseudopod:router-select-provider
                                       *model-router*
                                       :messages input-messages)
                                      (first (pseudopod:model-router-providers *model-router*))))
                        (assistant-tokens
                          (if provider
                              (max 0
                                   (pseudopod:estimate-provider-tokens
                                    provider
                                    (%slash-message-text assistant-message)))
                              0))
                        (estimate
                          (if provider
                              (pseudopod:cost-estimate provider input-messages :output-tokens assistant-tokens)
                              (list :total-cost-usd 0.0d0 :input-tokens 0 :output-tokens assistant-tokens :provider "n/a" :model "n/a"))))
                   (incf total (getf estimate :total-cost-usd 0.0d0))
                   (push (format nil "~D. ~A (~A): in ~D tok, out ~D tok -> ~A"
                                 index
                                 (getf estimate :provider "n/a")
                                 (getf estimate :model "n/a")
                                 (getf estimate :input-tokens 0)
                                 (getf estimate :output-tokens 0)
                                 (%format-usd (getf estimate :total-cost-usd 0.0d0)))
                         rows)))
               (make-slash-command-result
                :output (with-output-to-string (out)
                          (format out "Estimated cost for last ~D interaction~:P:~%" (length interactions))
                          (dolist (row (nreverse rows))
                            (format out "~A~%" row))
                          (format out "Total: ~A" (%format-usd total)))))))))))

(defun %image-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((args-text (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments args-text))
         (action (string-downcase (or (first tokens) "list"))))
    (cond
      ((string= action "save")
       (make-slash-command-result :output "Image save requested. Use save-amoebum-image for actual persistence."))
      ((string= action "restore")
       (make-slash-command-result :output "Image restore requested. Specify path to restore from."))
      ((string= action "rotate")
       (make-slash-command-result :output "Image rotation: keeping latest 5 images."))
      (t
       (make-slash-command-result :output "Image management: /image save|restore|list|rotate")))))

(defun %extensions-asdf-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((args-text (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments args-text))
         (action (string-downcase (or (first tokens) "list"))))
    (cond
      ((string= action "discover")
       (make-slash-command-result :output "Discovering ASDF extensions in Quicklisp local-projects and ~/.amoebum/systems/..."))
      ((string= action "load")
       (let ((system-name (second tokens)))
         (if system-name
             (make-slash-command-result :output (format nil "Loading ASDF extension: ~A" system-name))
             (make-slash-command-result :output "Usage: /extensions-asdf load <system-name>"))))
      ((string= action "unload")
       (let ((system-name (second tokens)))
         (if system-name
             (make-slash-command-result :output (format nil "Unloading ASDF extension: ~A" system-name))
             (make-slash-command-result :output "Usage: /extensions-asdf unload <system-name>"))))
      (t
       (make-slash-command-result :output "ASDF extensions: /extensions-asdf list|load|unload|discover")))))

(defun %perf-handler (_invocation arguments _context)
  (%profile-handler _invocation arguments _context))

(defun %profile-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((args-text (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments args-text))
         (action (string-downcase (or (first tokens) "report"))))
    (cond
      ((string= action "start")
       (start-profiling)
       (make-slash-command-result :output "Profiling started. Run /profile stop to capture a report."))
      ((string= action "stop")
       (let ((report (stop-profiling)))
         (make-slash-command-result :output (render-profiling-report-table :report report))))
      ((string= action "report")
       (make-slash-command-result :output (render-profiling-report-table :report (report-profiling))))
      (t
       (make-slash-command-result :output "Profiling: /profile start|stop|report")))))

(defun %mcp-status-tool-count (server-name)
  (let ((normalized (string-downcase (%slash-trim (princ-to-string server-name))))
        (count 0))
    (maphash (lambda (_name binding)
               (declare (ignore _name))
               (when (string= (mcp-tool-binding-server-name binding) normalized)
                 (incf count)))
             *mcp-tool-binding-registry*)
    count))

(defun %mcp-status-handler (_invocation _arguments _context)
  (declare (ignore _invocation _arguments _context))
  (let ((servers '()))
    (maphash (lambda (_name server)
               (declare (ignore _name))
               (push server servers))
             *mcp-tool-server-registry*)
    (if (null servers)
        (make-slash-command-result :echo-input-p t :output "No MCP servers are currently registered.")
        (make-slash-command-result
         :echo-input-p t
         :output
         (with-output-to-string (out)
           (format out "MCP servers: ~D~%" (length servers))
           (dolist (server (sort (copy-list servers) #'string< :key #'mcp-server-name))
             (let* ((info (mcp-server-server-info server))
                    (status (if (mcp-server-running-p server) "running" "stopped"))
                    (protocol (or (and info (mcp-server-info-protocol-version info)) "unknown"))
                    (match (if (and info (mcp-server-info-protocol-version-match-p info)) "match" "mismatch"))
                    (capabilities (or (mcp-server-capability-summary server) '()))
                    (declared-count (length (or (and info (mcp-server-info-declared-tools info)) '())))
                    (discovered-count (%mcp-status-tool-count (mcp-server-name server))))
               (format out
                       "- ~A (~A) protocol=~A [~A] capabilities=~:[none~;~{~A~^, ~}~] declared-tools=~D discovered-tools=~D~%"
                       (mcp-server-name server)
                       status
                       protocol
                       match
                       capabilities
                       capabilities
                       declared-count
                       discovered-count))))))))

(defun %format-cultivar-status-timestamp (timestamp)
  (if (and (integerp timestamp) (plusp timestamp))
      (multiple-value-bind (second minute hour day month year)
          (decode-universal-time timestamp)
        (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                year month day hour minute second))
      "never"))

(defun %cultivar-status-adapter ()
  (or *cultivar-tool-adapter*
      *cultivar-adapter*
      (setf *cultivar-adapter*
            (make-cultivar-adapter :enabled-p t
                                   :daemon-mode :prefer
                                   :daemon-auto-start-p t))))

(defun %cultivar-status-last-slice-line (last-slice)
  (if (not (listp last-slice))
      "Latest slice: none recorded yet."
      (format nil
              "Latest slice: ~A origin=~(~A~) digest=~A materialized=~:[no~;yes~] kind=~A query=~@[~A:~D:~D~] at ~A~@[ warning=~A~]~@[ notes=~{~A~^, ~}~]"
              (or (getf last-slice :symbol-id) "unknown")
              (or (getf last-slice :origin) :unknown)
              (or (getf last-slice :results-digest) "unknown")
              (getf last-slice :served-from-materialization)
              (or (getf last-slice :materialization-kind) "direct")
              (getf last-slice :query-file)
              (getf last-slice :query-line)
              (getf last-slice :query-col)
              (%format-cultivar-status-timestamp
               (getf last-slice :recorded-at))
              (getf last-slice :quality-warning)
              (let ((notes (getf last-slice :notes)))
                (and (listp notes) notes)))))

(defun %cultivar-handler (_invocation _arguments _context)
  (declare (ignore _invocation _arguments _context))
  (let* ((adapter (%cultivar-status-adapter))
         (status (cultivar-daemon-status adapter))
         (last-slice (getf status :last-slice)))
    (make-slash-command-result
     :echo-input-p t
     :output
     (with-output-to-string (out)
       (format out
               "Cultivar daemon: mode=~(~A~) auto-start=~:[off~;on~] running=~:[no~;yes~] socket=~A~%"
               (or (getf status :mode) :unknown)
               (getf status :auto-start-p)
               (getf status :running-p)
               (or (getf status :socket-path) "unknown"))
       (format out
               "Paths: root=~A index=~A~%"
               (or (getf status :root-path) "unknown")
               (or (getf status :index-path) "unknown"))
       (format out
               "Last start: status=~A at ~A~@[ reason=~A~]~%"
               (or (getf status :last-start-status) "none")
               (%format-cultivar-status-timestamp
                (getf status :last-start-at))
               (getf status :last-start-reason))
       (write-string (%cultivar-status-last-slice-line last-slice) out)))))

(defun %mcp-auth-usage ()
  "/mcp-auth [list|set <server|default> <allow|deny|prompt>|clear <server|all>]")

(defun %mcp-auth-string (value)
  (cond
    ((null value) "")
    ((stringp value) value)
    ((symbolp value) (symbol-name value))
    (t (princ-to-string value))))

(defun %mcp-auth-normalize-server (value)
  (let ((trimmed (string-downcase (%slash-trim (%mcp-auth-string value)))))
    (cond
      ((or (string= trimmed "") (string= trimmed "*") (string= trimmed "default")) "default")
      ((uiop:string-prefix-p "mcp/" trimmed)
       (let* ((rest (subseq trimmed (length "mcp/")))
              (separator (position #\/ rest)))
         (if (and separator (> separator 0))
             (subseq rest 0 separator)
             rest)))
      (t trimmed))))

(defun %mcp-auth-normalize-decision (value)
  (let ((trimmed (string-downcase (%slash-trim (%mcp-auth-string value)))))
    (cond
      ((string= trimmed "allow") :allow)
      ((string= trimmed "deny") :deny)
      ((or (string= trimmed "prompt") (string= trimmed "ask")) :prompt)
      (t nil))))

(defun %mcp-auth-config-pairs (value)
  (cond
    ((hash-table-p value)
     (loop for key being the hash-keys of value using (hash-value decision)
           collect (cons key decision)))
    ((and (listp value) (every #'consp value))
     value)
    ((and (listp value) (evenp (length value)))
     (loop for (key decision) on value by #'cddr collect (cons key decision)))
    (t nil)))

(defun %mcp-auth-normalized-rules ()
  (let* ((raw (cfg :mcp-server-permissions))
         (rules '()))
    (dolist (entry (%mcp-auth-config-pairs raw))
      (let* ((server (%mcp-auth-normalize-server (car entry)))
             (decision (%mcp-auth-normalize-decision (cdr entry))))
        (when (and decision server (not (assoc server rules :test #'string=)))
          (push (cons server decision) rules))))
    (nreverse rules)))

(defun %mcp-auth-render-rules (&optional (rules (%mcp-auth-normalized-rules)))
  (with-output-to-string (out)
    (let ((default (or (cdr (assoc "default" rules :test #'string=)) :prompt)))
      (format out "MCP authorization rules:~%")
      (format out "- default: ~A~%" (string-downcase (symbol-name default)))
      (dolist (entry (sort (remove-if (lambda (entry) (string= (car entry) "default"))
                                      (copy-list rules))
                           #'string< :key #'car))
        (format out "- ~A: ~A~%"
                (car entry)
                (string-downcase (symbol-name (cdr entry))))))))

(defun %mcp-auth-upsert-rule (rules server decision)
  (let* ((normalized-server (%mcp-auth-normalize-server server))
         (existing (assoc normalized-server rules :test #'string=)))
    (if existing
        (setf (cdr existing) decision)
        (push (cons normalized-server decision) rules))
    rules))

(defun %mcp-auth-remove-rule (rules server)
  (let ((normalized-server (%mcp-auth-normalize-server server)))
    (remove-if (lambda (entry) (string= (car entry) normalized-server)) rules)))

(defun %mcp-auth-known-server-names ()
  (let ((names '()))
    (dolist (entry (%mcp-auth-normalized-rules))
      (unless (string= (car entry) "default")
        (pushnew (car entry) names :test #'string=)))
    (sort names #'string<)))

(defun %mcp-auth-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((raw (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments raw))
         (action (if tokens (string-downcase (first tokens)) "list")))
    (labels ((invalid-usage (&optional detail)
               (make-slash-command-result
                :echo-input-p t
                :output (format nil "~@[~A~%~]Usage: ~A" detail (%mcp-auth-usage)))))
      (cond
        ((member action '("list" "ls") :test #'string=)
         (if (> (length tokens) 1)
             (invalid-usage (format nil "Unexpected argument ~S." (second tokens)))
             (make-slash-command-result :echo-input-p t :output (%mcp-auth-render-rules))))
        ((string= action "set")
         (let* ((server-token (second tokens))
                (decision-token (third tokens))
                (extra (fourth tokens))
                (server (%mcp-auth-normalize-server server-token))
                (decision (%mcp-auth-normalize-decision decision-token)))
           (cond
             (extra (invalid-usage (format nil "Unexpected argument ~S." extra)))
             ((or (null server-token) (%slash-blank-p server-token))
              (invalid-usage "Missing server token for set action."))
             ((null decision)
              (invalid-usage (format nil "Unknown MCP decision ~S." decision-token)))
             (t
              (let* ((updated (%mcp-auth-upsert-rule (%mcp-auth-normalized-rules) server decision))
                     (next-rules (sort (copy-list updated) #'string< :key #'car)))
                (setconfig :mcp-server-permissions next-rules)
                (make-slash-command-result
                 :echo-input-p t
                 :output (format nil "Set MCP authorization for ~A to ~A."
                                 server
                                 (string-downcase (symbol-name decision)))))))))
        ((string= action "clear")
         (let ((target (second tokens))
               (extra (third tokens)))
           (cond
             (extra (invalid-usage (format nil "Unexpected argument ~S." extra)))
             ((or (null target) (%slash-blank-p target))
              (invalid-usage "Missing server token for clear action."))
             ((member (string-downcase (%slash-trim target)) '("all" "*") :test #'string=)
              (setconfig :mcp-server-permissions nil)
              (make-slash-command-result :echo-input-p t :output "Cleared all MCP authorization overrides (default prompt)."))
             (t
              (let* ((server (%mcp-auth-normalize-server target))
                     (updated (%mcp-auth-remove-rule (%mcp-auth-normalized-rules) server)))
                (setconfig :mcp-server-permissions updated)
                (make-slash-command-result
                 :echo-input-p t
                 :output (format nil "Cleared MCP authorization override for ~A." server)))))))
        (t
         (invalid-usage (format nil "Unknown /mcp-auth action ~S." action)))))))

(defun %mcp-auth-arg-completer (_command _invocation index fragment prefix-tokens)
  (declare (ignore _command _invocation))
  (let ((prefix (%slash-trim fragment))
        (action (and prefix-tokens (string-downcase (first prefix-tokens)))))
    (cond
      ((= index 0)
       (loop for option in '("list" "set" "clear")
             when (%starts-with-ci-p prefix option)
               collect option))
      ((and (string= action "set") (= index 1))
       (loop for option in (append '("default") (%mcp-auth-known-server-names))
             when (%starts-with-ci-p prefix option)
               collect option))
      ((and (string= action "set") (= index 2))
       (loop for option in '("allow" "deny" "prompt")
             when (%starts-with-ci-p prefix option)
               collect option))
      ((and (string= action "clear") (= index 1))
       (loop for option in (append '("all" "default") (%mcp-auth-known-server-names))
             when (%starts-with-ci-p prefix option)
               collect option))
      (t nil))))

(defun %slash-approval-policy-keyword (token)
  (let ((candidate (intern (string-upcase (%slash-trim token)) :keyword)))
    (when (member candidate *known-approval-policies* :test #'eq)
      candidate)))

(defun %approvals-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((args-text (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments args-text))
         (action (string-downcase (or (first tokens) "status")))
         (policy-token (second tokens))
         (current-policy (cfg :approval-policy)))
    (cond
      ((or (string= action "status") (string= action "list"))
       (make-slash-command-result
        :output (format nil
                        "Approval policy: ~A (presets: untrusted, on-failure, on-request, never)."
                        (string-downcase (symbol-name (or current-policy :on-request))))))
      ((string= action "set")
       (if (null policy-token)
           (make-slash-command-result :output "Usage: /approvals set <untrusted|on-failure|on-request|never>")
           (let ((normalized (%slash-approval-policy-keyword policy-token)))
             (if (member normalized *known-approval-policies* :test #'eq)
                 (progn
                   (setconfig :approval-policy normalized)
                   (make-slash-command-result
                    :output (format nil "Approval policy set to ~A."
                                    (string-downcase (symbol-name normalized)))))
                 (make-slash-command-result
                  :output (format nil
                                  "Unknown approval policy ~S. Valid values: untrusted, on-failure, on-request, never."
                                  policy-token))))))
      (t
       (make-slash-command-result :output "Approvals: /approvals status | /approvals set <policy>")))))

(defun register-phase5-slash-commands ()
  (register-slash-command
   (make-slash-command :name "mcp-status" :description "Show MCP server negotiation state, capabilities, and discovered tool counts." :usage "/mcp-status" :handler #'%mcp-status-handler))
  (register-slash-command
   (make-slash-command :name "cultivar" :description "Show Cultivar daemon warmth and latest structured slice provenance." :usage "/cultivar" :handler #'%cultivar-handler))
  (register-slash-command
   (make-slash-command
    :name "mcp-auth"
    :description "Inspect or update MCP per-server authorization decisions."
    :usage (%mcp-auth-usage)
    :parameters (list (make-slash-command-parameter :name "args" :type :string :required-p nil :greedy-p t :description "Optional action."))
    :handler #'%mcp-auth-handler
    :completer #'%mcp-auth-arg-completer))
  (register-slash-command
   (make-slash-command :name "models" :description "List configured providers, their models, and health status." :usage "/models [provider-name]" :parameters (list (make-slash-command-parameter :name "provider" :type :string :required-p nil :description "Optional provider name to inspect.")) :handler #'%models-handler))
  (register-slash-command
   (make-slash-command :name "providers" :description "Toggle provider health dashboard visibility." :usage "/providers [on|off]" :parameters (list (make-slash-command-parameter :name "action" :type :string :required-p nil :description "Optional on/off to explicitly set visibility.")) :handler #'%providers-handler))
  (register-slash-command
   (make-slash-command :name "cost" :description "Estimate cost of the most recent chat interactions." :usage "/cost [count]" :parameters (list (make-slash-command-parameter :name "count" :type :integer :required-p nil :description "Number of recent interactions to estimate (default 5).")) :handler #'%cost-handler))
  (register-slash-command
   (make-slash-command :name "image" :description "Save or restore a Lisp image snapshot." :usage "/image [save [path]|restore [path]|list|rotate]" :parameters (list (make-slash-command-parameter :name "args" :type :string :required-p nil :greedy-p t :description "Action: save, restore, list, or rotate.")) :handler #'%image-handler))
  (register-slash-command
   (make-slash-command :name "extensions-asdf" :description "Manage ASDF-based extensions: discover, load, unload." :usage "/extensions-asdf [list|load <system>|unload <system>|discover]" :parameters (list (make-slash-command-parameter :name "args" :type :string :required-p nil :greedy-p t :description "Subcommand and optional system name.")) :handler #'%extensions-asdf-handler))
  (register-slash-command
   (make-slash-command :name "perf" :description "Backward-compatible alias for /profile commands." :usage "/perf [start|stop|report]" :parameters (list (make-slash-command-parameter :name "args" :type :string :required-p nil :greedy-p t :description "Subcommand for profiling.")) :handler #'%perf-handler))
  (register-slash-command
   (make-slash-command :name "profile" :description "Control SBCL statistical profiling and show reports." :usage "/profile [start|stop|report]" :parameters (list (make-slash-command-parameter :name "args" :type :string :required-p nil :greedy-p t :description "Subcommand for profiling.")) :handler #'%profile-handler))
  (register-slash-command
   (make-slash-command :name "approvals" :description "Inspect or set approval policy presets." :usage "/approvals [status|set <untrusted|on-failure|on-request|never>]" :parameters (list (make-slash-command-parameter :name "args" :type :string :required-p nil :greedy-p t :description "Optional action: status or set <policy>.")) :handler #'%approvals-handler))
  t)
