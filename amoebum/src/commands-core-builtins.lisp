(in-package :amoebum)

(defun %help-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let ((topic (gethash :TOPIC arguments)))
    (make-slash-command-result
     :output (if (and topic (plusp (length (%slash-trim topic))))
                 (%help-for-command topic)
                 (%help-listing))
     :echo-input-p t)))

(defun %current-config-safe ()
  (or (ignore-errors (current-config))
      (load-config)))

(defun %mode-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((cfg (or (slash-command-context-config context)
                  (%current-config-safe)))
         (mode (gethash :MODE arguments)))
    (if mode
        (let ((next (setconfig :permission-mode mode)))
          (make-slash-command-result
           :output (format nil "Permission mode set to ~A."
                           (string-downcase (symbol-name next)))))
        (make-slash-command-result
         :output (format nil "Current permission mode: ~A."
                         (string-downcase
                          (symbol-name (config-permission-mode cfg))))))))

(defun %model-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((cfg (or (slash-command-context-config context)
                  (%current-config-safe)))
         (model (gethash :MODEL arguments)))
    (if (and model (plusp (length (%slash-trim model))))
        (let ((next (setconfig :model model)))
          (make-slash-command-result
           :output (format nil "Model set to ~A." next)))
        (make-slash-command-result
         :output (format nil "Current model: ~A." (config-model cfg))))))

(defun %maxturns-handler (_invocation arguments context)
  "Handle /maxturns command to get/set per-conversation max agentic iterations."
  (declare (ignore _invocation))
  (let* ((chat-state (slash-command-context-chat-state context))
         (global-default +max-agentic-iterations+)
         (current-override (and chat-state
                                (chat-ui-state-max-agentic-iterations-override chat-state)))
         (current-effective (if chat-state
                                (%chat-effective-max-iterations chat-state)
                                global-default))
         (raw-value (gethash :LIMIT arguments))
         (new-value (and raw-value
                         (stringp raw-value)
                         (%slash-trim raw-value))))
    (cond
      ;; Setting a new value
      (new-value
       (cond
         ((string-equal new-value "reset")
          (when chat-state
            (setf (chat-ui-state-max-agentic-iterations-override chat-state) nil))
          (make-slash-command-result
           :output (format nil "Max agentic iterations reset to global default (~A)." global-default)))
         (t
          (handler-case
              (let ((n (parse-integer new-value)))
                (if (plusp n)
                    (progn
                      (when chat-state
                        (setf (chat-ui-state-max-agentic-iterations-override chat-state) n))
                      (make-slash-command-result
                       :output (format nil "Max agentic iterations set to ~A for this conversation.~%~A"
                                       n
                                       (if (> n global-default)
                                           (format nil "Note: This exceeds the global default (~A)." global-default)
                                           ""))))
                    (make-slash-command-result
                     :output "Value must be a positive integer.")))
            (error ()
              (make-slash-command-result
               :output "Invalid value. Use a positive integer or 'reset'."))))))
      ;; Just querying current value
      (t
       (make-slash-command-result
        :output (with-output-to-string (out)
                  (format out "Max agentic iterations for this conversation: ~A~%"
                          current-effective)
                  (format out "Global default: ~A~%" global-default)
                  (when current-override
                    (format out "(Conversation override active)~%"))
                  (format out "~%Usage: /maxturns <number> - set limit for this conversation~%")
                  (format out "       /maxturns reset   - revert to global default~%")))))))

(defun %config-key-sort-token (key)
  (string-downcase
   (cond
     ((keywordp key) (symbol-name key))
     ((symbolp key) (symbol-name key))
     (t (princ-to-string key)))))

(defun %config-layer-label (layer)
  (case layer
    (:built-in "built-in")
    (:global "global")
    (:project "project")
    (:directory "directory")
    (:env "env")
    (:cli "cli")
    (:runtime "runtime")
    (otherwise "unknown")))

(defun %config-report-output (cfg)
  (let ((keys (sort (loop for key being the hash-keys of (config-values cfg)
                          collect key)
                    #'string<
                    :key #'%config-key-sort-token)))
    (with-output-to-string (out)
      (format out "Configuration values:~%")
      (dolist (key keys)
        (format out "- ~A = ~S (source: ~A)~%"
                (%config-key-sort-token key)
                (config-value key cfg)
                (%config-layer-label (config-layer-source key cfg)))))))

(defun %config-handler (_invocation _arguments context)
  (declare (ignore _invocation _arguments))
  (let ((cfg (or (slash-command-context-config context)
                 (%current-config-safe))))
    (make-slash-command-result
     :output (%config-report-output cfg)
     :echo-input-p t)))

(defun %execute-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((args-text (or (and (hash-table-p arguments) (gethash :ARGS arguments)) ""))
         (interactive-p (or (search "--interactive" args-text)
                            (search "-i" args-text)))
         (plan-state (current-plan-mode-state))
         (active-p (plan-mode-active-p plan-state))
         (captured-plan (plan-mode-state-last-plan-markdown plan-state))
         (available-step-indexes (plan-step-indexes plan-state))
         (approved-step-indexes (or (plan-mode-state-approved-step-indexes plan-state) '()))
         (review-decision (plan-mode-state-review-decision plan-state)))
    (when active-p
      (multiple-value-bind (_ output-path)
          (exit-plan-mode :state plan-state
                          :reason :execute-transition
                          :write-output-p t)
        (declare (ignore _ output-path))
        (setconfig :plan-mode nil)
        (setf active-p nil
              captured-plan (plan-mode-state-last-plan-markdown plan-state)
              available-step-indexes (plan-step-indexes plan-state)
              approved-step-indexes (or (plan-mode-state-approved-step-indexes plan-state) '())
              review-decision (plan-mode-state-review-decision plan-state))))
    (cond
      ((or active-p
           (not (stringp captured-plan))
           (zerop (length (%slash-trim captured-plan)))
           (null available-step-indexes))
       (make-slash-command-result
        :output "No captured plan is available yet. Use /plan to draft a plan before /execute."))
      ((null approved-step-indexes)
       (make-slash-command-result
        :output "No approved steps are available for execution. Use /plan approve first."))
      ((not (member review-decision '(:approved :partially-approved) :test #'eq))
       (make-slash-command-result
        :output (format nil
                        "Plan review decision is ~A. Approve steps with /plan approve before /execute."
                        (amoebum.commands.plan::%plan-review-decision-label
                         review-decision))))
      (t
       (setconfig :plan-mode nil)
       (setf (plan-mode-state-review-pending-p plan-state) nil)
       (refresh-plan-review-markdown plan-state)
       (let* ((execution-state (initialize-plan-execution :plan-state plan-state))
              (approved-count (length approved-step-indexes))
              (step-count (length available-step-indexes))
              (next-step-index (plan-execution-next-step-index execution-state))
              (run-id (plan-execution-state-run-id execution-state)))
         (when interactive-p
           (setf (plan-execution-state-interactive-p execution-state) t))
         (plan-execution-append-output
          (format nil "LIVE> /execute accepted: run ~A with ~D approved step~:P."
                  run-id
                  approved-count)
          :phase :system
          :style :meta
          :state execution-state)
         (make-slash-command-result
          :output (with-output-to-string (out)
                    (write-string "Execution pathways re-enabled after user approval." out)
                    (write-string " Plan mode is OFF." out)
                    (format out " Approved steps: ~D/~D (~A)."
                            approved-count
                            step-count
                            (amoebum.commands.plan::%format-step-index-list
                             approved-step-indexes))
                    (format out " Execution run initialized: ~A." run-id)
                    (when interactive-p
                      (write-string " Interactive mode: each step requires approval." out))
                    (when next-step-index
                      (format out " Next approved step: ~D." next-step-index)))))))))

(defun %clear-confirmed-p (raw-arguments)
  (let ((tokens (%tokenize-command-arguments (or raw-arguments ""))))
    (loop for token in tokens
          thereis (member (string-downcase (%slash-trim token))
                          '("--yes" "yes" "confirm" "--confirm")
                          :test #'string=))))

(defun %clear-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let ((raw-arguments (or (gethash :ARGS arguments) "")))
    (if (%clear-confirmed-p raw-arguments)
        (make-slash-command-result
         :echo-input-p nil
         :output "Conversation cleared."
         :action :clear-chat)
        (make-slash-command-result
         :echo-input-p t
         :output "Confirm clear with /clear --yes."))))

(defun %lint-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((raw (or (gethash :PATHS arguments) ""))
         (paths (let ((tokens (%tokenize-command-arguments raw)))
                  (and tokens (not (null tokens)) tokens)))
         (run-symbol (find-symbol "RUN-MACRO-LINT" :amoebum))
         (format-symbol (find-symbol "FORMAT-MACRO-LINT-REPORT" :amoebum)))
    (cond
      ((or (null run-symbol)
           (null format-symbol)
           (not (fboundp run-symbol))
           (not (fboundp format-symbol)))
       (make-slash-command-result
        :echo-input-p t
        :output "/lint unavailable: compile validation module is not loaded."))
      (t
       (handler-case
           (let* ((report (if paths
                              (funcall (symbol-function run-symbol) :paths paths)
                              (funcall (symbol-function run-symbol))))
                  (output (funcall (symbol-function format-symbol) report)))
             (make-slash-command-result
              :echo-input-p t
              :output output))
         (error (condition)
           (make-slash-command-result
            :echo-input-p t
            :output (format nil "/lint failed: ~A" condition))))))))

(defun %mode-arg-completer (_command _invocation _index fragment _prefix)
  (declare (ignore _command _invocation _index _prefix))
  (let ((prefix (%slash-trim fragment)))
    (loop for mode in *known-permission-modes*
          for text = (string-downcase (symbol-name mode))
          when (%starts-with-ci-p prefix text)
            collect text)))

(defun %help-arg-completer (_command _invocation _index fragment _prefix)
  (declare (ignore _command _invocation _index _prefix))
  (let ((prefix (%normalize-command-name fragment)))
    (loop for command in (list-slash-commands)
          for name = (%normalize-command-name (slash-command-name command))
          when (%starts-with-ci-p prefix name)
            collect name)))

(defun %plan-arg-completer (_command _invocation _index fragment _prefix)
  (declare (ignore _command _invocation _index _prefix))
  (let ((prefix (%slash-trim fragment)))
    (loop for option in '("on" "off" "status" "review"
                          "approve" "reorder" "reject" "modify"
                          "request-modifications" "request-changes")
          when (%starts-with-ci-p prefix option)
            collect option)))

(defun register-core-slash-commands ()
  (register-slash-command
   (make-slash-command
    :name "help"
    :description "Show available slash commands or help for one command."
    :usage "/help [command]"
    :parameters
    (list (make-slash-command-parameter
           :name "topic"
           :type :string
           :required-p nil
           :description "Optional command name to describe."))
    :handler #'%help-handler
    :completer #'%help-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "mode"
    :description "Show or set permission mode for this session."
    :usage "/mode [supervised|auto-edit|full-auto|yolo|no-confirm]"
    :parameters
    (list (make-slash-command-parameter
           :name "mode"
           :type :keyword
           :required-p nil
           :choices *known-permission-modes*
           :description "Target permission mode."))
    :handler #'%mode-handler
    :completer #'%mode-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "model"
    :description "Show or set active model."
    :usage "/model [name]"
    :parameters
    (list (make-slash-command-parameter
           :name "model"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Model name to use for this session."))
    :handler #'%model-handler))
  (register-slash-command
   (make-slash-command
    :name "config"
    :description "Show resolved configuration values with layer provenance."
    :usage "/config"
    :handler #'%config-handler))
  (register-slash-command
   (make-slash-command
    :name "plan"
    :description "Toggle plan mode, review captured plan output, reorder steps, record review decisions, and approve specific steps."
    :usage "/plan [on|off|status|review|approve|reorder|reject|modify|request-modifications|request-changes] [args...]"
    :parameters
    (list (make-slash-command-parameter
           :name "state"
           :type :keyword
           :required-p nil
           :choices '(:on :off :status :review :approve :reorder :reject :modify
                      :request-modifications :request-changes)
           :description "Optional explicit plan mode action.")
          (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional action arguments."))
    :handler #'amoebum.commands.plan:%plan-command-handler
    :completer #'%plan-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "execute"
    :description "Transition from approved plan review into execution mode."
    :usage "/execute"
    :handler #'%execute-handler))
  (register-slash-command
   (make-slash-command
    :name "clear"
    :description "Reset the active conversation (requires confirmation)."
    :usage "/clear --yes"
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Pass --yes to confirm reset."))
    :handler #'%clear-handler))
  (register-slash-command
   (make-slash-command
    :name "lint"
    :description "Re-expand deftool/defhook/defwidget forms and report compile-time warnings/errors."
    :usage "/lint [path ...]"
    :parameters
    (list (make-slash-command-parameter
           :name "paths"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional files/directories to scan; defaults to amoebum/src and ptui/src/widgets."))
    :handler #'%lint-handler))
  (register-slash-command
   (make-slash-command
    :name "maxturns"
    :description "Show or set the maximum agentic iterations for this conversation."
    :usage "/maxturns [number|reset]"
    :parameters
    (list (make-slash-command-parameter
           :name "limit"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Maximum iterations (positive integer), 'reset' to revert, or omit to see current value."))
    :handler #'%maxturns-handler))
  t)
