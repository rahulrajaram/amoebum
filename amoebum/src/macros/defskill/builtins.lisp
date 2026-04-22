(in-package :amoebum)

;;;; ---------------------------------------------------------------------------
;;;; Built-in skills: `/commit`, `/review`, `/compact`, `/status`.
;;;;
;;;; These are the canonical skill definitions shipped with amoebum. Each is a
;;;; `defskill` form expanding against the registry/runtime helpers loaded
;;;; earlier in `src/macros/defskill/`.
;;;;
;;;; `%status-token-usage` lives here because it is the only runtime helper
;;;; exclusive to `/status`; if another built-in starts needing it, move it
;;;; to `defskill/runtime.lisp`.
;;;;
;;;; Behavior is preserved verbatim from the original
;;;; `amoebum/src/macros/defskill.lisp`; only file boundaries change.
;;;; ---------------------------------------------------------------------------

(defun %status-token-usage (context)
  (let* ((chat-state (and (typep context 'slash-command-context)
                          (slash-command-context-chat-state context)))
         (used (and chat-state
                    (ignore-errors
                      (chat-ui-state-context-used-tokens chat-state))))
         (limit (and chat-state
                     (ignore-errors
                       (chat-ui-state-context-window-limit chat-state)))))
    (list :used (or (and (integerp used) used) 0)
          :limit (or (and (integerp limit) limit) +default-context-window-limit+)
          :known-p (and (integerp used) (integerp limit)))))

(defskill commit ((files :string
                         :required nil
                         :greedy t
                         :description "Optional explicit file paths to stage before committing."))
  "Create a git commit with an AI-generated message from staged diff."
  (:category :git)
  (:usage "/commit [files...]")
  (let* ((file-tokens (slash-command-invocation-argument-tokens invocation))
         (args (if file-tokens
                   (%skill-tool-arguments :files file-tokens)
                   (%skill-tool-arguments)))
         (raw (%skill-invoke-tool "git-commit" args))
         (result (%skill-json->data raw))
         (sha (or (%skill-plist-entry result :sha) "unknown"))
         (branch (or (%skill-plist-entry result :branch) "unknown"))
         (summary (or (%skill-plist-entry result :message-summary) ""))
         (source (or (%skill-plist-entry result :message-source) "unknown")))
    (make-slash-command-result
     :echo-input-p t
     :output (format nil "Created commit ~A on ~A (~A).~@[~%~A~]"
                     sha
                     branch
                     source
                     (and (plusp (length (%slash-trim (princ-to-string summary))))
                          summary)))))

(defskill review ((base-branch :string
                              :required nil
                              :description "Optional base branch override (default auto-detect)."))
  "Analyze current branch diff and return a review summary."
  (:category :git)
  (:usage "/review [base-branch]")
  (let* ((args (if (and (stringp base-branch)
                        (plusp (length (%slash-trim base-branch))))
                   (%skill-tool-arguments :base-branch (%slash-trim base-branch))
                   (%skill-tool-arguments)))
         (raw (%skill-invoke-tool "git-diff-branch" args))
         (diff-data (%skill-json->data raw))
         (diff-text (or (%skill-plist-entry diff-data :diff) ""))
         (analysis
           (if (%skill-review-empty-p diff-text)
               "No branch diff found to review."
               (if (functionp *skill-review-analyzer*)
                   (funcall *skill-review-analyzer*
                            diff-data
                            :model (ignore-errors (config-model (current-config))))
                   (%skill-review-fallback diff-data))))
         (report (%skill-review-build-report diff-data analysis)))
    (make-slash-command-result
     :echo-input-p t
     :output (%skill-review-render-human report)
     :payload report)))

(defskill compact ((keep-last :integer
                         :required nil
                         :default 6
                         :description "How many recent turns to keep verbatim."))
  "Compress conversation context by summarizing older messages."
  (:category :session)
  (:usage "/compact [keep-last-turns]")
  (make-slash-command-result
   :echo-input-p t
   :output nil
   :action :compact-chat
   :payload keep-last))

(defskill status ()
  "Show current config, branch, and token usage status."
  (:category :session)
  (:usage "/status")
  (let* ((cfg (or (ignore-errors (current-config))
                  (ignore-errors (load-config))))
         (mode (if (typep cfg 'config)
                   (config-permission-mode cfg)
                   :unknown))
         (model (if (typep cfg 'config)
                    (config-model cfg)
                    "unknown"))
         (branch-result
           (handler-case
               (%skill-json->data
                (%skill-invoke-tool "git-status" (%skill-tool-arguments)))
             (error ()
               nil)))
         (branch (or (%skill-plist-entry branch-result :branch) "-"))
         (token-usage (%status-token-usage context))
         (used (getf token-usage :used 0))
         (limit (getf token-usage :limit +default-context-window-limit+))
         (known-p (getf token-usage :known-p))
         (percent (context-usage-percent used limit)))
    (make-slash-command-result
     :echo-input-p t
     :output (with-output-to-string (out)
               (format out "Status:~%")
               (format out "- branch: ~A~%" branch)
               (format out "- mode: ~A~%" (string-downcase (symbol-name mode)))
                (format out "- model: ~A~%" model)
                (if known-p
                    (format out "- tokens: ~D/~D (~D%%)" used limit percent)
                    (format out "- tokens: ~D/~D (~D%%, estimated)" used limit percent))))))
