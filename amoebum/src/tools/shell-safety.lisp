(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; Shell Safety Policy Hooks (I112)
;;;
;;; Evaluates shell commands against a configurable safety policy before
;;; execution. Dangerous commands are blocked with clear deny reasons,
;;; ambiguous commands are escalated for approval, and safe commands pass
;;; through. Emits events for blocked and escalated commands.
;;; ---------------------------------------------------------------------------

;;; --- Configurable pattern sets ---------------------------------------------

;; The policy library accepts both legacy (REGEX . REASON) cons cells and
;; structured entries for explicit IDs/decisions.
(defstruct (shell-safety-pattern
            (:constructor make-shell-safety-pattern
                (&key id regex reason decision)))
  id
  regex
  reason
  decision)

(defparameter *shell-safety-deny-patterns*
  '(("(?i)\\brm\\s+[^\\n]*-r[^\\n]*\\s+/\\s*$" .
     "Recursive delete of root filesystem")
    ("(?i)\\brm\\s+-rf\\s+/\\s*$" .
     "rm -rf / destroys the entire filesystem")
    ("(?i)\\bdd\\s+.*\\bof=/dev/sd[a-z]\\b" .
     "dd writing directly to block device can destroy partitions")
    ("(?i)\\bdd\\s+.*\\bof=/dev/nvme" .
     "dd writing to NVMe device can destroy partitions")
    ("(?i)\\bmkfs(?:\\.[A-Za-z0-9_+-]+)?\\s" .
     "mkfs formats a device, destroying all data")
    ("(?i)\\b:[(][)][{][:][|][:][&][}];" .
     "Fork bomb will exhaust system resources")
    ("(?i)> /dev/sd[a-z]\\b" .
     "Redirecting output to a block device destroys data")
    ("(?i)\\bchmod\\s+-R\\s+777\\s+/" .
     "Recursive chmod 777 on root removes all file protections")
    ("(?i)\\bwget\\s[^\\n]*\\|\\s*(?:bash|sh|zsh)\\b" .
     "Piping remote content directly to shell is unsafe")
    ("(?i)\\bcurl\\s[^\\n]*\\|\\s*(?:bash|sh|zsh)\\b" .
     "Piping remote content directly to shell is unsafe"))
  "Alist of (REGEX . DENY-REASON) for commands that must be blocked outright.
Each entry is a cons of a CL-PPCRE regex string and a human-readable reason
explaining why the command is denied.")

(defparameter *shell-safety-escalate-patterns*
  '(("(?i)^\\s*sudo\\b" .
     "Command uses sudo, which requires elevated privileges")
    ("(?i)\\bsystemctl\\s+(?:start|stop|restart|enable|disable)\\b" .
     "Modifying system services can affect system stability")
    ("(?i)\\bapt(?:-get)?\\s+(?:install|remove|purge|dist-upgrade)\\b" .
     "Package management modifies system packages")
    ("(?i)\\byum\\s+(?:install|remove|erase|update)\\b" .
     "Package management modifies system packages")
    ("(?i)\\bdnf\\s+(?:install|remove|erase|update)\\b" .
     "Package management modifies system packages")
    ("(?i)\\bpacman\\s+-[SRU]" .
     "Package management modifies system packages")
    ("(?i)\\b(?:mv|cp|rm)\\s+[^\\n]*/(?:etc|boot|usr|var/lib)/" .
     "Modifying system directories can compromise system integrity")
    ("(?i)\\bchown\\s+-R\\b" .
     "Recursive ownership change can affect system file access")
    ("(?i)\\biptables\\b" .
     "Modifying firewall rules affects network security")
    ("(?i)\\bnpm\\s+publish\\b" .
     "Publishing packages has irreversible public consequences")
    ("(?i)\\bcargo\\s+publish\\b" .
     "Publishing packages has irreversible public consequences")
    ("(?i)\\bgit\\s+push\\s+[^\\n]*--force" .
     "Force pushing can rewrite shared history")
    ("(?i)\\bgit\\s+reset\\s+--hard\\b" .
     "Hard reset discards uncommitted changes permanently")
    ("(?i)\\bdocker\\s+system\\s+prune\\b" .
     "Docker system prune removes unused containers, images, and volumes"))
  "Alist of (REGEX . ESCALATION-REASON) for commands requiring approval.
These commands are not outright dangerous but need explicit user confirmation.")

(defparameter *shell-safety-pattern-library*
  (append
   (loop for (regex . reason) in *shell-safety-deny-patterns*
         collect (make-shell-safety-pattern
                  :id :deny
                  :regex regex
                  :reason reason
                  :decision :deny))
   (loop for (regex . reason) in *shell-safety-escalate-patterns*
         collect (make-shell-safety-pattern
                  :id :escalate
                  :regex regex
                  :reason reason
                  :decision :escalate)))
  "Combined dangerous/supervised shell pattern library.")

;;; --- Event types for shell safety ------------------------------------------

(defparameter +event-type-shell-command-blocked+
  (%event-type-keyword "shell:command-blocked"))

(defparameter +event-type-shell-command-escalated+
  (%event-type-keyword "shell:command-escalated"))

;;; --- Event payload structs -------------------------------------------------

(defstruct (shell-command-blocked-payload
            (:constructor make-shell-command-blocked-payload
                (&key command deny-reason matched-pattern)))
  command
  deny-reason
  matched-pattern)

(defstruct (shell-command-escalated-payload
            (:constructor make-shell-command-escalated-payload
                (&key command escalation-reason matched-pattern)))
  command
  escalation-reason
  matched-pattern)

;;; --- Event constructors ----------------------------------------------------

(defun make-shell-command-blocked-event (&key command deny-reason matched-pattern)
  (make-event :type +event-type-shell-command-blocked+
              :source :amoebum
              :severity :warning
              :payload (make-shell-command-blocked-payload
                        :command command
                        :deny-reason deny-reason
                        :matched-pattern matched-pattern)))

(defun make-shell-command-escalated-event (&key command escalation-reason matched-pattern)
  (make-event :type +event-type-shell-command-escalated+
              :source :amoebum
              :severity :warning
              :payload (make-shell-command-escalated-payload
                        :command command
                        :escalation-reason escalation-reason
                        :matched-pattern matched-pattern)))

;;; --- Policy evaluation result struct ---------------------------------------

(defstruct (shell-safety-result
            (:constructor make-shell-safety-result
                (&key decision reason matched-pattern matched-command matched-pattern-id)))
  (decision :allow :type keyword)
  reason
  matched-pattern
  matched-command
  matched-pattern-id)

;;; --- Core policy evaluation ------------------------------------------------

(defun %coerce-shell-safety-pattern (entry default-decision)
  (cond
    ((shell-safety-pattern-p entry)
     (let ((decision (or (shell-safety-pattern-decision entry)
                         default-decision)))
       (and (shell-safety-pattern-regex entry)
            (shell-safety-pattern-reason entry)
            (make-shell-safety-pattern
             :id (shell-safety-pattern-id entry)
             :regex (shell-safety-pattern-regex entry)
             :reason (shell-safety-pattern-reason entry)
             :decision decision))))
    ((and (consp entry)
          (stringp (car entry))
          (stringp (cdr entry)))
     (make-shell-safety-pattern
      :id nil
      :regex (car entry)
      :reason (cdr entry)
      :decision default-decision))
    (t nil)))

(defun %normalize-shell-safety-patterns (patterns default-decision)
  (let ((entries (or patterns '())))
    (loop for entry in entries
          for normalized = (%coerce-shell-safety-pattern entry default-decision)
          when normalized
            collect normalized)))

(defun %match-command-against-patterns (command patterns &key decision)
  "Try each (REGEX . REASON) in PATTERNS against COMMAND.
Returns (VALUES MATCHED-PATTERN REASON PATTERN-ID) on first match, or NIL."
  (let ((cmd (%command-string command)))
    (when cmd
      (loop for pattern-entry in (%normalize-shell-safety-patterns patterns decision)
            for regex = (shell-safety-pattern-regex pattern-entry)
            when (cl-ppcre:scan regex cmd)
              do (return (values regex
                                 (shell-safety-pattern-reason pattern-entry)
                                 (shell-safety-pattern-id pattern-entry)))))))

(defun %segment->command-string (segment)
  (let ((text (and (listp segment)
                   (format nil "~{~A~^ ~}" segment))))
    (when text
      (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
        (when (> (length trimmed) 0)
          trimmed)))))

(defun %command-segment-candidates (command)
  (let* ((canonical (ignore-errors (canonicalize-permission-command command)))
         (segments (and canonical (command-canonical-form-commands canonical)))
         (segment-count (length segments)))
    (when (> segment-count 1)
      (loop for segment in segments
            for index from 1
            for segment-command = (%segment->command-string segment)
            when segment-command
              collect (list :command segment-command
                            :segment-index index
                            :segment-count segment-count)))))

(defun %match-segment-against-patterns (command patterns decision)
  (let ((candidates (%command-segment-candidates command)))
    (when candidates
      (loop for candidate in candidates
            for segment-command = (getf candidate :command)
            do (multiple-value-bind (pattern reason pattern-id)
                   (%match-command-against-patterns segment-command
                                                   patterns
                                                   :decision decision)
                 (when pattern
                   (return (values pattern
                                   reason
                                   pattern-id
                                   segment-command
                                   (getf candidate :segment-index)
                                   (getf candidate :segment-count)))))))))

(defun %segment-match-reason (base-reason segment-command segment-index segment-count)
  (format nil "~A (detected in command segment ~D/~D: ~A)"
          base-reason
          segment-index
          segment-count
          segment-command))

(defun evaluate-shell-safety-policy (command
                                     &key (deny-patterns *shell-safety-deny-patterns*)
                                       (escalate-patterns *shell-safety-escalate-patterns*))
  "Evaluate COMMAND against the shell safety policy.
Returns a SHELL-SAFETY-RESULT with :decision being one of:
  :deny     - command is blocked (dangerous)
  :escalate - command requires approval (ambiguous)
  :allow    - command may proceed"
  (let ((cmd (%command-string command)))
    (unless cmd
      (return-from evaluate-shell-safety-policy
        (make-shell-safety-result :decision :deny
                                  :reason "Empty or nil command")))
    ;; Check deny patterns first (highest priority)
    (multiple-value-bind (pattern reason pattern-id)
        (%match-command-against-patterns cmd deny-patterns :decision :deny)
      (when pattern
        (return-from evaluate-shell-safety-policy
          (make-shell-safety-result :decision :deny
                                    :reason reason
                                    :matched-pattern pattern
                                    :matched-command cmd
                                    :matched-pattern-id pattern-id))))
    (multiple-value-bind (pattern reason pattern-id segment-command segment-index segment-count)
        (%match-segment-against-patterns cmd deny-patterns :deny)
      (when pattern
        (return-from evaluate-shell-safety-policy
          (make-shell-safety-result :decision :deny
                                    :reason (%segment-match-reason reason
                                                                   segment-command
                                                                   segment-index
                                                                   segment-count)
                                    :matched-pattern pattern
                                    :matched-command segment-command
                                    :matched-pattern-id pattern-id))))
    ;; Check escalation patterns
    (multiple-value-bind (pattern reason pattern-id)
        (%match-command-against-patterns cmd escalate-patterns :decision :escalate)
      (when pattern
        (return-from evaluate-shell-safety-policy
          (make-shell-safety-result :decision :escalate
                                    :reason reason
                                    :matched-pattern pattern
                                    :matched-command cmd
                                    :matched-pattern-id pattern-id))))
    (multiple-value-bind (pattern reason pattern-id segment-command segment-index segment-count)
        (%match-segment-against-patterns cmd escalate-patterns :escalate)
      (when pattern
        (return-from evaluate-shell-safety-policy
          (make-shell-safety-result :decision :escalate
                                    :reason (%segment-match-reason reason
                                                                   segment-command
                                                                   segment-index
                                                                   segment-count)
                                    :matched-pattern pattern
                                    :matched-command segment-command
                                    :matched-pattern-id pattern-id))))
    ;; No patterns matched -- allow
    (make-shell-safety-result :decision :allow)))

;;; --- Policy hook for pipeline integration ----------------------------------

(defun shell-safety-policy-hook (command &key (event-bus nil)
                                           (deny-patterns *shell-safety-deny-patterns*)
                                           (escalate-patterns *shell-safety-escalate-patterns*))
  "Enforce the shell safety policy on COMMAND.
- Blocked commands signal TOOL-PERMISSION-DENIED with a deny reason.
- Escalated commands signal TOOL-PERMISSION-DENIED with an escalation reason.
- Safe commands return :ALLOW.
Emits shell:command-blocked or shell:command-escalated events when applicable."
  (let* ((bus (or event-bus (ignore-errors (current-event-bus))))
         (result (evaluate-shell-safety-policy command
                                               :deny-patterns deny-patterns
                                               :escalate-patterns escalate-patterns))
         (decision (shell-safety-result-decision result))
         (reason (shell-safety-result-reason result))
         (pattern (shell-safety-result-matched-pattern result)))
    (case decision
      (:deny
       (when (and bus (event-bus-p bus))
         (publish bus
                  (make-shell-command-blocked-event
                   :command command
                   :deny-reason reason
                   :matched-pattern pattern)))
       (error 'tool-permission-denied
              :tool-name "bash-exec"
              :arguments nil
              :message (format nil "Shell command blocked: ~A" reason)
              :reason reason))
      (:escalate
       (when (and bus (event-bus-p bus))
         (publish bus
                  (make-shell-command-escalated-event
                   :command command
                   :escalation-reason reason
                   :matched-pattern pattern)))
       (error 'tool-permission-denied
              :tool-name "bash-exec"
              :arguments nil
              :message (format nil "Shell command requires approval: ~A" reason)
              :reason reason))
      (otherwise
       :allow))))

;;; --- Convenience: check without signalling ---------------------------------

(defun shell-command-safe-p (command &key (deny-patterns *shell-safety-deny-patterns*)
                                       (escalate-patterns *shell-safety-escalate-patterns*))
  "Return T if COMMAND passes both deny and escalate checks."
  (let ((result (evaluate-shell-safety-policy command
                                              :deny-patterns deny-patterns
                                              :escalate-patterns escalate-patterns)))
    (eq (shell-safety-result-decision result) :allow)))
