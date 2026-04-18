(in-package :amoebum)

;;; ---- NXT-344: /worktree-handoff ----

(defun %worktree-handoff-usage ()
  "Usage: /worktree-handoff <list|inspect|panel|accept|defer|resolve|abandon|help> [args...]
  list
  inspect <handoff-id>
  panel [on|off|toggle]
  accept <handoff-id> [note...]
  defer <handoff-id> [note...]
  resolve <handoff-id> [note...]
  abandon <handoff-id> [note...]")

(defun %worktree-handoff-status-text (status)
  (string-downcase (symbol-name (or status :pending))))

(defun %worktree-handoff-summary-line (snapshot)
  (let* ((handoff-id (or (getf snapshot :handoff-id) "?"))
         (status (%worktree-handoff-status-text (getf snapshot :status)))
         (worktree (getf snapshot :worktree))
         (worktree-id (or (getf worktree :id) "?"))
         (branch (getf worktree :branch))
         (target-ref (or (getf snapshot :target-ref) "?"))
         (room-id (getf snapshot :negotiation-room-id))
         (conflicts (getf (getf snapshot :preflight) :conflicts)))
    (format nil
            "  ~A | ~A | wt ~A~@[ (~A)~] -> ~A~@[ | conflicts ~D~]~@[ | room ~A~]"
            handoff-id
            status
            worktree-id
            branch
            target-ref
            (and conflicts (length conflicts))
            room-id)))

(defun %worktree-handoff-inspect-output (snapshot)
  (let* ((worktree (getf snapshot :worktree))
         (preflight (getf snapshot :preflight))
         (conflicts (getf preflight :conflicts))
         (negotiation-status (getf snapshot :negotiation-status))
         (resolution (getf snapshot :resolution)))
    (with-output-to-string (out)
      (format out "Worktree handoff ~A~%" (or (getf snapshot :handoff-id) "?"))
      (format out "status: ~A~%"
              (%worktree-handoff-status-text (getf snapshot :status)))
      (format out "worktree: ~A~@[ (~A)~]~%"
              (or (getf worktree :id) "?")
              (getf worktree :branch))
      (format out "target-ref: ~A~%" (or (getf snapshot :target-ref) "?"))
      (format out "agent: ~A~@[ via ~A~]~%"
              (or (getf snapshot :agent-id) "?")
              (getf snapshot :backend))
      (format out "room: ~A~@[ | artifact: ~A~]~%"
              (or (getf snapshot :negotiation-room-id) "?")
              (getf snapshot :artifact-id))
      (when negotiation-status
        (format out "negotiation-status: ~S~%" negotiation-status))
      (when conflicts
        (format out "conflicts: ~{~A~^, ~}~%" conflicts))
      (when resolution
        (format out "resolution: ~A owned by ~A~%"
                (%worktree-handoff-status-text (getf resolution :status))
                (%worktree-handoff-status-text (getf resolution :owner))))
      (when (getf snapshot :note)
        (format out "note: ~A~%" (getf snapshot :note)))
      (when (getf snapshot :task)
        (format out "task: ~A~%" (getf snapshot :task))))))

(defun %worktree-handoff-note (tokens)
  (let ((note (%slash-trim (format nil "~{~A~^ ~}" tokens))))
    (unless (zerop (length note))
      note)))

(defun %worktree-handoff-update-result (command verb updater tokens)
  (let ((handoff-id (second tokens)))
    (if (%slash-blank-p handoff-id)
        (make-slash-command-result
         :echo-input-p t
         :output (format nil "Usage: /worktree-handoff ~A <handoff-id> [note...]"
                         command))
        (handler-case
            (let ((snapshot (funcall updater
                                     handoff-id
                                     :note (%worktree-handoff-note (cddr tokens)))))
              (make-slash-command-result
               :echo-input-p t
               :output (format nil "~A worktree handoff ~A. Status: ~A"
                               verb
                               handoff-id
                               (%worktree-handoff-status-text
                                (getf snapshot :status)))))
          (error (condition)
            (make-slash-command-result
             :echo-input-p t
             :output (format nil "Failed to ~A worktree handoff ~A: ~A"
                             command
                             handoff-id
                             condition)))))))

(defun %worktree-handoff-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((args (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments args))
         (subcommand (or (first tokens) "list")))
    (cond
      ((or (string-equal subcommand "help")
           (string-equal subcommand "--help"))
       (make-slash-command-result
        :echo-input-p t
        :output (%worktree-handoff-usage)))
      ((string-equal subcommand "list")
       (let ((handoffs (list-worktree-conflict-handoffs)))
         (make-slash-command-result
          :echo-input-p t
          :output (if (null handoffs)
                      "No worktree conflict handoffs."
                      (with-output-to-string (out)
                        (format out "Worktree conflict handoffs (~D):~%"
                                (length handoffs))
                        (dolist (snapshot handoffs)
                          (format out "~A~%"
                                  (%worktree-handoff-summary-line snapshot))))))))
      ((string-equal subcommand "inspect")
       (let ((handoff-id (second tokens)))
         (if (%slash-blank-p handoff-id)
             (make-slash-command-result
              :echo-input-p t
              :output "Usage: /worktree-handoff inspect <handoff-id>")
             (let ((snapshot (find-worktree-conflict-handoff
                              handoff-id
                              :include-room-status-p t)))
               (make-slash-command-result
                :echo-input-p t
                :output (if snapshot
                            (%worktree-handoff-inspect-output snapshot)
                            (format nil "Unknown worktree handoff ~A."
                                    handoff-id)))))))
      ((string-equal subcommand "panel")
       (let* ((mode-token (and (second tokens)
                               (string-downcase (%slash-trim (second tokens)))))
              (visible
                (cond
                  ((or (null mode-token)
                       (string= mode-token "")
                       (string= mode-token "toggle"))
                   (toggle-worktree-handoff-dashboard))
                  ((member mode-token '("on" "show" "open") :test #'string=)
                   (toggle-worktree-handoff-dashboard t))
                  ((member mode-token '("off" "hide" "close") :test #'string=)
                   (toggle-worktree-handoff-dashboard nil))
                  (t
                   (return-from %worktree-handoff-handler
                     (make-slash-command-result
                      :echo-input-p t
                      :output "Usage: /worktree-handoff panel [on|off|toggle]"))))))
         (make-slash-command-result
          :echo-input-p t
          :output (format nil "Worktree handoff panel ~:[hidden~;visible~]."
                          visible))))
      ((string-equal subcommand "accept")
       (%worktree-handoff-update-result
        "accept" "Accepted" #'accept-worktree-conflict-handoff tokens))
      ((string-equal subcommand "defer")
       (%worktree-handoff-update-result
        "defer" "Deferred" #'defer-worktree-conflict-handoff tokens))
      ((string-equal subcommand "resolve")
       (%worktree-handoff-update-result
        "resolve" "Resolved" #'resolve-worktree-conflict-handoff tokens))
      ((string-equal subcommand "abandon")
       (%worktree-handoff-update-result
        "abandon" "Abandoned" #'abandon-worktree-conflict-handoff tokens))
      (t
       (make-slash-command-result
        :echo-input-p t
        :output (format nil
                        "Unknown worktree-handoff subcommand ~S. Try /worktree-handoff help."
                        subcommand))))))

(defun register-worktree-handoff-slash-command ()
  (register-slash-command
   (make-slash-command
    :name "worktree-handoff"
    :description "Inspect or update manual worktree merge conflict handoffs."
    :usage "/worktree-handoff <list|inspect|accept|defer|resolve|abandon|help> [args...]"
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Subcommand and arguments."))
    :handler #'%worktree-handoff-handler))
  t)
