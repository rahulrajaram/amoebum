(in-package :amoebum)

(defun %swarm-status-handler (_invocation _arguments _context)
  (declare (ignore _invocation _arguments _context))
  (let* ((mode (%configured-swarm-delegation-mode))
         (all-swarm (list-swarm-agents))
         (active-swarm (remove-if #'runtime-agent-terminal-p all-swarm))
         (handoff-count (if (boundp '*user-handoff-sequence*)
                            *user-handoff-sequence*
                            0))
         (room-count (if (and (boundp '*user-negotiation-room-participants*)
                              (hash-table-p *user-negotiation-room-participants*))
                         (hash-table-count *user-negotiation-room-participants*)
                         0))
         (local-active (active-agent-count)))
    (make-slash-command-result
     :output (with-output-to-string (out)
               (format out "SW4RM status:~%")
               (format out "  delegation-mode : ~A~%"
                       (string-downcase (symbol-name mode)))
               (format out "  local agents    : ~D active~%" local-active)
               (format out "  swarm agents    : ~D active / ~D total~%"
                       (length active-swarm) (length all-swarm))
               (format out "  handoffs issued : ~D~%" handoff-count)
               (format out "  review rooms    : ~D open~%" room-count)))))

(defun %format-peer-entry (peer)
  (let ((session-id (or (getf peer :session-id) "?"))
        (user-id (or (getf peer :user-id) "?"))
        (agent-id (or (getf peer :agent-id) "?"))
        (capabilities (getf peer :capabilities)))
    (format nil "  ~A (user: ~A, agent: ~A)~@[ caps: ~{~A~^, ~}~]"
            session-id user-id agent-id capabilities)))

(defun %swarm-peers-handler (_invocation _arguments _context)
  (declare (ignore _invocation _arguments _context))
  (let ((peers (list-user-session-peers)))
    (make-slash-command-result
     :echo-input-p t
     :output (if (null peers)
                 "No registered swarm peers."
                 (with-output-to-string (out)
                   (format out "Swarm peers (~D):~%" (length peers))
                   (dolist (peer peers)
                     (format out "~A~%" (%format-peer-entry peer))))))))

(defun %review-room-usage ()
  "Usage: /review-room <create|submit|critique|status|wait> [args...]
  create <room-id> <session-id> [session-id...]
  submit <room-id> <artifact-id> <artifact-text>
  critique <room-id> <artifact-id> <pass|fail> [details...]
  status <room-id>
  wait <artifact-id> [--timeout N]")

(defun %review-room-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((args (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments args))
         (subcommand (first tokens)))
    (cond
      ((or (null subcommand) (string-equal subcommand "help"))
       (make-slash-command-result
        :echo-input-p t
        :output (%review-room-usage)))
      ((string-equal subcommand "create")
       (%review-room-create-handler (rest tokens)))
      ((string-equal subcommand "submit")
       (%review-room-submit-handler (rest tokens)))
      ((string-equal subcommand "critique")
       (%review-room-critique-handler (rest tokens)))
      ((string-equal subcommand "status")
       (%review-room-status-handler (rest tokens)))
      ((string-equal subcommand "wait")
       (%review-room-wait-handler (rest tokens)))
      (t
       (make-slash-command-result
        :echo-input-p t
        :output (format nil "Unknown review-room subcommand ~S. Try /review-room help." subcommand))))))

(defun %review-room-create-handler (tokens)
  (let ((room-id (first tokens))
        (participants (rest tokens)))
    (unless (and room-id participants)
      (return-from %review-room-create-handler
        (make-slash-command-result
         :echo-input-p t
         :output "Usage: /review-room create <room-id> <session-id> [session-id...]")))
    (handler-case
        (let ((result (create-user-negotiation-room room-id participants)))
          (declare (ignore result))
          (make-slash-command-result
           :echo-input-p t
           :output (format nil "Created review room ~A with ~D participant~:P."
                           room-id (length participants))))
      (error (condition)
        (make-slash-command-result
         :echo-input-p t
         :output (format nil "Failed to create review room: ~A" condition))))))

(defun %review-room-submit-handler (tokens)
  (let ((room-id (first tokens))
        (artifact-id (second tokens))
        (artifact-text (format nil "~{~A~^ ~}" (cddr tokens))))
    (unless (and room-id artifact-id (plusp (length artifact-text)))
      (return-from %review-room-submit-handler
        (make-slash-command-result
         :echo-input-p t
         :output "Usage: /review-room submit <room-id> <artifact-id> <artifact-text>")))
    (handler-case
        (let ((result (submit-user-negotiation-artifact
                       room-id nil artifact-id artifact-text)))
          (declare (ignore result))
          (make-slash-command-result
           :echo-input-p t
           :output (format nil "Submitted artifact ~A to room ~A." artifact-id room-id)))
      (error (condition)
        (make-slash-command-result
         :echo-input-p t
         :output (format nil "Failed to submit artifact: ~A" condition))))))

(defun %review-room-critique-handler (tokens)
  (let* ((room-id (first tokens))
         (artifact-id (second tokens))
         (verdict-text (third tokens))
         (passed (and verdict-text (string-equal verdict-text "pass")))
         (details (format nil "~{~A~^ ~}" (cdddr tokens))))
    (unless (and room-id artifact-id verdict-text)
      (return-from %review-room-critique-handler
        (make-slash-command-result
         :echo-input-p t
         :output "Usage: /review-room critique <room-id> <artifact-id> <pass|fail> [details...]")))
    (handler-case
        (let ((result (add-user-negotiation-critique
                       room-id artifact-id nil passed
                       :details (when (plusp (length details)) details))))
          (declare (ignore result))
          (make-slash-command-result
           :echo-input-p t
           :output (format nil "Critique ~A for artifact ~A in room ~A."
                           (if passed "PASS" "FAIL") artifact-id room-id)))
      (error (condition)
        (make-slash-command-result
         :echo-input-p t
         :output (format nil "Failed to add critique: ~A" condition))))))

(defun %review-room-status-handler (tokens)
  (let ((room-id (first tokens)))
    (unless room-id
      (return-from %review-room-status-handler
        (make-slash-command-result
         :echo-input-p t
         :output "Usage: /review-room status <room-id>")))
    (handler-case
        (let ((status (get-user-negotiation-room-status room-id)))
          (make-slash-command-result
           :echo-input-p t
           :output (if status
                       (with-output-to-string (out)
                         (format out "Room ~A:~%" room-id)
                         (format out "  Participants: ~{~A~^, ~}~%"
                                 (or (getf status :participant-session-ids) '("none")))
                         (format out "  Active critics: ~{~A~^, ~}~%"
                                 (or (getf status :active-critic-session-ids) '("none")))
                         (format out "  Status: ~A" (or (getf status :status) "unknown")))
                       (format nil "Room ~A not found." room-id))))
      (error (condition)
        (make-slash-command-result
         :echo-input-p t
         :output (format nil "Failed to get room status: ~A" condition))))))

(defun %review-room-wait-handler (tokens)
  (let* ((artifact-id (first tokens))
         (timeout-s 30.0))
    (loop for rest on (rest tokens) by #'cddr
          when (string-equal (first rest) "--timeout")
            do (handler-case
                   (setf timeout-s (coerce (parse-integer (second rest)) 'double-float))
                 (error () nil)))
    (unless artifact-id
      (return-from %review-room-wait-handler
        (make-slash-command-result
         :echo-input-p t
         :output "Usage: /review-room wait <artifact-id> [--timeout N]")))
    (handler-case
        (let ((decision (wait-for-user-negotiation-decision artifact-id :timeout-s timeout-s)))
          (make-slash-command-result
           :echo-input-p t
           :output (if decision
                       (format nil "Decision for ~A: ~A" artifact-id (getf decision :decision))
                       (format nil "Timed out waiting for decision on ~A." artifact-id))))
      (error (condition)
        (make-slash-command-result
         :echo-input-p t
         :output (format nil "Wait failed: ~A" condition))))))

(defun register-swarm-runtime-slash-commands ()
  (register-slash-command
   (make-slash-command
    :name "swarm-peers"
    :description "List registered SW4RM session peers and capabilities."
    :usage "/swarm-peers"
    :handler #'%swarm-peers-handler))
  (register-slash-command
   (make-slash-command
    :name "review-room"
    :description "Multi-user code review rooms: create, submit, critique, status, wait."
    :usage "/review-room <create|submit|critique|status|wait> [args...]"
    :parameters
    (list (make-slash-command-parameter :name "args" :type :string :required-p nil :greedy-p t :description "Subcommand and arguments."))
    :handler #'%review-room-handler))
  (register-slash-command
   (make-slash-command
    :name "swarm-status"
    :description "Surface SW4RM state: delegation mode, agent counts, handoff count, and open review rooms."
    :usage "/swarm-status"
    :handler #'%swarm-status-handler))
  t)
