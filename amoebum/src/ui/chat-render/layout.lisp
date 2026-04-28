(in-package :amoebum)

;;; Plan-mode presentation layout helpers extracted from ui/chat-render.lisp
;;; for NXT-545. Owns the data shaping that feeds
;;; ptui.components.plan-presentation:make-plan-mode-presentation-widget —
;;; step previews, command-preview line synthesis, execution progress
;;; lines, output entry composition, and the workspace-tree cache key.
;;; Loaded after chat-render/widgets and chat-render/scrollback so this
;;; module can rely on shared widget primitives without forward references.

(defun %chat-plan-mode-enabled-p ()
  (not (null (cfg :plan-mode))))

(defun %chat-plan-execution-surface-active-p (&optional (execution-state (current-plan-execution-state)))
  (and (plan-execution-state-p execution-state)
       (let ((run-id (plan-execution-state-run-id execution-state))
             (status (plan-execution-state-status execution-state))
             (continuity (plan-execution-state-continuity-output execution-state))
             (steps (plan-execution-state-steps execution-state)))
         (or (and (stringp run-id)
                  (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) run-id))))
             (and (keywordp status)
                  (not (eq status :idle)))
             continuity
             steps))))

(defun %chat-plan-workspace-visible-p (plan-state execution-state)
  (and (plan-mode-state-p plan-state)
       (plan-mode-state-steps plan-state)
       (or (%chat-plan-mode-enabled-p)
           (%chat-plan-execution-surface-active-p execution-state))))

(defun %chat-plan-presentation-safe-string (value &optional (fallback ""))
  (cond
    ((and (stringp value)
          (plusp (length value)))
     value)
    ((null value)
     fallback)
    (t
     (princ-to-string value))))

(defun %chat-plan-inline-code-spans (text)
  (let* ((source (%chat-plan-presentation-safe-string text ""))
         (length (length source))
         (index 0)
         (spans '()))
    (loop while (< index length) do
      (let ((start (position #\` source :start index)))
        (if (null start)
            (setf index length)
            (let ((end (position #\` source :start (1+ start))))
              (if (null end)
                  (setf index length)
                  (let ((snippet
                          (string-trim '(#\Space #\Tab #\Newline #\Return)
                                       (subseq source (1+ start) end))))
                    (when (plusp (length snippet))
                      (push snippet spans))
                    (setf index (1+ end))))))))
    (nreverse spans)))

(defun %chat-plan-leading-token (text)
  (let* ((trimmed
           (string-trim '(#\Space #\Tab #\Newline #\Return)
                        (%chat-plan-presentation-safe-string text "")))
         (length (length trimmed)))
    (if (zerop length)
        ""
        (let ((end
                (or (position-if #'%whitespace-char-p trimmed)
                    length)))
          (string-downcase (subseq trimmed 0 end))))))

(defun %chat-plan-commandish-p (text)
  (let* ((trimmed
           (string-trim '(#\Space #\Tab #\Newline #\Return)
                        (%chat-plan-presentation-safe-string text "")))
         (length (length trimmed))
         (token (%chat-plan-leading-token trimmed)))
    (and (plusp length)
         (or (member token +chat-plan-command-heads+ :test #'string=)
             (and (>= length 2)
                  (string= (subseq trimmed 0 2) "./"))
             (and (>= length 2)
                  (string= (subseq trimmed 0 2) "~/"))
             (char= (char trimmed 0) #\/)
             (search "&&" trimmed :test #'char=)
             (search "||" trimmed :test #'char=)
             (search "|" trimmed :test #'char=)
             (search ";" trimmed :test #'char=)
             (search ">" trimmed :test #'char=)
             (search "<" trimmed :test #'char=)
             (and (>= length 3)
                  (string= (subseq trimmed (- length 3)) ".sh"))))))

(defun %chat-plan-step-command-previews (step)
  (check-type step plan-step)
  (let* ((description (%chat-plan-presentation-safe-string
                       (plan-step-description step)
                       ""))
         (inline-spans (%chat-plan-inline-code-spans description)))
    (remove-duplicates
     (loop for span in inline-spans
           for normalized = (string-trim '(#\Space #\Tab #\Newline #\Return)
                                         (%normalize-inline-text span))
           when (%chat-plan-commandish-p normalized)
             collect normalized)
     :test #'string=)))

(defun %chat-plan-sorted-steps (plan-state)
  (sort (copy-list (or (plan-mode-state-steps plan-state) '()))
        #'<
        :key #'plan-step-index))

(defun %chat-plan-visible-steps (plan-state)
  (let* ((sorted-steps (%chat-plan-sorted-steps plan-state))
         (visible-count (min (length sorted-steps) +chat-plan-presentation-max-steps+)))
    (subseq sorted-steps 0 visible-count)))

(defun %chat-plan-normalize-path-list (paths)
  (remove nil
          (loop for path in (or paths '())
                for text = (%chat-plan-presentation-safe-string path "")
                for trimmed = (string-trim '(#\Space #\Tab #\Newline #\Return) text)
                when (plusp (length trimmed))
                  collect trimmed)))

(defun %chat-plan-rationale-snippet (step)
  (check-type step plan-step)
  (%truncate-inline-text
   (%chat-plan-presentation-safe-string (plan-step-description step) "")
   +chat-plan-rationale-snippet-chars+))

(defun %chat-plan-step-by-index (steps step-index)
  (when (integerp step-index)
    (find step-index (or steps '()) :key #'plan-step-index :test #'=)))

(defun %chat-plan-resolve-selected-step-index (chat-state plan-state visible-steps)
  (let* ((visible-indexes
           (loop for step in (or visible-steps '())
                 for index = (plan-step-index step)
                 when (integerp index)
                   collect index))
         (approved-visible-indexes
           (loop for index in (plan-mode-state-approved-step-indexes plan-state)
                 when (member index visible-indexes :test #'=)
                   collect index))
         (current-selection (chat-ui-state-plan-selected-step-index chat-state))
         (resolved-selection
           (cond
             ((and (integerp current-selection)
                   (member current-selection visible-indexes :test #'=))
              current-selection)
             (approved-visible-indexes
              (first approved-visible-indexes))
             (visible-indexes
              (first visible-indexes))
             (t
              nil))))
    (setf (chat-ui-state-plan-selected-step-index chat-state) resolved-selection)
    resolved-selection))

(defun %chat-plan-command-preview-lines (plan-state selected-step-index)
  (let* ((steps (%chat-plan-sorted-steps plan-state))
         (approved-indexes (plan-mode-state-approved-step-indexes plan-state))
         (entries '()))
    (dolist (step steps)
      (let* ((step-index (or (plan-step-index step) 0))
             (approved-p (member step-index approved-indexes :test #'=)))
        (dolist (command (%chat-plan-step-command-previews step))
          (push (format nil
                        "DRY-RUN> [step ~D ~A~:[~; | selected~] | non-executed] ~A"
                        step-index
                        (if approved-p "approved" "pending")
                        (and (integerp selected-step-index)
                             (= step-index selected-step-index))
                        command)
                entries))))
    (let ((ordered (nreverse entries)))
      (subseq ordered
              0
              (min (length ordered)
                   +chat-plan-command-preview-max-lines+)))))

(defun %chat-plan-step-status-from-execution-step (execution-step approved-p)
  (if execution-step
      (case (plan-execution-step-status execution-step)
        (:running :running)
        (:completed :done)
        (:done :done)
        (:blocked :blocked)
        (:failed :blocked)
        (:aborted :blocked)
        (otherwise :pending))
      (if approved-p :approved :pending)))

(defun %chat-plan-step-status-event-table (chat-state execution-state)
  (let* ((run-id (and (plan-execution-state-p execution-state)
                      (plan-execution-state-run-id execution-state)))
         (bus (and chat-state (%context-event-bus chat-state))))
    (unless (and (%chat-plan-execution-surface-active-p execution-state)
                 (event-bus-p bus)
                 (stringp run-id)
                 (plusp (length run-id)))
      (return-from %chat-plan-step-status-event-table nil))
    (let ((table (make-hash-table :test #'eql)))
      (dolist (event (event-history bus))
        (when (eq (event-type event) +event-type-plan-step-status+)
          (let ((payload (event-payload event)))
            (when (and (plan-step-status-payload-p payload)
                       (integerp (plan-step-status-payload-step-index payload))
                       (stringp (plan-step-status-payload-run-id payload))
                       (string= (plan-step-status-payload-run-id payload) run-id))
              (setf (gethash (plan-step-status-payload-step-index payload) table)
                    (case (plan-step-status-payload-status payload)
                      (:running :running)
                      (:blocked :blocked)
                      (:done :done)
                      (otherwise :pending)))))))
      table)))

(defun %chat-plan-step-status-event-signature (chat-state execution-state)
  (let* ((run-id (and (plan-execution-state-p execution-state)
                      (plan-execution-state-run-id execution-state)))
         (bus (and chat-state (%context-event-bus chat-state))))
    (unless (and (%chat-plan-execution-surface-active-p execution-state)
                 (event-bus-p bus)
                 (stringp run-id)
                 (plusp (length run-id)))
      (return-from %chat-plan-step-status-event-signature nil))
    (loop for event in (event-history bus)
          for payload = (event-payload event)
          when (and (eq (event-type event) +event-type-plan-step-status+)
                    (plan-step-status-payload-p payload)
                    (integerp (plan-step-status-payload-step-index payload))
                    (stringp (plan-step-status-payload-run-id payload))
                    (string= (plan-step-status-payload-run-id payload) run-id))
            collect (list (event-seq event)
                          (plan-step-status-payload-step-index payload)
                          (plan-step-status-payload-status payload)))))

(defun %chat-plan-presentation-steps (plan-state visible-steps execution-state &optional chat-state)
  (let ((execution-step-table
          (when (%chat-plan-execution-surface-active-p execution-state)
            (let ((table (make-hash-table :test #'eql)))
              (dolist (execution-step (plan-execution-state-steps execution-state))
                (let ((step-index (plan-execution-step-index execution-step)))
                  (when (integerp step-index)
                    (setf (gethash step-index table) execution-step))))
              table)))
        (event-status-table (%chat-plan-step-status-event-table chat-state execution-state)))
    (loop for step in visible-steps
          for step-index = (or (plan-step-index step) 0)
          for execution-step = (and execution-step-table
                                    (gethash step-index execution-step-table))
          for approved-p = (if execution-step
                               (plan-execution-step-approved-p execution-step)
                               (member step-index
                                       (plan-mode-state-approved-step-indexes plan-state)
                                       :test #'=))
          for fallback-status = (%chat-plan-step-status-from-execution-step
                                 execution-step
                                 approved-p)
          for event-status = (and event-status-table
                                  (gethash step-index event-status-table))
          for status = (or event-status fallback-status)
          collect
          (ptui.components.plan-presentation:make-plan-presentation-step
           :index step-index
           :description (%chat-plan-presentation-safe-string
                         (plan-step-description step)
                         "Describe this step.")
           :approved-p (not (null approved-p))
           :file-paths (%chat-plan-normalize-path-list (plan-step-file-paths step))
           :rationale-snippet (%chat-plan-rationale-snippet step)
           :risk (or (plan-step-risk step) :medium)
           :status status))))

(defun %chat-plan-presentation-output-lines (plan-state selected-step-index)
  (let* ((steps (or (plan-mode-state-steps plan-state) '()))
         (total (length steps))
         (approved (length (plan-mode-state-approved-step-indexes plan-state)))
         (selected-step (%chat-plan-step-by-index steps selected-step-index))
         (selected-file-path
           (car (%chat-plan-normalize-path-list
                 (and selected-step (plan-step-file-paths selected-step)))))
         (selected-step-line
           (when (integerp selected-step-index)
             (if selected-file-path
                 (format nil
                         "Selected step: ~D (Ctrl-N/Ctrl-P to change) | file ~A"
                         selected-step-index
                         selected-file-path)
                 (format nil "Selected step: ~D (Ctrl-N/Ctrl-P to change)"
                         selected-step-index))))
         (decision-text
           (string-downcase
            (symbol-name (or (plan-mode-state-review-decision plan-state)
                             :pending))))
         (pending-p (plan-mode-state-review-pending-p plan-state))
         (command-previews (%chat-plan-command-preview-lines plan-state
                                                             selected-step-index)))
    (if command-previews
        (if selected-step-line
            (cons selected-step-line command-previews)
            command-previews)
        (append
         (when selected-step-line
           (list selected-step-line))
         (list "Plan mode active. Mutating tools remain blocked."
               (format nil "Review decision: ~A~:[~; (pending)~]" decision-text pending-p)
               (format nil "Step approvals: ~D/~D" approved total)
               "DRY-RUN> [non-executed] No command snippets detected in proposed steps yet.")))))

(defun %chat-plan-output-stdin-capture-policy ()
  (if (or (%chat-plan-mode-enabled-p)
          (%chat-plan-execution-surface-active-p))
      :disabled
      :enabled))

(defun %chat-plan-execution-output-line-entries (execution-state)
  (when (%chat-plan-execution-surface-active-p execution-state)
    (loop for entry in (plan-execution-state-continuity-output execution-state)
          collect (list :text (%chat-plan-presentation-safe-string
                               (plan-execution-output-entry-line entry)
                               "")
                        :step-index (plan-execution-output-entry-step-index entry)
                        :severity (or (plan-execution-output-entry-severity entry) :info)
                        :style (or (plan-execution-output-entry-style entry) :plain)
                        :recovery-actions
                        (copy-list (or (plan-execution-output-entry-recovery-actions entry)
                                       '()))))))

(defun %chat-plan-format-elapsed-seconds (elapsed-seconds)
  (let* ((total-seconds (max 0 (or elapsed-seconds 0)))
         (hours (truncate total-seconds 3600))
         (remaining (mod total-seconds 3600))
         (minutes (truncate remaining 60))
         (seconds (mod remaining 60)))
    (cond
      ((> hours 0)
       (format nil "~Dh ~Dm ~Ds" hours minutes seconds))
      ((> minutes 0)
       (format nil "~Dm ~Ds" minutes seconds))
      (t
       (format nil "~Ds" seconds)))))

(defun %chat-plan-execution-elapsed-seconds (execution-state)
  (let* ((started-at (plan-execution-state-started-at execution-state))
         (finished-at (plan-execution-state-finished-at execution-state))
         (status (plan-execution-state-status execution-state))
         (end-time (if (member status '(:completed :failed :aborted) :test #'eq)
                       finished-at
                       (get-universal-time))))
    (if (and (integerp started-at)
             (integerp end-time))
        (max 0 (- end-time started-at))
        0)))

(defun %chat-plan-execution-progress-line (execution-state)
  (let* ((approved-indexes (or (plan-execution-state-approved-step-indexes execution-state) '()))
         (total (length approved-indexes)))
    (unless (plusp total)
      (return-from %chat-plan-execution-progress-line
        "Execution progress: step 0 of 0 (elapsed 0s)"))
    (let* ((current-index (plan-execution-state-current-step-index execution-state))
           (current-position (and (integerp current-index)
                                  (position current-index approved-indexes :test #'=)))
           (completed (length (plan-execution-state-completed-step-indexes execution-state)))
           (status (plan-execution-state-status execution-state))
           (step-number
             (cond
               ((integerp current-position)
                (1+ current-position))
               ((eq status :completed)
                total)
               ((plusp completed)
                (min total (1+ completed)))
               (t
                1)))
           (elapsed-seconds (%chat-plan-execution-elapsed-seconds execution-state)))
      (format nil "Execution progress: step ~D of ~D (elapsed ~A)"
              step-number
              total
              (%chat-plan-format-elapsed-seconds elapsed-seconds)))))

(defun %chat-plan-presentation-context-lines (plan-state selected-step visible-steps execution-state)
  (let* ((steps (or (plan-mode-state-steps plan-state) '()))
         (high-risk-count
           (count-if (lambda (step)
                       (eq (plan-step-risk step) :high))
                     steps))
         (flattened-file-paths
           (remove-duplicates
            (loop for step in steps
                  append (or (plan-step-file-paths step) '()))
            :test #'string=))
         (notes (plan-mode-state-review-notes plan-state))
         (extra-step-count
           (max 0 (- (length steps) +chat-plan-presentation-max-steps+))))
    (append
     (list (format nil "Captured steps: ~D~:[~; (showing first ~D)~]"
                   (length steps)
                   (> extra-step-count 0)
                   +chat-plan-presentation-max-steps+)
           (format nil "Visible steps: ~D" (length visible-steps))
           (format nil "High-risk steps: ~D" high-risk-count)
           (if flattened-file-paths
               (format nil "Referenced files: ~{~A~^, ~}"
                       (subseq flattened-file-paths
                               0
                               (min 3 (length flattened-file-paths))))
               "Referenced files: none")
           (format nil "Review notes: ~A"
                   (%chat-plan-presentation-safe-string notes "none"))
           "Selection controls: Ctrl-N next, Ctrl-P previous.")
     (when selected-step
       (list (format nil "Selected rationale chars: ~D"
                     (length (%chat-plan-rationale-snippet selected-step)))))
     (when (%chat-plan-execution-surface-active-p execution-state)
       (list (format nil "Execution run: ~A"
                     (%chat-plan-presentation-safe-string
                      (plan-execution-state-run-id execution-state)
                      "none"))
             (format nil "Run status: ~A"
                     (string-downcase
                      (symbol-name (or (plan-execution-state-status execution-state)
                                       :idle))))
             (%chat-plan-execution-progress-line execution-state)
             (format nil "Execution progress: done ~D / pending ~D"
                     (length (plan-execution-state-completed-step-indexes execution-state))
                     (length (plan-execution-state-pending-step-indexes execution-state))))))))

(defun %chat-plan-presentation-widget (plan-state chat-state)
  (let ((execution-state (current-plan-execution-state)))
    (when (%chat-plan-workspace-visible-p plan-state execution-state)
      (let* ((visible-steps (%chat-plan-visible-steps plan-state))
             (selected-step-index (%chat-plan-resolve-selected-step-index
                                   chat-state
                                   plan-state
                                   visible-steps))
             (selected-step (%chat-plan-step-by-index visible-steps
                                                      selected-step-index))
             (output-line-entries (%chat-plan-execution-output-line-entries execution-state))
             (output-lines (if output-line-entries
                               nil
                               (%chat-plan-presentation-output-lines plan-state
                                                                    selected-step-index))))
        (ptui.components.plan-presentation:make-plan-mode-presentation-widget
         :id :chat-plan-presentation
         :steps (%chat-plan-presentation-steps plan-state
                                               visible-steps
                                               execution-state
                                               chat-state)
         :selected-step-index selected-step-index
         :output-lines output-lines
         :output-line-entries output-line-entries
         :output-stdin-capture-policy (%chat-plan-output-stdin-capture-policy)
         :context-lines (%chat-plan-presentation-context-lines plan-state
                                                               selected-step
                                                               visible-steps
                                                               execution-state)
         :output-viewport-height +chat-plan-presentation-output-viewport-height+)))))

(defun %chat-plan-workspace-tree-key (chat-state)
  (let* ((plan-state (current-plan-mode-state))
         (execution-state (current-plan-execution-state)))
    (list (%chat-plan-mode-enabled-p)
          (chat-ui-state-plan-selected-step-index chat-state)
          (loop for step in (or (plan-mode-state-steps plan-state) '())
                collect (list (plan-step-index step)
                              (plan-step-description step)
                              (copy-list (or (plan-step-file-paths step) '()))
                              (plan-step-risk step)))
          (copy-list (or (plan-mode-state-approved-step-indexes plan-state) '()))
          (plan-mode-state-review-decision plan-state)
          (plan-mode-state-review-pending-p plan-state)
          (%chat-plan-execution-surface-active-p execution-state)
          (plan-execution-state-run-id execution-state)
          (plan-execution-state-status execution-state)
          (plan-execution-state-current-step-index execution-state)
          (and (%chat-plan-execution-surface-active-p execution-state)
               (%chat-plan-execution-elapsed-seconds execution-state))
          (%chat-plan-step-status-event-signature chat-state execution-state)
          (copy-list (or (plan-execution-state-pending-step-indexes execution-state) '()))
          (copy-list (or (plan-execution-state-completed-step-indexes execution-state) '()))
          (loop for entry in (or (plan-execution-state-continuity-output execution-state) '())
                collect (list (plan-execution-output-entry-line entry)
                              (plan-execution-output-entry-step-index entry)
                              (plan-execution-output-entry-style entry)
                              (plan-execution-output-entry-severity entry)
                              (plan-execution-output-entry-phase entry)
                              (plan-execution-output-entry-timestamp entry))))))
