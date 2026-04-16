(in-package :amoebum)

(defun %normalize-output-phase (value)
  (let* ((text (%safe-plan-execution-string value "execution"))
         (phase (intern (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                    text))
                        :keyword)))
    (if (member phase '(:execution :dry-run :system) :test #'eq)
        phase
        :execution)))

(defun %normalize-output-severity (value)
  (let* ((text (%safe-plan-execution-string value "info"))
         (severity (intern (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                       text))
                           :keyword)))
    (if (member severity '(:debug :info :warning :error :critical) :test #'eq)
        severity
        :info)))

(defun %normalize-output-style (value)
  (let* ((text (%safe-plan-execution-string value "plain"))
         (style (intern (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                    text))
                        :keyword)))
    (if (member style '(:plain :meta :preview :success :warning :error) :test #'eq)
        style
        :plain)))

(defun %normalize-recovery-actions (actions)
  (remove-duplicates
   (remove nil
           (loop for action in (or actions '())
                 for text = (string-trim '(#\Space #\Tab #\Newline #\Return)
                                         (%safe-plan-execution-string action ""))
                 when (plusp (length text))
                   collect text))
   :test #'string=))

(defun %default-recovery-actions (state step-index)
  (check-type state plan-execution-state)
  (let* ((step (and (integerp step-index)
                    (%find-plan-execution-step state step-index)))
         (file-paths (and step
                          (copy-list (or (plan-execution-step-file-paths step) '()))))
         (recovery-actions
           (list (format nil
                         "Retry step ~D after fixing the failing command, then rerun /execute."
                         step-index)
                 (format nil
                         "Review step ~D details with /plan review before retrying."
                         step-index)
                 "Choose next action: rerun /execute, inspect with /plan review, or revise with /plan modify.")))
    (when file-paths
      (let ((visible-paths (subseq file-paths 0 (min 3 (length file-paths)))))
        (setf recovery-actions
              (append recovery-actions
                      (list (format nil
                                    "Inspect referenced files: ~{~A~^, ~}."
                                    visible-paths))))))
    (%normalize-recovery-actions recovery-actions)))

(defun %trim-plan-execution-output (entries)
  (let ((overflow (- (length entries) *plan-execution-continuity-max-lines*)))
    (if (> overflow 0)
        (nthcdr overflow entries)
        entries)))

(defun plan-execution-append-output (line &key
                                            step-index
                                            (phase :execution)
                                            (severity :info)
                                            (style :plain)
                                            recovery-actions
                                            (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (let* ((text (string-trim '(#\Space #\Tab #\Newline #\Return)
                            (%safe-plan-execution-string line "")))
         (normalized-step-index (and (integerp step-index) step-index))
         (normalized-severity (%normalize-output-severity severity))
         (normalized-recovery-actions (%normalize-recovery-actions recovery-actions))
         (resolved-recovery-actions
           (if (and (null normalized-recovery-actions)
                    normalized-step-index
                    (member normalized-severity '(:error :critical) :test #'eq))
               (%default-recovery-actions state normalized-step-index)
               normalized-recovery-actions)))
    (unless (plusp (length text))
      (return-from plan-execution-append-output state))
    (let* ((entry (make-plan-execution-output-entry
                   :line text
                   :step-index normalized-step-index
                   :phase (%normalize-output-phase phase)
                   :severity normalized-severity
                   :style (%normalize-output-style style)
                   :recovery-actions (copy-list resolved-recovery-actions)
                   :timestamp (get-universal-time)))
           (updated (append (plan-execution-state-continuity-output state)
                            (list entry))))
      (setf (plan-execution-state-continuity-output state)
            (%trim-plan-execution-output updated))
      state)))

(defun plan-execution-output-lines (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (loop for entry in (plan-execution-state-continuity-output state)
        collect (plan-execution-output-entry-line entry)))
