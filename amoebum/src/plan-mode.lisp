(in-package :amoebum)

(defparameter *known-plan-step-risk-levels*
  '(:low :medium :high))

(defparameter *known-plan-review-decisions*
  '(:pending :approved :partially-approved :rejected :modification-requested))

(defparameter *plan-mode-exploration-tool-names*
  '("read-file" "glob-files" "grep-content" "search-project"))

(defparameter *known-plan-path-extensions*
  '("lisp" "asd" "md" "txt" "toml" "json" "yaml" "yml"
    "sh" "bash" "zsh" "py" "js" "ts" "tsx" "css" "scss"
    "html" "xml" "c" "h" "cpp" "hpp" "go" "rs" "java"
    "kt" "swift" "sql" "proto" "nix"))

(defstruct (plan-step
            (:constructor make-plan-step
                (&key index
                 description
                 (file-paths '())
                 (risk :medium)
                 (depends-on '()))))
  index
  description
  (file-paths '() :type list)
  (risk :medium)
  (depends-on '() :type list))

(defstruct (plan-mode-state
            (:constructor %make-plan-mode-state
                (&key
                   (active-p nil)
                   entered-at
                   exited-at
                   (steps '())
                   (approved-step-indexes '())
                   (execution-pathways-enabled-p t)
                   (review-pending-p nil)
                   (review-decision :pending)
                   review-notes
                   review-decided-at
                   review-last-presented-at
                   (exploration-call-count 0)
                   (exploration-tool-names '())
                   exploration-last-updated-at
                   last-plan-markdown
                   last-output-path
                   last-exit-reason)))
  (active-p nil :type boolean)
  entered-at
  exited-at
  (steps '() :type list)
  (approved-step-indexes '() :type list)
  (execution-pathways-enabled-p t :type boolean)
  (review-pending-p nil :type boolean)
  (review-decision :pending)
  review-notes
  review-decided-at
  review-last-presented-at
  (exploration-call-count 0 :type integer)
  (exploration-tool-names '() :type list)
  exploration-last-updated-at
  last-plan-markdown
  last-output-path
  last-exit-reason)

(defparameter *plan-mode-state* (%make-plan-mode-state))

(defun %timestamp-compact (&optional (timestamp (get-universal-time)))
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time timestamp)
    (format nil "~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0D"
            year month day hour minute second)))

(defun %timestamp-iso8601 (&optional (timestamp (get-universal-time)))
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time timestamp)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour minute second)))

(defun %safe-plan-string (value &optional (fallback ""))
  (cond
    ((and (stringp value)
          (plusp (length value)))
     value)
    ((null value)
     fallback)
    (t
     (princ-to-string value))))

(defun %normalize-plan-risk (value)
  (let* ((risk-text (typecase value
                      (symbol (symbol-name value))
                      (string value)
                      (t nil)))
         (risk (if (and (stringp risk-text)
                        (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                    risk-text))))
                   (intern (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                       risk-text))
                           :keyword)
                   :medium)))
    (if (member risk *known-plan-step-risk-levels* :test #'eq)
        risk
        :medium)))

(defun %normalize-plan-tool-name (tool-name)
  (let* ((text (typecase tool-name
                 (string tool-name)
                 (symbol (symbol-name tool-name))
                 (t nil)))
         (trimmed (and (stringp text)
                       (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
    (and (stringp trimmed)
         (plusp (length trimmed))
         (string-downcase trimmed))))

(defun plan-mode-exploration-tool-p (tool-name)
  (let ((normalized (%normalize-plan-tool-name tool-name)))
    (and (stringp normalized)
         (member normalized
                 *plan-mode-exploration-tool-names*
                 :test #'string=))))

(defun %normalize-plan-review-decision (value)
  (let* ((decision-text (typecase value
                          (symbol (symbol-name value))
                          (string value)
                          (t nil)))
         (decision (if (and (stringp decision-text)
                            (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                        decision-text))))
                       (intern (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                           decision-text))
                               :keyword)
                       :pending)))
    (if (member decision *known-plan-review-decisions* :test #'eq)
        decision
        :pending)))

(defun %normalize-step-indexes (indexes max-index)
  (sort (remove-duplicates
         (loop for entry in (or indexes '())
               when (and (integerp entry)
                         (>= entry 1)
                         (<= entry max-index))
                 collect entry)
         :test #'=)
        #'<))

(defun plan-step-indexes (&optional (state (current-plan-mode-state)))
  (check-type state plan-mode-state)
  (sort (loop for step in (plan-mode-state-steps state)
              for index = (plan-step-index step)
              when (integerp index)
                collect index)
        #'<))

(defun clear-plan-step-approvals (&optional (state (current-plan-mode-state)))
  (check-type state plan-mode-state)
  (setf (plan-mode-state-approved-step-indexes state) '())
  state)

(defun set-plan-step-approvals (step-indexes &key (state (current-plan-mode-state)))
  (check-type state plan-mode-state)
  (let* ((available-indexes (plan-step-indexes state))
         (max-index (if available-indexes
                        (reduce #'max available-indexes)
                        0)))
    (setf (plan-mode-state-approved-step-indexes state)
          (%normalize-step-indexes step-indexes max-index)))
  state)

(defun approve-plan-steps (step-indexes &key (state (current-plan-mode-state)))
  (check-type state plan-mode-state)
  (let ((merged (append (plan-mode-state-approved-step-indexes state)
                        (or step-indexes '()))))
    (set-plan-step-approvals merged :state state)))

(defun plan-step-approved-p (step-index &optional (state (current-plan-mode-state)))
  (check-type state plan-mode-state)
  (and (integerp step-index)
       (member step-index
               (plan-mode-state-approved-step-indexes state)
               :test #'=)))

(defun set-plan-review-decision (decision &key notes (state (current-plan-mode-state)))
  (check-type state plan-mode-state)
  (let ((normalized-notes (string-trim '(#\Space #\Tab #\Newline #\Return)
                                       (%safe-plan-string notes "")))
        (normalized-decision (%normalize-plan-review-decision decision)))
    (setf (plan-mode-state-review-decision state) normalized-decision
          (plan-mode-state-review-notes state)
          (and (plusp (length normalized-notes)) normalized-notes)
          (plan-mode-state-review-decided-at state) (get-universal-time)
          (plan-mode-state-review-pending-p state)
          (not (null (member normalized-decision
                             '(:pending :partially-approved :modification-requested)
                             :test #'eq)))))
  state)

(defun set-plan-execution-pathways-enabled (enabled-p &key (state (current-plan-mode-state)))
  (check-type state plan-mode-state)
  (setf (plan-mode-state-execution-pathways-enabled-p state)
        (not (null enabled-p)))
  state)

(defun %captured-plan-available-p (state)
  (let ((plan-markdown (plan-mode-state-last-plan-markdown state)))
    (and (stringp plan-markdown)
         (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                     plan-markdown))))))

(defun plan-input-gating-snapshot (&optional (state (current-plan-mode-state)))
  (check-type state plan-mode-state)
  (let* ((active-plan-mode-p (not (null (plan-mode-state-active-p state))))
         (captured-plan-p (%captured-plan-available-p state))
         (review-pending-p (not (null (plan-mode-state-review-pending-p state))))
         (review-decision (%normalize-plan-review-decision
                           (plan-mode-state-review-decision state)))
         (terminal-stdin-enabled-p (not active-plan-mode-p))
         (execution-pathways-enabled-p
           (and (not active-plan-mode-p)
                (plan-mode-state-execution-pathways-enabled-p state)
                (or (not captured-plan-p)
                    (and (eq review-decision :approved)
                         (not review-pending-p)))))
         (reason
           (cond
             (active-plan-mode-p :plan-mode-active)
             ((and captured-plan-p review-pending-p) :review-pending)
             ((and captured-plan-p (not (eq review-decision :approved)))
              :review-not-approved)
             ((not execution-pathways-enabled-p)
              :awaiting-explicit-execute)
             (t
              :open)))
         (active-p
           (or (not terminal-stdin-enabled-p)
               (not execution-pathways-enabled-p))))
    (list :active-p active-p
          :reason reason
          :terminal-stdin-enabled-p terminal-stdin-enabled-p
          :execution-pathways-enabled-p execution-pathways-enabled-p
          :captured-plan-p captured-plan-p
          :review-pending-p review-pending-p
          :review-decision review-decision)))

(defun plan-input-gating-active-p (&optional (state (current-plan-mode-state)))
  (not (null (getf (plan-input-gating-snapshot state) :active-p))))

(defun plan-input-gating-reason (&optional (state (current-plan-mode-state)))
  (getf (plan-input-gating-snapshot state) :reason))

(defun plan-input-gating-terminal-stdin-enabled-p (&optional (state (current-plan-mode-state)))
  (not (null (getf (plan-input-gating-snapshot state) :terminal-stdin-enabled-p))))

(defun plan-input-gating-execution-pathways-enabled-p (&optional (state (current-plan-mode-state)))
  (not (null (getf (plan-input-gating-snapshot state) :execution-pathways-enabled-p))))

(defun plan-review-snapshot (&optional (state (current-plan-mode-state)))
  (check-type state plan-mode-state)
  (let* ((approved-step-indexes
           (copy-list (or (plan-mode-state-approved-step-indexes state) '())))
         (input-gating-snapshot (plan-input-gating-snapshot state)))
    (list :review-decision (%normalize-plan-review-decision
                            (plan-mode-state-review-decision state))
          :review-pending-p (not (null (plan-mode-state-review-pending-p state)))
          :review-notes (plan-mode-state-review-notes state)
          :approved-step-indexes approved-step-indexes
          :approved-step-count (length approved-step-indexes)
          :input-gating-snapshot input-gating-snapshot
          :input-gating-reason (getf input-gating-snapshot :reason)
          :terminal-stdin-enabled-p
          (not (null (getf input-gating-snapshot :terminal-stdin-enabled-p)))
          :execution-pathways-enabled-p
          (not (null (getf input-gating-snapshot :execution-pathways-enabled-p))))))

(defun %normalize-path-list (values)
  (loop for value in values
        for text = (typecase value
                     (pathname (namestring value))
                     (string value)
                     (symbol (symbol-name value))
                     (t (princ-to-string value)))
        when (and (stringp text)
                  (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
          collect text))

(defun %string-contains-digits-p (value)
  (and (stringp value)
       (loop for char across value
             thereis (digit-char-p char))))

(defun %extract-first-integer (value)
  (let* ((text (%safe-plan-string value ""))
         (length (length text)))
    (loop with start = nil
          for index from 0 below length
          for char = (char text index) do
            (cond
              ((digit-char-p char)
               (unless start
                 (setf start index)))
              (start
               (return (parse-integer text :start start :end index))))
          finally (when start
                    (return (parse-integer text :start start :end length))))))

(defun %normalize-dependency-list (depends-on max-index)
  (let ((result '()))
    (dolist (entry (or depends-on '()))
      (let ((index
              (cond
                ((integerp entry)
                 entry)
                ((and (stringp entry)
                      (%string-contains-digits-p entry))
                 (%extract-first-integer entry))
                ((symbolp entry)
                 (%extract-first-integer (symbol-name entry)))
                (t
                 nil))))
        (when (and (integerp index)
                   (>= index 1)
                   (<= index max-index))
          (push index result))))
    (sort (remove-duplicates result :test #'=) #'<)))

(defun %description-references-step-indexes (description max-index)
  (let ((text (string-downcase (%safe-plan-string description "")))
        (result '()))
    (loop for index from 1 to max-index do
      (let ((token (format nil "step ~D" index)))
        (when (search token text :test #'char=)
          (push index result))))
    (sort (remove-duplicates result :test #'=) #'<)))

(defun %description-sequential-cue-p (description)
  (let ((text (string-downcase (%safe-plan-string description ""))))
    (or (search " then " (format nil " ~A " text) :test #'char=)
        (search " next " (format nil " ~A " text) :test #'char=)
        (search " after " (format nil " ~A " text) :test #'char=)
        (search " once " (format nil " ~A " text) :test #'char=)
        (search " following " (format nil " ~A " text) :test #'char=))))

(defun %infer-step-dependencies (description depends-on next-index)
  (let* ((max-prior-index (1- next-index))
         (normalized-explicit (%normalize-dependency-list depends-on max-prior-index))
         (inferred-by-reference (%description-references-step-indexes description max-prior-index)))
    (cond
      (normalized-explicit
       normalized-explicit)
      (inferred-by-reference
       inferred-by-reference)
      ((and (> next-index 1)
            (%description-sequential-cue-p description))
       (list max-prior-index))
      (t
       '()))))

(defun %plan-whitespace-char-p (char)
  (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=))

(defun %split-whitespace (text)
  (let* ((source (%safe-plan-string text ""))
         (length (length source))
         (start nil)
         (result '()))
    (labels ((push-fragment (end)
               (when (and start (< start end))
                 (push (subseq source start end) result))))
      (loop for index from 0 below length
            for char = (char source index) do
              (if (%plan-whitespace-char-p char)
                  (progn
                    (push-fragment index)
                    (setf start nil))
                  (unless start
                    (setf start index))))
      (push-fragment length))
    (nreverse result)))

(defun %parse-numbered-step-line (line)
  (let* ((source (%safe-plan-string line ""))
         (trimmed (string-left-trim '(#\Space #\Tab) source))
         (length (length trimmed)))
    (unless (plusp length)
      (return-from %parse-numbered-step-line (values nil nil)))
    (let ((position 0))
      (loop while (and (< position length)
                       (digit-char-p (char trimmed position)))
            do (incf position))
      (unless (plusp position)
        (return-from %parse-numbered-step-line (values nil nil)))
      (unless (< position length)
        (return-from %parse-numbered-step-line (values nil nil)))
      (let ((delimiter (char trimmed position)))
        (unless (member delimiter '(#\. #\)) :test #'char=)
          (return-from %parse-numbered-step-line (values nil nil)))
        (let ((description-start (1+ position)))
          (loop while (and (< description-start length)
                           (%plan-whitespace-char-p
                            (char trimmed description-start)))
                do (incf description-start))
          (values (parse-integer trimmed :start 0 :end position)
                  (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (if (< description-start length)
                                   (subseq trimmed description-start)
                                   ""))))))))

(defun %collect-plan-step-blocks (response-text)
  (let ((blocks '())
        (current-index nil)
        (current-lines '()))
    (with-input-from-string (stream (%safe-plan-string response-text ""))
      (loop for line = (read-line stream nil nil)
            while line do
              (multiple-value-bind (step-index remainder)
                  (%parse-numbered-step-line line)
                (if (integerp step-index)
                    (progn
                      (when current-index
                        (push (list :index current-index
                                    :lines (nreverse current-lines))
                              blocks))
                      (setf current-index step-index
                            current-lines (list remainder)))
                    (when current-index
                      (push line current-lines))))))
    (when current-index
      (push (list :index current-index
                  :lines (nreverse current-lines))
            blocks))
    (nreverse blocks)))

(defun %plan-step-metadata-line-p (line)
  (let ((text (string-downcase
               (string-trim '(#\Space #\Tab #\Newline #\Return)
                            (%safe-plan-string line "")))))
    (or (uiop:string-prefix-p "risk:" text)
        (uiop:string-prefix-p "- risk:" text)
        (uiop:string-prefix-p "files:" text)
        (uiop:string-prefix-p "- files:" text)
        (uiop:string-prefix-p "file:" text)
        (uiop:string-prefix-p "- file:" text)
        (uiop:string-prefix-p "paths:" text)
        (uiop:string-prefix-p "- paths:" text)
        (uiop:string-prefix-p "path:" text)
        (uiop:string-prefix-p "- path:" text)
        (uiop:string-prefix-p "depends_on:" text)
        (uiop:string-prefix-p "- depends_on:" text)
        (uiop:string-prefix-p "depends on:" text)
        (uiop:string-prefix-p "- depends on:" text))))

(defun %plan-path-extension (token)
  (let* ((text (%safe-plan-string token ""))
         (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) text))
         (position (position #\. trimmed :from-end t)))
    (when (and position
               (< (1+ position) (length trimmed)))
      (string-downcase (subseq trimmed (1+ position))))))

(defun %sanitize-plan-path-token (token)
  (string-trim '(#\Space #\Tab #\Newline #\Return
                 #\` #\' #\" #\( #\) #\[ #\] #\{ #\}
                 #\< #\> #\, #\; #\: #\! #\?)
               (%safe-plan-string token "")))

(defun %plan-path-token-p (token)
  (let* ((trimmed (%sanitize-plan-path-token token))
         (length (length trimmed))
         (extension (%plan-path-extension trimmed))
         (has-separator (or (find #\/ trimmed)
                            (find #\\ trimmed)))
         (looks-like-known-file
           (member trimmed
                   '("Makefile" "Dockerfile" "Jenkinsfile")
                   :test #'string=)))
    (and (plusp length)
         (not (uiop:string-prefix-p "-" trimmed))
         (not (search "://" trimmed :test #'char=))
         (loop for char across trimmed
               thereis (alphanumericp char))
         (or has-separator
             looks-like-known-file
             (and extension
                  (member extension
                          *known-plan-path-extensions*
                          :test #'string=))))))

(defun %extract-inline-code-spans (text)
  (let* ((source (%safe-plan-string text ""))
         (length (length source))
         (index 0)
         (result '()))
    (loop while (< index length) do
      (let ((start (position #\` source :start index)))
        (if (null start)
            (setf index length)
            (let ((end (position #\` source :start (1+ start))))
              (if (null end)
                  (setf index length)
                  (let ((snippet (subseq source (1+ start) end)))
                    (push snippet result)
                    (setf index (1+ end))))))))
    (nreverse result)))

(defun %collect-plan-path-candidates (text)
  (let ((result '()))
    (dolist (token (%split-whitespace text))
      (let ((trimmed (%sanitize-plan-path-token token)))
        (when (%plan-path-token-p trimmed)
          (push trimmed result))))
    (nreverse result)))

(defun %collect-plan-step-file-paths (lines)
  (let ((result '())
        (seen (make-hash-table :test #'equal)))
    (labels ((record-path (candidate)
               (let ((trimmed (%sanitize-plan-path-token candidate)))
                 (when (and (%plan-path-token-p trimmed)
                            (not (gethash trimmed seen)))
                   (setf (gethash trimmed seen) t)
                   (push trimmed result)))))
      (dolist (line (or lines '()))
        (dolist (candidate (%collect-plan-path-candidates line))
          (record-path candidate))
        (dolist (span (%extract-inline-code-spans line))
          (record-path span)
          (dolist (candidate (%collect-plan-path-candidates span))
            (record-path candidate)))))
    (nreverse result)))

(defun %extract-plan-risk-from-lines (lines)
  (dolist (line (or lines '()) :medium)
    (let* ((text (string-downcase
                  (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (%safe-plan-string line ""))))
           (position (or (search "risk:" text :test #'char=)
                         (search "- risk:" text :test #'char=))))
      (when position
        (let* ((value-start (+ position
                               (if (uiop:string-prefix-p "- risk:" (subseq text position))
                                   7
                                   5)))
               (value (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   (if (< value-start (length text))
                                       (subseq text value-start)
                                       ""))))
          (return (%normalize-plan-risk value)))))))

(defun %extract-plan-dependencies-from-lines (lines)
  (let ((result '()))
    (dolist (line (or lines '()))
      (let ((text (string-downcase (%safe-plan-string line ""))))
        (when (or (search "depends_on" text :test #'char=)
                  (search "depends on" text :test #'char=))
          (dolist (fragment (%split-whitespace text))
            (when (%string-contains-digits-p fragment)
              (let ((index (%extract-first-integer fragment)))
                (when (and (integerp index)
                           (>= index 1))
                  (push index result))))))))
    (sort (remove-duplicates result :test #'=) #'<)))

(defun %normalize-plan-step-description (lines)
  (let ((description-lines
          (loop for line in (or lines '())
                for text = (string-trim '(#\Space #\Tab #\Newline #\Return)
                                        (%safe-plan-string line ""))
                unless (or (zerop (length text))
                           (%plan-step-metadata-line-p text))
                  collect text)))
    (string-trim '(#\Space #\Tab #\Newline #\Return)
                 (format nil "~{~A~^ ~}" description-lines))))

(defun %parse-plan-step-block (block)
  (let* ((lines (copy-list (or (getf block :lines) '())))
         (description (%normalize-plan-step-description lines))
         (file-paths (%collect-plan-step-file-paths lines))
         (risk (%extract-plan-risk-from-lines lines))
         (depends-on (%extract-plan-dependencies-from-lines lines)))
    (when (and (plusp (length description))
               file-paths)
      (list :description description
            :file-paths file-paths
            :risk risk
            :depends-on depends-on))))

(defun capture-plan-steps-from-response (response-text &key
                                                       (state (current-plan-mode-state)))
  (check-type state plan-mode-state)
  (unless (plan-mode-active-p state)
    (return-from capture-plan-steps-from-response 0))
  (let* ((blocks (%collect-plan-step-blocks response-text))
         (parsed (loop for block in blocks
                       for candidate = (%parse-plan-step-block block)
                       when candidate
                         collect candidate)))
    (unless (and blocks
                 (= (length parsed) (length blocks)))
      (return-from capture-plan-steps-from-response 0))
    (setf (plan-mode-state-steps state) '())
    (clear-plan-step-approvals state)
    (dolist (step parsed)
      (add-plan-step (getf step :description)
                     :file-paths (getf step :file-paths)
                     :risk (getf step :risk)
                     :depends-on (getf step :depends-on)
                     :state state))
    (length parsed)))

(defun current-plan-mode-state ()
  (or *plan-mode-state*
      (setf *plan-mode-state* (%make-plan-mode-state))))

(defun plan-mode-active-p (&optional (state (current-plan-mode-state)))
  (and (plan-mode-state-p state)
       (plan-mode-state-active-p state)))

(defun clear-plan-mode-exploration (&optional (state (current-plan-mode-state)))
  (check-type state plan-mode-state)
  (setf (plan-mode-state-exploration-call-count state) 0
        (plan-mode-state-exploration-tool-names state) '()
        (plan-mode-state-exploration-last-updated-at state) nil)
  state)

(defun record-plan-mode-exploration (tool-name &key
                                                (state (current-plan-mode-state))
                                                (successful-p t))
  (check-type state plan-mode-state)
  (let ((normalized-tool-name (%normalize-plan-tool-name tool-name)))
    (when (and successful-p
               (plan-mode-exploration-tool-p normalized-tool-name))
      (incf (plan-mode-state-exploration-call-count state))
      (setf (plan-mode-state-exploration-tool-names state)
            (sort (remove-duplicates
                   (cons normalized-tool-name
                         (plan-mode-state-exploration-tool-names state))
                   :test #'string=)
                  #'string<)
            (plan-mode-state-exploration-last-updated-at state)
            (get-universal-time))))
  state)

(defun plan-mode-exploration-snapshot (&optional (state (current-plan-mode-state)))
  (check-type state plan-mode-state)
  (let ((call-count (max 0 (or (plan-mode-state-exploration-call-count state) 0))))
    (list :call-count call-count
          :tool-names (copy-list (or (plan-mode-state-exploration-tool-names state) '()))
          :last-updated-at (plan-mode-state-exploration-last-updated-at state)
          :complete-p (plusp call-count))))

(defun plan-mode-exploration-complete-p (&optional (state (current-plan-mode-state)))
  (not (null (getf (plan-mode-exploration-snapshot state) :complete-p))))

(defun clear-plan-mode-steps (&optional (state (current-plan-mode-state)))
  (check-type state plan-mode-state)
  (setf (plan-mode-state-steps state) '())
  (clear-plan-mode-exploration state)
  (clear-plan-step-approvals state)
  state)

(defun add-plan-step (description &key file-paths (risk :medium) depends-on
                                     (state (current-plan-mode-state)))
  (check-type state plan-mode-state)
  (let ((next-index (1+ (length (plan-mode-state-steps state)))))
    (push (make-plan-step :index next-index
                          :description (%safe-plan-string description "Describe the step.")
                          :file-paths (%normalize-path-list file-paths)
                          :risk (%normalize-plan-risk risk)
                          :depends-on (%infer-step-dependencies description
                                                                depends-on
                                                                next-index))
          (plan-mode-state-steps state))
    (setf (plan-mode-state-steps state)
          (sort (copy-list (plan-mode-state-steps state)) #'< :key #'plan-step-index)))
  state)

(defun reorder-plan-step (from-index to-index &key (state (current-plan-mode-state)))
  (check-type state plan-mode-state)
  (let* ((steps (sort (copy-list (plan-mode-state-steps state))
                      #'<
                      :key #'plan-step-index))
         (step-count (length steps)))
    (unless (and (integerp from-index)
                 (integerp to-index)
                 (<= 1 from-index step-count)
                 (<= 1 to-index step-count))
      (return-from reorder-plan-step nil))
    (when (= from-index to-index)
      (return-from reorder-plan-step state))
    (let* ((source-position (1- from-index))
           (target-position (1- to-index))
           (moved-step (nth source-position steps))
           (remaining-steps (append (subseq steps 0 source-position)
                                    (subseq steps (1+ source-position))))
           (reordered-steps (append (subseq remaining-steps 0 target-position)
                                    (list moved-step)
                                    (subseq remaining-steps target-position)))
           (old-index->new-index (make-hash-table :test #'eql))
           (old-dependencies-by-step (make-hash-table :test #'eq)))
      (dolist (step reordered-steps)
        (setf (gethash step old-dependencies-by-step)
              (copy-list (plan-step-depends-on step))))
      (loop for step in reordered-steps
            for new-index from 1 do
              (setf (gethash (plan-step-index step) old-index->new-index) new-index
                    (plan-step-index step) new-index))
      (dolist (step reordered-steps)
        (let ((remapped-dependencies
                (loop for prior-index in (gethash step old-dependencies-by-step)
                      for mapped-index = (gethash prior-index old-index->new-index)
                      when (integerp mapped-index)
                        collect mapped-index)))
          (setf (plan-step-depends-on step)
                (sort (remove-duplicates remapped-dependencies :test #'=) #'<))))
      (setf (plan-mode-state-steps state) reordered-steps
            (plan-mode-state-approved-step-indexes state)
            (%normalize-step-indexes
             (loop for approved-index in (plan-mode-state-approved-step-indexes state)
                   for mapped-index = (gethash approved-index old-index->new-index)
                   when (integerp mapped-index)
                     collect mapped-index)
             step-count))))
  state)

(defun %plan-step-markdown (step stream approved-p)
  (format stream "~D. ~A~%"
          (or (plan-step-index step) 0)
          (%safe-plan-string (plan-step-description step) "Describe the step."))
  (format stream "   - risk: ~A~%"
          (string-downcase (symbol-name (%normalize-plan-risk (plan-step-risk step)))))
  (when (plan-step-file-paths step)
    (format stream "   - files: ~{`~A`~^, ~}~%"
            (%normalize-path-list (plan-step-file-paths step))))
  (when (plan-step-depends-on step)
    (format stream "   - depends_on: ~{~A~^, ~}~%"
            (plan-step-depends-on step)))
  (format stream "   - approved_for_execution: ~A~%"
          (if approved-p "true" "false")))

(defun %plan-markdown (state reason)
  (with-output-to-string (stream)
    (let* ((steps (plan-mode-state-steps state))
           (review-snapshot (plan-review-snapshot state))
           (approved-step-indexes (getf review-snapshot :approved-step-indexes))
           (input-gating-reason (getf review-snapshot :input-gating-reason)))
      (format stream "# Amoebum Plan~%~%")
      (format stream "- generated_at: ~A~%" (%timestamp-iso8601))
      (format stream "- exit_reason: ~A~%"
              (if reason
                  (%safe-plan-string reason "manual-exit")
                  "manual-exit"))
      (format stream "- step_count: ~D~%~%" (length steps))
      (format stream "- approved_step_count: ~D~%~%"
              (length approved-step-indexes))
      (format stream "- review_decision: ~(~A~)~%"
              (getf review-snapshot :review-decision))
      (format stream "- review_pending: ~:[false~;true~]~%"
              (getf review-snapshot :review-pending-p))
      (format stream "- approved_steps: ~:[none~;~:*~{~D~^, ~}~]~%"
              approved-step-indexes)
      (format stream "- input_gating_reason: ~(~A~)~%"
              (or input-gating-reason :open))
      (format stream "- terminal_stdin_enabled: ~:[false~;true~]~%"
              (getf review-snapshot :terminal-stdin-enabled-p))
      (format stream "- execution_pathways_enabled: ~:[false~;true~]~%~%"
              (getf review-snapshot :execution-pathways-enabled-p))
      (format stream "## Steps~%~%")
      (if steps
          (dolist (step steps)
            (%plan-step-markdown step
                                 stream
                                 (member (plan-step-index step)
                                         approved-step-indexes
                                         :test #'=)))
          (format stream "1. No explicit steps captured.~%")))))

(defun plan-markdown (&key
                        (state (current-plan-mode-state))
                        reason)
  (check-type state plan-mode-state)
  (%plan-markdown state reason))

(defun refresh-plan-review-markdown (&optional (state (current-plan-mode-state)))
  (check-type state plan-mode-state)
  (when (stringp (plan-mode-state-last-plan-markdown state))
    (setf (plan-mode-state-last-plan-markdown state)
          (%plan-markdown state
                          (or (plan-mode-state-last-exit-reason state)
                              :review-update))))
  state)

(defun default-plan-output-path (&key project-root (timestamp (get-universal-time)))
  (let* ((root-path
           (uiop:ensure-directory-pathname
            (or project-root
                (ignore-errors (config-project-root (current-config)))
                *default-pathname-defaults*)))
         (output-dir (merge-pathnames #P".amoebum/plans/" root-path))
         (filename (format nil "plan-~A.md" (%timestamp-compact timestamp)))
         (output-path (merge-pathnames filename output-dir)))
    (ensure-directories-exist output-path)
    output-path))

(defun write-plan-output (&key
                            (state (current-plan-mode-state))
                            output-path
                            reason)
  (check-type state plan-mode-state)
  (let ((resolved-output-path (or output-path
                                  (default-plan-output-path))))
    (ensure-directories-exist resolved-output-path)
    (with-open-file (stream resolved-output-path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string (%plan-markdown state reason) stream))
    (setf (plan-mode-state-last-output-path state) resolved-output-path
          (plan-mode-state-last-exit-reason state) reason)
    resolved-output-path))

(defun enter-plan-mode (&key
                          (state (current-plan-mode-state))
                          (clear-steps-p t))
  (check-type state plan-mode-state)
  (when clear-steps-p
    (clear-plan-mode-steps state))
  (setf (plan-mode-state-active-p state) t
        (plan-mode-state-entered-at state) (get-universal-time)
        (plan-mode-state-exited-at state) nil
        (plan-mode-state-approved-step-indexes state) '()
        (plan-mode-state-execution-pathways-enabled-p state) nil
        (plan-mode-state-review-pending-p state) nil
        (plan-mode-state-review-decision state) :pending
        (plan-mode-state-review-notes state) nil
        (plan-mode-state-review-decided-at state) nil
        (plan-mode-state-review-last-presented-at state) nil
        (plan-mode-state-exploration-call-count state) 0
        (plan-mode-state-exploration-tool-names state) '()
        (plan-mode-state-exploration-last-updated-at state) nil
        (plan-mode-state-last-plan-markdown state) nil)
  state)

(defun exit-plan-mode (&key
                         (state (current-plan-mode-state))
                         output-path
                         (reason :user-approved-plan)
                         (write-output-p t))
  (check-type state plan-mode-state)
  (when (plan-mode-state-active-p state)
    (let ((captured-plan (%plan-markdown state reason)))
      (setf (plan-mode-state-last-plan-markdown state) captured-plan
            (plan-mode-state-approved-step-indexes state) '()
            (plan-mode-state-execution-pathways-enabled-p state) nil
            (plan-mode-state-review-pending-p state) t
            (plan-mode-state-review-decision state) :pending
            (plan-mode-state-review-notes state) nil
            (plan-mode-state-review-decided-at state) nil)))
  (let ((written-output-path
          (and (plan-mode-state-active-p state)
               write-output-p
               (write-plan-output :state state
                                  :output-path output-path
                                  :reason reason))))
    (when (and (plan-mode-state-active-p state)
               (not write-output-p))
      (setf (plan-mode-state-last-output-path state) nil
            (plan-mode-state-last-exit-reason state) reason))
    (setf (plan-mode-state-active-p state) nil
          (plan-mode-state-exited-at state) (get-universal-time))
    (values state written-output-path)))

(defun toggle-plan-mode (&key
                           (state (current-plan-mode-state))
                           output-path
                           (reason :toggle)
                           (write-output-p t))
  (check-type state plan-mode-state)
  (if (plan-mode-state-active-p state)
      (multiple-value-bind (updated-state written-output-path)
          (exit-plan-mode :state state
                          :output-path output-path
                          :reason reason
                          :write-output-p write-output-p)
        (values updated-state :disabled written-output-path))
      (values (enter-plan-mode :state state :clear-steps-p t) :enabled nil)))
