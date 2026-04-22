(in-package :amoebum)

;;;; ---------------------------------------------------------------------------
;;;; Skill review-report helpers.
;;;;
;;;; Support for the `/review` built-in skill: finding normalization, severity
;;;; ranking, machine-readable report construction, human-rendering, and the
;;;; default LLM-backed analyzer that drives the report when a real diff is
;;;; present. `*skill-review-analyzer*` is wired here (lazy default) so that
;;;; builtins.lisp loads with the registry-level global already populated.
;;;;
;;;; Behavior is preserved verbatim from the original
;;;; `amoebum/src/macros/defskill.lisp`; only file boundaries change.
;;;; ---------------------------------------------------------------------------

(defun %skill-review-table (&rest pairs)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr do
      (setf (gethash key table) value))
    table))

(defun %skill-review-empty-p (value)
  (or (null value)
      (and (stringp value)
           (zerop (length (%slash-trim value))))))

(defun %skill-review-seq->list (value)
  (cond
    ((vectorp value)
     (loop for item across value collect item))
    ((listp value) value)
    (t '())))

(defun %skill-review-json-string (value)
  (handler-case
      (jonathan:to-json value)
    (error ()
      "{}")))

(defun %skill-review-normalize-severity (value)
  (let* ((raw (%slash-trim (if value (princ-to-string value) "")))
         (normalized (string-downcase raw)))
    (cond
      ((or (string= normalized "critical")
           (string= normalized "blocker"))
       "critical")
      ((or (string= normalized "high")
           (string= normalized "error"))
       "high")
      ((or (string= normalized "medium")
           (string= normalized "med")
           (string= normalized "warning"))
       "medium")
      ((string= normalized "low")
       "low")
      ((or (string= normalized "info")
           (string= normalized "informational"))
       "info")
      (t
       "medium"))))

(defun %skill-review-severity-rank (severity)
  (position (%skill-review-normalize-severity severity)
            '("critical" "high" "medium" "low" "info")
            :test #'string=))

(defun %skill-review-safe-int (value)
  (cond
    ((integerp value) value)
    ((and (stringp value)
          (plusp (length (%slash-trim value))))
     (handler-case
         (parse-integer (%slash-trim value))
       (error ()
         nil)))
    (t
     nil)))

(defun %skill-review-title-from-detail (detail index)
  (let* ((trimmed (%slash-trim (or detail "")))
         (max-len 72))
    (cond
      ((plusp (length trimmed))
       (if (> (length trimmed) max-len)
           (format nil "~A..." (subseq trimmed 0 max-len))
           trimmed))
      (t
       (format nil "Finding ~D" index)))))

(defun %skill-review-findings-from-text (text)
  (let ((findings '()))
    (when (and (stringp text)
               (plusp (length (%slash-trim text))))
      (with-input-from-string (stream text)
        (loop for line = (read-line stream nil nil)
              while line do
                (let ((trimmed (%slash-trim line)))
                  (when (plusp (length trimmed))
                    (let ((matched-p nil))
                      (cl-ppcre:register-groups-bind (severity detail)
                          ("(?i)^(?:[-*]\\s*|\\d+[\\).]\\s*)?(critical|high|medium|low|info|warning|error)\\s*[:\\-]\\s*(.+)$"
                           trimmed)
                        (setf matched-p t)
                        (push (%skill-review-table
                               "severity" (%skill-review-normalize-severity severity)
                               "title" (%skill-review-title-from-detail detail
                                                                        (1+ (length findings)))
                               "detail" (%slash-trim detail))
                              findings))
                      (unless matched-p
                        (cl-ppcre:register-groups-bind (detail)
                            ("^(?:[-*]\\s*|\\d+[\\).]\\s+)(.+)$" trimmed)
                          (push (%skill-review-table
                                 "severity" "medium"
                                 "title" (%skill-review-title-from-detail detail
                                                                          (1+ (length findings)))
                                 "detail" (%slash-trim detail))
                                findings)))))))))
    (nreverse findings)))

(defun %skill-review-normalize-finding (value index)
  (let* ((severity (%skill-review-normalize-severity
                    (or (%skill-plist-entry value :severity)
                        (%skill-plist-entry value "severity")
                        "medium")))
         (title (or (%skill-plist-entry value :title)
                    (%skill-plist-entry value "title")
                    (%skill-plist-entry value :summary)
                    (%skill-plist-entry value "summary")
                    (%skill-plist-entry value :message)
                    (%skill-plist-entry value "message")))
         (detail (or (%skill-plist-entry value :detail)
                     (%skill-plist-entry value "detail")
                     (%skill-plist-entry value :description)
                     (%skill-plist-entry value "description")
                     (%skill-plist-entry value :rationale)
                     (%skill-plist-entry value "rationale")
                     title))
         (file (or (%skill-plist-entry value :file)
                   (%skill-plist-entry value "file")
                   (%skill-plist-entry value :path)
                   (%skill-plist-entry value "path")))
         (line (%skill-review-safe-int
                (or (%skill-plist-entry value :line)
                    (%skill-plist-entry value "line")
                    (%skill-plist-entry value :line-number)
                    (%skill-plist-entry value "line_number"))))
         (title* (%slash-trim (or (and title (princ-to-string title)) "")))
         (detail* (%slash-trim (or (and detail (princ-to-string detail)) ""))))
    (%skill-review-table
     "severity" severity
     "title" (if (plusp (length title*))
                 title*
                 (%skill-review-title-from-detail detail* index))
     "detail" detail*
     "file" (and (not (%skill-review-empty-p file))
                 (%slash-trim (princ-to-string file)))
     "line" line)))

(defun %skill-review-finding< (left right)
  (let* ((left-rank (or (%skill-review-severity-rank (gethash "severity" left))
                        99))
         (right-rank (or (%skill-review-severity-rank (gethash "severity" right))
                         99))
         (left-file (or (gethash "file" left) ""))
         (right-file (or (gethash "file" right) ""))
         (left-line (or (gethash "line" left) 0))
         (right-line (or (gethash "line" right) 0))
         (left-title (or (gethash "title" left) ""))
         (right-title (or (gethash "title" right) "")))
    (or (< left-rank right-rank)
        (and (= left-rank right-rank)
             (or (string< left-file right-file)
                 (and (string= left-file right-file)
                      (or (< left-line right-line)
                          (and (= left-line right-line)
                               (string< left-title right-title)))))))))

(defun %skill-review-normalize-findings (raw-findings &key fallback-text)
  (let* ((seed (append (%skill-review-seq->list raw-findings)
                       (if (or raw-findings
                               (%skill-review-empty-p fallback-text))
                           '()
                           (%skill-review-findings-from-text fallback-text))))
         (normalized
           (loop for item in seed
                 for index from 1
                 collect (cond
                           ((hash-table-p item)
                            (%skill-review-normalize-finding item index))
                           ((and (listp item) (keywordp (first item)))
                            (%skill-review-normalize-finding item index))
                           ((stringp item)
                            (%skill-review-normalize-finding
                             (list :detail item :title (%skill-review-title-from-detail item index))
                             index))
                           (t
                            (%skill-review-normalize-finding
                             (list :detail (princ-to-string item))
                             index)))))
         (sorted (stable-sort (copy-list normalized) #'%skill-review-finding<)))
    (loop for finding in sorted
          for index from 1
          do (setf (gethash "id" finding) (format nil "R~D" index)))
    sorted))

(defun %skill-review-strip-fence (text)
  (let ((trimmed (%slash-trim text)))
    (if (and (>= (length trimmed) 6)
             (uiop:string-prefix-p "```" trimmed)
             (uiop:string-suffix-p "```" trimmed))
        (let* ((first-break (position #\Newline trimmed))
               (body (if first-break
                         (subseq trimmed (1+ first-break))
                         trimmed))
               (last-fence (search "```" body :from-end t)))
          (if last-fence
              (%slash-trim (subseq body 0 last-fence))
              (%slash-trim body)))
        trimmed)))

(defun %skill-review-parse-json-text (text)
  (handler-case
      (jonathan:parse (%skill-review-strip-fence text) :as :hash-table)
    (error ()
      nil)))

(defun %skill-review-coerce-analysis-object (analysis)
  (cond
    ((hash-table-p analysis)
     analysis)
    ((and (listp analysis)
          (keywordp (first analysis)))
     (let ((table (make-hash-table :test #'equal)))
       (loop for (key value) on analysis by #'cddr do
         (let ((name (if (keywordp key)
                         (string-downcase (symbol-name key))
                         (string-downcase (princ-to-string key)))))
           (setf (gethash name table) value)))
       table))
    ((and (stringp analysis)
          (plusp (length (%slash-trim analysis))))
     (%skill-review-parse-json-text analysis))
    (t
     nil)))

(defun %skill-review-build-report (diff-data analysis)
  (let* ((branch (or (%skill-plist-entry diff-data :branch) "unknown"))
         (base (or (%skill-plist-entry diff-data :base-branch) "unknown"))
         (range (or (%skill-plist-entry diff-data :range) ""))
         (diff-summary (or (%skill-plist-entry diff-data :summary) ""))
         (diff-text (or (%skill-plist-entry diff-data :diff) ""))
         (truncated-p (not (null (%skill-plist-entry diff-data :truncated-p))))
         (analysis-text (and (stringp analysis) (%slash-trim analysis)))
         (analysis-object (%skill-review-coerce-analysis-object analysis))
         (raw-summary (or (and analysis-object
                               (or (%skill-plist-entry analysis-object :summary)
                                   (%skill-plist-entry analysis-object "summary")))
                          (and (stringp analysis-text)
                               (plusp (length analysis-text))
                               analysis-text)
                          diff-summary
                          ""))
         (findings (%skill-review-normalize-findings
                    (and analysis-object
                         (or (%skill-plist-entry analysis-object :findings)
                             (%skill-plist-entry analysis-object "findings")))
                    :fallback-text analysis-text))
         (status
           (cond
             ((%skill-review-empty-p diff-text) "missing-diff")
             ((plusp (length findings)) "findings-present")
             (t "no-findings"))))
    (%skill-review-table
     "schema_version" *skill-review-schema-version*
     "status" status
     "branch" branch
     "base_branch" base
     "range" range
     "summary" (if (%skill-review-empty-p raw-summary)
                   "No review summary available."
                   (%slash-trim (princ-to-string raw-summary)))
     "diff_summary" (or diff-summary "")
     "findings_count" (length findings)
     "findings" (coerce findings 'vector)
     "truncated_diff" truncated-p)))

(defun %skill-review-render-human (report)
  (let* ((findings (%skill-review-seq->list (gethash "findings" report)))
         (status (or (gethash "status" report) "no-findings")))
    (with-output-to-string (out)
      (format out "Review report (~A vs ~A):~%"
              (or (gethash "branch" report) "unknown")
              (or (gethash "base_branch" report) "unknown"))
      (format out "Status: ~A~%" status)
      (format out "Summary: ~A~%" (or (gethash "summary" report) ""))
      (if findings
          (progn
            (format out "Findings:~%")
            (loop for finding in findings
                  for index from 1 do
                    (format out "~D. [~A] ~A~%"
                            index
                            (or (gethash "severity" finding) "medium")
                            (or (gethash "title" finding) "Finding"))
                    (when (plusp (length (%slash-trim (or (gethash "file" finding) ""))))
                      (format out "   File: ~A~%"
                              (gethash "file" finding)))
                    (let ((line (gethash "line" finding)))
                      (when (integerp line)
                        (format out "   Line: ~D~%" line)))
                    (let ((detail (%slash-trim (or (gethash "detail" finding) ""))))
                      (when (plusp (length detail))
                        (format out "   Detail: ~A~%" detail)))))
          (format out "Findings: none.~%"))
      (format out "Machine-readable payload:~%```json~%~A~%```"
              (%skill-review-json-string report)))))

(defun %skill-review-fallback (diff-data)
  (let* ((branch (or (%skill-plist-entry diff-data :branch) "unknown"))
         (base (or (%skill-plist-entry diff-data :base-branch) "unknown"))
         (summary (or (%skill-plist-entry diff-data :summary) "No diff summary available."))
         (files (or (%skill-plist-entry diff-data :files-changed) '()))
         (file-count (if (listp files) (length files) 0)))
    (with-output-to-string (out)
      (format out "Review summary (~A vs ~A):~%" branch base)
      (format out "- ~A~%" summary)
      (format out "- Changed files: ~D~%" file-count)
      (when (and (listp files) files)
        (format out "- Files: ~{~A~^, ~}" files)))))

(defun %default-skill-review-analyzer (diff-data &key model)
  (let* ((diff (or (%skill-plist-entry diff-data :diff) ""))
         (summary (or (%skill-plist-entry diff-data :summary) ""))
         (branch (or (%skill-plist-entry diff-data :branch) ""))
         (base (or (%skill-plist-entry diff-data :base-branch) ""))
         (effective-model (or model
                              (ignore-errors (config-model (current-config)))
                              "moonshot-v1-128k")))
    (if (or (not (stringp diff))
            (zerop (length (%slash-trim diff))))
        "No branch diff found to review."
        (handler-case
            (let* ((client (pseudopod:make-client :model effective-model))
                   (prompt (format nil
                                   "Review this git diff and respond with JSON only.~%\
Output schema: {\"summary\":string,\"findings\":[{\"severity\":\"high|medium|low|critical|info\",\"title\":string,\"detail\":string,\"file\":string? ,\"line\":number?}]}~%\
Focus on correctness risks, missing tests, and regressions. If no issues, return findings=[].~2%\
Branch: ~A~%\
Base: ~A~%\
Summary: ~A~2%\
Diff:~%~A"
                                   branch
                                   base
                                   summary
                                   diff))
                   (response (pseudopod:chat-completion*
                              client
                              prompt
                              :system-prompt
                              "You are a strict code reviewer. Return valid JSON only, no markdown. Keep findings concrete and prioritized."))
                   (text (%slash-trim (%skill-message->text response))))
              (if (plusp (length text))
                  text
                  (%skill-review-fallback diff-data)))
          (error ()
            (%skill-review-fallback diff-data))))))

(unless (functionp *skill-review-analyzer*)
  (setf *skill-review-analyzer* #'%default-skill-review-analyzer))
