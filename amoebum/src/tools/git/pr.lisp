(in-package :amoebum)

(defun %git-branch-commits (root range)
  (let* ((result (%git-run-command-or-error
                  root
                  (list "log"
                        "--reverse"
                        "--format=%H%x1f%s%x1f%b%x1e"
                        range)))
         (records (%git-split-on-char (getf result :stdout) #\u001E))
         (commits
           (loop for record in records
                 for trimmed = (%git-trim-whitespace record)
                 for parsed = (and (plusp (length trimmed))
                                   (%git-parse-log-record trimmed))
                 when parsed
                   collect parsed)))
    commits))

(defun %git-truncate-for-prompt (text limit)
  (let ((value (or text "")))
    (if (> (length value) limit)
        (subseq value 0 limit)
        value)))

(defun %git-build-pr-prompt (branch base-branch range commits)
  (let ((details
          (with-output-to-string (stream)
            (loop for commit in commits
                  for index from 1 do
                    (format stream "~D. ~A ~A~%"
                            index
                            (subseq (getf commit :sha)
                                    0
                                    (min 12 (length (getf commit :sha))))
                            (or (getf commit :subject) ""))
                    (when (plusp (length (or (getf commit :body) "")))
                      (format stream "   ~A~%"
                              (cl-ppcre:regex-replace-all
                               "\\s+"
                               (getf commit :body)
                               " ")))))))
    (%git-truncate-for-prompt
     (format nil
             "Create a GitHub pull request title/body for this branch.~%\
Rules:~%\
- Use ALL commits listed below (not just latest).~%\
- Title must be under 70 characters.~%\
- Body must include '## Summary' bullet list and '## Test Plan' section.~%\
- Mention key behavior changes and risks clearly.~%\
- Return exactly this format:~%\
  TITLE: <title>~%\
  BODY:~%\
  <markdown body>~2%\
Branch: ~A~%\
Base: ~A~%\
Range: ~A~%\
Commit count: ~D~2%\
Commits:~%~A"
             branch
             base-branch
             range
             (length commits)
             details)
     *git-max-pr-context-chars*)))

(defun %git-normalize-pr-title (title)
  (let* ((trimmed (%git-trim-whitespace title))
         (collapsed (cl-ppcre:regex-replace-all "\\s+" trimmed " "))
         (max-title-length 69))
    (when (zerop (length collapsed))
      (error "Generated PR title is empty."))
    (if (> (length collapsed) max-title-length)
        (%git-trim-whitespace (subseq collapsed 0 max-title-length))
        collapsed)))

(defun %git-normalize-pr-body (body)
  (let ((trimmed (%git-trim-whitespace body)))
    (when (zerop (length trimmed))
      (error "Generated PR body is empty."))
    trimmed))

(defun %git-parse-generated-pr-description (text)
  (let* ((normalized (%git-strip-code-fence text))
         (lines (%git-lines normalized))
         (title nil)
         (body-lines '())
         (in-body nil))
    (dolist (line lines)
      (cond
        ((and (not in-body)
              (%git-prefix-ci-p "TITLE:" line))
         (setf title (%git-trim-whitespace (subseq line (length "TITLE:")))))
        ((%git-prefix-ci-p "BODY:" line)
         (setf in-body t)
         (let ((inline (%git-trim-whitespace (subseq line (length "BODY:")))))
           (when (plusp (length inline))
             (push inline body-lines))))
        (in-body
         (push line body-lines))))
    (unless title
      (error "PR generator did not return TITLE: line."))
    (unless body-lines
      (error "PR generator did not return BODY: content."))
    (list :title (%git-normalize-pr-title title)
          :body (%git-normalize-pr-body
                 (format nil "~{~A~%~}" (nreverse body-lines))))))

(defun %git-fallback-pr-description (branch commits)
  (let* ((subjects
           (loop for commit in commits
                 for subject = (%git-trim-whitespace (or (getf commit :subject) ""))
                 unless (zerop (length subject))
                   collect subject))
         (title
           (%git-normalize-pr-title
            (if subjects
                (first subjects)
                (format nil "chore: update ~A branch changes" branch))))
         (summary-lines
           (or (loop for subject in subjects
                     for index from 1
                     while (<= index 8)
                     collect (format nil "- ~A" subject))
               (list "- Update branch changes.")))
         (body
           (%git-normalize-pr-body
            (format nil
                    "## Summary~%~{~A~%~}~%## Test Plan~%- [ ] Not run (describe validation before merge)"
                    summary-lines))))
    (list :title title :body body)))

(defun %generate-pr-description-via-llm (commits
                                         &key branch base-branch range model project-root
                                         merge-base)
  (declare (ignore merge-base project-root))
  (let* ((resolved-model (%git-commit-model model))
         (client (pseudopod:make-client :model resolved-model))
         (prompt (%git-build-pr-prompt branch base-branch range commits))
         (response
           (pseudopod:chat-completion*
            client
            prompt
            :system-prompt
            "You write precise GitHub pull request metadata. Follow requested TITLE/BODY format exactly.")))
    (%git-parse-generated-pr-description (%git-message->text response))))

(unless *git-pr-description-generator*
  (setf *git-pr-description-generator* #'%generate-pr-description-via-llm))

(unless *git-pr-command-runner*
  (setf *git-pr-command-runner* #'%git-run-bash-command))

(defun %git-generate-pr-description (branch commits base-branch range merge-base root model)
  (let ((generator (or *git-pr-description-generator*
                       #'%generate-pr-description-via-llm)))
    (handler-case
        (let ((generated
                (funcall generator
                         commits
                         :branch branch
                         :base-branch base-branch
                         :range range
                         :merge-base merge-base
                         :model model
                         :project-root root)))
          (list :title (%git-normalize-pr-title (getf generated :title))
                :body (%git-normalize-pr-body (getf generated :body))
                :source :llm
                :fallback-reason nil))
      (error (condition)
        (let ((fallback (%git-fallback-pr-description branch commits)))
          (list :title (getf fallback :title)
                :body (getf fallback :body)
                :source :fallback
                :fallback-reason (princ-to-string condition)))))))

(defun %git-create-pr-via-gh! (root base-branch branch title body)
  (let* ((marker (%git-heredoc-marker))
         (command
           (format nil
                   "gh pr create --base ~A --head ~A --title ~A --body-file - <<'~A'~%~A~%~A"
                   (%git-shell-quote base-branch)
                   (%git-shell-quote branch)
                   (%git-shell-quote title)
                   marker
                   body
                   marker))
         (runner (or *git-pr-command-runner* #'%git-run-bash-command))
         (result (funcall runner root command))
         (combined-output
           (format nil "~A~%~A" (getf result :stdout) (getf result :stderr)))
         (url (%git-extract-pr-url combined-output)))
    (unless (zerop (getf result :exit-code))
      (error "gh pr create failed: ~A"
             (%git-trim-whitespace combined-output)))
    (unless (and (stringp url) (plusp (length url)))
      (error "gh pr create succeeded but no PR URL was found in output: ~A"
             (%git-trim-whitespace combined-output)))
    (list :url url
          :stdout (getf result :stdout)
          :stderr (getf result :stderr)
          :command command)))

(defun %git-create-pr-tool-data (&key base-branch model project-root)
  (let* ((root (%git-ensure-work-tree (%git-project-root project-root)))
         (branch (%git-current-branch root)))
    (when (or (zerop (length branch))
              (string= branch "HEAD"))
      (error "Cannot create PR from detached HEAD. Check out a branch first."))
    (let* ((resolved-base (%git-resolve-base-branch root base-branch))
           (merge-base (%git-merge-base root resolved-base))
           (range (format nil "~A..HEAD" merge-base))
           (commits (%git-branch-commits root range)))
      (when (null commits)
        (error "No commits found between ~A and HEAD; nothing to open in PR." resolved-base))
      (let* ((description
               (%git-generate-pr-description branch
                                            commits
                                            resolved-base
                                            range
                                            merge-base
                                            root
                                            model))
             (push-result (%git-push-set-upstream-if-needed! root branch))
             (upstream (or (getf push-result :upstream)
                           (%git-upstream-branch root)))
             (pr-result
               (%git-create-pr-via-gh!
                root
                resolved-base
                branch
                (getf description :title)
                (getf description :body))))
        (%git-publish-lifecycle-event
         +event-type-git-branch+
         (make-branch-event :old-branch resolved-base
                            :new-branch branch
                            :action :create-pr))
        (list :url (getf pr-result :url)
              :branch branch
              :base-branch resolved-base
              :merge-base merge-base
              :range range
              :commit-count (length commits)
              :commits-analyzed
              (mapcar (lambda (commit)
                        (list :sha (getf commit :sha)
                              :subject (getf commit :subject)))
                      commits)
              :title (getf description :title)
              :body (getf description :body)
              :description-source (getf description :source)
              :fallback-reason (getf description :fallback-reason)
              :auto-pushed-p (getf push-result :pushed-p)
              :upstream upstream)))))
