(in-package :amoebum)

(defun %git-normalize-stage-file (file root)
  (let* ((raw (typecase file
                (pathname (namestring file))
                (string file)
                (symbol (symbol-name file))
                (t (princ-to-string file))))
         (trimmed (%git-trim-whitespace raw)))
    (when (zerop (length trimmed))
      (error "Stage file path must not be empty."))
    (when (member trimmed '("." "./" "-A" "--all") :test #'string=)
      (error "Refusing broad staging path ~S. Stage explicit file names only." trimmed))
    (let* ((candidate (pathname trimmed))
           (relative (if (uiop:absolute-pathname-p candidate)
                         (%normalize-slashes (enough-namestring candidate root))
                         (%normalize-slashes trimmed))))
      (when (uiop:absolute-pathname-p (pathname relative))
        (error "Stage file path must be under repository root: ~A." trimmed))
      (when (or (uiop:string-prefix-p "../" relative)
                (string= relative ".."))
        (error "Stage file path escapes repository root: ~A." trimmed))
      (if (uiop:string-prefix-p "./" relative)
          (subseq relative 2)
          relative))))

(defun %git-normalize-stage-files (files root)
  (let* ((items
           (cond
             ((null files) nil)
             ((stringp files) (list files))
             ((pathnamep files) (list files))
             ((listp files) files)
             (t (error "FILES must be NIL, string, pathname, or list. Got ~S." files))))
         (normalized
           (loop for file in items
                 collect (%git-normalize-stage-file file root))))
    (%git-ensure-safe-paths normalized :reason "selected")
    normalized))

(defun %git-stage-files! (root files)
  (when files
    (%git-run-command-or-error root (append (list "add" "--") files)))
  files)

(defun %git-staged-paths (root)
  (let* ((result (%git-run-command-or-error root '("diff" "--cached" "--name-only")))
         (paths (remove-if (lambda (line) (zerop (length line)))
                           (mapcar #'%git-normalize-change-path
                                   (%git-lines (getf result :stdout))))))
    (%git-ensure-safe-paths paths :reason "staged")
    paths))

(defun %git-recent-commit-subjects (root &optional (count 20))
  (let* ((result (%git-run-command-or-error
                  root
                  (list "log"
                        (format nil "-n~D" (max 1 count))
                        "--pretty=format:%s"))))
    (remove-if (lambda (line) (zerop (length line)))
               (mapcar #'%git-trim-whitespace
                       (%git-lines (getf result :stdout))))))

(defun %git-normalize-generated-message (message)
  (let* ((normalized (%git-strip-code-fence message))
         (lines (%git-lines normalized))
         (kept
           (loop for line in lines
                 unless (cl-ppcre:scan "(?i)^co-authored-by\\s*:" line)
                   collect line))
         (result (%git-trim-whitespace (format nil "~{~A~%~}" kept))))
    (when (zerop (length result))
      (error "Generated commit message is empty."))
    result))

(defun %git-commit-model (explicit-model)
  (or explicit-model
      (ignore-errors (config-model (current-config)))
      "moonshot-v1-128k"))

(defun %git-build-commit-prompt (diff staged-paths recent-subjects)
  (format nil
          "Write a git commit message for this staged diff.~%\
Rules:~%\
- Match the repository style inferred from recent subjects.~%\
- Keep subject concise (<72 chars) and imperative.~%\
- Include body only if needed.~%\
- Output commit message text only (no markdown, no fences).~%\
- Do not include a Co-Authored-By line.~2%\
Recent subjects:~%~{* ~A~%~}~2%\
Staged files:~%~{* ~A~%~}~2%\
Staged diff:~%~A"
          recent-subjects
          staged-paths
          diff))

(defun %generate-commit-message-via-llm (diff recent-subjects
                                         &key model staged-paths project-root)
  (declare (ignore project-root))
  (let* ((resolved-model (%git-commit-model model))
         (client (pseudopod:make-client :model resolved-model))
         (prompt (%git-build-commit-prompt diff staged-paths recent-subjects))
         (response
           (pseudopod:chat-completion*
            client
            prompt
            :system-prompt
            "You generate high quality git commit messages. Respond with plain commit message text only.")))
    (%git-normalize-generated-message (%git-message->text response))))

(unless *git-commit-message-generator*
  (setf *git-commit-message-generator* #'%generate-commit-message-via-llm))

(defun %git-infer-fallback-prefix (recent-subjects)
  (let ((subject (first recent-subjects)))
    (or (and (stringp subject)
             (cl-ppcre:register-groups-bind (prefix)
                 ("^([a-z]+(?:\\([^)]+\\))?):" subject)
               prefix))
        "chore")))

(defun %git-fallback-commit-message (staged-paths recent-subjects)
  (let* ((prefix (%git-infer-fallback-prefix recent-subjects))
         (count (length staged-paths))
         (subject (format nil "~A: update ~D file~:P" prefix count))
         (samples (subseq staged-paths 0 (min 3 count))))
    (if samples
        (format nil "~A~2%Files: ~{~A~^, ~}" subject samples)
        subject)))

(defun %git-generate-commit-message (diff staged-paths recent-subjects root model)
  (let ((generator (or *git-commit-message-generator*
                       #'%generate-commit-message-via-llm)))
    (handler-case
        (let ((message
                (funcall generator
                         diff
                         recent-subjects
                         :model model
                         :staged-paths staged-paths
                         :project-root root)))
          (list :message (%git-normalize-generated-message message)
                :source :llm
                :fallback-reason nil))
      (error (condition)
        (list :message (%git-fallback-commit-message staged-paths recent-subjects)
              :source :fallback
              :fallback-reason (princ-to-string condition))))))

(defun %git-co-author-line (co-author)
  (let ((value (%git-trim-whitespace (or co-author *git-default-co-author*))))
    (if (zerop (length value))
        (format nil "Co-Authored-By: ~A" *git-default-co-author*)
        (format nil "Co-Authored-By: ~A" value))))

(defun %git-append-co-author (message co-author)
  (if (cl-ppcre:scan "(?im)^Co-Authored-By\\s*:" message)
      message
      (format nil "~A~2%~A"
              (%git-trim-whitespace message)
              (%git-co-author-line co-author))))

(defun %git-commit-via-heredoc! (root message amend-p)
  (let* ((marker (%git-heredoc-marker))
         (command
           (format nil "git commit~A -F - <<'~A'~%~A~%~A"
                   (if amend-p " --amend" "")
                   marker
                   message
                   marker)))
    (let ((result (%git-run-bash-command root command)))
      (unless (zerop (getf result :exit-code))
        (error "git commit failed: ~A"
               (%git-trim-whitespace
                (if (plusp (length (getf result :stderr)))
                    (getf result :stderr)
                    (getf result :stdout)))))
      result)))

(defun %git-commit-sha (root)
  (%git-trim-whitespace
   (getf (%git-run-command-or-error root '("rev-parse" "HEAD")) :stdout)))

(defun %git-commit-tool-data (&key files co-author model amend allow-amend project-root)
  (when (and amend (not allow-amend))
    (error "Refusing --amend without explicit ALLOW-AMEND=true confirmation."))
  (let* ((root (%git-ensure-work-tree (%git-project-root project-root)))
         (stage-files (%git-normalize-stage-files files root)))
    (%git-stage-files! root stage-files)
    (let ((staged-paths (%git-staged-paths root)))
      (when (null staged-paths)
        (error "No staged changes to commit."))
      (let* ((diff (%git-staged-diff root))
             (recent-subjects (%git-recent-commit-subjects root 20))
             (message-data (%git-generate-commit-message diff
                                                        staged-paths
                                                        recent-subjects
                                                        root
                                                        model))
             (generated-message (getf message-data :message))
             (commit-message (%git-append-co-author generated-message co-author)))
        (%git-commit-via-heredoc! root commit-message amend)
        (let* ((sha (%git-commit-sha root))
               (branch (%git-current-branch root))
               (result (list :sha sha
                             :branch branch
                             :files-changed staged-paths
                             :message commit-message
                             :message-summary (car (%git-lines commit-message))
                             :message-source (getf message-data :source)
                             :fallback-reason (getf message-data :fallback-reason))))
          (run-hooks :on-commit sha commit-message staged-paths)
          (%git-publish-lifecycle-event
           +event-type-git-commit+
           (make-commit-event :hash sha
                              :message commit-message
                              :author (%git-head-author root)
                              :files-changed staged-paths))
          (%git-publish-lifecycle-event
           +event-type-git-branch+
           (make-branch-event :old-branch branch
                              :new-branch branch
                              :action :commit)
           :severity :debug)
          result)))))
