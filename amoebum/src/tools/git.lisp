(in-package :amoebum)

(defparameter *git-default-co-author* "Amoebum <amoebum@local>")
(defparameter *git-max-diff-chars* 24000)
(defparameter *git-max-branch-diff-chars* 48000)
(defparameter *git-max-pr-context-chars* 36000)
(defparameter *git-commit-message-generator* nil)
(defparameter *git-pr-description-generator* nil)
(defparameter *git-pr-command-runner* nil)

(defun %git-trim-whitespace (value)
  (string-trim '(#\Space #\Tab #\Newline #\Return) (or value "")))

(defun %git-lines (text)
  (with-input-from-string (stream (or text ""))
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun %git-split-whitespace (line)
  (remove-if (lambda (part) (zerop (length part)))
             (cl-ppcre:split "\\s+" line)))

(defun %git-string-suffix-p (suffix string)
  (let ((suffix-length (length suffix))
        (string-length (length string)))
    (and (<= suffix-length string-length)
         (string= suffix string :start1 0 :end1 suffix-length
                              :start2 (- string-length suffix-length)
                              :end2 string-length))))

(defun %git-project-root (&optional project-root)
  (let* ((base (or project-root
                   (ignore-errors (config-project-root (current-config)))
                   *default-pathname-defaults*))
         (resolved (or (ignore-errors (truename base)) base))
         (directory (uiop:ensure-directory-pathname resolved)))
    (unless (probe-file directory)
      (error "Git project root does not exist: ~A." (%path-text directory)))
    directory))

(defun %git-run-command (root args)
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program (append (list "git") args)
                        :directory root
                        :ignore-error-status t
                        :output :string
                        :error-output :string)
    (list :stdout (or stdout "")
          :stderr (or stderr "")
          :exit-code (or exit-code 0))))

(defun %git-run-bash-command (root command)
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program (list "bash" "-lc" command)
                        :directory root
                        :ignore-error-status t
                        :output :string
                        :error-output :string)
    (list :stdout (or stdout "")
          :stderr (or stderr "")
          :exit-code (or exit-code 0))))

(defun %git-run-command-or-error (root args)
  (let ((result (%git-run-command root args)))
    (when (not (zerop (getf result :exit-code)))
      (error "Git command failed (~{~A~^ ~}): ~A"
             (append (list "git") args)
             (%git-trim-whitespace
              (if (plusp (length (getf result :stderr)))
                  (getf result :stderr)
                  (getf result :stdout)))))
    result))

(defun %git-inside-work-tree-p (root)
  (let* ((result (%git-run-command root '("rev-parse" "--is-inside-work-tree")))
         (value (%git-trim-whitespace (getf result :stdout))))
    (and (zerop (getf result :exit-code))
         (string= value "true"))))

(defun %git-ensure-work-tree (root)
  (unless (%git-inside-work-tree-p root)
    (error "Path is not a git work tree: ~A." (%path-text root)))
  root)

(defun %git-safe-parse-int (text)
  (handler-case
      (parse-integer text)
    (error () 0)))

(defun %git-normalize-change-path (path)
  (let ((normalized (%normalize-slashes (%git-trim-whitespace path))))
    (if (uiop:string-prefix-p "./" normalized)
        (subseq normalized 2)
        normalized)))

(defun %git-sensitive-path-p (path)
  (let ((value (string-downcase (%git-normalize-change-path path))))
    (or (cl-ppcre:scan "(^|/)\\.env(\\..*)?$" value)
        (cl-ppcre:scan "(^|/).*credentials.*" value)
        (cl-ppcre:scan "(^|/).*api[-_]?key.*" value))))

(defun %git-ensure-safe-paths (paths &key (reason "selected"))
  (let ((blocked (remove-if-not #'%git-sensitive-path-p paths)))
    (when blocked
      (error "Refusing commit: ~A files include probable secrets: ~{~A~^, ~}."
             reason
             blocked)))
  paths)

(defun %git-parse-status-porcelain (lines)
  (let ((branch "HEAD")
        (upstream nil)
        (ahead 0)
        (behind 0)
        (staged '())
        (unstaged '())
        (untracked '()))
    (labels ((push-unique (item list)
               (if (or (null item)
                       (zerop (length item))
                       (member item list :test #'string=))
                   list
                   (cons item list)))
             (tracked-path (line rename-p)
               (let ((parts (%git-split-whitespace line)))
                 (if rename-p
                     (and (>= (length parts) 3)
                          (nth (- (length parts) 2) parts))
                     (car (last parts))))))
      (dolist (line lines)
        (cond
          ((uiop:string-prefix-p "# branch.head " line)
           (setf branch (%git-trim-whitespace (subseq line (length "# branch.head ")))))
          ((uiop:string-prefix-p "# branch.upstream " line)
           (setf upstream (%git-trim-whitespace (subseq line (length "# branch.upstream ")))))
          ((uiop:string-prefix-p "# branch.ab " line)
           (let* ((parts (%git-split-whitespace line))
                  (ahead-token (or (third parts) "+0"))
                  (behind-token (or (fourth parts) "-0")))
             (setf ahead (%git-safe-parse-int (subseq ahead-token 1))
                   behind (%git-safe-parse-int (subseq behind-token 1)))))
          ((uiop:string-prefix-p "? " line)
           (let ((path (%git-normalize-change-path (subseq line 2))))
             (setf untracked (push-unique path untracked)
                   unstaged (push-unique path unstaged))))
          ((or (uiop:string-prefix-p "1 " line)
               (uiop:string-prefix-p "2 " line)
               (uiop:string-prefix-p "u " line))
           (let* ((parts (%git-split-whitespace line))
                  (xy (or (second parts) ".."))
                  (x (if (> (length xy) 0) (char xy 0) #\.))
                  (y (if (> (length xy) 1) (char xy 1) #\.))
                  (rename-p (uiop:string-prefix-p "2 " line))
                  (path (%git-normalize-change-path (tracked-path line rename-p))))
             (when (and path (> (length path) 0))
               (unless (char= x #\.)
                 (setf staged (push-unique path staged)))
               (unless (char= y #\.)
                 (setf unstaged (push-unique path unstaged)))))))))
    (list :branch branch
          :tracking (list :upstream upstream
                          :ahead ahead
                          :behind behind
                          :tracking-p (not (null upstream)))
          :staged (nreverse staged)
          :unstaged (nreverse unstaged)
          :untracked (nreverse untracked))))

(defun %git-status-data (&key project-root)
  (let* ((root (%git-ensure-work-tree (%git-project-root project-root)))
         (result (%git-run-command-or-error root '("status" "--porcelain=2" "--branch")))
         (parsed (%git-parse-status-porcelain (%git-lines (getf result :stdout)))))
    (append (list :project-root (%path-text root)) parsed)))

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

(defun %git-staged-diff (root)
  (let* ((result (%git-run-command-or-error root '("diff" "--cached" "--")))
         (diff (or (getf result :stdout) "")))
    (if (> (length diff) *git-max-diff-chars*)
        (subseq diff 0 *git-max-diff-chars*)
        diff)))

(defun %git-recent-commit-subjects (root &optional (count 20))
  (let* ((result (%git-run-command-or-error
                  root
                  (list "log"
                        (format nil "-n~D" (max 1 count))
                        "--pretty=format:%s"))))
    (remove-if (lambda (line) (zerop (length line)))
               (mapcar #'%git-trim-whitespace
                       (%git-lines (getf result :stdout))))))

(defun %git-content-part-text (part)
  (let ((type (string-downcase (or (pseudopod:content-part-type part) "text"))))
    (cond
      ((string= type "text")
       (or (pseudopod:content-part-text part) ""))
      ((string= type "think")
       (or (pseudopod:content-part-think part) ""))
      (t
       (or (pseudopod:content-part-text part)
           (pseudopod:content-part-think part)
           "")))))

(defun %git-message->text (message)
  (with-output-to-string (stream)
    (loop for part in (pseudopod:message-content message)
          for index from 0 do
            (when (> index 0)
              (write-char #\Newline stream))
            (write-string (%git-content-part-text part) stream))))

(defun %git-strip-code-fence (message)
  (let ((trimmed (%git-trim-whitespace message)))
    (if (and (>= (length trimmed) 6)
             (uiop:string-prefix-p "```" trimmed)
             (%git-string-suffix-p "```" trimmed))
        (%git-trim-whitespace
         (subseq trimmed 3 (- (length trimmed) 3)))
        trimmed)))

(defun %git-shell-quote (value)
  (let ((text (or value "")))
    (with-output-to-string (stream)
      (write-char #\' stream)
      (loop for char across text do
            (if (char= char #\')
                (write-string "'\"'\"'" stream)
                (write-char char stream)))
      (write-char #\' stream))))

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

(defun %git-heredoc-marker ()
  (format nil "AMOEBUM_HEREDOC_~D_~D"
          (get-universal-time)
          (random 999999)))

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

(defun %git-current-branch (root)
  (%git-trim-whitespace
   (getf (%git-run-command-or-error root '("rev-parse" "--abbrev-ref" "HEAD")) :stdout)))

(defun %git-revision-exists-p (root revision)
  (let ((result (%git-run-command root
                                  (list "rev-parse"
                                        "--verify"
                                        "--quiet"
                                        (format nil "~A^{commit}" revision)))))
    (zerop (getf result :exit-code))))

(defun %git-resolve-base-branch (root &optional explicit-base)
  (let ((candidate (%git-trim-whitespace explicit-base)))
    (cond
      ((plusp (length candidate))
       (unless (%git-revision-exists-p root candidate)
         (error "Base branch ~S was not found in repository." candidate))
       candidate)
      (t
       (or (find-if (lambda (branch) (%git-revision-exists-p root branch))
                    '("main" "master" "origin/main" "origin/master"))
           (error "Unable to resolve base branch. Tried main/master and origin/main/origin/master."))))))

(defun %git-merge-base (root base-branch)
  (let ((merge-base
          (%git-trim-whitespace
           (getf (%git-run-command-or-error root
                                            (list "merge-base" base-branch "HEAD"))
                 :stdout))))
    (when (zerop (length merge-base))
      (error "Unable to compute merge-base for ~A and HEAD." base-branch))
    merge-base))

(defun %git-parse-lines-non-empty (text)
  (remove-if (lambda (line) (zerop (length line)))
             (mapcar #'%git-trim-whitespace (%git-lines text))))

(defun %git-upstream-branch (root)
  (let* ((result (%git-run-command root
                                   '("rev-parse"
                                     "--abbrev-ref"
                                     "--symbolic-full-name"
                                     "@{u}")))
         (upstream (%git-trim-whitespace (getf result :stdout))))
    (when (and (zerop (getf result :exit-code))
               (plusp (length upstream)))
      upstream)))

(defun %git-push-set-upstream-if-needed! (root branch)
  (let ((upstream (%git-upstream-branch root)))
    (if upstream
        (list :pushed-p nil :upstream upstream)
        (progn
          (%git-run-command-or-error root (list "push" "-u" "origin" branch))
          (list :pushed-p t :upstream (%git-upstream-branch root))))))

(defun %git-split-on-char (text delimiter)
  (let ((source (or text "")))
    (loop with start = 0
          with parts = '()
          for index = (position delimiter source :start start)
          do (if index
                 (progn
                   (push (subseq source start index) parts)
                   (setf start (1+ index)))
                 (progn
                   (push (subseq source start) parts)
                   (return (nreverse parts)))))))

(defun %git-parse-log-record (record)
  (let* ((first-separator (position #\u001F record))
         (second-separator (and first-separator
                                (position #\u001F record :start (1+ first-separator)))))
    (when (and first-separator second-separator)
      (let* ((sha (%git-trim-whitespace (subseq record 0 first-separator)))
             (subject (%git-trim-whitespace
                       (subseq record (1+ first-separator) second-separator)))
             (body (%git-trim-whitespace (subseq record (1+ second-separator)))))
        (when (plusp (length sha))
          (list :sha sha
                :subject subject
                :body body))))))

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

(defun %git-prefix-ci-p (prefix value)
  (let ((prefix-length (length prefix))
        (text (or value "")))
    (and (<= prefix-length (length text))
         (string-equal prefix text :end2 prefix-length))))

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

(defun %git-extract-pr-url (text)
  (cl-ppcre:register-groups-bind (url)
      ("(https?://[^\\s]+/pull/[0-9]+)" (or text ""))
    url))

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

(defun %git-diff-branch-data (&key project-root base-branch)
  (let* ((root (%git-ensure-work-tree (%git-project-root project-root)))
         (branch (%git-current-branch root))
         (resolved-base (%git-resolve-base-branch root base-branch))
         (merge-base (%git-merge-base root resolved-base))
         (range (format nil "~A..HEAD" merge-base))
         (diff-result (%git-run-command-or-error root
                                                 (list "diff" "--no-color" range "--")))
         (summary-result (%git-run-command-or-error root
                                                    (list "diff" "--shortstat" range "--")))
         (files-result (%git-run-command-or-error root
                                                  (list "diff" "--name-only" range "--")))
         (ahead-result (%git-run-command-or-error root
                                                  (list "rev-list" "--count" range)))
         (diff (or (getf diff-result :stdout) ""))
         (truncated-p (> (length diff) *git-max-branch-diff-chars*))
         (diff* (if truncated-p
                    (subseq diff 0 *git-max-branch-diff-chars*)
                    diff)))
    (list :project-root (%path-text root)
          :branch branch
          :base-branch resolved-base
          :merge-base merge-base
          :range range
          :ahead-commits (%git-safe-parse-int (%git-trim-whitespace
                                               (getf ahead-result :stdout)))
          :files-changed (%git-parse-lines-non-empty (getf files-result :stdout))
          :summary (%git-trim-whitespace (getf summary-result :stdout))
          :diff diff*
          :truncated-p truncated-p)))

(deftool git-status ()
  "Return repository status: branch, tracking, staged, and unstaged changes."
  (:permission :auto)
  (:dangerous nil)
  (:category :git)
  (:timeout 30)
  (%git-status-data))

(deftool git-diff-branch ((base-branch (or null string)
                             :description "Optional base branch override (defaults to main/master detection)."
                             :default nil))
  "Return current-branch diff vs base branch for PR context."
  (:permission :auto)
  (:dangerous nil)
  (:category :git)
  (:timeout 60)
  (%git-diff-branch-data :base-branch base-branch))

(deftool git-commit ((files (or null list string)
                    :description "Optional explicit files to stage before commit."
                    :default nil)
                     (co-author (or null string)
                      :description "Co-Authored-By identity to append."
                      :default nil)
                     (model (or null string)
                      :description "Optional model override for commit message generation."
                      :default nil)
                     (amend boolean
                      :description "Request amend mode (requires ALLOW-AMEND true)."
                      :default nil)
                     (allow-amend boolean
                      :description "Explicit acknowledgement to permit amend."
                      :default nil))
  "Stage explicit files, generate commit message from staged diff, and create commit."
  (:permission :full-auto)
  (:dangerous t)
  (:category :git)
  (:timeout 180)
  (when (and amend (not allow-amend))
    (error "Refusing --amend without explicit ALLOW-AMEND=true confirmation."))
  (let* ((root (%git-ensure-work-tree (%git-project-root)))
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
        (list :sha (%git-commit-sha root)
              :branch (%git-current-branch root)
              :files-changed staged-paths
              :message commit-message
              :message-summary (car (%git-lines commit-message))
              :message-source (getf message-data :source)
              :fallback-reason (getf message-data :fallback-reason))))))

(deftool create-pr ((base-branch (or null string)
                        :description "Optional base branch override (defaults to main/master detection)."
                        :default nil)
                    (model (or null string)
                      :description "Optional model override for PR title/body generation."
                      :default nil))
  "Generate pull request title/body from full branch history, push if needed, and create PR via gh."
  (:permission :full-auto)
  (:dangerous t)
  (:category :git)
  (:timeout 240)
  (let* ((root (%git-ensure-work-tree (%git-project-root)))
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
