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
      (error "Git project root does not exist: ~A." (coerce-path-string directory)))
    directory))

(defun %git-delegated-process-env ()
  (when (current-delegated-agent-id)
    (%coerce-process-env
     (shell-env-to-string-list
      (assemble-shell-env
       (merge-shell-environment
        (%default-shell-environment)
        :env-overrides (current-delegated-agent-secret-env-overrides)
        :inherit-env-p t
        :filter-sensitive-p t))))))

(defun %git-run-command (root args)
  (let ((process-env (%git-delegated-process-env)))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program (append (list "git") args)
                          :directory root
                          :ignore-error-status t
                          :output :string
                          :error-output :string
                          :env process-env)
      (list :stdout (or stdout "")
            :stderr (or stderr "")
            :exit-code (or exit-code 0)))))

(defun %git-run-bash-command (root command)
  (let ((process-env (%git-delegated-process-env)))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program (list "bash" "-lc" command)
                          :directory root
                          :ignore-error-status t
                          :output :string
                          :error-output :string
                          :env process-env)
      (list :stdout (or stdout "")
            :stderr (or stderr "")
            :exit-code (or exit-code 0)))))

(defun %git-publish-lifecycle-event (event-type payload &key (severity :info))
  (handler-case
      (let ((bus (current-event-bus)))
        (when (event-bus-p bus)
          (publish bus
                   event-type
                   :source :amoebum
                   :severity severity
                   :payload payload)))
    (error ()
      nil))
  payload)

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
    (error "Path is not a git work tree: ~A." (coerce-path-string root)))
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

(defun %git-current-branch (root)
  (%git-trim-whitespace
   (getf (%git-run-command-or-error root '("rev-parse" "--abbrev-ref" "HEAD")) :stdout)))

(defun %git-head-author (root)
  (%git-trim-whitespace
   (getf (%git-run-command-or-error root '("show" "-s" "--format=%an <%ae>" "HEAD"))
         :stdout)))

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

(defun %git-ensure-delegated-push-branch-allowed (branch)
  (let ((allowed-branch (current-delegated-agent-push-branch))
        (requested-branch (%git-trim-whitespace branch)))
    (when (and (plusp (length requested-branch))
               allowed-branch
               (not (string= requested-branch allowed-branch)))
      (let ((reason (format nil
                            "Delegated agent ~A may only push branch ~A; attempted ~A."
                            (or (current-delegated-agent-id) "<unknown>")
                            allowed-branch
                            requested-branch)))
        (error 'tool-permission-denied
               :tool-name :git-push
               :arguments (list requested-branch)
               :reason-code :worktree-branch-scope
               :reason reason
               :message reason))))
  branch)

(defun %git-push-set-upstream-if-needed! (root branch)
  (%git-ensure-delegated-push-branch-allowed branch)
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

(defun %git-prefix-ci-p (prefix value)
  (let ((prefix-length (length prefix))
        (text (or value "")))
    (and (<= prefix-length (length text))
         (string-equal prefix text :end2 prefix-length))))

(defun %git-heredoc-marker ()
  (format nil "AMOEBUM_HEREDOC_~D_~D"
          (get-universal-time)
          (random 999999)))

(defun %git-extract-pr-url (text)
  (cl-ppcre:register-groups-bind (url)
      ("(https?://[^\\s]+/pull/[0-9]+)" (or text ""))
    url))
