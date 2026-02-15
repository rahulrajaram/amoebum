(in-package :amoebum)

(defparameter +system-prompt-base-layer+
  "You are Amoebum, a programmable AI engineering assistant.

Core behavior rules:
- Be accurate, explicit about assumptions, and prefer verified facts.
- Respect user intent and repository instructions.
- Prefer minimal, safe edits with clear verification evidence.
- Never use destructive git operations without explicit user request.
- Use available tools to inspect, edit, and verify changes before claiming completion.")

(defparameter *system-prompt-git-command-runner* nil)

(defun %system-prompt-trim (value)
  (string-trim '(#\Space #\Tab #\Newline #\Return) (or value "")))

(defun %system-prompt-empty-p (value)
  (zerop (length (%system-prompt-trim value))))

(defun %system-prompt-lines (text)
  (with-input-from-string (stream (or text ""))
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun %system-prompt-split-whitespace (line)
  (remove-if (lambda (part) (zerop (length part)))
             (cl-ppcre:split "\\s+" (or line ""))))

(defun %system-prompt-first-existing-path (paths)
  (loop for candidate in paths
        for found = (and candidate (ignore-errors (probe-file candidate)))
        when found
          do (return found)))

(defun %system-prompt-existing-paths (paths)
  (let ((result '()))
    (dolist (candidate paths)
      (let ((found (and candidate (ignore-errors (probe-file candidate)))))
        (when found
          (push found result))))
    (nreverse result)))

(defun %system-prompt-read-file (path)
  (when path
    (let* ((text (ignore-errors (uiop:read-file-string path :external-format :utf-8)))
           (trimmed (%system-prompt-trim text)))
      (unless (zerop (length trimmed))
        trimmed))))

(defun %system-prompt-path-text (path)
  (when path
    (ignore-errors (namestring path))))

(defun %system-prompt-resolve-project-root (&optional project-root)
  (let ((base (or project-root
                  (ignore-errors (config-project-root (current-config)))
                  *default-pathname-defaults*)))
    (uiop:ensure-directory-pathname
     (or (ignore-errors (truename base))
         (pathname base)))))

(defun %system-prompt-resolve-cwd (&optional cwd)
  (let ((base (or cwd
                  (ignore-errors (uiop:getcwd))
                  *default-pathname-defaults*)))
    (uiop:ensure-directory-pathname
     (or (ignore-errors (truename base))
         (pathname base)))))

(defun %system-prompt-path-under-root-p (path root)
  (let* ((path-text (%system-prompt-path-text (uiop:ensure-directory-pathname path)))
         (root-text (%system-prompt-path-text (uiop:ensure-directory-pathname root))))
    (and (stringp path-text)
         (stringp root-text)
         (uiop:string-prefix-p root-text path-text))))

(defun %system-prompt-relative-components (root cwd)
  (unless (%system-prompt-path-under-root-p cwd root)
    (return-from %system-prompt-relative-components '()))
  (let* ((root-text (%system-prompt-path-text (uiop:ensure-directory-pathname root)))
         (cwd-text (%system-prompt-path-text (uiop:ensure-directory-pathname cwd)))
         (relative (if (and root-text cwd-text
                            (<= (length root-text) (length cwd-text)))
                       (subseq cwd-text (length root-text))
                       "")))
    (remove-if (lambda (part) (zerop (length part)))
               (uiop:split-string relative :separator "/"))))

(defun %system-prompt-relative-prefixes (components)
  (loop for index from 1 to (length components)
        collect (subseq components 0 index)))

(defun %system-prompt-join-path-components (components)
  (format nil "~{~A~^/~}" components))

(defun %system-prompt-directory-layer-candidates (project-root cwd)
  (let* ((root (%system-prompt-resolve-project-root project-root))
         (working-dir (%system-prompt-resolve-cwd cwd))
         (components (%system-prompt-relative-components root working-dir))
         (candidates '()))
    (dolist (prefix (%system-prompt-relative-prefixes components))
      (let* ((relative (%system-prompt-join-path-components prefix))
             (directory (merge-pathnames (format nil "~A/" relative) root))
             (mapped (merge-pathnames (format nil ".amoebum/prompts/~A.md" relative) root)))
        (push mapped candidates)
        (push (merge-pathnames #P".amoebum/system-prompt.md" directory) candidates)
        (push (merge-pathnames #P".amoebum/SYSTEM_PROMPT.md" directory) candidates)))
    (nreverse candidates)))

(defun %system-prompt-global-layer-candidates ()
  (let ((home (user-homedir-pathname)))
    (list (merge-pathnames #P".amoebum/SYSTEM_PROMPT.md" home)
          (merge-pathnames #P".amoebum/system-prompt.md" home))))

(defun %system-prompt-project-layer-candidates (project-root)
  (let ((root (%system-prompt-resolve-project-root project-root)))
    (list (merge-pathnames #P".amoebum/SYSTEM_PROMPT.md" root)
          (merge-pathnames #P".amoebum/system-prompt.md" root))))

(defun default-system-prompt-git-command-runner (directory args)
  (handler-case
      (multiple-value-bind (stdout stderr exit-code)
          (uiop:run-program (append (list "git") args)
                            :directory directory
                            :ignore-error-status t
                            :output :string
                            :error-output :string)
        (list :stdout (or stdout "")
              :stderr (or stderr "")
              :exit-code (or exit-code 0)))
    (error (condition)
      (list :stdout ""
            :stderr (princ-to-string condition)
            :exit-code 127))))

(defun %system-prompt-run-git (directory args)
  (let ((runner (or *system-prompt-git-command-runner*
                    #'default-system-prompt-git-command-runner)))
    (funcall runner directory args)))

(defun %system-prompt-parse-git-status (text)
  (let ((branch "HEAD")
        (upstream nil)
        (ahead 0)
        (behind 0)
        (staged 0)
        (unstaged 0)
        (untracked 0))
    (dolist (line (%system-prompt-lines text))
      (cond
        ((uiop:string-prefix-p "# branch.head " line)
         (setf branch (%system-prompt-trim (subseq line (length "# branch.head ")))))
        ((uiop:string-prefix-p "# branch.upstream " line)
         (setf upstream (%system-prompt-trim (subseq line (length "# branch.upstream ")))))
        ((uiop:string-prefix-p "# branch.ab " line)
         (let* ((parts (%system-prompt-split-whitespace line))
                (ahead-token (or (third parts) "+0"))
                (behind-token (or (fourth parts) "-0")))
           (setf ahead (ignore-errors (parse-integer (subseq ahead-token 1))))
           (setf behind (ignore-errors (parse-integer (subseq behind-token 1))))))
        ((uiop:string-prefix-p "? " line)
         (incf untracked))
        ((or (uiop:string-prefix-p "1 " line)
             (uiop:string-prefix-p "2 " line)
             (uiop:string-prefix-p "u " line))
         (let* ((parts (%system-prompt-split-whitespace line))
                (xy (or (second parts) ".."))
                (x (if (> (length xy) 0) (char xy 0) #\.))
                (y (if (> (length xy) 1) (char xy 1) #\.)))
           (unless (char= x #\.)
             (incf staged))
           (unless (char= y #\.)
             (incf unstaged))))))
    (list :branch branch
          :upstream upstream
          :ahead (or ahead 0)
          :behind (or behind 0)
          :staged staged
          :unstaged unstaged
          :untracked untracked)))

(defun %system-prompt-git-context (&key project-root)
  (let* ((root (%system-prompt-resolve-project-root project-root))
         (inside (%system-prompt-run-git root '("rev-parse" "--is-inside-work-tree"))))
    (if (or (not (zerop (getf inside :exit-code 1)))
            (not (string= (%system-prompt-trim (getf inside :stdout "")) "true")))
        (list :repository-p nil)
        (let* ((status-result
                 (%system-prompt-run-git root '("status" "--porcelain=2" "--branch")))
               (status-data
                 (if (zerop (getf status-result :exit-code 1))
                     (%system-prompt-parse-git-status (getf status-result :stdout ""))
                     (list :branch "HEAD"
                           :upstream nil
                           :ahead 0
                           :behind 0
                           :staged 0
                           :unstaged 0
                           :untracked 0)))
               (log-result
                 (%system-prompt-run-git root
                                         '("log" "--oneline" "--decorate=no" "-n" "5")))
               (recent-commits
                 (if (zerop (getf log-result :exit-code 1))
                     (remove-if (lambda (line)
                                  (%system-prompt-empty-p line))
                                (%system-prompt-lines (getf log-result :stdout "")))
                     '())))
          (append (list :repository-p t
                        :root root)
                  status-data
                  (list :recent-commits recent-commits))))))

(defun %system-prompt-tool-definitions (toolset)
  (cond
    ((and toolset (pseudopod:toolset-p toolset))
     (pseudopod:toolset-tools toolset))
    ((and (listp toolset)
          (every #'pseudopod:tool-definition-p toolset))
     toolset)
    ((and (boundp '*toolset*)
          (pseudopod:toolset-p *toolset*))
     (pseudopod:toolset-tools *toolset*))
    (t
     '())))

(defun %system-prompt-tool-lines (&key toolset)
  (let* ((definitions (%system-prompt-tool-definitions toolset))
         (sorted
           (sort (copy-list definitions)
                 #'string<
                 :key #'pseudopod:tool-definition-name)))
    (mapcar (lambda (tool)
              (let* ((name (pseudopod:tool-definition-name tool))
                     (description (%system-prompt-trim
                                   (pseudopod:tool-definition-description tool)))
                     (metadata (and (hash-table-p *tool-metadata*)
                                    (gethash name *tool-metadata*)))
                     (permission (and (tool-metadata-p metadata)
                                      (tool-metadata-permission metadata)))
                     (category (and (tool-metadata-p metadata)
                                    (tool-metadata-category metadata)))
                     (summary (if (%system-prompt-empty-p description)
                                  "no description"
                                  description)))
                (format nil "- ~A~@[ (permission: ~A)~]~@[ (category: ~A)~]: ~A"
                        name
                        permission
                        category
                        summary)))
            sorted)))

(defun resolve-system-prompt-layers (&key project-root
                                          cwd
                                          global-layer-path
                                          project-layer-path
                                          directory-layer-paths)
  (let* ((root (%system-prompt-resolve-project-root project-root))
         (working-dir (%system-prompt-resolve-cwd cwd))
         (global-path
           (%system-prompt-first-existing-path
            (or (and global-layer-path
                     (if (listp global-layer-path)
                         global-layer-path
                         (list global-layer-path)))
                (%system-prompt-global-layer-candidates))))
         (project-path
           (%system-prompt-first-existing-path
            (or (and project-layer-path
                     (if (listp project-layer-path)
                         project-layer-path
                         (list project-layer-path)))
                (%system-prompt-project-layer-candidates root))))
         (directory-candidates
           (or (and directory-layer-paths
                    (if (listp directory-layer-paths)
                        directory-layer-paths
                        (list directory-layer-paths)))
               (%system-prompt-directory-layer-candidates root working-dir)))
         (directory-paths (%system-prompt-existing-paths directory-candidates))
         (directory-content
           (with-output-to-string (stream)
             (loop for path in directory-paths
                   for content = (%system-prompt-read-file path)
                   when content do
                     (format stream "From ~A~%~A~2%"
                             (%system-prompt-path-text path)
                             content))))
         (directory-content
           (unless (%system-prompt-empty-p directory-content)
             (%system-prompt-trim directory-content))))
    (list
     (list :index 1
           :layer :base
           :label "Amoebum Base"
           :source :built-in
           :content +system-prompt-base-layer+)
     (list :index 2
           :layer :global
           :label "Global User"
           :source global-path
           :content (%system-prompt-read-file global-path))
     (list :index 3
           :layer :project
           :label "Project"
           :source project-path
           :content (%system-prompt-read-file project-path))
     (list :index 4
           :layer :directory
           :label "Directory"
           :source directory-paths
           :content directory-content))))

(defun system-prompt-dynamic-context (&key project-root cwd toolset)
  (let* ((root (%system-prompt-resolve-project-root project-root))
         (working-dir (%system-prompt-resolve-cwd cwd))
         (git (%system-prompt-git-context :project-root root))
         (tool-lines (%system-prompt-tool-lines :toolset toolset))
         (shell (or (uiop:getenv "SHELL") "unknown"))
         (platform (format nil "~A ~A"
                           (software-type)
                           (or (machine-type) "unknown"))))
    (with-output-to-string (stream)
      (format stream "Git Context~%")
      (if (getf git :repository-p)
          (progn
            (format stream "- Branch: ~A~%" (getf git :branch "HEAD"))
            (format stream "- Tracking: ~A (ahead ~D / behind ~D)~%"
                    (or (getf git :upstream) "none")
                    (or (getf git :ahead) 0)
                    (or (getf git :behind) 0))
            (format stream "- Changes: staged ~D, unstaged ~D, untracked ~D~%"
                    (or (getf git :staged) 0)
                    (or (getf git :unstaged) 0)
                    (or (getf git :untracked) 0))
            (format stream "- Recent commits:~%")
            (let ((commits (getf git :recent-commits)))
              (if commits
                  (dolist (line commits)
                    (format stream "  * ~A~%" line))
                  (format stream "  * none~%"))))
          (format stream "- Repository: not detected at ~A~%"
                  (%system-prompt-path-text root)))
      (format stream "~%Environment Context~%")
      (format stream "- OS/Platform: ~A~%" platform)
      (format stream "- Shell: ~A~%" shell)
      (format stream "- Working directory: ~A~%"
              (%system-prompt-path-text working-dir))
      (format stream "- Project root: ~A~%"
              (%system-prompt-path-text root))
      (format stream "~%Available Tools~%")
      (if tool-lines
          (dolist (line tool-lines)
            (format stream "~A~%" line))
          (format stream "- none~%")))))

(defun assemble-system-prompt (&key project-root
                                    cwd
                                    toolset
                                    global-layer-path
                                    project-layer-path
                                    directory-layer-paths)
  (let* ((layers (resolve-system-prompt-layers
                  :project-root project-root
                  :cwd cwd
                  :global-layer-path global-layer-path
                  :project-layer-path project-layer-path
                  :directory-layer-paths directory-layer-paths))
         (dynamic-context (system-prompt-dynamic-context
                           :project-root project-root
                           :cwd cwd
                           :toolset toolset)))
    (with-output-to-string (stream)
      (format stream "Amoebum system prompt hierarchy.~%")
      (format stream "Precedence: layer 4 overrides layer 3 overrides layer 2 overrides layer 1.~2%")
      (dolist (layer layers)
        (let ((index (getf layer :index))
              (label (getf layer :label))
              (source (getf layer :source))
              (content (getf layer :content)))
          (format stream "Layer ~D: ~A~%" index label)
          (cond
            ((and (listp source) source)
             (dolist (path source)
               (format stream "Source: ~A~%" (%system-prompt-path-text path))))
            ((pathnamep source)
             (format stream "Source: ~A~%" (%system-prompt-path-text source)))
            ((eq source :built-in)
             (format stream "Source: built-in~%"))
            (t
             (format stream "Source: none~%")))
          (if (%system-prompt-empty-p content)
              (format stream "(no content)~2%")
              (format stream "~A~2%" content))))
      (format stream "Dynamic Runtime Context~%~A"
              dynamic-context))))
