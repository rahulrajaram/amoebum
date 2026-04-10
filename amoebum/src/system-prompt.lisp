(in-package :amoebum)

(defparameter +system-prompt-base-layer+
  "CRITICAL — FIRST ACTION: When exploring a codebase, your first tool call MUST be: glob-files with pattern \"*\" (omit root). Never start with bash-exec. Never start with ls, pwd, or find. Always start with glob-files.

<identity>
You are Amoebum, a programmable AI engineering assistant. You inspect, edit, and verify code across languages and frameworks using the tools available to you. You operate inside a terminal-based conversational interface.
</identity>

<tool_selection>
Tool selection — mandatory rules:

1. ORIENT FIRST: Always call glob-files (pattern \"*\", omit root) as your first action when exploring a project. This returns file names and modification times. Do not use bash-exec with ls, pwd, or find for orientation.

2. NO SHOTGUNNING: Never search for multiple ecosystems at once. Do not look for Cargo.toml, package.json, *.py, *.go, *.ts, *.yaml in the same call or in parallel. First see what exists, then investigate only the files and extensions you actually found.

3. SEQUENTIAL NARROWING: After the initial glob-files, form a hypothesis about the project type, then make targeted follow-up calls. For example: glob-files \"*\" shows *.asd and *.lisp files → this is a Common Lisp project → read-file on the .asd file.

4. DEDICATED TOOLS FIRST:
  glob-files replaces find and ls — never use bash-exec for file listing
  read-file replaces cat/head/tail — never use bash-exec to read files
  grep-content replaces grep/rg — never use bash-exec to search file contents
  edit-file replaces sed/awk — never use bash-exec to edit files
  bash-exec is ONLY for commands with no dedicated equivalent (make, git, npm, cargo, etc.)

5. NO PATH GUESSING: Only read-file on paths you have confirmed exist via glob-files or another tool result. Never fabricate or guess file paths. Omit the root parameter for glob-files and grep-content to use the project root automatically.

6. RESPOND AFTER GATHERING CONTEXT: After making tool calls to gather enough information, you MUST produce a text response to the user. Do not make more than 5 tool calls for a simple question. If a tool call fails, note the error and adjust your approach or respond with what you have — do not retry the same failing call.
</tool_selection>

<engineering_behavior>
Read before editing: understand existing code before suggesting modifications. Open the file first.
Edit before creating: prefer modifying existing files over creating new ones.
Verify before claiming: use tools to confirm changes work before reporting completion.
Minimal safe edits: make the smallest change that achieves the goal. Do not refactor surrounding code, add comments to unchanged code, or improve things that were not asked about.
Never use destructive git operations (force push, reset --hard, checkout .) without explicit user request.
Be explicit about assumptions. When uncertain, state what you are assuming and why.
</engineering_behavior>

<tone_and_formatting>
Respond in prose and paragraphs, not bullet-point lists, unless the user explicitly asks for lists or the response is genuinely multifaceted.
Use the minimum formatting needed for clarity. Avoid over-using bold, headers, and bullets.
Keep responses concise. Lead with the answer or action, not reasoning.
No emojis unless the user uses them first.
No filler words, preamble, or unnecessary transitions. Do not restate what the user said.
One question per response maximum. Address the query before asking for clarification.
</tone_and_formatting>

<responding_to_mistakes>
Own mistakes honestly and fix them. Do not collapse into excessive apology or self-abasement.
Stay focused on solving the problem. Acknowledge what went wrong, then move forward.
If blocked, try alternative approaches rather than repeating the same failing action.
</responding_to_mistakes>

<knowledge_boundaries>
Be honest about what you do not know. If you are uncertain about a fact, say so.
When information might be outdated, note the limitation and suggest the user verify.
Do not guess at URLs, API endpoints, or version numbers you are not confident about.
</knowledge_boundaries>")

(defparameter +system-prompt-plan-mode-exploration-guidance+
  "Plan mode authoring workflow:
- This turn is planning-only; do not make code edits while plan mode is enabled.
- Explore the codebase first using read/search/glob/grep/index-style tools before drafting a plan.
- Build understanding from concrete files and summarize the current architecture/behavior with file references.
- If critical context is missing after exploration, ask a focused follow-up question before proposing execution steps.
- Output the plan as numbered steps.
- For every step, include a concise step description and explicit file paths to inspect or change.
- Keep each step action-oriented and implementation-ready.")

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
        (push (merge-pathnames #P"AGENTS.md" directory) candidates)
        (push (merge-pathnames #P"agents.md" directory) candidates)
        (push mapped candidates)
        (push (merge-pathnames #P".amoebum/system-prompt.md" directory) candidates)
        (push (merge-pathnames #P".amoebum/SYSTEM_PROMPT.md" directory) candidates)))
    (nreverse candidates)))

(defun %system-prompt-global-layer-candidates ()
  (let ((home (user-homedir-pathname)))
    (list (merge-pathnames #P".amoebum/AGENTS.md" home)
          (merge-pathnames #P".amoebum/agents.md" home)
          (merge-pathnames #P".amoebum/SYSTEM_PROMPT.md" home)
          (merge-pathnames #P".amoebum/system-prompt.md" home)
          (merge-pathnames #P".config/amoebum/AGENTS.md" home)
          (merge-pathnames #P".config/amoebum/SYSTEM_PROMPT.md" home)
          ;; Codex-style user defaults are treated as a fallback global layer
          ;; when amoebum-specific user defaults are not present.
          (merge-pathnames #P".codex/AGENTS.md" home)
          (merge-pathnames #P".codex/agents.md" home)
          (merge-pathnames #P".config/codex/AGENTS.md" home)
          (merge-pathnames #P".config/codex/agents.md" home))))

(defun %system-prompt-project-layer-candidates (project-root)
  (let ((root (%system-prompt-resolve-project-root project-root)))
    (list (merge-pathnames #P"AGENTS.md" root)
          (merge-pathnames #P"agents.md" root)
          (merge-pathnames #P".amoebum/SYSTEM_PROMPT.md" root)
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
                     (metadata (find-tool-metadata name))
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

(defun %system-prompt-tool-name-table (definitions)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (tool definitions table)
      (setf (gethash (pseudopod:tool-definition-name tool) table) t))))

(defun %system-prompt-tool-present-p (tool-name-table tool-name)
  (and (hash-table-p tool-name-table)
       (gethash tool-name tool-name-table)))

(defun %system-prompt-cultivar-guidance-lines (definitions)
  (let ((tool-name-table (%system-prompt-tool-name-table definitions)))
    (when (and (%system-prompt-tool-present-p tool-name-table "cultivar-location-slice")
               (%system-prompt-tool-present-p tool-name-table "cultivar-symbol-slice"))
      (remove nil
              (list "Cultivar Retrieval Strategy"
                    "- For Common Lisp code intelligence, prefer `cultivar-location-slice` for file/line/column lookups and `cultivar-symbol-slice` for follow-up symbol-id retrieval."
                    (when (%system-prompt-tool-present-p tool-name-table "cultivar-symbol-resolve")
                      "- Use `cultivar-symbol-resolve` only when you specifically need the symbol identifier without the full canonical slice payload.")
                    (when (%system-prompt-tool-present-p tool-name-table "cultivar-symbol-references")
                      "- Use `cultivar-symbol-references` only when you explicitly need the flattened reference list; the canonical slice already carries richer definition, caller, and callee context.")
                    (when (%system-prompt-tool-present-p tool-name-table "cultivar-span-preview")
                      "- Treat `cultivar-span-preview` as the human-readable fallback, not the default machine reasoning path.")
                    "- Preserve `results-digest`, `served-from-materialization`, `materialization-kind`, `quality`, `truncation`, and `notes` when summarizing or chaining Cultivar results.")))))

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
         (definitions (%system-prompt-tool-definitions toolset))
         (tool-lines (%system-prompt-tool-lines :toolset definitions))
         (cultivar-guidance (%system-prompt-cultivar-guidance-lines definitions))
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
          (format stream "- none~%"))
      (when cultivar-guidance
        (format stream "~%")
        (dolist (line cultivar-guidance)
          (format stream "~A~%" line)))
      ;; Persona manifest
      (let ((personas (ignore-errors
                        (discover-persona-files :project-root root))))
        (when personas
          (format stream "~%Available Agent Personas~%")
          (dolist (line (persona-manifest-lines personas))
            (format stream "~A~%" line))
          (format stream "~%Use spawn-agent-worker with :persona to leverage these.~%"))))))

(defun %system-prompt-plan-mode-enabled-p ()
  (not (null (cfg :plan-mode))))

(defun %system-prompt-plan-mode-exploration-checkpoint ()
  (let* ((snapshot (ignore-errors (plan-mode-exploration-snapshot)))
         (call-count (or (and (listp snapshot)
                              (getf snapshot :call-count))
                         0))
         (tool-names (or (and (listp snapshot)
                              (getf snapshot :tool-names))
                         '())))
    (if (plusp call-count)
        (format nil
                "Exploration checkpoint: ~D exploration tool call~:P recorded using ~{`~A`~^, ~}. Continue building understanding from concrete file evidence before finalizing steps."
                call-count
                tool-names)
        "Exploration checkpoint: no codebase exploration tool calls recorded in this plan session yet. Call read/search/glob/grep tools first, then summarize concrete findings with file references before drafting steps.")))

(defun system-prompt-plan-mode-guidance ()
  (if (%system-prompt-plan-mode-enabled-p)
      (format nil "~A~2%~A"
              +system-prompt-plan-mode-exploration-guidance+
              (%system-prompt-plan-mode-exploration-checkpoint))
      "Plan mode guidance inactive."))

(defun assemble-system-prompt (&key project-root
                                    cwd
                                    toolset
                                    global-layer-path
                                    project-layer-path
                                    directory-layer-paths
                                    base-layer-override)
  (let* ((layers (resolve-system-prompt-layers
                  :project-root project-root
                  :cwd cwd
                  :global-layer-path global-layer-path
                  :project-layer-path project-layer-path
                  :directory-layer-paths directory-layer-paths))
         (dynamic-context (system-prompt-dynamic-context
                           :project-root project-root
                           :cwd cwd
                           :toolset toolset))
         (plan-guidance (system-prompt-plan-mode-guidance)))
    ;; Apply base-layer-override if provided
    (when (and base-layer-override
               (stringp base-layer-override)
               (plusp (length base-layer-override)))
      (let ((base-layer (first layers)))
        (when base-layer
          (setf (getf base-layer :content) base-layer-override
                (getf base-layer :source) :persona-override))))
    (with-output-to-string (stream)
      (format stream "Amoebum system prompt hierarchy.~%")
      (format stream "Precedence: layer 4 overrides layer 3 overrides layer 2 overrides layer 1.~2%")
      (format stream "Instruction ingestion order: global user defaults -> project root instructions -> nested directory instructions (root to cwd).~%")
      (format stream "Global defaults lookup order: ~~/.amoebum/*, ~~/.config/amoebum/*, then ~~/.codex/* and ~~/.config/codex/* fallback files.~2%")
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
      (format stream "Plan Mode Guidance~%~A~2%"
              plan-guidance)
      (format stream "Dynamic Runtime Context~%~A"
              dynamic-context))))
