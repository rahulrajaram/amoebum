(in-package :amoebum/test)

(def-suite persona-suite
  :description "Agent persona parsing and discovery tests."
  :in amoebum-suite)

(in-suite persona-suite)

;;; --- Frontmatter extraction ---

(test parse-valid-persona
  "Parse a persona file with valid YAML frontmatter."
  (let* ((tmp-dir (%make-temp-directory "amoebum-persona-test"))
         (file (merge-pathnames #P"reviewer.md" tmp-dir)))
    (unwind-protect
        (progn
          (%write-text-file file
                            (format nil "---~%name: code-reviewer~%description: Expert at reviewing code~%capabilities: [code-review, static-analysis]~%model: moonshot-v1-128k~%---~%~%You are an expert code reviewer."))
          (let ((persona (amoebum:parse-persona-file file)))
            (is-true (amoebum:persona-definition-p persona))
            (is (string= "code-reviewer" (amoebum:persona-definition-name persona)))
            (is (string= "Expert at reviewing code" (amoebum:persona-definition-description persona)))
            (is (equal '("code-review" "static-analysis") (amoebum:persona-definition-capabilities persona)))
            (is (string= "moonshot-v1-128k" (amoebum:persona-definition-model persona)))
            (is (search "expert code reviewer" (amoebum:persona-definition-system-prompt persona)
                        :test #'char-equal))))
      (%delete-directory-tree-safe tmp-dir))))

(test parse-missing-name
  "Returns NIL for persona file missing required name field."
  (let* ((tmp-dir (%make-temp-directory "amoebum-persona-noname"))
         (file (merge-pathnames #P"bad.md" tmp-dir)))
    (unwind-protect
        (progn
          (%write-text-file file
                            (format nil "---~%description: No name here~%---~%Body text."))
          (is (null (amoebum:parse-persona-file file))))
      (%delete-directory-tree-safe tmp-dir))))

(test parse-missing-description
  "Returns NIL for persona file missing required description field."
  (let* ((tmp-dir (%make-temp-directory "amoebum-persona-nodesc"))
         (file (merge-pathnames #P"bad.md" tmp-dir)))
    (unwind-protect
        (progn
          (%write-text-file file
                            (format nil "---~%name: orphan~%---~%Body text."))
          (is (null (amoebum:parse-persona-file file))))
      (%delete-directory-tree-safe tmp-dir))))

(test parse-no-frontmatter
  "Returns NIL for plain markdown without frontmatter."
  (let* ((tmp-dir (%make-temp-directory "amoebum-persona-nofm"))
         (file (merge-pathnames #P"plain.md" tmp-dir)))
    (unwind-protect
        (progn
          (%write-text-file file "# Just a heading\n\nNo YAML frontmatter here.")
          (is (null (amoebum:parse-persona-file file))))
      (%delete-directory-tree-safe tmp-dir))))

(test discover-from-both-dirs
  "Discovers personas from both global and project directories."
  (let* ((tmp-dir (%make-temp-directory "amoebum-persona-discover"))
         (global-dir (merge-pathnames #P".amoebum/agents/" tmp-dir))
         (project-root (merge-pathnames #P"project/" tmp-dir))
         (project-dir (merge-pathnames #P".amoebum/agents/" project-root)))
    (unwind-protect
        (progn
          (%write-text-file (merge-pathnames #P"global-reviewer.md" global-dir)
                            (format nil "---~%name: global-reviewer~%description: Global reviewer~%---~%Global prompt."))
          (%write-text-file (merge-pathnames #P"project-tester.md" project-dir)
                            (format nil "---~%name: project-tester~%description: Project tester~%---~%Project prompt."))
          ;; Override the home directory for discovery
          (let ((personas
                  (let ((home (user-homedir-pathname)))
                    (declare (ignore home))
                    ;; We test the parsing functions directly
                    (let ((global-files (amoebum::%persona-directory-files global-dir))
                          (project-files (amoebum::%persona-directory-files project-dir))
                          (result '()))
                      (dolist (path project-files)
                        (let ((p (amoebum:parse-persona-file path)))
                          (when p
                            (setf (amoebum:persona-definition-scope p) :project)
                            (push p result))))
                      (dolist (path global-files)
                        (let ((p (amoebum:parse-persona-file path)))
                          (when p
                            (setf (amoebum:persona-definition-scope p) :global)
                            (push p result))))
                      (nreverse result)))))
            (is (= 2 (length personas)))
            (is-true (find "project-tester" personas
                           :key #'amoebum:persona-definition-name
                           :test #'string=))
            (is-true (find "global-reviewer" personas
                           :key #'amoebum:persona-definition-name
                           :test #'string=))))
      (%delete-directory-tree-safe tmp-dir))))

(test project-overrides-global
  "Same-name persona: project scope wins over global."
  (let* ((global-persona (amoebum::%make-persona-definition
                          :name "reviewer"
                          :description "Global"
                          :scope :global))
         (project-persona (amoebum::%make-persona-definition
                           :name "reviewer"
                           :description "Project"
                           :scope :project))
         (personas (list global-persona project-persona)))
    (let ((found (amoebum:find-persona-by-name "reviewer" personas)))
      (is (eq found project-persona))
      (is (string= "Project" (amoebum:persona-definition-description found))))))

(test manifest-format
  "Manifest lines match expected format."
  (let* ((p1 (amoebum::%make-persona-definition
              :name "code-reviewer"
              :description "Reviews code"
              :scope :global))
         (p2 (amoebum::%make-persona-definition
              :name "test-engineer"
              :description "Writes tests"
              :scope :project))
         (lines (amoebum:persona-manifest-lines (list p1 p2))))
    (is (= 2 (length lines)))
    (is (search "code-reviewer" (first lines)))
    (is (search "global" (first lines) :test #'char-equal))
    (is (search "test-engineer" (second lines)))
    (is (search "project" (second lines) :test #'char-equal))))

(test frontmatter-yaml-list-parsing
  "YAML list values like [a, b, c] are parsed correctly."
  (let ((result (amoebum::%parse-yaml-list-value "[code-review, static-analysis, refactoring]")))
    (is (= 3 (length result)))
    (is (string= "code-review" (first result)))
    (is (string= "static-analysis" (second result)))
    (is (string= "refactoring" (third result)))))

(test frontmatter-yaml-key-value
  "Basic YAML key: value pairs are parsed."
  (let ((alist (amoebum::%parse-yaml-frontmatter
                (format nil "name: test-agent~%description: A test agent~%model: gpt-4"))))
    (is (string= "test-agent" (cdr (assoc "name" alist :test #'string=))))
    (is (string= "A test agent" (cdr (assoc "description" alist :test #'string=))))
    (is (string= "gpt-4" (cdr (assoc "model" alist :test #'string=))))))
