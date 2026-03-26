(in-package :amoebum/test)

(def-suite memory-command-suite
  :in amoebum-suite
  :description "Memory source visibility and @import regressions.")

(in-suite memory-command-suite)

(defun %write-memory-lines (path lines)
  (%write-text-file path
                    (with-output-to-string (out)
                      (dolist (line lines)
                        (write-line line out)))))

(defun %make-memory-import-fixture ()
  (let* ((tmp-root (%make-temp-directory "memory-import"))
         (project-root (uiop:ensure-directory-pathname
                        (merge-pathnames #P"project/" tmp-root)))
         (global-memory (merge-pathnames #P"home/.amoebum/memory/MEMORY.md" tmp-root))
         (project-memory (merge-pathnames #P".amoebum/MEMORY.md" project-root))
         (topic-index (merge-pathnames #P".amoebum/memory/MEMORY.md" project-root))
         (topic-tools (merge-pathnames #P".amoebum/memory/tools.md" project-root))
         (backend (amoebum:make-file-memory-backend
                   :project-root project-root
                   :global-path global-memory
                   :project-path project-memory)))
    (%write-memory-lines global-memory
                         '("# Amoebum Memory"
                           ""
                           "- [package-manager] Use npm everywhere"))
    (%write-memory-lines topic-index
                         '("# Project topics"
                           ""
                           "@tools.md"
                           "- [approval-style] Prompt before writes"))
    (%write-memory-lines topic-tools
                         '("# Tool memory"
                           ""
                           "- [package-manager] Use pnpm in scripts"
                           "- [tool-timeout] Allow 120 second shell tool budgets"))
    (%write-memory-lines project-memory
                         '("# Project memory"
                           ""
                           "- [package-manager] Use bun for this repo"))
    (list :tmp-root tmp-root
          :project-root project-root
          :global-memory global-memory
          :project-memory project-memory
          :topic-index topic-index
          :topic-tools topic-tools
          :backend backend)))

(test effective-memory-honors-imported-topic-precedence
  (let* ((fixture (%make-memory-import-fixture))
         (tmp-root (getf fixture :tmp-root))
         (backend (getf fixture :backend))
         (topic-index (getf fixture :topic-index))
         (topic-tools (getf fixture :topic-tools)))
    (unwind-protect
         (let* ((effective (amoebum:memory-list backend :scope :effective))
                (topics (amoebum:memory-list backend :scope :topics))
                (package-manager (find "package-manager"
                                       effective
                                       :key #'amoebum:memory-entry-key
                                       :test #'string=))
                (tool-timeout (find "tool-timeout"
                                    effective
                                    :key #'amoebum:memory-entry-key
                                    :test #'string=))
                (approval-style (find "approval-style"
                                      topics
                                      :key #'amoebum:memory-entry-key
                                      :test #'string=)))
           (is-true package-manager)
           (is-true tool-timeout)
           (is-true approval-style)
           (is (string= "Use bun for this repo"
                        (amoebum:memory-entry-value package-manager)))
           (is (string= "Allow 120 second shell tool budgets"
                        (amoebum:memory-entry-value tool-timeout)))
           (is (string= (namestring topic-tools)
                        (amoebum:memory-entry-source tool-timeout)))
           (is (string= (namestring topic-index)
                        (amoebum:memory-entry-source approval-style))))
      (%delete-directory-tree-safe tmp-root))))

(test memory-command-show-lists-loaded-source-files
  (let* ((fixture (%make-memory-import-fixture))
         (tmp-root (getf fixture :tmp-root))
         (backend (getf fixture :backend))
         (global-memory (getf fixture :global-memory))
         (project-memory (getf fixture :project-memory))
         (topic-index (getf fixture :topic-index))
         (topic-tools (getf fixture :topic-tools)))
    (unwind-protect
         (let ((output (amoebum:memory-command-show :backend backend)))
           (is (search "Loaded sources:" output :test #'char-equal))
           (is (search (namestring global-memory) output :test #'char-equal))
           (is (search (namestring topic-index) output :test #'char-equal))
           (is (search (namestring topic-tools) output :test #'char-equal))
           (is (search (namestring project-memory) output :test #'char-equal))
           (is (search "[source: " output :test #'char-equal)))
      (%delete-directory-tree-safe tmp-root))))
