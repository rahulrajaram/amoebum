(let* ((smoke-file (or *load-truename* *compile-file-truename*))
       (amoebum-dir (and smoke-file (make-pathname :name nil :type nil :defaults smoke-file)))
       (repo-root (and amoebum-dir (truename (merge-pathnames #P"../" amoebum-dir)))))
  (unless repo-root
    (error "Unable to resolve repository root from ~S" smoke-file))

  (load (merge-pathnames #P"ptui/.tools/quicklisp/setup.lisp" repo-root))
  (require :asdf)

  (let* ((asdf-pkg (or (find-package "ASDF")
                       (error "Missing package ASDF")))
         (load-asd-sym (or (find-symbol "LOAD-ASD" asdf-pkg)
                           (error "Missing symbol LOAD-ASD in ASDF package")))
         (load-system-sym (or (find-symbol "LOAD-SYSTEM" asdf-pkg)
                              (error "Missing symbol LOAD-SYSTEM in ASDF package")))
         (load-asd-fn (symbol-function load-asd-sym))
         (load-system-fn (symbol-function load-system-sym)))
    (funcall load-asd-fn (merge-pathnames #P"pseudopod/pseudopod.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"ptui/ptui.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"amoebum/amoebum.asd" repo-root))
    (funcall load-system-fn "amoebum"))

  (let* ((amoebum-pkg (or (find-package "AMOEBUM")
                          (error "Missing package AMOEBUM after load.")))
         (elements-pkg (or (find-package "PTUI.UI.ELEMENTS")
                           (error "Missing package PTUI.UI.ELEMENTS after load.")))
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn-in
           (lambda (name package)
             (symbol-function (funcall symbol-in name package))))
         (make-tree-node-fn (funcall fn-in "MAKE-TREE-NODE" amoebum-pkg))
         (make-tree-browser-state-fn (funcall fn-in "MAKE-TREE-BROWSER-STATE" amoebum-pkg))
         (tree-browser-visible-entries-fn (funcall fn-in "TREE-BROWSER-VISIBLE-ENTRIES" amoebum-pkg))
         (tree-browser-handle-key-fn (funcall fn-in "TREE-BROWSER-HANDLE-KEY!" amoebum-pkg))
         (tree-browser-selected-index-fn (funcall fn-in "TREE-BROWSER-STATE-SELECTED-INDEX" amoebum-pkg))
         (tree-browser-selected-node-fn (funcall fn-in "TREE-BROWSER-SELECTED-NODE" amoebum-pkg))
         (tree-node-label-fn (funcall fn-in "TREE-NODE-LABEL" amoebum-pkg))
         (make-tree-browser-widget-fn (funcall fn-in "MAKE-TREE-BROWSER-WIDGET" amoebum-pkg))
         (make-git-file-tree-browser-state-fn (funcall fn-in "MAKE-GIT-FILE-TREE-BROWSER-STATE" amoebum-pkg))
         (chat-make-state-fn (funcall fn-in "MAKE-CHAT-UI-STATE" amoebum-pkg))
         (chat-handle-event-fn (funcall fn-in "HANDLE-CHAT-UI-EVENT" amoebum-pkg))
         (chat-build-tree-fn (funcall fn-in "CHAT-UI-BUILD-TREE" amoebum-pkg))
         (events-pkg (or (find-package "PTUI.CORE.EVENTS")
                         (error "Missing package PTUI.CORE.EVENTS after load.")))
         (make-key-event-fn (funcall fn-in "MAKE-KEY-EVENT" events-pkg))
         (ui-element-type-fn (funcall fn-in "UI-ELEMENT-TYPE" elements-pkg))
         (ui-element-id-fn (funcall fn-in "UI-ELEMENT-ID" elements-pkg))
         (ui-element-props-fn (funcall fn-in "UI-ELEMENT-PROPS" elements-pkg))
         (ui-element-children-fn (funcall fn-in "UI-ELEMENT-CHILDREN" elements-pkg))
         (uiop-pkg (or (find-package "UIOP")
                       (error "Missing package UIOP after load.")))
         (run-program-fn (symbol-function (or (find-symbol "RUN-PROGRAM" uiop-pkg)
                                              (error "Missing UIOP:RUN-PROGRAM.")))))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (make-temp-root (prefix)
               (make-pathname
                :directory (list :absolute
                                 "tmp"
                                 (format nil "~A-~D" prefix (get-universal-time)))
                :name nil
                :type nil))
             (write-file (path contents)
               (ensure-directories-exist path)
               (with-open-file (stream path
                                       :direction :output
                                       :if-does-not-exist :create
                                       :if-exists :supersede
                                       :external-format :utf-8)
                 (write-string contents stream)))
             (run-command (directory args)
               (multiple-value-bind (stdout stderr exit-code)
                   (funcall run-program-fn
                            args
                            :directory directory
                            :ignore-error-status t
                            :output :string
                            :error-output :string)
                 (unless (zerop (or exit-code 1))
                   (error "Command failed (~{~A~^ ~}) in ~A: ~A~%~A"
                          args
                          directory
                          (or stderr "")
                          (or stdout "")))
                 (or stdout "")))
             (collect-tree-text-lines (element)
               (let ((lines '()))
                 (labels ((walk (node)
                            (when (eq (funcall ui-element-type-fn node) :text)
                              (let* ((props (funcall ui-element-props-fn node))
                                     (text (and (listp props) (getf props :text))))
                                (when (and (stringp text) (plusp (length text)))
                                  (push text lines))))
                            (dolist (child (funcall ui-element-children-fn node))
                              (walk child))))
                   (walk element))
                 (nreverse lines)))
             (tree-has-id-p (element target-id)
               (or (equal (funcall ui-element-id-fn element) target-id)
                   (some (lambda (child)
                           (tree-has-id-p child target-id))
                         (funcall ui-element-children-fn element)))))
      ;; Generic lazy tree behavior + expand/collapse navigation.
      (let ((root-calls 0)
            (child-calls 0))
        (let* ((root-node
                 (funcall make-tree-node-fn
                          :label "root"
                          :expanded-p nil
                          :children-fn
                          (lambda ()
                            (incf root-calls)
                            (list
                             (funcall make-tree-node-fn
                                      :label "child-a"
                                      :children-fn
                                      (lambda ()
                                        (incf child-calls)
                                        (list (funcall make-tree-node-fn
                                                       :label "leaf-a"))))
                             (funcall make-tree-node-fn :label "child-b")))))
               (state
                 (funcall make-tree-browser-state-fn
                          :root-node root-node
                          :show-root-p t
                          :active-p t
                          :visible-row-count 20)))
          (assert-true (= (length (funcall tree-browser-visible-entries-fn state)) 1)
                       "Expected collapsed tree to render root only.")
          (assert-true (= root-calls 0)
                       "Expected children-fn not to run before expand, got ~D calls."
                       root-calls)

          (assert-true (funcall tree-browser-handle-key-fn state :enter)
                       "Expected Enter to expand selected root node.")
          (assert-true (= (length (funcall tree-browser-visible-entries-fn state)) 3)
                       "Expected expanded root to reveal children.")
          (assert-true (= root-calls 1)
                       "Expected root children-fn to run exactly once after expand, got ~D."
                       root-calls)
          (funcall tree-browser-visible-entries-fn state)
          (assert-true (= root-calls 1)
                       "Expected cached root children to avoid re-loading.")

          (funcall tree-browser-handle-key-fn state :down)
          (assert-true (= (funcall tree-browser-selected-index-fn state) 1)
                       "Expected Down navigation to move selection to first child.")
          (assert-true (funcall tree-browser-handle-key-fn state :right)
                       "Expected Right to expand selected child node.")
          (assert-true (= child-calls 1)
                       "Expected nested children-fn to run once on first expand.")
          (assert-true (= (length (funcall tree-browser-visible-entries-fn state)) 4)
                       "Expected nested expand to reveal leaf.")
          (assert-true (funcall tree-browser-handle-key-fn state :left)
                       "Expected Left to collapse selected expanded child.")
          (assert-true (funcall tree-browser-handle-key-fn state :right)
                       "Expected Right to re-expand child using cached children.")
          (assert-true (= child-calls 1)
                       "Expected re-expand to reuse cached children without extra loads.")
          (assert-true (funcall tree-browser-handle-key-fn state :right)
                       "Expected Right on expanded node to move selection into first child.")

          (let* ((selected-node (funcall tree-browser-selected-node-fn state))
                 (selected-label (and selected-node (funcall tree-node-label-fn selected-node)))
                 (widget (funcall make-tree-browser-widget-fn state))
                 (lines (collect-tree-text-lines widget)))
            (assert-true (string= selected-label "leaf-a")
                         "Expected right on expanded child to move to first child; got ~S."
                         selected-label)
            (assert-true (some (lambda (line)
                                 (search "child-a" line :test #'char-equal))
                               lines)
                         "Expected rendered tree rows to include child-a label."))))

      ;; Git-aware file tree statuses (M/A/?/D).
      (let* ((root (make-temp-root "amoebum-tree-browser-smoke"))
             (m-path (merge-pathnames #P"m.txt" root))
             (d-path (merge-pathnames #P"d.txt" root))
             (a-path (merge-pathnames #P"a.txt" root))
             (q-path (merge-pathnames #P"q.txt" root)))
        (ensure-directories-exist (merge-pathnames #P"placeholder" root))
        (run-command root '("git" "init"))
        (run-command root '("git" "config" "user.email" "smoke@example.com"))
        (run-command root '("git" "config" "user.name" "Smoke Test"))
        (write-file m-path "base")
        (write-file d-path "delete me")
        (run-command root '("git" "add" "m.txt" "d.txt"))
        (run-command root '("git" "commit" "-m" "init"))
        (write-file m-path "modified")
        (write-file a-path "added")
        (run-command root '("git" "add" "a.txt"))
        (write-file q-path "untracked")
        (run-command root '("git" "rm" "d.txt"))

        (let* ((state (funcall make-git-file-tree-browser-state-fn
                               :root root
                               :show-root-p t
                               :active-p t
                               :visible-row-count 40))
               (widget (funcall make-tree-browser-widget-fn state))
               (lines (collect-tree-text-lines widget)))
          (assert-true (some (lambda (line)
                               (and (search "[M]" line :test #'char-equal)
                                    (search "m.txt" line :test #'char-equal)))
                             lines)
                       "Expected modified file indicator [M] for m.txt. Rows: ~S"
                       lines)
          (assert-true (some (lambda (line)
                               (and (search "[A]" line :test #'char-equal)
                                    (search "a.txt" line :test #'char-equal)))
                             lines)
                       "Expected added file indicator [A] for a.txt. Rows: ~S"
                       lines)
          (assert-true (some (lambda (line)
                               (and (search "[?]" line :test #'char-equal)
                                    (search "q.txt" line :test #'char-equal)))
                             lines)
                       "Expected untracked file indicator [?] for q.txt. Rows: ~S"
                       lines)
          (assert-true (some (lambda (line)
                               (and (search "[D]" line :test #'char-equal)
                                    (search "d.txt" line :test #'char-equal)))
                             lines)
                       "Expected deleted file indicator [D] for d.txt. Rows: ~S"
                       lines)))

      ;; Chat wiring: tree browser appears in chat UI tree.
      (let* ((chat-state (funcall chat-make-state-fn :stream-runner nil))
             (chat-state (funcall chat-handle-event-fn
                                  chat-state
                                  (funcall make-key-event-fn :right)))
             (chat-tree (funcall chat-build-tree-fn chat-state 120 32)))
        (assert-true (tree-has-id-p chat-tree :tree-browser)
                     "Expected chat UI tree to include :tree-browser widget."))))

  (format t "AMOEBUM_TREE_BROWSER_SMOKE_OK~%"))
