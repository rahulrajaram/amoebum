(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Overlay Appearance Tests (fuzzy picker, approval dialog, tree browser)
;;; ---------------------------------------------------------------------------

(def-suite overlay-appearance-suite :in amoebum-suite
  :description "Snapshot tests for overlay/popup components rendered within the chat UI.")

(in-suite overlay-appearance-suite)

(defun %overlay-snapshot-path (name)
  (merge-pathnames (pathname (format nil "~A.snap" name))
                   +chat-snapshot-dir*))

(defun %assert-overlay-snapshot (buffer name)
  (multiple-value-bind (pass diff)
      (ptui.test-support.harness:assert-snapshot
       buffer
       (%overlay-snapshot-path name)
       :test-name (string-capitalize (princ-to-string name)))
    (is (eq t pass))
    (is (null diff))))

(defun %overlay-test-chat-state (&key messages (input-text ""))
  "Build a chat-ui-state for overlay appearance tests."
  (with-safe-chat-env
    (let ((state (%safe-make-chat-ui-state :branch-name "test/overlay")))
      (dolist (msg messages)
        (amoebum:chat-ui-add-message state (first msg) (second msg)))
      (when (plusp (length input-text))
        (amoebum:chat-ui-set-input state input-text))
      state)))

;;; --- Fuzzy Picker Tests ---

(test overlay-fuzzy-picker-with-results
  "Fuzzy picker active with matching file results."
  (let* ((state (%overlay-test-chat-state
                 :messages '(("user" "checking files"))
                 :input-text "@main")))
    ;; The @main input should have triggered fuzzy picker sync via chat-ui-set-input.
    ;; Force the picker active with synthetic results for deterministic snapshot.
    (let ((picker (amoebum::%ensure-chat-fuzzy-picker-state state)))
      (setf (amoebum::fuzzy-picker-state-active-p picker) t
            (amoebum::fuzzy-picker-state-query picker) "main"
            (amoebum::fuzzy-picker-state-context-label picker) "@ file/dir"
            (amoebum::fuzzy-picker-state-top-results picker)
            (list (amoebum::%make-fuzzy-match :path "amoebum/src/main.lisp" :score 1000 :kind :fuzzy :spans '() :depth 2)
                  (amoebum::%make-fuzzy-match :path "ptui/src/main.lisp" :score 900 :kind :fuzzy :spans '() :depth 2)
                  (amoebum::%make-fuzzy-match :path "sw4rm-sdk/src/main.lisp" :score 800 :kind :fuzzy :spans '() :depth 2))
            (amoebum::fuzzy-picker-state-selected-index picker) 0
            (amoebum::fuzzy-picker-state-scan-complete-p picker) t))
    (let ((buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
      (%assert-overlay-snapshot buffer "fuzzy-picker-with-results"))))

(test overlay-fuzzy-picker-no-matches
  "Fuzzy picker active with no matching results."
  (let* ((state (%overlay-test-chat-state
                 :input-text "@zzznomatchzzz")))
    (let ((picker (amoebum::%ensure-chat-fuzzy-picker-state state)))
      (setf (amoebum::fuzzy-picker-state-active-p picker) t
            (amoebum::fuzzy-picker-state-query picker) "zzznomatchzzz"
            (amoebum::fuzzy-picker-state-context-label picker) "@ file/dir"
            (amoebum::fuzzy-picker-state-top-results picker) '()
            (amoebum::fuzzy-picker-state-scan-complete-p picker) t))
    (let ((buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
      (%assert-overlay-snapshot buffer "fuzzy-picker-no-matches"))))

(test overlay-fuzzy-picker-selection-moved
  "Fuzzy picker with selection on the second item."
  (let* ((state (%overlay-test-chat-state
                 :input-text "@src")))
    (let ((picker (amoebum::%ensure-chat-fuzzy-picker-state state)))
      (setf (amoebum::fuzzy-picker-state-active-p picker) t
            (amoebum::fuzzy-picker-state-query picker) "src"
            (amoebum::fuzzy-picker-state-context-label picker) "@ file/dir"
            (amoebum::fuzzy-picker-state-top-results picker)
            (list (amoebum::%make-fuzzy-match :path "amoebum/src/" :score 1000 :kind :fuzzy :spans '() :depth 1)
                  (amoebum::%make-fuzzy-match :path "ptui/src/" :score 900 :kind :fuzzy :spans '() :depth 1)
                  (amoebum::%make-fuzzy-match :path "sw4rm-sdk/src/" :score 800 :kind :fuzzy :spans '() :depth 1))
            (amoebum::fuzzy-picker-state-selected-index picker) 1
            (amoebum::fuzzy-picker-state-scan-complete-p picker) t))
    (let ((buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
      (%assert-overlay-snapshot buffer "fuzzy-picker-selection-moved"))))

;;; --- Approval Dialog Tests ---

(test overlay-approval-dialog-initial
  "Approval dialog for a tool call, no option selected yet."
  (let* ((state (%overlay-test-chat-state
                 :messages '(("assistant" "Let me check the files in this directory."))))
         (dialog (amoebum::chat-ui-state-approval-dialog-state state)))
    (amoebum:approval-dialog-activate! dialog "glob-files"
                                       :command "glob-files src/**/*.lisp"
                                       :decision-id "test-001")
    (let ((buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
      (%assert-overlay-snapshot buffer "approval-dialog-initial"))))

(test overlay-approval-dialog-approve-selected
  "Approval dialog with Approve option highlighted."
  (let* ((state (%overlay-test-chat-state
                 :messages '(("assistant" "I will search for matching files."))))
         (dialog (amoebum::chat-ui-state-approval-dialog-state state)))
    (amoebum:approval-dialog-activate! dialog "search-files"
                                       :command "search-files --pattern 'defun'"
                                       :decision-id "test-002")
    (setf (amoebum:approval-dialog-state-selected-option dialog) :approve)
    (let ((buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
      (%assert-overlay-snapshot buffer "approval-dialog-approve"))))

(test overlay-approval-dialog-deny-selected
  "Approval dialog with Deny option highlighted."
  (let* ((state (%overlay-test-chat-state
                 :messages '(("assistant" "I need to run a shell command."))))
         (dialog (amoebum::chat-ui-state-approval-dialog-state state)))
    (amoebum:approval-dialog-activate! dialog "bash-exec"
                                       :command "rm -rf /tmp/test"
                                       :reason "Destructive shell command"
                                       :decision-id "test-003")
    (setf (amoebum:approval-dialog-state-selected-option dialog) :deny)
    (let ((buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
      (%assert-overlay-snapshot buffer "approval-dialog-deny"))))

;;; --- Tree Browser Tests ---

(test overlay-tree-browser-collapsed
  "Tree browser with root node collapsed."
  (let* ((state (%overlay-test-chat-state)))
    ;; Replace the tree-browser state with a deterministic one
    (let ((tree (amoebum::make-tree-browser-state
                 :root-node (amoebum::make-tree-node
                             :label "amoebum"
                             :expanded-p nil
                             :metadata (list :path "" :directory-p t)
                             :children-fn (lambda () '()))
                 :show-root-p t
                 :active-p t
                 :visible-row-count 3)))
      (setf (amoebum::chat-ui-state-tree-browser-state state) tree))
    (let ((buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
      (%assert-overlay-snapshot buffer "tree-browser-collapsed"))))

(test overlay-tree-browser-expanded
  "Tree browser with root expanded showing child nodes."
  (let* ((state (%overlay-test-chat-state)))
    (let* ((children (list (amoebum::make-tree-node
                            :label "src/"
                            :metadata (list :path "src/" :directory-p t)
                            :children-fn (lambda () '()))
                           (amoebum::make-tree-node
                            :label "test/"
                            :metadata (list :path "test/" :directory-p t)
                            :children-fn (lambda () '()))
                           (amoebum::make-tree-node
                            :label "README.md"
                            :metadata (list :path "README.md" :directory-p nil))))
           (root (amoebum::make-tree-node
                  :label "amoebum"
                  :expanded-p t
                  :metadata (list :path "" :directory-p t)
                  :children-fn (lambda () children)))
           (tree (amoebum::make-tree-browser-state
                  :root-node root
                  :show-root-p t
                  :active-p t
                  :visible-row-count 5
                  :selected-index 1)))
      ;; Pre-load children so expansion is visible
      (setf (amoebum::tree-node-children-loaded-p root) t
            (amoebum::tree-node-children-cache root) children)
      (setf (amoebum::chat-ui-state-tree-browser-state state) tree))
    (let ((buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
      (%assert-overlay-snapshot buffer "tree-browser-expanded"))))

(test overlay-tree-browser-git-status
  "Tree browser showing git status badges (M, A)."
  (let* ((state (%overlay-test-chat-state)))
    (let* ((children (list (amoebum::make-tree-node
                            :label "modified-file.lisp"
                            :metadata (list :path "modified-file.lisp" :directory-p nil))
                           (amoebum::make-tree-node
                            :label "new-file.lisp"
                            :metadata (list :path "new-file.lisp" :directory-p nil))
                           (amoebum::make-tree-node
                            :label "clean-file.lisp"
                            :metadata (list :path "clean-file.lisp" :directory-p nil))))
           (root (amoebum::make-tree-node
                  :label "project"
                  :expanded-p t
                  :metadata (list :path "" :directory-p t)
                  :children-fn (lambda () children)))
           (git-map (make-hash-table :test #'equal))
           (tree (amoebum::make-tree-browser-state
                  :root-node root
                  :show-root-p t
                  :active-p t
                  :visible-row-count 5
                  :git-status-map git-map)))
      (setf (gethash "modified-file.lisp" git-map) :modified
            (gethash "new-file.lisp" git-map) :added)
      (setf (amoebum::tree-node-children-loaded-p root) t
            (amoebum::tree-node-children-cache root) children)
      (setf (amoebum::chat-ui-state-tree-browser-state state) tree))
    (let ((buffer (%safe-render-chat-ui state :cols 84 :rows 20)))
      (%assert-overlay-snapshot buffer "tree-browser-git-status"))))
