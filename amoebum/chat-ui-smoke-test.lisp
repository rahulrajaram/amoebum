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
    (funcall load-asd-fn (merge-pathnames #P"sw4rm-sdk/sw4rm-sdk.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"ptui/ptui.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"amoebum/amoebum.asd" repo-root))
    (funcall load-system-fn "amoebum"))

  (let* ((amoebum-pkg (or (find-package "AMOEBUM")
                          (error "Missing package AMOEBUM after load.")))
         (events-pkg (or (find-package "PTUI.CORE.EVENTS")
                         (error "Missing package PTUI.CORE.EVENTS after load.")))
         (types-pkg (or (find-package "PTUI.CORE.TYPES")
                        (error "Missing package PTUI.CORE.TYPES after load.")))
         (elements-pkg (or (find-package "PTUI.UI.ELEMENTS")
                           (error "Missing package PTUI.UI.ELEMENTS after load.")))
         (plan-presentation-pkg
           (or (find-package "PTUI.COMPONENTS.PLAN-PRESENTATION")
               (error "Missing package PTUI.COMPONENTS.PLAN-PRESENTATION after load.")))
         (pseudopod-pkg (or (find-package "PSEUDOPOD")
                            (error "Missing package PSEUDOPOD after load.")))
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn-in
           (lambda (name package)
             (symbol-function (funcall symbol-in name package))))
         (make-chat-ui-state-fn (funcall fn-in "MAKE-CHAT-UI-STATE" amoebum-pkg))
         (chat-ui-build-tree-fn (funcall fn-in "CHAT-UI-BUILD-TREE" amoebum-pkg))
         (chat-ui-add-message-fn (funcall fn-in "CHAT-UI-ADD-MESSAGE" amoebum-pkg))
         (chat-ui-submit-input-fn (funcall fn-in "CHAT-UI-SUBMIT-INPUT" amoebum-pkg))
         (chat-ui-state-input-text-fn (funcall fn-in "CHAT-UI-STATE-INPUT-TEXT" amoebum-pkg))
         (chat-ui-state-messages-fn (funcall fn-in "CHAT-UI-STATE-MESSAGES" amoebum-pkg))
         (chat-ui-state-scrollback-fn (funcall fn-in "CHAT-UI-STATE-MESSAGE-SCROLLBACK-LINES" amoebum-pkg))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (current-plan-state-fn (funcall fn-in "CURRENT-PLAN-MODE-STATE" amoebum-pkg))
         (clear-plan-steps-fn (funcall fn-in "CLEAR-PLAN-MODE-STEPS" amoebum-pkg))
         (add-plan-step-fn (funcall fn-in "ADD-PLAN-STEP" amoebum-pkg))
         (set-plan-step-approvals-fn (funcall fn-in "SET-PLAN-STEP-APPROVALS" amoebum-pkg))
         (reset-plan-execution-state-fn (funcall fn-in "RESET-PLAN-EXECUTION-STATE" amoebum-pkg))
         (initialize-plan-execution-fn (funcall fn-in "INITIALIZE-PLAN-EXECUTION" amoebum-pkg))
         (plan-execution-state-run-id-fn (funcall fn-in "PLAN-EXECUTION-STATE-RUN-ID" amoebum-pkg))
         (plan-execution-append-output-fn (funcall fn-in "PLAN-EXECUTION-APPEND-OUTPUT" amoebum-pkg))
         (current-event-bus-fn (funcall fn-in "CURRENT-EVENT-BUS" amoebum-pkg))
         (publish-fn (funcall fn-in "PUBLISH" amoebum-pkg))
         (make-plan-step-status-event-fn (funcall fn-in "MAKE-PLAN-STEP-STATUS-EVENT" amoebum-pkg))
         (plan-output-stdin-policy-fn
           (funcall fn-in "%CHAT-PLAN-OUTPUT-STDIN-CAPTURE-POLICY" amoebum-pkg))
         (chat-role-cell-fn (funcall fn-in "CHAT-ROLE-CELL" amoebum-pkg))
         (chat-ui-set-input-fn (funcall fn-in "CHAT-UI-SET-INPUT" amoebum-pkg))
         (render-chat-ui-buffer-fn (funcall fn-in "RENDER-CHAT-UI-BUFFER" amoebum-pkg))
         (handle-chat-ui-event-fn (funcall fn-in "HANDLE-CHAT-UI-EVENT" amoebum-pkg))
         (make-key-event-fn (funcall fn-in "MAKE-KEY-EVENT" events-pkg))
         (make-size-fn (funcall fn-in "MAKE-SIZE" types-pkg))
         (buffer-cols-fn (funcall fn-in "CELL-BUFFER-COLS" types-pkg))
         (buffer-rows-fn (funcall fn-in "CELL-BUFFER-ROWS" types-pkg))
         (buffer-cells-fn (funcall fn-in "CELL-BUFFER-CELLS" types-pkg))
         (cell-glyph-fn (funcall fn-in "CELL-GLYPH" types-pkg))
         (cell-fg-fn (funcall fn-in "CELL-FG" types-pkg))
         (ui-element-id-fn (funcall fn-in "UI-ELEMENT-ID" elements-pkg))
         (ui-element-type-fn (funcall fn-in "UI-ELEMENT-TYPE" elements-pkg))
         (ui-element-children-fn (funcall fn-in "UI-ELEMENT-CHILDREN" elements-pkg))
         (make-plan-mode-presentation-widget-sym
           (funcall symbol-in "MAKE-PLAN-MODE-PRESENTATION-WIDGET" plan-presentation-pkg))
         (message-role-fn (funcall fn-in "MESSAGE-ROLE" pseudopod-pkg))
         (message-content-fn (funcall fn-in "MESSAGE-CONTENT" pseudopod-pkg))
         (content-part-text-fn (funcall fn-in "CONTENT-PART-TEXT" pseudopod-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (tree-has-type-p (node type)
               (or (eq (funcall ui-element-type-fn node) type)
                   (some (lambda (child)
                           (tree-has-type-p child type))
                         (funcall ui-element-children-fn node))))
             (tree-has-id-p (node id)
               (or (equal (funcall ui-element-id-fn node) id)
                   (some (lambda (child)
                           (tree-has-id-p child id))
                         (funcall ui-element-children-fn node))))
             (buffer-cell-at (buffer col row)
               (let* ((cols (funcall buffer-cols-fn buffer))
                      (cells (funcall buffer-cells-fn buffer))
                      (index (+ col (* row cols))))
                 (svref cells index)))
             (buffer-lines (buffer)
               (let* ((cols (funcall buffer-cols-fn buffer))
                      (rows (funcall buffer-rows-fn buffer)))
                 (loop for row from 0 below rows
                       collect
                       (with-output-to-string (out)
                         (loop for col from 0 below cols do
                           (let ((glyph (funcall cell-glyph-fn
                                                 (buffer-cell-at buffer col row))))
                             (when (> (length glyph) 0)
                               (write-string glyph out))))))))
             (rows-contain-p (rows text)
               (loop for row in rows
                     thereis (search text row :test #'char-equal)))
             (find-prefix-position (rows prefix)
               (loop for row-text in rows
                     for row-index from 0 do
                       (let ((col-index (search prefix row-text :test #'char-equal)))
                         (when col-index
                           (return (values row-index col-index))))))
             (prefix-fg-string (buffer prefix)
               (let ((rows (buffer-lines buffer)))
                 (multiple-value-bind (row col)
                     (find-prefix-position rows prefix)
                   (when (and row col)
                     (let ((cell (buffer-cell-at buffer col row)))
                       (prin1-to-string (funcall cell-fg-fn cell)))))))
             (message-text (message)
               (with-output-to-string (out)
                 (loop for part in (funcall message-content-fn message)
                       for index from 0 do
                         (when (> index 0)
                           (write-char #\Newline out))
                         (write-string (or (funcall content-part-text-fn part) "")
                                       out))))
             (make-chat-state ()
               (funcall make-chat-ui-state-fn :stream-runner nil))
             (make-text-event (text)
               (funcall make-key-event-fn :text :text? text)))
      (let ((tree (funcall chat-ui-build-tree-fn
                           (make-chat-state)
                           100
                           24)))
        (assert-true (tree-has-type-p tree :prompt-box)
                     "Expected chat UI tree to include a prompt-box widget."))

      (let ((state (make-chat-state)))
        (funcall setconfig-fn :plan-mode t)
        (funcall clear-plan-steps-fn)
        (funcall add-plan-step-fn
                 "Inspect preview output with `rg -n plan`."
                 :file-paths (list "amoebum/src/ui/chat.lisp")
                 :state (funcall current-plan-state-fn))
        (funcall add-plan-step-fn
                 "Validate dry-run rendering with `timeout 60 ./ptui/bin/test.sh`."
                 :file-paths (list "ptui/src/components/plan-presentation.lisp")
                 :risk :high
                 :state (funcall current-plan-state-fn))
        (assert-true (eq (funcall plan-output-stdin-policy-fn) :disabled)
                     "Expected plan-mode output stdin policy helper to resolve :disabled.")
        (let* ((original-plan-widget-fn
                 (symbol-function make-plan-mode-presentation-widget-sym))
               (captured-output-stdin-policy :missing))
          (unwind-protect
              (progn
                (setf (symbol-function make-plan-mode-presentation-widget-sym)
                      (lambda (&rest args &key &allow-other-keys)
                        (setf captured-output-stdin-policy
                              (getf args :output-stdin-capture-policy :missing))
                        (apply original-plan-widget-fn args)))
                (let* ((tree (funcall chat-ui-build-tree-fn state 110 26))
                       (buffer (funcall render-chat-ui-buffer-fn
                                        state
                                        (funcall make-size-fn 110 26)))
                       (rows (buffer-lines buffer)))
                  (assert-true (eq captured-output-stdin-policy :disabled)
                               "Expected plan presentation to force :output-stdin-capture-policy :disabled, got ~S."
                               captured-output-stdin-policy)
                  (assert-true (tree-has-id-p tree :chat-plan-presentation)
                               "Expected chat UI tree to include the plan-mode presentation widget.")
                  (assert-true (rows-contain-p rows "Plan Mode Workspace")
                               "Expected rendered chat UI to show plan-mode presentation title.")
                  (assert-true (rows-contain-p rows "Plan Steps")
                               "Expected rendered chat UI to show plan steps panel heading.")
                  (assert-true (rows-contain-p rows "Plan Output")
                               "Expected rendered chat UI to show plan output panel heading.")
                  (assert-true (rows-contain-p rows "DRY-RUN>")
                               "Expected plan output terminal pane to show dry-run command previews.")
                  (assert-true (rows-contain-p rows "timeout 60 ./ptui/bin/test.sh")
                               "Expected dry-run command preview to include proposed shell command.")
                  (assert-true (rows-contain-p rows "Context Inspector")
                               "Expected rendered chat UI to show context inspector heading.")
                  (assert-true (rows-contain-p rows "Selected step: 1")
                               "Expected initial plan-step selection to target step 1.")
                  (assert-true (rows-contain-p rows "amoebum/src/ui/chat.lisp")
                               "Expected selected step context to include first-step file reference.")))
            (setf (symbol-function make-plan-mode-presentation-widget-sym)
                  original-plan-widget-fn)))
        (funcall reset-plan-execution-state-fn)
        (funcall setconfig-fn :plan-mode nil)
        (assert-true (eq (funcall plan-output-stdin-policy-fn) :enabled)
                     "Expected non-plan output stdin policy helper to resolve :enabled.")
        (funcall clear-plan-steps-fn))

      (let ((state (make-chat-state)))
        (funcall reset-plan-execution-state-fn)
        (funcall clear-plan-steps-fn)
        (let ((plan-state (funcall current-plan-state-fn)))
          (funcall add-plan-step-fn
                   "Run `rg -n plan`."
                   :file-paths (list "amoebum/src/ui/chat.lisp")
                   :state plan-state)
          (funcall add-plan-step-fn
                   "Run `./bin/yarli-run-verification.sh`."
                   :file-paths (list "amoebum/chat-ui-smoke-test.lisp")
                   :state plan-state)
          (funcall set-plan-step-approvals-fn '(1 2) :state plan-state)
          (funcall setconfig-fn :plan-mode nil)
          (let ((execution-state (funcall initialize-plan-execution-fn :plan-state plan-state)))
            (let ((run-id (funcall plan-execution-state-run-id-fn execution-state)))
              (funcall publish-fn
                       (funcall current-event-bus-fn)
                       (funcall make-plan-step-status-event-fn
                                :run-id run-id
                                :step-index 1
                                :status :running
                                :description "Executing preview command.")))
            (funcall plan-execution-append-output-fn
                     "LIVE> [step 1 running] executing rg -n plan."
                     :step-index 1
                     :phase :execution
                     :style :meta
                     :state execution-state)
            (assert-true (eq (funcall plan-output-stdin-policy-fn) :disabled)
                         "Expected execution continuity surface to keep output stdin policy disabled.")
            (let* ((tree (funcall chat-ui-build-tree-fn state 110 26))
                   (buffer (funcall render-chat-ui-buffer-fn
                                    state
                                    (funcall make-size-fn 110 26)))
                   (rows (buffer-lines buffer)))
              (assert-true (tree-has-id-p tree :chat-plan-presentation)
                           "Expected chat UI tree to keep plan presentation widget after execution handoff.")
              (assert-true (rows-contain-p rows "Plan Mode Workspace")
                           "Expected execution handoff to preserve plan workspace container.")
              (assert-true (rows-contain-p rows "Execution continuity initialized for run")
                           "Expected execution continuity initialization line in terminal pane output.")
              (assert-true (rows-contain-p rows "LIVE> [step 1 running]")
                           "Expected execution terminal pane output to include live running line.")
              (assert-true (rows-contain-p rows "status: running")
                           "Expected plan-step status badge to update to running from event bus."))
            (let ((run-id (funcall plan-execution-state-run-id-fn execution-state)))
              (funcall publish-fn
                       (funcall current-event-bus-fn)
                       (funcall make-plan-step-status-event-fn
                                :run-id run-id
                                :step-index 1
                                :status :done
                                :description "Execution complete."))
              (funcall publish-fn
                       (funcall current-event-bus-fn)
                       (funcall make-plan-step-status-event-fn
                                :run-id run-id
                                :step-index 2
                                :status :blocked
                                :description "Execution blocked by dependency."))
              (let* ((buffer (funcall render-chat-ui-buffer-fn
                                      state
                                      (funcall make-size-fn 110 26)))
                     (rows (buffer-lines buffer)))
                (assert-true (rows-contain-p rows "status: done")
                             "Expected plan-step status badge to update to done from event bus.")
                (assert-true (rows-contain-p rows "status: blocked")
                             "Expected plan-step status badge to update to blocked from event bus.")))))
        (funcall reset-plan-execution-state-fn)
        (assert-true (eq (funcall plan-output-stdin-policy-fn) :enabled)
                     "Expected output stdin policy helper to return :enabled after execution continuity reset.")
        (funcall clear-plan-steps-fn))

      (let ((state (make-chat-state)))
        (funcall chat-ui-add-message-fn state :system "System primed.")
        (funcall chat-ui-add-message-fn state :user "Show me the diff.")
        (funcall chat-ui-add-message-fn state :assistant "I found 3 changed files.")
        (let* ((buffer (funcall render-chat-ui-buffer-fn
                                state
                                (funcall make-size-fn 110 24)))
               (rows (buffer-lines buffer))
               (user-fg (prefix-fg-string buffer "YOU>"))
               (assistant-fg (prefix-fg-string buffer "ASSISTANT>"))
               (system-fg (prefix-fg-string buffer "SYSTEM>")))
          (assert-true (rows-contain-p rows "YOU> Show me the diff.")
                       "Expected rendered history to include user role prefix/text.")
          (assert-true (rows-contain-p rows "ASSISTANT> I found 3 changed files.")
                       "Expected rendered history to include assistant role prefix/text.")
          (assert-true (rows-contain-p rows "SYSTEM> System primed.")
                       "Expected rendered history to include system role prefix/text.")
          (assert-true (and user-fg assistant-fg system-fg)
                       "Expected to locate role-specific styled cells in rendered buffer.")
          (assert-true (not (string= user-fg assistant-fg))
                       "Expected user and assistant role styles to differ.")
          (assert-true (not (string= user-fg system-fg))
                       "Expected user and system role styles to differ.")
          (assert-true (not (string= assistant-fg system-fg))
                       "Expected assistant and system role styles to differ.")))

      (let ((state (make-chat-state)))
        (funcall render-chat-ui-buffer-fn state (funcall make-size-fn 70 16))
        (setf state (funcall handle-chat-ui-event-fn state (make-text-event "h")))
        (setf state (funcall handle-chat-ui-event-fn state (make-text-event "i")))
        (assert-true (string= (funcall chat-ui-state-input-text-fn state) "hi")
                     "Expected prompt input text to accumulate keystrokes.")
        (setf state (funcall handle-chat-ui-event-fn state (funcall make-key-event-fn :enter)))
        (assert-true (string= (funcall chat-ui-state-input-text-fn state) "")
                     "Expected Enter submission to clear prompt input.")
        (let* ((messages (funcall chat-ui-state-messages-fn state))
               (last-message (car (last messages))))
          (assert-true last-message
                       "Expected submitted input to append a user message.")
          (assert-true (string= (funcall message-role-fn last-message) "user")
                       "Expected submitted message role to be user.")
          (assert-true (string= (message-text last-message) "hi")
                       "Expected submitted message content to match prompt text."))
        (funcall chat-ui-set-input-fn state "manual submit")
        (funcall chat-ui-submit-input-fn state)
        (assert-true (>= (length (funcall chat-ui-state-messages-fn state)) 2)
                     "Expected CHAT-UI-SUBMIT-INPUT to append user message."))

      (let ((state (make-chat-state)))
        (loop for index from 1 to 30 do
          (funcall chat-ui-add-message-fn
                   state
                   :assistant
                   (format nil "entry-~D" index)))
        (let* ((size (funcall make-size-fn 60 10))
               (bottom-buffer (funcall render-chat-ui-buffer-fn state size))
               (bottom-rows (buffer-lines bottom-buffer)))
          (assert-true (rows-contain-p bottom-rows "entry-30")
                       "Expected default history view to show newest messages.")
          (dotimes (n 20)
            (declare (ignore n))
            (setf state (funcall handle-chat-ui-event-fn
                                 state
                                 (funcall make-key-event-fn :pgup))))
          (let* ((top-buffer (funcall render-chat-ui-buffer-fn state size))
                 (top-rows (buffer-lines top-buffer)))
            (assert-true (> (funcall chat-ui-state-scrollback-fn state) 0)
                         "Expected PgUp to increase message scrollback.")
            (assert-true (rows-contain-p top-rows "entry-1")
                         "Expected scrolled history view to reach older messages."))
          (dotimes (n 20)
            (declare (ignore n))
            (setf state (funcall handle-chat-ui-event-fn
                                 state
                                 (funcall make-key-event-fn :pgdn))))
          (let* ((bottom-again-buffer (funcall render-chat-ui-buffer-fn state size))
                 (bottom-again-rows (buffer-lines bottom-again-buffer)))
            (assert-true (= (funcall chat-ui-state-scrollback-fn state) 0)
                         "Expected PgDn to return history scrollback to newest.")
            (assert-true (rows-contain-p bottom-again-rows "entry-30")
                         "Expected returned history view to show newest message again.")))))

  (format t "AMOEBUM_CHAT_UI_SMOKE_OK~%")))
