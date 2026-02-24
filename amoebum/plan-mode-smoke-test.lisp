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
         (pseudopod-pkg (or (find-package "PSEUDOPOD")
                            (error "Missing package PSEUDOPOD after load.")))
         (uiop-pkg (or (find-package "UIOP")
                       (find-package "ASDF/UTILITY")
                       (error "Missing UIOP package after requiring ASDF.")))
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn-in
           (lambda (name package)
             (symbol-function (funcall symbol-in name package))))
         (dispatch-fn (funcall fn-in "DISPATCH-SLASH-COMMAND" amoebum-pkg))
         (handle-input-key-fn (funcall fn-in "%HANDLE-INPUT-KEY" amoebum-pkg))
         (result-output-fn (funcall fn-in "SLASH-COMMAND-RESULT-OUTPUT" amoebum-pkg))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (current-config-fn (funcall fn-in "CURRENT-CONFIG" amoebum-pkg))
         (config-value-fn (funcall fn-in "CONFIG-VALUE" amoebum-pkg))
         (make-status-bar-state-fn (funcall fn-in "MAKE-STATUS-BAR-STATE" amoebum-pkg))
         (make-chat-ui-state-fn (funcall fn-in "MAKE-CHAT-UI-STATE" amoebum-pkg))
         (chat-ui-set-input-fn (funcall fn-in "CHAT-UI-SET-INPUT" amoebum-pkg))
         (chat-ui-state-messages-fn (funcall fn-in "CHAT-UI-STATE-MESSAGES" amoebum-pkg))
         (message-role-fn (funcall fn-in "MESSAGE-ROLE" pseudopod-pkg))
         (message-content-fn (funcall fn-in "MESSAGE-CONTENT" pseudopod-pkg))
         (content-part-text-fn (funcall fn-in "CONTENT-PART-TEXT" pseudopod-pkg))
         (status-bar-line-fn (funcall fn-in "STATUS-BAR-LINE" amoebum-pkg))
         (clear-steps-fn (funcall fn-in "CLEAR-PLAN-MODE-STEPS" amoebum-pkg))
         (add-plan-step-fn (funcall fn-in "ADD-PLAN-STEP" amoebum-pkg))
         (current-plan-state-fn (funcall fn-in "CURRENT-PLAN-MODE-STATE" amoebum-pkg))
         (plan-mode-steps-fn (funcall fn-in "PLAN-MODE-STATE-STEPS" amoebum-pkg))
         (plan-approved-steps-fn (funcall fn-in "PLAN-MODE-STATE-APPROVED-STEP-INDEXES" amoebum-pkg))
         (plan-step-description-fn (funcall fn-in "PLAN-STEP-DESCRIPTION" amoebum-pkg))
         (plan-step-depends-on-fn (funcall fn-in "PLAN-STEP-DEPENDS-ON" amoebum-pkg))
         (plan-review-pending-fn (funcall fn-in "PLAN-MODE-STATE-REVIEW-PENDING-P" amoebum-pkg))
         (plan-review-decision-fn (funcall fn-in "PLAN-MODE-STATE-REVIEW-DECISION" amoebum-pkg))
         (plan-review-notes-fn (funcall fn-in "PLAN-MODE-STATE-REVIEW-NOTES" amoebum-pkg))
         (plan-output-path-fn (funcall fn-in "PLAN-MODE-STATE-LAST-OUTPUT-PATH" amoebum-pkg))
         (stream-markdown-styled-lines-fn (funcall fn-in "STREAM-MARKDOWN-STYLED-LINES" amoebum-pkg))
         (make-context-fn (funcall fn-in "MAKE-AMOEBUM-CONTEXT" amoebum-pkg))
         (execute-tool-fn (funcall fn-in "EXECUTE-TOOL" amoebum-pkg))
         (assemble-system-prompt-fn (funcall fn-in "ASSEMBLE-SYSTEM-PROMPT" amoebum-pkg))
         (toolset-sym (funcall symbol-in "*TOOLSET*" amoebum-pkg))
         (permission-denied-sym (funcall symbol-in "TOOL-PERMISSION-DENIED" amoebum-pkg))
         (make-tool-call-fn (funcall fn-in "MAKE-TOOL-CALL" pseudopod-pkg))
         (temporary-directory-fn (funcall fn-in "TEMPORARY-DIRECTORY" uiop-pkg))
         (ensure-directory-pathname-fn (funcall fn-in "ENSURE-DIRECTORY-PATHNAME" uiop-pkg))
         (read-file-string-fn (funcall fn-in "READ-FILE-STRING" uiop-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
            (contains-text-p (haystack needle)
               (and (stringp haystack)
                    (search needle haystack :test #'char-equal)))
            (message-text (message)
              (let ((parts (funcall message-content-fn message)))
                (with-output-to-string (out)
                  (loop for part in parts
                        for index from 0 do
                          (when (> index 0)
                            (write-char #\Newline out))
                          (write-string (or (funcall content-part-text-fn part) "") out)))))
            (message-role (message)
              (string-downcase (or (funcall message-role-fn message) "")))
            (bool-true-p (value)
               (not (null value))))
      (funcall setconfig-fn :plan-mode nil)
      (funcall setconfig-fn :permission-mode :full-auto)
      (funcall clear-steps-fn)

      (multiple-value-bind (handledp toggle-on-result)
          (funcall dispatch-fn "/plan")
        (assert-true handledp "Expected /plan toggle-on to be handled.")
        (assert-true (contains-text-p (funcall result-output-fn toggle-on-result)
                                      "Plan mode enabled")
                     "Expected /plan toggle-on output to mention enabled state, got ~S."
                     (funcall result-output-fn toggle-on-result)))
      (assert-true (bool-true-p (funcall config-value-fn :plan-mode (funcall current-config-fn)))
                   "Expected :plan-mode config value to be true after /plan toggle-on.")

      (multiple-value-bind (handledp toggle-off-result)
          (funcall dispatch-fn "/plan")
        (assert-true handledp "Expected /plan toggle-off to be handled.")
        (assert-true (contains-text-p (funcall result-output-fn toggle-off-result)
                                      "Plan mode disabled")
                     "Expected /plan toggle-off output to mention disabled state, got ~S."
                     (funcall result-output-fn toggle-off-result)))
      (assert-true (not (bool-true-p (funcall config-value-fn :plan-mode (funcall current-config-fn))))
                   "Expected :plan-mode config value to be false after /plan toggle-off.")

      (let ((status-state (funcall make-status-bar-state-fn
                                   :config (funcall current-config-fn))))
        (multiple-value-bind (handledp plan-on-result)
            (funcall dispatch-fn "/plan on")
          (assert-true handledp "Expected /plan on to be handled.")
          (assert-true (contains-text-p (funcall result-output-fn plan-on-result) "Plan mode enabled")
                       "Expected /plan on output to mention enabled state, got ~S."
                       (funcall result-output-fn plan-on-result)))
        (assert-true (bool-true-p (funcall config-value-fn :plan-mode (funcall current-config-fn)))
                     "Expected :plan-mode config value to be true after /plan on.")
        (assert-true (contains-text-p (funcall status-bar-line-fn status-state) "PLAN MODE -- read-only")
                     "Expected status bar to show plan mode banner while active."))

      (let ((assembled (funcall assemble-system-prompt-fn)))
        (assert-true (contains-text-p assembled "Plan Mode Guidance")
                     "Expected assembled system prompt to include plan mode guidance section.")
        (assert-true (contains-text-p assembled "Explore the codebase first")
                     "Expected plan mode guidance to instruct exploration before planning."))

      (let ((chat-state (funcall make-chat-ui-state-fn)))
        (funcall chat-ui-set-input-fn chat-state
                 "Please enter plan mode so we can do read-only planning first.")
        (funcall handle-input-key-fn chat-state :enter nil)
        (assert-true (bool-true-p (funcall config-value-fn :plan-mode (funcall current-config-fn)))
                     "Expected natural-language user instruction to enable :plan-mode.")
        (let* ((messages (funcall chat-ui-state-messages-fn chat-state))
               (saw-user-instruction
                 (loop for message in messages
                       thereis (and (string= (message-role message) "user")
                                    (contains-text-p (message-text message) "enter plan mode"))))
               (saw-system-ack
                 (loop for message in messages
                       thereis (and (string= (message-role message) "system")
                                    (or (contains-text-p (message-text message) "Plan mode enabled")
                                        (contains-text-p (message-text message) "Plan mode already enabled"))))))
          (assert-true saw-user-instruction
                       "Expected a user message to retain natural-language plan instruction.")
          (assert-true saw-system-ack
                       "Expected a system acknowledgment for inferred /plan on.")))
      (funcall add-plan-step-fn
               "Review target implementation files."
               :file-paths (list "amoebum/src/plan-mode.lisp"
                                 "amoebum/src/commands.lisp")
               :risk :low)
      (funcall add-plan-step-fn
               "Then update command parsing after step 1."
               :file-paths (list "amoebum/src/commands.lisp")
               :risk :medium)
      (funcall add-plan-step-fn
               "Assess integration boundary risk."
               :file-paths (list "amoebum/src/system-prompt.lisp")
               :risk "HIGH")
      (funcall add-plan-step-fn
               "Document rollout checklist."
               :file-paths (list "IMPLEMENTATION_PLAN.md")
               :risk :unknown)
      (let* ((plan-state (funcall current-plan-state-fn))
             (steps (funcall plan-mode-steps-fn plan-state))
             (second-step (second steps)))
        (assert-true (>= (length steps) 2)
                     "Expected at least two captured plan steps before plan mode exit, got ~D."
                     (length steps))
        (assert-true (equal '(1) (funcall plan-step-depends-on-fn second-step))
                     "Expected second step dependencies to infer '(1), got ~S."
                     (funcall plan-step-depends-on-fn second-step)))

      (let* ((tmp-root
               (funcall ensure-directory-pathname-fn
                        (merge-pathnames
                         (make-pathname :directory
                                        `(:relative ,(format nil "amoebum-i40-~A"
                                                              (get-universal-time))))
                         (funcall temporary-directory-fn))))
             (read-target (merge-pathnames #P"plan-mode-read.txt" tmp-root))
             (write-target (merge-pathnames #P"plan-mode-write.txt" tmp-root))
             (context (funcall make-context-fn
                               :toolset (symbol-value toolset-sym)
                               :permission-mode :full-auto)))
        (ensure-directories-exist read-target)
        (with-open-file (stream read-target
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
          (write-line "hello plan mode" stream))

        (let* ((read-call (funcall make-tool-call-fn
                                   :name "read-file"
                                   :arguments (format nil "{\"path\":\"~A\"}"
                                                      (namestring read-target))))
               (read-output (funcall execute-tool-fn read-call context)))
          (assert-true (contains-text-p read-output "hello plan mode")
                       "Expected read-file to remain allowed in plan mode, got ~S."
                       read-output))

        (let ((write-call (funcall make-tool-call-fn
                                   :name "write-file"
                                   :arguments (format nil
                                                      "{\"path\":\"~A\",\"content\":\"blocked\"}"
                                                      (namestring write-target))))
              (saw-denied nil))
          (handler-case
              (funcall execute-tool-fn write-call context)
            (error (condition)
              (when (typep condition permission-denied-sym)
                (setf saw-denied t))))
          (assert-true saw-denied
                       "Expected write-file to be blocked during plan mode.")))

      (let ((status-state (funcall make-status-bar-state-fn
                                   :config (funcall current-config-fn))))
        (multiple-value-bind (handledp plan-off-result)
            (funcall dispatch-fn "/plan off")
          (assert-true handledp "Expected /plan off to be handled.")
          (assert-true (contains-text-p (funcall result-output-fn plan-off-result)
                                        "Plan mode disabled")
                       "Expected /plan off output to mention disabled state, got ~S."
                       (funcall result-output-fn plan-off-result)))
        (assert-true (not (bool-true-p (funcall config-value-fn :plan-mode (funcall current-config-fn))))
                     "Expected :plan-mode config value to be false after /plan off.")
        (assert-true (not (contains-text-p (funcall status-bar-line-fn status-state)
                                           "PLAN MODE -- read-only"))
                     "Expected status bar to hide plan mode banner after exit."))

      (let* ((plan-state (funcall current-plan-state-fn))
             (output-path (funcall plan-output-path-fn plan-state))
             (output-text (and output-path
                               (probe-file output-path)
                               (funcall read-file-string-fn output-path))))
        (assert-true (and output-path (probe-file output-path))
                     "Expected plan mode exit to write plan output file, got ~S."
                     output-path)
        (assert-true (contains-text-p output-text "# Amoebum Plan")
                     "Expected plan output markdown header, got ~S."
                     output-text)
        (assert-true (contains-text-p output-text "Review target implementation files.")
                     "Expected plan output to include captured step, got ~S."
                     output-text)
        (assert-true (contains-text-p output-text "risk: low")
                     "Expected explicit low-risk annotation in plan markdown, got ~S."
                     output-text)
        (assert-true (contains-text-p output-text "risk: high")
                     "Expected string risk input to normalize to high-risk annotation, got ~S."
                     output-text)
        (assert-true (contains-text-p output-text "risk: medium")
                     "Expected unknown risk input to default to medium annotation, got ~S."
                     output-text)
        (assert-true (contains-text-p output-text "depends_on: 1")
                     "Expected plan output to include inferred dependency annotation, got ~S."
                     output-text)
        (assert-true (contains-text-p output-text "approved_step_count: 0")
                     "Expected plan output to include approved step count metadata, got ~S."
                     output-text)
        (assert-true (contains-text-p output-text "approved_for_execution: false")
                     "Expected plan output to initialize step approvals as false, got ~S."
                     output-text)
        (assert-true (bool-true-p (funcall plan-review-pending-fn plan-state))
                     "Expected plan review pending marker after plan mode exit.")
        (multiple-value-bind (handledp status-result)
            (funcall dispatch-fn "/plan status")
          (assert-true handledp "Expected /plan status to be handled.")
          (assert-true (contains-text-p (funcall result-output-fn status-result)
                                        "Plan review pending")
                       "Expected /plan status output to mention review pending, got ~S."
                       (funcall result-output-fn status-result))
          (assert-true (contains-text-p (funcall result-output-fn status-result)
                                        "/plan review")
                       "Expected /plan status output to suggest /plan review, got ~S."
                       (funcall result-output-fn status-result)))
        (multiple-value-bind (handledp review-result)
            (funcall dispatch-fn "/plan review")
          (assert-true handledp "Expected /plan review to be handled.")
          (assert-true (contains-text-p (funcall result-output-fn review-result)
                                        "Plan review:")
                       "Expected /plan review output heading, got ~S."
                       (funcall result-output-fn review-result))
          (assert-true (contains-text-p (funcall result-output-fn review-result)
                                        "# Amoebum Plan")
                       "Expected /plan review output to include plan markdown header, got ~S."
                       (funcall result-output-fn review-result))
          (assert-true (contains-text-p (funcall result-output-fn review-result)
                                        "Review target implementation files.")
                       "Expected /plan review output to include captured step text, got ~S."
                       (funcall result-output-fn review-result)))
        (multiple-value-bind (handledp partial-approve-result)
            (funcall dispatch-fn "/plan approve 1,3")
          (assert-true handledp "Expected /plan approve 1,3 to be handled.")
          (assert-true (contains-text-p (funcall result-output-fn partial-approve-result)
                                        "Approved step(s): 1, 3.")
                       "Expected /plan approve 1,3 to acknowledge approved step subset, got ~S."
                       (funcall result-output-fn partial-approve-result))
          (assert-true (contains-text-p (funcall result-output-fn partial-approve-result)
                                        "Current approval: 2/4.")
                       "Expected /plan approve 1,3 to report partial approval ratio, got ~S."
                       (funcall result-output-fn partial-approve-result)))
        (assert-true (equal '(1 3) (funcall plan-approved-steps-fn plan-state))
                     "Expected approved step indexes '(1 3) after /plan approve 1,3, got ~S."
                     (funcall plan-approved-steps-fn plan-state))
        (assert-true (bool-true-p (funcall plan-review-pending-fn plan-state))
                     "Expected partial step approval to keep plan review pending.")
        (assert-true (eq :partially-approved (funcall plan-review-decision-fn plan-state))
                     "Expected partial step approval to set decision :partially-approved, got ~S."
                     (funcall plan-review-decision-fn plan-state))
        (multiple-value-bind (handledp partial-status-result)
            (funcall dispatch-fn "/plan status")
          (assert-true handledp "Expected /plan status to be handled after partial approval.")
          (assert-true (contains-text-p (funcall result-output-fn partial-status-result)
                                        "Approved steps: 2/4 (1, 3)")
                       "Expected /plan status to include partial step approval summary, got ~S."
                       (funcall result-output-fn partial-status-result)))
        (multiple-value-bind (handledp partial-review-result)
            (funcall dispatch-fn "/plan review")
          (assert-true handledp "Expected /plan review after partial step approval.")
          (assert-true (contains-text-p (funcall result-output-fn partial-review-result)
                                        "Approved steps: 2/4 (1, 3).")
                       "Expected /plan review to include partial step approval summary, got ~S."
                       (funcall result-output-fn partial-review-result))
          (assert-true (contains-text-p (funcall result-output-fn partial-review-result)
                                        "approved_step_count: 2")
                       "Expected /plan review markdown to include updated approved step count, got ~S."
                       (funcall result-output-fn partial-review-result))
          (assert-true (contains-text-p (funcall result-output-fn partial-review-result)
                                        "approved_for_execution: true")
                       "Expected /plan review markdown to include step-level approval markers, got ~S."
                       (funcall result-output-fn partial-review-result)))
        (multiple-value-bind (handledp reorder-result)
            (funcall dispatch-fn "/plan reorder 3 1")
          (assert-true handledp "Expected /plan reorder 3 1 to be handled.")
          (assert-true (contains-text-p (funcall result-output-fn reorder-result)
                                        "Reordered step 3 to position 1.")
                       "Expected /plan reorder to acknowledge source/target indexes, got ~S."
                       (funcall result-output-fn reorder-result))
          (assert-true (contains-text-p (funcall result-output-fn reorder-result)
                                        "Approved steps: 2/4 (1, 2).")
                       "Expected /plan reorder to remap approved indexes in output, got ~S."
                       (funcall result-output-fn reorder-result)))
        (assert-true (equal '(1 2) (funcall plan-approved-steps-fn plan-state))
                     "Expected /plan reorder 3 1 to remap approved step indexes to '(1 2), got ~S."
                     (funcall plan-approved-steps-fn plan-state))
        (assert-true (eq :partially-approved (funcall plan-review-decision-fn plan-state))
                     "Expected /plan reorder to preserve partial approval decision, got ~S."
                     (funcall plan-review-decision-fn plan-state))
        (let* ((reordered-steps (funcall plan-mode-steps-fn plan-state))
               (first-step (first reordered-steps))
               (third-step (third reordered-steps)))
          (assert-true (contains-text-p (funcall plan-step-description-fn first-step)
                                        "Assess integration boundary risk.")
                       "Expected reordered step 1 description to match original step 3, got ~S."
                       (funcall plan-step-description-fn first-step))
          (assert-true (equal '(2) (funcall plan-step-depends-on-fn third-step))
                       "Expected dependency remap to track reordered step indexes, got ~S."
                       (funcall plan-step-depends-on-fn third-step)))
        (multiple-value-bind (handledp reordered-status-result)
            (funcall dispatch-fn "/plan status")
          (assert-true handledp "Expected /plan status to be handled after reorder.")
          (assert-true (contains-text-p (funcall result-output-fn reordered-status-result)
                                        "Approved steps: 2/4 (1, 2)")
                       "Expected /plan status to reflect remapped approvals after reorder, got ~S."
                       (funcall result-output-fn reordered-status-result)))
        (multiple-value-bind (handledp reordered-review-result)
            (funcall dispatch-fn "/plan review")
          (assert-true handledp "Expected /plan review after step reorder.")
          (assert-true (contains-text-p (funcall result-output-fn reordered-review-result)
                                        "1. Assess integration boundary risk.")
                       "Expected reordered /plan review markdown to show moved step at index 1, got ~S."
                       (funcall result-output-fn reordered-review-result))
          (assert-true (contains-text-p (funcall result-output-fn reordered-review-result)
                                        "3. Then update command parsing after step 1.")
                       "Expected reordered /plan review markdown to keep moved ordering for prior step 2, got ~S."
                       (funcall result-output-fn reordered-review-result))
          (assert-true (contains-text-p (funcall result-output-fn reordered-review-result)
                                        "depends_on: 2")
                       "Expected reordered /plan review markdown to remap dependency indexes, got ~S."
                       (funcall result-output-fn reordered-review-result)))
        (multiple-value-bind (handledp modify-result)
            (funcall dispatch-fn "/plan modify Split step 2 into two explicit steps.")
          (assert-true handledp "Expected /plan modify to be handled.")
          (assert-true (contains-text-p (funcall result-output-fn modify-result)
                                        "Plan modifications requested")
                       "Expected /plan modify output to acknowledge modification request, got ~S."
                       (funcall result-output-fn modify-result)))
        (assert-true (bool-true-p (funcall plan-review-pending-fn plan-state))
                     "Expected /plan modify to keep review pending.")
        (assert-true (eq :modification-requested
                         (funcall plan-review-decision-fn plan-state))
                     "Expected /plan modify to set decision :modification-requested, got ~S."
                     (funcall plan-review-decision-fn plan-state))
        (assert-true (contains-text-p (funcall plan-review-notes-fn plan-state)
                                      "Split step 2")
                     "Expected /plan modify to store review notes, got ~S."
                     (funcall plan-review-notes-fn plan-state))
        (multiple-value-bind (handledp request-mods-result)
            (funcall dispatch-fn "/plan request-modifications Add explicit rollback validation checks.")
          (assert-true handledp "Expected /plan request-modifications to be handled.")
          (assert-true (contains-text-p (funcall result-output-fn request-mods-result)
                                        "Plan modifications requested")
                       "Expected /plan request-modifications output to acknowledge modification request, got ~S."
                       (funcall result-output-fn request-mods-result)))
        (assert-true (eq :modification-requested
                         (funcall plan-review-decision-fn plan-state))
                     "Expected /plan request-modifications to set decision :modification-requested, got ~S."
                     (funcall plan-review-decision-fn plan-state))
        (assert-true (contains-text-p (funcall plan-review-notes-fn plan-state)
                                      "rollback validation")
                     "Expected /plan request-modifications to store review notes, got ~S."
                     (funcall plan-review-notes-fn plan-state))
        (multiple-value-bind (handledp request-changes-result)
            (funcall dispatch-fn "/plan request-changes Clarify dependency ordering.")
          (assert-true handledp "Expected /plan request-changes to be handled.")
          (assert-true (contains-text-p (funcall result-output-fn request-changes-result)
                                        "Plan modifications requested")
                       "Expected /plan request-changes output to acknowledge modification request, got ~S."
                       (funcall result-output-fn request-changes-result)))
        (assert-true (eq :modification-requested
                         (funcall plan-review-decision-fn plan-state))
                     "Expected /plan request-changes to keep decision :modification-requested, got ~S."
                     (funcall plan-review-decision-fn plan-state))
        (assert-true (contains-text-p (funcall plan-review-notes-fn plan-state)
                                      "dependency ordering")
                     "Expected /plan request-changes to store review notes, got ~S."
                     (funcall plan-review-notes-fn plan-state))
        (multiple-value-bind (handledp reject-result)
            (funcall dispatch-fn "/plan reject Missing rollback coverage.")
          (assert-true handledp "Expected /plan reject to be handled.")
          (assert-true (contains-text-p (funcall result-output-fn reject-result)
                                        "Plan rejected")
                       "Expected /plan reject output to acknowledge rejection, got ~S."
                       (funcall result-output-fn reject-result)))
        (assert-true (not (bool-true-p (funcall plan-review-pending-fn plan-state)))
                     "Expected /plan reject to clear review pending marker.")
        (assert-true (eq :rejected (funcall plan-review-decision-fn plan-state))
                     "Expected /plan reject to set decision :rejected, got ~S."
                     (funcall plan-review-decision-fn plan-state))
        (assert-true (contains-text-p (funcall plan-review-notes-fn plan-state)
                                      "Missing rollback coverage")
                     "Expected /plan reject to store rejection notes, got ~S."
                     (funcall plan-review-notes-fn plan-state))
        (multiple-value-bind (handledp approve-result)
            (funcall dispatch-fn "/plan approve Ready for execution.")
          (assert-true handledp "Expected /plan approve to be handled.")
          (assert-true (contains-text-p (funcall result-output-fn approve-result)
                                        "Plan approved")
                       "Expected /plan approve output to acknowledge approval, got ~S."
                       (funcall result-output-fn approve-result)))
        (assert-true (not (bool-true-p (funcall plan-review-pending-fn plan-state)))
                     "Expected /plan approve to keep review pending cleared.")
        (assert-true (eq :approved (funcall plan-review-decision-fn plan-state))
                     "Expected /plan approve to set decision :approved, got ~S."
                     (funcall plan-review-decision-fn plan-state))
        (assert-true (contains-text-p (funcall plan-review-notes-fn plan-state)
                                      "Ready for execution")
                     "Expected /plan approve to store approval notes, got ~S."
                     (funcall plan-review-notes-fn plan-state))
        (assert-true (equal '(1 2 3 4) (funcall plan-approved-steps-fn plan-state))
                     "Expected /plan approve to mark all steps approved, got ~S."
                     (funcall plan-approved-steps-fn plan-state))
        (multiple-value-bind (handledp status-result)
            (funcall dispatch-fn "/plan status")
          (assert-true handledp "Expected /plan status to be handled after approval.")
          (assert-true (contains-text-p (funcall result-output-fn status-result)
                                        "Last review decision: approved")
                       "Expected /plan status to mention the approved decision, got ~S."
                       (funcall result-output-fn status-result))
          (assert-true (contains-text-p (funcall result-output-fn status-result)
                                        "Approved steps: 4/4")
                       "Expected /plan status to report full step approval after /plan approve, got ~S."
                       (funcall result-output-fn status-result))))

      (funcall setconfig-fn :plan-mode nil)
      (funcall clear-steps-fn)
      (let ((chat-state (funcall make-chat-ui-state-fn)))
        (funcall chat-ui-set-input-fn chat-state "/plan on")
        (funcall handle-input-key-fn chat-state :enter nil)
        (funcall add-plan-step-fn
                 "Persist the drafted plan in chat history."
                 :file-paths (list "amoebum/src/commands.lisp")
                 :risk :low)
        (funcall chat-ui-set-input-fn chat-state "/plan off false")
        (funcall handle-input-key-fn chat-state :enter nil)
        (assert-true (not (bool-true-p (funcall config-value-fn :plan-mode (funcall current-config-fn))))
                     "Expected /plan off false to disable plan mode.")
        (assert-true (null (funcall plan-output-path-fn (funcall current-plan-state-fn)))
                     "Expected /plan off false to skip plan file output path capture.")
        (assert-true (bool-true-p (funcall plan-review-pending-fn (funcall current-plan-state-fn)))
                     "Expected /plan off false to keep captured review pending.")
        (assert-true (eq :pending
                         (funcall plan-review-decision-fn (funcall current-plan-state-fn)))
                     "Expected /plan off false to reset review decision to :pending.")
        (let* ((messages (funcall chat-ui-state-messages-fn chat-state))
               (captured-message
                 (loop for message in messages
                       for text = (message-text message)
                       when (and (string= (message-role message) "system")
                                 (contains-text-p text "Plan captured in conversation")
                                 (contains-text-p text "# Amoebum Plan")
                                 (contains-text-p text
                                                  "Persist the drafted plan in chat history.")
                                 (contains-text-p text
                                                  "Plan file output skipped."))
                         do (return text))))
          (assert-true captured-message
                       "Expected /plan off false system output to include captured plan markdown.")
          (assert-true (contains-text-p captured-message "```markdown")
                       "Expected captured plan output to include a fenced markdown block.")
          (let* ((styled-lines (funcall stream-markdown-styled-lines-fn
                                        captured-message
                                        120))
                 (has-code-highlighting
                   (loop for line in styled-lines
                         thereis (loop for segment in line
                                       for role = (and (listp segment)
                                                       (keywordp (first segment))
                                                       (getf segment :role))
                                       thereis (member role
                                                       '(:assistant-code
                                                         :assistant-code-keyword
                                                         :assistant-code-fence)
                                                       :test #'eq)))))
            (assert-true has-code-highlighting
                         "Expected captured plan markdown to render with code syntax highlighting roles.")))
        (multiple-value-bind (handledp review-result)
            (funcall dispatch-fn "/plan review")
          (assert-true handledp "Expected /plan review to be handled after /plan off false.")
          (assert-true (contains-text-p (funcall result-output-fn review-result)
                                        "Persist the drafted plan in chat history.")
                       "Expected /plan review to return latest captured plan from /plan off false, got ~S."
                       (funcall result-output-fn review-result))))))

  (format t "AMOEBUM_PLAN_MODE_SMOKE_OK~%"))
