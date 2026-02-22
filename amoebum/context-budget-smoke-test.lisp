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
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn-in
           (lambda (name package)
             (symbol-function (funcall symbol-in name package))))
         (setf-fn
           (lambda (name package)
             (fdefinition (list 'setf (funcall symbol-in name package)))))
         (make-event-bus-fn (funcall fn-in "MAKE-EVENT-BUS" amoebum-pkg))
         (subscribe-fn (funcall fn-in "SUBSCRIBE" amoebum-pkg))
         (event-payload-fn (funcall fn-in "EVENT-PAYLOAD" amoebum-pkg))
         (make-status-bar-state-fn (funcall fn-in "MAKE-STATUS-BAR-STATE" amoebum-pkg))
         (status-bar-line-fn (funcall fn-in "STATUS-BAR-LINE" amoebum-pkg))
         (status-bar-styled-segments-fn (funcall fn-in "STATUS-BAR-STYLED-SEGMENTS" amoebum-pkg))
         (set-status-used-fn (funcall setf-fn "STATUS-BAR-STATE-CONTEXT-USED-TOKENS" amoebum-pkg))
         (set-status-max-fn (funcall setf-fn "STATUS-BAR-STATE-CONTEXT-MAX-TOKENS" amoebum-pkg))
         (make-chat-ui-state-fn (funcall fn-in "MAKE-CHAT-UI-STATE" amoebum-pkg))
         (chat-ui-add-message-fn (funcall fn-in "CHAT-UI-ADD-MESSAGE" amoebum-pkg))
         (chat-ui-messages-fn (funcall fn-in "CHAT-UI-STATE-MESSAGES" amoebum-pkg))
         (handle-slash-input-fn (funcall fn-in "%HANDLE-SLASH-COMMAND-INPUT" amoebum-pkg))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (current-config-fn (funcall fn-in "CURRENT-CONFIG" amoebum-pkg))
         (config-value-fn (funcall fn-in "CONFIG-VALUE" amoebum-pkg))
         (context-event-type
           (symbol-value (funcall symbol-in "+EVENT-TYPE-CONTEXT-COMPRESSED+" amoebum-pkg)))
         (context-before-fn
           (funcall fn-in "CONTEXT-COMPRESSED-PAYLOAD-BEFORE-TOKENS" amoebum-pkg))
         (context-after-fn
           (funcall fn-in "CONTEXT-COMPRESSED-PAYLOAD-AFTER-TOKENS" amoebum-pkg))
         (context-saved-fn
           (funcall fn-in "CONTEXT-COMPRESSED-PAYLOAD-SAVED-TOKENS" amoebum-pkg))
         (context-summarized-fn
           (funcall fn-in "CONTEXT-COMPRESSED-PAYLOAD-SUMMARIZED-MESSAGES" amoebum-pkg))
         (context-trigger-fn
           (funcall fn-in "CONTEXT-COMPRESSED-PAYLOAD-TRIGGER" amoebum-pkg))
         (message-content-fn (funcall fn-in "MESSAGE-CONTENT" pseudopod-pkg))
         (content-part-text-fn (funcall fn-in "CONTENT-PART-TEXT" pseudopod-pkg))
         (event-bus-symbol (funcall symbol-in "*EVENT-BUS*" amoebum-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (line-contains-p (line needle)
               (and (stringp line)
                    (search needle line :test #'char-equal)))
             (context-role (segments)
               (loop for segment in segments
                     when (and (consp segment)
                               (stringp (car segment))
                               (search "Tokens:" (car segment) :test #'char-equal))
                       do (return (cdr segment))
                     finally (return nil)))
             (message-text (message)
               (with-output-to-string (out)
                 (loop for part in (funcall message-content-fn message)
                       for index from 0 do
                         (when (> index 0)
                           (write-char #\Newline out))
                         (write-string (or (funcall content-part-text-fn part) "")
                                       out)))))
      (let ((state (funcall make-status-bar-state-fn
                            :event-bus (funcall make-event-bus-fn :capacity 16)
                            :model-name "moonshot-v1-8k"
                            :branch-name "feature/i44")))
        (funcall set-status-max-fn 4000 state)

        (funcall set-status-used-fn 1200 state)
        (assert-true (line-contains-p (funcall status-bar-line-fn state)
                                      "Tokens: 1200/4000 (30%)")
                     "Expected green token usage format, got ~S."
                     (funcall status-bar-line-fn state))
        (assert-true (eq (context-role (funcall status-bar-styled-segments-fn state))
                         :context-green)
                     "Expected green token role for low usage.")

        (funcall set-status-used-fn 2000 state)
        (assert-true (line-contains-p (funcall status-bar-line-fn state)
                                      "Tokens: 2000/4000 (50%)")
                     "Expected yellow token usage format at 50%, got ~S."
                     (funcall status-bar-line-fn state))
        (assert-true (eq (context-role (funcall status-bar-styled-segments-fn state))
                         :context-yellow)
                     "Expected yellow token role at threshold usage.")

        (funcall set-status-used-fn 3601 state)
        (assert-true (line-contains-p (funcall status-bar-line-fn state)
                                      "Tokens: 3601/4000 (90%)")
                     "Expected red token usage format above threshold, got ~S."
                     (funcall status-bar-line-fn state))
        (assert-true (eq (context-role (funcall status-bar-styled-segments-fn state))
                         :context-red)
                     "Expected red token role for high usage."))

      (let* ((old-event-bus (symbol-value event-bus-symbol))
             (old-context-limit
               (funcall config-value-fn :context-window-limit (funcall current-config-fn)))
             (bus (funcall make-event-bus-fn :capacity 64))
             (captured-events '()))
        (unwind-protect
            (progn
              (setf (symbol-value event-bus-symbol) bus)
              (funcall setconfig-fn :context-window-limit 50000)
              (funcall subscribe-fn
                       bus
                       context-event-type
                       (lambda (event)
                         (push event captured-events)))
              (let ((chat-state (funcall make-chat-ui-state-fn :stream-runner nil)))
                (loop for idx from 1 to 24 do
                  (funcall chat-ui-add-message-fn
                           chat-state
                           (if (oddp idx) :user :assistant)
                           (format nil
                                   "message-~D: demonstrate compact context behavior with repeated details and verification traces."
                                   idx)))
                (assert-true (funcall handle-slash-input-fn chat-state "/compact 2")
                             "Expected /compact to be handled by chat UI command dispatcher.")
                (let* ((messages (funcall chat-ui-messages-fn chat-state))
                       (last-message (car (last messages)))
                       (last-text (and last-message (message-text last-message)))
                       (event (car captured-events))
                       (payload (and event (funcall event-payload-fn event)))
                       (before (and payload (funcall context-before-fn payload)))
                       (after (and payload (funcall context-after-fn payload)))
                       (saved (and payload (funcall context-saved-fn payload)))
                       (summarized (and payload (funcall context-summarized-fn payload)))
                       (trigger (and payload (funcall context-trigger-fn payload))))
                  (assert-true (line-contains-p (or last-text "") "Compacted context:")
                               "Expected /compact to report savings in system output, got ~S."
                               last-text)
                  (assert-true payload
                               "Expected context compression event payload from /compact.")
                  (assert-true (and (integerp before) (integerp after) (> before after))
                               "Expected context compression event to reduce tokens, before=~S after=~S."
                               before
                               after)
                  (assert-true (= saved (- before after))
                               "Expected saved tokens to equal before-after, got saved=~S before=~S after=~S."
                               saved
                               before
                               after)
                  (assert-true (and (integerp summarized) (> summarized 0))
                               "Expected summarized message count > 0, got ~S."
                               summarized)
                  (assert-true (eq trigger :manual)
                               "Expected manual trigger for /compact event, got ~S."
                               trigger))))
          (setf (symbol-value event-bus-symbol) old-event-bus)
          (funcall setconfig-fn :context-window-limit old-context-limit)))))

  (format t "AMOEBUM_CONTEXT_BUDGET_SMOKE_OK~%"))
