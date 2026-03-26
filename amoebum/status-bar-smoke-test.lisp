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
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn-in
           (lambda (name package)
             (symbol-function (funcall symbol-in name package))))
         (make-event-bus-fn (funcall fn-in "MAKE-EVENT-BUS" amoebum-pkg))
         (publish-fn (funcall fn-in "PUBLISH" amoebum-pkg))
         (make-config-changed-event-fn (funcall fn-in "MAKE-CONFIG-CHANGED-EVENT" amoebum-pkg))
         (make-status-bar-state-fn (funcall fn-in "MAKE-STATUS-BAR-STATE" amoebum-pkg))
         (status-bar-segments-fn (funcall fn-in "STATUS-BAR-SEGMENTS" amoebum-pkg))
         (status-bar-line-fn (funcall fn-in "STATUS-BAR-LINE" amoebum-pkg))
         (publish-status-bar-stream-summary-fn
           (funcall fn-in "PUBLISH-STATUS-BAR-STREAM-SUMMARY" amoebum-pkg))
         (context-used-fn (funcall fn-in "STATUS-BAR-STATE-CONTEXT-USED-TOKENS" amoebum-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (line-contains-p (line needle)
               (and (stringp line)
                    (search needle line :test #'char-equal))))
      (let* ((bus (funcall make-event-bus-fn :capacity 64))
             (state (funcall make-status-bar-state-fn
                             :event-bus bus
                             :permission-mode :supervised
                             :model-name "moonshot-v1-128k"
                             :branch-name "feature/i37"))
             (segments (funcall status-bar-segments-fn state))
             (line (funcall status-bar-line-fn state)))
        (assert-true (= (length segments) 5)
                     "Expected five status bar segments, got ~D."
                     (length segments))
        (assert-true (string= (first segments) "branch feature/i37")
                     "Expected branch segment to render first (left segment), got ~S."
                     segments)
        (assert-true (line-contains-p line "mode supervised")
                     "Expected permission segment in status bar, got ~S."
                     line)
        (assert-true (line-contains-p line "branch feature/i37")
                     "Expected branch segment in status bar, got ~S."
                     line)
        (assert-true (line-contains-p line "Tokens: 0/128000 (0%)")
                     "Expected context usage segment in status bar, got ~S."
                     line)
        (assert-true (line-contains-p line "model moonshot-v1-128k")
                     "Expected model segment in status bar, got ~S."
                     line)
        (assert-true (line-contains-p line "stream idle")
                     "Expected streaming indicator segment in status bar, got ~S."
                     line)

        (funcall publish-fn
                 bus
                 (funcall make-config-changed-event-fn
                          :key :permission-mode
                          :old-value :supervised
                          :new-value :full-auto))
        (let ((updated-line (funcall status-bar-line-fn state)))
          (assert-true (line-contains-p updated-line "mode full-auto")
                       "Expected permission mode to update from config event, got ~S."
                       updated-line))

        (funcall publish-fn
                 bus
                 (funcall make-config-changed-event-fn
                          :key :plan-mode
                          :old-value nil
                          :new-value t))
        (let ((plan-line (funcall status-bar-line-fn state)))
          (assert-true (line-contains-p plan-line "PLAN MODE -- read-only")
                       "Expected plan-mode banner in status bar, got ~S."
                       plan-line)
          (assert-true (line-contains-p plan-line "[LOCK mutating tools blocked]")
                       "Expected plan-mode lock badge in status bar, got ~S."
                       plan-line))

        (funcall publish-fn
                 bus
                 (funcall make-config-changed-event-fn
                          :key :model
                          :old-value "moonshot-v1-128k"
                          :new-value "demo-model-32k"))
        (let ((updated-line (funcall status-bar-line-fn state)))
          (assert-true (line-contains-p updated-line "model demo-model-32k")
                       "Expected model name to update from config event, got ~S."
                       updated-line)
          (assert-true (line-contains-p updated-line "Tokens: 0/32000 (0%)")
                       "Expected context max window to follow model capacity, got ~S."
                       updated-line))

        (funcall publish-status-bar-stream-summary-fn
                 '(:status :running :activep t :tokens 512 :tokens-per-second 27.25d0)
                 :event-bus bus)
        (let ((running-line (funcall status-bar-line-fn state)))
          (assert-true (line-contains-p running-line "Tokens: 512/32000 (1%)")
                       "Expected context usage to track token consumption, got ~S."
                       running-line)
          (assert-true (line-contains-p running-line "stream 27.25 tok/s")
                       "Expected streaming indicator to show tok/s while running, got ~S."
                       running-line))

        (funcall publish-status-bar-stream-summary-fn
                 '(:status :completed :activep nil :tokens 640 :tokens-per-second 0.0d0)
                 :event-bus bus)
        (let ((completed-line (funcall status-bar-line-fn state))
              (used-tokens (funcall context-used-fn state)))
          (assert-true (line-contains-p completed-line "Tokens: 640/32000 (2%)")
                       "Expected context usage to retain final token count, got ~S."
                       completed-line)
          (assert-true (line-contains-p completed-line "stream done")
                       "Expected streaming indicator to show completion state, got ~S."
                       completed-line)
          (assert-true (= used-tokens 640)
                       "Expected context-used-tokens accessor to report 640, got ~S."
                       used-tokens))

        (funcall publish-fn
                 bus
                 (funcall make-config-changed-event-fn
                          :key :plan-mode
                          :old-value t
                          :new-value nil))
        (funcall publish-fn
                 bus
                 (funcall make-config-changed-event-fn
                          :key :status-bar-mode
                          :old-value :arch
                          :new-value :lean))
        (let ((lean-segments (funcall status-bar-segments-fn state))
              (lean-line (funcall status-bar-line-fn state)))
          (assert-true (= (length lean-segments) 3)
                       "Expected lean focus mode to reduce status bar to three segments, got ~D."
                       (length lean-segments))
          (assert-true (line-contains-p lean-line "branch feature/i37")
                       "Expected branch segment to remain in lean mode, got ~S."
                       lean-line)
          (assert-true (line-contains-p lean-line "stream done")
                       "Expected stream segment to remain in lean mode, got ~S."
                       lean-line)
          (assert-true (line-contains-p lean-line "model demo-model-32k")
                       "Expected model segment to remain in lean mode, got ~S."
                       lean-line)
          (assert-true (not (line-contains-p lean-line "mode "))
                       "Expected lean focus mode to omit permission mode segment, got ~S."
                       lean-line)
          (assert-true (not (line-contains-p lean-line "Tokens:"))
                       "Expected lean focus mode to omit context budget segment, got ~S."
                       lean-line))))

  (format t "AMOEBUM_STATUS_BAR_SMOKE_OK~%")))
