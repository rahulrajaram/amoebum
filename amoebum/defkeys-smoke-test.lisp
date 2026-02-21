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
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn-in
           (lambda (name package)
             (symbol-function (funcall symbol-in name package))))
         (defkeys-sym (funcall symbol-in "DEFKEYS" amoebum-pkg))
         (define-chord-fn (funcall fn-in "DEFINE-CHORD" amoebum-pkg))
         (find-keymap-fn (funcall fn-in "FIND-KEYMAP" amoebum-pkg))
         (reset-keymap-stack-fn (funcall fn-in "RESET-KEYMAP-STACK" amoebum-pkg))
         (push-keymap-fn (funcall fn-in "PUSH-KEYMAP" amoebum-pkg))
         (pop-keymap-fn (funcall fn-in "POP-KEYMAP" amoebum-pkg))
         (push-keymap-overlay-fn (funcall fn-in "PUSH-KEYMAP-OVERLAY" amoebum-pkg))
         (pop-keymap-overlay-fn (funcall fn-in "POP-KEYMAP-OVERLAY" amoebum-pkg))
         (clear-keymap-overlays-fn (funcall fn-in "CLEAR-KEYMAP-OVERLAYS" amoebum-pkg))
         (current-keymap-overlay-fn (funcall fn-in "CURRENT-KEYMAP-OVERLAY" amoebum-pkg))
         (current-keymap-fn (funcall fn-in "CURRENT-KEYMAP" amoebum-pkg))
         (dispatch-active-keymaps-fn (funcall fn-in "DISPATCH-ACTIVE-KEYMAPS" amoebum-pkg))
         (flush-key-dispatch-timeouts-fn (funcall fn-in "FLUSH-KEY-DISPATCH-TIMEOUTS" amoebum-pkg))
         (make-key-dispatch-on-event-fn (funcall fn-in "MAKE-KEY-DISPATCH-ON-EVENT" amoebum-pkg))
         (activate-default-keymaps-fn (funcall fn-in "ACTIVATE-DEFAULT-KEYMAPS" amoebum-pkg))
         (keymap-name-fn (funcall fn-in "KEYMAP-NAME" amoebum-pkg))
         (keymap-bindings-fn (funcall fn-in "KEYMAP-BINDINGS" amoebum-pkg))
         (make-event-bus-fn (funcall fn-in "MAKE-EVENT-BUS" amoebum-pkg))
         (current-event-bus-fn (funcall fn-in "CURRENT-EVENT-BUS" amoebum-pkg))
         (event-history-fn (funcall fn-in "EVENT-HISTORY" amoebum-pkg))
         (event-type-fn (funcall fn-in "EVENT-TYPE" amoebum-pkg))
         (make-key-event-fn (funcall fn-in "MAKE-KEY-EVENT" events-pkg))
         (make-paste-event-fn (funcall fn-in "MAKE-PASTE-EVENT" events-pkg))
         (event-bus-var-sym (funcall symbol-in "*EVENT-BUS*" amoebum-pkg))
         (sequence-timeout-sym (funcall symbol-in "*KEY-SEQUENCE-TIMEOUT-MS*" amoebum-pkg))
         (escape-timeout-sym (funcall symbol-in "*KEY-DISAMBIGUATION-TIMEOUT-MS*" amoebum-pkg))
         (terminal-profile-sym (funcall symbol-in "*TERMINAL-KEY-NORMALIZATION-PROFILE*" amoebum-pkg))
         (counter-sym (intern "*I34-CHORD-COUNT*" amoebum-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (make-text-event (text)
               (funcall make-key-event-fn :text :text? text))
             (make-escape-event ()
               (funcall make-key-event-fn :escape))
             (make-ctrl-c-event ()
               (funcall make-key-event-fn :ctrl-c :ctrlp t)))
      (setf (symbol-value counter-sym) 0)

      (assert-true (funcall find-keymap-fn 'chat-mode)
                   "Expected CHAT-MODE default keymap to be registered.")
      (assert-true (funcall find-keymap-fn 'command-mode)
                   "Expected COMMAND-MODE default keymap to be registered.")
      (assert-true (funcall find-keymap-fn 'diff-mode)
                   "Expected DIFF-MODE default keymap to be registered.")

      (eval
       `(,defkeys-sym i34-chat-mode
          "I34 smoke chat keymap."
          ("a" (+ state 1) :description "Increment state")
          ("C-c" state :when (> state 0) :description "Guarded ctrl-c")))

      (eval
       `(,defkeys-sym i34-command-mode
          "I34 smoke command keymap."
          ("b" (+ state 10))))

      (eval
       `(,defkeys-sym i34-diff-mode
          "I34 smoke diff keymap."
          ("d" (+ state 100))))

      (eval
       `(,defkeys-sym i66-esc-alt-mode
          "I66 esc/alt disambiguation keymap."
          ("ESC" (+ state 1000))
          ("M-x" (+ state 2000))
          ("up" (+ state 77))
          ("C-x" (+ state 5))))

      (eval
       `(,defkeys-sym i66-overlay-mode
          "I66 temporary overlay keymap."
          ("z" (+ state 900))))

      (let ((chat-keymap (funcall find-keymap-fn 'i34-chat-mode)))
        (assert-true chat-keymap
                     "Expected DEFKEYS to register i34-chat-mode keymap.")
        (assert-true (> (hash-table-count (funcall keymap-bindings-fn chat-keymap)) 0)
                     "Expected keymap bindings table to contain entries."))

      (funcall reset-keymap-stack-fn '(i34-chat-mode))
      (funcall push-keymap-fn 'i34-command-mode)
      (funcall push-keymap-fn 'i34-diff-mode)
      (assert-true (eq (funcall keymap-name-fn (funcall current-keymap-fn))
                       'i34-diff-mode)
                   "Expected top of keymap stack to be i34-diff-mode.")
      (funcall pop-keymap-fn)
      (assert-true (eq (funcall keymap-name-fn (funcall current-keymap-fn))
                       'i34-command-mode)
                   "Expected pop-keymap to reveal i34-command-mode.")
      (funcall pop-keymap-fn)
      (assert-true (eq (funcall keymap-name-fn (funcall current-keymap-fn))
                       'i34-chat-mode)
                   "Expected pop-keymap to reveal i34-chat-mode.")

      (funcall define-chord-fn
               'i34-chat-mode
               '("g" "g")
               (lambda (state key-event)
                 (declare (ignore key-event))
                 (incf (symbol-value counter-sym))
                 (+ state 50))
               :description "Double-g chord")

      (funcall reset-keymap-stack-fn '(i34-chat-mode))

      (multiple-value-bind (state handledp metadata)
          (funcall dispatch-active-keymaps-fn (make-text-event "a") 1)
        (assert-true handledp
                     "Expected single-key binding dispatch to handle text key.")
        (assert-true (= state 2)
                     "Expected single-key handler to update state to 2, got ~S." state)
        (assert-true (eq (getf metadata :kind) :binding)
                     "Expected dispatch metadata kind :binding, got ~S." metadata))

      (multiple-value-bind (state handledp metadata)
          (funcall dispatch-active-keymaps-fn (make-text-event "g") 3)
        (assert-true handledp
                     "Expected first chord stroke to be consumed as pending.")
        (assert-true (= state 3)
                     "Expected pending chord stroke to preserve state.")
        (assert-true (eq (getf metadata :kind) :pending)
                     "Expected pending chord metadata, got ~S." metadata))

      (multiple-value-bind (state handledp metadata)
          (funcall dispatch-active-keymaps-fn (make-text-event "g") 3)
        (assert-true handledp
                     "Expected second chord stroke to dispatch chord handler.")
        (assert-true (= state 53)
                     "Expected chord handler to update state to 53, got ~S." state)
        (assert-true (eq (getf metadata :kind) :chord)
                     "Expected chord dispatch metadata kind :chord, got ~S." metadata))

      (assert-true (= (symbol-value counter-sym) 1)
                   "Expected chord handler counter to increment exactly once.")

      (let ((previous-timeout (symbol-value sequence-timeout-sym)))
        (unwind-protect
             (progn
               (setf (symbol-value sequence-timeout-sym) 30)
               (funcall reset-keymap-stack-fn '(i34-chat-mode))
               (multiple-value-bind (state handledp metadata)
                   (funcall dispatch-active-keymaps-fn (make-text-event "g") 11)
                 (assert-true handledp
                              "Expected first g stroke to become pending.")
                 (assert-true (= state 11)
                              "Expected pending first stroke to preserve state.")
                 (assert-true (eq (getf metadata :kind) :pending)
                              "Expected pending metadata for first stroke."))
               (sleep 0.06d0)
               (multiple-value-bind (state handledp metadata)
                   (funcall dispatch-active-keymaps-fn (make-text-event "g") 11)
                 (assert-true handledp
                              "Expected expired sequence to treat second g as fresh pending stroke.")
                 (assert-true (= state 11)
                              "Expected timed-out sequence to avoid chord dispatch.")
                 (assert-true (eq (getf metadata :kind) :pending)
                              "Expected second stroke after timeout to remain pending, got ~S." metadata))
               (assert-true (= (symbol-value counter-sym) 1)
                            "Expected chord counter to remain unchanged after sequence timeout."))
          (setf (symbol-value sequence-timeout-sym) previous-timeout)))

      (multiple-value-bind (state handledp)
          (funcall dispatch-active-keymaps-fn (make-ctrl-c-event) 7)
        (assert-true handledp
                     "Expected guarded ctrl-c binding to handle positive state.")
        (assert-true (= state 7)
                     "Expected ctrl-c binding to return unchanged state."))

      (multiple-value-bind (state handledp)
          (funcall dispatch-active-keymaps-fn (make-ctrl-c-event) -2)
        (assert-true (not handledp)
                     "Expected :when guard to skip ctrl-c binding for negative state.")
        (assert-true (= state -2)
                     "Expected skipped binding to preserve state."))

      (let ((previous-disambiguation-timeout (symbol-value escape-timeout-sym)))
        (unwind-protect
             (progn
               (setf (symbol-value escape-timeout-sym) 50)
               (funcall reset-keymap-stack-fn '(i66-esc-alt-mode))

               (multiple-value-bind (state handledp metadata)
                   (funcall dispatch-active-keymaps-fn (make-escape-event) 5)
                 (assert-true handledp
                              "Expected ESC to be held pending for disambiguation.")
                 (assert-true (= state 5)
                              "Expected pending ESC to not mutate state yet.")
                 (assert-true (eq (getf metadata :kind) :pending-escape)
                              "Expected pending escape metadata kind, got ~S." metadata))

               (multiple-value-bind (state handledp metadata)
                   (funcall dispatch-active-keymaps-fn (make-text-event "x") 5)
                 (assert-true handledp
                              "Expected ESC + x within timeout to dispatch M-x.")
                 (assert-true (= state 2005)
                              "Expected M-x handler to update state to 2005, got ~S." state)
                 (assert-true (eq (getf metadata :kind) :binding)
                              "Expected ESC+Alt disambiguation to dispatch binding, got ~S." metadata))

               (multiple-value-bind (state handledp)
                   (funcall dispatch-active-keymaps-fn (make-escape-event) 9)
                 (assert-true handledp
                              "Expected lone ESC to become pending.")
                 (assert-true (= state 9)
                              "Expected pending lone ESC to preserve state."))
               (sleep 0.06d0)
               (multiple-value-bind (state handledp metadata)
                   (funcall flush-key-dispatch-timeouts-fn 9)
                 (assert-true handledp
                              "Expected timeout flush to dispatch lone ESC binding.")
                 (assert-true (= state 1009)
                              "Expected lone ESC binding after timeout, got ~S." state)
                 (assert-true (eq (getf metadata :kind) :binding)
                              "Expected lone ESC timeout flush to dispatch binding, got ~S." metadata)))
          (setf (symbol-value escape-timeout-sym) previous-disambiguation-timeout)))

      (let ((previous-profile (symbol-value terminal-profile-sym)))
        (unwind-protect
             (progn
               (funcall reset-keymap-stack-fn '(i66-esc-alt-mode))
               (setf (symbol-value terminal-profile-sym) :kitty)
               (multiple-value-bind (state handledp metadata)
                   (funcall dispatch-active-keymaps-fn (funcall make-key-event-fn "kitty-up") 4)
                 (assert-true handledp
                              "Expected kitty-up normalization to map into up binding.")
                 (assert-true (= state 81)
                              "Expected normalized kitty-up to update state to 81, got ~S." state)
                 (assert-true (eq (getf metadata :kind) :binding)
                              "Expected normalized keycode to dispatch binding, got ~S." metadata))

               (setf (symbol-value terminal-profile-sym) :tmux)
               (multiple-value-bind (state handledp metadata)
                   (funcall dispatch-active-keymaps-fn
                            (funcall make-key-event-fn "tmux:C-b C-x")
                            10)
                 (assert-true handledp
                              "Expected tmux-prefixed key to strip prefix and dispatch C-x binding.")
                 (assert-true (= state 15)
                              "Expected tmux normalized C-x to update state to 15, got ~S." state)
                 (assert-true (eq (getf metadata :kind) :binding)
                              "Expected tmux normalized keycode to dispatch binding, got ~S." metadata)))
          (setf (symbol-value terminal-profile-sym) previous-profile)))

      (setf (symbol-value event-bus-var-sym) (funcall make-event-bus-fn))
      (funcall reset-keymap-stack-fn '(i34-chat-mode))
      (funcall clear-keymap-overlays-fn)
      (funcall push-keymap-overlay-fn 'i66-overlay-mode)

      (multiple-value-bind (state handledp metadata)
          (funcall dispatch-active-keymaps-fn (make-text-event "z") 2)
        (assert-true handledp
                     "Expected overlay to preempt base keymaps.")
        (assert-true (= state 902)
                     "Expected overlay binding to update state to 902, got ~S." state)
        (assert-true (eq (getf metadata :kind) :binding)
                     "Expected overlay z binding dispatch metadata kind :binding."))

      (multiple-value-bind (state handledp metadata)
          (funcall dispatch-active-keymaps-fn (make-escape-event) 902)
        (assert-true handledp
                     "Expected ESC inside overlay to become pending for overlay cleanup.")
        (assert-true (= state 902)
                     "Expected pending ESC in overlay to preserve state.")
        (assert-true (eq (getf metadata :kind) :pending-escape)
                     "Expected pending escape metadata for overlay escape path, got ~S." metadata))

      (sleep 0.06d0)
      (multiple-value-bind (state handledp metadata)
          (funcall flush-key-dispatch-timeouts-fn 902)
        (assert-true handledp
                     "Expected timeout flush to auto-pop overlay on ESC.")
        (assert-true (= state 902)
                     "Expected overlay escape pop to preserve state.")
        (assert-true (eq (getf metadata :kind) :overlay-exit)
                     "Expected overlay escape cleanup metadata kind :overlay-exit, got ~S." metadata))

      (assert-true (null (funcall current-keymap-overlay-fn))
                   "Expected overlay stack to be empty after ESC auto-cleanup.")

      (let ((events (funcall event-history-fn (funcall current-event-bus-fn))))
        (assert-true (find :KEYMAP-OVERLAY-ENTER events :key event-type-fn :test #'eq)
                     "Expected keymap-overlay-enter event in event bus history.")
        (assert-true (find :KEYMAP-OVERLAY-EXIT events :key event-type-fn :test #'eq)
                     "Expected keymap-overlay-exit event in event bus history."))

      (funcall pop-keymap-overlay-fn)

      (let* ((fallback-called nil)
             (dispatcher
               (funcall make-key-dispatch-on-event-fn
                        :fallback (lambda (state event)
                                    (declare (ignore event))
                                    (setf fallback-called t)
                                    (+ state 1000)))))
        (let ((key-result (funcall dispatcher 5 (make-text-event "a"))))
          (assert-true (= key-result 6)
                       "Expected PTUI key dispatcher wrapper to use key binding path.")
          (assert-true (not fallback-called)
                       "Expected fallback to not run for handled key event."))
        (let ((paste-result (funcall dispatcher 5 (funcall make-paste-event-fn "paste"))))
          (assert-true (= paste-result 1005)
                       "Expected non-key events to flow through fallback.")
          (assert-true fallback-called
                       "Expected fallback to run for non-key events.")))

      (funcall activate-default-keymaps-fn)))

  (format t "AMOEBUM_DEFKEYS_SMOKE_OK~%"))
