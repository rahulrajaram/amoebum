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
         (current-keymap-fn (funcall fn-in "CURRENT-KEYMAP" amoebum-pkg))
         (dispatch-active-keymaps-fn (funcall fn-in "DISPATCH-ACTIVE-KEYMAPS" amoebum-pkg))
         (make-key-dispatch-on-event-fn (funcall fn-in "MAKE-KEY-DISPATCH-ON-EVENT" amoebum-pkg))
         (activate-default-keymaps-fn (funcall fn-in "ACTIVATE-DEFAULT-KEYMAPS" amoebum-pkg))
         (keymap-name-fn (funcall fn-in "KEYMAP-NAME" amoebum-pkg))
         (keymap-bindings-fn (funcall fn-in "KEYMAP-BINDINGS" amoebum-pkg))
         (make-key-event-fn (funcall fn-in "MAKE-KEY-EVENT" events-pkg))
         (make-paste-event-fn (funcall fn-in "MAKE-PASTE-EVENT" events-pkg))
         (counter-sym (intern "*I34-CHORD-COUNT*" amoebum-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (make-text-event (text)
               (funcall make-key-event-fn :text :text? text))
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
