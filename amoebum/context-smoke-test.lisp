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
         (count-tokens-fn (funcall fn-in "COUNT-TOKENS" amoebum-pkg))
         (context-window-limit-for-model-fn
           (funcall fn-in "CONTEXT-WINDOW-LIMIT-FOR-MODEL" amoebum-pkg))
         (make-message-fn (funcall fn-in "MAKE-MESSAGE" pseudopod-pkg))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (current-config-fn (funcall fn-in "CURRENT-CONFIG" amoebum-pkg))
         (config-value-fn (funcall fn-in "CONFIG-VALUE" amoebum-pkg))
         (make-chat-ui-state-fn (funcall fn-in "MAKE-CHAT-UI-STATE" amoebum-pkg))
         (chat-ui-add-message-fn (funcall fn-in "CHAT-UI-ADD-MESSAGE" amoebum-pkg))
         (chat-context-used-fn (funcall fn-in "CHAT-UI-STATE-CONTEXT-USED-TOKENS" amoebum-pkg))
         (chat-context-limit-fn (funcall fn-in "CHAT-UI-STATE-CONTEXT-WINDOW-LIMIT" amoebum-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args)))))
      (assert-true (= (funcall count-tokens-fn "abcdefghijklmnop") 4)
                   "Expected 16 chars to estimate as 4 tokens.")
      (assert-true (= (funcall count-tokens-fn "abcd") 1)
                   "Expected 4 chars to estimate as 1 token.")
      (assert-true (= (funcall count-tokens-fn "abcde") 2)
                   "Expected ceil(5/4) token rounding.")

      (let* ((user-message (funcall make-message-fn :role "user" :content "abcdefgh"))
             (assistant-message (funcall make-message-fn :role "assistant" :content "abcd"))
             (tokens (funcall count-tokens-fn (list user-message assistant-message))))
        (assert-true (= tokens 3)
                     "Expected message list token estimate 3, got ~S."
                     tokens))

      (assert-true (= (funcall context-window-limit-for-model-fn "moonshot-v1-32k") 32000)
                   "Expected moonshot-v1-32k context window lookup to be 32000.")
      (assert-true (= (funcall context-window-limit-for-model-fn "moonshot-v1-128k") 128000)
                   "Expected moonshot-v1-128k context window lookup to be 128000.")
      (assert-true (= (funcall context-window-limit-for-model-fn "demo-model-8k") 8000)
                   "Expected numeric model suffix inference for demo-model-8k.")

      (let* ((config (funcall current-config-fn))
             (context-limit (funcall config-value-fn :context-window-limit config)))
        (assert-true (or (null context-limit)
                         (and (integerp context-limit) (> context-limit 0)))
                     "Expected :context-window-limit config value to be NIL or positive integer, got ~S."
                     context-limit))

      (let* ((old-limit (funcall config-value-fn
                                 :context-window-limit
                                 (funcall current-config-fn))))
        (unwind-protect
            (progn
              (funcall setconfig-fn :context-window-limit 4096)
              (let ((state (funcall make-chat-ui-state-fn :stream-runner nil)))
                (funcall chat-ui-add-message-fn state :user "abcdefgh")
                (funcall chat-ui-add-message-fn state :assistant "abcd")
                (assert-true (= (funcall chat-context-used-fn state) 3)
                             "Expected chat state context-used-tokens to track cumulative conversation tokens.")
                (assert-true (= (funcall chat-context-limit-fn state) 4096)
                             "Expected chat state context-window-limit to honor config override.")))
          (funcall setconfig-fn :context-window-limit old-limit)))))

  (format t "AMOEBUM_CONTEXT_SMOKE_OK~%"))
