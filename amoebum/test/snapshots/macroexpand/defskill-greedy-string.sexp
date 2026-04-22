
(progn
 (defun %skill-handler-r013-golden-skill-greedy-string (invocation arguments context)
   (declare (ignorable invocation context))
   (let ((missing
          (%skill-missing-required-arguments
           '((:name #A((5) base-char . "files") :type :string :required-p nil :default nil
              :default-supplied-p nil :prompt nil :choices nil :greedy-p t :description
              #A((29) base-char . "Optional explicit file paths.") :completer nil))
           arguments)))
     (when missing
       (return-from %skill-handler-r013-golden-skill-greedy-string
         (make-slash-command-result :echo-input-p t :output
                                    (%skill-missing-arguments-output
                                     'common-lisp-user::r013-golden-skill-greedy-string
                                     "/r013-golden-skill-greedy-string [files...]" missing)))))
   (let* ((common-lisp-user::files
           (%skill-argument-value arguments #A((5) base-char . "files") 'nil nil)))
     (declare (ignorable common-lisp-user::files))
     (common-lisp-user::make-slash-command-result :echo-input-p t :output
      (format nil "~S" common-lisp-user::files))))
 (defun %skill-completer-r013-golden-skill-greedy-string
        (command invocation index fragment prefix-tokens)
   (declare (ignore command invocation))
   (or (and nil (funcall nil index fragment prefix-tokens))
       (%skill-default-completer
        '((:name #A((5) base-char . "files") :type :string :required-p nil :default nil
           :default-supplied-p nil :prompt nil :choices nil :greedy-p t :description
           #A((29) base-char . "Optional explicit file paths.") :completer nil))
        index fragment prefix-tokens)))
 (register-skill
  (make-skill-metadata :name #A((31) base-char . "r013-golden-skill-greedy-string") :description
                       "Greedy string skill." :usage "/r013-golden-skill-greedy-string [files...]"
                       :aliases 'nil :category :git :keybinding nil :arguments
                       (list
                        (make-skill-argument :name #A((5) base-char . "files") :variable
                                             'common-lisp-user::files :type :string :required-p nil
                                             :default 'nil :default-supplied-p nil :choices 'nil
                                             :greedy-p t :prompt nil :description
                                             #A((29) base-char . "Optional explicit file paths.")
                                             :completer nil))
                       :handler #'%skill-handler-r013-golden-skill-greedy-string :completer
                       #'%skill-completer-r013-golden-skill-greedy-string :source-file
                       #P"/dev/stdin" :source-line nil :defined-at (%skill-now-ms))))