
(progn
 (defun %skill-handler-r013-golden-skill-multi-typed (invocation arguments context)
   (declare (ignorable invocation context))
   (let ((missing
          (%skill-missing-required-arguments
           '((:name #A((9) base-char . "keep-last") :type :integer :required-p nil :default 6
              :default-supplied-p t :prompt nil :choices nil :greedy-p nil :description
              #A((23) base-char . "How many turns to keep.") :completer nil)
             (:name #A((4) base-char . "mode") :type :keyword :required-p nil :default :fast
              :default-supplied-p t :prompt nil :choices (:fast :slow) :greedy-p nil :description
              #A((5) base-char . "Mode.") :completer nil)
             (:name #A((5) base-char . "label") :type :string :required-p t :default nil
              :default-supplied-p nil :prompt #A((6) base-char . "Label:") :choices nil :greedy-p
              nil :description nil :completer nil))
           arguments)))
     (when missing
       (return-from %skill-handler-r013-golden-skill-multi-typed
         (make-slash-command-result :echo-input-p t :output
                                    (%skill-missing-arguments-output
                                     'common-lisp-user::r013-golden-skill-multi-typed
                                     "/r013-golden-skill-multi-typed <label> [keep-last] [mode]"
                                     missing)))))
   (let* ((common-lisp-user::keep-last
           (%skill-argument-value arguments #A((9) base-char . "keep-last") '6 t))
          (common-lisp-user::mode
           (%skill-argument-value arguments #A((4) base-char . "mode") ':fast t))
          (common-lisp-user::label
           (%skill-argument-value arguments #A((5) base-char . "label") 'nil nil)))
     (declare
      (ignorable common-lisp-user::keep-last common-lisp-user::mode common-lisp-user::label))
     (common-lisp-user::make-slash-command-result :echo-input-p t :output
      (format nil "~A keep=~D mode=~A" common-lisp-user::label common-lisp-user::keep-last
              common-lisp-user::mode))))
 (defun %skill-completer-r013-golden-skill-multi-typed
        (command invocation index fragment prefix-tokens)
   (declare (ignore command invocation))
   (or (and nil (funcall nil index fragment prefix-tokens))
       (%skill-default-completer
        '((:name #A((9) base-char . "keep-last") :type :integer :required-p nil :default 6
           :default-supplied-p t :prompt nil :choices nil :greedy-p nil :description
           #A((23) base-char . "How many turns to keep.") :completer nil)
          (:name #A((4) base-char . "mode") :type :keyword :required-p nil :default :fast
           :default-supplied-p t :prompt nil :choices (:fast :slow) :greedy-p nil :description
           #A((5) base-char . "Mode.") :completer nil)
          (:name #A((5) base-char . "label") :type :string :required-p t :default nil
           :default-supplied-p nil :prompt #A((6) base-char . "Label:") :choices nil :greedy-p nil
           :description nil :completer nil))
        index fragment prefix-tokens)))
 (register-skill
  (make-skill-metadata :name #A((29) base-char . "r013-golden-skill-multi-typed") :description
                       "Multi-typed skill." :usage
                       "/r013-golden-skill-multi-typed <label> [keep-last] [mode]" :aliases
                       '("r013-mt-skill") :category :session :keybinding "C-c m" :arguments
                       (list
                        (make-skill-argument :name #A((9) base-char . "keep-last") :variable
                                             'common-lisp-user::keep-last :type :integer
                                             :required-p nil :default '6 :default-supplied-p t
                                             :choices 'nil :greedy-p nil :prompt nil :description
                                             #A((23) base-char . "How many turns to keep.")
                                             :completer nil)
                        (make-skill-argument :name #A((4) base-char . "mode") :variable
                                             'common-lisp-user::mode :type :keyword :required-p nil
                                             :default ':fast :default-supplied-p t :choices
                                             '(:fast :slow) :greedy-p nil :prompt nil :description
                                             #A((5) base-char . "Mode.") :completer nil)
                        (make-skill-argument :name #A((5) base-char . "label") :variable
                                             'common-lisp-user::label :type :string :required-p t
                                             :default 'nil :default-supplied-p nil :choices 'nil
                                             :greedy-p nil :prompt #A((6) base-char . "Label:")
                                             :description nil :completer nil))
                       :handler #'%skill-handler-r013-golden-skill-multi-typed :completer
                       #'%skill-completer-r013-golden-skill-multi-typed :source-file #P"/dev/stdin"
                       :source-line nil :defined-at (%skill-now-ms))))