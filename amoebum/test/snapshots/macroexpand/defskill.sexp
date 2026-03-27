
(progn
 (defun %skill-handler-r013-golden-skill (invocation arguments context)
   (declare (ignorable invocation context))
   (let ((missing
          (%skill-missing-required-arguments
           '((:name #A((5) base-char . "count") :type :integer :required-p t :default nil
              :default-supplied-p nil :prompt #A((6) base-char . "Count:") :choices (1 2 3)
              :greedy-p nil :description nil :completer nil)
             (:name #A((4) base-char . "mode") :type :keyword :required-p nil :default :fast
              :default-supplied-p t :prompt nil :choices (:fast :slow) :greedy-p nil :description
              #A((4) base-char . "Mode") :completer nil))
           arguments)))
     (when missing
       (return-from %skill-handler-r013-golden-skill
         (make-slash-command-result :echo-input-p t :output
                                    (%skill-missing-arguments-output
                                     'common-lisp-user::r013-golden-skill
                                     "/r013-golden-skill <count> [<mode>]" missing)))))
   (let* ((count (%skill-argument-value arguments #A((5) base-char . "count") 'nil nil))
          (common-lisp-user::mode
           (%skill-argument-value arguments #A((4) base-char . "mode") ':fast t)))
     (declare (ignorable count common-lisp-user::mode))
     (format nil "count=~D mode=~A" count common-lisp-user::mode)))
 (defun %skill-completer-r013-golden-skill (command invocation index fragment prefix-tokens)
   (declare (ignore command invocation))
   (or (and nil (funcall nil index fragment prefix-tokens))
       (%skill-default-completer
        '((:name #A((5) base-char . "count") :type :integer :required-p t :default nil
           :default-supplied-p nil :prompt #A((6) base-char . "Count:") :choices (1 2 3) :greedy-p
           nil :description nil :completer nil)
          (:name #A((4) base-char . "mode") :type :keyword :required-p nil :default :fast
           :default-supplied-p t :prompt nil :choices (:fast :slow) :greedy-p nil :description
           #A((4) base-char . "Mode") :completer nil))
        index fragment prefix-tokens)))
 (register-skill
  (make-skill-metadata :name #A((17) base-char . "r013-golden-skill") :description
                       "Golden macroexpand skill" :usage "/r013-golden-skill <count> [<mode>]"
                       :aliases 'nil :category :smoke :keybinding nil :arguments
                       (list
                        (make-skill-argument :name #A((5) base-char . "count") :variable 'count
                                             :type :integer :required-p t :default 'nil
                                             :default-supplied-p nil :choices '(1 2 3) :greedy-p
                                             nil :prompt #A((6) base-char . "Count:") :description
                                             nil :completer nil)
                        (make-skill-argument :name #A((4) base-char . "mode") :variable
                                             'common-lisp-user::mode :type :keyword :required-p nil
                                             :default ':fast :default-supplied-p t :choices
                                             '(:fast :slow) :greedy-p nil :prompt nil :description
                                             #A((4) base-char . "Mode") :completer nil))
                       :handler #'%skill-handler-r013-golden-skill :completer
                       #'%skill-completer-r013-golden-skill :source-file #P"/dev/stdin"
                       :source-line nil :defined-at (%skill-now-ms))))
