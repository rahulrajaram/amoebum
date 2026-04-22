
(progn
 (defun %skill-handler-r013-golden-skill-zero-args (invocation arguments context)
   (declare (ignorable invocation context))
   (let ((missing (%skill-missing-required-arguments 'nil arguments)))
     (when missing
       (return-from %skill-handler-r013-golden-skill-zero-args
         (make-slash-command-result :echo-input-p t :output
                                    (%skill-missing-arguments-output
                                     'common-lisp-user::r013-golden-skill-zero-args
                                     "/r013-golden-skill-zero-args" missing)))))
   (let* ()
     (declare (ignorable))
     (common-lisp-user::make-slash-command-result :echo-input-p t :output "ok")))
 (defun %skill-completer-r013-golden-skill-zero-args
        (command invocation index fragment prefix-tokens)
   (declare (ignore command invocation))
   (or (and nil (funcall nil index fragment prefix-tokens))
       (%skill-default-completer 'nil index fragment prefix-tokens)))
 (register-skill
  (make-skill-metadata :name #A((27) base-char . "r013-golden-skill-zero-args") :description
                       "Print a zero-argument skill response." :usage
                       "/r013-golden-skill-zero-args" :aliases 'nil :category :session :keybinding
                       nil :arguments (list) :handler #'%skill-handler-r013-golden-skill-zero-args
                       :completer #'%skill-completer-r013-golden-skill-zero-args :source-file
                       #P"/dev/stdin" :source-line nil :defined-at (%skill-now-ms))))