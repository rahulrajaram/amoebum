
(progn
 (defparameter *r013-golden-keys-with-guard-keymap*
   (let ((map
          (make-keymap :name 'common-lisp-user::r013-golden-keys-with-guard :description
                       "Guarded keymap.")))
     (register-key-binding map '(:key :enter :ctrl nil :alt nil :shift nil)
                           (lambda (#:state-0 #:key-event-1)
                             (let ((state #:state-0) (key-event #:key-event-1))
                               (declare (ignorable state key-event))
                               common-lisp-user::state))
                           :guard
                           (lambda (#:state-0 #:key-event-1)
                             (let ((state #:state-0) (key-event #:key-event-1))
                               (declare (ignorable key-event))
                               t))
                           :description #A((16) base-char . "Submit if ready.") :source-file
                           #P"/dev/stdin")
     (register-key-binding map '(:key :escape :ctrl nil :alt nil :shift nil)
                           (lambda (#:state-2 #:key-event-3)
                             (let ((state #:state-2) (key-event #:key-event-3))
                               (declare (ignorable state key-event))
                               common-lisp-user::state))
                           :guard nil :description #A((7) base-char . "Cancel.") :source-file
                           #P"/dev/stdin")
     (register-keymap map)
     map))
 'common-lisp-user::r013-golden-keys-with-guard)