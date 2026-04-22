
(progn
 (defparameter *r013-golden-keys-trivial-keymap*
   (let ((map
          (make-keymap :name 'common-lisp-user::r013-golden-keys-trivial :description
                       "Trivial keymap.")))
     (register-key-binding map '(:key :enter :ctrl nil :alt nil :shift nil)
                           (lambda (#:state-0 #:key-event-1)
                             (let ((state #:state-0) (key-event #:key-event-1))
                               (declare (ignorable state key-event))
                               common-lisp-user::state))
                           :guard nil :description #A((7) base-char . "Submit.") :source-file
                           #P"/dev/stdin")
     (register-key-binding map '(:key #\LATIN_SMALL_LETTER_C :ctrl t :alt nil :shift nil)
                           (lambda (#:state-2 #:key-event-3)
                             (let ((state #:state-2) (key-event #:key-event-3))
                               (declare (ignorable state key-event))
                               common-lisp-user::state))
                           :guard nil :description #A((5) base-char . "Quit.") :source-file
                           #P"/dev/stdin")
     (register-key-binding map '(:key :tab :ctrl nil :alt nil :shift nil)
                           (lambda (#:state-4 #:key-event-5)
                             (let ((state #:state-4) (key-event #:key-event-5))
                               (declare (ignorable state key-event))
                               common-lisp-user::state))
                           :guard nil :description #A((9) base-char . "Complete.") :source-file
                           #P"/dev/stdin")
     (register-keymap map)
     map))
 'common-lisp-user::r013-golden-keys-trivial)