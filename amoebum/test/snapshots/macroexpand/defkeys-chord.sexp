
(progn
 (defparameter *r013-golden-keys-chord-keymap*
   (let ((map
          (make-keymap :name 'common-lisp-user::r013-golden-keys-chord :description
                       "Chord keymap.")))
     (register-key-binding map '(:key #\LATIN_SMALL_LETTER_X :ctrl t :alt nil :shift nil)
                           (lambda (#:state-0 #:key-event-1)
                             (let ((state #:state-0) (key-event #:key-event-1))
                               (declare (ignorable state key-event))
                               common-lisp-user::state))
                           :guard nil :description #A((19) base-char . "Prefix placeholder.")
                           :source-file #P"/dev/stdin")
     (register-key-binding map '(:key #\LATIN_SMALL_LETTER_X :ctrl nil :alt t :shift nil)
                           (lambda (#:state-2 #:key-event-3)
                             (let ((state #:state-2) (key-event #:key-event-3))
                               (declare (ignorable state key-event))
                               common-lisp-user::state))
                           :guard nil :description #A((24) base-char . "Meta prefix placeholder.")
                           :source-file #P"/dev/stdin")
     (register-key-binding map '(:key :up :ctrl t :alt nil :shift t)
                           (lambda (#:state-4 #:key-event-5)
                             (let ((state #:state-4) (key-event #:key-event-5))
                               (declare (ignorable state key-event))
                               common-lisp-user::state))
                           :guard nil :description
                           #A((29) base-char . "Control-shift-up placeholder.") :source-file
                           #P"/dev/stdin")
     (register-keymap map)
     map))
 'common-lisp-user::r013-golden-keys-chord)