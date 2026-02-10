(defpackage :ptui.core.events
  (:use :cl)
  (:export
   #:key-event #:make-key-event #:key-event-key #:key-event-ctrlp #:key-event-altp
   #:key-event-shiftp #:key-event-text?
   #:mouse-event #:make-mouse-event #:mouse-event-kind #:mouse-event-x #:mouse-event-y
   #:mouse-event-button
   #:paste-event #:make-paste-event #:paste-event-text))

(in-package :ptui.core.events)

(defstruct (key-event (:constructor make-key-event (key &key (ctrlp nil) (altp nil) (shiftp nil) (text? nil))))
  (key :unknown)
  (ctrlp nil :type boolean)
  (altp nil :type boolean)
  (shiftp nil :type boolean)
  (text? nil))

(defstruct (mouse-event (:constructor make-mouse-event (&key (kind :move) (x 0) (y 0) (button nil))))
  (kind :move)
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (button nil))

(defstruct (paste-event (:constructor make-paste-event (text)))
  (text "" :type string))
