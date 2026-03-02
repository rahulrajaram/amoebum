(defpackage :ptui.core.events
  (:use :cl)
  (:export
   #:key-event #:key-event-p #:make-key-event #:key-event-key #:key-event-ctrlp
   #:key-event-altp #:key-event-shiftp #:key-event-text?
   #:key-event-modifiers
   #:resize-event
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

(defun key-event-modifiers (event)
  "Return a list of active modifier keywords for `event`."
  (let ((modifiers '()))
    (when (key-event-ctrlp event) (push :ctrl modifiers))
    (when (key-event-altp event) (push :alt modifiers))
    (when (key-event-shiftp event) (push :shift modifiers))
    (nreverse modifiers)))

(defun resize-event (event)
  "Return true when EVENT is a resize key event."
  (and (typep event 'key-event)
       (eq (key-event-key event) :resize)))

(defstruct (mouse-event (:constructor make-mouse-event (&key (kind :move) (x 0) (y 0) (button nil))))
  (kind :move)
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (button nil))

(defstruct (paste-event (:constructor make-paste-event (text)))
  (text "" :type string))
