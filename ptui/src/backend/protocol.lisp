(defpackage :ptui.backend.protocol
  (:use :cl)
  (:export
   #:terminal-backend
   #:backend-init #:backend-shutdown
   #:backend-poll-events #:backend-size
   #:backend-commit))

(in-package :ptui.backend.protocol)

(defclass terminal-backend ()
  ((caps :initarg :caps :reader backend-caps))
  (:documentation "Backend boundary object shared by all terminal backends."))

(defgeneric backend-init (backend)
  (:documentation "Initialize backend resources."))

(defgeneric backend-shutdown (backend)
  (:documentation "Release backend resources and restore terminal state."))

(defgeneric backend-poll-events (backend)
  (:documentation "Return a list of ptui.core.events instances."))

(defgeneric backend-size (backend)
  (:documentation "Return current terminal size as ptui.core.types:size."))

(defgeneric backend-commit (backend draw-ops)
  (:documentation "Apply draw-ops and return number of bytes written or ops applied."))
