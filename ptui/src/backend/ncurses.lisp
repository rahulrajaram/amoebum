(defpackage :ptui.backend.ncurses
  (:use :cl)
  (:export #:make-ncurses-backend))

(in-package :ptui.backend.ncurses)

(defclass ncurses-backend (ptui.backend.protocol:terminal-backend)
  ((delegate :initarg :delegate :reader backend-delegate)
   (compat-warning-logged-p :initform nil :accessor backend-compat-warning-logged-p))
  (:documentation
   "Compatibility ncurses backend.

When no native ncurses renderer is available, delegate protocol methods to ANSI."))

(defun make-ncurses-backend (&key caps)
  (let ((caps* (or caps (ptui.term.caps:probe-terminal-caps))))
    (make-instance 'ncurses-backend
                   :caps caps*
                   :delegate (ptui.backend.ansi:make-ansi-backend :caps caps*))))

(defun %maybe-log-compat-warning (backend)
  (unless (backend-compat-warning-logged-p backend)
    (setf (backend-compat-warning-logged-p backend) t)
    (ptui.util.log:log-warn "ncurses backend using ANSI compatibility path.")))

(defmethod ptui.backend.protocol:backend-init ((backend ncurses-backend))
  (%maybe-log-compat-warning backend)
  (ptui.backend.protocol:backend-init (backend-delegate backend)))

(defmethod ptui.backend.protocol:backend-shutdown ((backend ncurses-backend))
  (ptui.backend.protocol:backend-shutdown (backend-delegate backend)))

(defmethod ptui.backend.protocol:backend-poll-events ((backend ncurses-backend))
  (ptui.backend.protocol:backend-poll-events (backend-delegate backend)))

(defmethod ptui.backend.protocol:backend-size ((backend ncurses-backend))
  (ptui.backend.protocol:backend-size (backend-delegate backend)))

(defmethod ptui.backend.protocol:backend-commit ((backend ncurses-backend) draw-ops)
  (ptui.backend.protocol:backend-commit (backend-delegate backend) draw-ops))
