(defpackage :ptui.backend.ansi
  (:use :cl)
  (:export #:make-ansi-backend))

(in-package :ptui.backend.ansi)

(defclass ansi-backend (ptui.backend.protocol:terminal-backend)
  ((stdout :initarg :stdout :reader backend-stdout)
   (parser :initform (ptui.term.input:make-input-parser) :reader backend-parser)
   (raw-tty-state :initform nil :accessor backend-raw-tty-state)
   (signals-enabled-p :initform nil :accessor backend-signals-enabled-p)
   (color-mode :initform :x16 :accessor backend-color-mode)))

(defun %make-escape (fmt &rest args)
  (apply #'format nil (concatenate 'string "~C[" fmt) #\Esc args))

(defun %attrs-codes (attrs)
  (let ((codes '()))
    (unless attrs
      (return-from %attrs-codes '("22" "23" "24" "27" "29")))
    (when (ptui.core.types:attrs-boldp attrs) (push "1" codes))
    (when (ptui.core.types:attrs-dimp attrs) (push "2" codes))
    (when (ptui.core.types:attrs-italicp attrs) (push "3" codes))
    (when (ptui.core.types:attrs-underlinep attrs) (push "4" codes))
    (when (ptui.core.types:attrs-invertp attrs) (push "7" codes))
    (when (ptui.core.types:attrs-strikep attrs) (push "9" codes))
    (if codes
        (nreverse codes)
      '("22" "23" "24" "27" "29"))))

(defun %style->escape (mode fg bg attrs)
  (let ((codes (append '("0")
                       (%attrs-codes attrs)
                       (list (ptui.core.color:color->sgr fg :mode mode :fg-or-bg :fg)
                             (ptui.core.color:color->sgr bg :mode mode :fg-or-bg :bg)))))
    (%make-escape "~{~A~^;~}m" codes)))

(defun %emit-control-op (backend kind)
  (let ((stdout (backend-stdout backend)))
    (write-string
     (ecase kind
       (:clear-screen (%make-escape "2J"))
       (:clear-eol (%make-escape "K"))
       (:hide-cursor (%make-escape "?25l"))
       (:show-cursor (%make-escape "?25h"))
       (:enter-alt (%make-escape "?1049h"))
       (:exit-alt (%make-escape "?1049l")))
     stdout)))

(defun make-ansi-backend (&key caps (stdout *standard-output*))
  (make-instance 'ansi-backend
                 :caps (or caps (ptui.term.caps:probe-terminal-caps))
                 :stdout stdout))

(defmethod ptui.backend.protocol:backend-init ((backend ansi-backend))
  (let ((init-ok nil)
        (raw-state nil))
    (unwind-protect
         (progn
           (setf raw-state (ptui.term.tty::%tty-enable-raw)
                 (backend-raw-tty-state backend) raw-state
                 (backend-color-mode backend)
                 (ptui.core.color:resolve-color-mode
                  (ptui.backend.protocol::backend-caps backend)))
           (when (ptui.term.caps:terminal-caps-alt-screenp
                  (ptui.backend.protocol::backend-caps backend))
             (%emit-control-op backend :enter-alt))
           (%emit-control-op backend :hide-cursor)
           (handler-case
               (progn
                 (ptui.term.signals:signals-init)
                 (setf (backend-signals-enabled-p backend) t))
             (error (err)
               (setf (backend-signals-enabled-p backend) nil)
               (ptui.util.log:log-warn "signals-init failed: ~A" err)))
           (finish-output (backend-stdout backend))
           (setf init-ok t)
           nil)
      (unless init-ok
        (ignore-errors
          (ptui.term.tty::%tty-restore-raw raw-state))
        (setf (backend-raw-tty-state backend) nil)))))

(defmethod ptui.backend.protocol:backend-shutdown ((backend ansi-backend))
  (let ((raw-state (backend-raw-tty-state backend)))
    (%emit-control-op backend :show-cursor)
    (when (ptui.term.caps:terminal-caps-alt-screenp
           (ptui.backend.protocol::backend-caps backend))
      (%emit-control-op backend :exit-alt))
    (write-string (%make-escape "0m") (backend-stdout backend))
    (finish-output (backend-stdout backend))
    (setf (backend-raw-tty-state backend) nil)
    (ignore-errors
      (ptui.term.tty::%tty-restore-raw raw-state)))
  nil)

(defun %key-event-for-signal (signal)
  (case signal
    ((:int :term) (ptui.core.events:make-key-event :ctrl-c :ctrlp t))
    (:winch (ptui.core.events:make-key-event :resize))
    (otherwise (ptui.core.events:make-key-event signal))))

(defmethod ptui.backend.protocol:backend-poll-events ((backend ansi-backend))
  (let ((events '()))
    (let ((octets (ptui.term.tty:tty-read-bytes :max 4096)))
      (when (> (length octets) 0)
        (ptui.term.input:input-feed (backend-parser backend) octets)))
    (multiple-value-bind (parsed-events ignored-count)
        (ptui.term.input:input-drain-events (backend-parser backend))
      (declare (ignore ignored-count))
      (setf events (nconc events parsed-events)))
    (when (backend-signals-enabled-p backend)
      (dolist (signal (ptui.term.signals:signals-poll))
        (setf events (nconc events (list (%key-event-for-signal signal))))))
    events))

(defmethod ptui.backend.protocol:backend-size ((backend ansi-backend))
  (declare (ignore backend))
  (ptui.term.tty:tty-get-size))

(defmethod ptui.backend.protocol:backend-commit ((backend ansi-backend) draw-ops)
  (let ((stdout (backend-stdout backend))
        (mode (backend-color-mode backend))
        (bytes 0))
    (flet ((emit (string)
             (write-string string stdout)
             (incf bytes (length string))))
      (dolist (op draw-ops)
        (case (ptui.render.diff::draw-op-kind op)
          (:move
           (emit (%make-escape "~D;~DH"
                               (1+ (ptui.render.diff::draw-op-row op))
                               (1+ (ptui.render.diff::draw-op-col op)))))
          (:style
           (emit (%style->escape mode
                                 (ptui.render.diff::draw-op-fg op)
                                 (ptui.render.diff::draw-op-bg op)
                                 (ptui.render.diff::draw-op-attrs op))))
          (:write
           (emit (ptui.render.diff::draw-op-text op)))
          ((:clear-screen :clear-eol :hide-cursor :show-cursor :enter-alt :exit-alt)
           (let* ((kind (ptui.render.diff::draw-op-kind op))
                  (esc (ecase kind
                         (:clear-screen (%make-escape "2J"))
                         (:clear-eol (%make-escape "K"))
                         (:hide-cursor (%make-escape "?25l"))
                         (:show-cursor (%make-escape "?25h"))
                         (:enter-alt (%make-escape "?1049h"))
                         (:exit-alt (%make-escape "?1049l")))))
             (emit esc))))))
      (finish-output stdout)
      bytes))
