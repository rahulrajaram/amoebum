(in-package :amoebum)

;;; ============================================================
;;; I257: Sound Backend Protocol (Hailer Adapter)
;;;
;;; Defines a CLOS protocol for sound playback with three backends:
;;; :builtin (platform player), :hailer-cli (delegate to hailer),
;;; :hailer-mcp (stub for future MCP server).
;;; ============================================================

;;; --- Sound backend protocol ---

(defclass sound-backend () ())

(defgeneric sound-play (backend category &key theme-name)
  (:documentation
   "Play the sound for CATEGORY using the active (or specified) theme.
    Returns T on success, NIL on failure or silence."))

(defgeneric sound-stop (backend)
  (:documentation "Stop any currently playing sound."))

(defgeneric sound-list-themes (backend)
  (:documentation "Return a list of available theme name keywords."))

(defgeneric sound-resolve-category (backend category &key theme-name)
  (:documentation
   "Resolve CATEGORY to a pathname, :silence, or NIL (unmapped)."))

(defgeneric sound-backend-available-p (backend)
  (:documentation "Return T if this backend is operational."))

(defgeneric sound-backend-kind (backend)
  (:documentation "Return the backend kind keyword (:builtin, :hailer-cli, :hailer-mcp)."))

;;; --- Platform player detection ---

(defparameter *sound-player-command* nil
  "Cached platform sound player command, or :none if none found.")

(defparameter *sound-max-concurrent* 3)
(defparameter *sound-active-count* 0)

#+sb-thread
(defparameter *sound-count-lock*
  (sb-thread:make-mutex :name "amoebum-sound-count-lock"))

(defun %detect-platform-player ()
  "Detect available sound player. Returns command string or NIL."
  (flet ((command-exists-p (cmd)
           (ignore-errors
             (multiple-value-bind (_stdout _stderr exit-code)
                 (uiop:run-program (list "sh" "-c"
                                         (format nil "command -v ~A >/dev/null 2>&1" cmd))
                                   :ignore-error-status t
                                   :output :string
                                   :error-output :string)
               (declare (ignore _stdout _stderr))
               (zerop (or exit-code 1))))))
    #+linux
    (cond
      ((command-exists-p "paplay") "paplay")
      ((command-exists-p "aplay") "aplay")
      ((command-exists-p "mpv") "mpv --no-video")
      (t nil))
    #+darwin
    (if (command-exists-p "afplay") "afplay" nil)
    #-(or linux darwin)
    nil))

(defun %ensure-platform-player ()
  (when (null *sound-player-command*)
    (setf *sound-player-command* (or (%detect-platform-player) :none)))
  (unless (eq *sound-player-command* :none)
    *sound-player-command*))

(defun %play-wav-file (path)
  "Play a WAV file using the detected platform player.
   Returns T if playback started successfully."
  (let ((player (%ensure-platform-player)))
    (unless player
      (return-from %play-wav-file nil))
    (let ((resolved-path (namestring (truename path))))
      (unless (probe-file resolved-path)
        (return-from %play-wav-file nil))
      ;; Check concurrency limit
      #+sb-thread
      (sb-thread:with-mutex (*sound-count-lock*)
        (when (>= *sound-active-count* *sound-max-concurrent*)
          (return-from %play-wav-file nil))
        (incf *sound-active-count*))
      ;; Fire and forget
      (handler-case
          (let ((process
                  (uiop:launch-program
                   (format nil "~A ~A" player
                           (uiop:escape-shell-token resolved-path))
                   :output nil
                   :error-output nil)))
            ;; Track completion in background
            #+sb-thread
            (sb-thread:make-thread
             (lambda ()
               (ignore-errors
                 (uiop:wait-process process))
               (sb-thread:with-mutex (*sound-count-lock*)
                 (decf *sound-active-count*)))
             :name "amoebum-sound-reaper")
            t)
        (error ()
          #+sb-thread
          (sb-thread:with-mutex (*sound-count-lock*)
            (decf *sound-active-count*))
          nil)))))

;;; --- Builtin backend ---

(defclass builtin-sound-backend (sound-backend) ())

(defmethod sound-backend-kind ((backend builtin-sound-backend))
  :builtin)

(defmethod sound-backend-available-p ((backend builtin-sound-backend))
  (not (null (%ensure-platform-player))))

(defmethod sound-list-themes ((backend builtin-sound-backend))
  (list-sound-theme-names))

(defmethod sound-resolve-category ((backend builtin-sound-backend) category
                                   &key theme-name)
  (let* ((theme (or (and theme-name (find-sound-theme theme-name))
                    (active-sound-theme)))
         (resolved (and theme (resolve-sound theme category))))
    (cond
      ((eq resolved :silence) :silence)
      ((null resolved) nil)
      ((or (stringp resolved) (pathnamep resolved))
       (let ((path (pathname resolved)))
         (if (probe-file path) path nil)))
      (t nil))))

(defmethod sound-play ((backend builtin-sound-backend) category &key theme-name)
  (let ((resolved (sound-resolve-category backend category :theme-name theme-name)))
    (cond
      ((eq resolved :silence) nil)
      ((pathnamep resolved) (%play-wav-file resolved))
      (t nil))))

(defmethod sound-stop ((backend builtin-sound-backend))
  ;; Builtin backend plays fire-and-forget; no stop mechanism
  nil)

;;; --- Hailer CLI backend ---

(defclass hailer-cli-sound-backend (sound-backend)
  ((command :initarg :command
            :initform "hailer"
            :reader hailer-cli-command)
   (available-cached :initform nil
                     :accessor hailer-cli-available-cached)))

(defparameter *hailer-cli-runner* nil
  "Override runner for testing. Takes (list-of-strings) returns plist (:exit-code :stdout :stderr).")

(defun %hailer-run (backend args)
  "Run a hailer CLI command. Returns plist (:exit-code :stdout :stderr)."
  (let ((command (hailer-cli-command backend)))
    (handler-case
        (if *hailer-cli-runner*
            (funcall *hailer-cli-runner* (cons command args))
            (multiple-value-bind (stdout stderr exit-code)
                (uiop:run-program (cons command args)
                                  :ignore-error-status t
                                  :output :string
                                  :error-output :string)
              (list :exit-code (or exit-code 1)
                    :stdout (or stdout "")
                    :stderr (or stderr ""))))
      (error (c)
        (list :exit-code 127
              :stdout ""
              :stderr (princ-to-string c))))))

(defmethod sound-backend-kind ((backend hailer-cli-sound-backend))
  :hailer-cli)

(defmethod sound-backend-available-p ((backend hailer-cli-sound-backend))
  (let ((cached (hailer-cli-available-cached backend)))
    (if cached
        (eq cached :yes)
        (let* ((result (%hailer-run backend (list "--version")))
               (available (zerop (getf result :exit-code 1))))
          (setf (hailer-cli-available-cached backend)
                (if available :yes :no))
          available))))

(defmethod sound-list-themes ((backend hailer-cli-sound-backend))
  (let* ((result (%hailer-run backend (list "themes")))
         (exit-code (getf result :exit-code 1)))
    (if (zerop exit-code)
        (let ((lines (remove-if
                      (lambda (s)
                        (zerop (length (string-trim '(#\Space #\Tab) s))))
                      (uiop:split-string (getf result :stdout "")
                                         :separator '(#\Newline)))))
          (mapcar (lambda (name)
                    (intern (string-upcase (string-trim '(#\Space #\Tab) name))
                            :keyword))
                  lines))
        ;; Fall back to amoebum's own theme list
        (list-sound-theme-names))))

(defmethod sound-resolve-category ((backend hailer-cli-sound-backend) category
                                   &key theme-name)
  (let* ((cat-str (string-downcase (symbol-name category)))
         (args (if theme-name
                   (list "resolve" cat-str "--theme"
                         (string-downcase (symbol-name theme-name)))
                   (list "resolve" cat-str)))
         (result (%hailer-run backend args))
         (exit-code (getf result :exit-code 1))
         (stdout (string-trim '(#\Space #\Tab #\Newline #\Return)
                              (getf result :stdout ""))))
    (cond
      ((not (zerop exit-code)) nil)        ; not mapped
      ((string-equal stdout "silence") :silence)
      ((plusp (length stdout))
       (let ((path (pathname stdout)))
         (if (probe-file path) path nil)))
      (t nil))))

(defmethod sound-play ((backend hailer-cli-sound-backend) category &key theme-name)
  (let* ((cat-str (string-downcase (symbol-name category)))
         (args (if theme-name
                   (list "play" cat-str "--theme"
                         (string-downcase (symbol-name theme-name)))
                   (list "play" cat-str)))
         (result (%hailer-run backend args))
         (exit-code (getf result :exit-code 1)))
    (zerop exit-code)))

(defmethod sound-stop ((backend hailer-cli-sound-backend))
  (let ((result (%hailer-run backend (list "stop"))))
    (zerop (getf result :exit-code 1))))

;;; --- Hailer MCP backend (stub) ---

(defclass hailer-mcp-sound-backend (sound-backend) ())

(defmethod sound-backend-kind ((backend hailer-mcp-sound-backend))
  :hailer-mcp)

(defmethod sound-backend-available-p ((backend hailer-mcp-sound-backend))
  nil) ;; Not yet implemented

(defmethod sound-list-themes ((backend hailer-mcp-sound-backend))
  (list-sound-theme-names))

(defmethod sound-resolve-category ((backend hailer-mcp-sound-backend) category
                                   &key theme-name)
  (declare (ignore category theme-name))
  nil)

(defmethod sound-play ((backend hailer-mcp-sound-backend) category &key theme-name)
  (declare (ignore category theme-name))
  nil)

(defmethod sound-stop ((backend hailer-mcp-sound-backend))
  nil)

;;; --- Backend selection and autodetection ---

(defvar *sound-backend-instance* nil
  "Active sound backend instance.")

(defun %detect-hailer-available-p (&optional (command "hailer"))
  "Check if hailer command is available in PATH."
  (ignore-errors
    (multiple-value-bind (_stdout _stderr exit-code)
        (uiop:run-program (list "sh" "-c"
                                (format nil "command -v ~A >/dev/null 2>&1" command))
                          :ignore-error-status t
                          :output :string
                          :error-output :string)
      (declare (ignore _stdout _stderr))
      (zerop (or exit-code 1)))))

(defun select-sound-backend (&key (backend :auto) (hailer-command "hailer"))
  "Select and return a sound backend instance.
   BACKEND: :auto, :builtin, :hailer-cli, :hailer-mcp."
  (ecase backend
    (:auto
     (if (%detect-hailer-available-p hailer-command)
         (make-instance 'hailer-cli-sound-backend :command hailer-command)
         (make-instance 'builtin-sound-backend)))
    (:builtin
     (make-instance 'builtin-sound-backend))
    (:hailer-cli
     (make-instance 'hailer-cli-sound-backend :command hailer-command))
    (:hailer-mcp
     (make-instance 'hailer-mcp-sound-backend))))

(defun ensure-sound-backend ()
  "Return or create the active sound backend."
  (or *sound-backend-instance*
      (setf *sound-backend-instance*
            (let* ((backend-key (or (cfg :sound-backend) :auto))
                   (hailer-cmd (or (cfg :hailer-command) "hailer")))
              (select-sound-backend :backend backend-key
                                    :hailer-command hailer-cmd)))))

(defun reset-sound-backend ()
  "Reset the sound backend (forces re-detection)."
  (setf *sound-backend-instance* nil))

(defun play-sound (category &key theme-name)
  "Play a sound for CATEGORY through the active backend."
  (let ((backend (ensure-sound-backend)))
    (when (sound-backend-available-p backend)
      (sound-play backend category :theme-name theme-name))))
