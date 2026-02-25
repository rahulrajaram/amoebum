(in-package :amoebum)

(defparameter *asr-run-command-function* #'pseudopod:run-command)
(defparameter *asr-backend* nil)
(defparameter *voice-input-mode-enabled-p* nil)
(defparameter *asr-barge-in-enabled-p* t)
(defparameter *asr-tts-speaking-function* nil)
(defparameter *asr-tts-stop-function* nil)
;; Defined by I230 when TTS backend is available.
(defvar *tts-backend* nil)

(defparameter *voice-transcription-lock*
  (bordeaux-threads:make-lock "amoebum-voice-transcription-lock"))
(defparameter *voice-transcription-queue* '())

(defclass asr-backend () ())

(defgeneric start-listening (backend &key on-transcription))
(defgeneric stop-listening (backend))
(defgeneric listening-p (backend))
(defgeneric set-language (backend language))

(defclass whisper-asr-backend (asr-backend)
  ((language
    :initarg :language
    :initform "en"
    :accessor whisper-asr-backend-language)
   (model
    :initarg :model
    :initform "base"
    :accessor whisper-asr-backend-model)
   (recorder
    :initarg :recorder
    :initform :auto
    :accessor whisper-asr-backend-recorder)
   (max-duration-seconds
    :initarg :max-duration-seconds
    :initform 6
    :accessor whisper-asr-backend-max-duration-seconds)
   (timeout-seconds
    :initarg :timeout-seconds
    :initform 30
    :accessor whisper-asr-backend-timeout-seconds)
   (whisper-command
    :initarg :whisper-command
    :initform "whisper"
    :accessor whisper-asr-backend-whisper-command)
   (listening-active-p
    :initform nil
    :accessor whisper-asr-backend-listening-active-p)
   (listener-thread
    :initform nil
    :accessor whisper-asr-backend-listener-thread)
   (transcription-callback
    :initform nil
    :accessor whisper-asr-backend-transcription-callback)
   (background-p
    :initarg :background-p
    :initform t
    :accessor whisper-asr-backend-background-p)
   (state-lock
    :initform (bordeaux-threads:make-lock "amoebum-whisper-asr-state-lock")
    :reader whisper-asr-backend-state-lock)))

(defun make-whisper-asr-backend (&key
                                   (language "en")
                                   (model "base")
                                   (recorder :auto)
                                   (max-duration-seconds 6)
                                   (timeout-seconds 30)
                                   (whisper-command "whisper")
                                   (background-p t))
  (make-instance 'whisper-asr-backend
                 :language language
                 :model model
                 :recorder recorder
                 :max-duration-seconds max-duration-seconds
                 :timeout-seconds timeout-seconds
                 :whisper-command whisper-command
                 :background-p background-p))

(defun ensure-asr-backend ()
  (unless (typep *asr-backend* 'asr-backend)
    (setf *asr-backend* (make-whisper-asr-backend)))
  *asr-backend*)

(defun %voice-shell-quote (text)
  (format nil "'~A'"
          (with-output-to-string (out)
            (loop for char across (or text "") do
              (if (char= char #\')
                  (write-string "'\"'\"'" out)
                  (write-char char out))))))

(defun %voice-command-string (arguments)
  (with-output-to-string (stream)
    (loop for argument in arguments
          for firstp = t then nil do
            (unless firstp
              (write-char #\Space stream))
            (write-string (%voice-shell-quote (or argument "")) stream))))

(defun %normalize-asr-run-result (result)
  (cond
    ((pseudopod:command-result-p result)
     (list :exit-code (or (pseudopod:command-result-exit-code result) 1)
           :stdout (or (pseudopod:command-result-stdout result) "")
           :stderr (or (pseudopod:command-result-stderr result) "")))
    ((listp result)
     (list :exit-code (or (getf result :exit-code) 1)
           :stdout (or (getf result :stdout) "")
           :stderr (or (getf result :stderr) "")))
    (t
     (list :exit-code 1
           :stdout ""
           :stderr (princ-to-string result)))))

(defun %asr-run-command (command &key timeout)
  (%normalize-asr-run-result
   (funcall *asr-run-command-function*
            command
            :timeout (or timeout 30))))

(defun %tts-speaking-p ()
  (cond
    ((functionp *asr-tts-speaking-function*)
     (ignore-errors (not (null (funcall *asr-tts-speaking-function*)))))
    ((and (fboundp 'speaking-p)
          (boundp '*tts-backend*))
     (ignore-errors (not (null (speaking-p *tts-backend*)))))
    (t nil)))

(defun %tts-stop-speaking ()
  (cond
    ((functionp *asr-tts-stop-function*)
     (ignore-errors (funcall *asr-tts-stop-function*)))
    ((and (fboundp 'stop-speaking)
          (boundp '*tts-backend*))
     (ignore-errors (stop-speaking *tts-backend*)))
    (t nil)))

(defun %barge-in-if-needed ()
  (when (and *asr-barge-in-enabled-p*
             (%tts-speaking-p))
    (%tts-stop-speaking)
    t))

(defun %voice-trim (text)
  (if (stringp text)
      (string-trim '(#\Space #\Tab #\Newline #\Return) text)
      ""))

(defun %whisper-recording-segment (backend)
  (let ((duration (max 1 (or (whisper-asr-backend-max-duration-seconds backend) 6)))
        (recorder (whisper-asr-backend-recorder backend)))
    (case recorder
      (:sox
       (format nil "sox -q -d -r 16000 -c 1 -b 16 -e signed-integer -t wav - trim 0 ~D"
               duration))
      ((:arecord :auto)
       (format nil "arecord -q -f S16_LE -r 16000 -c 1 -d ~D -t wav -"
               duration))
      (otherwise
       (format nil "sox -q -d -r 16000 -c 1 -b 16 -e signed-integer -t wav - trim 0 ~D"
               duration)))))

(defun %whisper-command (backend)
  (let* ((language (%voice-trim (whisper-asr-backend-language backend)))
         (model (%voice-trim (whisper-asr-backend-model backend)))
         (recording (%whisper-recording-segment backend))
         (arguments
           (append (list recording "|" (or (whisper-asr-backend-whisper-command backend) "whisper"))
                   (when (plusp (length model))
                     (list "--model" model))
                   (when (plusp (length language))
                     (list "--language" language))
                   '("--output_format" "txt" "-"))))
    (with-output-to-string (out)
      (loop for arg in arguments
            for firstp = t then nil do
              (unless firstp
                (write-char #\Space out))
              (write-string arg out)))))

(defun %queue-voice-transcription (text &key source)
  (let ((trimmed (%voice-trim text)))
    (when (plusp (length trimmed))
      (bordeaux-threads:with-lock-held (*voice-transcription-lock*)
        (setf *voice-transcription-queue*
              (append *voice-transcription-queue*
                      (list (list :text trimmed
                                  :source source
                                  :timestamp (get-universal-time)))))))))

(defun drain-voice-transcriptions ()
  (bordeaux-threads:with-lock-held (*voice-transcription-lock*)
    (prog1 (copy-list *voice-transcription-queue*)
      (setf *voice-transcription-queue* '()))))

(defun %default-transcription-callback (text)
  (%queue-voice-transcription text :source :whisper))

(defun %whisper-transcribe-once (backend)
  (%barge-in-if-needed)
  (let* ((command (%whisper-command backend))
         (result (%asr-run-command command
                                   :timeout (whisper-asr-backend-timeout-seconds backend)))
         (exit-code (getf result :exit-code 1)))
    (if (zerop exit-code)
        (let ((text (%voice-trim (getf result :stdout ""))))
          (unless (zerop (length text))
            text))
        (progn
          (ptui.util.log:log-warn "ASR command failed: exit=~S stderr=~S command=~S"
                                  exit-code
                                  (getf result :stderr)
                                  command)
          nil))))

(defun %whisper-listen-loop (backend)
  (loop while (listening-p backend) do
    (let* ((text (%whisper-transcribe-once backend))
           (callback (whisper-asr-backend-transcription-callback backend)))
      (when (and (stringp text) (plusp (length text)))
        (if (functionp callback)
            (ignore-errors (funcall callback text))
            (%default-transcription-callback text))))
    (sleep 0.1))
  (bordeaux-threads:with-lock-held ((whisper-asr-backend-state-lock backend))
    (setf (whisper-asr-backend-listener-thread backend) nil))
  t)

(defmethod start-listening ((backend whisper-asr-backend) &key on-transcription)
  (bordeaux-threads:with-lock-held ((whisper-asr-backend-state-lock backend))
    (setf (whisper-asr-backend-transcription-callback backend)
          (or on-transcription
              #'%default-transcription-callback))
    (setf (whisper-asr-backend-listening-active-p backend) t)
    (if (whisper-asr-backend-background-p backend)
        (unless (and (whisper-asr-backend-listener-thread backend)
                     (bordeaux-threads:thread-alive-p
                      (whisper-asr-backend-listener-thread backend)))
          (setf (whisper-asr-backend-listener-thread backend)
                (bordeaux-threads:make-thread
                 (lambda ()
                   (%whisper-listen-loop backend))
                 :name "amoebum-whisper-asr-listener")))
        (let ((text (%whisper-transcribe-once backend)))
          (when (and (stringp text) (plusp (length text)))
            (let ((callback (whisper-asr-backend-transcription-callback backend)))
              (if (functionp callback)
                  (ignore-errors (funcall callback text))
                  (%default-transcription-callback text))))))
    t))

(defmethod stop-listening ((backend whisper-asr-backend))
  (let ((thread nil))
    (bordeaux-threads:with-lock-held ((whisper-asr-backend-state-lock backend))
      (setf (whisper-asr-backend-listening-active-p backend) nil)
      (setf thread (whisper-asr-backend-listener-thread backend)))
    (when (and thread
               (bordeaux-threads:thread-alive-p thread)
               (not (eq thread (bordeaux-threads:current-thread))))
      (ignore-errors
        (bordeaux-threads:join-thread thread)))
    t))

(defmethod listening-p ((backend whisper-asr-backend))
  (not (null (whisper-asr-backend-listening-active-p backend))))

(defmethod set-language ((backend whisper-asr-backend) language)
  (let ((trimmed (%voice-trim language)))
    (unless (plusp (length trimmed))
      (error "ASR language must not be blank."))
    (setf (whisper-asr-backend-language backend) trimmed)
    trimmed))

(defun voice-input-mode-enabled-p ()
  (not (null *voice-input-mode-enabled-p*)))

(defun enable-voice-input-mode (&key backend)
  (let ((resolved (or backend (ensure-asr-backend))))
    (setf *voice-input-mode-enabled-p* t)
    (start-listening resolved :on-transcription #'%default-transcription-callback)
    t))

(defun disable-voice-input-mode (&key backend)
  (let ((resolved (or backend (ensure-asr-backend))))
    (setf *voice-input-mode-enabled-p* nil)
    (ignore-errors (stop-listening resolved))
    t))

(defun toggle-voice-input-mode (&key backend)
  (if (voice-input-mode-enabled-p)
      (disable-voice-input-mode :backend backend)
      (enable-voice-input-mode :backend backend)))
