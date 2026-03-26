(in-package :amoebum)

(defun %sounds-usage ()
  "/sounds [list] | /sounds set <theme> | /sounds preview [category]")

(defun %notifications-usage ()
  "/notifications [list] | /notifications enable <backend> | /notifications disable <backend> | /notifications test-fire [event-type]")

(defun %speak-usage ()
  "/speak [last|on|off|status|stop|voice <name>]")

(defun %voice-usage ()
  "/voice [on|off|toggle|status|language <code>]")

(defun %sound-theme-label (theme-name)
  (if theme-name
      (string-downcase (symbol-name theme-name))
      "none"))

(defun %sound-category-label (category)
  (string-downcase (symbol-name category)))

(defun %parse-sound-category (token)
  (when (and token (plusp (length (%slash-trim token))))
    (intern (string-upcase (%slash-trim token)) :keyword)))

(defun %render-sounds-list ()
  (let ((themes (list-sound-themes))
        (active-name (active-sound-theme-name)))
    (if (null themes)
        "No sound themes registered."
        (with-output-to-string (out)
          (format out "Sound themes (active: ~A):~%"
                  (%sound-theme-label active-name))
          (dolist (theme themes)
            (format out "- ~A~:[~; (active)~]~@[ parent=~A~] mappings=~D~%"
                    (%sound-theme-label (sound-theme-name theme))
                    (eq (sound-theme-name theme) active-name)
                    (and (sound-theme-parent theme)
                         (%sound-theme-label (sound-theme-parent theme)))
                    (hash-table-count (sound-theme-mappings theme))))))))

(defun %sound-preview-result-output (category theme-name success detail sound-path)
  (let ((category-label (%sound-category-label category))
        (theme-label (%sound-theme-label theme-name))
        (path-label (and sound-path (princ-to-string sound-path))))
    (cond
      (success
       (format nil "Previewed ~A using theme ~A (~A)."
               category-label
               theme-label
               path-label))
      ((eq detail :no-sound-configured)
       (format nil "Theme ~A has no sound for ~A."
               theme-label
               category-label))
      ((eq detail :backend-disabled)
       (format nil "Sound backend is disabled. Resolved path: ~A."
               path-label))
      ((eq detail :backend-unavailable)
       (format nil "Sound backend is unavailable. Resolved path: ~A."
               path-label))
      ((eq detail :missing-sound-file)
       (format nil "Resolved preview sound is missing: ~A."
               path-label))
      ((stringp detail)
       (format nil "Sound preview failed: ~A" detail))
      (t
       (format nil "Sound preview could not play (~S)." detail)))))

(defun %preview-sound (category config)
  (let ((theme-name (or (active-sound-theme-name) :standard)))
    (if (fboundp 'preview-notification-sound)
        (multiple-value-bind (success detail sound-path)
            (funcall (symbol-function 'preview-notification-sound)
                     :category category
                     :config config
                     :theme theme-name)
          (%sound-preview-result-output category theme-name success detail sound-path))
        (let ((sound-path (resolve-active-sound-path category :config config)))
          (%sound-preview-result-output category theme-name nil :backend-unavailable sound-path)))))

(defun %notification-backend-label (backend-name)
  (string-downcase (symbol-name backend-name)))

(defun %notification-filter-label (filter)
  (cond
    ((eq filter :*)
     "*")
    ((listp filter)
     (format nil "~{~A~^, ~}"
             (mapcar (lambda (item)
                       (string-downcase (symbol-name item)))
                     filter)))
    (t
     (string-downcase (symbol-name filter)))))

(defun %ensure-notification-dispatcher-for-command (context)
  (let* ((cfg (or (slash-command-context-config context)
                  (%current-config-safe)))
         (bus (current-event-bus))
         (manager (ensure-notification-manager :config cfg :event-bus bus)))
    (or (and *notification-dispatcher*
             (notification-dispatcher-p *notification-dispatcher*)
             *notification-dispatcher*)
        (ensure-notification-dispatcher :manager manager :event-bus bus))))

(defun %render-notification-backend-list (dispatcher)
  (let ((entries (list-notification-dispatch-backends dispatcher)))
    (if (null entries)
        "No notification dispatch backends configured."
        (with-output-to-string (out)
          (format out "Notification backends (~D):~%" (length entries))
          (dolist (entry entries)
            (format out "- ~A enabled=~A priority=~D filter=~A~%"
                    (%notification-backend-label
                     (notification-dispatch-backend-name entry))
                    (if (notification-dispatch-backend-enabled-p entry) "yes" "no")
                    (notification-dispatch-backend-priority entry)
                    (%notification-filter-label
                     (notification-dispatch-backend-filter entry))))))))

(defun %parse-notification-event-type (token)
  (when (and token (plusp (length (%slash-trim token))))
    (%normalize-event-type token)))

(defun %render-tts-status ()
  (let* ((auto-p (eq t (cfg :tts-auto-speak)))
         (backend *tts-backend*)
         (active-voice
           (when (and backend (typep backend 'kokoro-tts-backend))
             (kokoro-tts-voice backend)))
         (configured-voice (cfg :tts-voice))
         (voice (or active-voice configured-voice *tts-default-voice*)))
    (format nil "TTS auto-speak: ~:[off~;on~], voice: ~A, speaking: ~:[no~;yes~]."
            auto-p
            voice
            (and backend (speaking-p backend)))))

(defun %sounds-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((raw (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments raw))
         (action-token (if tokens
                           (string-downcase (first tokens))
                           "list"))
         (cfg (or (slash-command-context-config context)
                  (%current-config-safe))))
    (labels ((invalid-usage (&optional details)
               (make-slash-command-result
                :echo-input-p t
                :output (format nil "~@[~A~%~]Usage: ~A"
                                details
                                (%sounds-usage)))))
      (cond
        ((member action-token '("list" "ls") :test #'string=)
         (if (> (length tokens) 1)
             (invalid-usage (format nil "Unexpected argument ~S." (second tokens)))
             (make-slash-command-result
              :echo-input-p t
              :output (%render-sounds-list))))
        ((string= action-token "set")
         (let ((theme-token (second tokens)))
           (cond
             ((/= (length tokens) 2)
              (invalid-usage "Usage: /sounds set <theme>"))
             ((null (find-sound-theme theme-token))
              (invalid-usage (format nil "Unknown sound theme ~S." theme-token)))
             (t
              (let ((active (set-active-sound-theme theme-token)))
                (make-slash-command-result
                 :echo-input-p t
                 :output (format nil "Active sound theme set to ~A."
                                 (%sound-theme-label active))))))))
        ((string= action-token "preview")
         (let* ((category (or (%parse-sound-category (second tokens)) :error))
                (extra (third tokens)))
           (if extra
               (invalid-usage (format nil "Unexpected argument ~S." extra))
               (make-slash-command-result
                :echo-input-p t
                :output (%preview-sound category cfg)))))
        (t
         (invalid-usage (format nil "Unknown /sounds action ~S." action-token)))))))

(defun %notifications-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((raw (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments raw))
         (action-token (if tokens
                           (string-downcase (first tokens))
                           "list"))
         (dispatcher (%ensure-notification-dispatcher-for-command context)))
    (labels ((invalid-usage (&optional details)
               (make-slash-command-result
                :echo-input-p t
                :output (format nil "~@[~A~%~]Usage: ~A"
                                details
                                (%notifications-usage)))))
      (cond
        ((member action-token '("list" "ls") :test #'string=)
         (if (> (length tokens) 1)
             (invalid-usage (format nil "Unexpected argument ~S." (second tokens)))
             (make-slash-command-result
              :echo-input-p t
              :output (%render-notification-backend-list dispatcher))))
        ((member action-token '("enable" "disable") :test #'string=)
         (let ((backend-token (second tokens))
               (extra (third tokens)))
           (cond
             ((or (null backend-token) extra)
              (invalid-usage (format nil "Usage: /notifications ~A <backend>"
                                     action-token)))
             (t
              (handler-case
                  (let* ((entry (set-notification-dispatch-backend-enabled-p
                                 dispatcher
                                 backend-token
                                 (string= action-token "enable")))
                         (status (if (notification-dispatch-backend-enabled-p entry)
                                     "enabled"
                                     "disabled")))
                    (make-slash-command-result
                     :echo-input-p t
                     :output (format nil "Notification backend ~A is now ~A."
                                     (%notification-backend-label
                                      (notification-dispatch-backend-name entry))
                                     status)))
                (error (condition)
                  (invalid-usage (princ-to-string condition))))))))
        ((string= action-token "test-fire")
         (let* ((event-type (or (%parse-notification-event-type (second tokens))
                                +event-type-tool-error+))
                (extra (third tokens)))
           (if extra
               (invalid-usage (format nil "Unexpected argument ~S." extra))
               (multiple-value-bind (ok destination)
                   (fire-notification-dispatch-test :dispatcher dispatcher
                                                    :event-type event-type)
                 (make-slash-command-result
                  :echo-input-p t
                  :output (if ok
                              (format nil "Notification test dispatched via ~A for ~A."
                                      (%notification-backend-label destination)
                                      (string-downcase (symbol-name event-type)))
                              (format nil "Notification test fallback exhausted (~A) for ~A."
                                      destination
                                      (string-downcase (symbol-name event-type)))))))))
        (t
         (invalid-usage (format nil "Unknown /notifications action ~S." action-token)))))))

(defun %speak-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((raw (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments raw))
         (action-token (if tokens
                           (string-downcase (first tokens))
                           "last"))
         (chat-state (slash-command-context-chat-state context)))
    (labels ((invalid-usage (&optional details)
               (make-slash-command-result
                :echo-input-p t
                :output (format nil "~@[~A~%~]Usage: ~A"
                                details
                                (%speak-usage)))))
      (cond
        ((member action-token '("last" "say" "speak") :test #'string=)
         (multiple-value-bind (ok text)
             (speak-last-assistant-response :chat-state chat-state)
           (make-slash-command-result
            :echo-input-p t
            :output (if ok
                        (format nil "Speaking last assistant response (~D chars)."
                                (length (or text "")))
                        "No assistant response available to speak yet."))))
        ((member action-token '("on" "off") :test #'string=)
         (let ((value (string= action-token "on")))
           (setconfig :tts-auto-speak value)
           (make-slash-command-result
            :echo-input-p t
            :output (format nil "TTS auto-speak ~:[disabled~;enabled~]." value))))
        ((member action-token '("status" "show") :test #'string=)
         (if (> (length tokens) 1)
             (invalid-usage (format nil "Unexpected argument ~S." (second tokens)))
             (make-slash-command-result
              :echo-input-p t
              :output (%render-tts-status))))
        ((string= action-token "stop")
         (let ((backend (or *tts-backend* (ensure-tts-backend))))
           (stop-speaking backend)
           (make-slash-command-result
            :echo-input-p t
            :output "TTS playback stopped.")))
        ((string= action-token "voice")
         (let ((voice-token (second tokens))
               (extra (third tokens)))
           (if (or (null voice-token) extra)
               (invalid-usage "Usage: /speak voice <name>")
               (let* ((trimmed (%slash-trim voice-token))
                      (backend (or *tts-backend*
                                   (ensure-tts-backend))))
                 (setconfig :tts-voice trimmed)
                 (set-voice backend trimmed)
                 (make-slash-command-result
                  :echo-input-p t
                  :output (format nil "TTS voice set to ~A." trimmed))))))
        (t
         (invalid-usage (format nil "Unknown /speak action ~S." action-token)))))))

(defun %voice-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((args-text (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments args-text))
         (action (if tokens
                     (string-downcase (first tokens))
                     "toggle"))
         (backend (ensure-asr-backend)))
    (labels ((status-output ()
               (format nil "Voice input ~:[disabled~;enabled~], listening=~:[no~;yes~], language=~A."
                       (voice-input-mode-enabled-p)
                       (listening-p backend)
                       (whisper-asr-backend-language backend)))
             (invalid-usage (&optional detail)
               (make-slash-command-result
                :echo-input-p t
                :output (format nil "~@[~A~%~]Usage: ~A"
                                detail
                                (%voice-usage)))))
      (cond
        ((member action '("on" "enable") :test #'string=)
         (enable-voice-input-mode :backend backend)
         (make-slash-command-result :echo-input-p t :output (status-output)))
        ((member action '("off" "disable") :test #'string=)
         (disable-voice-input-mode :backend backend)
         (make-slash-command-result :echo-input-p t :output (status-output)))
        ((string= action "toggle")
         (toggle-voice-input-mode :backend backend)
         (make-slash-command-result :echo-input-p t :output (status-output)))
        ((string= action "status")
         (make-slash-command-result :echo-input-p t :output (status-output)))
        ((member action '("language" "lang") :test #'string=)
         (let ((language (second tokens)))
           (if (or (null language)
                   (%slash-blank-p language))
               (invalid-usage "Missing language code.")
               (progn
                 (set-language backend language)
                 (make-slash-command-result
                  :echo-input-p t
                  :output (status-output))))))
        (t
         (invalid-usage (format nil "Unknown /voice action ~S." action)))))))

(defun %sounds-arg-completer (_command _invocation index fragment prefix-tokens)
  (declare (ignore _command _invocation))
  (let* ((head (and prefix-tokens (string-downcase (first prefix-tokens))))
         (prefix (%slash-trim fragment))
         (theme-options (mapcar #'%sound-theme-label (list-sound-theme-names)))
         (category-options '("error" "task-complete" "approval-needed")))
    (cond
      ((= index 0)
       (loop for option in '("list" "set" "preview")
             when (%starts-with-ci-p prefix option)
               collect option))
      ((and (string= head "set") (= index 1))
       (loop for option in theme-options
             when (%starts-with-ci-p prefix option)
               collect option))
      ((and (string= head "preview") (= index 1))
       (loop for option in category-options
             when (%starts-with-ci-p prefix option)
               collect option))
      (t
       nil))))

(defun %notifications-arg-completer (_command _invocation index fragment prefix-tokens)
  (declare (ignore _command _invocation))
  (let* ((head (and prefix-tokens (string-downcase (first prefix-tokens))))
         (prefix (%slash-trim fragment))
         (backends (mapcar (lambda (entry)
                             (%notification-backend-label
                              (notification-dispatch-backend-name entry)))
                           (list-notification-dispatch-backends
                            (or *notification-dispatcher*
                                (ignore-errors
                                  (%ensure-notification-dispatcher-for-command
                                   (make-slash-command-context))))))))
    (cond
      ((= index 0)
       (loop for option in '("list" "enable" "disable" "test-fire")
             when (%starts-with-ci-p prefix option)
               collect option))
      ((and (member head '("enable" "disable") :test #'string=) (= index 1))
       (loop for option in backends
             when (%starts-with-ci-p prefix option)
               collect option))
      ((and (string= head "test-fire") (= index 1))
       (loop for option in (mapcar (lambda (event-type)
                                     (string-downcase (symbol-name event-type)))
                                   +core-event-types+)
             when (%starts-with-ci-p prefix option)
               collect option))
      (t
       nil))))

(defun %speak-arg-completer (_command _invocation index fragment prefix-tokens)
  (declare (ignore _command _invocation))
  (let* ((prefix (%slash-trim fragment))
         (action (and prefix-tokens (string-downcase (first prefix-tokens)))))
    (cond
      ((= index 0)
       (loop for option in '("last" "on" "off" "status" "stop" "voice")
             when (%starts-with-ci-p prefix option)
               collect option))
      ((and (string= action "voice") (= index 1))
       (let* ((backend (or *tts-backend* (ignore-errors (ensure-tts-backend))))
              (voices (or (and backend (ignore-errors (list-voices backend)))
                          (copy-list *tts-default-voices*))))
         (loop for option in voices
               when (%starts-with-ci-p prefix option)
                 collect option)))
      (t nil))))

(defun %voice-arg-completer (_command _invocation index fragment _prefix-tokens)
  (if (= index 0)
      (let ((prefix (%slash-trim fragment)))
        (loop for option in '("on" "off" "toggle" "status" "language")
              when (%starts-with-ci-p prefix option)
                collect option))
      nil))

(defun register-notification-slash-commands ()
  (register-slash-command
   (make-slash-command
    :name "sounds"
    :description "List sound themes, set the active theme, or preview a theme sound."
    :usage (%sounds-usage)
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional action: list, set <theme>, preview [category]."))
    :handler #'%sounds-handler
    :completer #'%sounds-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "notifications"
    :description "Inspect dispatch backends, toggle them, and fire a dispatch test event."
    :usage (%notifications-usage)
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional action: list, enable <backend>, disable <backend>, test-fire [event-type]."))
    :handler #'%notifications-handler
    :completer #'%notifications-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "speak"
    :description "Speak the latest assistant response and manage TTS auto-speak."
    :usage (%speak-usage)
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional action: last, on, off, status, stop, voice <name>."))
    :handler #'%speak-handler
    :completer #'%speak-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "voice"
    :description "Toggle Whisper voice input and language."
    :usage (%voice-usage)
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional action: on, off, toggle, status, language <code>."))
    :handler #'%voice-handler
    :completer #'%voice-arg-completer))
  t)
