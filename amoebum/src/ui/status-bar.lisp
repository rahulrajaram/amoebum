(in-package :amoebum)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %status-event-type-keyword (name)
    (intern (string-upcase name) :keyword)))

(defparameter +event-type-ui-stream-progress+
  (%status-event-type-keyword "ui:stream-progress"))

(defparameter +default-context-window-tokens+ 128000)

(defstruct (status-bar-stream-payload
            (:constructor make-status-bar-stream-payload
                (&key
                   (status :idle)
                   (activep nil)
                   (tokens-used 0)
                   (tokens-per-second 0.0d0))))
  (status :idle)
  (activep nil :type boolean)
  (tokens-used 0 :type integer)
  (tokens-per-second 0.0d0 :type double-float))

(defstruct (status-bar-state
            (:constructor %make-status-bar-state
                (&key
                   permission-mode
                   (plan-mode-active-p nil)
                   branch-name
                   model-name
                   (context-used-tokens 0)
                   (context-max-tokens +default-context-window-tokens+)
                   (stream-status :idle)
                   (stream-tokens-per-second 0.0d0)
                   event-bus
                   (subscription-ids '()))))
  permission-mode
  (plan-mode-active-p nil :type boolean)
  (branch-name "-" :type string)
  (model-name "unknown" :type string)
  (context-used-tokens 0 :type integer)
  (context-max-tokens +default-context-window-tokens+ :type integer)
  (stream-status :idle)
  (stream-tokens-per-second 0.0d0 :type double-float)
  event-bus
  (subscription-ids '() :type list))

(defun %safe-string (value &optional (fallback "unknown"))
  (cond
    ((and (stringp value) (plusp (length value)))
     value)
    ((null value)
     fallback)
    (t
     (princ-to-string value))))

(defun %mode-string (mode)
  (string-downcase (%safe-string mode "supervised")))

(defun %coerce-nonnegative-integer (value &optional (fallback 0))
  (cond
    ((integerp value)
     (max 0 value))
    ((and (realp value) (not (complexp value)))
     (max 0 (truncate value)))
    (t
     fallback)))

(defun %coerce-double-float (value &optional (fallback 0.0d0))
  (if (and (realp value) (not (complexp value)))
      (coerce value 'double-float)
      fallback))

(defun %digit-run-end (text start)
  (let ((end start)
        (length (length text)))
    (loop while (and (< end length)
                     (digit-char-p (char text end)))
          do (incf end))
    end))

(defun %infer-context-window-tokens (model-name)
  (let* ((text (string-downcase (%safe-string model-name "")))
         (length (length text)))
    (or
     (loop for start from 0 below length do
       (when (digit-char-p (char text start))
         (let ((end (%digit-run-end text start)))
           (when (< end length)
             (let ((unit (char text end)))
               (when (member unit '(#\k #\m) :test #'char=)
                 (handler-case
                     (let ((number (parse-integer text :start start :end end))
                           (factor (if (char= unit #\k) 1000 1000000)))
                       (return (* number factor)))
                   (error () nil))))))))
     +default-context-window-tokens+)))

(defun %string-blank-p (value)
  (or (null value)
      (zerop (length value))
      (every (lambda (char)
               (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=))
             value)))

(defun %resolve-branch-name (&optional project-root)
  (let* ((root (or project-root *default-pathname-defaults*))
         (root-path (uiop:ensure-directory-pathname root))
         (root-string (namestring root-path))
         (output
           (handler-case
               (uiop:run-program
                (list "git" "-C" root-string "rev-parse" "--abbrev-ref" "HEAD")
                :output :string
                :error-output :string
                :ignore-error-status t)
             (error ()
               "")))
         (branch (string-trim '(#\Space #\Tab #\Newline #\Return) output)))
    (if (%string-blank-p branch)
        "-"
        branch)))

(defun %status-state-event-bus (state fallback-event-bus)
  (or fallback-event-bus
      (and state (status-bar-state-event-bus state))
      (current-event-bus)))

(defun %apply-config-changed-payload! (state payload)
  (when (config-changed-payload-p payload)
    (case (config-changed-payload-key payload)
      (:permission-mode
       (setf (status-bar-state-permission-mode state)
             (config-changed-payload-new-value payload)))
      (:plan-mode
       (setf (status-bar-state-plan-mode-active-p state)
             (not (null (config-changed-payload-new-value payload)))))
      (:model
       (let ((model (%safe-string (config-changed-payload-new-value payload) "unknown")))
         (setf (status-bar-state-model-name state) model
               (status-bar-state-context-max-tokens state)
               (%infer-context-window-tokens model)))))
    t))

(defun %coerce-stream-payload (payload)
  (cond
    ((status-bar-stream-payload-p payload)
     payload)
    ((listp payload)
     (make-status-bar-stream-payload
      :status (or (getf payload :status) :idle)
      :activep (not (null (getf payload :activep)))
      :tokens-used (%coerce-nonnegative-integer
                    (or (getf payload :tokens-used)
                        (getf payload :tokens))
                    0)
      :tokens-per-second (%coerce-double-float (getf payload :tokens-per-second) 0.0d0)))
    (t
     nil)))

(defun %apply-stream-payload! (state payload)
  (let ((stream-payload (%coerce-stream-payload payload)))
    (when stream-payload
      (setf (status-bar-state-stream-status state)
            (status-bar-stream-payload-status stream-payload)
            (status-bar-state-stream-tokens-per-second state)
            (status-bar-stream-payload-tokens-per-second stream-payload)
            (status-bar-state-context-used-tokens state)
            (status-bar-stream-payload-tokens-used stream-payload))
      t)))

(defun status-bar-unsubscribe (state)
  (check-type state status-bar-state)
  (let ((bus (status-bar-state-event-bus state)))
    (when (and (event-bus-p bus)
               (status-bar-state-subscription-ids state))
      (dolist (subscription-id (status-bar-state-subscription-ids state))
        (ignore-errors
          (unsubscribe bus subscription-id)))))
  (setf (status-bar-state-subscription-ids state) '())
  state)

(defun status-bar-subscribe (state &optional event-bus)
  (check-type state status-bar-state)
  (let ((bus (%status-state-event-bus state event-bus)))
    (unless (event-bus-p bus)
      (error "STATUS-BAR-SUBSCRIBE requires an EVENT-BUS, got ~S." bus))
    (when (and (event-bus-p (status-bar-state-event-bus state))
               (eq (status-bar-state-event-bus state) bus)
               (status-bar-state-subscription-ids state))
      (return-from status-bar-subscribe state))
    (status-bar-unsubscribe state)
    (setf (status-bar-state-event-bus state) bus)
    (let ((config-id
            (subscribe bus
                       +event-type-config-changed+
                       (lambda (event)
                         (%apply-config-changed-payload! state (event-payload event)))
                       :priority 25))
          (stream-id
            (subscribe bus
                       +event-type-ui-stream-progress+
                       (lambda (event)
                         (%apply-stream-payload! state (event-payload event)))
                       :priority 25)))
      (setf (status-bar-state-subscription-ids state)
            (list config-id stream-id)))
    state))

(defun make-status-bar-state (&key
                                (config (current-config))
                                event-bus
                                permission-mode
                                model-name
                                branch-name
                                project-root)
  (let* ((resolved-model
           (%safe-string (or model-name
                             (and (config-p config) (config-model config)))
                         "unknown"))
         (resolved-mode
           (or permission-mode
               (and (config-p config) (config-permission-mode config))
               :supervised))
         (resolved-plan-mode-active-p
           (and (config-p config)
                (not (null (config-value :plan-mode config)))))
         (resolved-root
           (or project-root
               (and (config-p config) (config-project-root config))
               *default-pathname-defaults*))
         (state
           (%make-status-bar-state
            :permission-mode resolved-mode
            :plan-mode-active-p resolved-plan-mode-active-p
            :branch-name (%safe-string (or branch-name
                                           (%resolve-branch-name resolved-root))
                                       "-")
            :model-name resolved-model
            :context-max-tokens (%infer-context-window-tokens resolved-model)
            :event-bus (%status-state-event-bus nil event-bus))))
    (status-bar-subscribe state)
    state))

(defun ensure-status-bar-state (state &key config event-bus)
  (if (and state (typep state 'status-bar-state))
      (status-bar-subscribe state event-bus)
      (make-status-bar-state :config config
                             :event-bus event-bus)))

(defun publish-status-bar-stream-summary (summary &key event-bus)
  (let* ((status (or (getf summary :status) :idle))
         (activep (if (getf summary :activep)
                      t
                      (eq status :running)))
         (tokens-used (%coerce-nonnegative-integer (getf summary :tokens) 0))
         (tokens-per-second (%coerce-double-float (getf summary :tokens-per-second) 0.0d0))
         (payload (make-status-bar-stream-payload
                   :status status
                   :activep activep
                   :tokens-used tokens-used
                   :tokens-per-second tokens-per-second))
         (bus (%status-state-event-bus nil event-bus)))
    (publish bus
             +event-type-ui-stream-progress+
             :source :amoebum
             :severity :debug
             :payload payload)
    payload))

(defun %stream-segment (state)
  (let ((status (status-bar-state-stream-status state))
        (tokens-per-second (status-bar-state-stream-tokens-per-second state)))
    (case status
      (:running
       (format nil "stream ~,2f tok/s" tokens-per-second))
      (:completed
       "stream done")
      (:cancelled
       "stream cancelled")
      (:failed
       "stream failed")
      (otherwise
       "stream idle"))))

(defun status-bar-segments (state)
  (check-type state status-bar-state)
  (let ((segments
          (list
           (format nil "mode ~A" (%mode-string (status-bar-state-permission-mode state)))
           (format nil "branch ~A" (%safe-string (status-bar-state-branch-name state) "-"))
           (format nil "ctx ~D/~D"
                   (%coerce-nonnegative-integer (status-bar-state-context-used-tokens state) 0)
                   (%coerce-nonnegative-integer (status-bar-state-context-max-tokens state)
                                                +default-context-window-tokens+))
           (format nil "model ~A" (%safe-string (status-bar-state-model-name state) "unknown"))
           (%stream-segment state))))
    (if (status-bar-state-plan-mode-active-p state)
        (cons "PLAN MODE -- read-only" segments)
        segments)))

(defun status-bar-line (state &key width)
  (check-type state status-bar-state)
  (let ((line (format nil "~{~A~^ | ~}" (status-bar-segments state))))
    (if (null width)
        line
        (let* ((target-width (max 0 width))
               (trimmed (ptui.text.layout:truncate-to-width line target-width))
               (actual-width (ptui.text.width:string-width trimmed)))
          (if (< actual-width target-width)
              (concatenate 'string
                           trimmed
                           (make-string (- target-width actual-width)
                                        :initial-element #\Space))
              trimmed)))))

(defun status-bar-render-key (state)
  (check-type state status-bar-state)
  (list (status-bar-state-permission-mode state)
        (status-bar-state-plan-mode-active-p state)
        (status-bar-state-branch-name state)
        (status-bar-state-model-name state)
        (status-bar-state-context-used-tokens state)
        (status-bar-state-context-max-tokens state)
        (status-bar-state-stream-status state)
        (truncate (* 100 (status-bar-state-stream-tokens-per-second state)))))

(defun make-status-bar-widget (state &key id key width)
  (ptui.widgets.core:make-text-widget
   (status-bar-line state :width width)
   :id id
   :key key))
