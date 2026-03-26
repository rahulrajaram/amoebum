(in-package :amoebum)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %status-event-type-keyword (name)
    (intern (string-upcase name) :keyword)))

(defparameter +event-type-ui-stream-progress+
  (%status-event-type-keyword "ui:stream-progress"))

(defparameter +default-context-window-tokens+ +default-context-window-limit+)
(defparameter +plan-mode-read-only-banner+ "PLAN MODE -- read-only")
(defparameter +plan-mode-lock-badge+ "[LOCK mutating tools blocked]")
(defparameter +known-status-bar-focus-modes+ '(:lean :code :docs :arch))
(defparameter +default-status-bar-focus-mode+ :arch)
(defparameter +status-bar-focus-mode-segments+
  '((:lean . (:branch :stream :model))
    (:code . (:branch :permission :context :stream :model :worker))
    (:docs . (:branch :context :stream :model :provider))
    (:arch . (:branch :permission :context :stream :model :provider :worker))))

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
                   (focus-mode +default-status-bar-focus-mode+)
                   (plan-mode-active-p nil)
                   (plan-mode-mutating-tools-blocked-p nil)
                   branch-name
                   model-name
                   context-limit-override
                   (context-used-tokens 0)
                   (context-max-tokens +default-context-window-tokens+)
                   (stream-status :idle)
                   (stream-tokens-per-second 0.0d0)
                   event-bus
                   (subscription-ids '()))))
  permission-mode
  (focus-mode +default-status-bar-focus-mode+)
  (plan-mode-active-p nil :type boolean)
  (plan-mode-mutating-tools-blocked-p nil :type boolean)
  (branch-name "-" :type string)
  (model-name "unknown" :type string)
  context-limit-override
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

(defun %normalize-status-bar-focus-mode (value)
  (let* ((text (string-trim '(#\Space #\Tab #\Newline #\Return)
                            (%safe-string value
                                          (symbol-name +default-status-bar-focus-mode+))))
         (candidate (intern (string-upcase text) :keyword)))
    (if (member candidate +known-status-bar-focus-modes+ :test #'eq)
        candidate
        +default-status-bar-focus-mode+)))

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

(defun %infer-context-window-tokens (model-name context-limit-override)
  (resolve-context-window-limit
   :model model-name
   :config-limit context-limit-override))

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
      (:status-bar-mode
       (setf (status-bar-state-focus-mode state)
             (%normalize-status-bar-focus-mode
              (config-changed-payload-new-value payload))))
      (:plan-mode
       (let ((active-p (not (null (config-changed-payload-new-value payload)))))
         (setf (status-bar-state-plan-mode-active-p state) active-p
               (status-bar-state-plan-mode-mutating-tools-blocked-p state)
               (and active-p
                    (plan-mode-mutating-tools-blocked-p nil active-p)))))
      (:context-window-limit
       (let ((override (config-changed-payload-new-value payload)))
         (setf (status-bar-state-context-limit-override state)
               (if (and (integerp override) (> override 0))
                   override
                   nil)
               (status-bar-state-context-max-tokens state)
               (%infer-context-window-tokens
                (status-bar-state-model-name state)
                (status-bar-state-context-limit-override state)))))
      (:model
       (let ((model (%safe-string (config-changed-payload-new-value payload) "unknown")))
         (setf (status-bar-state-model-name state) model
               (status-bar-state-context-max-tokens state)
               (%infer-context-window-tokens
                model
                (status-bar-state-context-limit-override state))))))
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
                                focus-mode
                                model-name
                                branch-name
                                context-window-limit
                                project-root)
  (let* ((resolved-model
           (%safe-string (or model-name
                             (and (config-p config) (config-model config)))
                         "unknown"))
         (resolved-mode
           (or permission-mode
               (and (config-p config) (config-permission-mode config))
               :supervised))
         (resolved-focus-mode
           (%normalize-status-bar-focus-mode focus-mode))
         (resolved-plan-mode-active-p
           (and (config-p config)
                (not (null (config-value :plan-mode config)))))
         (resolved-plan-mode-mutating-tools-blocked-p
           (and resolved-plan-mode-active-p
                (plan-mode-mutating-tools-blocked-p config)))
         (resolved-root
           (or project-root
               (and (config-p config) (config-project-root config))
               *default-pathname-defaults*))
         (resolved-context-limit
           (let ((candidate
                   (or context-window-limit
                       (and (config-p config)
                            (config-value :context-window-limit config)))))
             (if (and (integerp candidate) (> candidate 0))
                 candidate
                 nil)))
         (state
           (%make-status-bar-state
            :permission-mode resolved-mode
            :focus-mode resolved-focus-mode
            :plan-mode-active-p resolved-plan-mode-active-p
            :plan-mode-mutating-tools-blocked-p resolved-plan-mode-mutating-tools-blocked-p
            :branch-name (%safe-string (or branch-name
                                           (%resolve-branch-name resolved-root))
                                       "-")
            :model-name resolved-model
            :context-limit-override resolved-context-limit
            :context-max-tokens (%infer-context-window-tokens
                                 resolved-model
                                 resolved-context-limit)
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

(defun %context-budget-segment-text (state)
  (let* ((used (%coerce-nonnegative-integer (status-bar-state-context-used-tokens state) 0))
         (limit (%coerce-nonnegative-integer (status-bar-state-context-max-tokens state)
                                             +default-context-window-tokens+))
         (percent (context-usage-percent used limit)))
    (format nil "Tokens: ~D/~D (~D%)" used limit percent)))

(defun %context-budget-role (state)
  (case (context-usage-level
         (%coerce-nonnegative-integer (status-bar-state-context-used-tokens state) 0)
         (%coerce-nonnegative-integer (status-bar-state-context-max-tokens state)
                                      +default-context-window-tokens+))
    (:green :context-green)
    (:yellow :context-yellow)
    (otherwise :context-red)))

(defun %status-bar-focus-mode-segment-keys (state)
  (or (cdr (assoc (status-bar-state-focus-mode state)
                  +status-bar-focus-mode-segments+
                  :test #'eq))
      (cdr (assoc +default-status-bar-focus-mode+
                  +status-bar-focus-mode-segments+
                  :test #'eq))
      '()))

(defun %status-segment-spec (state segment-key)
  (case segment-key
    (:branch
     (list :text (format nil "branch ~A" (%safe-string (status-bar-state-branch-name state) "-"))
           :role :meta))
    (:permission
     (list :text (format nil "mode ~A" (%mode-string (status-bar-state-permission-mode state)))
           :role :meta))
    (:context
     (list :text (%context-budget-segment-text state)
           :role (%context-budget-role state)))
    (:stream
     (list :text (%stream-segment state)
           :role :meta))
    (:model
     (list :text (format nil "model ~A" (%safe-string (status-bar-state-model-name state) "unknown"))
           :role :meta))
    (:provider
     (let ((provider-indicator (provider-health-compact-indicator)))
       (when provider-indicator
         (list :text (getf provider-indicator :text)
               :role (or (getf provider-indicator :role) :meta)))))
    (:worker
     (let ((worker-segment (worker-status-bar-segment)))
       (when (and (stringp worker-segment)
                  (plusp (length worker-segment)))
         (list :text worker-segment
               :role :meta))))))

(defun %status-segment-specs (state)
  (remove nil
          (loop for segment-key in (%status-bar-focus-mode-segment-keys state)
                collect (%status-segment-spec state segment-key))))

(defun status-bar-segments (state)
  (check-type state status-bar-state)
  (let ((segments (mapcar (lambda (entry) (getf entry :text))
                          (%status-segment-specs state))))
    (if (and (status-bar-state-plan-mode-active-p state)
             (status-bar-state-plan-mode-mutating-tools-blocked-p state))
        (cons (format nil "~A ~A"
                      +plan-mode-read-only-banner+
                      +plan-mode-lock-badge+)
              segments)
        segments)))

(defun status-bar-styled-segments (state &key width)
  (check-type state status-bar-state)
  (let ((segments '())
        (segment-specs (%status-segment-specs state)))
    (when (and (status-bar-state-plan-mode-active-p state)
               (status-bar-state-plan-mode-mutating-tools-blocked-p state))
      (push (cons (format nil "~A ~A"
                          +plan-mode-read-only-banner+
                          +plan-mode-lock-badge+)
                  :system)
            segments)
      (push (cons " | " :status-bar) segments))
    (loop for spec in segment-specs
          for index from 0 do
            (when (> index 0)
              (push (cons " | " :status-bar) segments))
            (let ((role (getf spec :role)))
              (push (cons (getf spec :text)
                          (if (or (null role) (eq role :meta))
                              :status-bar
                              role))
                    segments)))
    (let ((result (nreverse segments)))
      ;; Pad to full width so status-bar background fills the line
      (when (and width (> width 0))
        (let* ((used (loop for seg in result
                           sum (ptui.text.width:string-width (car seg))))
               (remaining (- width used)))
          (when (> remaining 0)
            (setf result
                  (append result
                          (list (cons (make-string remaining :initial-element #\Space)
                                      :status-bar)))))))
      result)))

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
        (status-bar-state-focus-mode state)
        (status-bar-state-plan-mode-active-p state)
        (status-bar-state-plan-mode-mutating-tools-blocked-p state)
        (status-bar-state-branch-name state)
        (status-bar-state-model-name state)
        (status-bar-state-context-used-tokens state)
        (status-bar-state-context-max-tokens state)
        (context-usage-level (status-bar-state-context-used-tokens state)
                             (status-bar-state-context-max-tokens state))
        (status-bar-state-stream-status state)
        (truncate (* 100 (status-bar-state-stream-tokens-per-second state)))
        (provider-health-signature)
        (worker-status-bar-segment)))

(defun make-status-bar-widget (state &key id key width)
  (ptui.ui.elements:make-element
   :text
   :id id
   :key key
   :props (list :text (status-bar-line state :width width)
                :role :status-bar
                :styled-segments (status-bar-styled-segments state :width width))
   :children '()))
