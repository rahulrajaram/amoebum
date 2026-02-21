(in-package :amoebum)

(defparameter +provider-health-poll-interval-seconds+ 10
  "Polling cadence for provider health snapshots in the TUI.")

(defvar *model-router* nil
  "Global model router instance used by provider monitoring views.")

(defstruct (provider-health-entry
            (:constructor make-provider-health-entry
                (&key
                   name
                   (status :healthy)
                   (request-count 0)
                   (error-count 0)
                   (error-rate 0.0d0)
                   (last-latency-ms 0)
                   last-error-message)))
  (name "unknown" :type string)
  (status :healthy :type keyword)
  (request-count 0 :type integer)
  (error-count 0 :type integer)
  (error-rate 0.0d0 :type double-float)
  (last-latency-ms 0 :type integer)
  last-error-message)

(defstruct (provider-health-monitor-state
            (:constructor %make-provider-health-monitor-state
                (&key
                   (last-poll-at 0)
                   (last-updated-at 0)
                   (entries '()))))
  (last-poll-at 0 :type integer)
  (last-updated-at 0 :type integer)
  (entries '() :type list))

(defvar *provider-health-monitor* (%make-provider-health-monitor-state)
  "Global provider health monitor snapshot cache.")

(defun provider-health-monitor-reset! ()
  (setf *provider-health-monitor* (%make-provider-health-monitor-state))
  *provider-health-monitor*)

(defun %provider-dashboard-trim (value)
  (if (stringp value)
      (string-trim '(#\Space #\Tab #\Newline #\Return) value)
      ""))

(defun %provider-dashboard-safe-string (value &optional (fallback ""))
  (let ((trimmed (%provider-dashboard-trim (and value (princ-to-string value)))))
    (if (plusp (length trimmed))
        trimmed
        fallback)))

(defun %provider-dashboard-status (provider)
  (let ((healthy-p (pseudopod:provider-healthy-p provider))
        (error-rate (pseudopod:provider-error-rate provider))
        (error-count (pseudopod:provider-error-count provider)))
    (cond
      ((not healthy-p) :down)
      ((or (> error-rate 0.05d0)
           (> error-count 0))
       :degraded)
      (t
       :healthy))))

(defun %provider-dashboard-status-label (status)
  (ecase status
    (:healthy "healthy")
    (:degraded "degraded")
    (:down "down")))

(defun %provider-dashboard-status-short-label (status)
  (ecase status
    (:healthy "OK")
    (:degraded "DEG")
    (:down "ERR")))

(defun %provider-dashboard-status-role (status)
  (ecase status
    (:healthy :context-green)
    (:degraded :context-yellow)
    (:down :context-red)))

(defun %provider-dashboard-last-error-message (provider)
  (let* ((raw (pseudopod:provider-last-error provider))
         (text (%provider-dashboard-safe-string raw "")))
    (when (plusp (length text))
      (if (> (length text) 96)
          (concatenate 'string (subseq text 0 93) "...")
          text))))

(defun %provider-dashboard-collect-entry (provider)
  (let ((status (%provider-dashboard-status provider)))
    (make-provider-health-entry
     :name (%provider-dashboard-safe-string (pseudopod:provider-name provider) "unknown")
     :status status
     :request-count (max 0 (pseudopod:provider-request-count provider))
     :error-count (max 0 (pseudopod:provider-error-count provider))
     :error-rate (coerce (max 0.0d0 (pseudopod:provider-error-rate provider)) 'double-float)
     :last-latency-ms (max 0 (pseudopod:provider-last-latency-ms provider))
     :last-error-message (%provider-dashboard-last-error-message provider))))

(defun %provider-dashboard-collect-entries (router)
  (if (and router (pseudopod:model-router-p router))
      (let* ((providers (copy-list (or (pseudopod:model-router-providers router) '())))
             (ordered (nreverse providers)))
        (mapcar #'%provider-dashboard-collect-entry ordered))
      '()))

(defun %provider-dashboard-poll-due-p (monitor now)
  (or (zerop (provider-health-monitor-state-last-poll-at monitor))
      (>= (- now (provider-health-monitor-state-last-poll-at monitor))
          +provider-health-poll-interval-seconds+)))

(defun provider-health-refresh! (&key (router *model-router*) (force nil))
  "Refresh provider health snapshot, polling router health at most every 10 seconds."
  (let* ((monitor (or *provider-health-monitor* (provider-health-monitor-reset!)))
         (now (get-universal-time)))
    (when (or force (%provider-dashboard-poll-due-p monitor now))
      (when (and router (pseudopod:model-router-p router))
        (ignore-errors
          (pseudopod:router-check-health router :force t)))
      (setf (provider-health-monitor-state-last-poll-at monitor) now
            (provider-health-monitor-state-last-updated-at monitor) now
            (provider-health-monitor-state-entries monitor)
            (%provider-dashboard-collect-entries router)))
    (provider-health-monitor-state-entries monitor)))

(defun provider-health-entries (&key (router *model-router*) (force nil))
  (provider-health-refresh! :router router :force force))

(defun provider-health-last-updated-at ()
  (let ((monitor *provider-health-monitor*))
    (if monitor
        (provider-health-monitor-state-last-updated-at monitor)
        0)))

(defun %provider-dashboard-short-code (name)
  (let* ((text (%provider-dashboard-safe-string name "?"))
         (index (position-if #'alphanumericp text)))
    (if index
        (string-upcase (string (char text index)))
        "?")))

(defun %provider-dashboard-worst-status (entries)
  (cond
    ((some (lambda (entry)
             (eq (provider-health-entry-status entry) :down))
           entries)
     :down)
    ((some (lambda (entry)
             (eq (provider-health-entry-status entry) :degraded))
           entries)
     :degraded)
    (t
     :healthy)))

(defun provider-health-compact-indicator (&key (router *model-router*) (force nil))
  (let ((entries (provider-health-entries :router router :force force)))
    (when entries
      (let* ((parts
               (mapcar (lambda (entry)
                         (format nil "~A:~A"
                                 (%provider-dashboard-short-code
                                  (provider-health-entry-name entry))
                                 (%provider-dashboard-status-short-label
                                  (provider-health-entry-status entry))))
                       entries))
             (status (%provider-dashboard-worst-status entries)))
        (list :text (format nil "[~{~A~^ ~}]" parts)
              :role (%provider-dashboard-status-role status)
              :status status)))))

(defun provider-health-signature ()
  (let ((entries (and *provider-health-monitor*
                      (provider-health-monitor-state-entries *provider-health-monitor*))))
    (loop for entry in entries
          collect (list (provider-health-entry-name entry)
                        (provider-health-entry-status entry)
                        (provider-health-entry-request-count entry)
                        (provider-health-entry-error-count entry)
                        (truncate (* 1000 (provider-health-entry-error-rate entry)))
                        (provider-health-entry-last-latency-ms entry)
                        (provider-health-entry-last-error-message entry)))))

(defun %provider-dashboard-segments-text (segments)
  (with-output-to-string (out)
    (dolist (segment segments)
      (write-string (or (getf segment :text) "") out))))

(defun %provider-dashboard-text-element (id segments &key (role :meta))
  (ptui.ui.elements:make-element
   :text
   :id id
   :props (list :text (%provider-dashboard-segments-text segments)
                :role role
                :styled-segments segments)
   :children '()))

(defun %provider-dashboard-header-element (entries updated-at)
  (let* ((count (length entries))
         (updated (if (and (integerp updated-at) (> updated-at 0))
                      (write-to-string updated-at)
                      "never"))
         (text (format nil "Provider Health (~D)  poll=~Ds  updated=~A"
                       count
                       +provider-health-poll-interval-seconds+
                       updated)))
    (ptui.ui.elements:make-element
     :text
     :id :provider-health-header
     :props (list :text text :role :meta)
     :children '())))

(defun %provider-dashboard-row-element (entry index)
  (let* ((status (provider-health-entry-status entry))
         (error-rate (* 100.0d0 (provider-health-entry-error-rate entry)))
         (status-segments
           (list (list :text (format nil "~2,'0D " (1+ index)) :role :meta)
                 (list :text (format nil "~A " (provider-health-entry-name entry))
                       :role :meta)
                 (list :text (string-upcase (%provider-dashboard-status-label status))
                       :role (%provider-dashboard-status-role status)
                       :boldp t)
                 (list :text (format nil " req=~D err=~D rate=~,1f%% lat=~Dms"
                                     (provider-health-entry-request-count entry)
                                     (provider-health-entry-error-count entry)
                                     error-rate
                                     (provider-health-entry-last-latency-ms entry))
                       :role :meta)))
         (error-message (provider-health-entry-last-error-message entry)))
    (if (and (stringp error-message)
             (plusp (length error-message)))
        (%provider-dashboard-text-element
         (intern (format nil "PROVIDER-HEALTH-ROW-~D" index) :keyword)
         (append status-segments
                 (list (list :text " error="
                             :role :meta)
                       (list :text error-message
                             :role (%provider-dashboard-status-role status)))))
        (%provider-dashboard-text-element
         (intern (format nil "PROVIDER-HEALTH-ROW-~D" index) :keyword)
         status-segments))))

(defun %provider-dashboard-empty-element ()
  (ptui.ui.elements:make-element
   :text
   :id :provider-health-empty
   :props (list :text "No providers configured in model router."
                :role :meta)
   :children '()))

(defun provider-health-row-elements (entries)
  (if entries
      (loop for entry in entries
            for index from 0
            collect (%provider-dashboard-row-element entry index))
      (list (%provider-dashboard-empty-element))))

(ptui.widgets.defwidget:defwidget provider-health-panel (state)
  (:memoize nil)
  (let* ((force-refresh-p (not (null (getf state :force-refresh-p))))
         (entries (or (getf state :entries)
                      (provider-health-entries :force force-refresh-p)))
         (updated-at (or (getf state :updated-at)
                         (provider-health-last-updated-at))))
    (box
     (vstack
      (%provider-dashboard-header-element entries updated-at)
      (map-widget #'identity (provider-health-row-elements entries)))
     :id :provider-health-panel
     :border t)))
