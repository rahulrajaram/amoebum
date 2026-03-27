(in-package :amoebum)

;;; ============================================================
;;; I362: Real-time agent activity stream and filtering
;;; ============================================================

(defparameter +event-type-agent-activity+
  (%event-type-keyword "agent:activity"))

(setf +core-event-types+
      (remove-duplicates (append +core-event-types+
                                 (list +event-type-agent-activity+))
                         :test #'eq))

(defparameter +agent-activity-types+
  '(:inference :tool-call :waiting :idle))

(defparameter *agent-activity-max-entries* 512)
(defparameter *agent-activity-stream* '())
(defparameter *agent-activity-sequence* 0)
(defparameter *agent-activity-lock*
  (bordeaux-threads:make-lock "amoebum-agent-activity-lock"))
(defparameter *agent-activity-subscription-id* nil)
(defparameter *agent-activity-subscription-bus* nil)

(defstruct (agent-activity-entry
            (:constructor make-agent-activity-entry
                (&key
                   (sequence 0)
                   (timestamp (get-universal-time))
                   (agent-id "main")
                   (activity-type :idle)
                   description
                   metadata
                   source-event-type)))
  (sequence 0 :type integer)
  (timestamp (get-universal-time) :type integer)
  (agent-id "main" :type string)
  (activity-type :idle :type keyword)
  description
  metadata
  source-event-type)

(defun %agent-activity-trim (value)
  (if (stringp value)
      (string-trim '(#\Space #\Tab #\Newline #\Return) value)
      ""))

(defun %agent-activity-blank-p (value)
  (zerop (length (%agent-activity-trim value))))

(defun %normalize-agent-activity-agent-id (agent-id)
  (let ((trimmed (%agent-activity-trim (and agent-id (princ-to-string agent-id)))))
    (if (plusp (length trimmed))
        trimmed
        "main")))

(defun %normalize-agent-activity-type (activity-type)
  (let ((keyword (cond
                   ((keywordp activity-type) activity-type)
                   ((symbolp activity-type)
                    (intern (string-upcase (symbol-name activity-type)) :keyword))
                   ((stringp activity-type)
                    (intern (string-upcase (%agent-activity-trim activity-type)) :keyword))
                   (t :idle))))
    (if (member keyword +agent-activity-types+ :test #'eq)
        keyword
        :idle)))

(defun %normalize-agent-activity-description (description)
  (let ((trimmed (%agent-activity-trim (and description (princ-to-string description)))))
    (if (plusp (length trimmed))
        trimmed
        nil)))

(defun %agent-activity-plist-p (value)
  (and (listp value)
       (or (null value)
           (keywordp (first value)))))

(defun %agent-activity-payload-value (payload key)
  (cond
    ((%agent-activity-plist-p payload)
     (getf payload key))
    ((hash-table-p payload)
     (gethash key payload))
    (t
     nil)))

(defun %event->agent-id (event)
  (let* ((payload (event-payload event))
         (id (or (%agent-activity-payload-value payload :agent-id)
                 (%agent-activity-payload-value payload :id)
                 (%agent-activity-payload-value payload :worker-id))))
    (%normalize-agent-activity-agent-id id)))

(defun %event->agent-activity (event)
  (let ((event-type (event-type event))
        (payload (event-payload event)))
    (cond
      ((eq event-type +event-type-agent-activity+)
       nil)
      ((eq event-type +event-type-agent-spawn+)
       (list :agent-id (%event->agent-id event)
             :activity-type :waiting
             :description
             (or (%normalize-agent-activity-description
                  (%agent-activity-payload-value payload :task))
                 "Agent queued.")
             :metadata payload))
      ((eq event-type +event-type-agent-complete+)
       (list :agent-id (%event->agent-id event)
             :activity-type :idle
             :description
             (if (%agent-activity-payload-value payload :error-message)
                 "Agent finished with error."
                 "Agent finished.")
             :metadata payload))
      ((eq event-type +event-type-agent-cancelled+)
       (list :agent-id (%event->agent-id event)
             :activity-type :idle
             :description "Agent cancellation requested."
             :metadata payload))
      ((eq event-type +event-type-user-handoff-requested+)
       (list :agent-id (%event->agent-id event)
             :activity-type :waiting
             :description "SW4RM handoff requested."
             :metadata payload))
      ((eq event-type +event-type-user-handoff-accepted+)
       (list :agent-id (%event->agent-id event)
             :activity-type :waiting
             :description "SW4RM handoff accepted."
             :metadata payload))
      ((eq event-type +event-type-user-handoff-rejected+)
       (list :agent-id (%event->agent-id event)
             :activity-type :idle
             :description "SW4RM handoff rejected."
             :metadata payload))
      ((eq event-type +event-type-user-handoff-completed+)
       (list :agent-id (%event->agent-id event)
             :activity-type :idle
             :description "SW4RM handoff completed."
             :metadata payload))
      ((eq event-type +event-type-user-negotiation-room-created+)
       (list :agent-id (%event->agent-id event)
             :activity-type :waiting
             :description "SW4RM negotiation room created."
             :metadata payload))
      ((eq event-type +event-type-user-negotiation-artifact-submitted+)
       (list :agent-id (%event->agent-id event)
             :activity-type :waiting
             :description "SW4RM negotiation artifact submitted."
             :metadata payload))
      ((eq event-type +event-type-user-negotiation-critique-added+)
       (list :agent-id (%event->agent-id event)
             :activity-type :inference
             :description "SW4RM negotiation critique submitted."
             :metadata payload))
      ((eq event-type +event-type-user-negotiation-decision+)
       (list :agent-id (%event->agent-id event)
             :activity-type :idle
             :description "SW4RM negotiation decision recorded."
             :metadata payload))
      ((or (eq event-type +event-type-tool-call-started+)
           (eq event-type +event-type-tool-call-argument-complete+)
           (eq event-type +event-type-tool-invoked+)
           (eq event-type +event-type-tool-completed+)
           (eq event-type +event-type-tool-error+))
       (let* ((tool-name
                (or (and (typep payload 'tool-call-started-payload)
                         (tool-call-started-payload-tool-name payload))
                    (and (typep payload 'tool-call-argument-complete-payload)
                         (tool-call-argument-complete-payload-tool-name payload))
                    (and (typep payload 'tool-invoked-payload)
                         (tool-invoked-payload-tool-name payload))
                    (and (typep payload 'tool-completed-payload)
                         (tool-completed-payload-tool-name payload))
                    (and (typep payload 'tool-error-payload)
                         (tool-error-payload-tool-name payload))
                    (%agent-activity-payload-value payload :tool-name))))
         (list :agent-id (%event->agent-id event)
               :activity-type :tool-call
               :description (if (%agent-activity-blank-p tool-name)
                                "Tool call activity."
                                (format nil "Tool call: ~A" tool-name))
               :metadata payload)))
      ((eq event-type +event-type-llm-stream-chunk+)
       (list :agent-id (%event->agent-id event)
             :activity-type :inference
             :description "Streaming inference."
             :metadata payload))
      (t
       nil))))

(defun ensure-agent-activity-stream (&optional (event-bus (current-event-bus)))
  (unless (event-bus-p event-bus)
    (return-from ensure-agent-activity-stream nil))
  (let ((old-bus nil)
        (old-sub nil))
    (bordeaux-threads:with-lock-held (*agent-activity-lock*)
      (unless (eq *agent-activity-subscription-bus* event-bus)
        (setf old-bus *agent-activity-subscription-bus*
              old-sub *agent-activity-subscription-id*
              *agent-activity-subscription-bus* nil
              *agent-activity-subscription-id* nil)))
    (when (and old-bus old-sub)
      (ignore-errors
        (unsubscribe old-bus old-sub)))
    (bordeaux-threads:with-lock-held (*agent-activity-lock*)
      (unless *agent-activity-subscription-id*
        (setf *agent-activity-subscription-id*
              (subscribe event-bus :*
                         (lambda (event)
                           (let ((mapped (%event->agent-activity event)))
                             (when mapped
                               (record-agent-activity
                                (getf mapped :activity-type)
                                :agent-id (getf mapped :agent-id)
                                :description (getf mapped :description)
                                :metadata (getf mapped :metadata)
                                :source-event-type (event-type event)
                                :event-bus event-bus)))))
              *agent-activity-subscription-bus* event-bus))))
  t)

(defun clear-agent-activity-stream ()
  (bordeaux-threads:with-lock-held (*agent-activity-lock*)
    (setf *agent-activity-stream* '()
          *agent-activity-sequence* 0))
  t)

(defun record-agent-activity (activity-type &key
                                            agent-id
                                            description
                                            metadata
                                            source-event-type
                                            (event-bus (current-event-bus)))
  (ensure-agent-activity-stream event-bus)
  (let ((entry nil))
    (bordeaux-threads:with-lock-held (*agent-activity-lock*)
      (incf *agent-activity-sequence*)
      (setf entry
            (make-agent-activity-entry
             :sequence *agent-activity-sequence*
             :timestamp (get-universal-time)
             :agent-id (%normalize-agent-activity-agent-id agent-id)
             :activity-type (%normalize-agent-activity-type activity-type)
             :description (%normalize-agent-activity-description description)
             :metadata metadata
             :source-event-type source-event-type))
      (push entry *agent-activity-stream*)
      (when (> (length *agent-activity-stream*) *agent-activity-max-entries*)
        (setf *agent-activity-stream*
              (subseq *agent-activity-stream* 0 *agent-activity-max-entries*))))
    (when (event-bus-p event-bus)
      (publish event-bus
               +event-type-agent-activity+
               :source :agent-activity
               :severity :info
               :payload entry))
    entry))

(defun list-agent-activity (&key agent-id activity-type (limit 20))
  (ensure-agent-activity-stream)
  (let ((resolved-agent-id (and (not (%agent-activity-blank-p agent-id))
                                (%normalize-agent-activity-agent-id agent-id)))
        (resolved-type (and activity-type
                            (%normalize-agent-activity-type activity-type))))
    (bordeaux-threads:with-lock-held (*agent-activity-lock*)
      (let ((filtered
              (remove-if-not
               (lambda (entry)
                 (and (or (null resolved-agent-id)
                          (string= resolved-agent-id
                                   (agent-activity-entry-agent-id entry)))
                      (or (null resolved-type)
                          (eq resolved-type
                              (agent-activity-entry-activity-type entry)))))
               *agent-activity-stream*)))
        (if (and (integerp limit) (> limit 0))
            (subseq filtered 0 (min limit (length filtered)))
            filtered)))))

(defun %agent-activity-short-type (activity-type)
  (case activity-type
    (:inference "INF")
    (:tool-call "TOOL")
    (:waiting "WAIT")
    (:idle "IDLE")
    (otherwise "UNK")))

(defun %agent-activity-timestamp-string (timestamp)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time timestamp)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour minute second)))

(defun %activity-text (content)
  (ptui.ui.elements:make-element :text :props (list :content content)))

(defun %activity-box (children &key (direction :vertical))
  (ptui.ui.elements:make-element :box
                                 :props (list :direction direction)
                                 :children children))

(ptui.widgets.defwidget:defwidget agent-activity-stream (state)
  (:memoize :equal)
  (let* ((agent-id (getf state :agent-id))
         (activity-type (getf state :activity-type))
         (limit (or (getf state :limit) 20))
         (entries (list-agent-activity
                   :agent-id agent-id
                   :activity-type activity-type
                   :limit limit))
         (agent-label (if (%agent-activity-blank-p agent-id) "all" agent-id))
         (type-label (if activity-type
                         (string-downcase
                          (symbol-name (%normalize-agent-activity-type activity-type)))
                         "all")))
    (%activity-box
     (cons
      (%activity-text
       (format nil "Agent activity (~D) [agent=~A type=~A]"
               (length entries)
               agent-label
               type-label))
      (if (null entries)
          (list (%activity-text "  No activity entries."))
          (mapcar (lambda (entry)
                    (%activity-text
                     (format nil "  [~A] ~A ~A ~A"
                             (%agent-activity-short-type
                              (agent-activity-entry-activity-type entry))
                             (agent-activity-entry-agent-id entry)
                             (%agent-activity-timestamp-string
                              (agent-activity-entry-timestamp entry))
                             (or (agent-activity-entry-description entry)
                                 ""))))
                  entries))))))

(eval-when (:load-toplevel :execute)
  (ensure-agent-activity-stream))
