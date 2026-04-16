(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; NXT-355: Worktree conflict handoff registry and operator resolution state
;;;
;;; Keep handoff state ownership, room-status access, and resolution transitions
;;; behind a dedicated module boundary while preserving the :amoebum API.
;;; ---------------------------------------------------------------------------

(defstruct (worktree-conflict-handoff
            (:constructor %make-worktree-conflict-handoff
                (&key id
                      (status :pending)
                      (created-at 0)
                      (updated-at 0)
                      worktree
                      target-ref
                      preflight
                      agent-id
                      backend
                      task
                      result
                      resolution
                      negotiation-room-id
                      artifact-id
                      note)))
  (id nil :type (or null string))
  (status :pending :type keyword)
  (created-at 0 :type integer)
  (updated-at 0 :type integer)
  worktree
  (target-ref nil :type (or null string))
  preflight
  (agent-id nil :type (or null string))
  backend
  task
  result
  resolution
  (negotiation-room-id nil :type (or null string))
  (artifact-id nil :type (or null string))
  note)

(defparameter *worktree-conflict-handoffs* (make-hash-table :test #'equal)
  "Registry of worktree merge conflicts awaiting manual handling.")

(defparameter *worktree-conflict-handoff-sequence* 0)

(defparameter *worktree-conflict-handoff-lock*
  (bordeaux-threads:make-lock "amoebum-worktree-conflict-handoffs"))

(defparameter *worktree-negotiation-client* nil
  "Dedicated local SW4RM negotiation client for worktree-merge conflicts.")

(defmacro %with-worktree-conflict-handoff-lock (() &body body)
  `(bordeaux-threads:with-lock-held (*worktree-conflict-handoff-lock*)
     ,@body))

(defun %next-worktree-conflict-handoff-id ()
  (%with-worktree-conflict-handoff-lock ()
    (incf *worktree-conflict-handoff-sequence*)
    (format nil "worktree-handoff-~4,'0D"
            *worktree-conflict-handoff-sequence*)))

(defun %ensure-worktree-negotiation-client ()
  (or *worktree-negotiation-client*
      (setf *worktree-negotiation-client*
            (make-instance 'sw4rm-sdk:negotiation-room-client
                           :address "local://amoebum/worktree-negotiation"))))

(defun %make-worktree-conflict-resolution (handoff &key note)
  (let ((started-at (get-universal-time)))
    (list :status :active
          :owner :operator
          :started-at started-at
          :updated-at started-at
          :worktree (worktree-metadata-plist
                     (worktree-conflict-handoff-worktree handoff))
          :target-ref (worktree-conflict-handoff-target-ref handoff)
          :note (%normalize-worktree-string note))))

(defun %update-worktree-conflict-resolution (handoff status &key note)
  (let ((timestamp (get-universal-time))
        (resolution (or (%copy-worktree-data
                         (worktree-conflict-handoff-resolution handoff))
                        (%make-worktree-conflict-resolution handoff
                                                           :note note))))
    (setf (getf resolution :status) status
          (getf resolution :owner) :operator
          (getf resolution :updated-at) timestamp)
    (when (eq status :active)
      (setf (getf resolution :started-at)
            (or (getf resolution :started-at) timestamp)))
    (when (member status '(:resolved :abandoned))
      (setf (getf resolution :completed-at) timestamp))
    (when note
      (setf (getf resolution :note) (%normalize-worktree-string note)))
    resolution))

(defun %worktree-conflict-handoff-room-status (handoff)
  (let ((room-id (and handoff
                      (worktree-conflict-handoff-negotiation-room-id handoff))))
    (when room-id
      (ignore-errors
        (sw4rm-sdk:get-room-status (%ensure-worktree-negotiation-client)
                                   room-id)))))

(defun %worktree-conflict-handoff-snapshot (handoff &key include-room-status-p)
  (when handoff
    (let ((snapshot
            (list :handoff-id (worktree-conflict-handoff-id handoff)
                  :status (worktree-conflict-handoff-status handoff)
                  :created-at (worktree-conflict-handoff-created-at handoff)
                  :updated-at (worktree-conflict-handoff-updated-at handoff)
                  :worktree (worktree-metadata-plist
                             (worktree-conflict-handoff-worktree handoff))
                  :target-ref (worktree-conflict-handoff-target-ref handoff)
                  :preflight (%copy-worktree-data
                              (worktree-conflict-handoff-preflight handoff))
                  :agent-id (worktree-conflict-handoff-agent-id handoff)
                  :backend (worktree-conflict-handoff-backend handoff)
                  :task (worktree-conflict-handoff-task handoff)
                  :result (%copy-worktree-data
                           (worktree-conflict-handoff-result handoff))
                  :resolution (%copy-worktree-data
                               (worktree-conflict-handoff-resolution handoff))
                  :negotiation-room-id
                  (worktree-conflict-handoff-negotiation-room-id handoff)
                  :artifact-id (worktree-conflict-handoff-artifact-id handoff)
                  :note (worktree-conflict-handoff-note handoff))))
      (if include-room-status-p
          (append snapshot
                  (list :negotiation-status
                        (%worktree-conflict-handoff-room-status handoff)))
          snapshot))))

(defun %store-worktree-conflict-handoff! (handoff)
  (%with-worktree-conflict-handoff-lock ()
    (setf (gethash (worktree-conflict-handoff-id handoff)
                   *worktree-conflict-handoffs*)
          handoff))
  handoff)

(defun clear-worktree-conflict-handoffs ()
  (%with-worktree-conflict-handoff-lock ()
    (clrhash *worktree-conflict-handoffs*)
    (setf *worktree-negotiation-client* nil))
  t)

(defun list-worktree-conflict-handoffs ()
  (%with-worktree-conflict-handoff-lock ()
    (let ((handoffs '()))
      (maphash (lambda (_id handoff)
                 (declare (ignore _id))
                 (push (%worktree-conflict-handoff-snapshot handoff)
                       handoffs))
               *worktree-conflict-handoffs*)
      (sort handoffs #'> :key (lambda (snapshot)
                                (or (getf snapshot :created-at) 0))))))

(defun find-worktree-conflict-handoff (handoff-id &key include-room-status-p)
  (%with-worktree-conflict-handoff-lock ()
    (%worktree-conflict-handoff-snapshot
     (gethash (%normalize-worktree-string handoff-id)
              *worktree-conflict-handoffs*)
     :include-room-status-p include-room-status-p)))

(defun %update-worktree-conflict-handoff! (handoff-id updater)
  (%with-worktree-conflict-handoff-lock ()
    (let* ((resolved-id (%normalize-worktree-string handoff-id))
           (handoff (and resolved-id
                         (gethash resolved-id *worktree-conflict-handoffs*))))
      (unless handoff
        (error "Unknown worktree conflict handoff ~S." handoff-id))
      (funcall updater handoff)
      (setf (worktree-conflict-handoff-updated-at handoff)
            (get-universal-time))
      (%worktree-conflict-handoff-snapshot handoff
                                           :include-room-status-p t))))

(defun accept-worktree-conflict-handoff (handoff-id &key note)
  (%update-worktree-conflict-handoff!
   handoff-id
   (lambda (handoff)
     (setf (worktree-conflict-handoff-status handoff) :accepted
           (worktree-conflict-handoff-resolution handoff)
           (%make-worktree-conflict-resolution handoff :note note)
           (worktree-conflict-handoff-note handoff)
           (%normalize-worktree-string note)))))

(defun defer-worktree-conflict-handoff (handoff-id &key note)
  (%update-worktree-conflict-handoff!
   handoff-id
   (lambda (handoff)
     (setf (worktree-conflict-handoff-status handoff) :deferred
           (worktree-conflict-handoff-note handoff)
           (%normalize-worktree-string note)))))

(defun resolve-worktree-conflict-handoff (handoff-id &key note)
  (%update-worktree-conflict-handoff!
   handoff-id
   (lambda (handoff)
     (unless (eq (worktree-conflict-handoff-status handoff) :accepted)
       (error "Worktree conflict handoff ~A is not accepted." handoff-id))
     (setf (worktree-conflict-handoff-status handoff) :resolved
           (worktree-conflict-handoff-resolution handoff)
           (%update-worktree-conflict-resolution handoff :resolved :note note)
           (worktree-conflict-handoff-note handoff)
           (%normalize-worktree-string note)))))

(defun abandon-worktree-conflict-handoff (handoff-id &key note)
  (%update-worktree-conflict-handoff!
   handoff-id
   (lambda (handoff)
     (unless (eq (worktree-conflict-handoff-status handoff) :accepted)
       (error "Worktree conflict handoff ~A is not accepted." handoff-id))
     (setf (worktree-conflict-handoff-status handoff) :abandoned
           (worktree-conflict-handoff-resolution handoff)
           (%update-worktree-conflict-resolution handoff :abandoned :note note)
           (worktree-conflict-handoff-note handoff)
           (%normalize-worktree-string note)))))

(defun create-worktree-conflict-handoff (&key worktree
                                              target-ref
                                              preflight
                                              agent-id
                                              backend
                                              task
                                              result)
  (let* ((metadata (coerce-worktree-metadata :worktree worktree))
         (handoff-id (%next-worktree-conflict-handoff-id))
         (room-id (format nil "~A-room" handoff-id))
         (artifact-id (format nil "~A-artifact" handoff-id))
         (created-at (get-universal-time))
         (room-metadata (list :handoff-id handoff-id
                              :target-ref (%normalize-worktree-string target-ref)
                              :worktree (worktree-metadata-plist metadata)
                              :agent-id (%normalize-worktree-string agent-id)
                              :backend backend
                              :conflicts (%copy-worktree-data
                                          (getf preflight :conflicts))))
         (artifact (list :type :worktree-merge-conflict
                         :handoff-id handoff-id
                         :target-ref (%normalize-worktree-string target-ref)
                         :worktree (worktree-metadata-plist metadata)
                         :preflight (%copy-worktree-data preflight)
                         :task task
                         :result (%copy-worktree-data result))))
    (sw4rm-sdk:create-room (%ensure-worktree-negotiation-client)
                           room-id
                           :description "Amoebum worktree merge conflict"
                           :metadata room-metadata)
    (sw4rm-sdk:submit-artifact
     (%ensure-worktree-negotiation-client)
     (list :artifact-id artifact-id
           :negotiation-room-id room-id
           :proposer-id (or (%normalize-worktree-string agent-id)
                            "amoebum/worktree-merger")
           :artifact artifact
           :metadata room-metadata
           :requested-critics '()
           :aggregation-strategy :confidence-weighted))
    (%worktree-conflict-handoff-snapshot
     (%store-worktree-conflict-handoff!
      (%make-worktree-conflict-handoff
       :id handoff-id
       :status :pending
       :created-at created-at
       :updated-at created-at
       :worktree metadata
       :target-ref (%normalize-worktree-string target-ref)
       :preflight (%copy-worktree-data preflight)
       :agent-id (%normalize-worktree-string agent-id)
       :backend backend
       :task task
       :result (%copy-worktree-data result)
       :resolution nil
       :negotiation-room-id room-id
       :artifact-id artifact-id))
     :include-room-status-p t)))
