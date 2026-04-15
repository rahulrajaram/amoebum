(in-package :amoebum)

;;; ============================================================
;;; I262: Worker Status TUI Dashboard
;;;
;;; defwidget showing active and recent workers with status
;;; indicators, event bus subscription for live updates, and
;;; drill-down output view.
;;; ============================================================

;;; --- Status indicators ---

(defun %worker-status-indicator (status)
  "Return a text indicator for a worker status keyword."
  (case status
    (:pending   "[..]")
    (:running   "[>>]")
    (:completed "[OK]")
    (:failed    "[XX]")
    (:timeout   "[TO]")
    (:cancelled "[--]")
    (otherwise  "[??]")))

(defun %worker-elapsed-string (worker)
  "Return a human-readable elapsed time for WORKER."
  (let ((started (worker-record-started-at worker))
        (finished (worker-record-finished-at worker))
        (now (get-universal-time)))
    (cond
      ((and (plusp finished) (plusp started))
       (format nil "~Ds" (- finished started)))
      ((plusp started)
       (format nil "~Ds..." (- now started)))
      (t "—"))))

(defun %worker-output-tail (worker &optional (max-chars 60))
  "Return the last MAX-CHARS of a worker's output buffer."
  (let ((buf (worker-record-output-buffer worker)))
    (if (and buf (plusp (length buf)))
        (let ((trimmed (string-trim '(#\Newline #\Return #\Space) buf)))
          (if (> (length trimmed) max-chars)
              (concatenate 'string "..." (subseq trimmed (- (length trimmed) (- max-chars 3))))
              trimmed))
        "")))

(defun %worker-runtime-backend (worker)
  (case (worker-record-backend worker)
    (:swarm :swarm)
    (:in-process :local)
    (otherwise nil)))

(defun %worker-runtime-status-label (worker)
  (when (and (eq (worker-record-type worker) :agent)
             (worker-record-inner-id worker))
    (let* ((backend (%worker-runtime-backend worker))
           (agent (and backend
                       (find-runtime-agent (worker-record-inner-id worker)
                                           :backend backend)))
           (status (or (and agent (runtime-agent-status agent :backend backend))
                       (worker-record-status worker))))
      (format nil "[~A/~A]"
              (string-downcase (symbol-name (or backend :agent)))
              (string-downcase (symbol-name status))))))

(defun %worker-worktree-label (worker)
  (let* ((metadata (worker-record-worktree worker))
         (worktree-id (and metadata (worktree-metadata-id metadata))))
    (when worktree-id
      (let* ((inspection (and (worktree-metadata-path metadata)
                              (ignore-errors
                                (inspect-local-worktree :worktree metadata))))
             (status-fragment
               (and inspection
                    (getf inspection :abandoned-p)
                    (format nil " ~A/~A"
                            (string-downcase
                             (symbol-name
                              (or (getf inspection :lifecycle-state)
                                  :abandoned)))
                            (string-downcase
                             (symbol-name
                              (or (getf inspection :cleanup-classification)
                                  :unknown)))))))
        (format nil "[wt:~A~A]"
                worktree-id
                (or status-fragment ""))))))

(defun %worker-status-line (worker)
  (format nil "  ~A ~A~@[ ~A~]~@[ ~A~] ~A  ~A  ~A"
          (%worker-status-indicator (worker-record-status worker))
          (worker-record-id worker)
          (%worker-runtime-status-label worker)
          (%worker-worktree-label worker)
          (let ((label (worker-record-label worker)))
            (if (> (length label) 30)
                (concatenate 'string (subseq label 0 27) "...")
                label))
          (%worker-elapsed-string worker)
          (%worker-output-tail worker 40)))

(defun %active-agent-worker-runtime-counts ()
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (worker (worker-list :include-finished nil))
      (when (and (eq (worker-record-type worker) :agent)
                 (worker-record-inner-id worker))
        (let* ((backend (%worker-runtime-backend worker))
               (agent (and backend
                           (find-runtime-agent (worker-record-inner-id worker)
                                               :backend backend)))
          (status (or (and agent (runtime-agent-status agent :backend backend))
                           (worker-record-status worker))))
          (when backend
            (incf (gethash (list backend status) counts 0))))))
    (let ((entries '()))
      (maphash (lambda (key count)
                 (push (list (first key) (second key) count) entries))
               counts)
      (sort entries #'string<
            :key (lambda (entry)
                   (format nil "~A/~A"
                           (first entry)
                           (second entry)))))))

(defun %worker-runtime-summary-segment ()
  (let ((entries (%active-agent-worker-runtime-counts)))
    (when entries
      (format nil "A:~{~A~^, ~}"
              (mapcar (lambda (entry)
                        (format nil "~Dx~A/~A"
                                (third entry)
                                (string-downcase (symbol-name (first entry)))
                                (string-downcase (symbol-name (second entry)))))
                      entries)))))

;;; --- Helper to create UI elements ---

(defun %dash-text (content)
  "Create a text UI element for the dashboard."
  (ptui.ui.elements:make-element :text :props (list :content content)))

(defun %dash-box (children &key (direction :vertical))
  "Create a box UI element for the dashboard."
  (ptui.ui.elements:make-element :box
    :props (list :direction direction)
    :children children))

;;; --- Dashboard widget ---

(ptui.widgets.defwidget:defwidget worker-dashboard (state)
  (:memoize :equal)
  (let* ((show-finished (getf state :show-finished t))
         (limit (or (getf state :limit) 20))
         (workers (worker-list :include-finished show-finished))
         (visible (subseq workers 0 (min limit (length workers))))
         (active (count-if (lambda (w)
                             (not (%worker-terminal-status-p
                                   (worker-record-status w))))
                           workers)))
    (%dash-box
     (cons
      (%dash-text (format nil "Workers — ~D active, ~D total"
                          active (length workers)))
      (if (null visible)
          (list (%dash-text "  No workers."))
          (mapcar
           (lambda (worker)
             (%dash-text (%worker-status-line worker)))
           visible))))))

;;; --- Dashboard state for event-driven updates ---

(defstruct (worker-dashboard-state
            (:constructor %make-worker-dashboard-state
                (&key subscription-ids visible-p selected-worker-id)))
  (subscription-ids nil :type list)
  (visible-p nil :type boolean)
  (selected-worker-id nil :type (or null string)))

(defvar *worker-dashboard-state* nil)

(defun ensure-worker-dashboard-state ()
  (or *worker-dashboard-state*
      (setf *worker-dashboard-state*
            (%make-worker-dashboard-state :visible-p nil))))

(defun worker-dashboard-subscribe (&key (event-bus (current-event-bus)))
  "Subscribe the worker dashboard to worker events for live updates."
  (let ((state (ensure-worker-dashboard-state))
        (sub-ids '()))
    ;; Subscribe to all worker event types
    (dolist (evt-type (list +event-type-worker-spawned+
                            +event-type-worker-started+
                            +event-type-worker-completed+
                            +event-type-worker-failed+
                            +event-type-worker-cancelled+
                            +event-type-worker-retry+))
      (push (subscribe event-bus evt-type
                       (lambda (event)
                         (declare (ignore event))
                         ;; In a real TUI this would trigger a re-render
                         nil))
            sub-ids))
    (setf (worker-dashboard-state-subscription-ids state) sub-ids)
    state))

(defun worker-dashboard-unsubscribe (&key (event-bus (current-event-bus)))
  "Unsubscribe the worker dashboard from events."
  (let ((state *worker-dashboard-state*))
    (when state
      (dolist (sub-id (worker-dashboard-state-subscription-ids state))
        (ignore-errors (unsubscribe event-bus sub-id)))
      (setf (worker-dashboard-state-subscription-ids state) nil))
    state))

(defun worker-dashboard-visible-p ()
  "Return whether the worker dashboard is visible."
  (let ((state *worker-dashboard-state*))
    (and state (worker-dashboard-state-visible-p state))))

(defun toggle-worker-dashboard ()
  "Toggle worker dashboard visibility."
  (let ((state (ensure-worker-dashboard-state)))
    (setf (worker-dashboard-state-visible-p state)
          (not (worker-dashboard-state-visible-p state)))
    (worker-dashboard-state-visible-p state)))

;;; --- Drill-down: selected worker output ---

(defun worker-dashboard-select (worker-id)
  "Select a worker for drill-down output view."
  (let ((state (ensure-worker-dashboard-state)))
    (setf (worker-dashboard-state-selected-worker-id state) worker-id)))

(defun worker-dashboard-selected-output ()
  "Return the full output of the currently selected worker."
  (let* ((state *worker-dashboard-state*)
         (wid (when state (worker-dashboard-state-selected-worker-id state))))
    (when wid
      (worker-output wid))))

;;; --- Worktree handoff dashboard ---

(defstruct (worktree-handoff-dashboard-state
            (:constructor %make-worktree-handoff-dashboard-state
                (&key visible-p selected-handoff-id selected-action last-message)))
  (visible-p nil :type boolean)
  (selected-handoff-id nil :type (or null string))
  (selected-action nil :type (or null keyword))
  (last-message nil :type (or null string)))

(defvar *worktree-handoff-dashboard-state* nil)

(defun ensure-worktree-handoff-dashboard-state ()
  (or *worktree-handoff-dashboard-state*
      (setf *worktree-handoff-dashboard-state*
            (%make-worktree-handoff-dashboard-state :visible-p nil))))

(defun %worktree-handoff-open-p (snapshot)
  (member (getf snapshot :status) '(:pending :deferred :accepted) :test #'eq))

(defun %worktree-handoff-dashboard-snapshots ()
  (remove-if-not #'%worktree-handoff-open-p
                 (list-worktree-conflict-handoffs)))

(defun %worktree-handoff-dashboard-find (handoff-id snapshots)
  (find handoff-id
        snapshots
        :key (lambda (snapshot)
               (getf snapshot :handoff-id))
        :test #'equal))

(defun %worktree-handoff-dashboard-actions (snapshot)
  (case (getf snapshot :status)
    ((:pending :deferred) '(:accept :defer))
    (:accepted '(:resolve))
    (otherwise '())))

(defun %sync-worktree-handoff-dashboard-state (&optional (state (ensure-worktree-handoff-dashboard-state)))
  (let* ((snapshots (%worktree-handoff-dashboard-snapshots))
         (selected
           (or (%worktree-handoff-dashboard-find
                (worktree-handoff-dashboard-state-selected-handoff-id state)
                snapshots)
               (first snapshots)))
         (actions (%worktree-handoff-dashboard-actions selected))
         (selected-action
           (and actions
                (if (member (worktree-handoff-dashboard-state-selected-action state)
                            actions
                            :test #'eq)
                    (worktree-handoff-dashboard-state-selected-action state)
                    (first actions)))))
    (setf (worktree-handoff-dashboard-state-selected-handoff-id state)
          (and selected (getf selected :handoff-id))
          (worktree-handoff-dashboard-state-selected-action state)
          selected-action)
    state))

(defun worktree-handoff-dashboard-visible-p ()
  (let ((state *worktree-handoff-dashboard-state*))
    (and state
         (worktree-handoff-dashboard-state-visible-p state))))

(defun toggle-worktree-handoff-dashboard (&optional (visible-p nil visible-p-supplied-p))
  (let ((state (ensure-worktree-handoff-dashboard-state)))
    (setf (worktree-handoff-dashboard-state-visible-p state)
          (if visible-p-supplied-p
              (not (null visible-p))
              (not (worktree-handoff-dashboard-state-visible-p state))))
    (%sync-worktree-handoff-dashboard-state state)
    (worktree-handoff-dashboard-state-visible-p state)))

(defun dismiss-worktree-handoff-dashboard ()
  (toggle-worktree-handoff-dashboard nil))

(defun %worktree-handoff-dashboard-selected-snapshot (&optional (state (ensure-worktree-handoff-dashboard-state)))
  (let* ((synced (%sync-worktree-handoff-dashboard-state state))
         (snapshots (%worktree-handoff-dashboard-snapshots)))
    (%worktree-handoff-dashboard-find
     (worktree-handoff-dashboard-state-selected-handoff-id synced)
     snapshots)))

(defun worktree-handoff-dashboard-move-selection (delta)
  (let* ((state (%sync-worktree-handoff-dashboard-state))
         (snapshots (%worktree-handoff-dashboard-snapshots))
         (count (length snapshots)))
    (when (plusp count)
      (let* ((selected-id (worktree-handoff-dashboard-state-selected-handoff-id state))
             (current-index
               (or (position selected-id
                             snapshots
                             :key (lambda (snapshot)
                                    (getf snapshot :handoff-id))
                             :test #'equal)
                   0))
             (next-index (mod (+ current-index delta) count))
             (snapshot (nth next-index snapshots)))
        (setf (worktree-handoff-dashboard-state-selected-handoff-id state)
              (getf snapshot :handoff-id)
              (worktree-handoff-dashboard-state-selected-action state)
              (first (%worktree-handoff-dashboard-actions snapshot)))
        t))))

(defun worktree-handoff-dashboard-cycle-action (delta)
  (let* ((state (%sync-worktree-handoff-dashboard-state))
         (snapshot (%worktree-handoff-dashboard-selected-snapshot state))
         (actions (%worktree-handoff-dashboard-actions snapshot))
         (count (length actions)))
    (when (plusp count)
      (let* ((current
               (or (position (worktree-handoff-dashboard-state-selected-action state)
                             actions
                             :test #'eq)
                   0))
             (next-index (mod (+ current delta) count)))
        (setf (worktree-handoff-dashboard-state-selected-action state)
              (nth next-index actions))
        t))))

(defun %worktree-handoff-action-prefix (action)
  (case action
    (:accept "Accepted")
    (:defer "Deferred")
    (:resolve "Resolved")
    (otherwise "Updated")))

(defun worktree-handoff-dashboard-apply-selected-action ()
  (let* ((state (%sync-worktree-handoff-dashboard-state))
         (snapshot (%worktree-handoff-dashboard-selected-snapshot state))
         (handoff-id (and snapshot (getf snapshot :handoff-id)))
         (action (worktree-handoff-dashboard-state-selected-action state)))
    (when (and handoff-id action)
      (let ((message
              (handler-case
                  (progn
                    (case action
                      (:accept (accept-worktree-conflict-handoff handoff-id))
                      (:defer (defer-worktree-conflict-handoff handoff-id))
                      (:resolve (resolve-worktree-conflict-handoff handoff-id))
                      (otherwise
                       (error "Unsupported worktree handoff action ~S." action)))
                    (format nil "~A worktree handoff ~A."
                            (%worktree-handoff-action-prefix action)
                            handoff-id))
                (error (condition)
                  (format nil "Failed to update worktree handoff ~A: ~A"
                          handoff-id
                          condition)))))
        (setf (worktree-handoff-dashboard-state-last-message state)
              message)
        (%sync-worktree-handoff-dashboard-state state)
        message))))

(defun %worktree-handoff-action-label (action selected-p)
  (let ((label (string-downcase (symbol-name action))))
    (if selected-p
        (format nil "[~A]" label)
        label)))

(defun %worktree-handoff-row-lines (snapshot state)
  (let* ((handoff-id (or (getf snapshot :handoff-id) "?"))
         (selected-p
           (equal handoff-id
                  (worktree-handoff-dashboard-state-selected-handoff-id state)))
         (status (string-downcase
                  (symbol-name (or (getf snapshot :status) :pending))))
         (worktree (getf snapshot :worktree))
         (worktree-id (or (getf worktree :id) "?"))
         (branch (getf worktree :branch))
         (target-ref (or (getf snapshot :target-ref) "?"))
         (conflicts (getf (getf snapshot :preflight) :conflicts))
         (actions (%worktree-handoff-dashboard-actions snapshot)))
    (append
     (list
      (format nil "~A ~A | ~A | wt ~A~@[ (~A)~] -> ~A"
              (if selected-p ">" " ")
              handoff-id
              status
              worktree-id
              branch
              target-ref))
     (when selected-p
       (append
        (when conflicts
          (list (format nil "    conflicts: ~{~A~^, ~}" conflicts)))
        (when actions
          (list (format nil "    actions: ~{~A~^  ~}"
                        (mapcar (lambda (action)
                                  (%worktree-handoff-action-label
                                   action
                                   (eq action
                                       (worktree-handoff-dashboard-state-selected-action state))))
                                actions))))
        (when (getf snapshot :note)
          (list (format nil "    note: ~A" (getf snapshot :note)))))))))

(ptui.widgets.defwidget:defwidget worktree-handoff-dashboard (state)
  (let* ((dashboard-state (%sync-worktree-handoff-dashboard-state))
         (limit (or (getf state :limit) 4))
         (snapshots (%worktree-handoff-dashboard-snapshots))
         (visible (subseq snapshots 0 (min limit (length snapshots))))
         (last-message (worktree-handoff-dashboard-state-last-message dashboard-state)))
    (%dash-box
     (append
      (list (%dash-text (format nil "Worktree handoffs — ~D open"
                                (length snapshots))))
      (when (and (stringp last-message)
                 (plusp (length last-message)))
        (list (%dash-text (format nil "  ~A" last-message))))
      (if (null visible)
          (list (%dash-text "  No open worktree handoffs."))
          (mapcan (lambda (snapshot)
                    (mapcar #'%dash-text
                            (%worktree-handoff-row-lines
                             snapshot
                             dashboard-state)))
                  visible))))))

;;; --- Status bar segment ---

(defun worker-status-bar-segment ()
  "Return a status bar segment string for the worker system."
  (let ((active (active-worker-count))
        (runtime-summary (%worker-runtime-summary-segment)))
    (cond
      ((and (zerop active) (null runtime-summary))
       "")
      ((null runtime-summary)
       (format nil "W:~D" active))
      ((zerop active)
       runtime-summary)
      (t
       (format nil "W:~D ~A" active runtime-summary)))))
