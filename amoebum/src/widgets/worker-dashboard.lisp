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
             (%dash-text
              (format nil "  ~A ~A ~A  ~A  ~A"
                      (%worker-status-indicator (worker-record-status worker))
                      (worker-record-id worker)
                      (let ((label (worker-record-label worker)))
                        (if (> (length label) 30)
                            (concatenate 'string (subseq label 0 27) "...")
                            label))
                      (%worker-elapsed-string worker)
                      (%worker-output-tail worker 40))))
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

;;; --- Status bar segment ---

(defun worker-status-bar-segment ()
  "Return a status bar segment string for the worker system."
  (let ((active (active-worker-count)))
    (if (plusp active)
        (format nil "W:~D" active)
        "")))
