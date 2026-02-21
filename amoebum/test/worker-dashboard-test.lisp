(in-package :amoebum/test)

;;; ============================================================
;;; I262: Worker Status TUI Dashboard — Smoke Tests
;;; ============================================================

(def-suite worker-dashboard-suite :in amoebum-suite)
(in-suite worker-dashboard-suite)

;;; --- Widget exists ---

(test worker-dashboard-widget-exists
  "worker-dashboard widget function is defined."
  (is (fboundp 'amoebum::worker-dashboard)))

;;; --- Status indicators ---

(test worker-status-indicators
  "Status indicators return recognizable strings."
  (is (stringp (amoebum::%worker-status-indicator :pending)))
  (is (stringp (amoebum::%worker-status-indicator :running)))
  (is (stringp (amoebum::%worker-status-indicator :completed)))
  (is (stringp (amoebum::%worker-status-indicator :failed)))
  (is (search "OK" (amoebum::%worker-status-indicator :completed)))
  (is (search "XX" (amoebum::%worker-status-indicator :failed))))

;;; --- Dashboard state ---

(test dashboard-state-struct-exists
  "worker-dashboard-state struct is defined."
  (is (fboundp 'amoebum::worker-dashboard-state-visible-p))
  (is (fboundp 'amoebum::worker-dashboard-state-selected-worker-id)))

(test ensure-dashboard-state
  "ensure-worker-dashboard-state creates state on demand."
  (let ((old-state amoebum::*worker-dashboard-state*))
    (unwind-protect
         (progn
           (setf amoebum::*worker-dashboard-state* nil)
           (let ((state (amoebum:ensure-worker-dashboard-state)))
             (is (not (null state)))
             (is (eq nil (amoebum::worker-dashboard-state-visible-p state)))))
      (setf amoebum::*worker-dashboard-state* old-state))))

;;; --- Visibility toggle ---

(test toggle-worker-dashboard
  "toggle-worker-dashboard flips visibility."
  (let ((old-state amoebum::*worker-dashboard-state*))
    (unwind-protect
         (progn
           (setf amoebum::*worker-dashboard-state* nil)
           (is (eq t (amoebum:toggle-worker-dashboard)))     ; nil -> t
           (is (amoebum:worker-dashboard-visible-p))
           (is (eq nil (amoebum:toggle-worker-dashboard)))   ; t -> nil
           (is (not (amoebum:worker-dashboard-visible-p))))
      (setf amoebum::*worker-dashboard-state* old-state))))

;;; --- Subscribe/unsubscribe ---

(test dashboard-subscribe-unsubscribe
  "Dashboard can subscribe and unsubscribe from event bus."
  (let ((old-state amoebum::*worker-dashboard-state*)
        (bus (amoebum:make-event-bus)))
    (unwind-protect
         (progn
           (setf amoebum::*worker-dashboard-state* nil)
           (let ((state (amoebum:worker-dashboard-subscribe :event-bus bus)))
             ;; Should have subscription IDs for all 6 worker event types
             (is (= 6 (length (amoebum::worker-dashboard-state-subscription-ids state))))
             ;; Unsubscribe
             (amoebum:worker-dashboard-unsubscribe :event-bus bus)
             (is (null (amoebum::worker-dashboard-state-subscription-ids state)))))
      (setf amoebum::*worker-dashboard-state* old-state))))

;;; --- Widget rendering ---

(test dashboard-renders-empty
  "Dashboard renders with no workers."
  (let ((old-sup amoebum:*worker-supervisor*))
    (unwind-protect
         (progn
           (setf amoebum:*worker-supervisor* nil)
           (amoebum:clear-workers)
           (let ((tree (amoebum::worker-dashboard '(:show-finished t :limit 10))))
             (is (typep tree 'ptui.ui.elements:ui-element))
             (is (eq :box (ptui.ui.elements:ui-element-type tree)))
             (is (listp (ptui.ui.elements:ui-element-children tree)))))
      (setf amoebum:*worker-supervisor* old-sup))))

;;; --- Status bar segment ---

(test worker-status-bar-segment
  "worker-status-bar-segment returns string."
  (let ((old-sup amoebum:*worker-supervisor*))
    (unwind-protect
         (progn
           (setf amoebum:*worker-supervisor* nil)
           (amoebum:clear-workers)
           (let ((segment (amoebum:worker-status-bar-segment)))
             (is (stringp segment))))
      (setf amoebum:*worker-supervisor* old-sup))))

;;; --- Select/drill-down ---

(test dashboard-select-worker
  "worker-dashboard-select sets selected worker ID."
  (let ((old-state amoebum::*worker-dashboard-state*))
    (unwind-protect
         (progn
           (setf amoebum::*worker-dashboard-state* nil)
           (amoebum:ensure-worker-dashboard-state)
           (amoebum:worker-dashboard-select "w-test-001")
           (is (equal "w-test-001"
                      (amoebum::worker-dashboard-state-selected-worker-id
                       amoebum::*worker-dashboard-state*))))
      (setf amoebum::*worker-dashboard-state* old-state))))

(test dashboard-selected-output-nil
  "worker-dashboard-selected-output returns nil when no selection."
  (let ((old-state amoebum::*worker-dashboard-state*))
    (unwind-protect
         (progn
           (setf amoebum::*worker-dashboard-state* nil)
           (is (null (amoebum:worker-dashboard-selected-output))))
      (setf amoebum::*worker-dashboard-state* old-state))))
