(in-package :amoebum/test)

;;; ============================================================
;;; I262: Worker Status TUI Dashboard — Smoke Tests
;;; ============================================================

(def-suite worker-dashboard-suite :in amoebum-suite)
(in-suite worker-dashboard-suite)

(defun %ui-element-content (element)
  (getf (ptui.ui.elements:ui-element-props element) :content))

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
           (is (eq t (amoebum.workers:toggle-worker-dashboard)))     ; nil -> t
           (is (amoebum.workers:worker-dashboard-visible-p))
           (is (eq nil (amoebum.workers:toggle-worker-dashboard)))   ; t -> nil
           (is (not (amoebum.workers:worker-dashboard-visible-p))))
      (setf amoebum::*worker-dashboard-state* old-state))))

;;; --- Subscribe/unsubscribe ---

(test dashboard-subscribe-unsubscribe
  "Dashboard can subscribe and unsubscribe from event bus."
  (let ((old-state amoebum::*worker-dashboard-state*)
        (bus (amoebum:make-event-bus)))
    (unwind-protect
         (progn
           (setf amoebum::*worker-dashboard-state* nil)
           (let ((state (amoebum.workers:worker-dashboard-subscribe :event-bus bus)))
             ;; Should have subscription IDs for all 6 worker event types
             (is (= 6 (length (amoebum::worker-dashboard-state-subscription-ids state))))
             ;; Unsubscribe
             (amoebum.workers:worker-dashboard-unsubscribe :event-bus bus)
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

(test dashboard-renders-runtime-agent-status-labels
  "Agent-backed rows should show unified local/SW4RM runtime status labels."
  (let ((old-sup amoebum:*worker-supervisor*)
        (old-agent-registry amoebum::*agent-registry*)
        (old-swarm-registry amoebum::*swarm-registry*))
    (unwind-protect
         (progn
           (setf amoebum:*worker-supervisor* nil
                 amoebum::*agent-registry* (make-hash-table :test #'equal)
                 amoebum::*swarm-registry* (make-hash-table :test #'equal))
           (amoebum:clear-workers)
           (setf (gethash "task-0001" amoebum::*agent-registry*)
                 (amoebum::%make-agent-record
                  :id "task-0001"
                  :type :task
                  :task "local task"
                  :status :running))
           (setf (gethash "swarm-1" amoebum::*swarm-registry*)
                 (amoebum:make-swarm-agent
                  :id "swarm-1"
                  :task "swarm task"
                  :status :completed))
           (amoebum::%store-worker
            (amoebum::%make-worker-record
             :id "w-agent-local" :type :agent :label "Local worker row"
             :status :running :created-at 1
             :backend :in-process :inner-id "task-0001"
             :worktree (amoebum:make-worktree-metadata
                        :id "wt-local-1"
                        :branch "sw4rm/demo/local-1")))
           (amoebum::%store-worker
            (amoebum::%make-worker-record
             :id "w-agent-swarm" :type :agent :label "Swarm worker row"
             :status :completed :created-at 2
             :backend :swarm :inner-id "swarm-1"))
           (let ((local-line (amoebum::%worker-status-line
                              (find "w-agent-local"
                                    (amoebum.workers:worker-list)
                                    :key #'amoebum.workers:worker-record-id
                                    :test #'equal)))
                 (swarm-line (amoebum::%worker-status-line
                              (find "w-agent-swarm"
                                    (amoebum.workers:worker-list)
                                    :key #'amoebum.workers:worker-record-id
                                    :test #'equal))))
             (is (search "[local/running]" local-line :test #'char-equal))
             (is (search "[wt:wt-local-1]" local-line :test #'char-equal))
             (is (search "[swarm/completed]" swarm-line :test #'char-equal))))
      (amoebum:clear-workers)
      (setf amoebum:*worker-supervisor* old-sup
            amoebum::*agent-registry* old-agent-registry
            amoebum::*swarm-registry* old-swarm-registry))))

;;; --- Status bar segment ---

(test worker-status-bar-segment
  "worker-status-bar-segment returns string."
  (let ((old-sup amoebum:*worker-supervisor*))
    (unwind-protect
         (progn
           (setf amoebum:*worker-supervisor* nil)
           (amoebum:clear-workers)
           (let ((segment (amoebum.workers:worker-status-bar-segment)))
             (is (stringp segment))))
      (setf amoebum:*worker-supervisor* old-sup))))

(test worker-status-bar-segment-includes-runtime-agent-summary
  "worker-status-bar-segment should expose unified runtime-agent status for active agent workers."
  (let ((old-sup amoebum:*worker-supervisor*)
        (old-agent-registry amoebum::*agent-registry*))
    (unwind-protect
         (progn
           (setf amoebum:*worker-supervisor* nil
                 amoebum::*agent-registry* (make-hash-table :test #'equal))
           (amoebum:clear-workers)
           (setf (gethash "task-0001" amoebum::*agent-registry*)
                 (amoebum::%make-agent-record
                  :id "task-0001"
                  :type :task
                  :task "local status"
                  :status :running))
           (amoebum::%store-worker
            (amoebum::%make-worker-record
             :id "w-agent-local" :type :agent :label "Local worker"
             :status :running :created-at 1
             :backend :in-process :inner-id "task-0001"))
           (let ((segment (amoebum.workers:worker-status-bar-segment)))
             (is (search "W:1" segment :test #'char-equal))
             (is (search "A:1xlocal/running" (remove #\Space segment) :test #'char-equal))))
      (amoebum:clear-workers)
      (setf amoebum:*worker-supervisor* old-sup
            amoebum::*agent-registry* old-agent-registry))))

;;; --- Select/drill-down ---

(test dashboard-select-worker
  "worker-dashboard-select sets selected worker ID."
  (let ((old-state amoebum::*worker-dashboard-state*))
    (unwind-protect
         (progn
           (setf amoebum::*worker-dashboard-state* nil)
           (amoebum:ensure-worker-dashboard-state)
           (amoebum.workers:worker-dashboard-select "w-test-001")
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
           (is (null (amoebum.workers:worker-dashboard-selected-output))))
      (setf amoebum::*worker-dashboard-state* old-state))))

(test worktree-handoff-dashboard-toggle
  "The worktree handoff dashboard visibility toggle should be controllable."
  (let ((old-state amoebum::*worktree-handoff-dashboard-state*))
    (unwind-protect
         (progn
           (setf amoebum::*worktree-handoff-dashboard-state* nil)
           (is-false (amoebum::worktree-handoff-dashboard-visible-p))
           (is-true (amoebum::toggle-worktree-handoff-dashboard t))
           (is-true (amoebum::worktree-handoff-dashboard-visible-p))
           (is-false (amoebum::toggle-worktree-handoff-dashboard nil))
           (is-false (amoebum::worktree-handoff-dashboard-visible-p)))
      (setf amoebum::*worktree-handoff-dashboard-state* old-state))))

(test worktree-handoff-dashboard-renders-open-conflicts
  "The worktree handoff dashboard should surface target refs, conflicts, and available actions."
  (let ((old-state amoebum::*worktree-handoff-dashboard-state*))
    (unwind-protect
         (progn
           (setf amoebum::*worktree-handoff-dashboard-state* nil)
           (amoebum:clear-worktree-conflict-handoffs)
           (let* ((snapshot (amoebum:create-worktree-conflict-handoff
                             :worktree (amoebum:make-worktree-metadata
                                        :id "wt-dashboard-handoff"
                                        :branch "sw4rm/dashboard/node"
                                        :path "/tmp/wt-dashboard-handoff/")
                             :target-ref "sw4rm/dashboard"
                             :preflight '(:status :conflict
                                          :conflicts ("README.md" "guide.md")
                                          :conflict-kind :file-overlap)
                             :agent-id "swarm-dashboard"
                             :backend :swarm
                             :task "dashboard handoff"
                             :result '(:status :completed)))
                  (handoff-id (getf snapshot :handoff-id)))
             (amoebum::toggle-worktree-handoff-dashboard t)
             (let* ((tree (amoebum::worktree-handoff-dashboard '(:limit 4)))
                    (children (ptui.ui.elements:ui-element-children tree))
                    (lines (mapcar #'%ui-element-content children)))
               (is (some (lambda (line)
                           (and (stringp line)
                                (search "Worktree handoffs" line :test #'char-equal)))
                         lines))
               (is (some (lambda (line)
                           (and (stringp line)
                                (search handoff-id line :test #'char-equal)))
                         lines))
               (is (some (lambda (line)
                           (and (stringp line)
                                (search "README.md, guide.md" line :test #'char-equal)))
                         lines))
               (is (some (lambda (line)
                           (and (stringp line)
                                (search "[accept]" line :test #'char-equal)))
                         lines))
               (is (some (lambda (line)
                           (and (stringp line)
                                (search "sw4rm/dashboard" line :test #'char-equal)))
                         lines)))))
      (amoebum:clear-worktree-conflict-handoffs)
      (setf amoebum::*worktree-handoff-dashboard-state* old-state))))

(test worktree-handoff-dashboard-applies-selected-action
  "The selected worktree handoff action should drive the existing accept and resolve lifecycle."
  (let ((old-state amoebum::*worktree-handoff-dashboard-state*))
    (unwind-protect
         (progn
           (setf amoebum::*worktree-handoff-dashboard-state* nil)
           (amoebum:clear-worktree-conflict-handoffs)
           (let* ((snapshot (amoebum:create-worktree-conflict-handoff
                             :worktree (amoebum:make-worktree-metadata
                                        :id "wt-dashboard-apply"
                                        :branch "sw4rm/dashboard/apply"
                                        :path "/tmp/wt-dashboard-apply/")
                             :target-ref "sw4rm/dashboard"
                             :preflight '(:status :conflict
                                          :conflicts ("status.md")
                                          :conflict-kind :file-overlap)
                             :agent-id "swarm-dashboard"
                             :backend :swarm
                             :task "dashboard apply"
                             :result '(:status :completed)))
                  (handoff-id (getf snapshot :handoff-id)))
             (amoebum::toggle-worktree-handoff-dashboard t)
             (let ((accept-message
                     (amoebum::worktree-handoff-dashboard-apply-selected-action)))
               (is (search "Accepted worktree handoff" accept-message))
               (let ((accepted (amoebum:find-worktree-conflict-handoff handoff-id)))
                 (is (eq :accepted (getf accepted :status)))))
             (let ((resolve-message
                     (amoebum::worktree-handoff-dashboard-apply-selected-action)))
               (is (search "Resolved worktree handoff" resolve-message))
               (let ((resolved (amoebum:find-worktree-conflict-handoff handoff-id)))
                 (is (eq :resolved (getf resolved :status)))))))
      (amoebum:clear-worktree-conflict-handoffs)
      (setf amoebum::*worktree-handoff-dashboard-state* old-state))))
