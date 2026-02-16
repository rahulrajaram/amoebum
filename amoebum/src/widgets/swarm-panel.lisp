(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; Swarm Panel Widget (I93)
;;;
;;; defwidget showing active swarm agents and their status.
;;; ---------------------------------------------------------------------------

(ptui.widgets.defwidget:defwidget swarm-panel (state)
  (:memoize :equal)
  (let* ((status-filter (getf state :status nil))
         (agents (list-swarm-agents :status status-filter))
         (limit (or (getf state :limit) 20))
         (visible (subseq agents 0 (min limit (length agents)))))
    (list
     :type :box
     :direction :vertical
     :children
     (cons
      (list :type :text
            :content (format nil "Swarm Agents (~A total, showing ~A)"
                             (length agents) (length visible)))
      (if (null visible)
          (list (list :type :text :content "  No agents."))
          (mapcar
           (lambda (agent)
             (list :type :text
                   :content (format nil "  ~A [~A] ~A"
                                    (swarm-agent-id agent)
                                    (swarm-agent-status agent)
                                    (let ((task (swarm-agent-task agent)))
                                      (if (> (length task) 60)
                                          (concatenate 'string (subseq task 0 57) "...")
                                          task)))))
           visible))))))
