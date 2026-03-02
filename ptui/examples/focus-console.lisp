(defpackage :ptui.examples.focus-console
  (:use :cl)
  (:export #:run-focus-console))

(in-package :ptui.examples.focus-console)

(defun %render-focus-task (task index selectedp)
  (declare (ignore index))
  (ptui.widgets.core:make-text-widget
   (if selectedp
       (format nil ">> ~A" task)
       (format nil "   ~A" task))))

(ptui.ui.panel:defpanel focus-console-panel (session-name tasks)
  (:state
    (selected-task 0 :type fixnum)
    (minutes-elapsed 17 :type fixnum)
    (break-mode nil :type boolean))
  (:data
    (session-label (if break-mode
                       (format nil "~A | break window" session-name)
                       (format nil "~A | focus sprint" session-name))
      :deps (session-name break-mode))
    (task-count (length tasks) :deps (tasks)))
  (:layout
    (:column
      (hero :fixed 1
        (ptui.widgets.core:make-text-widget
         (format nil "FOCUS CONSOLE :: ~A" session-label)))
      (timer :fixed 1
        (ptui.widgets.core:make-text-widget
         (format nil "elapsed: ~D min" minutes-elapsed)))
      (tasks-region :flex 1
        (ptui.views:list-view tasks #'%render-focus-task 8 nil selected-task nil))
      (footer :fixed 1
        (ptui.views:status-bar
         (list :left (format nil "tasks:~D" task-count)
               :center "keys: up/down, enter"
               :right (if break-mode "mode:break" "mode:focus"))
         nil nil))))
  (:keys
    (:up (funcall set-selected-task (max 0 (1- selected-task))))
    (:down (funcall set-selected-task
                    (if (plusp task-count)
                        (min (1- task-count) (1+ selected-task))
                        0)))
    (:enter (funcall set-break-mode (not break-mode)))))

(ptui.ui.app:defapp focus-console-app (:fps 8)
  (focus-console-panel
   "quiet-room"
   (list "Draft rollout memo"
         "Review benchmark deltas"
         "Trim cold-start latency budget"
         "Prepare release notes")))

(defun run-focus-console ()
  (run-focus-console-app))
