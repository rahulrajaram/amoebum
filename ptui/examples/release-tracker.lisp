(defpackage :ptui.examples.release-tracker
  (:use :cl)
  (:export #:run-release-tracker))

(in-package :ptui.examples.release-tracker)

(defun %format-column (title items)
  (format nil "~A~%~{ - ~A~%~}" title items))

(ptui.ui.panel:defpanel release-tracker-panel (queued active completed)
  (:data
    (queued-title (format nil "[queued] ~D" (length queued)) :deps (queued))
    (active-title (format nil "[active] ~D" (length active)) :deps (active))
    (done-title (format nil "[done] ~D" (length completed)) :deps (completed)))
  (:layout
    (:row
      (queued-col :flex 1
        (ptui.widgets.core:make-text-widget
         (%format-column queued-title queued)))
      (active-col :flex 1
        (ptui.widgets.core:make-text-widget
         (%format-column active-title active)))
      (done-col :flex 1
        (ptui.widgets.core:make-text-widget
         (%format-column done-title completed))))))

(ptui.ui.app:defapp release-tracker-app (:fps 8)
  (release-tracker-panel
   (list "schema-migrate" "search-index-refresh")
   (list "region-us-east rollout" "mobile-api canary")
   (list "billing patch" "sso token rotation" "cache warmup")))

(defun run-release-tracker ()
  (run-release-tracker-app))
