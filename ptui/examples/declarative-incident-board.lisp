(:ptui
 (defpackage :ptui.examples.declarative.incident-board
  (:use :cl)
  (:export #:incident-board-app))
 (in-package :ptui.examples.declarative.incident-board)
 (:panel incident-board-panel (service-lines incident-count)
  (:layout
   (:column
    (header :fixed 1
     (ptui.widgets.core:make-text-widget
      (format nil "INCIDENT BOARD | active=~D" incident-count)))
    (body :flex 1
     (ptui.widgets.core:make-text-widget
      (format nil "~{~A~%~}" service-lines)))
    (footer :fixed 1
     (ptui.widgets.core:make-text-widget
      "description-first declarative PTUI source")))))
 (:app incident-board-app (:fps 8)
  (incident-board-panel
   (list "api: healthy"
         "worker: degraded"
         "db: healthy")
   1)))
