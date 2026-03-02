(in-package :cl-user)

(:ptui
 (:defpackage :ptui.examples.golden.g1-status-strip
  (:use :cl)
  (:export #:status-strip-app))
 (:in-package :ptui.examples.golden.g1-status-strip)
 (:panel status-strip-panel (title status-line)
  (:layout
   (:column
    (header :fixed 1
     (ptui.widgets.core:make-text-widget title))
    (body :flex 1
     (ptui.widgets.core:make-text-widget
      "Golden packet G1 baseline screen."))
    (footer :fixed 1
     (ptui.widgets.core:make-text-widget status-line)))))
 (:app status-strip-app (:fps 8)
  (status-strip-panel
   "G1 STATUS STRIP"
   "[ok] q quit | arrows navigate")))
