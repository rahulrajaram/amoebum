(in-package :cl-user)

(:ptui
 (:defpackage :ptui.examples.golden.g3-invalid
  (:use :cl)
  (:export #:broken-app))
 (:in-package :ptui.examples.golden.g3-invalid)
 (:pattern project-tree (:node-id "root"))
 (:app broken-app (:fps 4)
  (ptui.widgets.core:make-text-widget "This should never load.")))
