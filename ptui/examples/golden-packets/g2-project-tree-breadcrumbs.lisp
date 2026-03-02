(in-package :cl-user)

(:ptui
 (:defpackage :ptui.examples.golden.g2-project-tree
  (:use :cl)
  (:export #:project-tree-app))
 (:in-package :ptui.examples.golden.g2-project-tree)
 (:widget breadcrumb-line (segments)
  (ptui.widgets.core:make-text-widget
   (format nil "Path: ~{~A~^ / ~}" segments)))
 (:panel project-tree-panel (segments rows selected-index)
  (:layout
   (:column
    (breadcrumbs :fixed 1
     (breadcrumb-line segments))
    (tree :flex 1
     (ptui.views:list-view
      rows
      (lambda (entry index selected-p)
        (declare (ignore index))
        (ptui.widgets.core:make-text-widget
         (format nil "~A ~A"
                 (if selected-p ">" " ")
                 entry)))
      10 nil selected-index nil))
    (footer :fixed 1
     (ptui.widgets.core:make-text-widget
      "[j/k] move  [enter] open  [q] quit")))))
 (:app project-tree-app (:fps 8)
  (project-tree-panel
   (list "amoebum" "ptui" "src")
   (list "core/" "ui/" "widgets/" "definition-loader.lisp")
   2)))
