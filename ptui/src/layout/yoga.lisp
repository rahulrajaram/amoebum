(defpackage :ptui.layout.yoga
  (:use :cl)
  (:export
   #:layout-engine-name
   #:compute-layout))

(in-package :ptui.layout.yoga)

(defun layout-engine-name ()
  :yoga-adapter-stub)

(defun compute-layout (root &key width height (x 0) (y 0))
  "Feature-gated Yoga boundary adapter.
Current implementation delegates to ptui/layout until Yoga bindings land."
  (ptui.layout:compute-layout root :width width :height height :x x :y y))
