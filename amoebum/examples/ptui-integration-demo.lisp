;;; PTUI Integration Demo for Amoebum
;;; Demonstrates scroll utilities, layouts, and behavior config

(in-package :amoebum)

(defun demo-scroll ()
  "Demo scroll utilities."
  (format t "~%=== Scroll Utilities ===~%")
  (format t "Max scroll (100 lines, 24px viewport): ~D~%"
          (max-scroll-offset 100 24))
  (format t "Page step: ~D~%" (scroll-page-step 24))
  (format t "Clamp 150: ~D~%" (clamp-scroll-offset 150 100 24))
  (multiple-value-bind (action delta) (key-scroll-action :pgup :viewport-height 24)
    (format t "Page Up: ~A ~A~%" action delta)))

(defun demo-layout ()
  "Demo layout parsing."
  (format t "~%=== Layout Parsing ===~%")
  (let ((layout (make-default-yaml-layout)))
    (format t "Direction: ~A~%" (yaml-layout-direction layout))
    (format t "Children: ~D~%" (length (yaml-layout-children layout)))
    (dolist (c (yaml-layout-children layout))
      (format t "  ~A: height=~A~%"
              (yaml-layout-child-name c)
              (yaml-layout-child-height c)))))

(defun run-demos ()
  "Run all demos."
  (format t "~%PTUI Integration Demo~%")
  (demo-scroll)
  (demo-layout)
  (format t "~%Done!~%"))

(run-demos)
