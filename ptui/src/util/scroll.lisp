(defpackage :ptui.util.scroll
  (:use :cl)
  (:export #:scroll-page-step
           #:max-scroll-offset
           #:clamp-scroll-offset
           #:render-scroll-offset
           #:apply-scroll-delta
           #:key-scroll-action))

(in-package :ptui.util.scroll)

(defun scroll-page-step (viewport-height)
  "Return the default page-scroll step for VIEWPORT-HEIGHT."
  (max 1 (1- (max 1 (or viewport-height 1)))))

(defun max-scroll-offset (content-height viewport-height)
  "Return the largest valid scroll offset for CONTENT-HEIGHT and VIEWPORT-HEIGHT."
  (max 0 (- (max 0 (or content-height 0))
            (max 1 (or viewport-height 1)))))

(defun clamp-scroll-offset (offset content-height viewport-height)
  "Clamp OFFSET into the valid scroll range for CONTENT-HEIGHT and VIEWPORT-HEIGHT."
  (max 0
       (min (max-scroll-offset content-height viewport-height)
            (or offset 0))))

(defun apply-scroll-delta (offset delta content-height viewport-height)
  "Apply DELTA to OFFSET, then clamp it for CONTENT-HEIGHT and VIEWPORT-HEIGHT."
  (clamp-scroll-offset (+ (or offset 0) (or delta 0))
                       content-height
                       viewport-height))

(defun render-scroll-offset (offset content-height viewport-height &key (origin :bottom))
  "Convert OFFSET from ORIGIN semantics into a top-origin paint offset.

PTUI stateful history widgets use bottom-origin semantics by convention:
OFFSET 0 means the live tail, and larger offsets move upward into history.
The low-level :scroll paint path uses top-origin semantics instead, so callers
can use this helper to translate shared PTUI scroll state into widget offsets."
  (let* ((max-offset (max-scroll-offset content-height viewport-height))
         (clamped (clamp-scroll-offset offset content-height viewport-height)))
    (ecase origin
      (:top clamped)
      (:bottom (- max-offset clamped)))))

(defun key-scroll-action (key &key viewport-height page-step (origin :bottom) (line-step 1))
  "Map KEY to a shared scroll action.

Returns two values:
1. action keyword: :delta, :home, :end, or NIL
2. delta integer when action is :delta

Positive deltas scroll back into history. Supports both :pgup/:pgdn and
:page-up/:page-down spellings so PTUI consumers stay consistent."
  (let* ((step (max 1 (or page-step (scroll-page-step viewport-height))))
         (unit (max 1 (or line-step 1)))
         (signed-unit (if (eq origin :top) (- unit) unit))
         (signed-step (if (eq origin :top) (- step) step)))
    (case key
      (:up (values :delta signed-unit))
      (:down (values :delta (- signed-unit)))
      ((:pgup :page-up) (values :delta signed-step))
      ((:pgdn :page-down) (values :delta (- signed-step)))
      (:home (values :home nil))
      (:end (values :end nil))
      (otherwise (values nil nil)))))
