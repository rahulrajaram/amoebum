(in-package :amoebum)

;;; ============================================================================
;;; Amoebum Scroll Utilities — Borrowed from PTUI's scroll system
;;; ============================================================================
;;;
;;; This module provides consistent scroll behavior across amoebum's UI,
;;; implementing bottom-origin scroll semantics (offset 0 = live tail).
;;; Adapted from ptui/src/util/scroll.lisp

(defparameter +default-scroll-page-step-ratio+ 0.8
  "Ratio of viewport height to use for page scrolling.")

(defparameter +default-scroll-line-step+ 1
  "Default number of lines for line-by-line scrolling.")

;;; ----------------------------------------------------------------------------
;;; Core Scroll Math
;;; ----------------------------------------------------------------------------

(defun scroll-page-step (viewport-height)
  "Return the default page-scroll step for VIEWPORT-HEIGHT."
  (max 1 (floor (* (max 1 (or viewport-height 1))
                   +default-scroll-page-step-ratio+))))

(defun max-scroll-offset (content-height viewport-height)
  "Return the largest valid scroll offset for CONTENT-HEIGHT and VIEWPORT-HEIGHT.
With bottom-origin semantics, this represents how far up from the tail we can scroll."
  (max 0 (- (max 0 (or content-height 0))
            (max 1 (or viewport-height 1)))))

(defun clamp-scroll-offset (offset content-height viewport-height)
  "Clamp OFFSET into the valid scroll range for CONTENT-HEIGHT and VIEWPORT-HEIGHT.
Returns a non-negative integer within [0, max-scroll-offset]."
  (max 0
       (min (max-scroll-offset content-height viewport-height)
            (or offset 0))))

(defun apply-scroll-delta (offset delta content-height viewport-height)
  "Apply DELTA to OFFSET, then clamp it for CONTENT-HEIGHT and VIEWPORT-HEIGHT.
Positive DELTA scrolls back into history (away from tail)."
  (clamp-scroll-offset (+ (or offset 0) (or delta 0))
                       content-height
                       viewport-height))

(defun render-scroll-offset (offset content-height viewport-height &key (origin :bottom))
  "Convert OFFSET from ORIGIN semantics into a top-origin paint offset.

PTUI and amoebum use bottom-origin semantics by convention:
- OFFSET 0 means the live tail (bottom of content)
- Larger offsets move upward into history

The low-level rendering uses top-origin semantics, so this helper translates
shared scroll state into widget offsets.

ORIGIN can be :bottom (default) or :top."
  (let* ((max-offset (max-scroll-offset content-height viewport-height))
         (clamped (clamp-scroll-offset offset content-height viewport-height)))
    (ecase origin
      (:top clamped)
      (:bottom (- max-offset clamped)))))

;;; ----------------------------------------------------------------------------
;;; Key-to-Scroll Mapping
;;; ----------------------------------------------------------------------------

(defun key-scroll-action (key &key viewport-height page-step (origin :bottom) (line-step 1))
  "Map KEY to a scroll action.

Returns two values:
1. action keyword: :delta, :home, :end, or NIL
2. delta integer when action is :delta

Positive deltas scroll back into history (away from tail).
Supports both :pgup/:pgdn and :page-up/:page-down spellings.

Examples:
  (:up :viewport-height 24) → (:delta 1)    ; scroll up one line
  (:down :viewport-height 24) → (:delta -1) ; scroll down one line
  (:home) → (:home nil)                     ; jump to oldest content
  (:end) → (:end nil)                       ; jump to live tail"
  (let* ((step (max 1 (or page-step (scroll-page-step viewport-height))))
         (unit (max 1 (or line-step 1)))
         (signed-unit (if (eq origin :top) (- unit) unit))
         (signed-step (if (eq origin :top) (- step) step)))
    (case key
      (:up (values :delta signed-unit))
      (:down (values :delta (- signed-unit)))
      ((:pgup :page-up) (values :delta signed-step))
      ((:pgdn :pg-down :pagedown) (values :delta (- signed-step)))
      (:home (values :home nil))
      (:end (values :end nil))
      (otherwise (values nil nil)))))

;;; ----------------------------------------------------------------------------
;;; Scroll Debug Logging
;;; ----------------------------------------------------------------------------

(defparameter *scroll-debug-enabled-p* nil
  "Whether scroll debug logging is enabled.")

(defparameter *scroll-debug-log-path* "/tmp/amoebum-scroll-debug.log"
  "Path for scroll debug log output.")

(defun scroll-debug-enabled-p ()
  "Check if scroll debugging is enabled via AMOEBUM_SCROLL_DEBUG env var.
Must be called at runtime (not cached) to allow toggling."
  (or *scroll-debug-enabled-p*
      (let ((value (uiop:getenv "AMOEBUM_SCROLL_DEBUG")))
        (and value
             (not (string= value ""))
             (not (member (string-downcase value)
                          '("0" "false" "no" "off")
                          :test #'string=))))))

(defun scroll-debug-log (fmt &rest args)
  "Write a scroll debug log entry if debugging is enabled."
  (when (scroll-debug-enabled-p)
    (ignore-errors
      (with-open-file (out *scroll-debug-log-path*
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create)
        (format out "[~A] ~?~%" 
                (format-timestamp nil (get-universal-time))
                fmt args)))))

;;; ----------------------------------------------------------------------------
;;; Chat State Integration
;;; ----------------------------------------------------------------------------

(defun chat-ui-scroll-by-key (chat-state key &key (page-step 10))
  "Handle a scroll KEY for the chat state's message history.
Returns the new scrollback line count."
  (let* ((viewport-height (or (chat-ui-state-max-message-scrollback-lines chat-state) 24))
         (content-height (length (chat-ui-state-messages chat-state)))
         (current-offset (chat-ui-state-message-scrollback-lines chat-state)))
    (multiple-value-bind (action delta)
        (key-scroll-action key 
                          :viewport-height viewport-height
                          :page-step page-step
                          :origin :bottom)
      (let ((new-offset
              (case action
                (:delta (apply-scroll-delta current-offset delta 
                                           content-height viewport-height))
                (:home (max-scroll-offset content-height viewport-height))
                (:end 0)
                (otherwise current-offset))))
        (scroll-debug-log "SCROLL-BY-KEY key=~S action=~S delta=~D before=~D after=~D"
                          key action delta current-offset new-offset)
        (setf (chat-ui-state-message-scrollback-lines chat-state) new-offset)
        ;; When user scrolls up, disable scroll-follow
        (when (and (eq action :delta) 
                   (> delta 0)
                   (> new-offset 0))
          (setf (chat-ui-state-stream-scroll-follow-p chat-state) nil))
        new-offset))))

;;; ----------------------------------------------------------------------------
;;; Scroll State Helpers
;;; ----------------------------------------------------------------------------

(defun scroll-at-tail-p (chat-state)
  "Return T if the chat is scrolled to the live tail (offset 0)."
  (zerop (chat-ui-state-message-scrollback-lines chat-state)))

(defun ensure-scroll-at-tail (chat-state)
  "Reset scroll to the live tail, enabling scroll-follow."
  (setf (chat-ui-state-message-scrollback-lines chat-state) 0
        (chat-ui-state-stream-scroll-follow-p chat-state) t))

(defun update-max-scrollback (chat-state content-height viewport-height)
  "Update the maximum scrollback based on content and viewport.
Call this when content or viewport dimensions change."
  (let ((max-offset (max-scroll-offset content-height viewport-height)))
    (setf (chat-ui-state-max-message-scrollback-lines chat-state) max-offset)
    ;; Clamp current scrollback to new max
    (setf (chat-ui-state-message-scrollback-lines chat-state)
          (min (chat-ui-state-message-scrollback-lines chat-state)
               max-offset))))
