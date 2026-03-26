(defpackage :ptui.preview.app
  (:use :cl)
  (:export #:run-preview
           #:preview-app))

(in-package :ptui.preview.app)

;;; ===================================================================
;;; Live-Reloading YAML TUI Preview Application
;;; ===================================================================

;;; --- Shared mutable state (toggled by app-level key handler) ---

(defvar *preview-yaml-path* nil)
(defvar *force-reload* nil)
(defvar *show-palette* nil)
(defvar *show-roles* nil)

;;; --- Scroll state ---

(defvar *scroll-offset* 0)
(defvar *content-height* 0)
(defvar *history-viewport-height* 1)

;;; --- Ctrl+C double-tap state ---

(defvar *ctrl-c-pending* nil)
(defvar *ctrl-c-time* 0)

(defparameter +ctrl-c-timeout-ms+ 1500
  "Time window in milliseconds for the second Ctrl+C press.")

;;; --- Preview scroll debug logging ---

(defun %preview-scroll-debug-enabled-p ()
  "Read PTUI_PREVIEW_SCROLL_DEBUG at runtime."
  (let ((value (uiop:getenv "PTUI_PREVIEW_SCROLL_DEBUG")))
    (and value
         (not (string= value ""))
         (not (member (string-downcase value)
                      '("0" "false" "no" "off")
                      :test #'string=)))))

(defun %preview-scroll-debug-log-path ()
  (or (uiop:getenv "PTUI_PREVIEW_SCROLL_DEBUG_LOG")
      "/tmp/ptui-preview-scroll-debug.log"))

(defun %preview-scroll-debug-log (fmt &rest args)
  (when (%preview-scroll-debug-enabled-p)
    (ignore-errors
      (with-open-file (out (%preview-scroll-debug-log-path)
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create)
        (format out "~A ~?~%" (get-universal-time) fmt args)))))

;;; --- File mtime checking ---

(defun %file-mtime (path)
  "Return file modification time via file-write-date, or NIL if file doesn't exist."
  (ignore-errors (file-write-date path)))

;;; --- YAML file parsing ---

(defun %parse-yaml-file (path)
  "Parse a YAML file, returning (values data error-string).
On success: data is the parsed result, error-string is nil.
On failure: data is nil, error-string describes the error."
  (handler-case
      (let ((data (cl-yaml:parse (pathname path))))
        (values data nil))
    (error (e)
      (values nil (princ-to-string e)))))

;;; --- Ctrl+C / q double-tap handler ---

(defun %handle-ctrl-c-or-quit (event)
  "Handle Ctrl+C and q with double-tap exit logic.
First press shows a banner; second press within timeout returns :quit.
Returns :quit, :consume (truthy), or NIL."
  (let ((key (ptui.core.events:key-event-key event)))
    (when (or (eq key :ctrl-c)
              (and (eq key :text)
                   (string-equal (or (ptui.core.events:key-event-text? event) "") "q")))
      (let ((now (ptui.util.time:monotonic-ms)))
        (if (and *ctrl-c-pending*
                 (< (- now *ctrl-c-time*) +ctrl-c-timeout-ms+))
            ;; Second press within window — quit
            :quit
            ;; First press — show banner
            (progn
              (setf *ctrl-c-pending* t
                    *ctrl-c-time* now)
              :consume))))))

;;; --- Scroll navigation handler ---

(defun %clamp-scroll ()
  "Clamp preview scroll state using shared PTUI bottom-origin semantics."
  (let* ((before *scroll-offset*)
         (viewport-height (max 1 *history-viewport-height*))
         (after (ptui.util.scroll:clamp-scroll-offset
                 before
                 *content-height*
                 viewport-height)))
    (setf *scroll-offset* after)
    (%preview-scroll-debug-log
     "CLAMP before=~D after=~D content=~D viewport=~D max=~D"
     before after *content-height* viewport-height
     (ptui.util.scroll:max-scroll-offset *content-height* viewport-height))))

(defun %preview-yaml-lookup (node key)
  (cond
    ((hash-table-p node) (gethash key node))
    ((listp node)
     (let ((pair (assoc key node :test #'equal)))
       (when pair (cdr pair))))
    (t nil)))

(defun %preview-yaml-lookup-any (node &rest keys)
  (dolist (key keys)
    (let ((value (%preview-yaml-lookup node key)))
      (when value
        (return value)))))

(defun %preview-yaml-children (layout-node)
  (let ((raw (%preview-yaml-lookup-any layout-node "children")))
    (cond
      ((listp raw) raw)
      ((vectorp raw) (coerce raw 'list))
      (t '()))))

(defun %preview-yaml-visible-p (node)
  (let ((visible (%preview-yaml-lookup-any node "visible")))
    (or (null visible)
        (not (eq visible nil)))))

(defun %preview-height-constraint (node)
  (let ((size-val (%preview-yaml-lookup-any node "height"))
        (fill-weight (or (%preview-yaml-lookup-any node "fill-weight" "fill_weight") 1.0)))
    (cond
      ((null size-val)
       (ptui.layout.constraints:flex (intern (string-upcase (or (%preview-yaml-lookup-any node "name") "unnamed")) :keyword)
                                     :weight (coerce fill-weight 'single-float)))
      ((and (stringp size-val) (string-equal size-val "fill"))
       (ptui.layout.constraints:flex (intern (string-upcase (or (%preview-yaml-lookup-any node "name") "unnamed")) :keyword)
                                     :weight (coerce fill-weight 'single-float)))
      ((and (stringp size-val) (string-equal size-val "content"))
       (ptui.layout.constraints:fixed (intern (string-upcase (or (%preview-yaml-lookup-any node "name") "unnamed")) :keyword) 1))
      ((integerp size-val)
       (ptui.layout.constraints:fixed (intern (string-upcase (or (%preview-yaml-lookup-any node "name") "unnamed")) :keyword) size-val))
      ((realp size-val)
       (ptui.layout.constraints:fixed (intern (string-upcase (or (%preview-yaml-lookup-any node "name") "unnamed")) :keyword) (round size-val)))
      (t
       (ptui.layout.constraints:flex (intern (string-upcase (or (%preview-yaml-lookup-any node "name") "unnamed")) :keyword)
                                     :weight 1.0)))))

(defun %preview-section-entry-count (value)
  (cond
    ((hash-table-p value) (hash-table-count value))
    ((listp value) (length value))
    ((vectorp value) (length value))
    (t 0)))

(defun %preview-extra-panel-height (yaml-data)
  (let* ((palette-section (%preview-yaml-lookup-any yaml-data "palette"))
         (roles-section (%preview-yaml-lookup-any yaml-data "roles"))
         (palette-count (if (and *show-palette* palette-section)
                            (%preview-section-entry-count palette-section)
                            0))
         (roles-count (if (and *show-roles* roles-section)
                          (%preview-section-entry-count roles-section)
                          0)))
    (if (plusp (+ palette-count roles-count))
        (min 15 (+ 2
                   palette-count
                   (if (plusp roles-count)
                       (+ 2 roles-count)
                       0)))
        0)))

(defun %current-preview-rows ()
  (or (ignore-errors
        (ptui.core.types:size-rows (ptui.term.tty:tty-get-size)))
      (ignore-errors
        (let ((lines (uiop:getenv "LINES")))
          (and lines (parse-integer lines))))
      24))

(defun %estimate-history-viewport-height (yaml-data)
  "Estimate the actual history viewport height for the live preview."
  (let* ((layout (%preview-yaml-lookup-any yaml-data "layout"))
         (children (and layout (%preview-yaml-children layout)))
         (preview-rows (max 1 (%current-preview-rows)))
         (main-rows (max 1 (- preview-rows (%preview-extra-panel-height yaml-data)))))
    (if (null children)
        (progn
          (%preview-scroll-debug-log
           "VIEWPORT preview_rows=~D main_rows=~D history=1 reason=no-children"
           preview-rows main-rows)
          1)
        (let* ((constraints
                 (loop for child in children
                       when (%preview-yaml-visible-p child)
                         collect (%preview-height-constraint child)))
               (solved (ptui.layout.solver:solve-constraints constraints
                                                             main-rows))
               (history-height
                 (cdr (assoc :HISTORY solved :test #'eq))))
          (%preview-scroll-debug-log
           "VIEWPORT preview_rows=~D main_rows=~D history=~D constraints=~S solved=~S"
           preview-rows main-rows (or history-height 1) constraints solved)
          (max 1 (or history-height 1))))))

(defun %apply-preview-scroll-key (key &key (page-step 10))
  "Apply shared PTUI history scroll semantics to preview navigation keys."
  (%preview-scroll-debug-log
   "KEY key=~S before=~D content=~D viewport=~D max=~D page_step=~D"
   key *scroll-offset* *content-height* (max 1 *history-viewport-height*)
   (ptui.util.scroll:max-scroll-offset *content-height* (max 1 *history-viewport-height*))
   page-step)
  (multiple-value-bind (action delta)
      (ptui.util.scroll:key-scroll-action key
                                          :viewport-height (max 1 *history-viewport-height*)
                                          :page-step page-step)
    (case action
      (:delta
       (let* ((viewport-height (max 1 *history-viewport-height*))
              (before *scroll-offset*)
              (after (ptui.util.scroll:apply-scroll-delta
                      before delta *content-height* viewport-height)))
         (setf *scroll-offset* after)
         (%preview-scroll-debug-log
          "SCROLL key=~S action=:delta delta=~D before=~D after=~D content=~D viewport=~D max=~D"
          key delta before after *content-height* viewport-height
          (ptui.util.scroll:max-scroll-offset *content-height* viewport-height)))
       t)
      (:home
       (let ((after (ptui.util.scroll:max-scroll-offset
                     *content-height*
                     (max 1 *history-viewport-height*))))
         (setf *scroll-offset* after)
         (%preview-scroll-debug-log
          "SCROLL key=~S action=:home after=~D content=~D viewport=~D"
          key after *content-height* (max 1 *history-viewport-height*)))
       t)
      (:end
       (setf *scroll-offset* 0)
       (%preview-scroll-debug-log
        "SCROLL key=~S action=:end after=0 content=~D viewport=~D"
        key *content-height* (max 1 *history-viewport-height*))
       t)
      (otherwise
       (%preview-scroll-debug-log "KEY key=~S action=nil" key)
       nil))))

(defun %handle-preview-nav (event)
  "Handle navigation key events for scrolling.
Returns non-nil to consume the event."
  (let ((key (ptui.core.events:key-event-key event)))
    (%apply-preview-scroll-key key :page-step 10)))

;;; --- Key event handler (app-level interceptor) ---

(defun %handle-preview-key (event)
  "Handle text key events for the preview app.
Called as an interceptor — return non-nil to consume the event."
  (let ((ch (ptui.core.events:key-event-text? event)))
    (cond
      ((string-equal ch "r")
       (setf *force-reload* t)
       t)
      ((string-equal ch "p")
       (setf *show-palette* (not *show-palette*))
       t)
      ((string-equal ch "s")
       (setf *show-roles* (not *show-roles*))
       t)
      ((string-equal ch "j")
       (%apply-preview-scroll-key :down))
      ((string-equal ch "k")
       (%apply-preview-scroll-key :up))
      (t nil))))

;;; --- Ctrl+C banner auto-dismiss ---

(defun %maybe-dismiss-ctrl-c-banner ()
  "Auto-dismiss the ctrl+c banner if the timeout has elapsed.
Returns the effective status-hints string."
  (when (and *ctrl-c-pending*
             (>= (- (ptui.util.time:monotonic-ms) *ctrl-c-time*)
                 +ctrl-c-timeout-ms+))
    (setf *ctrl-c-pending* nil))
  (if *ctrl-c-pending*
      "(ctrl+c) again to exit"
      nil))

;;; --- Preview panel ---

(ptui.ui.panel:defpanel preview-panel (yaml-path)
  (:state
    (last-mtime 0 :type integer)
    (parsed-yaml nil :type t)
    (parse-error-msg nil :type t))
  (:data
    (status-hints
     (cond
       (parse-error-msg "PARSE ERROR")
       (t "h - help"))
     :deps (parse-error-msg)))
  (:layout
    (:column
      (main :flex 1
        (let* ((current-mtime (%file-mtime yaml-path))
               (needs-reload (or *force-reload*
                                 (and current-mtime
                                      (not (eql current-mtime last-mtime)))))
               ;; Check ctrl+c banner status
               (ctrl-c-hints (%maybe-dismiss-ctrl-c-banner))
               (effective-hints (or ctrl-c-hints status-hints)))
          ;; Reload if file changed
          (when needs-reload
            (when current-mtime
              (funcall set-last-mtime current-mtime))
            (when *force-reload*
              (setf *force-reload* nil))
            (multiple-value-bind (data err) (%parse-yaml-file yaml-path)
              (if err
                  (progn
                    (%preview-scroll-debug-log "RELOAD parse-error=~S path=~A" err yaml-path)
                    (funcall set-parse-error-msg err))
                  (progn
                    (%preview-scroll-debug-log "RELOAD success path=~A" yaml-path)
                    (funcall set-parse-error-msg nil)
                    (funcall set-parsed-yaml data)))))
          ;; Render
          (cond
            (parse-error-msg
             (ptui.widgets.core:make-stack-widget
              (list
               (ptui.widgets.core:make-text-widget
                "YAML Parse Error:"
                :styled-segments
                (list (list "YAML Parse Error:"
                            (ptui.core.types:make-cell
                             " "
                             (ptui.core.color:make-color-rgb 255 80 80)
                             nil
                             (ptui.core.types:make-attrs :boldp t)))))
               (ptui.widgets.core:make-text-widget "")
               (ptui.widgets.core:make-text-widget parse-error-msg))))
            (parsed-yaml
             (setf *history-viewport-height* (%estimate-history-viewport-height parsed-yaml))
             (%clamp-scroll)
             (%preview-scroll-debug-log
              "RENDER scroll=~D content=~D viewport=~D max=~D show_palette=~A show_roles=~A"
              *scroll-offset*
              *content-height*
              *history-viewport-height*
              (ptui.util.scroll:max-scroll-offset *content-height* (max 1 *history-viewport-height*))
              *show-palette*
              *show-roles*)
             (ptui.preview.yaml-translator:translate-yaml-to-tree
              parsed-yaml 80 24
              :show-palette-p *show-palette*
              :show-roles-p *show-roles*
              :status-hints effective-hints
              :scroll-offset *scroll-offset*))
            (t
             (ptui.widgets.core:make-text-widget
              (format nil "Loading ~A..." (namestring yaml-path))))))))))

;;; --- App definition ---

(ptui.ui.app:defapp preview-app (:fps 10
                                  :interceptors
                                  (list
                                   ;; Priority -1: Ctrl+C / q double-tap (runs first)
                                   (list -1
                                         (lambda (event)
                                           (and (typep event 'ptui.core.events:key-event)
                                                (member (ptui.core.events:key-event-key event)
                                                        '(:ctrl-c :text) :test #'eq)))
                                         #'%handle-ctrl-c-or-quit)
                                   ;; Priority 0: Text key handler (r/p/s/j/k)
                                   (list 0
                                         (lambda (event)
                                           (and (typep event 'ptui.core.events:key-event)
                                                (eq (ptui.core.events:key-event-key event) :text)))
                                         #'%handle-preview-key)
                                   ;; Priority 1: Navigation keys (arrows, pgup/pgdn)
                                   (list 1
                                         (lambda (event)
                                           (and (typep event 'ptui.core.events:key-event)
                                                (member (ptui.core.events:key-event-key event)
                                                        '(:up :down :pgup :pgdn) :test #'eq)))
                                         #'%handle-preview-nav)))
  (preview-panel *preview-yaml-path*))

;;; --- Entry point ---

(defun run-preview (yaml-path)
  "Start the preview TUI for the given YAML file path.
The TUI polls the file for changes and re-renders on save."
  (let ((path (pathname yaml-path)))
    (unless (probe-file path)
      (error "File not found: ~A" (namestring path)))
    (format t "~&Starting YAML preview for: ~A~%" (namestring path))
    (format t "  Keys: q=quit(x2)  r=reload  p=palette  s=roles~%")
    (format t "  Scroll: arrows/j/k  pgup/pgdn~%")
    (when (%preview-scroll-debug-enabled-p)
      (format t "  Scroll debug: ~A~%" (%preview-scroll-debug-log-path)))
    (format t "  Edit the YAML in another terminal to see live updates.~%~%")
    (let ((*preview-yaml-path* path)
          (*force-reload* nil)
          (*show-palette* nil)
          (*show-roles* nil)
          (*scroll-offset* 0)
          (*content-height* 0)
          (*history-viewport-height* 1)
          (*ctrl-c-pending* nil)
          (*ctrl-c-time* 0))
      (run-preview-app))))
