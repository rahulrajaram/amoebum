(in-package :amoebum)

;;; ============================================================================
;;; Amoebum YAML Theme Layout & Behavior Extension
;;; ============================================================================
;;;
;;; This module extends yaml-theme-loader.lisp with layout and behavior
;;; configuration support, borrowed from PTUI's preview system.

;;; ----------------------------------------------------------------------------
;;; Layout Integration
;;; ----------------------------------------------------------------------------

(defparameter *yaml-layout-loaded* nil
  "The loaded YAML layout configuration, or NIL if none loaded.")

(defparameter *yaml-behavior-loaded* nil
  "The loaded YAML behavior configuration, or NIL if none loaded.")

(defun %yaml-theme-parse-layout (yaml-data)
  "Extract layout configuration from YAML data.
Returns a yaml-layout struct or NIL if no layout section."
  (let ((layout-section (%yaml-theme-lookup-any yaml-data "layout")))
    (when layout-section
      (handler-case
          (parse-yaml-layout layout-section)
        (error (e)
          (log-runtime-event :level :warn
                             :kind "yaml-layout-parse-failed"
                             :source :yaml-theme-loader
                             :message "Failed to parse layout section."
                             :details (list :error (princ-to-string e)))
          nil)))))

(defun %yaml-theme-parse-behavior (yaml-data)
  "Extract behavior configuration from YAML data.
Returns a plist of behavior settings or NIL if no behavior section."
  (let ((behavior-section (%yaml-theme-lookup-any yaml-data "behavior")))
    (when behavior-section
      (let ((result '()))
        ;; Parse scroll behavior
        (let ((scroll (%yaml-theme-lookup behavior-section "scroll")))
          (when scroll
            (let ((page-step (%yaml-theme-lookup-any scroll "page-step" "page_step"))
                  (follow-output (%yaml-theme-lookup-any scroll "follow-output" "follow_output")))
              (when page-step
                (setf (getf result :scroll-page-step) page-step))
              (when follow-output
                (setf (getf result :scroll-follow-output) (eq follow-output t))))))
        ;; Parse input behavior
        (let ((input (%yaml-theme-lookup behavior-section "input")))
          (when input
            (let ((fuzzy-picker (%yaml-theme-lookup-any input "fuzzy-picker" "fuzzy_picker"))
                  (history-search (%yaml-theme-lookup-any input "history-search" "history_search")))
              (when fuzzy-picker
                (setf (getf result :input-fuzzy-picker) (eq fuzzy-picker t)))
              (when history-search
                (setf (getf result :input-history-search) (eq history-search t))))))
        ;; Parse notification behavior
        (let ((notifications (%yaml-theme-lookup behavior-section "notifications")))
          (when notifications
            (let ((desktop (%yaml-theme-lookup notifications "desktop"))
                  (sound (%yaml-theme-lookup notifications "sound")))
              (when desktop
                (setf (getf result :notifications-desktop) (eq desktop t)))
              (when sound
                (setf (getf result :notifications-sound) (eq sound t))))))
        ;; Parse key bindings
        (let ((keys (%yaml-theme-lookup behavior-section "keys")))
          (when keys
            (let ((reload (%yaml-theme-lookup-any keys "reload"))
                  (quit (%yaml-theme-lookup-any keys "quit"))
                  (palette (%yaml-theme-lookup-any keys "palette"))
                  (roles (%yaml-theme-lookup-any keys "roles")))
              (when reload
                (setf (getf result :key-reload) (and reload (princ-to-string reload))))
              (when quit
                (setf (getf result :key-quit) (and quit (princ-to-string quit))))
              (when palette
                (setf (getf result :key-palette) (and palette (princ-to-string palette))))
              (when roles
                (setf (getf result :key-roles) (and roles (princ-to-string roles)))))))
        result))))

;;; ----------------------------------------------------------------------------
;;; Enhanced Theme Application with Layout & Behavior
;;; ----------------------------------------------------------------------------

(defun %yaml-theme-apply-with-layout (yaml-data &key source-path)
  "Apply YAML theme data including layout and behavior sections.
Returns (values success-p theme-name palette-entries role-entries layout behavior)."
  (let* ((metadata (%yaml-theme-parse-metadata yaml-data))
         (palette-entries (%yaml-theme-parse-palette yaml-data))
         (role-entries (%yaml-theme-parse-roles yaml-data palette-entries))
         (layout (%yaml-theme-parse-layout yaml-data))
         (behavior (%yaml-theme-parse-behavior yaml-data))
         (bg-rgb (%yaml-theme-compute-background palette-entries))
         (fg-rgb (%yaml-theme-compute-foreground palette-entries))
         (base-theme (or (getf metadata :base) :dark))
         (theme-name (intern (string-upcase 
                              (or (getf metadata :name)
                                  (format nil "amoebum-yaml-~A" 
                                          (get-universal-time))))
                             :keyword)))
    
    ;; Validate layout if present
    (when layout
      (multiple-value-bind (valid-p error-msg)
          (yaml-layout-validate layout)
        (unless valid-p
          (log-runtime-event :level :warn
                             :kind "yaml-layout-invalid"
                             :source :yaml-theme-loader
                             :message error-msg
                             :details (list :source source-path))
          (setf layout nil))))
    
    ;; Build role definitions for define-theme
    (let ((role-defs (mapcar #'%yaml-theme-role-definition-form role-entries))
          (amoebum-pkg (find-package :amoebum)))
      ;; Create and register the theme (bind *package* so define-theme
      ;; interns the variable in the correct package - I369 fix)
      (let ((*package* amoebum-pkg))
        (eval
         `(ptui.core.theme:define-theme ,theme-name
            (:base ,base-theme)
            (:background (rgb ,(first bg-rgb) ,(second bg-rgb) ,(third bg-rgb)))
            (:foreground (rgb ,(first fg-rgb) ,(second fg-rgb) ,(third fg-rgb)))
            (:roles
              ,@role-defs))))
      
      ;; Activate the theme
      (setf ptui.core.theme:*active-theme*
            (ptui.core.theme:find-theme theme-name))
      
      ;; Store layout and behavior
      (setf *yaml-layout-loaded* (or layout *yaml-layout-default*)
            *yaml-behavior-loaded* behavior)
      
      ;; Update tracking variables
      (setf *yaml-theme-loaded-p* t
            *yaml-theme-source-path* source-path
            *yaml-theme-last-modified* (and source-path
                                            (ignore-errors
                                              (file-write-date source-path))))
      
      ;; Log the theme load
      (log-runtime-event :level :info
                         :kind "yaml-theme-loaded"
                         :source :yaml-theme-loader
                         :message "YAML theme with layout/behavior applied successfully."
                         :details (list :source source-path
                                        :palette-count (length palette-entries)
                                        :role-count (length role-entries)
                                        :layout-p (not (null layout))
                                        :behavior-p (not (null behavior))
                                        :base base-theme
                                        :theme-name theme-name))
      
      (values t theme-name palette-entries role-entries layout behavior))))

;;; ----------------------------------------------------------------------------
;;; Application Integration
;;; ----------------------------------------------------------------------------

(defun apply-yaml-layout-to-chat (chat-state)
  "Apply the currently loaded YAML layout to a chat state.
Configures scroll behavior, focus, and other layout-related settings."
  (when *yaml-layout-loaded*
    (yaml-layout-apply-to-chat chat-state :layout *yaml-layout-loaded*))
  ;; Apply behavior settings
  (when *yaml-behavior-loaded*
    (let ((scroll-page-step (getf *yaml-behavior-loaded* :scroll-page-step))
          (scroll-follow (getf *yaml-behavior-loaded* :scroll-follow-output)))
      (when scroll-follow
        (setf (chat-ui-state-stream-scroll-follow-p chat-state) scroll-follow))))
  chat-state)

(defun yaml-theme-status-extended ()
  "Return extended theme status including layout and behavior."
  (append (yaml-theme-status)
          (list :layout-loaded-p (not (null *yaml-layout-loaded*))
                :behavior-loaded-p (not (null *yaml-behavior-loaded*))
                :layout-children-count (when *yaml-layout-loaded*
                                        (length (yaml-layout-children *yaml-layout-loaded*))))))

;;; ----------------------------------------------------------------------------
;;; Key Binding Helpers (from PTUI preview-app)
;;; ----------------------------------------------------------------------------

(defun %yaml-theme-key-binding (key-name)
  "Get a key binding from the loaded behavior config.
KEY-NAME should be a keyword like :reload, :quit, etc."
  (when *yaml-behavior-loaded*
    (case key-name
      (:reload (getf *yaml-behavior-loaded* :key-reload))
      (:quit (getf *yaml-behavior-loaded* :key-quit))
      (:palette (getf *yaml-behavior-loaded* :key-palette))
      (:roles (getf *yaml-behavior-loaded* :key-roles))
      (otherwise nil))))

(defun yaml-theme-reload-key ()
  "Get the reload key from behavior config, or 'r' as default."
  (or (%yaml-theme-key-binding :reload) "r"))

(defun yaml-theme-quit-key ()
  "Get the quit key from behavior config, or 'q' as default."
  (or (%yaml-theme-key-binding :quit) "q"))

;;; ----------------------------------------------------------------------------
;;; Auto-reload Integration
;;; ----------------------------------------------------------------------------

(defun reload-yaml-theme-with-layout-if-changed ()
  "Reload the YAML theme including layout and behavior if the file has changed.
Returns (values reloaded-p theme-name-or-status)."
  (if (yaml-theme-needs-reload-p)
      (progn
        (log-runtime-event :level :info
                           :kind "yaml-theme-reloading"
                           :source :yaml-theme-loader
                           :message "YAML theme file changed, reloading with layout/behavior..."
                           :details (list :path *yaml-theme-source-path*))
        (let ((result (multiple-value-list 
                       (load-yaml-theme :cli-path *yaml-theme-source-path*))))
          ;; Re-apply layout/behavior after reload
          (when (first result)
            (log-runtime-event :level :info
                               :kind "yaml-theme-reloaded"
                               :source :yaml-theme-loader
                               :message "Theme reloaded successfully."
                               :details (list :layout-p (not (null *yaml-layout-loaded*))
                                              :behavior-p (not (null *yaml-behavior-loaded*)))))
          (values-list result)))
      (values nil :no-change)))
