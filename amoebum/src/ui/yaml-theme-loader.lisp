(in-package :amoebum)

;;; ============================================================================
;;; Amoebum YAML Theme Loader — Integrates PTUI-style YAML themes
;;; ============================================================================
;;;
;;; This module allows amoebum to load themes, palettes, and role definitions
;;; from YAML files similar to PTUI's preview system. The YAML configuration
;;; can be specified via:
;;;   - Global config: ~/.config/amoebum/theme.yaml
;;;   - Project config: <project-root>/.amoebum/theme.yaml
;;;   - CLI option: --theme-yaml <path>
;;;
;;; Example YAML structure (see examples/amoebum-theme.yaml):
;;;   palette:
;;;     bg: "16 18 24"
;;;     text: "220 226 236"
;;;     amber: "224 175 104"
;;;   
;;;   roles:
;;;     user-label:
;;;       fg: amber
;;;       bold: true
;;;     assistant-label:
;;;       fg: blue
;;;       bold: true

(defparameter *yaml-theme-search-paths*
  '("~/.config/amoebum/theme.yaml"
    "~/.amoebum/theme.yaml"
    ".amoebum/theme.yaml")
  "Default search paths for YAML theme files (in order of precedence).")

(defparameter *yaml-bundled-theme-path*
  ;; Try multiple locations to find the bundled theme
  (or
   ;; 1. Check relative to current working directory (project root)
   (ignore-errors
     (probe-file (merge-pathnames "amoebum/resources/themes/amoebum.tui-spec.yaml"
                                  (truename "."))))
   ;; 2. Check in user's local-projects (quicklisp setup)
   (ignore-errors
     (probe-file (merge-pathnames "resources/themes/amoebum.tui-spec.yaml"
                                  (asdf:system-source-directory :amoebum))))
   ;; 3. Check absolute path from home
   (ignore-errors
     (probe-file (merge-pathnames "Documents/amoebum/amoebum/resources/themes/amoebum.tui-spec.yaml"
                                  (user-homedir-pathname))))
   ;; 4. Fallback - compute path from ASDF (may fail, that's ok)
   (ignore-errors
     (merge-pathnames "resources/themes/amoebum.tui-spec.yaml"
                      (asdf:system-source-directory :amoebum))))
  "Path to the bundled default theme that ships with amoebum.")

(defparameter *yaml-theme-loaded-p* nil
  "Whether a YAML theme has been successfully loaded.")

(defparameter *yaml-theme-source-path* nil
  "Path to the currently loaded YAML theme file.")

(defparameter *yaml-theme-last-modified* nil
  "File modification time of the loaded theme for hot-reload detection.")

;;; ----------------------------------------------------------------------------
;;; Color Parsing (adapted from PTUI's yaml-translator)
;;; ----------------------------------------------------------------------------

(defun %yaml-theme-parse-color (color-str)
  "Parse a color string like '16 18 24' or '#7aa2f7' into (r g b) or nil.
Supports:
  - Space-separated RGB: '220 226 236'
  - Hex format: '#7aa2f7' or '7aa2f7'"
  (cond
    ((null color-str) nil)
    ((and (stringp color-str)
          (or (char= (char color-str 0) #\#)
              (and (= (length color-str) 6)
                   (every (lambda (c) (digit-char-p c 16)) color-str))))
     ;; Hex format
     (let ((hex (if (char= (char color-str 0) #\#)
                    (subseq color-str 1)
                    color-str)))
       (when (= (length hex) 6)
         (list (parse-integer hex :start 0 :end 2 :radix 16)
               (parse-integer hex :start 2 :end 4 :radix 16)
               (parse-integer hex :start 4 :end 6 :radix 16)))))
    ((stringp color-str)
     ;; Space-separated RGB
     (let ((parts (uiop:split-string color-str :separator " ")))
       (when (= (length parts) 3)
         (ignore-errors
           (mapcar #'parse-integer parts)))))
    (t nil)))

(defun %yaml-theme-color-to-rgb-form (color-value palette-entries)
  "Convert a color value (name string or RGB string) to an (rgb r g b) form.
Uses PALETTE-ENTRIES (alist of name -> (r g b)) for name resolution."
  (let ((rgb
          (cond
            ((null color-value) nil)
            ((and (stringp color-value)
                  (or (char= (char color-value 0) #\#)
                      (every (lambda (c) (digit-char-p c 16)) color-value)))
             ;; Direct hex or RGB specification
             (%yaml-theme-parse-color color-value))
            ((stringp color-value)
             ;; Palette name lookup
             (cdr (assoc color-value palette-entries :test #'string-equal)))
            (t nil))))
    (when rgb
      `(rgb ,(first rgb) ,(second rgb) ,(third rgb)))))

;;; ----------------------------------------------------------------------------
;;; YAML Parsing Helpers
;;; ----------------------------------------------------------------------------

(defun %yaml-theme-lookup (data key)
  "Look up KEY in YAML-parsed data (handles both hash-tables and alists)."
  (cond
    ((hash-table-p data)
     (gethash key data))
    ((listp data)
     (let ((pair (assoc key data :test #'equal)))
       (when pair (cdr pair))))
    (t nil)))

(defun %yaml-theme-lookup-any (data &rest keys)
  "Try each key in order, return first non-nil value."
  (dolist (key keys)
    (let ((val (%yaml-theme-lookup data key)))
      (when val (return val)))))

(defun %yaml-theme-hash-table->alist (ht)
  "Convert a hash-table to an alist for easier processing."
  (let ((alist '()))
    (when (hash-table-p ht)
      (maphash (lambda (k v) (push (cons k v) alist)) ht))
    (nreverse alist)))

;;; ----------------------------------------------------------------------------
;;; Palette Translation
;;; ----------------------------------------------------------------------------

(defun %yaml-theme-parse-palette (yaml-data)
  "Extract palette entries from YAML data.
Returns an alist of (name-string . (r g b))."
  (let ((palette-section (%yaml-theme-lookup-any yaml-data "palette")))
    (unless palette-section
      (return-from %yaml-theme-parse-palette '()))
    (let ((entries '()))
      (dolist (pair (%yaml-theme-hash-table->alist palette-section))
        (let* ((name (car pair))
               (color-str (cdr pair))
               (rgb (%yaml-theme-parse-color (if (stringp color-str)
                                                 color-str
                                                 (princ-to-string color-str)))))
          (when rgb
            (push (cons name rgb) entries))))
      (nreverse entries))))

;;; ----------------------------------------------------------------------------
;;; Role Translation
;;; ----------------------------------------------------------------------------

(defun %yaml-theme-parse-roles (yaml-data palette-entries)
  "Extract role definitions from YAML data.
Returns a list of (role-name fg-rgb bg-rgb attrs-plist).
Attrs-plist contains :boldp, :dimp, :italicp, :underlinep, etc."
  (let ((roles-section (%yaml-theme-lookup-any yaml-data "roles")))
    (unless roles-section
      (return-from %yaml-theme-parse-roles '()))
    (let ((entries '()))
      (dolist (pair (%yaml-theme-hash-table->alist roles-section))
        (let* ((role-name (intern (string-upcase (car pair)) :keyword))
               (role-def (cdr pair))
               (fg-name (%yaml-theme-lookup-any role-def "fg"))
               (bg-name (%yaml-theme-lookup-any role-def "bg"))
               (fg-rgb (%yaml-theme-color-to-rgb-form fg-name palette-entries))
               (bg-rgb (%yaml-theme-color-to-rgb-form bg-name palette-entries))
               (attrs (list :boldp (not (null (%yaml-theme-lookup-any role-def "bold")))
                            :dimp (not (null (%yaml-theme-lookup-any role-def "dim")))
                            :italicp (not (null (%yaml-theme-lookup-any role-def "italic")))
                            :underlinep (not (null (%yaml-theme-lookup-any role-def "underline")))
                            :invertp (not (null (%yaml-theme-lookup-any role-def "inverse")))
                            :strikep (not (null (%yaml-theme-lookup-any role-def "strike"))))))
          (push (list role-name fg-rgb bg-rgb attrs) entries)))
      (nreverse entries))))

;;; ----------------------------------------------------------------------------
;;; Metadata Extraction
;;; ----------------------------------------------------------------------------

(defun %yaml-theme-parse-metadata (yaml-data)
  "Extract metadata from YAML data.
Returns a plist with :name, :description, :version, :base keys."
  (let ((metadata-section (%yaml-theme-lookup-any yaml-data "metadata")))
    (when metadata-section
      (list :name (%yaml-theme-lookup-any metadata-section "name")
            :description (%yaml-theme-lookup-any metadata-section "description")
            :version (%yaml-theme-lookup-any metadata-section "version")
            :base (intern 
                   (string-upcase 
                    (or (%yaml-theme-lookup-any metadata-section "base")
                        "dark"))
                   :keyword)))))

;;; ----------------------------------------------------------------------------
;;; Theme Application
;;; ----------------------------------------------------------------------------

(defun %yaml-theme-role-definition-form (role-entry)
  "Convert a role entry to a PTUI theme role definition form.
ROLE-ENTRY is (role-name fg-rgb bg-rgb attrs-plist)."
  (destructuring-bind (role-name fg-rgb bg-rgb attrs) role-entry
    (let ((forms '()))
      ;; Add foreground if present
      (when fg-rgb
        (push `(:fg ,fg-rgb) forms))
      ;; Add background if present
      (when bg-rgb
        (push `(:bg ,bg-rgb) forms))
      ;; Add boolean attributes
      (when (getf attrs :boldp)
        (push '(:boldp t) forms))
      (when (getf attrs :dimp)
        (push '(:dimp t) forms))
      (when (getf attrs :italicp)
        (push '(:italicp t) forms))
      (when (getf attrs :underlinep)
        (push '(:underlinep t) forms))
      (when (getf attrs :invertp)
        (push '(:invertp t) forms))
      ;; Combine into role definition
      `(,role-name ,@(apply #'append (nreverse forms))))))

(defun %yaml-theme-compute-background (palette-entries)
  "Compute background color from palette, or use default."
  (or (cdr (assoc "bg" palette-entries :test #'string-equal))
      (cdr (assoc "background" palette-entries :test #'string-equal))
      '(16 18 24)))  ; Default dark background

(defun %yaml-theme-compute-foreground (palette-entries)
  "Compute foreground color from palette, or use default."
  (or (cdr (assoc "text" palette-entries :test #'string-equal))
      (cdr (assoc "foreground" palette-entries :test #'string-equal))
      '(220 226 236)))  ; Default light foreground

(defun %yaml-theme-apply (yaml-data &key source-path)
  "Apply YAML theme data to amoebum's active theme.
Creates and registers a new PTUI theme based on the YAML specification."
  (let* ((metadata (%yaml-theme-parse-metadata yaml-data))
         (palette-entries (%yaml-theme-parse-palette yaml-data))
         (role-entries (%yaml-theme-parse-roles yaml-data palette-entries))
         (bg-rgb (%yaml-theme-compute-background palette-entries))
         (fg-rgb (%yaml-theme-compute-foreground palette-entries))
         (base-theme (or (getf metadata :base) :dark))
         (theme-name (intern (string-upcase 
                              (or (getf metadata :name)
                                  (format nil "amoebum-yaml-~A" 
                                          (get-universal-time))))
                             :keyword)))
    
    ;; Build role definitions for define-theme
    (let ((role-defs (mapcar #'%yaml-theme-role-definition-form role-entries))
          (amoebum-pkg (find-package :amoebum)))
      ;; Create and register the theme (bind *package* so define-theme
      ;; interns the variable in the correct package - I369 fix)
      (let ((*package* amoebum-pkg))
        (eval
         `(,(find-symbol "DEFINE-THEME" :ptui.core.theme) ,theme-name
            (:base ,base-theme)
            (:background (rgb ,(first bg-rgb) ,(second bg-rgb) ,(third bg-rgb)))
            (:foreground (rgb ,(first fg-rgb) ,(second fg-rgb) ,(third fg-rgb)))
            (:roles
              ,@role-defs))))
      
      ;; Activate the theme
      (setf (symbol-value (find-symbol "*ACTIVE-THEME*" :ptui.core.theme))
            (funcall (find-symbol "FIND-THEME" :ptui.core.theme) theme-name))
      
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
                         :message "YAML theme applied successfully."
                         :details (list :source source-path
                                        :theme-name theme-name
                                        :base base-theme
                                        :description (getf metadata :description)
                                        :version (getf metadata :version)
                                        :palette-count (length palette-entries)
                                        :role-count (length role-entries)))
      
      (values t theme-name palette-entries role-entries))))

;;; ----------------------------------------------------------------------------
;;; File Loading
;;; ----------------------------------------------------------------------------

(defun %yaml-theme-compute-bundled-path ()
  "Compute the bundled theme path, trying multiple strategies.
Returns a pathname or NIL if not found."
  (or
   ;; 1. Check if already computed and valid
   (and *yaml-bundled-theme-path*
        (probe-file *yaml-bundled-theme-path*)
        *yaml-bundled-theme-path*)
   ;; 2. Check relative to project root (common in dev setups)
   (probe-file (merge-pathnames "amoebum/resources/themes/amoebum.tui-spec.yaml"
                                (truename ".")))
   ;; 3. Check via ASDF (if properly configured)
   (ignore-errors
     (probe-file (merge-pathnames "resources/themes/amoebum.tui-spec.yaml"
                                  (asdf:system-source-directory :amoebum))))
   ;; 4. Check user's Documents folder
   (probe-file (merge-pathnames "Documents/amoebum/amoebum/resources/themes/amoebum.tui-spec.yaml"
                                (user-homedir-pathname)))
   ;; 5. Check common locations
   (probe-file #P"~/projects/amoebum/amoebum/resources/themes/amoebum.tui-spec.yaml")
   (probe-file #P"~/dev/amoebum/amoebum/resources/themes/amoebum.tui-spec.yaml")))

(defun %yaml-theme-find-file (&key project-root cli-path)
  "Find the YAML theme file to load.
Search order:
  1. CLI-specified path (if provided and exists)
  2. Project config: <project-root>/.amoebum/theme.yaml
  3. Global config: ~/.config/amoebum/theme.yaml
  4. Home directory: ~/.amoebum/theme.yaml
  5. Bundled default theme (ships with amoebum)"
  ;; Check CLI path first
  (when (and cli-path (probe-file (pathname cli-path)))
    (return-from %yaml-theme-find-file (pathname cli-path)))
  
  ;; Check project root
  (when project-root
    (let ((project-theme (merge-pathnames ".amoebum/theme.yaml"
                                           (uiop:ensure-directory-pathname project-root))))
      (when (probe-file project-theme)
        (return-from %yaml-theme-find-file project-theme))))
  
  ;; Check global paths
  (dolist (path *yaml-theme-search-paths*)
    (let ((expanded (uiop:subpathname* (user-homedir-pathname)
                                       (string-left-trim "~/" path))))
      (when (and expanded (probe-file expanded))
        (return-from %yaml-theme-find-file expanded))))
  
  ;; Fall back to bundled theme
  (let ((bundled (%yaml-theme-compute-bundled-path)))
    (when bundled
      (return-from %yaml-theme-find-file bundled)))
  
  ;; Not found
  nil)

(defun load-yaml-theme (&key project-root cli-path (if-not-loaded nil))
  "Load and apply a YAML theme file.
If IF-NOT-LOADED is true, only load if no theme is currently loaded.
Returns (values success-p theme-name-or-error)."
  ;; Check if we should skip loading
  (when (and if-not-loaded *yaml-theme-loaded-p*)
    (return-from load-yaml-theme (values t :already-loaded)))
  
  ;; Find theme file
  (let ((theme-path (%yaml-theme-find-file :project-root project-root
                                           :cli-path cli-path)))
    (unless theme-path
      (return-from load-yaml-theme (values nil :no-theme-file)))
    
    ;; Parse and apply
    (handler-case
        (let* ((yaml-content (uiop:read-file-string theme-path))
               (yaml-data (cl-yaml:parse yaml-content)))
          (unless yaml-data
            (return-from load-yaml-theme (values nil :empty-yaml)))
          (if (fboundp '%yaml-theme-apply-with-layout)
              (%yaml-theme-apply-with-layout yaml-data :source-path theme-path)
              (%yaml-theme-apply yaml-data :source-path theme-path)))
      (error (e)
        (log-runtime-condition e
                               :kind "yaml-theme-load-failed"
                               :source :yaml-theme-loader
                               :message "Failed to load YAML theme."
                               :details (list :path theme-path
                                              :error (princ-to-string e))
                               :path (runtime-log-path)
                               :include-backtrace-p nil)
        (values nil (princ-to-string e))))))

;;; ----------------------------------------------------------------------------
;;; Hot Reload Support
;;; ----------------------------------------------------------------------------

(defun yaml-theme-needs-reload-p ()
  "Check if the loaded YAML theme file has been modified since loading."
  (and *yaml-theme-loaded-p*
       *yaml-theme-source-path*
       *yaml-theme-last-modified*
       (let ((current-mtime (ignore-errors
                              (file-write-date *yaml-theme-source-path*))))
         (and current-mtime
              (not (= current-mtime *yaml-theme-last-modified*))))))

(defun reload-yaml-theme-if-changed ()
  "Reload the YAML theme if the file has changed.
Returns (values reloaded-p theme-name-or-status)."
  (if (yaml-theme-needs-reload-p)
      (progn
        (log-runtime-event :level :info
                           :kind "yaml-theme-reloading"
                           :source :yaml-theme-loader
                           :message "YAML theme file changed, reloading..."
                           :details (list :path *yaml-theme-source-path*))
        (load-yaml-theme :cli-path *yaml-theme-source-path*))
      (values nil :no-change)))

;;; ----------------------------------------------------------------------------
;;; Config Integration
;;; ----------------------------------------------------------------------------

(defun yaml-theme-config-handler (config-value project-root)
  "Handler for :theme-yaml config key.
Called by the config system when a theme-yaml value is present.
CONFIG-VALUE can be:
  - A string path to the YAML file
  - T to auto-detect from standard locations
  - NIL to disable"
  (cond
    ((null config-value)
     ;; Disabled explicitly
     nil)
    ((eq config-value t)
     ;; Auto-detect
     (load-yaml-theme :project-root project-root :if-not-loaded t))
    ((stringp config-value)
     ;; Specific path
     (if (probe-file (pathname config-value))
         (load-yaml-theme :cli-path config-value :if-not-loaded t)
         (log-runtime-event :level :warn
                            :kind "yaml-theme-path-not-found"
                            :source :yaml-theme-loader
                            :message "Theme YAML file not found."
                            :details (list :path config-value))))
    (t
     (log-runtime-event :level :warn
                        :kind "yaml-theme-invalid-config"
                        :source :yaml-theme-loader
                        :message "Invalid theme-yaml config value."
                        :details (list :value config-value)))))

;;; Register with config system (if available)
(eval-when (:load-toplevel :execute)
  ;; Validation and defaults for :theme-yaml now live in
  ;; amoebum/src/config.lisp's declarative config schema.
  )

;;; ----------------------------------------------------------------------------
;;; Command Integration
;;; ----------------------------------------------------------------------------

(defun %cli-extract-theme-yaml (arguments)
  "Extract --theme-yaml option from CLI arguments.
Returns (values theme-yaml-path remaining-arguments)."
  (let ((theme-path nil)
        (remaining '()))
    (loop for args on arguments
          for arg = (car args)
          do (cond
               ((or (string= arg "--theme-yaml")
                    (string= arg "--theme"))
                (when (cdr args)
                  (setf theme-path (cadr args))
                  (pop args)))
               ((uiop:string-prefix-p "--theme-yaml=" arg)
                (setf theme-path (subseq arg (length "--theme-yaml="))))
               ((uiop:string-prefix-p "--theme=" arg)
                (setf theme-path (subseq arg (length "--theme="))))
               (t
                (push arg remaining))))
    (values theme-path (nreverse remaining))))

;;; ----------------------------------------------------------------------------
;;; Status/Info
;;; ----------------------------------------------------------------------------

(defun yaml-theme-status ()
  "Return the current YAML theme loading status as a plist."
  (list :loaded-p *yaml-theme-loaded-p*
        :source-path *yaml-theme-source-path*
        :last-modified *yaml-theme-last-modified*
        :needs-reload (yaml-theme-needs-reload-p)))

(defun install-default-yaml-theme ()
  "Install the default amoebum YAML theme to ~/.config/amoebum/theme.yaml.
Creates the directory structure if needed.
Returns the path to the installed theme file, or nil if installation failed."
  (let* ((source-path (merge-pathnames 
                       "resources/themes/amoebum.tui-spec.yaml"
                       (asdf:system-source-directory :amoebum)))
         (config-dir (merge-pathnames ".config/amoebum/" (user-homedir-pathname)))
         (target-path (merge-pathnames "theme.yaml" config-dir)))
    (handler-case
        (progn
          ;; Ensure config directory exists
          (ensure-directories-exist config-dir :verbose nil)
          ;; Copy theme file
          (uiop:copy-file source-path target-path)
          (log-runtime-event :level :info
                             :kind "yaml-theme-installed"
                             :source :yaml-theme-loader
                             :message "Default YAML theme installed."
                             :details (list :source source-path
                                            :target target-path))
          target-path)
      (error (e)
        (log-runtime-event :level :error
                           :kind "yaml-theme-install-failed"
                           :source :yaml-theme-loader
                           :message "Failed to install default YAML theme."
                           :details (list :error (princ-to-string e)))
        nil))))
