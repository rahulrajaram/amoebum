(defpackage :ptui.core.theme
  (:use :cl)
  (:export
   ;; theme struct
   #:theme #:make-theme #:theme-name #:theme-parent
   #:theme-background #:theme-foreground
   #:theme-roles #:theme-gradients #:theme-markdown
   ;; role-style struct
   #:role-style #:make-role-style
   #:role-style-fg #:role-style-bg
   #:role-style-boldp #:role-style-italicp #:role-style-underlinep
   #:role-style-invertp #:role-style-dimp #:role-style-strikep
   ;; gradient-spec struct
   #:gradient-spec #:make-gradient-spec
   #:gradient-spec-start #:gradient-spec-stop
   #:gradient-color-at
   ;; markdown-config struct
   #:markdown-config #:make-markdown-config
   #:markdown-config-code-background #:markdown-config-code-foreground
   #:markdown-config-fence-color #:markdown-config-keyword-color
   #:markdown-config-heading-color
   #:markdown-config-inline-code-background #:markdown-config-inline-code-foreground
   ;; registry + resolution
   #:*theme-registry*
   #:register-theme
   #:find-theme
   #:theme-resolve-role
   #:theme-resolve-color
   #:theme-role-cell
   ;; macro
   #:define-theme
   ;; active theme
   #:*active-theme*))

(in-package :ptui.core.theme)

;;; ---------------------------------------------------------------------------
;;; Structs
;;; ---------------------------------------------------------------------------

(defstruct (role-style (:constructor make-role-style
                            (&key fg bg boldp italicp underlinep invertp dimp strikep)))
  (fg nil)
  (bg nil)
  (boldp nil :type boolean)
  (italicp nil :type boolean)
  (underlinep nil :type boolean)
  (invertp nil :type boolean)
  (dimp nil :type boolean)
  (strikep nil :type boolean))

(defstruct (gradient-spec (:constructor make-gradient-spec (&key start stop)))
  (start nil)
  (stop nil))

(defstruct (markdown-config (:constructor make-markdown-config
                                 (&key code-background code-foreground
                                       fence-color keyword-color heading-color
                                       inline-code-background inline-code-foreground)))
  (code-background nil)
  (code-foreground nil)
  (fence-color nil)
  (keyword-color nil)
  (heading-color nil)
  (inline-code-background nil)
  (inline-code-foreground nil))

(defstruct (theme (:constructor %make-theme))
  (name nil :type symbol)
  (parent nil :type (or null theme))
  (background nil)
  (foreground nil)
  (roles (make-hash-table :test #'eq) :type hash-table)
  (gradients (make-hash-table :test #'eq) :type hash-table)
  (markdown nil :type (or null markdown-config)))

;;; ---------------------------------------------------------------------------
;;; Registry
;;; ---------------------------------------------------------------------------

(defvar *theme-registry* (make-hash-table :test #'eq))
(defvar *active-theme* nil)

(defun register-theme (theme)
  (setf (gethash (theme-name theme) *theme-registry*) theme)
  theme)

(defun find-theme (name)
  (gethash name *theme-registry*))

;;; ---------------------------------------------------------------------------
;;; Constructor
;;; ---------------------------------------------------------------------------

(defun make-theme (&key name parent background foreground roles gradients markdown)
  (let ((theme (%make-theme)))
    (setf (theme-name theme) name)
    (setf (theme-parent theme) (if (and parent (symbolp parent))
                                   (find-theme parent)
                                   parent))
    (setf (theme-background theme) background)
    (setf (theme-foreground theme) foreground)
    (when roles
      (setf (theme-roles theme) roles))
    (when gradients
      (setf (theme-gradients theme) gradients))
    (setf (theme-markdown theme) markdown)
    theme))

;;; ---------------------------------------------------------------------------
;;; Resolution (walks parent chain)
;;; ---------------------------------------------------------------------------

(defun theme-resolve-role (theme role-key)
  "Resolve a role-style for ROLE-KEY, walking parent chain."
  (when theme
    (or (gethash role-key (theme-roles theme))
        (theme-resolve-role (theme-parent theme) role-key))))

(defun theme-resolve-color (theme slot)
  "Resolve a theme color for SLOT (:background or :foreground), walking parent chain."
  (when theme
    (let ((value (ecase slot
                   (:background (theme-background theme))
                   (:foreground (theme-foreground theme)))))
      (or value
          (theme-resolve-color (theme-parent theme) slot)))))

(defun theme-resolve-markdown (theme)
  "Return the markdown-config from THEME or its parent chain."
  (when theme
    (or (theme-markdown theme)
        (theme-resolve-markdown (theme-parent theme)))))

;;; ---------------------------------------------------------------------------
;;; Gradient interpolation
;;; ---------------------------------------------------------------------------

(defun %lerp-channel (a b t-val)
  (let ((result (round (+ a (* (- b a) t-val)))))
    (max 0 (min 255 result))))

(defun gradient-color-at (spec t-val)
  "Interpolate between gradient START and STOP at T-VAL in [0,1]."
  (let ((start (gradient-spec-start spec))
        (stop (gradient-spec-stop spec))
        (t-clamped (max 0.0 (min 1.0 (coerce t-val 'single-float)))))
    (ptui.core.color:make-color-rgb
     (%lerp-channel (ptui.core.color:color-rgb-r start)
                    (ptui.core.color:color-rgb-r stop) t-clamped)
     (%lerp-channel (ptui.core.color:color-rgb-g start)
                    (ptui.core.color:color-rgb-g stop) t-clamped)
     (%lerp-channel (ptui.core.color:color-rgb-b start)
                    (ptui.core.color:color-rgb-b stop) t-clamped))))

;;; ---------------------------------------------------------------------------
;;; theme-role-cell — returns a ptui cell for a given theme role
;;; ---------------------------------------------------------------------------

(defun theme-role-cell (theme role-key &key boldp italicp underlinep invertp dimp strikep)
  "Create a ptui cell from THEME's role-style for ROLE-KEY, with optional attr overrides."
  (let ((style (theme-resolve-role theme role-key)))
    (if style
        (ptui.core.types:make-cell
         " "
         (role-style-fg style)
         (role-style-bg style)
         (ptui.core.types:make-attrs
          :boldp (or (role-style-boldp style) boldp)
          :italicp (or (role-style-italicp style) italicp)
          :underlinep (or (role-style-underlinep style) underlinep)
          :invertp (or (role-style-invertp style) invertp)
          :dimp (or (role-style-dimp style) dimp)
          :strikep (or (role-style-strikep style) strikep)))
        ;; fallback: default gray
        (ptui.core.types:make-cell
         " "
         (ptui.core.color:make-color-rgb 175 175 175)
         :default
         (ptui.core.types:make-attrs
          :boldp boldp :italicp italicp :underlinep underlinep
          :invertp invertp :dimp dimp :strikep strikep)))))

;;; ---------------------------------------------------------------------------
;;; Built-in :dark base theme
;;; ---------------------------------------------------------------------------

(defun %make-dark-base-theme ()
  (let ((roles (make-hash-table :test #'eq)))
    (flet ((add-role (key &rest args)
             (setf (gethash key roles) (apply #'make-role-style args))))
      (add-role :system     :fg (ptui.core.color:make-color-rgb 160 170 186))
      (add-role :user       :fg (ptui.core.color:make-color-rgb 220 226 236))
      (add-role :assistant  :fg (ptui.core.color:make-color-rgb 220 226 236))
      (add-role :tool       :fg (ptui.core.color:make-color-rgb 125 207 255))
      (add-role :meta       :fg (ptui.core.color:make-color-rgb 116 124 138))
      (add-role :warning    :fg (ptui.core.color:make-color-rgb 240 198 116) :boldp t)
      (add-role :error      :fg (ptui.core.color:make-color-rgb 242 143 173) :boldp t)
      (add-role :prompt     :fg (ptui.core.color:make-color-rgb 220 226 236))
      (add-role :prompt-border :fg (ptui.core.color:make-color-rgb 109 140 189))

      ;; I369: reusable PTUI row/markup tokens
      (add-role :badge          :fg (ptui.core.color:make-color-rgb 172 212 255))
      (add-role :badge-ok       :fg (ptui.core.color:make-color-rgb 122 211 130))
      (add-role :badge-muted    :fg (ptui.core.color:make-color-rgb 142 150 164) :dimp t)
      (add-role :badge-warning  :fg (ptui.core.color:make-color-rgb 240 198 116) :boldp t)
      (add-role :badge-error    :fg (ptui.core.color:make-color-rgb 242 143 173) :boldp t)
      (add-role :gutter         :fg (ptui.core.color:make-color-rgb 116 124 138))
      (add-role :gutter-selected :fg (ptui.core.color:make-color-rgb 220 226 236) :boldp t)
      (add-role :tree-gutter    :fg (ptui.core.color:make-color-rgb 86 96 112))
      (add-role :tree-row       :fg (ptui.core.color:make-color-rgb 220 226 236))
      (add-role :tree-row-selected :fg (ptui.core.color:make-color-rgb 255 255 255) :boldp t)
      (add-role :tree-connector :fg (ptui.core.color:make-color-rgb 130 141 160))
      (add-role :block-title    :fg (ptui.core.color:make-color-rgb 208 216 240) :boldp t)
      (add-role :block-key      :fg (ptui.core.color:make-color-rgb 166 206 253))
      (add-role :block-value    :fg (ptui.core.color:make-color-rgb 229 233 244))
      (add-role :block-meta     :fg (ptui.core.color:make-color-rgb 128 142 162) :dimp t))
    (let ((theme (make-theme
                  :name :dark
                  :background (ptui.core.color:make-color-rgb 16 18 24)
                  :foreground (ptui.core.color:make-color-rgb 220 226 236)
                  :roles roles)))
      (register-theme theme)
      theme)))

(defvar *dark-base-theme* (%make-dark-base-theme))

;;; ---------------------------------------------------------------------------
;;; define-theme macro
;;; ---------------------------------------------------------------------------

(defun %parse-rgb-form (form)
  "Compile-time: convert (rgb R G B) to a make-color-rgb call."
  (if (and (listp form)
           (symbolp (first form))
           (string= (symbol-name (first form)) "RGB")
           (= (length form) 4))
      `(ptui.core.color:make-color-rgb ,(second form) ,(third form) ,(fourth form))
      form))

(defun %parse-role-entry (entry)
  "Parse a single role entry like (:user-label :fg (rgb ...) :boldp t)."
  (let ((role-key (first entry))
        (plist (rest entry)))
    (let ((fg (getf plist :fg))
          (bg (getf plist :bg))
          (boldp (getf plist :boldp))
          (italicp (getf plist :italicp))
          (underlinep (getf plist :underlinep))
          (invertp (getf plist :invertp))
          (dimp (getf plist :dimp))
          (strikep (getf plist :strikep)))
      `(setf (gethash ,role-key roles)
             (make-role-style
              ,@(when fg `(:fg ,(%parse-rgb-form fg)))
              ,@(when bg `(:bg ,(%parse-rgb-form bg)))
              ,@(when boldp `(:boldp ,boldp))
              ,@(when italicp `(:italicp ,italicp))
              ,@(when underlinep `(:underlinep ,underlinep))
              ,@(when invertp `(:invertp ,invertp))
              ,@(when dimp `(:dimp ,dimp))
              ,@(when strikep `(:strikep ,strikep)))))))

(defun %parse-markdown-entries (entries)
  "Parse markdown clause entries like ((:code-background (rgb 36 42 54)) ...) into a make-markdown-config form."
  (let ((plist (loop for entry in entries
                     collect (first entry)
                     collect (second entry))))
    (let ((code-bg (getf plist :code-background))
          (code-fg (getf plist :code-foreground))
          (fence (getf plist :fence-color))
          (kw (getf plist :keyword-color))
          (heading (getf plist :heading-color))
          (inline-bg (getf plist :inline-code-background))
          (inline-fg (getf plist :inline-code-foreground)))
      `(make-markdown-config
        ,@(when code-bg `(:code-background ,(%parse-rgb-form code-bg)))
        ,@(when code-fg `(:code-foreground ,(%parse-rgb-form code-fg)))
        ,@(when fence `(:fence-color ,(%parse-rgb-form fence)))
        ,@(when kw `(:keyword-color ,(%parse-rgb-form kw)))
        ,@(when heading `(:heading-color ,(%parse-rgb-form heading)))
        ,@(when inline-bg `(:inline-code-background ,(%parse-rgb-form inline-bg)))
        ,@(when inline-fg `(:inline-code-foreground ,(%parse-rgb-form inline-fg)))))))

(defmacro define-theme (name &body clauses)
  "Define a named theme with roles, gradients, markdown config, and optional parent.

Usage:
  (define-theme my-theme
    (:base :dark)
    (:background (rgb 16 18 24))
    (:foreground (rgb 220 226 236))
    (:roles
      (:user-label :fg (rgb 224 175 104) :boldp t)
      ...)
    (:markdown
      (:code-background (rgb 36 42 54))
      ...))"
  (let ((base-name (second (assoc :base clauses)))
        (bg-form (second (assoc :background clauses)))
        (fg-form (second (assoc :foreground clauses)))
        (role-entries (rest (assoc :roles clauses)))
        (md-entries (rest (assoc :markdown clauses)))
        (gradient-entries (rest (assoc :gradients clauses)))
        ;; I369: explicitly intern in *package* to avoid package confusion
        (var-name (intern (format nil "*THEME/~A*" name) *package*))
        (theme-sym (gensym "THEME")))
    `(progn
       ;; Declare theme variable at top-level (I369: avoid undefined var warning)
       (defvar ,var-name nil)
       (let ((roles (make-hash-table :test #'eq)))
         ,@(mapcar #'%parse-role-entry role-entries)
         (let ((gradients (make-hash-table :test #'eq)))
           ,@(mapcar (lambda (ge)
                       (let ((gname (first ge))
                             (start (getf (rest ge) :start))
                             (stop (getf (rest ge) :stop)))
                         `(setf (gethash ,gname gradients)
                                (make-gradient-spec
                                 :start ,(%parse-rgb-form start)
                                 :stop ,(%parse-rgb-form stop)))))
                     gradient-entries)
           (let ((,theme-sym
                   (make-theme
                    :name ',name
                    :parent ,(when base-name `(find-theme ',base-name))
                    ,@(when bg-form `(:background ,(%parse-rgb-form bg-form)))
                    ,@(when fg-form `(:foreground ,(%parse-rgb-form fg-form)))
                    :roles roles
                    :gradients gradients
                    ,@(when md-entries
                        `(:markdown ,(%parse-markdown-entries md-entries))))))
             (register-theme ,theme-sym)
             (setf ,var-name ,theme-sym)
             ,theme-sym))))))
