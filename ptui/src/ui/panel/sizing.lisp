(defpackage :ptui.ui.panel
  (:use :cl)
  (:export
   #:defpanel
   #:embed-panel
   #:defpanel-syntax-error
   #:defpanel-syntax-warning
   #:defpanel-syntax-error-panel-name
   #:defpanel-syntax-error-section
   #:defpanel-syntax-error-detail
   #:defpanel-syntax-warning-panel-name
   #:defpanel-syntax-warning-section
   #:defpanel-syntax-warning-detail
   ;; Compilation helpers (exposed for testing)
   #:%compile-panel-state
   #:%compile-panel-data
   #:%compile-panel-layout
   #:%compile-panel-keys
   #:%compile-panel-effects
   #:%compile-panel-styles
   #:%parse-panel-sections
   #:%normalize-padding
   #:%parse-container-options))

(in-package :ptui.ui.panel)

;;; ===================================================================
;;; defpanel sizing, style, conditions & syntax helpers
;;;
;;; First-loaded module: owns the package, the defpanel-syntax-error /
;;; defpanel-syntax-warning conditions and their signal helpers
;;; (referenced by every other panel/* module), plus cross-axis sizing
;;; primitives (padding, container options, region constraint sizing
;;; for :fixed/:flex/:percentage with :min/:max) and the :style
;;; theme-evaluation pipeline that lowers per-region style plists into
;;; a region->style lookup function and validates style regions
;;; against the layout tree.
;;; ===================================================================

(defparameter +panel-section-keywords+
  '(:state :data :layout :keys :effects :context :slots :style))

(define-condition defpanel-syntax-error (error)
  ((panel-name :initarg :panel-name
               :reader defpanel-syntax-error-panel-name)
   (section :initarg :section
            :reader defpanel-syntax-error-section)
   (detail :initarg :detail
           :reader defpanel-syntax-error-detail))
  (:report (lambda (condition stream)
             (format stream "DEFPANEL ~S section ~S: ~A"
                     (defpanel-syntax-error-panel-name condition)
                     (defpanel-syntax-error-section condition)
                     (defpanel-syntax-error-detail condition)))))

(define-condition defpanel-syntax-warning (style-warning)
  ((panel-name :initarg :panel-name
               :reader defpanel-syntax-warning-panel-name)
   (section :initarg :section
            :reader defpanel-syntax-warning-section)
   (detail :initarg :detail
           :reader defpanel-syntax-warning-detail))
  (:report (lambda (condition stream)
             (format stream "DEFPANEL ~S section ~S warning: ~A"
                     (defpanel-syntax-warning-panel-name condition)
                     (defpanel-syntax-warning-section condition)
                     (defpanel-syntax-warning-detail condition)))))

(defun %signal-syntax-error (panel-name section detail)
  (error 'defpanel-syntax-error
         :panel-name panel-name
         :section section
         :detail detail))

(defun %signal-syntax-warning (panel-name section detail)
  (warn 'defpanel-syntax-warning
        :panel-name panel-name
        :section section
        :detail detail))

;;; -------------------------------------------------------------------
;;; :style parsing and theme evaluation
;;; -------------------------------------------------------------------

(defun %style-key-valid-p (key)
  (member key '(:border :fg :bg :bold :italic :underline :inverse :dim) :test #'eq))

(defun %style-border-valid-p (border)
  (member border '(:rounded :single :double :none) :test #'eq))

(defun %parse-style-spec (spec panel-name)
  (unless (consp spec)
    (%signal-syntax-error panel-name :style
                          (format nil "Invalid style spec ~S." spec)))
  (let ((region-name (first spec))
        (options (cdr spec)))
    (unless (symbolp region-name)
      (%signal-syntax-error panel-name :style
                            (format nil "Invalid style spec ~S." spec)))
    (when (oddp (length options))
      (%signal-syntax-error panel-name :style
                            (format nil "Invalid style options in spec ~S." spec)))
    (loop for (key value) on options by #'cddr
          unless (%style-key-valid-p key)
            do (%signal-syntax-error panel-name :style
                                     (format nil "Invalid style option ~S." key)))
    (when (and (getf options :border)
               (not (%style-border-valid-p (getf options :border))))
      (%signal-syntax-error panel-name :style
                            (format nil "Invalid border style ~S." (getf options :border))))
    (destructuring-bind (region-name &key border fg bg bold italic underline inverse dim &allow-other-keys) spec
      (declare (ignore region-name))
      (let ((style '()))
        (when border
          (setf style (append style (list :border border))))
        (when fg
          (setf style (append style (list :fg fg))))
        (when bg
          (setf style (append style (list :bg bg))))
        (when bold
          (setf style (append style (list :bold bold))))
        (when italic
          (setf style (append style (list :italic italic))))
        (when underline
          (setf style (append style (list :underline underline))))
        (when inverse
          (setf style (append style (list :inverse inverse))))
        (when dim
          (setf style (append style (list :dim dim))))
        (cons (first spec) style)))))

(defun %parse-panel-styles (style-section panel-name)
  (loop for style-spec in style-section
        collect (%parse-style-spec style-spec panel-name)))

(defun %compile-panel-styles (style-specs)
  "Compile style specs into a region-style lookup function.
Returns NIL when no styles are defined."
  (when style-specs
    `(let ((%styles ',style-specs))
       (lambda (region-name)
         (cdr (assoc region-name %styles :test #'eq))))))

(defun %collect-layout-regions (panel-name layout-forms)
  (when (and layout-forms (consp layout-forms))
    (let ((container-form (first layout-forms))
          (seen (make-hash-table :test #'eq)))
      (unless (and (consp container-form)
                   (member (car container-form) '(:column :row)))
        (%signal-syntax-error panel-name :layout
                              (format nil "Invalid :layout form ~S." container-form)))
      (multiple-value-bind (options regions)
          (%parse-container-options (cdr container-form))
        (declare (ignore options))
        (loop for region in regions
              for region-name = (first region)
              do (when (gethash region-name seen)
                   (%signal-syntax-error panel-name :layout
                                         (format nil "Duplicate region name ~S in :layout."
                                                 region-name)))
              do (setf (gethash region-name seen) t)
              collect region-name)))))

(defun %validate-style-regions (panel-name layout-regions style-specs)
  (dolist (style-spec style-specs)
    (let ((region-name (car style-spec)))
      (unless (member region-name layout-regions :test #'eq)
        (%signal-syntax-error panel-name :style
                              (format nil "Unknown style region ~S." region-name))))))

;;; -------------------------------------------------------------------
;;; Padding normalization and container option parsing
;;; -------------------------------------------------------------------

(defun %normalize-padding (spec)
  "Normalize padding spec to (top right bottom left).
NIL -> (0 0 0 0), N -> (N N N N), (V H) -> (V H V H), (T R B L) -> (T R B L)."
  (cond
    ((null spec) '(0 0 0 0))
    ((integerp spec) (list spec spec spec spec))
    ((and (listp spec) (= (length spec) 2))
     (list (first spec) (second spec) (first spec) (second spec)))
    ((and (listp spec) (= (length spec) 4))
     spec)
    (t (error "Invalid padding spec ~S. Expected integer, (vert horiz), or (top right bottom left)." spec))))

(defun %parse-container-options (rest)
  "Parse keyword options from container form until first region.
Regions are lists whose car is a symbol (not a keyword).
Returns (values options-plist region-list)."
  (let ((options '())
        (remaining rest))
    (loop while (and remaining (keywordp (car remaining)))
          do (push (car remaining) options)
             (push (cadr remaining) options)
             (setf remaining (cddr remaining)))
    (values (nreverse options) remaining)))

;;; -------------------------------------------------------------------
;;; Region sizing: :fixed / :flex / :percentage with :min / :max
;;; -------------------------------------------------------------------

(defun %parse-region-form (region-form)
  "Parse a region form into (values name sizing-type sizing-value when-clause gutter min-height max-height body).
Handles optional :when, :gutter, :min-height, and :max-height clauses."
  (let ((name (first region-form))
        (sizing-type (second region-form))
        (sizing-value (third region-form))
        (rest (cdddr region-form))
        (when-clause nil)
        (gutter nil)
        (min-height nil)
        (max-height nil))
    ;; Scan trailing keyword options in any order
    (loop while (and rest (keywordp (first rest)))
          do (cond
               ((eq (first rest) :when)
                (setf when-clause (second rest))
                (setf rest (cddr rest)))
               ((eq (first rest) :gutter)
                (setf gutter (second rest))
                (setf rest (cddr rest)))
               ((eq (first rest) :min-height)
                (setf min-height (second rest))
                (setf rest (cddr rest)))
               ((eq (first rest) :max-height)
                (setf max-height (second rest))
                (setf rest (cddr rest)))
               (t
                ;; Unknown keyword — stop scanning, treat as body
                (return))))
    (values name sizing-type sizing-value when-clause gutter min-height max-height rest)))

(defun %compile-region-constraint (region-form)
  "Extract constraint spec from a :region form.
(name :fixed N body...) -> (ptui.layout.constraints:fixed 'name N)
(name :flex N body...) -> (ptui.layout.constraints:flex 'name :weight N)
(name :percentage N body...) -> (ptui.layout.constraints:percentage 'name N)
:min-height and :max-height are threaded into :flex as :min/:max."
  (multiple-value-bind (name sizing-type sizing-value when-clause gutter min-height max-height body)
      (%parse-region-form region-form)
    (declare (ignore when-clause gutter body))
    (case sizing-type
      (:fixed `(ptui.layout.constraints:fixed ',name ,sizing-value))
      (:flex `(ptui.layout.constraints:flex ',name :weight ,(or sizing-value 1)
                                            ,@(when min-height `(:min ,min-height))
                                            ,@(when max-height `(:max ,max-height))))
      (:percentage `(ptui.layout.constraints:percentage ',name ,sizing-value))
      (t (error "defpanel :region sizing must be :fixed, :flex, or :percentage. Got: ~S"
                sizing-type)))))

(defun %region-when-clause (region-form)
  "Extract the :when clause from a region form, or NIL if absent."
  (multiple-value-bind (name sizing-type sizing-value when-clause gutter min-height max-height body)
      (%parse-region-form region-form)
    (declare (ignore name sizing-type sizing-value gutter min-height max-height body))
    when-clause))

(defun %region-gutter (region-form)
  "Extract the :gutter value from a region form, or NIL if absent."
  (multiple-value-bind (name sizing-type sizing-value when-clause gutter min-height max-height body)
      (%parse-region-form region-form)
    (declare (ignore name sizing-type sizing-value when-clause min-height max-height body))
    gutter))
