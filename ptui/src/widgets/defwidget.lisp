(defpackage :ptui.widgets.defwidget
  (:use :cl)
  (:export
   #:defwidget
   #:*widget-registry*
   #:*widget-caches*
   #:register-widget
   #:find-widget
   #:list-widgets
   #:widget-dirty-p
   #:mark-widget-dirty
   #:clear-widget-dirty
   #:invalidate-widget
   #:invalidate-all-widgets
   #:widget-render-count))

(in-package :ptui.widgets.defwidget)

(define-condition defwidget-definition-warning (style-warning)
  ((widget :initarg :widget
           :reader defwidget-definition-warning-widget)
   (prop :initarg :prop
         :initform nil
         :reader defwidget-definition-warning-prop)
   (reason :initarg :reason
           :reader defwidget-definition-warning-reason))
  (:report (lambda (condition stream)
             (let ((widget (defwidget-definition-warning-widget condition))
                   (prop (defwidget-definition-warning-prop condition))
                   (reason (defwidget-definition-warning-reason condition)))
               (if prop
                   (format stream "DEFWIDGET ~S prop ~S: ~A"
                           widget prop reason)
                   (format stream "DEFWIDGET ~S: ~A" widget reason))))))

(defstruct (widget-definition
            (:constructor %make-widget-definition
                (name render-fn memoize-mode arity cache)))
  (name nil :type symbol)
  (render-fn (lambda (&rest _) (declare (ignore _)) nil) :type function)
  (memoize-mode :equal :type (or null keyword))
  (arity 0 :type fixnum)
  (cache (make-hash-table :test #'equal))
  (dirty-p t :type boolean)
  (last-props '() :type list)
  (last-props-known-p nil :type boolean)
  (render-count 0 :type fixnum))

(defparameter *widget-registry* (make-hash-table :test #'eq))
(defparameter *widget-caches* (make-hash-table :test #'eq))

(defparameter +widget-root-heads+
  '(vstack hstack text box scroll input spacer when-widget map-widget
    if when unless cond case ecase typecase etypecase
    let let* flet labels lambda
    progn block return-from))

(defparameter +lambda-list-keywords+
  '(&optional &rest &body &key &allow-other-keys &aux &whole &environment))

(defun %make-widget-cache (memoize-mode arity)
  (let ((test (if (and (eq memoize-mode :eq) (= arity 1))
                  'eq
                  'equal)))
    #+sbcl
    (make-hash-table :test test :weakness :key)
    #-sbcl
    (make-hash-table :test test)))

(defun %validate-lambda-list (name lambda-list)
  (unless (listp lambda-list)
    (error "DEFWIDGET ~S requires a simple required-arguments lambda list. Got: ~S"
           name lambda-list))
  (dolist (entry lambda-list)
    (unless (and (symbolp entry)
                 (not (member entry +lambda-list-keywords+)))
      (error "DEFWIDGET ~S only supports required symbol parameters. Invalid entry: ~S"
             name entry)))
  (let ((dupes (set-difference lambda-list
                               (remove-duplicates lambda-list :test #'eq))))
    (when dupes
      (error "DEFWIDGET ~S has duplicate parameter names: ~S"
             name (remove-duplicates dupes :test #'eq)))))

(defun %known-widget-root-form-p (form lambda-list)
  (cond
    ((symbolp form)
     (or (member form lambda-list :test #'eq)
         (member form '(nil t) :test #'eq)))
    ((atom form) nil)
    (t
     (let ((head (car form)))
       (or (member head +widget-root-heads+ :test #'eq)
           (member head lambda-list :test #'eq))))))

(defun %form-uses-symbol-p (form symbol)
  (cond
    ((eq form symbol) t)
    ((consp form)
     (or (%form-uses-symbol-p (car form) symbol)
         (%form-uses-symbol-p (cdr form) symbol)))
    ((vectorp form)
     (loop for item across form
           thereis (%form-uses-symbol-p item symbol)))
    (t nil)))

(defun %warn-unused-widget-props (name lambda-list body)
  (dolist (prop lambda-list)
    (unless (some (lambda (form) (%form-uses-symbol-p form prop)) body)
      (warn 'defwidget-definition-warning
            :widget name
            :prop prop
            :reason "Prop is never referenced in widget body."))))

(defparameter +element-heads+
  '(vstack hstack text box scroll input spacer))

(defparameter +element-list-heads+
  '(when-widget map-widget))

(defparameter +body-last-heads+
  '(progn let let* flet labels))

(defun %classify-branches (forms lambda-list)
  "Classify each form; return :non-element if all are, else :unknown."
  (let ((kinds (loop for form in forms
                     collect (%widget-root-classification (car (last form))
                                                          lambda-list))))
    (if (every (lambda (kind) (eq kind :non-element)) kinds)
        :non-element
        :unknown)))

(defun %classify-conditional (form lambda-list)
  "Classify if/when/unless/cond/case-family forms."
  (let ((head (car form)))
    (cond
      ((eq head 'if)
       (let ((then-kind (%widget-root-classification (third form) lambda-list))
             (else-kind (%widget-root-classification (fourth form) lambda-list)))
         (if (and (eq then-kind :non-element)
                  (eq else-kind :non-element))
             :non-element
             :unknown)))
      ((member head '(when unless) :test #'eq)
       (let ((body-kind (%widget-root-classification (car (last (cddr form))) lambda-list)))
         (if (eq body-kind :non-element) :non-element :unknown)))
      ((eq head 'cond)
       (%classify-branches (cdr form) lambda-list))
      ((member head '(case ecase typecase etypecase) :test #'eq)
       (%classify-branches (cddr form) lambda-list))
      (t :unknown))))

(defun %classify-compound-form (form lambda-list)
  "Classify a compound (list) form."
  (let ((head (car form)))
    (cond
      ((member head +element-heads+ :test #'eq)
       :element)
      ((member head +element-list-heads+ :test #'eq)
       :element-list)
      ((member head +body-last-heads+ :test #'eq)
       (%widget-root-classification (car (last (cddr form))) lambda-list))
      ((member head '(if when unless cond case ecase typecase etypecase) :test #'eq)
       (%classify-conditional form lambda-list))
      (t :unknown))))

(defun %widget-root-classification (form lambda-list)
  "Classify FORM as :element, :element-list, :non-element, or :unknown."
  (cond
    ((or (null form) (eq form t))
     :non-element)
    ((symbolp form)
     (if (member form lambda-list :test #'eq) :unknown :non-element))
    ((atom form)
     :non-element)
    (t
     (%classify-compound-form form lambda-list))))

(defun %validate-widget-root-form (name lambda-list body)
  (when (null body)
    (error "DEFWIDGET ~S requires at least one render form." name))
  (let* ((root (first (last body)))
         (normalized-root (if (and (consp root)
                                   (null (rest root))
                                   (consp (first root)))
                              (first root)
                              root)))
    (case (%widget-root-classification normalized-root lambda-list)
      (:non-element
       (error "DEFWIDGET ~S has non-element root form ~S."
              name
              normalized-root))
      (:element-list
       (error "DEFWIDGET ~S root form ~S returns a list; widget root must be a single UI element."
              name
              normalized-root))
      (otherwise
       t))))

(defun %parse-defwidget-options (name forms)
  (let ((docstring nil)
        (memoize-mode :equal)
        (focusable nil)
        (focusable-specified-p nil)
        (tail forms))
    (when (and tail (stringp (first tail)))
      (setf docstring (first tail)
            tail (rest tail)))
    (loop while (and tail
                     (consp (first tail))
                     (keywordp (first (first tail))))
          do (destructuring-bind (option-key &rest option-args) (pop tail)
               (case option-key
                 (:memoize
                  (unless (= (length option-args) 1)
                    (error "DEFWIDGET ~S option :MEMOIZE expects one value. Got: ~S"
                           name option-args))
                  (let ((mode (first option-args)))
                    (unless (member mode '(:equal :eq nil))
                      (error "DEFWIDGET ~S option :MEMOIZE must be :EQUAL, :EQ, or NIL. Got: ~S"
                             name mode))
                    (setf memoize-mode mode)))
                 (:focusable
                  (unless (= (length option-args) 1)
                    (error "DEFWIDGET ~S option :FOCUSABLE expects one boolean. Got: ~S"
                           name option-args))
                  (let ((value (first option-args)))
                    (unless (or (null value) (eq value t))
                      (error "DEFWIDGET ~S option :FOCUSABLE must be T or NIL. Got: ~S"
                             name value))
                    (setf focusable (not (null value))
                          focusable-specified-p t)))
                 (otherwise
                  (error "DEFWIDGET ~S unknown option ~S." name option-key)))))
    (values docstring memoize-mode focusable focusable-specified-p tail)))

(defun %ensure-widget-element (value widget-name)
  (unless (typep value 'ptui.ui.elements:ui-element)
    (error "Widget ~S must return PTUI.UI.ELEMENTS:UI-ELEMENT. Got: ~S"
           widget-name value))
  value)

(defun %copy-element-with-focusable (element focusablep)
  (ptui.ui.elements:make-element
   (ptui.ui.elements:ui-element-type element)
   :id (ptui.ui.elements:ui-element-id element)
   :key (ptui.ui.elements:ui-element-key element)
   :props (copy-list (ptui.ui.elements:ui-element-props element))
   :children (ptui.ui.elements:ui-element-children element)
   :focusablep focusablep))

(defun %finalize-widget-result (widget-name value focusable-specified-p focusable)
  (let ((element (%ensure-widget-element value widget-name)))
    (if focusable-specified-p
        (%copy-element-with-focusable element focusable)
        element)))

(defun %normalize-widget-children (&rest children)
  (let ((out '()))
    (labels ((walk (value)
               (cond
                 ((null value) nil)
                 ((typep value 'ptui.ui.elements:ui-element)
                  (push value out))
                 ((listp value)
                  (dolist (entry value)
                    (walk entry)))
                 (t
                  (error "Widget child must be a UI-ELEMENT, list of UI-ELEMENTs, or NIL. Got: ~S"
                         value)))))
      (dolist (child children)
        (walk child)))
    (nreverse out)))

(defun %normalize-single-widget-child (child owner-name)
  (let ((children (%normalize-widget-children child)))
    (cond
      ((null children) nil)
      ((= (length children) 1) (first children))
      (t
       (error "~A expects exactly one child. Got ~D children."
              owner-name
              (length children))))))

(defun %widget-vstack (&rest children)
  (ptui.widgets.core:make-stack-widget
   (%normalize-widget-children children)
   :direction :column))

(defun %widget-hstack (&rest children)
  (ptui.widgets.core:make-stack-widget
   (%normalize-widget-children children)
   :direction :row))

(defun %widget-text (content &key style (wrap nil wrap-supplied-p) id key role)
  (let ((base (ptui.widgets.core:make-text-widget content :id id :key key :role role)))
    (if (or style wrap-supplied-p)
        (let ((props (copy-list (ptui.ui.elements:ui-element-props base))))
          (when style
            (setf props (append props (list :style style))))
          (when wrap-supplied-p
            (setf props (append props (list :wrap (not (null wrap))))))
          (ptui.ui.elements:make-element
           (ptui.ui.elements:ui-element-type base)
           :id (ptui.ui.elements:ui-element-id base)
           :key (ptui.ui.elements:ui-element-key base)
           :props props
           :children (ptui.ui.elements:ui-element-children base)
           :focusablep (ptui.ui.elements:ui-element-focusablep base)))
        base)))

(defun %widget-box (child &key (padding 0) (border nil) id key)
  (ptui.widgets.core:make-box-widget
   (%normalize-single-widget-child child "BOX")
   :id id
   :key key
   :padding padding
   :borderp border))

(defun %widget-scroll (child &key width height (offset 0) id key)
  (ptui.widgets.core:make-scroll-widget
   (%normalize-single-widget-child child "SCROLL")
   :id id
   :key key
   :viewport-width width
   :viewport-height height
   :offset offset))

(defun %widget-input (value &key (min-width 0) on-event id key)
  (ptui.widgets.core:make-input-widget
   value
   :id id
   :key key
   :min-width min-width
   :on-event on-event))

(defun %widget-spacer (width height)
  (ptui.widgets.core:make-spacer-widget width height))

(defun register-widget (name render-fn &key (memoize :equal) (arity 0))
  (check-type name symbol)
  (check-type render-fn function)
  (unless (member memoize '(:equal :eq nil))
    (error "Widget ~S memoization must be :EQUAL, :EQ, or NIL. Got: ~S" name memoize))
  (let ((cache (%make-widget-cache memoize arity)))
    (setf (gethash name *widget-registry*)
          (%make-widget-definition name render-fn memoize arity cache))
    (setf (gethash name *widget-caches*) cache))
  name)

(defun find-widget (name)
  (gethash name *widget-registry*))

(defun list-widgets ()
  (let ((names '()))
    (maphash (lambda (name _definition)
               (declare (ignore _definition))
               (push name names))
             *widget-registry*)
    (sort names #'string< :key #'symbol-name)))

(defun %ensure-widget-definition (name)
  (or (find-widget name)
      (error "No widget definition registered for ~S." name)))

(defun widget-dirty-p (name)
  (widget-definition-dirty-p (%ensure-widget-definition name)))

(defun mark-widget-dirty (name)
  (let ((definition (%ensure-widget-definition name)))
    (setf (widget-definition-dirty-p definition) t)
    t))

(defun clear-widget-dirty (name)
  (let ((definition (%ensure-widget-definition name)))
    (setf (widget-definition-dirty-p definition) nil)
    nil))

(defun widget-render-count (name)
  (widget-definition-render-count (%ensure-widget-definition name)))

(defun invalidate-widget (name)
  (let ((definition (%ensure-widget-definition name))
        (cache (gethash name *widget-caches*)))
    (when cache
      (clrhash cache))
    (setf (widget-definition-last-props definition) '()
          (widget-definition-last-props-known-p definition) nil
          (widget-definition-dirty-p definition) t)
    t))

(defun invalidate-all-widgets ()
  (maphash (lambda (name _definition)
             (declare (ignore _definition))
             (invalidate-widget name))
           *widget-registry*)
  t)

(defun %props-eq-list-p (left right)
  (and (= (length left) (length right))
       (loop for l in left
             for r in right
             always (eq l r))))

(defun %props-changed-p (definition props)
  (if (not (widget-definition-last-props-known-p definition))
      t
      (let ((last (widget-definition-last-props definition))
            (mode (widget-definition-memoize-mode definition)))
        (case mode
          (:eq (not (%props-eq-list-p props last)))
          (otherwise (not (equal props last)))))))

(defun %props-cache-key (definition props)
  (let ((mode (widget-definition-memoize-mode definition))
        (arity (widget-definition-arity definition)))
    (cond
      ((null mode) nil)
      ((and (eq mode :eq) (= arity 1))
       (first props))
      ((eq mode :equal)
       (copy-tree props))
      (t
       (copy-list props)))))

(defun %props-tracking-snapshot (definition props)
  (let ((mode (widget-definition-memoize-mode definition)))
    (if (eq mode :equal)
        (copy-tree props)
        (copy-list props))))

(defun %invoke-widget (name props render-thunk)
  (let* ((definition (%ensure-widget-definition name))
         (cache (gethash name *widget-caches*)))
    (when (%props-changed-p definition props)
      (setf (widget-definition-dirty-p definition) t))
    (when (and cache
               (not (widget-definition-dirty-p definition))
               (widget-definition-memoize-mode definition))
      (let ((cache-key (%props-cache-key definition props)))
        (multiple-value-bind (cached cachedp) (gethash cache-key cache)
          (when cachedp
            (return-from %invoke-widget cached)))))
    (let ((element (%ensure-widget-element (funcall render-thunk) name)))
      (incf (widget-definition-render-count definition))
      (setf (widget-definition-last-props definition)
            (%props-tracking-snapshot definition props)
            (widget-definition-last-props-known-p definition) t
            (widget-definition-dirty-p definition) nil)
      (when (and cache (widget-definition-memoize-mode definition))
        (setf (gethash (%props-cache-key definition props) cache) element))
      element)))

(defmacro defwidget (name lambda-list &body forms)
  (check-type name symbol)
  (%validate-lambda-list name lambda-list)
  (multiple-value-bind (docstring memoize-mode focusable focusable-specified-p body)
      (%parse-defwidget-options name forms)
    (%warn-unused-widget-props name lambda-list body)
    (%validate-widget-root-form name lambda-list body)
    (let* ((widget-package (or (symbol-package name) *package*))
           (render-name (intern (format nil "RENDER-~A" (symbol-name name))
                                widget-package))
           (vstack-sym (intern "VSTACK" widget-package))
           (hstack-sym (intern "HSTACK" widget-package))
           (text-sym (intern "TEXT" widget-package))
           (box-sym (intern "BOX" widget-package))
           (scroll-sym (intern "SCROLL" widget-package))
           (input-sym (intern "INPUT" widget-package))
           (spacer-sym (intern "SPACER" widget-package))
           (when-widget-sym (intern "WHEN-WIDGET" widget-package))
           (map-widget-sym (intern "MAP-WIDGET" widget-package))
          (arity (length lambda-list))
          ;; I270: detect :key or :id in lambda-list for instance-key
          (key-prop (find :key lambda-list :test #'string-equal :key #'symbol-name))
          (id-prop (find :id lambda-list :test #'string-equal :key #'symbol-name)))
      `(progn
         (defun ,render-name ,lambda-list
           ,@(when docstring (list docstring))
           (macrolet ((,vstack-sym (&rest children)
                        `(ptui.widgets.defwidget::%widget-vstack ,@children))
                      (,hstack-sym (&rest children)
                        `(ptui.widgets.defwidget::%widget-hstack ,@children))
                      (,text-sym (content &rest args)
                        `(ptui.widgets.defwidget::%widget-text ,content ,@args))
                      (,box-sym (child &rest args)
                        `(ptui.widgets.defwidget::%widget-box ,child ,@args))
                      (,scroll-sym (child &rest args)
                        `(ptui.widgets.defwidget::%widget-scroll ,child ,@args))
                      (,input-sym (value &rest args)
                        `(ptui.widgets.defwidget::%widget-input ,value ,@args))
                      (,spacer-sym (width height)
                        `(ptui.widgets.defwidget::%widget-spacer ,width ,height))
                      (,when-widget-sym (predicate &body children)
                        `(if ,predicate
                             (ptui.widgets.defwidget::%normalize-widget-children ,@children)
                             nil))
                      (,map-widget-sym (fn sequence)
                        `(mapcar ,fn ,sequence)))
             ;; I270: Bind widget context around body for hooks support
             (let ((ptui.ui.runtime:*current-widget-context*
                     (ptui.ui.runtime::%make-widget-context
                      ',name
                      ,(cond
                         (key-prop key-prop)
                         (id-prop id-prop)
                         (t `',name))
                      ptui.ui.runtime:*current-runtime*)))
               (ptui.widgets.defwidget::%finalize-widget-result
                ',name
                (progn ,@body)
                ,focusable-specified-p
                ,focusable))))
         (defun ,name ,lambda-list
           (ptui.widgets.defwidget::%invoke-widget
            ',name
            (list ,@lambda-list)
            (lambda ()
              (,render-name ,@lambda-list))))
         (eval-when (:load-toplevel :execute)
           (ptui.widgets.defwidget:register-widget
            ',name
            #',render-name
            :memoize ,memoize-mode
            :arity ,arity))
         ',name))))
