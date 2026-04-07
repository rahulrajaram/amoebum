(defpackage :ptui.preview.yaml-translator
  (:use :cl)
  (:export #:translate-yaml-to-tree
           #:translate-yaml-layout
           #:translate-yaml-palette
           #:translate-yaml-roles
           #:yaml-translation-error))

(in-package :ptui.preview.yaml-translator)

;;; ===================================================================
;;; YAML-to-Widget-Tree Translator for tui-spec.yaml Preview
;;; ===================================================================

(define-condition yaml-translation-error (error)
  ((detail :initarg :detail :reader yaml-translation-error-detail))
  (:report (lambda (c s)
             (format s "YAML translation error: ~A"
                     (yaml-translation-error-detail c)))))

;;; --- Utility helpers ---

(defun %yaml-lookup (alist key)
  "Look up KEY in a YAML-parsed alist (string keys from cl-yaml)."
  (cond
    ((hash-table-p alist)
     (gethash key alist))
    ((listp alist)
     (let ((pair (assoc key alist :test #'equal)))
       (when pair (cdr pair))))
    (t nil)))

(defun %yaml-lookup-any (alist &rest keys)
  "Try each key in order, return first non-nil value."
  (dolist (key keys)
    (let ((val (%yaml-lookup alist key)))
      (when val (return val)))))

(defun %ensure-string (val)
  (if val (princ-to-string val) nil))

;;; --- Inset / Padding normalization ---

(defun %yaml-inset-to-padding (value)
  "Convert YAML Inset shorthand to (top right bottom left).
Handles: integer, [vert horiz], [top right bottom left], nil."
  (cond
    ((null value) '(0 0 0 0))
    ((integerp value) (list value value value value))
    ((and (listp value) (= (length value) 2))
     (list (first value) (second value) (first value) (second value)))
    ((and (listp value) (= (length value) 4))
     value)
    ((and (vectorp value) (= (length value) 2))
     (list (aref value 0) (aref value 1) (aref value 0) (aref value 1)))
    ((and (vectorp value) (= (length value) 4))
     (list (aref value 0) (aref value 1) (aref value 2) (aref value 3)))
    (t '(0 0 0 0))))

;;; --- Gutter normalization ---

(defun %yaml-gutter-value (value)
  "Normalize gutter: integer -> left-only, [left right] -> left value."
  (cond
    ((null value) 0)
    ((integerp value) value)
    ((and (listp value) (>= (length value) 1)) (first value))
    ((and (vectorp value) (>= (length value) 1)) (aref value 0))
    (t 0)))

;;; --- Size to constraint ---

(defun %yaml-size-to-constraint (name node direction)
  "Map YAML sizing to a ptui constraint spec.
NAME is the interned symbol. NODE is the YAML alist for this layout child.
DIRECTION is the parent's stack direction (:column or :row)."
  (let* ((size-key (if (eq direction :column) "height" "width"))
         (size-val (%yaml-lookup-any node size-key))
         (fill-weight (or (%yaml-lookup-any node "fill-weight" "fill_weight") 1.0)))
    (cond
      ((null size-val)
       ;; Default: height defaults to "content" (treat as flex 1 in preview)
       (ptui.layout.constraints:flex name :weight (coerce fill-weight 'single-float)))
      ((and (stringp size-val) (string-equal size-val "fill"))
       (ptui.layout.constraints:flex name :weight (coerce fill-weight 'single-float)))
      ((and (stringp size-val) (string-equal size-val "content"))
       ;; content → fixed 1 as a reasonable preview default
       (ptui.layout.constraints:fixed name 1))
      ((integerp size-val)
       (ptui.layout.constraints:fixed name size-val))
      ((realp size-val)
       (ptui.layout.constraints:fixed name (round size-val)))
      (t
       (ptui.layout.constraints:flex name :weight 1)))))

;;; --- Border keyword ---

(defun %yaml-border-to-keyword (value)
  "Convert YAML border string to ptui keyword."
  (cond
    ((null value) nil)
    ((and (stringp value) (string-equal value "none")) nil)
    ((and (stringp value) (string-equal value "single")) :single)
    ((and (stringp value) (string-equal value "double")) :double)
    ((and (stringp value) (string-equal value "rounded")) :rounded)
    ((and (stringp value) (string-equal value "heavy")) :single)
    ((and (stringp value) (string-equal value "ascii")) :single)
    (t nil)))

;;; --- Cross-axis width wrapper ---

(defun %yaml-cross-axis-width (node direction)
  "Get the cross-axis width value from a layout node, or NIL.
In a column layout, 'width' is the cross-axis; in a row layout, 'height' is."
  (let* ((cross-key (if (eq direction :column) "width" "height"))
         (val (%yaml-lookup-any node cross-key)))
    (cond
      ((integerp val) val)
      ((realp val) (round val))
      (t nil))))

(defun %wrap-with-cross-axis-width (widget width-val name-sym)
  "Wrap WIDGET in a row constraint-layout that fixes it to WIDTH-VAL columns."
  (ptui.ui.elements:make-element
   :constraint-layout
   :id name-sym
   :props (list :direction :row
                :constraints (list (ptui.layout.constraints:fixed
                                    (%intern-name
                                     (format nil "~A-INNER" (symbol-name name-sym)))
                                    width-val)))
   :children (list
              (ptui.ui.elements:make-element
               (ptui.ui.elements:ui-element-type widget)
               :id (%intern-name (format nil "~A-INNER" (symbol-name name-sym)))
               :key (ptui.ui.elements:ui-element-key widget)
               :props (ptui.ui.elements:ui-element-props widget)
               :children (ptui.ui.elements:ui-element-children widget)))))

;;; --- Placeholder widget ---

(defun %make-placeholder-widget (name-str)
  "Create a text widget showing [name] as placeholder content."
  (ptui.widgets.core:make-text-widget
   (format nil "[~A]" name-str)))

;;; --- Layout node translation ---

(defun %intern-name (name-str)
  "Intern a YAML name string as a keyword symbol for constraint IDs."
  (intern (string-upcase name-str) :keyword))

(defun %yaml-direction (node)
  "Get direction from YAML node, default :column."
  (let ((dir (%yaml-lookup-any node "direction")))
    (cond
      ((null dir) :column)
      ((and (stringp dir) (string-equal dir "horizontal")) :row)
      ((and (stringp dir) (string-equal dir "vertical")) :column)
      (t :column))))

(defun %is-fill-height-p (node direction)
  "Check if this node uses fill height (flex sizing in main axis)."
  (let* ((size-key (if (eq direction :column) "height" "width"))
         (size-val (%yaml-lookup-any node size-key)))
    (or (null size-val)
        (and (stringp size-val) (string-equal size-val "fill")))))

(defun %make-sample-widget (name-str node direction palette-entries role-entries
                            &key status-hints (scroll-offset 0))
  "Create sample content widget for a leaf node, using role/palette styling.
Falls back to a placeholder if no sample content matches.
STATUS-HINTS, when provided, is passed to the activity status region.
SCROLL-OFFSET controls the history panel's vertical scroll position."
  (cond
    ;; Fill-height region named 'history' → rich conversation
    ((and (string-equal name-str "history")
          (%is-fill-height-p node direction)
          palette-entries)
     (let ((w (%yaml-cross-axis-width node direction)))
       (ptui.preview.sample-content:make-sample-history-content
        palette-entries role-entries (or w 80)
        :scroll-offset scroll-offset)))
    ;; Prompt region → cursor placeholder
    ((and (string-equal name-str "prompt") palette-entries)
     (ptui.preview.sample-content:make-sample-prompt-content
      palette-entries role-entries))
    ;; Status bar → single unified line
    ((and (string-equal name-str "status") palette-entries)
     (ptui.preview.sample-content:make-sample-status-content
      palette-entries role-entries name-str status-hints))
    ;; Status container itself (has children, handled by recursion)
    ;; Any other leaf → placeholder
    (t (%make-placeholder-widget name-str))))

(defun translate-yaml-layout (layout-node &key palette-entries role-entries
                                               status-hints (scroll-offset 0))
  "Translate a YAML layout section into a ptui constraint-layout element.
Returns a ui-element tree. When PALETTE-ENTRIES and ROLE-ENTRIES are
provided, leaf regions get rich sample content instead of bare placeholders.
STATUS-HINTS, when provided, is passed to the activity status region.
SCROLL-OFFSET controls the history panel's vertical scroll position."
  (let* ((children-raw (%yaml-lookup-any layout-node "children"))
         (children (cond
                     ((listp children-raw) children-raw)
                     ((vectorp children-raw) (coerce children-raw 'list))
                     (t nil)))
         (direction (%yaml-direction layout-node))
         (constraints '())
         (widget-children '()))
    (when (null children)
      ;; Leaf node — sample content or placeholder
      (let ((name (%yaml-lookup-any layout-node "name")))
        (return-from translate-yaml-layout
          (%make-sample-widget (or name "?") layout-node direction
                               palette-entries role-entries
                               :status-hints status-hints
                               :scroll-offset scroll-offset))))
    (dolist (child-node children)
      (let* ((name-str (or (%yaml-lookup-any child-node "name") "unnamed"))
             (name-sym (%intern-name name-str))
             (visible (%yaml-lookup-any child-node "visible"))
             (sub-children (%yaml-lookup-any child-node "children")))
        ;; Skip invisible nodes
        (when (or (null visible) (not (eq visible nil)))
          (let ((constraint (%yaml-size-to-constraint name-sym child-node direction))
                (border (%yaml-border-to-keyword (%yaml-lookup-any child-node "border")))
                (padding (%yaml-inset-to-padding (%yaml-lookup-any child-node "padding")))
                (gutter (%yaml-gutter-value (%yaml-lookup-any child-node "gutter"))))
            (push constraint constraints)
            (let* ((raw-inner-widget
                     (if sub-children
                         ;; Recursive: this child has its own children
                         (translate-yaml-layout child-node
                                                :palette-entries palette-entries
                                                :role-entries role-entries
                                                :status-hints status-hints
                                                :scroll-offset scroll-offset)
                         ;; Leaf: sample content or placeholder
                         (%make-sample-widget name-str child-node direction
                                              palette-entries role-entries
                                              :status-hints status-hints
                                              :scroll-offset scroll-offset)))
                   ;; Apply cross-axis width constraint when specified.
                   ;; (Leaf only — recursive children manage their own constraints.)
                   (cross-width (and (null sub-children)
                                     (%yaml-cross-axis-width child-node direction)))
                   (inner-widget
                     (if cross-width
                         (%wrap-with-cross-axis-width raw-inner-widget
                                                      cross-width
                                                      name-sym)
                         raw-inner-widget))
                   ;; Apply gutter and padding via a box wrapper. Gutter adds
                   ;; to the left padding; the 4-tuple is (top right bottom left).
                   (effective-padding
                     (when (or (> gutter 0) (some #'plusp padding))
                       (list (first padding)
                             (second padding)
                             (third padding)
                             (+ (fourth padding) gutter))))
                   ;; Fold border + padding into a single box wrapper when either
                   ;; is set, otherwise re-tag the inner widget with the node id.
                   (final-widget
                     (cond
                       ((or border effective-padding)
                        (ptui.widgets.core:make-box-widget
                         inner-widget
                         :id name-sym
                         :border border
                         :padding (or effective-padding 0)))
                       (t
                        (ptui.ui.elements:make-element
                         (ptui.ui.elements:ui-element-type inner-widget)
                         :id name-sym
                         :key (ptui.ui.elements:ui-element-key inner-widget)
                         :props (ptui.ui.elements:ui-element-props inner-widget)
                         :children (ptui.ui.elements:ui-element-children inner-widget))))))
              (push final-widget widget-children))))))
    ;; Build the constraint-layout element. Layout-level padding is handled
    ;; by wrapping the whole thing in a box with padding below.
    (let* ((layout-padding (%yaml-inset-to-padding (%yaml-lookup-any layout-node "padding")))
           (constraint-node
             (ptui.ui.elements:make-element
              :constraint-layout
              :props (list :direction direction
                           :constraints (nreverse constraints))
              :children (nreverse widget-children))))
      (if (some #'plusp layout-padding)
          (ptui.widgets.core:make-box-widget
           constraint-node
           :padding layout-padding)
          constraint-node))))

;;; --- Palette translation ---

(defun %parse-color-string (color-str)
  "Parse a color string like '16 18 24' or '#7aa2f7' into (r g b)."
  (cond
    ((null color-str) nil)
    ((and (stringp color-str) (char= (char color-str 0) #\#))
     ;; Hex format
     (let ((hex (subseq color-str 1)))
       (when (= (length hex) 6)
         (list (parse-integer hex :start 0 :end 2 :radix 16)
               (parse-integer hex :start 2 :end 4 :radix 16)
               (parse-integer hex :start 4 :end 6 :radix 16)))))
    ((stringp color-str)
     ;; Space-separated RGB
     (let ((parts (uiop:split-string color-str :separator " ")))
       (when (= (length parts) 3)
         (mapcar #'parse-integer parts))))
    (t nil)))

(defun translate-yaml-palette (palette-data)
  "Translate YAML palette section into list of (name-string r g b).
PALETTE-DATA is a hash-table or alist from cl-yaml."
  (let ((entries '()))
    (cond
      ((hash-table-p palette-data)
       (maphash (lambda (name color-str)
                  (let ((rgb (%parse-color-string (%ensure-string color-str))))
                    (when rgb
                      (push (cons name rgb) entries))))
                palette-data))
      ((listp palette-data)
       (dolist (pair palette-data)
         (when (consp pair)
           (let ((rgb (%parse-color-string (%ensure-string (cdr pair)))))
             (when rgb
               (push (cons (car pair) rgb) entries)))))))
    (nreverse entries)))

;;; --- Roles translation ---

(defun %resolve-role-color (color-name palette-entries)
  "Resolve a palette name to (r g b) from translated palette entries."
  (let ((entry (assoc color-name palette-entries :test #'equal)))
    (when entry (cdr entry))))

(defun translate-yaml-roles (roles-data palette-entries)
  "Translate YAML roles section into list of (name fg-rgb bg-rgb attrs-plist).
ROLES-DATA is a hash-table or alist. PALETTE-ENTRIES is from translate-yaml-palette."
  (let ((entries '()))
    (labels ((process-role (name role-def)
               (let* ((fg-name (%yaml-lookup-any role-def "fg"))
                      (bg-name (%yaml-lookup-any role-def "bg"))
                      (fg-rgb (when fg-name (%resolve-role-color fg-name palette-entries)))
                      (bg-rgb (when bg-name (%resolve-role-color bg-name palette-entries)))
                      (attrs (list
                              :bold (not (null (%yaml-lookup-any role-def "bold")))
                              :dim (not (null (%yaml-lookup-any role-def "dim")))
                              :italic (not (null (%yaml-lookup-any role-def "italic")))
                              :underline (not (null (%yaml-lookup-any role-def "underline")))
                              :inverse (not (null (%yaml-lookup-any role-def "inverse")))
                              :strike (not (null (%yaml-lookup-any role-def "strike"))))))
                 (push (list name fg-rgb bg-rgb attrs) entries))))
      (cond
        ((hash-table-p roles-data)
         (maphash #'process-role roles-data))
        ((listp roles-data)
         (dolist (pair roles-data)
           (when (consp pair)
             (process-role (car pair) (cdr pair)))))))
    (nreverse entries)))

;;; --- Palette swatch widget ---

(defun %make-palette-swatch (palette-entries width)
  "Create a widget showing color swatches for all palette entries.
Each entry gets a labeled colored block."
  (declare (ignore width))
  (let ((lines '()))
    (dolist (entry palette-entries)
      (let* ((name (car entry))
             (rgb (cdr entry))
             (r (first rgb))
             (g (second rgb))
             (b (third rgb))
             (color (ptui.core.color:make-color-rgb r g b))
             (text-line (format nil "  ██ ~12A rgb(~3D,~3D,~3D)" name r g b)))
        (push (ptui.widgets.core:make-text-widget
               text-line
               :styled-segments
               (list (list "  "
                           (ptui.core.types:make-cell " " nil nil
                            (ptui.core.types:make-attrs)))
                     (list "██"
                           (ptui.core.types:make-cell " " color nil
                            (ptui.core.types:make-attrs)))
                     (list (format nil " ~12A rgb(~3D,~3D,~3D)" name r g b)
                           (ptui.core.types:make-cell " " nil nil
                            (ptui.core.types:make-attrs)))))
              lines)))
    (ptui.widgets.core:make-stack-widget
     (nreverse lines)
     :id :palette-swatch)))

;;; --- Roles sample widget ---

(defun %make-roles-sample (role-entries width)
  "Create a widget showing sample text rendered in each role's style."
  (declare (ignore width))
  (let ((lines '()))
    (dolist (entry role-entries)
      (destructuring-bind (name fg-rgb bg-rgb attrs-plist) entry
        (let* ((fg-color (when fg-rgb
                           (ptui.core.color:make-color-rgb
                            (first fg-rgb) (second fg-rgb) (third fg-rgb))))
               (bg-color (when bg-rgb
                           (ptui.core.color:make-color-rgb
                            (first bg-rgb) (second bg-rgb) (third bg-rgb))))
               (attrs (ptui.core.types:make-attrs
                       :boldp (getf attrs-plist :bold)
                       :dimp (getf attrs-plist :dim)
                       :italicp (getf attrs-plist :italic)
                       :underlinep (getf attrs-plist :underline)
                       :invertp (getf attrs-plist :inverse)))
               (sample-text (format nil "  ~18A  The quick brown fox" name))
               (cell (ptui.core.types:make-cell " " fg-color bg-color attrs)))
          (push (ptui.widgets.core:make-text-widget
                 sample-text
                 :styled-segments (list (list sample-text cell)))
                lines))))
    (ptui.widgets.core:make-stack-widget
     (nreverse lines)
     :id :roles-sample)))

;;; --- Error widget ---

(defun %make-error-widget (message)
  "Create a red-text error widget for displaying parse errors."
  (let ((red (ptui.core.color:make-color-rgb 255 80 80)))
    (ptui.widgets.core:make-text-widget
     (format nil "ERROR: ~A" message)
     :styled-segments
     (list (list (format nil "ERROR: ~A" message)
                 (ptui.core.types:make-cell
                  " " red nil
                  (ptui.core.types:make-attrs :boldp t)))))))

;;; --- Top-level translator ---

(defun translate-yaml-to-tree (yaml-data width height
                               &key show-palette-p show-roles-p status-hints
                                    (scroll-offset 0))
  "Top-level: translate parsed YAML data into a complete widget tree.
YAML-DATA is a hash-table or alist from cl-yaml:parse.
STATUS-HINTS, when provided, replaces the activity text in the status bar.
  SCROLL-OFFSET controls the history panel's vertical scroll position.
  Returns a ui-element tree suitable for painting."
  (declare (ignore height))
  (handler-case
      (let* ((layout-section (%yaml-lookup-any yaml-data "layout"))
             (palette-section (%yaml-lookup-any yaml-data "palette"))
             (roles-section (%yaml-lookup-any yaml-data "roles"))
             ;; Translate palette/roles first so layout can use them
             (palette-entries (when palette-section
                                (translate-yaml-palette palette-section)))
             (role-entries (when roles-section
                             (translate-yaml-roles roles-section palette-entries)))
             ;; Translate layout with palette/role context
             (layout-tree (if layout-section
                              (translate-yaml-layout layout-section
                                                     :palette-entries palette-entries
                                                     :role-entries role-entries
                                                     :status-hints status-hints
                                                     :scroll-offset scroll-offset)
                              (ptui.widgets.core:make-text-widget "[no layout section]")))
             ;; Build optional panels
             (extra-children nil))
        ;; Add palette swatch if toggled
        (when (and show-palette-p palette-entries)
          (push (ptui.widgets.core:make-text-widget
                 "─── Palette ───"
                 :id :palette-header)
                extra-children)
          (push (%make-palette-swatch palette-entries width) extra-children))
        ;; Add roles sample if toggled
        (when (and show-roles-p role-entries)
          (push (ptui.widgets.core:make-text-widget
                 "─── Roles ───"
                 :id :roles-header)
                extra-children)
          (push (%make-roles-sample role-entries width) extra-children))
        ;; Combine: layout tree in flex region, optional panels in fixed region
        (if extra-children
            (let* ((extra-stack (ptui.widgets.core:make-stack-widget
                                 (nreverse extra-children)
                                 :id :info-panel))
                   (extra-height (min 15 (+ 2 (length (or palette-entries nil))
                                           (if show-roles-p
                                               (+ 2 (length (or role-entries nil)))
                                               0)))))
              (ptui.ui.elements:make-element
               :constraint-layout
               :props (list :direction :column
                            :constraints
                            (list (ptui.layout.constraints:flex :main-layout :weight 1)
                                  (ptui.layout.constraints:fixed :info-panel extra-height)))
               :children (list
                          (ptui.ui.elements:make-element
                           (ptui.ui.elements:ui-element-type layout-tree)
                           :id :main-layout
                           :props (ptui.ui.elements:ui-element-props layout-tree)
                           :children (ptui.ui.elements:ui-element-children layout-tree))
                          (ptui.ui.elements:make-element
                           :scroll
                           :id :info-panel
                           :props (list :viewport-height extra-height)
                           :children (list extra-stack)))))
            layout-tree))
    (error (e)
      (%make-error-widget (princ-to-string e)))))
