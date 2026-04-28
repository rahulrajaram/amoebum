(in-package :amoebum)

;;; Widget construction primitives extracted from ui/chat-render.lisp for
;;; NXT-545. Owns ptui cell builders, role-cell resolution, the per-frame
;;; style-id cache, styled-segment normalization, and the approval-dialog
;;; widget. Loaded before chat-render/transcript, scrollback, layout, and
;;; the residual chat-render facade so all callers can resolve these helpers.

(defun %chat-text-widget (text id role &key styled-segments)
  (ptui.ui.elements:make-element
   :text
   :id id
   :props (list :text text :role role :styled-segments styled-segments)
   :children '()))

(defun %handle-approval-ui-error! (chat-state stage condition)
  (handler-case
      (let ((pending-tool nil)
            (pending-decision-id nil)
            (crash-log-text (or (ignore-errors (namestring (crash-log-path)))
                                "the crash log"))
            (message-text nil))
        (ignore-errors
          (bt:with-lock-held (*pending-approval-lock*)
            (let ((pending *pending-approval*))
              (setf pending-tool (and pending
                                      (pending-approval-tool-name pending))
                    pending-decision-id (and pending
                                             (pending-approval-decision-id pending))))))
        (setf message-text
              (format nil "Approval dialog failed during ~A. The pending tool request was denied safely. See ~A for details."
                      stage
                      crash-log-text))
        (ignore-errors
          (log-runtime-condition condition
                                 :kind "approval-ui-error"
                                 :source :chat-ui
                                 :message (format nil "Approval dialog failed during ~A." stage)
                                 :details (list :stage stage
                                                :pending-tool pending-tool
                                                :pending-decision-id pending-decision-id)
                                 :path (crash-log-path)))
        (handler-case
            (chat-ui-add-message chat-state "system" message-text)
          (error ()
            (setf (chat-ui-state-messages chat-state)
                  (append (chat-ui-state-messages chat-state)
                          (list (make-chat-message "system" message-text))))))
        (ignore-errors (submit-pending-approval :deny :source :ui-error))
        (ignore-errors
          (approval-dialog-deactivate!
           (chat-ui-state-approval-dialog-state chat-state)))
        nil)
    (error ()
      (ignore-errors (submit-pending-approval :deny :source :ui-error))
      (ignore-errors
        (approval-dialog-deactivate!
         (chat-ui-state-approval-dialog-state chat-state)))
      nil)))

(defun %chat-approval-dialog-widget (chat-state approval-state)
  (handler-case
      (make-approval-dialog-widget
       (list :tool-name (approval-dialog-state-tool-name approval-state)
             :command (approval-dialog-state-command approval-state)
             :path (approval-dialog-state-path approval-state)
             :reason (approval-dialog-state-reason approval-state)
             :selected-option (approval-dialog-state-selected-option approval-state)))
    (error (condition)
      (%handle-approval-ui-error! chat-state :render condition)
      (%chat-text-widget
       "Approval dialog unavailable. Pending tool request denied safely."
       :approval-dialog-error
       :error))))

(defun %approval-recovery-active-p (chat-state)
  (or (approval-dialog-state-active-p
       (chat-ui-state-approval-dialog-state chat-state))
      (bt:with-lock-held (*pending-approval-lock*)
        (not (null *pending-approval*)))))

(defun %sync-pending-approval-dialog! (chat-state)
  "Poll *pending-approval* and activate the dialog if a new approval is waiting."
  (handler-case
      (let ((dialog (chat-ui-state-approval-dialog-state chat-state)))
        (bt:with-lock-held (*pending-approval-lock*)
          (let ((pa *pending-approval*))
            (cond
              ;; A pending approval exists but dialog is not active yet — activate it
              ((and pa (not (approval-dialog-state-active-p dialog)))
               (approval-dialog-activate! dialog
                                          (pending-approval-tool-name pa)
                                          :command (pending-approval-command pa)
                                          :path (pending-approval-path pa)
                                          :reason (pending-approval-reason pa)
                                          :decision-id (pending-approval-decision-id pa)))
              ;; No pending approval but dialog is still active — deactivate
              ((and (null pa) (approval-dialog-state-active-p dialog))
               (approval-dialog-deactivate! dialog))))))
    (error (condition)
      (%handle-approval-ui-error! chat-state :sync condition))))

(defvar *%style-resolve-cache* (make-hash-table :test #'eql)
  "Per-frame cache: style-id → resolved cell. Cleared each frame.")

(defun %resolve-style-id-to-cell (style-id)
  "Resolve a style-id to a ptui cell, caching per frame."
  (or (gethash style-id *%style-resolve-cache*)
      (let* ((entry (lookup-style style-id))
             (cell (chat-role-cell (style-entry-role entry)
                                   :boldp (style-entry-boldp entry)
                                   :italicp (style-entry-italicp entry)
                                   :underlinep (style-entry-underlinep entry)
                                   :invertp (style-entry-invertp entry)
                                   :dimp (style-entry-dimp entry)
                                   :strikep (style-entry-strikep entry))))
        (setf (gethash style-id *%style-resolve-cache*) cell)
        cell)))

(defun %styled-segment->render-segment (segment default-role)
  (cond
    ;; Compact segment: (text . style-id)
    ((compact-segment-p segment)
     (let ((text (compact-segment-text segment)))
       (when (plusp (length text))
         (list text (%resolve-style-id-to-cell (compact-segment-style-id segment))))))
    ((and (consp segment)
          (stringp (first segment))
          (typep (ignore-errors (second segment)) 'ptui.core.types:cell))
     (list (first segment) (second segment)))
    ((and (listp segment)
          (keywordp (first segment)))
     (let* ((text (or (getf segment :text) ""))
            (cell (getf segment :cell)))
       (when (plusp (length text))
         (list text
               (if (typep cell 'ptui.core.types:cell)
                   cell
                   (chat-role-cell (or (getf segment :role) default-role :meta)
                                   :boldp (getf segment :boldp)
                                   :italicp (getf segment :italicp)
                                   :underlinep (getf segment :underlinep)
                                   :invertp (getf segment :invertp)
                                   :dimp (getf segment :dimp)
                                   :strikep (getf segment :strikep)))))))
    ((and (consp segment)
          (stringp (car segment)))
     (let* ((role (if (listp (cdr segment))
                      (second segment)
                      (cdr segment)))
            (text (car segment)))
       (when (plusp (length text))
         (list text (chat-role-cell (or role default-role :meta))))))
    ((stringp segment)
     (when (plusp (length segment))
       (list segment (chat-role-cell (or default-role :meta)))))
    (t
     nil)))

(defun %normalize-tree-styled-segments! (node)
  (when (eq (ptui.ui.elements:ui-element-type node) :text)
    (let* ((props (copy-list (ptui.ui.elements:ui-element-props node)))
           (segments (getf props :styled-segments))
           (default-role (getf props :role :meta)))
      (when segments
        (let* ((segment-list (if (listp segments)
                                 segments
                                 (list segments)))
               (normalized
                 (remove nil
                         (loop for segment in segment-list
                               collect (%styled-segment->render-segment
                                        segment
                                        default-role)))))
          (when normalized
            (setf (getf props :styled-segments) normalized
                  (ptui.ui.elements:ui-element-props node) props)))))
    )
  (dolist (child (ptui.ui.elements:ui-element-children node))
    (%normalize-tree-styled-segments! child))
  node)

(defun %chat-template-cell (&key (fg :default) (bg :default) (boldp nil))
  (ptui.core.types:make-cell
   " "
   fg
   bg
   (ptui.core.types:make-attrs :boldp boldp)))

(defun %chat-cell-with-attrs (cell
                              &key
                                boldp
                                italicp
                                underlinep
                                invertp
                                dimp
                                strikep)
  (let ((attrs (ptui.core.types:cell-attrs cell)))
    (ptui.core.types:make-cell
     (ptui.core.types:cell-glyph cell)
     (ptui.core.types:cell-fg cell)
     (ptui.core.types:cell-bg cell)
     (ptui.core.types:make-attrs
      :boldp (or (ptui.core.types:attrs-boldp attrs) (not (null boldp)))
      :italicp (or (ptui.core.types:attrs-italicp attrs) (not (null italicp)))
      :underlinep (or (ptui.core.types:attrs-underlinep attrs) (not (null underlinep)))
      :invertp (or (ptui.core.types:attrs-invertp attrs) (not (null invertp)))
      :dimp (or (ptui.core.types:attrs-dimp attrs) (not (null dimp)))
      :strikep (or (ptui.core.types:attrs-strikep attrs) (not (null strikep)))))))

(defun chat-role-cell (role
                       &key
                         (focusp nil)
                         boldp
                         italicp
                         underlinep
                         invertp
                         dimp
                         strikep)
  (let* ((role-key (intern (string-upcase (princ-to-string role)) :keyword))
         (theme ptui.core.theme:*active-theme*)
         (base (if theme
                   (ptui.core.theme:theme-role-cell theme role-key)
                   ;; fallback when no theme is active
                   (%chat-template-cell :fg (ptui.core.color:make-color-rgb 175 175 175))))
         (styled (%chat-cell-with-attrs base
                                        :boldp boldp
                                        :italicp italicp
                                        :underlinep underlinep
                                        :invertp invertp
                                        :dimp dimp
                                        :strikep strikep)))
    (if focusp
        (%chat-cell-with-attrs styled :boldp t :invertp t)
        styled)))

(defun %styled-text-segments (segments &key (focusp nil))
  (let ((result '()))
    (dolist (segment segments)
      (let* ((plist-segment (and (listp segment)
                                 (keywordp (first segment))))
             (text
               (cond
                 (plist-segment
                  (or (getf segment :text) ""))
                 ((and (consp segment) (stringp (car segment)))
                  (car segment))
                 ((stringp segment)
                  segment)
                 (t
                  (princ-to-string segment))))
             (role
               (cond
                 (plist-segment
                  (or (getf segment :role) :meta))
                 ((and (consp segment) (cdr segment))
                  (cdr segment))
                 (t
                  :meta))))
        (when (plusp (length text))
          (push
           (list text
                 (chat-role-cell role
                                 :focusp focusp
                                 :boldp (and plist-segment (getf segment :boldp))
                                 :italicp (and plist-segment (getf segment :italicp))
                                 :underlinep (and plist-segment (getf segment :underlinep))
                                 :invertp (and plist-segment (getf segment :invertp))
                                 :dimp (and plist-segment (getf segment :dimp))
                                 :strikep (and plist-segment (getf segment :strikep))))
           result))))
    (nreverse result)))
