(in-package :amoebum)

;;; Scrollback / viewport offset bookkeeping and chat-tree id normalization
;;; extracted from ui/chat-render.lisp for NXT-545. Scrollback semantics
;;; remain unchanged: %compute-scroll-offset clamps the requested scrollback
;;; to the available range and returns a tuple consumed by transcript layout
;;; and chat-panel virtual scroll. The tree-id helpers fold synthesized
;;; component-local ids back to their stable :tree-browser, :chat-plan-
;;; presentation, and :chat-input handles after composition.

(defun %compute-scroll-offset (total-lines viewport-height scrollback-lines)
  (let* ((max-scrollback (max 0 (- total-lines viewport-height)))
         (bounded-scrollback (max 0 (min max-scrollback scrollback-lines)))
         (offset (- max-scrollback bounded-scrollback)))
    (values offset bounded-scrollback max-scrollback)))

(defun %chat-tree-has-id-p (node target-id)
  (or (equal (ptui.ui.elements:ui-element-id node) target-id)
      (loop for child in (ptui.ui.elements:ui-element-children node)
            thereis (%chat-tree-has-id-p child target-id))))

(defun %chat-tree-has-id-prefix-p (node target-id)
  (let ((node-id (ptui.ui.elements:ui-element-id node)))
    (or (and (consp node-id)
             (equal (first node-id) target-id))
        (loop for child in (ptui.ui.elements:ui-element-children node)
              thereis (%chat-tree-has-id-prefix-p child target-id)))))

(defun %normalize-chat-tree-ids! (tree)
  (let ((id-remaps '()))
    (labels ((record-id-remap (old-id new-id)
               (let ((existing (assoc old-id id-remaps :test #'eq)))
                 (if existing
                     (setf (cdr existing) new-id)
                     (push (cons old-id new-id) id-remaps))))
             (normalize-node! (node)
               (let ((node-id (ptui.ui.elements:ui-element-id node)))
                 (when (and (symbolp node-id)
                            (string= (symbol-name node-id) "TREE")
                            (%chat-tree-has-id-p node :tree-browser-header))
                   (record-id-remap node-id :tree-browser)
                   (setf (ptui.ui.elements:ui-element-id node) :tree-browser))
                 (when (and (symbolp node-id)
                            (string= (symbol-name node-id) "PLAN")
                            (%chat-tree-has-id-prefix-p node :chat-plan-presentation))
                   (record-id-remap node-id :chat-plan-presentation)
                   (setf (ptui.ui.elements:ui-element-id node) :chat-plan-presentation))
                 (when (and (symbolp node-id)
                            (string= (symbol-name node-id) "INPUT")
                            (eql (ptui.ui.elements:ui-element-type node) :prompt-box))
                   (record-id-remap node-id :chat-input)
                   (setf (ptui.ui.elements:ui-element-id node) :chat-input)))
               (dolist (child (ptui.ui.elements:ui-element-children node))
                 (normalize-node! child)))
             (apply-constraint-id-remaps! (node)
               (when (eq (ptui.ui.elements:ui-element-type node) :constraint-layout)
                 (let ((constraints
                         (getf (ptui.ui.elements:ui-element-props node) :constraints)))
                   (dolist (spec constraints)
                     (let* ((spec-id (ptui.layout.constraints:constraint-spec-id spec))
                            (remap (cdr (assoc spec-id id-remaps :test #'eq))))
                       (when remap
                         (setf (ptui.layout.constraints:constraint-spec-id spec) remap))))))
               (dolist (child (ptui.ui.elements:ui-element-children node))
                 (apply-constraint-id-remaps! child))))
      (normalize-node! tree)
      (when id-remaps
        (apply-constraint-id-remaps! tree)))
    tree))
