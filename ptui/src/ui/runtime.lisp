(defpackage :ptui.ui.runtime
  (:use :cl)
  (:export
   #:patch-op
   #:patch-op-kind
   #:patch-op-node-id
   #:patch-op-path
   #:patch-op-detail
   #:runtime
   #:make-runtime
   #:runtime-root
   #:runtime-revision
   #:runtime-focus-order
   #:runtime-focus-id
   #:runtime-lifecycle-log
   #:reconcile-trees
   #:collect-focus-order
   #:update-runtime
   #:enqueue-effect
   #:runtime-state
   #:set-runtime-state
   #:advance-focus
   #:route-event))

(in-package :ptui.ui.runtime)

(defstruct (patch-op
            (:constructor make-patch-op (kind node-id path &key detail)))
  (kind :update :type keyword)
  (node-id nil)
  (path '() :type list)
  (detail nil))

(defstruct (runtime
            (:constructor %make-runtime
                (&key root revision state-table pending-effects lifecycle-log
                      focus-order focus-id)))
  (root nil)
  (revision 0 :type fixnum)
  (state-table (make-hash-table :test #'equal))
  (pending-effects '() :type list)
  (lifecycle-log '() :type list)
  (focus-order '() :type list)
  (focus-id nil))

(defun make-runtime ()
  (%make-runtime))

(defun %log! (runtime event)
  (setf (runtime-lifecycle-log runtime)
        (nconc (runtime-lifecycle-log runtime) (list event)))
  runtime)

(defun %normalize-path (path)
  (if path path (list :root)))

(defun %child-selector (child index)
  (or (ptui.ui.elements:ui-element-key child)
      (ptui.ui.elements:ui-element-id child)
      index))

(defun %path->node-id (node path)
  (or (ptui.ui.elements:ui-element-id node)
      (ptui.ui.elements:ui-element-key node)
      (format nil "~{~A~^.~}" path)))

(defun %identity-compatible-p (old new)
  (and (eql (ptui.ui.elements:ui-element-type old)
            (ptui.ui.elements:ui-element-type new))
       (equal (ptui.ui.elements:ui-element-key old)
              (ptui.ui.elements:ui-element-key new))
       (equal (ptui.ui.elements:ui-element-id old)
              (ptui.ui.elements:ui-element-id new))))

(defun %props-changed-p (old new)
  (or (not (equal (ptui.ui.elements:ui-element-props old)
                  (ptui.ui.elements:ui-element-props new)))
      (not (equal (ptui.ui.elements:ui-element-focusablep old)
                  (ptui.ui.elements:ui-element-focusablep new)))))

(defun %collect-mount-ops (node path)
  (let ((ops (list (make-patch-op :mount
                                  (%path->node-id node path)
                                  (%normalize-path path)))))
    (loop for child in (ptui.ui.elements:ui-element-children node)
          for idx from 0 do
            (setf ops
                  (nconc ops
                         (%collect-mount-ops child
                                            (append path (list (%child-selector child idx)))))))
    ops))

(defun %collect-unmount-ops (node path)
  (let ((ops '()))
    (loop for child in (ptui.ui.elements:ui-element-children node)
          for idx from 0 do
            (setf ops
                  (nconc ops
                         (%collect-unmount-ops child
                                              (append path (list (%child-selector child idx)))))))
    (nconc ops
           (list (make-patch-op :unmount
                                (%path->node-id node path)
                                (%normalize-path path))))))

(defun %child-map (children)
  (let ((map (make-hash-table :test #'equal)))
    (loop for child in children
          for idx from 0 do
            (setf (gethash (%child-selector child idx) map) child))
    map))

(defun %ordered-child-selectors (old-children new-children)
  (let ((seen (make-hash-table :test #'equal))
        (order '()))
    (loop for child in new-children
          for idx from 0 do
            (let ((selector (%child-selector child idx)))
              (unless (gethash selector seen)
                (setf (gethash selector seen) t)
                (push selector order))))
    (loop for child in old-children
          for idx from 0 do
            (let ((selector (%child-selector child idx)))
              (unless (gethash selector seen)
                (setf (gethash selector seen) t)
                (push selector order))))
    (nreverse order)))

(defun reconcile-trees (old-root new-root &key (path (list :root)))
  "Reconcile two element trees into deterministic patch operations."
  (cond
    ((and (null old-root) (null new-root))
     '())
    ((null old-root)
     (%collect-mount-ops new-root path))
    ((null new-root)
     (%collect-unmount-ops old-root path))
    ((not (%identity-compatible-p old-root new-root))
     (nconc (%collect-unmount-ops old-root path)
            (%collect-mount-ops new-root path)))
    (t
     (let ((ops '()))
       (when (%props-changed-p old-root new-root)
         (setf ops
               (nconc ops
                      (list
                       (make-patch-op :update
                                      (%path->node-id new-root path)
                                      (%normalize-path path)
                                      :detail :props)))))
       (let* ((old-children (ptui.ui.elements:ui-element-children old-root))
              (new-children (ptui.ui.elements:ui-element-children new-root))
              (old-map (%child-map old-children))
              (new-map (%child-map new-children)))
         (dolist (selector (%ordered-child-selectors old-children new-children))
           (let ((old-child (gethash selector old-map))
                 (new-child (gethash selector new-map)))
             (setf ops
                   (nconc ops
                          (reconcile-trees
                           old-child
                           new-child
                           :path (append path (list selector))))))))
       ops))))

(defun collect-focus-order (root &key (path (list :root)))
  "Collect focusable node ids in stable pre-order traversal."
  (if (null root)
      '()
      (let ((out '()))
        (labels ((walk (node node-path)
                   (when (ptui.ui.elements:ui-element-focusablep node)
                     (push (%path->node-id node node-path) out))
                   (loop for child in (ptui.ui.elements:ui-element-children node)
                         for idx from 0 do
                           (walk child
                                 (append node-path (list (%child-selector child idx)))))))
          (walk root path))
        (nreverse out))))

(defun %stabilize-focus! (runtime)
  (let ((order (runtime-focus-order runtime))
        (current (runtime-focus-id runtime)))
    (if (and current (member current order :test #'equal))
        current
        (setf (runtime-focus-id runtime) (first order)))))

(defun update-runtime (runtime new-root)
  "Apply reconciliation + commit lifecycle for a new root tree."
  (check-type runtime runtime)
  (unless (or (null new-root) (typep new-root 'ptui.ui.elements:ui-element))
    (error "NEW-ROOT must be NIL or UI-ELEMENT. Got: ~S" new-root))
  (%log! runtime :reconcile-begin)
  (let ((delta (reconcile-trees (runtime-root runtime) new-root)))
    (%log! runtime :reconcile-end)
    (setf (runtime-root runtime) new-root)
    (incf (runtime-revision runtime))
    (setf (runtime-focus-order runtime) (collect-focus-order new-root))
    (%stabilize-focus! runtime)
    (%log! runtime :commit)
    (let ((effects (runtime-pending-effects runtime))
          (index 0))
      (setf (runtime-pending-effects runtime) '())
      (dolist (effect effects)
        (incf index)
        (%log! runtime (list :effect index))
        (funcall effect)))
    delta))

(defun enqueue-effect (runtime thunk)
  (check-type runtime runtime)
  (check-type thunk function)
  (setf (runtime-pending-effects runtime)
        (nconc (runtime-pending-effects runtime) (list thunk)))
  runtime)

(defun runtime-state (runtime key &optional default)
  (check-type runtime runtime)
  (gethash key (runtime-state-table runtime) default))

(defun set-runtime-state (runtime key value)
  (check-type runtime runtime)
  (setf (gethash key (runtime-state-table runtime)) value)
  value)

(defun advance-focus (runtime &key (backward nil))
  (check-type runtime runtime)
  (let ((order (runtime-focus-order runtime)))
    (when order
      (let* ((current (runtime-focus-id runtime))
             (position (or (position current order :test #'equal) 0))
             (delta (if backward -1 1))
             (next (mod (+ position delta) (length order))))
        (setf (runtime-focus-id runtime) (nth next order)))))
  (runtime-focus-id runtime))

(defun route-event (runtime event)
  "Route event according to focus contracts.
Returns a plist with :kind and :target when applicable."
  (check-type runtime runtime)
  (cond
    ((typep event 'ptui.core.events:key-event)
     (let ((key (ptui.core.events:key-event-key event)))
       (cond
         ((eql key :tab)
          (list :kind :focus
                :target (advance-focus runtime
                                       :backward (ptui.core.events:key-event-shiftp event))
                :event event))
         ((runtime-focus-id runtime)
          (list :kind :key
                :target (runtime-focus-id runtime)
                :event event))
         (t
          (list :kind :unhandled :event event)))))
    (t
     (list :kind :unhandled :event event))))
