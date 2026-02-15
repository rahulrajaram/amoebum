(in-package :amoebum)

(defparameter +tree-browser-default-visible-row-count+ 10)

(defstruct (tree-node
            (:constructor make-tree-node
                (&key
                  (label "")
                  children-fn
                  (expanded-p nil)
                  (metadata '()))))
  (label "" :type string)
  children-fn
  (expanded-p nil :type boolean)
  (metadata '() :type list)
  (children-loaded-p nil :type boolean)
  (children-cache '() :type list))

(defstruct (tree-browser-state
            (:constructor make-tree-browser-state
                (&key
                  root-node
                  (selected-index 0)
                  (scroll-offset 0)
                  (visible-row-count +tree-browser-default-visible-row-count+)
                  (show-root-p t)
                  (active-p t)
                  (generation 0)
                  root-path
                  (git-status-map (make-hash-table :test #'equal))
                  (git-directory-status-map (make-hash-table :test #'equal))
                  (git-deleted-paths '())
                  (status-generation 0))))
  root-node
  (selected-index 0 :type fixnum)
  (scroll-offset 0 :type fixnum)
  (visible-row-count +tree-browser-default-visible-row-count+ :type fixnum)
  (show-root-p t :type boolean)
  (active-p t :type boolean)
  (generation 0 :type fixnum)
  root-path
  (git-status-map (make-hash-table :test #'equal))
  (git-directory-status-map (make-hash-table :test #'equal))
  (git-deleted-paths '() :type list)
  (status-generation 0 :type fixnum))

(defun %tree-default-root ()
  (let ((cfg (ignore-errors (current-config))))
    (or (and (config-p cfg)
             (config-project-root cfg))
        (ignore-errors (uiop:getcwd))
        *default-pathname-defaults*)))

(defun %tree-resolve-root (root)
  (%resolve-search-root (or root (%tree-default-root))))

(defun %tree-stringify (value)
  (cond
    ((stringp value) value)
    ((symbolp value) (symbol-name value))
    ((null value) "")
    (t (princ-to-string value))))

(defun %tree-bump-generation! (state)
  (incf (tree-browser-state-generation state))
  state)

(defun %tree-node-sort-key (node)
  (string-downcase (tree-node-label node)))

(defun %tree-node-directory-p (node)
  (not (null (getf (tree-node-metadata node) :directory-p))))

(defun %tree-normalize-child (child)
  (cond
    ((typep child 'tree-node)
     child)
    ((or (stringp child) (symbolp child))
     (make-tree-node :label (%tree-stringify child)))
    (t
     (error "Tree child must be TREE-NODE, string, or symbol. Got ~S." child))))

(defun %tree-node-children (node)
  (check-type node tree-node)
  (when (not (tree-node-children-loaded-p node))
    (let ((children
            (if (tree-node-children-fn node)
                (funcall (tree-node-children-fn node))
                '())))
      (setf (tree-node-children-cache node)
            (mapcar #'%tree-normalize-child (or children '()))
            (tree-node-children-loaded-p node) t)))
  (tree-node-children-cache node))

(defun %tree-node-has-children-p (node)
  (or (tree-node-children-fn node)
      (and (tree-node-children-loaded-p node)
           (not (null (tree-node-children-cache node))))))

(defun %tree-sorted-children (nodes)
  (sort nodes
        (lambda (left right)
          (cond
            ((and (%tree-node-directory-p left)
                  (not (%tree-node-directory-p right)))
             t)
            ((and (not (%tree-node-directory-p left))
                  (%tree-node-directory-p right))
             nil)
            (t
             (string< (%tree-node-sort-key left)
                      (%tree-node-sort-key right)))))))

(defun %tree-clamp-selection! (state entries)
  (let ((max-index (max 0 (1- (length entries)))))
    (setf (tree-browser-state-selected-index state)
          (max 0 (min max-index (tree-browser-state-selected-index state)))))
  state)

(defun %tree-sync-scroll! (state entries)
  (let* ((selected (tree-browser-state-selected-index state))
         (offset (max 0 (tree-browser-state-scroll-offset state)))
         (row-count (max 1 (tree-browser-state-visible-row-count state)))
         (total (length entries))
         (max-offset (max 0 (- total row-count))))
    (when (< selected offset)
      (setf offset selected))
    (when (>= selected (+ offset row-count))
      (setf offset (- selected row-count -1)))
    (setf (tree-browser-state-scroll-offset state)
          (max 0 (min max-offset offset))))
  state)

(defun tree-browser-visible-entries (state)
  (check-type state tree-browser-state)
  (let ((entries (make-array 0 :adjustable t :fill-pointer 0))
        (root (tree-browser-state-root-node state)))
    (labels ((emit-entry (node depth parent-index)
               (let ((entry-index (fill-pointer entries)))
                 (vector-push-extend
                  (list :index entry-index
                        :node node
                        :depth depth
                        :parent-index parent-index)
                  entries)
                 (when (tree-node-expanded-p node)
                   (dolist (child (%tree-node-children node))
                     (emit-entry child (1+ depth) entry-index))))))
      (when root
        (if (tree-browser-state-show-root-p state)
            (emit-entry root 0 nil)
            (dolist (child (%tree-node-children root))
              (emit-entry child 0 nil)))))
    (let ((result (coerce entries 'list)))
      (%tree-clamp-selection! state result)
      (%tree-sync-scroll! state result)
      result)))

(defun tree-browser-selected-entry (state)
  (check-type state tree-browser-state)
  (let* ((entries (tree-browser-visible-entries state))
         (index (tree-browser-state-selected-index state)))
    (and (>= index 0)
         (< index (length entries))
         (nth index entries))))

(defun tree-browser-selected-node (state)
  (let ((entry (tree-browser-selected-entry state)))
    (and entry
         (getf entry :node))))

(defun tree-browser-move-selection! (state delta)
  (check-type state tree-browser-state)
  (let* ((entries (tree-browser-visible-entries state))
         (max-index (max 0 (1- (length entries)))))
    (setf (tree-browser-state-selected-index state)
          (max 0
               (min max-index
                    (+ (tree-browser-state-selected-index state)
                       (or delta 0)))))
    (%tree-sync-scroll! state entries))
  state)

(defun %tree-expand-node! (state node)
  (when (and (%tree-node-has-children-p node)
             (not (tree-node-expanded-p node)))
    (setf (tree-node-expanded-p node) t)
    (%tree-node-children node)
    (%tree-bump-generation! state)
    t))

(defun %tree-collapse-node! (state node)
  (when (tree-node-expanded-p node)
    (setf (tree-node-expanded-p node) nil)
    (%tree-bump-generation! state)
    t))

(defun tree-browser-toggle-selected! (state)
  (check-type state tree-browser-state)
  (let* ((entry (tree-browser-selected-entry state))
         (node (and entry (getf entry :node))))
    (when (and node (%tree-node-has-children-p node))
      (if (tree-node-expanded-p node)
          (%tree-collapse-node! state node)
          (%tree-expand-node! state node)))))

(defun tree-browser-collapse-selected! (state)
  (check-type state tree-browser-state)
  (let* ((entries (tree-browser-visible-entries state))
         (entry (tree-browser-selected-entry state))
         (node (and entry (getf entry :node)))
         (parent-index (and entry (getf entry :parent-index))))
    (cond
      ((and node (tree-node-expanded-p node))
       (%tree-collapse-node! state node))
      ((and parent-index (>= parent-index 0))
       (setf (tree-browser-state-selected-index state) parent-index)
       (%tree-sync-scroll! state entries)
       t)
      (t
       nil))))

(defun tree-browser-expand-selected! (state)
  (check-type state tree-browser-state)
  (let* ((entries (tree-browser-visible-entries state))
         (entry (tree-browser-selected-entry state))
         (node (and entry (getf entry :node))))
    (cond
      ((null node)
       nil)
      ((not (%tree-node-has-children-p node))
       nil)
      ((not (tree-node-expanded-p node))
       (%tree-expand-node! state node))
      (t
       (let* ((index (tree-browser-state-selected-index state))
              (child-index (1+ index))
              (current-depth (getf entry :depth))
              (child-entry (and (< child-index (length entries))
                                (nth child-index entries))))
         (if (and child-entry
                  (> (getf child-entry :depth) current-depth))
             (progn
               (setf (tree-browser-state-selected-index state) child-index)
               (%tree-sync-scroll! state entries)
               t)
             nil))))))

(defun tree-browser-handle-key! (state key)
  (check-type state tree-browser-state)
  (case key
    (:up
     (tree-browser-move-selection! state -1)
     t)
    (:down
     (tree-browser-move-selection! state 1)
     t)
    ((:enter :return)
     (not (null (tree-browser-toggle-selected! state))))
    (:left
     (not (null (tree-browser-collapse-selected! state))))
    (:right
     (not (null (tree-browser-expand-selected! state))))
    (otherwise
     nil)))

(defun %tree-git-status-priority (indicator)
  (case (and indicator (char indicator 0))
    (#\D 4)
    (#\A 3)
    (#\M 2)
    (#\? 1)
    (otherwise 0)))

(defun %tree-git-better-status-p (candidate current)
  (> (%tree-git-status-priority candidate)
     (%tree-git-status-priority current)))

(defun %tree-git-store-status! (table path indicator)
  (let* ((key (%normalize-slashes (or path "")))
         (current (gethash key table)))
    (when (or (null current)
              (%tree-git-better-status-p indicator current))
      (setf (gethash key table) indicator))))

(defun %tree-git-parent-directories (path)
  (let ((parents '())
        (cursor path))
    (loop
      for slash = (position #\/ cursor :from-end t)
      while slash do
        (setf cursor (subseq cursor 0 slash))
        (when (plusp (length cursor))
          (push cursor parents)))
    parents))

(defun %tree-git-parse-status-line (line)
  (let* ((value (or line ""))
         (length* (length value)))
    (cond
      ((< length* 3)
       (values nil nil nil))
      ((string= value "?? " :end1 3 :end2 3)
       (values (%normalize-slashes (subseq value 3)) "?" nil))
      ((string= value "!! " :end1 3 :end2 3)
       (values nil nil nil))
      (t
       (let* ((xy (subseq value 0 2))
              (path-raw (subseq value 3))
              (rename-marker (search " -> " path-raw :test #'char=))
              (path (if rename-marker
                        (subseq path-raw (+ rename-marker 4))
                        path-raw))
              (indicator
                (cond
                  ((or (find #\D xy) (find #\d xy)) "D")
                  ((or (find #\A xy) (find #\a xy)) "A")
                  ((or (find #\? xy)) "?")
                  ((or (find #\M xy)
                       (find #\R xy)
                       (find #\C xy)
                       (find #\U xy))
                   "M")
                  (t
                   nil))))
         (values (%normalize-slashes path)
                 indicator
                 (and indicator (string= indicator "D"))))))))

(defun %tree-git-status-tables (root)
  (let ((file-statuses (make-hash-table :test #'equal))
        (directory-statuses (make-hash-table :test #'equal))
        (deleted-paths '()))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program '("git" "status" "--porcelain")
                          :directory root
                          :ignore-error-status t
                          :output :string
                          :error-output :string)
      (declare (ignore stderr))
      (when (zerop (or exit-code 1))
        (with-input-from-string (stream (or stdout ""))
          (loop for line = (read-line stream nil nil)
                while line do
                  (multiple-value-bind (path indicator deleted-p)
                      (%tree-git-parse-status-line line)
                    (when (and path indicator (plusp (length path)))
                      (%tree-git-store-status! file-statuses path indicator)
                      (dolist (parent (%tree-git-parent-directories path))
                        (%tree-git-store-status! directory-statuses parent indicator))
                      (when deleted-p
                        (pushnew path deleted-paths :test #'string=))))))))
    (values file-statuses
            directory-statuses
            (sort deleted-paths #'string<))))

(defun tree-browser-refresh-git-status! (state)
  (check-type state tree-browser-state)
  (let ((root (tree-browser-state-root-path state)))
    (if (and root (probe-file root))
        (multiple-value-bind (file-statuses directory-statuses deleted-paths)
            (%tree-git-status-tables root)
          (setf (tree-browser-state-git-status-map state) file-statuses
                (tree-browser-state-git-directory-status-map state) directory-statuses
                (tree-browser-state-git-deleted-paths state) deleted-paths))
        (setf (tree-browser-state-git-status-map state) (make-hash-table :test #'equal)
              (tree-browser-state-git-directory-status-map state) (make-hash-table :test #'equal)
              (tree-browser-state-git-deleted-paths state) '())))
  (incf (tree-browser-state-status-generation state))
  state)

(defun %tree-basename (relative-path)
  (let ((trimmed (string-trim "/" (or relative-path ""))))
    (if (zerop (length trimmed))
        trimmed
        (let ((slash (position #\/ trimmed :from-end t)))
          (if slash
              (subseq trimmed (1+ slash))
              trimmed)))))

(defun %tree-relative-from-parent (parent-relative child-relative)
  (let* ((parent (string-trim "/" (or parent-relative "")))
         (child (string-trim "/" (or child-relative ""))))
    (cond
      ((zerop (length child))
       "")
      ((zerop (length parent))
       child)
      ((uiop:string-prefix-p (concatenate 'string parent "/") child)
       (subseq child (1+ (length parent))))
      (t
       ""))))

(defun %tree-immediate-deleted-child-path-p (parent-relative candidate)
  (let* ((tail (%tree-relative-from-parent parent-relative candidate))
         (slash (position #\/ tail)))
    (and (plusp (length tail))
         (null slash))))

(defun %tree-root-label (root)
  (let* ((name (%path-text (uiop:ensure-directory-pathname root)))
         (trimmed (string-right-trim "/" name))
         (slash (position #\/ trimmed :from-end t)))
    (if (and slash (< slash (1- (length trimmed))))
        (subseq trimmed (1+ slash))
        trimmed)))

(defun %tree-status-for-path (state path directory-p)
  (let* ((key (%normalize-slashes (or path "")))
         (table (if directory-p
                    (tree-browser-state-git-directory-status-map state)
                    (tree-browser-state-git-status-map state))))
    (and (plusp (length key))
         (gethash key table))))

(defun %tree-directory-children (state directory root parent-relative)
  (let ((seen (make-hash-table :test #'equal))
        (nodes '()))
    (labels ((add-node (node)
               (let* ((metadata (tree-node-metadata node))
                      (path (%normalize-slashes (or (getf metadata :path) ""))))
                 (unless (gethash path seen)
                   (setf (gethash path seen) t)
                   (push node nodes)))))
      (dolist (subdir (or (ignore-errors (uiop:subdirectories directory)) '()))
        (let* ((relative (%relative-path-text subdir root))
               (name (%tree-basename relative)))
          (unless (string= name ".git")
            (add-node
             (make-tree-node
              :label name
              :expanded-p nil
              :metadata (list :path relative
                              :directory-p t)
              :children-fn
              (let ((next-dir subdir)
                    (next-relative relative))
                (lambda ()
                  (%tree-directory-children state
                                            next-dir
                                            root
                                            next-relative))))))))
      (dolist (file (or (ignore-errors (uiop:directory-files directory)) '()))
        (when (%regular-file-p file)
          (let* ((relative (%relative-path-text file root))
                 (name (%tree-basename relative)))
            (add-node
             (make-tree-node
              :label name
              :metadata (list :path relative
                              :directory-p nil))))))
      (dolist (deleted-path (tree-browser-state-git-deleted-paths state))
        (when (%tree-immediate-deleted-child-path-p parent-relative deleted-path)
          (let ((name (%tree-basename deleted-path)))
            (add-node
             (make-tree-node
              :label name
              :metadata (list :path deleted-path
                              :directory-p nil
                              :deleted-p t))))))
      (%tree-sorted-children (nreverse nodes)))))

(defun make-git-file-tree-browser-state (&key root
                                              (show-root-p t)
                                              (active-p t)
                                              (visible-row-count +tree-browser-default-visible-row-count+))
  (let* ((resolved-root (%tree-resolve-root root))
         (state (make-tree-browser-state
                 :show-root-p show-root-p
                 :active-p active-p
                 :visible-row-count visible-row-count
                 :root-path resolved-root))
         (root-node
           (make-tree-node
            :label (%tree-root-label resolved-root)
            :expanded-p t
            :metadata (list :path ""
                            :directory-p t)
            :children-fn
            (let ((root-dir resolved-root))
              (lambda ()
                (%tree-directory-children state root-dir root-dir ""))))))
    (setf (tree-browser-state-root-node state) root-node)
    (tree-browser-refresh-git-status! state)
    state))

(defun make-empty-tree-browser-state (&key (label "files"))
  (make-tree-browser-state
   :root-node (make-tree-node :label label :expanded-p t :children-fn (lambda () '()))
   :show-root-p t
   :active-p t
   :visible-row-count +tree-browser-default-visible-row-count+))

(defun tree-browser-render-key (state)
  (check-type state tree-browser-state)
  (list :tree-browser
        (tree-browser-state-active-p state)
        (tree-browser-state-generation state)
        (tree-browser-state-status-generation state)
        (tree-browser-state-selected-index state)
        (tree-browser-state-scroll-offset state)
        (tree-browser-state-visible-row-count state)
        (tree-browser-state-show-root-p state)))

(defun %tree-row-prefix (selected-p)
  (if selected-p "> " "  "))

(defun %tree-row-expander (node)
  (cond
    ((not (%tree-node-has-children-p node))
     " ")
    ((tree-node-expanded-p node)
     "-")
    (t
     "+")))

(defun %tree-status-role (status)
  (case (and status (char status 0))
    (#\M :context-yellow)
    (#\A :context-green)
    (#\D :context-red)
    (#\? :tool)
    (otherwise :meta)))

(defun %tree-row-segments (state entry selected-p)
  (let* ((node (getf entry :node))
         (depth (getf entry :depth))
         (metadata (tree-node-metadata node))
         (path (getf metadata :path))
         (directory-p (not (null (getf metadata :directory-p))))
         (status (%tree-status-for-path state path directory-p))
         (base-role (if selected-p :assistant :meta))
         (label (if directory-p
                    (concatenate 'string (tree-node-label node) "/")
                    (tree-node-label node))))
    (list (cons (%tree-row-prefix selected-p) base-role)
          (cons (make-string (* 2 (max 0 depth)) :initial-element #\Space) :meta)
          (cons (%tree-row-expander node) :meta)
          (cons " " :meta)
          (cons (format nil "[~A] " (or status " ")) (%tree-status-role status))
          (cons label base-role))))

(defun %tree-segments->text (segments)
  (with-output-to-string (stream)
    (dolist (segment segments)
      (write-string (car segment) stream))))

(defun %tree-header-element (state)
  (let* ((root (or (tree-browser-state-root-path state)
                   (%tree-default-root)))
         (changed-count (hash-table-count (tree-browser-state-git-status-map state)))
         (text (format nil "files: ~A (~D changed) [up/down enter left right]"
                       (%path-text root)
                       changed-count)))
    (ptui.ui.elements:make-element
     :text
     :id :tree-browser-header
     :props (list :text text
                  :role :meta)
     :children '())))

(defun %tree-empty-element ()
  (ptui.ui.elements:make-element
   :text
   :id :tree-browser-empty
   :props (list :text "  [ ] no files"
                :role :meta)
   :children '()))

(defun %tree-row-elements (state)
  (let* ((entries (tree-browser-visible-entries state))
         (count (length entries))
         (offset (tree-browser-state-scroll-offset state))
         (row-count (max 1 (tree-browser-state-visible-row-count state)))
         (start (min offset count))
         (end (min count (+ start row-count)))
         (selected-index (tree-browser-state-selected-index state)))
    (if (zerop count)
        (list (%tree-empty-element))
        (loop for absolute-index from start below end
              for entry = (nth absolute-index entries)
              for node = (getf entry :node)
              for selected-p = (= absolute-index selected-index)
              for segments = (%tree-row-segments state entry selected-p)
              collect
              (ptui.ui.elements:make-element
               :text
               :id (list :tree-browser-row absolute-index
                         (getf (tree-node-metadata node) :path))
               :props (list :text (%tree-segments->text segments)
                            :role (if selected-p :assistant :meta)
                            :styled-segments segments)
               :children '())))))

(ptui.widgets.defwidget:defwidget make-tree-browser-widget (state)
  (:memoize nil)
  (box
   (vstack
    (%tree-header-element state)
    (map-widget #'identity (%tree-row-elements state)))
   :id :tree-browser
   :border t))
