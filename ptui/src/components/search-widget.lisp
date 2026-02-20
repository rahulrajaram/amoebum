(defpackage :ptui.components.search-widget
  (:use :cl)
  (:export
   #:search-widget-state
   #:make-search-widget-state
   #:search-widget-mode
   #:search-widget-query
   #:search-widget-status
   #:search-widget-results
   #:search-widget-selected-index
   #:search-widget-visible-count
   #:search-widget-limit
   #:search-widget-regex-mode-p
   #:search-widget-case-insensitive-p
   #:search-widget-multiline-mode-p
   #:search-widget-before-context
   #:search-widget-after-context
   #:search-widget-set-file-candidates
   #:search-widget-set-documents
   #:search-widget-refresh
   #:search-widget-handle-event
   #:search-widget-selected-result
   #:search-widget-visible-results
   #:make-search-widget))

(in-package :ptui.components.search-widget)

(defstruct (search-widget-state
            (:constructor %make-search-widget-state
                (&key
                  (mode :files)
                  (query "")
                  (file-candidates #())
                  (documents #())
                  (results '())
                  (selected-index 0)
                  (visible-count 10)
                  (limit 50)
                  (status :idle)
                  (prompt "search> ")
                  (empty-message "[no matches]")
                  (regex-mode t)
                  (case-insensitive nil)
                  (multiline-mode nil)
                  (before-context 0)
                  (after-context 0)
                  on-select)))
  (mode :files :type keyword)
  (query "" :type string)
  (file-candidates #() :type vector)
  (documents #() :type vector)
  (results '() :type list)
  (selected-index 0 :type fixnum)
  (visible-count 10 :type fixnum)
  (limit 50 :type (or null (integer 1 *)))
  (status :idle :type keyword)
  (prompt "search> " :type string)
  (empty-message "[no matches]" :type string)
  (regex-mode t :type boolean)
  (case-insensitive nil :type boolean)
  (multiline-mode nil :type boolean)
  (before-context 0 :type fixnum)
  (after-context 0 :type fixnum)
  (on-select nil :type (or null function)))

(defun %normalize-mode (mode)
  (unless (member mode '(:files :content) :test #'eq)
    (error "MODE must be :files or :content. Got ~S." mode))
  mode)

(defun %normalize-candidate-path (candidate)
  (let ((text (typecase candidate
                (pathname (namestring candidate))
                (string candidate)
                (t (princ-to-string candidate)))))
    (substitute #\/ #\\ text)))

(defun %normalize-file-candidates (candidates)
  (coerce (map 'list #'%normalize-candidate-path (or candidates #())) 'vector))

(defun %coerce-search-document (entry)
  (cond
    ((ptui.search.engine:search-document-p entry)
     entry)
    ((and (listp entry) (getf entry :path) (getf entry :content))
     (ptui.search.engine:make-search-document
      :path (%normalize-candidate-path (getf entry :path))
      :content (princ-to-string (getf entry :content))))
    (t
     (error "Expected SEARCH-DOCUMENT or plist with :path/:content, got ~S." entry))))

(defun %normalize-documents (documents)
  (coerce (map 'list #'%coerce-search-document (or documents #())) 'vector))

(defun %clamp-selected-index! (state)
  (let ((count (length (search-widget-state-results state))))
    (setf (search-widget-state-selected-index state)
          (if (zerop count)
              0
              (max 0 (min (search-widget-state-selected-index state)
                          (1- count)))))))

(defun %recompute-results! (state)
  (let* ((mode (search-widget-state-mode state))
         (query (search-widget-state-query state))
         (limit (search-widget-state-limit state))
         (results
           (case mode
             (:files
              (ptui.search.engine:rank-file-matches
               query
               (search-widget-state-file-candidates state)
               :limit limit))
             (:content
              (ptui.search.engine:search-content-matches
               query
               (search-widget-state-documents state)
               :limit limit
               :regex-mode (search-widget-state-regex-mode state)
               :case-insensitive (search-widget-state-case-insensitive state)
               :multiline-mode (search-widget-state-multiline-mode state)
               :before-context (search-widget-state-before-context state)
               :after-context (search-widget-state-after-context state)))
             (t '()))))
    (setf (search-widget-state-results state) results
          (search-widget-state-status state)
          (cond
            ((and (eq mode :content) (zerop (length query))) :idle)
            ((null results) :empty)
            (t :ready)))
    (%clamp-selected-index! state)
    state))

(defun make-search-widget-state (&key
                                   (mode :files)
                                   (query "")
                                   (file-candidates #())
                                   (documents #())
                                   (visible-count 10)
                                   (limit 50)
                                   (prompt "search> ")
                                   (empty-message "[no matches]")
                                   (regex-mode t)
                                   (case-insensitive nil)
                                   (multiline-mode nil)
                                   (before-context 0)
                                   (after-context 0)
                                   on-select)
  "Create mutable state for the search widget."
  (check-type query string)
  (check-type visible-count (integer 1 *))
  (when limit
    (check-type limit (integer 1 *)))
  (check-type prompt string)
  (check-type empty-message string)
  (check-type before-context (integer 0 *))
  (check-type after-context (integer 0 *))
  (when on-select
    (check-type on-select function))
  (let ((state (%make-search-widget-state
                :mode (%normalize-mode mode)
                :query query
                :file-candidates (%normalize-file-candidates file-candidates)
                :documents (%normalize-documents documents)
                :visible-count visible-count
                :limit limit
                :prompt prompt
                :empty-message empty-message
                :regex-mode (not (null regex-mode))
                :case-insensitive (not (null case-insensitive))
                :multiline-mode (not (null multiline-mode))
                :before-context before-context
                :after-context after-context
                :on-select on-select)))
    (%recompute-results! state)))

(defun search-widget-mode (state)
  (search-widget-state-mode state))

(defun (setf search-widget-mode) (value state)
  (setf (search-widget-state-mode state) (%normalize-mode value))
  (%recompute-results! state)
  value)

(defun search-widget-query (state)
  (search-widget-state-query state))

(defun (setf search-widget-query) (value state)
  (check-type value string)
  (setf (search-widget-state-query state) value)
  (%recompute-results! state)
  value)

(defun search-widget-status (state)
  (search-widget-state-status state))

(defun search-widget-results (state)
  (search-widget-state-results state))

(defun search-widget-selected-index (state)
  (search-widget-state-selected-index state))

(defun (setf search-widget-selected-index) (value state)
  (check-type value (integer 0 *))
  (setf (search-widget-state-selected-index state) value)
  (%clamp-selected-index! state)
  value)

(defun search-widget-visible-count (state)
  (search-widget-state-visible-count state))

(defun search-widget-limit (state)
  (search-widget-state-limit state))

(defun search-widget-regex-mode-p (state)
  (search-widget-state-regex-mode state))

(defun search-widget-case-insensitive-p (state)
  (search-widget-state-case-insensitive state))

(defun search-widget-multiline-mode-p (state)
  (search-widget-state-multiline-mode state))

(defun search-widget-before-context (state)
  (search-widget-state-before-context state))

(defun search-widget-after-context (state)
  (search-widget-state-after-context state))

(defun search-widget-refresh (state)
  "Recompute results from current query/source/options."
  (check-type state search-widget-state)
  (%recompute-results! state))

(defun search-widget-set-file-candidates (state candidates)
  "Replace the widget file candidate corpus."
  (check-type state search-widget-state)
  (setf (search-widget-state-file-candidates state)
        (%normalize-file-candidates candidates))
  (when (eq (search-widget-state-mode state) :files)
    (%recompute-results! state))
  state)

(defun search-widget-set-documents (state documents)
  "Replace the widget content-search document corpus."
  (check-type state search-widget-state)
  (setf (search-widget-state-documents state)
        (%normalize-documents documents))
  (when (eq (search-widget-state-mode state) :content)
    (%recompute-results! state))
  state)

(defun search-widget-selected-result (state)
  "Return the currently selected result object or NIL."
  (check-type state search-widget-state)
  (nth (search-widget-state-selected-index state)
       (search-widget-state-results state)))

(defun %visible-window (state)
  (let* ((results (search-widget-state-results state))
         (count (length results))
         (visible (search-widget-state-visible-count state))
         (selected (search-widget-state-selected-index state))
         (start (if (<= count visible)
                    0
                    (min (max 0 (- selected (1- visible)))
                         (- count visible))))
         (end (min count (+ start visible))))
    (values (subseq results start end) start)))

(defun search-widget-visible-results (state)
  "Return currently visible result objects."
  (check-type state search-widget-state)
  (nth-value 0 (%visible-window state)))

(defun %move-selection! (state key)
  (let ((count (length (search-widget-state-results state))))
    (cond
      ((zerop count)
       (setf (search-widget-state-selected-index state) 0))
      ((eq key :up)
       (setf (search-widget-state-selected-index state)
             (max 0 (1- (search-widget-state-selected-index state)))))
      ((eq key :down)
       (setf (search-widget-state-selected-index state)
             (min (1- count) (1+ (search-widget-state-selected-index state)))))
      ((eq key :home)
       (setf (search-widget-state-selected-index state) 0))
      ((eq key :end)
       (setf (search-widget-state-selected-index state) (1- count)))))
  state)

(defun search-widget-handle-event (state event)
  "Apply EVENT to STATE and return a plist describing the action."
  (check-type state search-widget-state)
  (unless (typep event 'ptui.core.events:key-event)
    (return-from search-widget-handle-event
      (list :action :ignored :state state)))
  (let* ((key (ptui.core.events:key-event-key event))
         (text (ptui.core.events:key-event-text? event)))
    (cond
      ((and (stringp text) (> (length text) 0))
       (setf (search-widget-query state)
             (concatenate 'string (search-widget-state-query state) text))
       (list :action :query-updated :state state))
      ((eq key :backspace)
       (let ((query (search-widget-state-query state)))
         (when (> (length query) 0)
           (setf (search-widget-query state)
                 (subseq query 0 (1- (length query))))))
       (list :action :query-updated :state state))
      ((member key '(:up :down :home :end) :test #'eq)
       (%move-selection! state key)
       (list :action :selection-moved :state state))
      ((member key '(:escape :ctrl-c) :test #'eq)
       (setf (search-widget-query state) "")
       (list :action :cleared :state state))
      ((eq key :enter)
       (let ((selected (search-widget-selected-result state)))
         (when (and selected (search-widget-state-on-select state))
           (funcall (search-widget-state-on-select state) selected state))
         (list :action :selected
               :state state
               :result selected)))
      (t
       (list :action :ignored :state state)))))

(defun %status-line (state)
  (let ((count (length (search-widget-state-results state))))
    (format nil "~A~A (~D result~:P)"
            (search-widget-state-prompt state)
            (case (search-widget-state-status state)
              (:ready "ready")
              (:empty "empty")
              (:idle "idle")
              (t "state"))
            count)))

(defun %file-result-label (result)
  (ptui.search.engine:search-file-match-path result))

(defun %content-result-label (result)
  (format nil "~A:~D:~D ~A"
          (ptui.search.engine:search-content-match-path result)
          (ptui.search.engine:search-content-match-line result)
          (ptui.search.engine:search-content-match-column result)
          (ptui.search.engine:search-content-match-text result)))

(defun %result-label (state result)
  (case (search-widget-state-mode state)
    (:files (%file-result-label result))
    (:content (%content-result-label result))
    (t (princ-to-string result))))

(defun make-search-widget (state &key id key (input-id :search-input) (borderp t) (padding 0))
  "Build a composable PTUI element tree for search interaction."
  (check-type state search-widget-state)
  (let* ((input (ptui.widgets.core:make-input-widget
                 (search-widget-state-query state)
                 :id input-id
                 :min-width 1
                 :on-event (lambda (event node)
                             (declare (ignore node))
                             (search-widget-handle-event state event))))
         (status (ptui.widgets.core:make-text-widget (%status-line state)))
         (rows
           (multiple-value-bind (visible start) (%visible-window state)
             (if visible
                 (loop for result in visible
                       for offset from 0
                       for absolute = (+ start offset)
                       collect (ptui.widgets.core:make-text-widget
                                (format nil "~A ~A"
                                        (if (= absolute (search-widget-state-selected-index state))
                                            ">"
                                            " ")
                                        (%result-label state result))))
                 (list (ptui.widgets.core:make-text-widget
                        (search-widget-state-empty-message state))))))
         (content (ptui.widgets.core:make-stack-widget
                   (append (list input status) rows)
                   :direction :column
                   :gap 0)))
    (ptui.widgets.core:make-box-widget content
                                       :id id
                                       :key key
                                       :padding padding
                                       :borderp borderp)))
