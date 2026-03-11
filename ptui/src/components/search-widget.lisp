(defpackage :ptui.components.search-widget
  (:use :cl)
  (:import-from :ptui.components.list-selection
                #:clamp-index
                #:move-selection
                #:visible-window)
  (:export
   #:search-widget-stream
   #:make-search-widget-stream
   #:make-sequence-search-widget-stream
   #:search-widget-state
   #:make-search-widget-state
   #:search-widget-mode
   #:search-widget-query
   #:search-widget-status
   #:search-widget-file-results
   #:search-widget-content-results
   #:search-widget-selected-index
   #:search-widget-visible-count
   #:search-widget-batch-size
   #:search-widget-match-count
   #:search-widget-scanned-count
   #:search-widget-total-count
   #:search-widget-visible-results
   #:search-widget-set-file-candidates
   #:search-widget-set-documents
   #:search-widget-start-file-search
   #:search-widget-start-content-search
   #:search-widget-start-content-search-stream
   #:search-widget-step
   #:search-widget-cancel
   #:search-widget-set-documents
   #:search-widget-rerun
   #:search-widget-results
   #:search-widget-visible-results
   #:search-widget-selected-result
   #:search-widget-handle-event
   #:make-search-widget))

(in-package :ptui.components.search-widget)

(defstruct (search-widget-stream
            (:constructor %make-search-widget-stream
                (&key next-fn cancel-fn total-items)))
  (next-fn (lambda () (values nil t)) :type function)
  (cancel-fn (lambda () nil) :type function)
  (total-items 0 :type fixnum)
  (done-p nil :type boolean)
  (cancelled-p nil :type boolean))

(defstruct (search-widget-state
            (:constructor %make-search-widget-state
                (&key
                  (mode :files)
                  (query "")
                  (file-candidates '())
                  (documents '())
                  (file-results '())
                  (content-results '())
                  (stream-all-content-results '())
                  stream
                  (selected-index 0)
                  (visible-count 10)
                  (batch-size 32)
                  (match-count 0)
                  (scanned-count 0)
                  (total-count 0)
                  (limit 200)
                  (regex-mode t)
                  (case-insensitive nil)
                  (multiline-mode nil)
                  (before-context 0)
                  (after-context 0)
                  (status :idle)
                  (on-select nil)
                  (prompt "search> ")
                  (empty-message "[no results]"))))
  (mode :files :type keyword)
  (query "" :type string)
  (file-candidates '() :type list)
  (documents '() :type list)
  (file-results '() :type list)
  (content-results '() :type list)
  (stream-all-content-results '() :type list)
  (stream nil :type (or null search-widget-stream))
  (selected-index 0 :type fixnum)
  (visible-count 10 :type fixnum)
  (batch-size 32 :type fixnum)
  (match-count 0 :type fixnum)
  (scanned-count 0 :type fixnum)
  (total-count 0 :type fixnum)
  (limit 200 :type (or null fixnum))
  (regex-mode t :type boolean)
  (case-insensitive nil :type boolean)
  (multiline-mode nil :type boolean)
  (before-context 0 :type fixnum)
  (after-context 0 :type fixnum)
  (status :idle :type keyword)
  (on-select nil :type (or null function))
  (prompt "search> " :type string)
  (empty-message "[no results]" :type string))

(defun make-search-widget-state (&key
                                   (mode :files)
                                   (query "")
                                   (file-candidates '())
                                   (documents '())
                                   (visible-count 10)
                                   (batch-size 32)
                                   (limit 200)
                                   (regex-mode t)
                                   (case-insensitive nil)
                                   (multiline-mode nil)
                                   (before-context 0)
                                   (after-context 0)
                                   on-select
                                   (prompt "search> ")
                                   (empty-message "[no results]"))
  "Create mutable state for the search widget. Primary constructor.
MODE is :files for filename search or :content for full-text search."
  (check-type mode (member :files :content))
  (check-type query string)
  (check-type visible-count (integer 1 *))
  (check-type batch-size (integer 1 *))
  (when limit
    (check-type limit (integer 0 *)))
  (check-type before-context (integer 0 *))
  (check-type after-context (integer 0 *))
  (when on-select
    (check-type on-select function))
  (let ((state
          (%make-search-widget-state :mode mode
                                     :query query
                                     :file-candidates (%normalize-file-candidates file-candidates)
                                     :documents (%normalize-documents documents)
                                     :visible-count visible-count
                                     :batch-size batch-size
                                     :limit limit
                                     :regex-mode regex-mode
                                     :case-insensitive case-insensitive
                                     :multiline-mode multiline-mode
                                     :before-context before-context
                                     :after-context after-context
                                     :on-select on-select
                                     :prompt prompt
                                     :empty-message empty-message)))
    (unless (zerop (length query))
      (search-widget-rerun state))
    state))

(defun search-widget-mode (state)
  (search-widget-state-mode state))

(defun search-widget-query (state)
  (search-widget-state-query state))

(defun (setf search-widget-query) (value state)
  (check-type value string)
  (setf (search-widget-state-query state) value)
  (if (zerop (length value))
      (progn
        (%reset-content-stream-state! state)
        (setf (search-widget-state-file-results state) '()
              (search-widget-state-content-results state) '()
              (search-widget-state-status state) :idle
              (search-widget-state-selected-index state) 0)
        state)
      (search-widget-rerun state))
  value)

(defun search-widget-status (state)
  (search-widget-state-status state))

(defun search-widget-file-results (state)
  (search-widget-state-file-results state))

(defun search-widget-content-results (state)
  (search-widget-state-content-results state))

(defun search-widget-selected-index (state)
  (search-widget-state-selected-index state))

(defun (setf search-widget-selected-index) (value state)
  (check-type value (integer 0 *))
  (setf (search-widget-state-selected-index state) value))

(defun search-widget-visible-count (state)
  (search-widget-state-visible-count state))

(defun search-widget-batch-size (state)
  (search-widget-state-batch-size state))

(defun search-widget-match-count (state)
  (search-widget-state-match-count state))

(defun search-widget-scanned-count (state)
  (search-widget-state-scanned-count state))

(defun search-widget-total-count (state)
  (search-widget-state-total-count state))

(defun make-search-widget-stream (&key next cancel (total-items 0))
  "Create a streaming document source for content search.
NEXT must return two values: DOCUMENT and DONE-P."
  (check-type next function)
  (check-type total-items (integer 0 *))
  (let ((cancel-fn (or cancel (lambda () nil))))
    (check-type cancel-fn function)
    (%make-search-widget-stream :next-fn next
                                :cancel-fn cancel-fn
                                :total-items total-items)))

(defun make-sequence-search-widget-stream (documents)
  "Create a deterministic document stream backed by DOCUMENTS."
  (let* ((normalized (%normalize-documents documents))
         (vector (coerce normalized 'vector))
         (cursor 0)
         (total (length vector)))
    (make-search-widget-stream
     :total-items total
     :next (lambda ()
             (if (>= cursor total)
                 (values nil t)
                 (prog1
                     (values (aref vector cursor)
                             (= (1+ cursor) total))
                   (incf cursor))))
     :cancel (lambda ()
               (setf cursor total)
               t))))

(defun %normalize-path-text (value)
  (let ((text (typecase value
                (pathname (namestring value))
                (string value)
                (t (princ-to-string value)))))
    (substitute #\/ #\\ text)))

(defun %normalize-file-candidates (candidates)
  (if (null candidates)
      '()
      (map 'list #'%normalize-path-text candidates)))

(defun %ensure-search-document (entry)
  (cond
    ((ptui.search.engine:search-document-p entry)
     entry)
    ((and (listp entry)
          (getf entry :path)
          (getf entry :content))
     (ptui.search.engine:make-search-document
      :path (%normalize-path-text (getf entry :path))
      :content (princ-to-string (getf entry :content))))
    (t
     (error "Expected SEARCH-DOCUMENT or plist with :path/:content, got ~S." entry))))

(defun %normalize-documents (documents)
  (if (null documents)
      '()
      (map 'list #'%ensure-search-document documents)))

(defun %reset-content-stream-state! (state)
  (setf (search-widget-state-stream state) nil
        (search-widget-state-stream-all-content-results state) '()
        (search-widget-state-match-count state) 0
        (search-widget-state-scanned-count state) 0
        (search-widget-state-total-count state) 0)
  state)

(defun %content-match-better-p (left right)
  (cond
    ((> (ptui.search.engine:search-content-match-score left)
        (ptui.search.engine:search-content-match-score right))
     t)
    ((< (ptui.search.engine:search-content-match-score left)
        (ptui.search.engine:search-content-match-score right))
     nil)
    ((string< (ptui.search.engine:search-content-match-path left)
              (ptui.search.engine:search-content-match-path right))
     t)
    ((string< (ptui.search.engine:search-content-match-path right)
              (ptui.search.engine:search-content-match-path left))
     nil)
    ((< (ptui.search.engine:search-content-match-line left)
        (ptui.search.engine:search-content-match-line right))
     t)
    ((> (ptui.search.engine:search-content-match-line left)
        (ptui.search.engine:search-content-match-line right))
     nil)
    (t
     (< (ptui.search.engine:search-content-match-column left)
        (ptui.search.engine:search-content-match-column right)))))

(defun %refresh-streamed-content-results! (state)
  (let* ((sorted (sort (copy-list (search-widget-state-stream-all-content-results state))
                       #'%content-match-better-p))
         (limit (search-widget-state-limit state))
         (limited (if limit
                      (subseq sorted 0 (min limit (length sorted)))
                      sorted)))
    (setf (search-widget-state-content-results state) limited))
  state)

(defun search-widget-set-file-candidates (state candidates &key (rerun t))
  (check-type state search-widget-state)
  (setf (search-widget-state-file-candidates state) (%normalize-file-candidates candidates))
  (when rerun
    (search-widget-rerun state))
  state)

(defun search-widget-results (state)
  (ecase (search-widget-state-mode state)
    (:files (search-widget-state-file-results state))
    (:content (search-widget-state-content-results state))))

(defun search-widget-visible-results (state)
  (check-type state search-widget-state)
  (multiple-value-bind (visible _start) (%visible-window state)
    (declare (ignore _start))
    visible))

(defun %results-count (state)
  (length (search-widget-results state)))

(defun %clamp-selected-index! (state)
  (setf (search-widget-state-selected-index state)
        (clamp-index (search-widget-state-selected-index state)
                     (%results-count state))))

(defun %run-file-search! (state)
  (let ((ranked
          (ptui.search.engine:rank-file-matches
           (search-widget-state-query state)
           (search-widget-state-file-candidates state)
           :limit (search-widget-state-limit state))))
    (%reset-content-stream-state! state)
    (setf (search-widget-state-file-results state) ranked
          (search-widget-state-content-results state) '()
          (search-widget-state-status state) (if ranked :ready :empty))
    (%clamp-selected-index! state)
    state))

(defun %run-content-search! (state)
  (%reset-content-stream-state! state)
  (let* ((scan-result
           (ptui.search.engine:scan-content-matches
            (search-widget-state-query state)
            (search-widget-state-documents state)
            :limit (search-widget-state-limit state)
            :regex-mode (search-widget-state-regex-mode state)
            :case-insensitive (search-widget-state-case-insensitive state)
            :multiline-mode (search-widget-state-multiline-mode state)
            :before-context (search-widget-state-before-context state)
            :after-context (search-widget-state-after-context state)))
         (matches (ptui.search.engine:search-content-scan-result-matches scan-result)))
    (setf (search-widget-state-file-results state) '()
          (search-widget-state-stream-all-content-results state) (copy-list matches)
          (search-widget-state-content-results state) matches
          (search-widget-state-match-count state)
          (ptui.search.engine:search-content-scan-result-match-count scan-result)
          (search-widget-state-scanned-count state)
          (ptui.search.engine:search-content-scan-result-scanned-documents scan-result)
          (search-widget-state-total-count state)
          (ptui.search.engine:search-content-scan-result-total-documents scan-result)
          (search-widget-state-status state) (if matches :ready :empty))
    (%clamp-selected-index! state)
    state))

(defun search-widget-rerun (state)
  (check-type state search-widget-state)
  (ecase (search-widget-state-mode state)
    (:files (%run-file-search! state))
    (:content (%run-content-search! state))))

(defun search-widget-start-content-search-stream (state pattern stream
                                                  &key
                                                    documents
                                                    limit
                                                    regex-mode
                                                    case-insensitive
                                                    multiline-mode
                                                    before-context
                                                    after-context)
  "Start incremental content-search over STREAM.
Use SEARCH-WIDGET-STEP to advance search and SEARCH-WIDGET-CANCEL to interrupt."
  (check-type state search-widget-state)
  (check-type pattern string)
  (check-type stream search-widget-stream)
  (when documents
    (check-type documents list))
  (when limit
    (check-type limit (integer 0 *)))
  (when before-context
    (check-type before-context (integer 0 *)))
  (when after-context
    (check-type after-context (integer 0 *)))
  (%reset-content-stream-state! state)
  (setf (search-widget-state-mode state) :content
        (search-widget-state-query state) pattern
        (search-widget-state-file-candidates state) '()
        (search-widget-state-documents state)
        (if documents
            (%normalize-documents documents)
            (search-widget-state-documents state))
        (search-widget-state-file-results state) '()
        (search-widget-state-content-results state) '()
        (search-widget-state-selected-index state) 0
        (search-widget-state-stream state) stream
        (search-widget-state-total-count state)
        (let ((stream-total (search-widget-stream-total-items stream)))
          (if (> stream-total 0)
              stream-total
              (length (search-widget-state-documents state))))
        (search-widget-state-status state) :streaming)
  (when limit
    (setf (search-widget-state-limit state) limit))
  (when (not (null regex-mode))
    (setf (search-widget-state-regex-mode state) regex-mode))
  (when (not (null case-insensitive))
    (setf (search-widget-state-case-insensitive state) case-insensitive))
  (when (not (null multiline-mode))
    (setf (search-widget-state-multiline-mode state) multiline-mode))
  (when before-context
    (setf (search-widget-state-before-context state) before-context))
  (when after-context
    (setf (search-widget-state-after-context state) after-context))
  state)

(defun search-widget-step (state &key max-items)
  "Advance streaming content-search by consuming up to MAX-ITEMS documents."
  (check-type state search-widget-state)
  (let* ((stream (search-widget-state-stream state))
         (limit (or max-items (search-widget-state-batch-size state)))
         (consumed 0)
         (new-matches 0))
    (check-type limit (integer 1 *))
    (cond
      ((null stream)
       (values state consumed new-matches))
      ((member (search-widget-state-status state) '(:cancelled :done :empty) :test #'eq)
       (values state consumed new-matches))
      (t
       (loop while (< consumed limit) do
         (multiple-value-bind (entry done-p)
             (funcall (search-widget-stream-next-fn stream))
           (when entry
             (incf consumed)
             (incf (search-widget-state-scanned-count state))
             (let* ((document (%ensure-search-document entry))
                    (matches
                      (ptui.search.engine:search-content-matches
                       (search-widget-state-query state)
                       (list document)
                       :limit nil
                       :regex-mode (search-widget-state-regex-mode state)
                       :case-insensitive (search-widget-state-case-insensitive state)
                       :multiline-mode (search-widget-state-multiline-mode state)
                       :before-context (search-widget-state-before-context state)
                       :after-context (search-widget-state-after-context state))))
               (when matches
                 (setf (search-widget-state-stream-all-content-results state)
                       (nconc (search-widget-state-stream-all-content-results state)
                              matches))
                 (incf (search-widget-state-match-count state) (length matches))
                 (incf new-matches (length matches))
                 (%refresh-streamed-content-results! state))))
           (when done-p
             (setf (search-widget-stream-done-p stream) t
                   (search-widget-state-stream state) nil
                   (search-widget-state-status state)
                   (if (> (search-widget-state-match-count state) 0) :done :empty))
             (return))
           (when (null entry)
             ;; No document available in this tick; caller may poll again.
             (return))))
       (when (and (search-widget-state-stream state)
                  (not (eq (search-widget-state-status state) :cancelled)))
         (setf (search-widget-state-status state) :streaming))
       (%clamp-selected-index! state)
       (values state consumed new-matches)))))

(defun search-widget-cancel (state)
  "Cancel in-flight streaming content-search."
  (check-type state search-widget-state)
  (let ((stream (search-widget-state-stream state)))
    (when stream
      (setf (search-widget-stream-cancelled-p stream) t)
      (funcall (search-widget-stream-cancel-fn stream))))
  (setf (search-widget-state-stream state) nil
        (search-widget-state-status state) :cancelled)
  state)

(defun search-widget-start-file-search (state query candidates &key limit)
  (check-type state search-widget-state)
  (check-type query string)
  (when limit
    (check-type limit (integer 0 *)))
  (setf (search-widget-state-mode state) :files
        (search-widget-state-query state) query
        (search-widget-state-file-candidates state) (%normalize-file-candidates candidates)
        (search-widget-state-documents state) '()
        (search-widget-state-selected-index state) 0)
  (%reset-content-stream-state! state)
  (when limit
    (setf (search-widget-state-limit state) limit))
  (search-widget-rerun state))

(defun search-widget-start-content-search (state pattern documents
                                           &key
                                             limit
                                             regex-mode
                                             case-insensitive
                                             multiline-mode
                                             before-context
                                             after-context)
  (check-type state search-widget-state)
  (check-type pattern string)
  (when limit
    (check-type limit (integer 0 *)))
  (when before-context
    (check-type before-context (integer 0 *)))
  (when after-context
    (check-type after-context (integer 0 *)))
  (setf (search-widget-state-mode state) :content
        (search-widget-state-query state) pattern
        (search-widget-state-file-candidates state) '()
        (search-widget-state-documents state) (%normalize-documents documents)
        (search-widget-state-selected-index state) 0)
  (%reset-content-stream-state! state)
  (setf (search-widget-state-total-count state)
        (length (search-widget-state-documents state)))
  (when limit
    (setf (search-widget-state-limit state) limit))
  (when (not (null regex-mode))
    (setf (search-widget-state-regex-mode state) regex-mode))
  (when (not (null case-insensitive))
    (setf (search-widget-state-case-insensitive state) case-insensitive))
  (when (not (null multiline-mode))
    (setf (search-widget-state-multiline-mode state) multiline-mode))
  (when before-context
    (setf (search-widget-state-before-context state) before-context))
  (when after-context
    (setf (search-widget-state-after-context state) after-context))
  (search-widget-rerun state))

(defun search-widget-set-documents (state documents)
  (check-type state search-widget-state)
  (setf (search-widget-state-documents state) (%normalize-documents documents))
  (setf (search-widget-state-total-count state)
        (length (search-widget-state-documents state)))
  (when (eq (search-widget-state-mode state) :content)
    (setf (search-widget-state-selected-index state) 0)
    (search-widget-rerun state))
  state)

(defun search-widget-selected-result (state)
  (check-type state search-widget-state)
  (nth (search-widget-state-selected-index state)
       (search-widget-results state)))

(defun %move-selection! (state key)
  (setf (search-widget-state-selected-index state)
        (move-selection (search-widget-state-selected-index state)
                        (%results-count state)
                        key))
  state)

(defun %append-query! (state text)
  (setf (search-widget-state-query state)
        (concatenate 'string (search-widget-state-query state) text))
  (search-widget-rerun state))

(defun %drop-query-char! (state)
  (let ((query (search-widget-state-query state)))
    (when (> (length query) 0)
      (setf (search-widget-state-query state)
            (subseq query 0 (1- (length query))))
      (search-widget-rerun state)))
  state)

(defun search-widget-handle-event (state event)
  "Apply EVENT to STATE; returns (:action <keyword> :state STATE ...)."
  (check-type state search-widget-state)
  (unless (typep event 'ptui.core.events:key-event)
    (return-from search-widget-handle-event
      (list :action :ignored :state state)))
  (let ((key (ptui.core.events:key-event-key event))
        (text (ptui.core.events:key-event-text? event)))
    (cond
      ((and (stringp text) (> (length text) 0))
       (%append-query! state text)
       (list :action :query-updated :state state))
      ((eq key :backspace)
       (%drop-query-char! state)
       (list :action :query-updated :state state))
      ((member key '(:up :down :home :end) :test #'eq)
       (%move-selection! state key)
       (list :action :selection-moved :state state))
      ((member key '(:escape :ctrl-c) :test #'eq)
       (search-widget-cancel state)
       (list :action :cancelled :state state))
      ((eq key :enter)
       (let ((selected (search-widget-selected-result state)))
         (when (and selected (search-widget-state-on-select state))
           (funcall (search-widget-state-on-select state) selected state))
         (list :action :selected :state state :result selected)))
      (t
       (list :action :ignored :state state)))))

(defun %status-line (state)
  (let* ((status (search-widget-state-status state))
         (result-count (%results-count state))
         (match-count (search-widget-state-match-count state))
         (scanned-count (search-widget-state-scanned-count state))
         (total-count (search-widget-state-total-count state)))
    (if (eq status :streaming)
        (format nil "searching (~D match~:P, ~D/~D docs)"
                match-count
                scanned-count
                (max scanned-count total-count))
        (format nil "~A (~D result~:P)"
                (case status
                  (:ready "ready")
                  (:empty "empty")
                  (:done "done")
                  (:cancelled "cancelled")
                  (t "idle"))
                result-count))))

(defun %result->text (state result)
  (ecase (search-widget-state-mode state)
    (:files
     (format nil "~A"
             (ptui.search.engine:search-file-match-path result)))
    (:content
     (format nil "~A:~D:~D ~A"
             (ptui.search.engine:search-content-match-path result)
             (ptui.search.engine:search-content-match-line result)
             (ptui.search.engine:search-content-match-column result)
             (ptui.search.engine:search-content-match-text result)))))

(defun %visible-window (state)
  (visible-window (search-widget-results state)
                  (search-widget-state-selected-index state)
                  (search-widget-state-visible-count state)))

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
                                        (%result->text state result))))
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
