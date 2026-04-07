(defpackage :ptui.test.widget-status-dispatch
  (:use :cl :fiveam)
  (:export #:run-all #:widget-status-dispatch-suite))

(in-package :ptui.test.widget-status-dispatch)

(def-suite widget-status-dispatch-suite
  :description "Widget status/mode dispatch table tests (FP-Refine Phase 3, T4).")

(in-suite widget-status-dispatch-suite)

;;; ====================================================================
;;; Glob-widget status label table
;;; ====================================================================

(test glob-status-table-has-four-entries
  (is (= 4 (length ptui.components.glob-widget::+glob-status-labels+))))

(test glob-status-table-contains-expected-keys
  (let ((keys (mapcar #'car ptui.components.glob-widget::+glob-status-labels+)))
    (dolist (k '(:streaming :done :cancelled :idle))
      (is (member k keys :test #'eq)
          "Expected ~S in glob status labels." k))))

(test glob-status-label-streaming
  (is (string= "scanning"
               (ptui.components.glob-widget::%status-label :streaming))))

(test glob-status-label-done
  (is (string= "done"
               (ptui.components.glob-widget::%status-label :done))))

(test glob-status-label-cancelled
  (is (string= "cancelled"
               (ptui.components.glob-widget::%status-label :cancelled))))

(test glob-status-label-idle
  (is (string= "idle"
               (ptui.components.glob-widget::%status-label :idle))))

(test glob-status-label-unknown-falls-back-to-keyword-name
  (is (stringp (ptui.components.glob-widget::%status-label :unknown-status))))

;;; ====================================================================
;;; Search-widget status label table
;;; ====================================================================

(test search-status-table-has-five-entries
  (is (= 5 (length ptui.components.search-widget::+search-status-labels+))))

(test search-status-table-contains-expected-keys
  (let ((keys (mapcar #'car ptui.components.search-widget::+search-status-labels+)))
    (dolist (k '(:ready :empty :done :cancelled :idle))
      (is (member k keys :test #'eq)
          "Expected ~S in search status labels." k))))

(test search-status-label-ready
  (is (string= "ready"
               (ptui.components.search-widget::%search-status-label :ready))))

(test search-status-label-empty
  (is (string= "empty"
               (ptui.components.search-widget::%search-status-label :empty))))

(test search-status-label-done
  (is (string= "done"
               (ptui.components.search-widget::%search-status-label :done))))

(test search-status-label-cancelled
  (is (string= "cancelled"
               (ptui.components.search-widget::%search-status-label :cancelled))))

(test search-status-label-idle
  (is (string= "idle"
               (ptui.components.search-widget::%search-status-label :idle))))

;;; ====================================================================
;;; Search-widget mode dispatch tables
;;; ====================================================================

(test search-mode-results-accessor-table-has-two-entries
  (is (= 2 (length ptui.components.search-widget::+search-mode-results-accessor+))))

(test search-mode-result-formatter-table-has-two-entries
  (is (= 2 (length ptui.components.search-widget::+search-mode-result-formatter+))))

(test search-mode-rerun-handler-table-has-two-entries
  (is (= 2 (length ptui.components.search-widget::+search-mode-rerun-handler+))))

(test search-mode-results-accessor-entries-are-fboundp
  (dolist (entry ptui.components.search-widget::+search-mode-results-accessor+)
    (is (fboundp (cdr entry))
        "Accessor ~S should be fboundp." (cdr entry))))

(test search-mode-result-formatter-entries-are-fboundp
  (dolist (entry ptui.components.search-widget::+search-mode-result-formatter+)
    (is (fboundp (cdr entry))
        "Formatter ~S should be fboundp." (cdr entry))))

(test search-mode-rerun-handler-entries-are-fboundp
  (dolist (entry ptui.components.search-widget::+search-mode-rerun-handler+)
    (is (fboundp (cdr entry))
        "Rerun handler ~S should be fboundp." (cdr entry))))

;;; --- Mode dispatch: :files ---

(test search-results-files-mode-returns-file-results
  (let ((state (ptui.components.search-widget:make-search-widget-state
                :mode :files
                :file-candidates '("foo.lisp" "bar.lisp" "baz.lisp")
                :query "foo")))
    (let ((results (ptui.components.search-widget:search-widget-results state)))
      (is (listp results))
      (is (plusp (length results))))))

;;; --- Mode dispatch: :content ---

(test search-results-content-mode-returns-content-results
  (let ((state (ptui.components.search-widget:make-search-widget-state
                :mode :content
                :query "hello")))
    (ptui.components.search-widget:search-widget-start-content-search
     state "hello"
     (list (list :path "test.txt" :content "hello world"))
     :regex-mode t)
    (let ((results (ptui.components.search-widget:search-widget-results state)))
      (is (listp results))
      (is (plusp (length results))))))

;;; --- Result formatter dispatch ---

(test format-file-result-produces-path-string
  (let* ((state (ptui.components.search-widget:make-search-widget-state
                 :mode :files
                 :file-candidates '("src/main.lisp")
                 :query "main"))
         (results (ptui.components.search-widget:search-widget-results state)))
    (when results
      (let ((text (ptui.components.search-widget::%result->text state (first results))))
        (is (stringp text))
        (is (search "main.lisp" text))))))

(test format-content-result-produces-path-line-col-text
  (let ((state (ptui.components.search-widget:make-search-widget-state
                :mode :content
                :query "defun")))
    (ptui.components.search-widget:search-widget-start-content-search
     state "defun"
     (list (list :path "test.lisp" :content "(defun foo () 42)"))
     :regex-mode t)
    (let ((results (ptui.components.search-widget:search-widget-results state)))
      (when results
        (let ((text (ptui.components.search-widget::%result->text state (first results))))
          (is (stringp text))
          (is (search "test.lisp" text)))))))

(defun run-all ()
  (run! 'widget-status-dispatch-suite))
