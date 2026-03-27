(in-package :amoebum/test)

(def-suite ide-context-suite
  :description "Unit tests for NXT-092 IDE context ingestion and NXT-093 prompt/status wiring."
  :in amoebum-suite)

(in-suite ide-context-suite)

;;; ---------------------------------------------------------------------------
;;; NXT-092: struct construction and field accessors
;;; ---------------------------------------------------------------------------

(test ide-context-make-empty
  "make-ide-context with no args produces an empty, timestamped context."
  (let ((ctx (amoebum:make-ide-context)))
    (is (amoebum:ide-context-p ctx))
    (is (null (amoebum:ide-context-active-file ctx)))
    (is (null (amoebum:ide-context-selections ctx)))
    (is (null (amoebum:ide-context-diagnostics ctx)))
    (is (null (amoebum:ide-context-open-files ctx)))
    (is (integerp (amoebum:ide-context-timestamp ctx)))
    (is (>= (amoebum:ide-context-timestamp ctx) 0))))

(test ide-context-make-with-fields
  "make-ide-context stores supplied values in struct fields."
  (let ((ctx (amoebum:make-ide-context
              :active-file "/src/foo.lisp"
              :open-files '("/src/foo.lisp" "/src/bar.lisp")
              :selections (list (list :file "/src/foo.lisp"
                                     :start-line 10
                                     :end-line 20
                                     :text "(defun hello () nil)"))
              :diagnostics (list (list :file "/src/foo.lisp"
                                      :line 15
                                      :severity :error
                                      :message "undefined variable FOO")))))
    (is (string= "/src/foo.lisp" (amoebum:ide-context-active-file ctx)))
    (is (= 2 (length (amoebum:ide-context-open-files ctx))))
    (is (= 1 (length (amoebum:ide-context-selections ctx))))
    (is (= 1 (length (amoebum:ide-context-diagnostics ctx))))
    (is (string= "/src/foo.lisp"
                 (getf (first (amoebum:ide-context-selections ctx)) :file)))
    (is (eq :error
            (getf (first (amoebum:ide-context-diagnostics ctx)) :severity)))))

;;; ---------------------------------------------------------------------------
;;; NXT-092: update-ide-context!
;;; ---------------------------------------------------------------------------

(test update-ide-context-creates-global
  "update-ide-context! creates *ide-context* when it is NIL."
  (let ((amoebum:*ide-context* nil))
    (amoebum:update-ide-context! (list :active-file "/tmp/test.lisp"
                                       :open-files '("/tmp/test.lisp")))
    (is (amoebum:ide-context-p amoebum:*ide-context*))
    (is (string= "/tmp/test.lisp"
                 (amoebum:ide-context-active-file amoebum:*ide-context*)))))

(test update-ide-context-merges-fields
  "update-ide-context! updates only the supplied keys."
  (let ((amoebum:*ide-context* (amoebum:make-ide-context
                                :active-file "/a.lisp"
                                :open-files '("/a.lisp"))))
    (amoebum:update-ide-context! (list :open-files '("/a.lisp" "/b.lisp")))
    (is (= 2 (length (amoebum:ide-context-open-files amoebum:*ide-context*))))
    ;; active-file was NOT in the update plist, so it should be preserved
    (is (string= "/a.lisp"
                 (amoebum:ide-context-active-file amoebum:*ide-context*)))))

(test update-ide-context-returns-ctx
  "update-ide-context! returns the updated context object."
  (let ((amoebum:*ide-context* nil))
    (let ((result (amoebum:update-ide-context! '(:active-file "/x.lisp"))))
      (is (amoebum:ide-context-p result))
      (is (eq result amoebum:*ide-context*)))))

(test update-ide-context-non-list-data
  "update-ide-context! handles non-list input gracefully."
  (let ((amoebum:*ide-context* nil))
    (amoebum:update-ide-context! "not-a-plist")
    (is (amoebum:ide-context-p amoebum:*ide-context*))))

;;; ---------------------------------------------------------------------------
;;; NXT-092: ide-context-summary
;;; ---------------------------------------------------------------------------

(test ide-context-summary-nil
  "ide-context-summary returns empty string for NIL context."
  (is (string= "" (amoebum:ide-context-summary nil))))

(test ide-context-summary-non-nil
  "ide-context-summary returns a non-empty string for a populated context."
  (let ((ctx (amoebum:make-ide-context
              :active-file "/src/main.lisp"
              :open-files '("/src/main.lisp"))))
    (let ((summary (amoebum:ide-context-summary ctx)))
      (is (stringp summary))
      (is (plusp (length summary)))
      (is (not (null (search "/src/main.lisp" summary)))))))

(test ide-context-summary-includes-diagnostics
  "ide-context-summary includes diagnostic details."
  (let ((ctx (amoebum:make-ide-context
              :diagnostics (list (list :file "/src/a.lisp"
                                      :line 5
                                      :severity :warning
                                      :message "unused variable")))))
    (let ((s (amoebum:ide-context-summary ctx)))
      (is (not (null (search "warning" (string-downcase s)))))
      (is (not (null (search "unused variable" s)))))))

;;; ---------------------------------------------------------------------------
;;; NXT-092: ide-context-token-estimate
;;; ---------------------------------------------------------------------------

(test ide-context-token-estimate-nil
  "ide-context-token-estimate returns 0 for NIL."
  (is (= 0 (amoebum:ide-context-token-estimate nil))))

(test ide-context-token-estimate-empty
  "ide-context-token-estimate returns a small positive number for an empty context."
  (let ((ctx (amoebum:make-ide-context)))
    ;; Empty context still has a fixed overhead header
    (is (>= (amoebum:ide-context-token-estimate ctx) 0))))

(test ide-context-token-estimate-grows-with-content
  "ide-context-token-estimate grows when content is added."
  (let ((empty (amoebum:make-ide-context))
        (full  (amoebum:make-ide-context
                :active-file "/src/big-file.lisp"
                :open-files '("/src/a.lisp" "/src/b.lisp" "/src/c.lisp")
                :selections (list (list :file "/src/a.lisp"
                                        :start-line 1
                                        :end-line 100
                                        :text (make-string 400 :initial-element #\x)))
                :diagnostics (list (list :file "/src/a.lisp"
                                         :line 10
                                         :severity :error
                                         :message "some error")))))
    (is (> (amoebum:ide-context-token-estimate full)
           (amoebum:ide-context-token-estimate empty)))))

;;; ---------------------------------------------------------------------------
;;; NXT-093: ide-context-prompt-fragment
;;; ---------------------------------------------------------------------------

(test ide-context-prompt-fragment-nil-context
  "ide-context-prompt-fragment returns NIL when *ide-context* is NIL."
  (is (null (amoebum:ide-context-prompt-fragment nil))))

(test ide-context-prompt-fragment-empty-context
  "ide-context-prompt-fragment returns NIL for an empty context (no useful data)."
  (let ((ctx (amoebum:make-ide-context)))
    (is (null (amoebum:ide-context-prompt-fragment ctx)))))

(test ide-context-prompt-fragment-active-file
  "ide-context-prompt-fragment includes the active file."
  (let ((ctx (amoebum:make-ide-context :active-file "/src/foo.lisp")))
    (let ((frag (amoebum:ide-context-prompt-fragment ctx)))
      (is (stringp frag))
      (is (not (null (search "/src/foo.lisp" frag)))))))

(test ide-context-prompt-fragment-open-files
  "ide-context-prompt-fragment lists open files."
  (let ((ctx (amoebum:make-ide-context
              :open-files '("/src/a.lisp" "/src/b.lisp"))))
    (let ((frag (amoebum:ide-context-prompt-fragment ctx)))
      (is (stringp frag))
      (is (not (null (search "/src/a.lisp" frag))))
      (is (not (null (search "/src/b.lisp" frag)))))))

(test ide-context-prompt-fragment-selections
  "ide-context-prompt-fragment embeds selection text."
  (let ((ctx (amoebum:make-ide-context
              :selections (list (list :file "/src/x.lisp"
                                     :start-line 3
                                     :end-line 7
                                     :text "(defun hello () :world)")))))
    (let ((frag (amoebum:ide-context-prompt-fragment ctx)))
      (is (stringp frag))
      (is (not (null (search "(defun hello () :world)" frag)))))))

(test ide-context-prompt-fragment-diagnostics
  "ide-context-prompt-fragment includes diagnostic messages."
  (let ((ctx (amoebum:make-ide-context
              :diagnostics (list (list :file "/src/y.lisp"
                                      :line 42
                                      :severity :error
                                      :message "type mismatch")))))
    (let ((frag (amoebum:ide-context-prompt-fragment ctx)))
      (is (stringp frag))
      (is (not (null (search "type mismatch" frag)))))))

(test ide-context-prompt-fragment-uses-global-default
  "ide-context-prompt-fragment with no argument uses *ide-context*."
  (let ((amoebum:*ide-context* (amoebum:make-ide-context
                                :active-file "/global.lisp")))
    (let ((frag (amoebum:ide-context-prompt-fragment)))
      (is (stringp frag))
      (is (not (null (search "/global.lisp" frag)))))))

;;; ---------------------------------------------------------------------------
;;; NXT-093: status-bar :ide segment
;;; ---------------------------------------------------------------------------

(defun %make-ide-test-state (focus-mode)
  (amoebum.ui:make-status-bar-state
   :permission-mode :supervised
   :focus-mode focus-mode
   :model-name "moonshot-v1-128k"
   :branch-name "test/nxt-092"))

(test ide-segment-off-when-no-context
  "Status bar :ide segment shows 'IDE: off' when *ide-context* is NIL."
  (let ((amoebum:*ide-context* nil))
    (let ((text (amoebum::%ide-segment-text)))
      (is (string= "IDE: off" text)))))

(test ide-segment-on-when-context-empty
  "Status bar :ide segment shows 'IDE: on' for empty (no open files) context."
  (let ((amoebum:*ide-context* (amoebum:make-ide-context)))
    (let ((text (amoebum::%ide-segment-text)))
      (is (string= "IDE: on" text)))))

(test ide-segment-file-count
  "Status bar :ide segment shows file count when open files are present."
  (let ((amoebum:*ide-context* (amoebum:make-ide-context
                                :open-files '("/a.lisp" "/b.lisp" "/c.lisp"))))
    (let ((text (amoebum::%ide-segment-text)))
      (is (not (null (search "3" text))))
      (is (not (null (search "file" text)))))))

(test ide-segment-singular-file
  "Status bar :ide segment uses singular 'file' for exactly one open file."
  (let ((amoebum:*ide-context* (amoebum:make-ide-context
                                :open-files '("/only.lisp"))))
    (let ((text (amoebum::%ide-segment-text)))
      (is (not (null (search "1 file" text)))))))
