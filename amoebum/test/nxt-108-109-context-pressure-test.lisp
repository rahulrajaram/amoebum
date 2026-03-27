(in-package :amoebum/test)

;;; ============================================================
;;; NXT-108 / NXT-109: IDE context packet builder and
;;;                     context-pressure metadata
;;; ============================================================

(def-suite nxt-108-109-suite
  :description "Tests for NXT-108 ide-context-build-packet and NXT-109 context-pressure functions."
  :in amoebum-suite)

(in-suite nxt-108-109-suite)

;;; ------------------------------------------------------------------
;;; NXT-108: ide-context-build-packet — nil / empty context
;;; ------------------------------------------------------------------

(test build-packet-nil-context
  "ide-context-build-packet returns a zero-valued packet when CTX is NIL."
  (let ((pkt (amoebum:ide-context-build-packet nil)))
    (is (listp pkt))
    (is (null (getf pkt :selections)))
    (is (null (getf pkt :diagnostics)))
    (is (null (getf pkt :open-files)))
    (is (null (getf pkt :active-file)))
    (is (= 0 (getf pkt :token-estimate)))))

(test build-packet-empty-context
  "ide-context-build-packet on an empty context returns minimal packet."
  (let* ((ctx (amoebum:make-ide-context))
         (pkt (amoebum:ide-context-build-packet ctx)))
    (is (listp pkt))
    (is (null (getf pkt :selections)))
    (is (null (getf pkt :diagnostics)))
    (is (member :mode pkt))
    (is (member :token-estimate pkt))))

;;; ------------------------------------------------------------------
;;; NXT-108: mode keyword is stored in the packet
;;; ------------------------------------------------------------------

(test build-packet-records-mode
  "ide-context-build-packet stores the requested mode in :mode."
  (let ((ctx (amoebum:make-ide-context :active-file "/src/a.lisp")))
    (is (eq :compact  (getf (amoebum:ide-context-build-packet ctx :mode :compact)  :mode)))
    (is (eq :operator (getf (amoebum:ide-context-build-packet ctx :mode :operator) :mode)))
    (is (eq :verbose  (getf (amoebum:ide-context-build-packet ctx :mode :verbose)  :mode)))))

;;; ------------------------------------------------------------------
;;; NXT-108: :compact mode behaviour
;;; ------------------------------------------------------------------

(test build-packet-compact-no-selections
  ":compact mode produces no selections even when the source context has some."
  (let* ((ctx (amoebum:make-ide-context
               :selections (list (list :file "/a.lisp" :start-line 1 :end-line 5
                                       :text "hello"))
               :diagnostics (list (list :file "/a.lisp" :line 1 :severity :error
                                        :message "bad"))))
         (pkt (amoebum:ide-context-build-packet ctx :mode :compact)))
    (is (null (getf pkt :selections)))))

(test build-packet-compact-caps-diagnostics-at-3
  ":compact mode keeps at most 3 diagnostics."
  (let* ((diags (loop for i from 1 to 8
                      collect (list :file "/a.lisp" :line i :severity :warning
                                    :message (format nil "warn ~D" i))))
         (ctx (amoebum:make-ide-context :diagnostics diags))
         (pkt (amoebum:ide-context-build-packet ctx :mode :compact)))
    (is (<= (length (getf pkt :diagnostics)) 3))))

(test build-packet-compact-keeps-all-diagnostics-when-few
  ":compact mode keeps all diagnostics when there are fewer than 3."
  (let* ((diags (list (list :file "/a.lisp" :line 1 :severity :error :message "e1")
                      (list :file "/a.lisp" :line 2 :severity :error :message "e2")))
         (ctx (amoebum:make-ide-context :diagnostics diags))
         (pkt (amoebum:ide-context-build-packet ctx :mode :compact)))
    (is (= 2 (length (getf pkt :diagnostics))))))

;;; ------------------------------------------------------------------
;;; NXT-108: :operator mode behaviour
;;; ------------------------------------------------------------------

(test build-packet-operator-all-diagnostics
  ":operator mode preserves all diagnostics."
  (let* ((diags (loop for i from 1 to 10
                      collect (list :file "/a.lisp" :line i :severity :error
                                    :message (format nil "err ~D" i))))
         (ctx (amoebum:make-ide-context :diagnostics diags))
         (pkt (amoebum:ide-context-build-packet ctx :mode :operator)))
    (is (= 10 (length (getf pkt :diagnostics))))))

(test build-packet-operator-caps-selections-at-5
  ":operator mode keeps at most 5 selections."
  (let* ((sels (loop for i from 1 to 9
                     collect (list :file "/a.lisp" :start-line i :end-line (+ i 1)
                                   :text "x")))
         (ctx (amoebum:make-ide-context :selections sels))
         (pkt (amoebum:ide-context-build-packet ctx :mode :operator)))
    (is (<= (length (getf pkt :selections)) 5))))

;;; ------------------------------------------------------------------
;;; NXT-108: :verbose mode behaviour
;;; ------------------------------------------------------------------

(test build-packet-verbose-all-selections
  ":verbose mode preserves all selections."
  (let* ((sels (loop for i from 1 to 7
                     collect (list :file "/a.lisp" :start-line i :end-line (+ i 2)
                                   :text "code")))
         (ctx (amoebum:make-ide-context :selections sels))
         (pkt (amoebum:ide-context-build-packet ctx :mode :verbose)))
    (is (= 7 (length (getf pkt :selections))))))

(test build-packet-verbose-all-diagnostics
  ":verbose mode preserves all diagnostics."
  (let* ((diags (loop for i from 1 to 12
                      collect (list :file "/a.lisp" :line i :severity :info
                                    :message (format nil "msg ~D" i))))
         (ctx (amoebum:make-ide-context :diagnostics diags))
         (pkt (amoebum:ide-context-build-packet ctx :mode :verbose)))
    (is (= 12 (length (getf pkt :diagnostics))))))

;;; ------------------------------------------------------------------
;;; NXT-108: :open-files and :active-file are always passed through
;;; ------------------------------------------------------------------

(test build-packet-open-files-propagated
  "ide-context-build-packet propagates :open-files in all modes."
  (let* ((ctx (amoebum:make-ide-context
               :open-files '("/a.lisp" "/b.lisp" "/c.lisp")))
         (pkt-compact   (amoebum:ide-context-build-packet ctx :mode :compact))
         (pkt-operator  (amoebum:ide-context-build-packet ctx :mode :operator))
         (pkt-verbose   (amoebum:ide-context-build-packet ctx :mode :verbose)))
    (is (= 3 (length (getf pkt-compact  :open-files))))
    (is (= 3 (length (getf pkt-operator :open-files))))
    (is (= 3 (length (getf pkt-verbose  :open-files))))))

(test build-packet-active-file-propagated
  "ide-context-build-packet propagates :active-file in all modes."
  (let* ((ctx (amoebum:make-ide-context :active-file "/src/main.lisp"))
         (pkt (amoebum:ide-context-build-packet ctx :mode :compact)))
    (is (string= "/src/main.lisp" (getf pkt :active-file)))))

;;; ------------------------------------------------------------------
;;; NXT-108: budget truncation
;;; ------------------------------------------------------------------

(test build-packet-budget-drops-diagnostics-first
  "When budget is very tight, diagnostics are dropped before selections."
  (let* ((sels  (list (list :file "/a.lisp" :start-line 1 :end-line 2 :text "x")))
         (diags (loop for i from 1 to 20
                      collect (list :file "/a.lisp" :line i :severity :error
                                    :message "error message goes here with some length")))
         (ctx (amoebum:make-ide-context :selections sels :diagnostics diags))
         (pkt (amoebum:ide-context-build-packet ctx :mode :verbose :budget 50)))
    ;; With a tight budget, diagnostics should be reduced
    (is (<= (getf pkt :token-estimate) 50))))

(test build-packet-token-estimate-positive
  "ide-context-build-packet returns a non-negative :token-estimate."
  (let* ((ctx (amoebum:make-ide-context
               :active-file "/src/app.lisp"
               :open-files '("/src/app.lisp")
               :selections (list (list :file "/src/app.lisp"
                                       :start-line 1 :end-line 10
                                       :text "(defun foo () 42)"))
               :diagnostics (list (list :file "/src/app.lisp"
                                        :line 5 :severity :warning
                                        :message "unused var"))))
         (pkt (amoebum:ide-context-build-packet ctx :mode :verbose)))
    (is (>= (getf pkt :token-estimate) 0))))

;;; ------------------------------------------------------------------
;;; NXT-109: cultivar-context-pressure
;;; ------------------------------------------------------------------

(test cultivar-context-pressure-nil-adapter
  "cultivar-context-pressure returns a valid plist when adapter is NIL."
  (let ((p (amoebum:cultivar-context-pressure nil)))
    (is (listp p))
    (is (member :available-contexts p))
    (is (member :staleness-seconds  p))
    (is (integerp (getf p :available-contexts)))
    (is (integerp (getf p :staleness-seconds)))))

(test cultivar-context-pressure-disabled-adapter
  "cultivar-context-pressure returns stub values for a disabled adapter."
  (let* ((a (amoebum:make-cultivar-adapter))
         (p (amoebum:cultivar-context-pressure a)))
    (is (= 0 (getf p :available-contexts)))
    (is (= 0 (getf p :staleness-seconds)))))

(test cultivar-context-pressure-enabled-adapter
  "cultivar-context-pressure returns stub values even for an enabled adapter."
  (let* ((a (amoebum:make-cultivar-adapter :enabled-p t))
         (p (amoebum:cultivar-context-pressure a)))
    (is (listp p))
    (is (member :available-contexts p))
    (is (member :staleness-seconds  p))))

;;; ------------------------------------------------------------------
;;; NXT-109: yore-context-pressure
;;; ------------------------------------------------------------------

(test yore-context-pressure-nil-adapter
  "yore-context-pressure returns a valid plist when adapter is NIL."
  (let ((p (amoebum:yore-context-pressure nil)))
    (is (listp p))
    (is (member :indexed-documents      p))
    (is (member :last-index-seconds-ago p))
    (is (integerp (getf p :indexed-documents)))
    (is (integerp (getf p :last-index-seconds-ago)))))

(test yore-context-pressure-disabled-adapter
  "yore-context-pressure returns stub values for a disabled adapter."
  (let* ((a (amoebum:make-yore-adapter))
         (p (amoebum:yore-context-pressure a)))
    (is (= 0 (getf p :indexed-documents)))
    (is (= 0 (getf p :last-index-seconds-ago)))))

(test yore-context-pressure-enabled-adapter
  "yore-context-pressure returns stub values even for an enabled adapter."
  (let* ((a (amoebum:make-yore-adapter :enabled-p t))
         (p (amoebum:yore-context-pressure a)))
    (is (listp p))
    (is (member :indexed-documents p))
    (is (member :last-index-seconds-ago p))))

;;; ------------------------------------------------------------------
;;; NXT-109: %context-pressure-summary
;;; ------------------------------------------------------------------

(test context-pressure-summary-returns-string
  "%context-pressure-summary returns a string."
  (let ((s (amoebum::%context-pressure-summary)))
    (is (stringp s))
    (is (plusp (length s)))))

(test context-pressure-summary-no-ide-context
  "%context-pressure-summary works when *ide-context* is NIL."
  (let ((amoebum:*ide-context* nil))
    (let ((s (amoebum::%context-pressure-summary)))
      (is (stringp s))
      ;; Should report 0 selections and 0 diagnostics
      (is (not (null (search "0sel/0diag" s)))))))

(test context-pressure-summary-with-ide-context
  "%context-pressure-summary reflects IDE context selection and diagnostic counts."
  (let* ((ctx (amoebum:make-ide-context
               :selections  (list (list :file "/a.lisp" :start-line 1 :end-line 2 :text "x")
                                  (list :file "/b.lisp" :start-line 3 :end-line 4 :text "y"))
               :diagnostics (list (list :file "/a.lisp" :line 1 :severity :error :message "e"))))
         (amoebum:*ide-context* ctx)
         (s (amoebum::%context-pressure-summary)))
    (is (not (null (search "2sel" s))))
    (is (not (null (search "1diag" s))))))

(test context-pressure-summary-contains-cultivar-section
  "%context-pressure-summary includes 'cult' section."
  (let ((s (amoebum::%context-pressure-summary
            :cultivar (amoebum:make-cultivar-adapter))))
    (is (not (null (search "cult" s))))))

(test context-pressure-summary-contains-yore-section
  "%context-pressure-summary includes 'yore' section."
  (let ((s (amoebum::%context-pressure-summary
            :yore (amoebum:make-yore-adapter))))
    (is (not (null (search "yore" s))))))

(test context-pressure-summary-explicit-nil-adapters
  "%context-pressure-summary accepts explicit NIL adapters without error."
  (let ((s (amoebum::%context-pressure-summary
            :ide-ctx  nil
            :cultivar nil
            :yore     nil)))
    (is (stringp s))))
