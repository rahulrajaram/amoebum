(in-package :amoebum/test)

;;; ============================================================
;;; NXT-275: Cultivar deftool wrapper tests
;;;
;;; Focused tests covering the Cultivar tools defined in
;;; amoebum/src/tools/cultivar-tools.lisp:
;;;   - unavailable path returns a diagnostic string/plist
;;;   - location-slice preserves resolve provenance in one packet
;;;   - live resolve round-trip against chat-ui-submit-input
;;;   - live references returns a non-empty location list
;;;   - live span preview returns markdown containing a fenced
;;;     code block
;;;   - live canonical slice returns structured JSON fields
;;;
;;; Live tests are gated identically to the NXT-274 adapter
;;; suite: they skip with a passing assertion when the cultivar
;;; binary or .agent/cultivar-index/ is missing.
;;; ============================================================

(def-suite cultivar-tools-suite
  :description "Unit tests for the NXT-275 cultivar deftool wrappers."
  :in amoebum-suite)

(in-suite cultivar-tools-suite)

;;; ------------------------------------------------------------------
;;; Helpers — mirror cultivar-adapter-test.lisp so the two suites stay
;;; independently runnable without cross-file references.
;;; ------------------------------------------------------------------

(defun %cultivar-tools-repo-root ()
  "Return the repo root pathname based on this test file's truename."
  (let ((here (or *load-truename* *compile-file-truename*)))
    (when here
      (make-pathname
       :defaults here
       :name nil :type nil
       :directory (let ((d (pathname-directory here)))
                    (when (and (listp d) (>= (length d) 3))
                      (butlast d 2)))))))

(defun %cultivar-tools-live-adapter ()
  "Return an enabled adapter targeting the real cultivar binary
and index, or NIL when either is missing."
  (let* ((bin (or (uiop:getenv "CULTIVAR_BINARY")
                  "/home/rahul/Documents/cultivar/target/release/cultivar"))
         (repo-root (%cultivar-tools-repo-root))
         (idx (and repo-root
                   (merge-pathnames ".agent/cultivar-index/" repo-root))))
    (when (and bin (probe-file bin)
               idx (probe-file idx))
      (amoebum:make-cultivar-adapter
       :enabled-p t
       :binary-path bin
       :index-path idx))))

(defmacro %skip-unless-cultivar-tools-running (&body body)
  "Run BODY with `amoebum::*cultivar-tool-adapter*` bound to a
live adapter, or record a passing assertion when cultivar is
unavailable."
  `(let ((live (%cultivar-tools-live-adapter)))
     (if (null live)
         (is-true t "cultivar binary or index missing — skipping live test")
         (let ((amoebum::*cultivar-tool-adapter* live))
           ,@body))))

(defun %cultivar-tools-call (tool-name arguments)
  "Invoke the cultivar tool named TOOL-NAME (a string) through
the registered toolset with ARGUMENTS (a plist of string keys to
values). Returns whatever the tool returns."
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (k v) on arguments by #'cddr
          do (setf (gethash k ht) v))
    (let* ((toolset (amoebum::%ensure-toolset))
           (def (pseudopod:find-tool toolset tool-name)))
      (unless def
        (error "Cultivar tool ~S is not registered." tool-name))
      (funcall (pseudopod:tool-definition-fn def) ht))))

(defun %cultivar-tools-fake-cli-script (root)
  (let ((path (merge-pathnames "cultivar-tools-fake.sh" root)))
    (%write-text-file
     path
     "#!/bin/sh
set -eu
cmd=${1:-}
shift || true
case \"$cmd\" in
  query)
    sub=${1:-}
    shift || true
    case \"$sub\" in
      resolve)
        printf '%s' '{\"results_digest\":\"arena\",\"symbol\":{\"id\":\"sym_fake\",\"kind\":\"function\",\"file\":\"/tmp/fake.lisp\",\"qualified_name\":\"amoebum:fake\",\"range\":{\"start_line\":1,\"start_col\":2}},\"confidence\":{\"level\":\"must\"},\"reference_context\":{\"reference_mode\":\"structural_only\"}}'
        ;;
      *)
        printf '%s' '{}'
        ;;
    esac
    ;;
  slice)
    printf '%s' '{\"results_digest\":\"arena\",\"served_from_materialization\":true,\"materialization_kind\":\"neighborhood\",\"symbol\":{\"id\":\"sym_fake\",\"name\":\"amoebum:fake\",\"kind\":\"function\",\"file\":\"/tmp/fake.lisp\",\"line\":1,\"col\":2},\"definition_source\":\"(defun fake ())\",\"callers\":[{\"file\":\"/tmp/caller.lisp\",\"line\":4,\"col\":2,\"context_line\":\"(fake)\",\"confidence\":\"may\",\"from_symbol\":\"amoebum:caller\"}],\"callees\":[],\"quality\":{\"index_coverage_pct\":100.0,\"must_edges\":2,\"may_edges\":1,\"unknown_edges\":0,\"has_uncertainty\":true},\"truncation\":{\"budget_chars\":8000,\"callers_truncated\":false,\"callees_truncated\":false,\"original_callers\":1,\"original_callees\":0},\"notes\":[\"structural-only\"]}'
    ;;
  *)
    printf '%s' '{}'
    ;;
esac
")
    (uiop:run-program (list "chmod" "+x" (namestring path))
                      :output nil :error-output nil)
    path))

(defmacro %with-cultivar-tools-fake-adapter ((adapter-var) &body body)
  `(let* ((root (%make-temp-directory "amoebum-cultivar-tools"))
          (index (merge-pathnames "index/" root))
          (script (%cultivar-tools-fake-cli-script root)))
     (unwind-protect
          (let ((,adapter-var
                  (amoebum:make-cultivar-adapter
                   :enabled-p t
                   :binary-path (namestring script)
                   :index-path index
                   :daemon-mode :prefer
                   :daemon-auto-start-p nil)))
            (ensure-directories-exist (merge-pathnames ".keep" index))
            ,@body)
       (%delete-directory-tree-safe root))))

;;; ------------------------------------------------------------------
;;; Unavailable diagnostic path
;;; ------------------------------------------------------------------

(test cultivar-symbol-resolve-tool-unavailable-returns-diagnostic
  "cultivar-symbol-resolve returns the unavailable-diagnostic plist
when the tool adapter points at a non-existent binary."
  (let ((amoebum::*cultivar-tool-adapter*
          (amoebum:make-cultivar-adapter
           :enabled-p t
           :binary-path "/nonexistent/cultivar-binary-does-not-exist"
           :index-path #p"/nonexistent/cultivar-index/")))
    (let ((result (%cultivar-tools-call "cultivar-symbol-resolve"
                                        (list "file" "/tmp/does-not-matter.lisp"
                                              "line" 1
                                              "column" 0))))
      (is (listp result))
      (is (stringp (getf result :error))
          "unavailable path must surface an :error diagnostic string")
      (is (search "cultivar unavailable" (or (getf result :error) ""))
          "diagnostic must mention cultivar unavailability"))))

(test cultivar-symbol-slice-tool-fake-canonical-json
  "cultivar-symbol-slice returns the canonical JSON slice fields for machine callers."
  (%with-cultivar-tools-fake-adapter (adapter)
    (let ((amoebum::*cultivar-tool-adapter* adapter))
      (let ((result (%cultivar-tools-call "cultivar-symbol-slice"
                                          (list "symbol-id" "sym_fake"))))
        (is (listp result))
        (is (equal "sym_fake" (getf result :symbol-id)))
        (is (eq t (getf result :served-from-materialization)))
        (is (equal "neighborhood" (getf result :materialization-kind)))
        (is (stringp (getf (getf result :symbol) :name)))
        (is (= 1 (length (getf result :callers))))
        (is (equal "structural-only" (first (getf result :notes))))))))

(test cultivar-location-slice-tool-fake-canonical-json
  "cultivar-location-slice resolves and returns the canonical JSON slice in one packet."
  (%with-cultivar-tools-fake-adapter (adapter)
    (let ((amoebum::*cultivar-tool-adapter* adapter))
      (let ((result (%cultivar-tools-call "cultivar-location-slice"
                                          (list "file" "/tmp/fake.lisp"
                                                "line" 1
                                                "column" 2))))
        (is (listp result))
        (is (eq t (getf result :resolved-p)))
        (is (equal "sym_fake" (getf result :symbol-id)))
        (is (listp (getf result :resolution)))
        (is (eq t (getf result :served-from-materialization)))
        (is (listp (getf result :symbol)))
        (is (equal "neighborhood" (getf result :materialization-kind)))))))

(test cultivar-slash-command-reports-daemon-and-latest-slice
  "The /cultivar slash command should report daemon state and the latest structured slice provenance."
  (%with-cultivar-tools-fake-adapter (adapter)
    (let ((amoebum::*cultivar-tool-adapter* adapter))
      (amoebum:cultivar-location-slice adapter (list :file "/tmp/fake.lisp" :line 1 :col 2))
      (multiple-value-bind (handled result)
          (amoebum:dispatch-slash-command "/cultivar")
        (is-true handled)
        (is-true (typep result 'amoebum.commands:slash-command-result))
        (let ((output (or (amoebum.commands:slash-command-result-output result) "")))
          (is (search "Cultivar daemon:" output :test #'char-equal))
          (is (search "mode=prefer" output :test #'char-equal))
          (is (search "Latest slice: sym_fake" output :test #'char-equal))
          (is (search "origin=location-slice" output :test #'char-equal))
          (is (search "materialized=yes" output :test #'char-equal)))))))

;;; ------------------------------------------------------------------
;;; Live round-trip tests
;;; ------------------------------------------------------------------

(test cultivar-symbol-resolve-tool-live-chat-ui-submit-input
  "Live call through the cultivar-symbol-resolve tool resolves
chat-ui-submit-input at chat-input.lisp:55:10."
  (%skip-unless-cultivar-tools-running
    (let* ((repo (%cultivar-tools-repo-root))
           (file (namestring
                  (merge-pathnames "amoebum/src/ui/chat-input.lisp" repo)))
           (result (%cultivar-tools-call
                    "cultivar-symbol-resolve"
                    (list "file" file "line" 55 "column" 10))))
      (is (listp result))
      (is (stringp (getf result :symbol-id))
          "resolve tool must return a string :symbol-id on success")
      (is (search "chat_ui_submit_input" (or (getf result :symbol-id) ""))
          "symbol-id should reference chat_ui_submit_input")
      (is (or (equal "function" (getf result :kind))
              (null (getf result :kind)))
          "kind, when present, should be \"function\"")
      (is (or (null (getf result :qualified-name))
              (search "chat-ui-submit-input"
                      (or (getf result :qualified-name) "")))
          "qualified-name should mention chat-ui-submit-input"))))

(test cultivar-symbol-references-tool-live
  "Live call through the cultivar-symbol-references tool returns a
non-empty location list and a :returned count."
  (%skip-unless-cultivar-tools-running
    (let* ((repo (%cultivar-tools-repo-root))
           (file (namestring
                  (merge-pathnames "amoebum/src/ui/chat-input.lisp" repo)))
           (resolved (%cultivar-tools-call
                      "cultivar-symbol-resolve"
                      (list "file" file "line" 55 "column" 10)))
           (sym-id (getf resolved :symbol-id)))
      (is (stringp sym-id) "prerequisite: resolve must return a symbol-id")
      (when (stringp sym-id)
        (let ((refs (%cultivar-tools-call
                     "cultivar-symbol-references"
                     (list "symbol-id" sym-id "max-results" 10))))
          (is (listp refs))
          (is (listp (getf refs :locations))
              ":locations must be a list of location plists")
          (is (plusp (length (or (getf refs :locations) '())))
              "at least one location expected for chat-ui-submit-input")
          (is (integerp (getf refs :returned)))
          (is (<= (or (getf refs :returned) 0) 10)
              ":returned must honor the max-results cap"))))))

(test cultivar-span-preview-tool-live
  "Live call through the cultivar-span-preview tool returns a
markdown preview containing a fenced code block."
  (%skip-unless-cultivar-tools-running
    (let* ((repo (%cultivar-tools-repo-root))
           (file (namestring
                  (merge-pathnames "amoebum/src/ui/chat-input.lisp" repo)))
           (resolved (%cultivar-tools-call
                      "cultivar-symbol-resolve"
                      (list "file" file "line" 55 "column" 10)))
           (sym-id (getf resolved :symbol-id)))
      (is (stringp sym-id) "prerequisite: resolve must return a symbol-id")
      (when (stringp sym-id)
        (let* ((preview (%cultivar-tools-call
                         "cultivar-span-preview"
                         (list "symbol-id" sym-id "context-lines" 2)))
               (text (getf preview :preview)))
          (is (listp preview))
          (is (stringp text)
              "span-preview tool must return a markdown string")
          (is (search "```" (or text ""))
              "markdown preview must contain a fenced code block"))))))

(test cultivar-location-slice-tool-live
  "Live call through the cultivar-location-slice tool returns structured JSON fields."
  (%skip-unless-cultivar-tools-running
    (let* ((repo (%cultivar-tools-repo-root))
           (file (namestring
                  (merge-pathnames "amoebum/src/ui/chat-input.lisp" repo)))
           (slice (%cultivar-tools-call
                   "cultivar-location-slice"
                   (list "file" file "line" 55 "column" 10))))
      (is (listp slice))
      (is (eq t (getf slice :resolved-p)))
      (is (stringp (getf slice :symbol-id)))
      (is (listp (getf slice :resolution)))
      (is (listp (getf slice :symbol)))
      (is (member :callers slice))
      (is (member :quality slice)))))
