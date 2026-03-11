(in-package :amoebum/test)

(def-suite review-workflow-suite
  :in amoebum-suite
  :description "I342 local review workflow parity (interactive + headless).")

(in-suite review-workflow-suite)

(defun %i342-run-git (root &rest args)
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program (append (list "git") args)
                        :directory root
                        :ignore-error-status t
                        :output :string
                        :error-output :string)
    (let ((trimmed-out (string-trim '(#\Space #\Tab #\Newline #\Return)
                                    (or stdout "")))
          (trimmed-err (string-trim '(#\Space #\Tab #\Newline #\Return)
                                    (or stderr ""))))
      (unless (zerop (or exit-code 1))
        (error "git ~{~A~^ ~} failed (~D): ~A"
               args
               exit-code
               (if (plusp (length trimmed-err))
                   trimmed-err
                   trimmed-out)))
      trimmed-out)))

(defun %i342-setup-repo (root)
  (ensure-directories-exist root)
  (%i342-run-git root "init")
  (%i342-run-git root "config" "user.name" "Amoebum I342")
  (%i342-run-git root "config" "user.email" "amoebum-i342@example.com")
  (%write-text-file (merge-pathnames #P"README.md" root) "seed\n")
  (%i342-run-git root "add" "--" "README.md")
  (%i342-run-git root "commit" "-m" "chore: seed")
  (%i342-run-git root "branch" "-M" "main")
  root)

(defun %i342-add-feature-change (root)
  (%i342-run-git root "checkout" "-b" "feature/i342-review")
  (%write-text-file (merge-pathnames #P"review.txt" root)
                    "review fixture content\n")
  (%i342-run-git root "add" "--" "review.txt")
  (%i342-run-git root "commit" "-m" "feat: add review fixture")
  root)

(defun %i342-find-substring-position (needle haystack)
  (or (search needle (or haystack "") :test #'char-equal)
      most-positive-fixnum))

(defun %i342-seq->list (value)
  (cond
    ((vectorp value) (loop for item across value collect item))
    ((listp value) value)
    (t '())))

(defun %i342-entry (value key)
  (cond
    ((hash-table-p value)
     (gethash key value))
    ((and (listp value) (keywordp (first value)))
     (getf value (intern (string-upcase key) :keyword)))
    (t
     nil)))

(defun %i342-parse-json (payload)
  (jonathan:parse payload :as :hash-table))

(defun %i342-json-invoke (&rest args)
  (let ((ok nil))
    (let* ((stdout (with-output-to-string (stream)
                     (let ((*standard-output* stream))
                       (setf ok (apply #'amoebum::run-cli-json args)))))
           (json-text (string-trim '(#\Space #\Tab #\Newline #\Return) stdout)))
      (values ok (%i342-parse-json json-text)))))

(defmacro with-i342-review-repo ((root) &body body)
  `(let* ((tmp-root (%make-temp-directory "amoebum-i342"))
          (old-config (amoebum:current-config))
          (old-project-root (amoebum:config-project-root old-config))
          (old-mode (amoebum:config-permission-mode old-config))
          (old-analyzer amoebum::*skill-review-analyzer*))
     (unwind-protect
          (progn
            (%i342-setup-repo tmp-root)
            (amoebum:reload-config :project-root tmp-root)
            (amoebum:setconfig :permission-mode :full-auto)
            (let ((,root tmp-root))
              ,@body))
       (setf amoebum::*skill-review-analyzer* old-analyzer)
       (amoebum:reload-config :project-root old-project-root)
       (amoebum:setconfig :permission-mode old-mode)
       (%delete-directory-tree-safe tmp-root))))

(test i342-review-missing-diff-path
  (with-i342-review-repo (root)
    (declare (ignore root))
    (setf amoebum::*skill-review-analyzer*
          (lambda (&rest _)
            (declare (ignore _))
            (error "Analyzer must not run for missing diff.")))
    (multiple-value-bind (handled result)
        (amoebum:dispatch-slash-command
         "/review main"
         :config (amoebum:current-config)
         :chat-state (amoebum:make-chat-ui-state))
      (let* ((output (amoebum:slash-command-result-output result))
             (payload (amoebum:slash-command-result-payload result)))
        (is-true handled)
        (is (typep result 'amoebum:slash-command-result))
        (is-true (search "Status: missing-diff" output :test #'char-equal))
        (is (string= "missing-diff" (or (gethash "status" payload) "")))
        (is (= 0 (or (gethash "findings_count" payload) -1)))))))

(test i342-review-no-findings-path
  (with-i342-review-repo (root)
    (%i342-add-feature-change root)
    (setf amoebum::*skill-review-analyzer*
          (lambda (&rest _)
            (declare (ignore _))
            (let ((table (make-hash-table :test #'equal)))
              (setf (gethash "summary" table) "No actionable issues found.")
              (setf (gethash "findings" table) #())
              table)))
    (multiple-value-bind (handled result)
        (amoebum:dispatch-slash-command
         "/review main"
         :config (amoebum:current-config)
         :chat-state (amoebum:make-chat-ui-state))
      (let* ((output (amoebum:slash-command-result-output result))
             (payload (amoebum:slash-command-result-payload result))
             (json-block-pos (search "```json" output :test #'char-equal)))
        (is-true handled)
        (is-true (search "Status: no-findings" output :test #'char-equal))
        (is-true (search "Findings: none." output :test #'char-equal))
        (is (string= "no-findings" (or (gethash "status" payload) "")))
        (is (= 0 (or (gethash "findings_count" payload) -1)))
        (is-true (integerp json-block-pos))))))

(test i342-review-findings-present-order-and-headless-payload
  (with-i342-review-repo (root)
    (%i342-add-feature-change root)
    (setf amoebum::*skill-review-analyzer*
          (lambda (&rest _)
            (declare (ignore _))
            (let* ((first (make-hash-table :test #'equal))
                   (second (make-hash-table :test #'equal))
                   (table (make-hash-table :test #'equal)))
              ;; Intentionally unsorted to verify deterministic severity ordering.
              (setf (gethash "severity" first) "low"
                    (gethash "title" first) "Nit: rename local binding"
                    (gethash "detail" first) "Minor readability cleanup suggested."
                    (gethash "file" first) "amoebum/src/main.lisp")
              (setf (gethash "severity" second) "high"
                    (gethash "title" second) "Missing regression test"
                    (gethash "detail" second) "Add a test covering command payload shape."
                    (gethash "file" second) "amoebum/test/review-workflow-test.lisp")
              (setf (gethash "summary" table) "Two findings captured."
                    (gethash "findings" table) (vector first second))
              table)))

    (multiple-value-bind (handled result)
        (amoebum:dispatch-slash-command
         "/review main"
         :config (amoebum:current-config)
         :chat-state (amoebum:make-chat-ui-state))
      (let* ((output (amoebum:slash-command-result-output result))
             (payload (amoebum:slash-command-result-payload result))
             (findings (gethash "findings" payload))
             (first-finding (and (vectorp findings) (> (length findings) 0)
                                 (aref findings 0))))
        (is-true handled)
        (is-true (search "Status: findings-present" output :test #'char-equal))
        (is (< (%i342-find-substring-position "[high]" output)
               (%i342-find-substring-position "[low]" output)))
        (is (string= "findings-present" (or (gethash "status" payload) "")))
        (is (= 2 (or (gethash "findings_count" payload) -1)))
        (is (string= "high" (or (gethash "severity" first-finding) "")))))

    (multiple-value-bind (ok json-payload)
        (%i342-json-invoke "--json" "--command" "/review main")
      (let* ((result (gethash "result" json-payload))
             (result-kind (and (hash-table-p result) (gethash "kind" result)))
             (tool (and (hash-table-p result) (gethash "tool" result)))
             (review-payload (or (and (hash-table-p tool) (gethash "payload" tool))
                                 (gethash "command_payload" json-payload)))
             (review-status (and (hash-table-p review-payload)
                                 (gethash "status" review-payload)))
             (findings (and (hash-table-p review-payload)
                            (gethash "findings" review-payload)))
             (findings-list (%i342-seq->list findings))
             (first-finding (first findings-list))
             (first-severity (%i342-entry first-finding "severity")))
        (is-true ok)
        (is (eq t (gethash "ok" json-payload)))
        (is (string= "tool" (or result-kind "")))
        (is (hash-table-p review-payload))
        (is (string= "findings-present" (or review-status "")))
        (is (= 2 (or (gethash "findings_count" review-payload) -1)))
        (is (string= "high" (or first-severity "")))))))
