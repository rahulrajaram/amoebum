(require :asdf)

(let* ((smoke-file (or *load-truename* *compile-file-truename*))
       (amoebum-dir (and smoke-file (make-pathname :name nil :type nil :defaults smoke-file)))
       (repo-root (and amoebum-dir (truename (merge-pathnames #P"../" amoebum-dir)))))
  (unless repo-root
    (error "Unable to resolve repository root from ~S" smoke-file))

  (load (merge-pathnames #P"ptui/.tools/quicklisp/setup.lisp" repo-root))
  (require :asdf)

  (let* ((asdf-pkg (or (find-package "ASDF")
                       (error "Missing package ASDF")))
         (load-asd-sym (or (find-symbol "LOAD-ASD" asdf-pkg)
                           (error "Missing symbol LOAD-ASD in ASDF package")))
         (load-system-sym (or (find-symbol "LOAD-SYSTEM" asdf-pkg)
                              (error "Missing symbol LOAD-SYSTEM in ASDF package")))
         (load-asd-fn (symbol-function load-asd-sym))
         (load-system-fn (symbol-function load-system-sym)))
    (funcall load-asd-fn (merge-pathnames #P"pseudopod/pseudopod.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"ptui/ptui.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"sw4rm-sdk/sw4rm-sdk.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"amoebum/amoebum.asd" repo-root))
    (funcall load-system-fn "amoebum"))

  (let* ((amoebum-pkg (or (find-package "AMOEBUM")
                          (error "Missing package AMOEBUM after load.")))
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn
           (lambda (name)
             (symbol-function (funcall symbol-in name amoebum-pkg))))
         (clear-permission-rules (funcall fn "CLEAR-PERMISSION-RULES"))
         (clear-permission-cache (funcall fn "CLEAR-PERMISSION-CACHE"))
         (permission-cache-metrics (funcall fn "PERMISSION-CACHE-METRICS"))
         (add-permission-rule (funcall fn "ADD-PERMISSION-RULE"))
         (check-permission (funcall fn "CHECK-PERMISSION"))
         (last-permission-decision-trace (funcall fn "LAST-PERMISSION-DECISION-TRACE"))
         (permission-decision-history (funcall fn "PERMISSION-DECISION-HISTORY"))
         (clear-permission-decision-history (funcall fn "CLEAR-PERMISSION-DECISION-HISTORY"))
         (explain-permission-decision (funcall fn "EXPLAIN-PERMISSION-DECISION")))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args)))))
      (funcall clear-permission-decision-history)
      (funcall clear-permission-rules)
      (funcall clear-permission-cache :reason :smoke-start)

      (funcall add-permission-rule
               :effect :allow
               :tool :write-file
               :path "/tmp/obs/**"
               :source :global)
      (funcall add-permission-rule
               :effect :deny
               :tool :write-file
               :path "/tmp/obs/.env"
               :source :project)

      (assert-true
       (eq (funcall check-permission
                    :tool :write-file
                    :path "/tmp/obs/.env"
                    :permission-mode :full-auto)
           :deny)
       "Expected deny decision for protected path.")
      (assert-true
       (eq (funcall check-permission
                    :tool :write-file
                    :path "/tmp/obs/.env"
                    :permission-mode :full-auto)
           :deny)
       "Expected stable deny decision on repeated lookup.")

      (let* ((trace (funcall last-permission-decision-trace))
             (decision-id (and trace (getf trace :decision-id)))
             (phases (and trace (getf trace :evaluation-trace)))
             (explain (and decision-id
                           (funcall explain-permission-decision
                                    :decision-id decision-id)))
             (historical (and explain (getf explain :historical)))
             (replay (and explain (getf explain :replay)))
             (metrics (funcall permission-cache-metrics))
             (hits (or (getf metrics :hits) 0))
             (misses (or (getf metrics :misses) 0))
             (invalidations (or (getf metrics :invalidations) 0))
             (history (funcall permission-decision-history :limit 5)))
        (assert-true decision-id "Expected decision trace to have an ID.")
        (assert-true (and (listp phases) (plusp (length phases)))
                     "Expected machine-readable evaluation trace phases.")
        (let ((path-phase (find :path phases :key (lambda (entry) (getf entry :phase)) :test #'eq)))
          (assert-true path-phase "Expected path phase in evaluation trace.")
          (assert-true (getf path-phase :matched-rule-id)
                       "Expected matched-rule-id in path phase trace.")
          (assert-true (numberp (or (getf path-phase :specificity) 0))
                       "Expected numeric specificity in path phase trace."))
        (assert-true (and historical replay)
                     "Expected explain API to provide historical and replay payloads.")
        (assert-true (equal (getf historical :decision) (getf replay :decision))
                     "Expected replay decision to match historical decision.")
        (assert-true (plusp hits)
                     "Expected permission cache hits after repeated evaluation, got ~D."
                     hits)
        (assert-true (plusp misses)
                     "Expected permission cache misses after first evaluation, got ~D."
                     misses)
        (assert-true (plusp invalidations)
                     "Expected permission cache invalidations after rule reloads, got ~D."
                     invalidations)
        (assert-true (and (listp history) (plusp (length history)))
                     "Expected non-empty decision history for explain/log replay."))))

  (format t "AMOEBUM_PERMISSION_TRACE_SMOKE_OK~%"))
