;;;; amoebum/src/plan-execution-rollback.lisp
;;;;
;;;; NXT-415: Rollback baseline + restore helpers extracted from
;;;; amoebum/src/plan-execution.lisp.
;;;;
;;;; Owns the git baseline lifecycle for a plan-execution run:
;;;;   - %resolve-plan-execution-rollback-directory
;;;;   - %prepare-plan-execution-rollback-baseline
;;;;   - %drop-plan-execution-rollback-baseline
;;;;   - %rollback-plan-execution-via-git
;;;;
;;;; All git invocations are routed through %plan-execution-run-git from
;;;; src/plan-execution-helpers.lisp so the runner can be stubbed in tests.

(in-package :amoebum)

(defun %resolve-plan-execution-rollback-directory (rollback-directory)
  (let* ((raw (or rollback-directory (uiop:getcwd)))
         (resolved (ignore-errors (uiop:ensure-directory-pathname raw))))
    (or resolved raw)))

(defun %prepare-plan-execution-rollback-baseline (state rollback-directory)
  (check-type state plan-execution-state)
  (let* ((directory (%resolve-plan-execution-rollback-directory rollback-directory))
         (inside-result (%plan-execution-run-git directory
                                                 '("rev-parse" "--is-inside-work-tree"))))
    (setf (plan-execution-state-rollback-baseline-directory state) directory
          (plan-execution-state-rollback-baseline-stash state) nil
          (plan-execution-state-rollback-attempted-p state) nil
          (plan-execution-state-rollback-succeeded-p state) nil
          (plan-execution-state-rollback-notes state) nil)
    (unless (and (%plan-execution-git-ok-p inside-result)
                 (string-equal "true" (%plan-execution-git-output inside-result)))
      (setf (plan-execution-state-rollback-notes state)
            (format nil "Rollback baseline unavailable: ~A"
                    (%plan-execution-git-output inside-result)))
      (return-from %prepare-plan-execution-rollback-baseline nil))
    (let* ((before-stash (%plan-execution-run-git directory
                                                  '("rev-parse" "--verify" "-q" "refs/stash")))
           (before-hash (%plan-execution-git-output before-stash))
           (message (format nil "plan-exec-baseline-~A"
                            (%safe-plan-execution-string
                             (plan-execution-state-run-id state)
                             "unknown")))
           (stash-result (%plan-execution-run-git directory
                                                  (list "stash"
                                                        "push"
                                                        "--include-untracked"
                                                        "--message"
                                                        message)))
           (after-stash (%plan-execution-run-git directory
                                                 '("rev-parse" "--verify" "-q" "refs/stash")))
           (after-hash (%plan-execution-git-output after-stash))
           (stash-created-p (and (%plan-execution-git-ok-p stash-result)
                                 (%plan-execution-git-ok-p after-stash)
                                 (plusp (length after-hash))
                                 (not (string= after-hash before-hash)))))
      (unless (%plan-execution-git-ok-p stash-result)
        (setf (plan-execution-state-rollback-notes state)
              (format nil "Rollback baseline stash failed: ~A"
                      (%plan-execution-git-output stash-result)))
        (return-from %prepare-plan-execution-rollback-baseline nil))
      (when stash-created-p
        (let ((apply-result (%plan-execution-run-git directory
                                                     (list "stash" "apply" "--index" after-hash))))
          (unless (%plan-execution-git-ok-p apply-result)
            (setf (plan-execution-state-rollback-notes state)
                  (format nil "Rollback baseline apply failed: ~A"
                          (%plan-execution-git-output apply-result)))
            (return-from %prepare-plan-execution-rollback-baseline nil)))
        (setf (plan-execution-state-rollback-baseline-stash state) after-hash))
      t)))

(defun %drop-plan-execution-rollback-baseline (state)
  (check-type state plan-execution-state)
  (let ((stash (plan-execution-state-rollback-baseline-stash state))
        (directory (plan-execution-state-rollback-baseline-directory state)))
    (when (and (stringp stash)
               (plusp (length stash)))
      (%plan-execution-run-git directory (list "stash" "drop" stash))
      (setf (plan-execution-state-rollback-baseline-stash state) nil)))
  state)

(defun %rollback-plan-execution-via-git (state)
  (check-type state plan-execution-state)
  (let* ((directory (%resolve-plan-execution-rollback-directory
                     (plan-execution-state-rollback-baseline-directory state)))
         (stash (plan-execution-state-rollback-baseline-stash state))
         (reset-result (%plan-execution-run-git directory '("reset" "--hard" "HEAD")))
         (clean-result (%plan-execution-run-git directory '("clean" "-fd"))))
    (setf (plan-execution-state-rollback-attempted-p state) t)
    (unless (and (%plan-execution-git-ok-p reset-result)
                 (%plan-execution-git-ok-p clean-result))
      (setf (plan-execution-state-rollback-succeeded-p state) nil
            (plan-execution-state-rollback-notes state)
            (format nil "Rollback reset/clean failed: ~A | ~A"
                    (%plan-execution-git-output reset-result)
                    (%plan-execution-git-output clean-result)))
      (return-from %rollback-plan-execution-via-git nil))
    (when (and (stringp stash)
               (plusp (length stash)))
      (let ((apply-result (%plan-execution-run-git directory
                                                   (list "stash" "apply" "--index" stash))))
        (unless (%plan-execution-git-ok-p apply-result)
          (setf (plan-execution-state-rollback-succeeded-p state) nil
                (plan-execution-state-rollback-notes state)
                (format nil "Rollback stash apply failed: ~A"
                        (%plan-execution-git-output apply-result)))
          (return-from %rollback-plan-execution-via-git nil))))
    (%drop-plan-execution-rollback-baseline state)
    (setf (plan-execution-state-rollback-succeeded-p state) t
          (plan-execution-state-rollback-notes state) "Rollback restored git baseline.")
    t))
