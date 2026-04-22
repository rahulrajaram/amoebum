;;;; amoebum/test/package-export-golden-test.lisp
;;;;
;;;; NXT-398: Package-export stability golden fixture per Amoebum subsystem.
;;;;
;;;; This script can be used in two complementary modes:
;;;;
;;;;  (a) Standalone script — invoked as
;;;;        sbcl --script amoebum/test/package-export-golden-test.lisp
;;;;      Bootstraps Quicklisp + ASDF, loads the `amoebum` system, then
;;;;      compares the live external symbol set of each target subsystem
;;;;      package against a checked-in golden fixture under
;;;;      amoebum/test/snapshots/package-exports/.
;;;;
;;;;  (b) Loadable test entry — when the file is loaded inside an
;;;;      already-running SBCL where `:amoebum` is loaded (e.g. via the
;;;;      `--load` + `--eval` chain pattern from bin/amoebum-focused-verify.sh),
;;;;      the public function `amoebum.test.package-export-golden:run` runs the
;;;;      same comparison logic without re-bootstrapping.
;;;;
;;;; Modes:
;;;;
;;;;  - Default: comparison mode. Mismatches print a diff and exit non-zero.
;;;;  - AMOEBUM_UPDATE_SNAPSHOTS=1 in the environment: write/refresh every
;;;;    golden file and exit 0. Use this after a deliberate facade change.
;;;;
;;;; Determinism: exports are sorted with `string<` over their package-qualified
;;;; names so the on-disk goldens are byte-stable across SBCL versions.

(defpackage #:amoebum.test.package-export-golden
  (:use #:cl)
  (:export #:run
           #:run-and-exit
           #:+target-packages+
           #:+golden-relative-path+))

(in-package #:amoebum.test.package-export-golden)

;;; ---------------------------------------------------------------------------
;;; Target subsystem inventory
;;; ---------------------------------------------------------------------------

(defparameter +target-packages+
  '(;; Root facade — capacity-capped by +amoebum-root-export-max+.
    ("amoebum"             :registered-p t)
    ;; Wrapped facades — currently installed by api-facades.lisp.
    ("amoebum.ui"          :registered-p t)
    ("amoebum.commands"    :registered-p t)
    ("amoebum.workers"     :registered-p t)
    ("amoebum.config"      :registered-p t)
    ("amoebum.notifications" :registered-p t)
    ("amoebum.sessions"    :registered-p t)
    ("amoebum.plan"        :registered-p t)
    ("amoebum.extensions"  :registered-p t)
    ("amoebum.observability" :registered-p t)
    ("amoebum.safety"      :registered-p t)
    ("amoebum.tools"       :registered-p t)
    ("amoebum.sandbox"     :registered-p t)
    ;; FP kernel — small but stable surface; no facade wrap.
    ("amoebum.fp"          :registered-p t)
    ;; Sub-namespaces declared in src/package-domains.lisp.
    ("amoebum.commands.plan"          :registered-p t)
    ("amoebum.commands.history"       :registered-p t)
    ("amoebum.commands.index"         :registered-p t)
    ("amoebum.commands.self-modify"   :registered-p t)
    ("amoebum.commands.permissions"   :registered-p t)
    ;; Anticipated subsystem packages (post-NXT-388/NXT-390/NXT-386 splits).
    ;; If they don't exist yet we capture an EMPTY golden documenting that
    ;; the surface is intentionally sub-package-less today; the moment a
    ;; future split introduces them as real packages, the golden will fail
    ;; and the implementer must rerun with AMOEBUM_UPDATE_SNAPSHOTS=1.
    ("amoebum.memory"      :registered-p nil)
    ("amoebum.tools.shell" :registered-p nil)
    ("amoebum.macros"      :registered-p nil))
  "List of (PACKAGE-NAME-STRING &key REGISTERED-P) tuples for golden capture.

REGISTERED-P is the *expected* presence of the package after `amoebum` is
loaded. If a package is expected to exist but doesn't, we fail loudly. If a
package is not yet registered we capture an EMPTY golden (with a comment
header) so a later split that introduces the package fails the golden and
forces a deliberate update.")

(defparameter +golden-relative-path+
  #P"amoebum/test/snapshots/package-exports/")

;;; ---------------------------------------------------------------------------
;;; Path helpers
;;; ---------------------------------------------------------------------------

(defun %compute-script-truename ()
  (or *load-truename*
      *compile-file-truename*
      ;; Last resort: assume CWD contains the script at its canonical location.
      (truename
       (merge-pathnames
        #P"amoebum/test/package-export-golden-test.lisp"
        (make-pathname :directory '(:relative))))))

(defun %script-repo-root (script-truename)
  (truename
   (merge-pathnames #P"../../" (make-pathname :name nil
                                              :type nil
                                              :defaults script-truename))))

(defun %golden-directory (repo-root)
  (let ((merged (merge-pathnames +golden-relative-path+ repo-root)))
    ;; Force directory-form pathname without depending on UIOP at read time.
    (make-pathname
     :directory (append (pathname-directory merged)
                        (let ((name (pathname-name merged)))
                          (when (and name (not (eq name :unspecific)))
                            (list name))))
     :name nil
     :type nil
     :defaults merged)))

(defun %golden-path (repo-root package-name)
  (merge-pathnames (concatenate 'string package-name ".txt")
                   (%golden-directory repo-root)))

;;; ---------------------------------------------------------------------------
;;; Symbol gathering (deterministic)
;;; ---------------------------------------------------------------------------

(defun %find-package-by-string (package-name)
  "Locate a package by lowercase NAME-STRING; tolerate case differences.

In Common Lisp, package names are case-sensitive strings (typically
upper-case) so `find-package \"amoebum\"` returns NIL while
`find-package \"AMOEBUM\"` succeeds. We accept either case in the input
and probe both variants."
  (or (find-package package-name)
      (find-package (string-upcase package-name))
      (find-package (string-downcase package-name))))

(defun %external-symbol-names (package-name)
  "Return a sorted, de-duplicated list of external symbol names for PACKAGE-NAME.

Returns NIL if the package does not exist (caller decides what to do)."
  (let ((package (%find-package-by-string package-name)))
    (if (null package)
        nil
        (let ((names (make-hash-table :test #'equal)))
          (do-external-symbols (symbol package)
            (setf (gethash (symbol-name symbol) names) t))
          (sort (loop for name being the hash-keys of names collect name)
                #'string<)))))

(defun %render-golden-text (package-name names &key empty-stub-reason)
  "Build the byte-deterministic golden text for PACKAGE-NAME.

If NAMES is empty and EMPTY-STUB-REASON is supplied, prepend an explanatory
comment header so the golden self-documents why the surface is empty."
  (with-output-to-string (out)
    (cond
      ((and (null names) empty-stub-reason)
       (format out ";; package-exports golden: ~A~%" package-name)
       (format out ";; status: empty (intentional stub)~%")
       (format out ";; reason: ~A~%" empty-stub-reason)
       (format out ";; If a future subsystem split registers this package, this golden")
       (format out "~%;; will fail and the implementer must rerun with")
       (format out "~%;; AMOEBUM_UPDATE_SNAPSHOTS=1 to capture the new public surface.~%"))
      (t
       (dolist (name names)
         (format out "~A:~A~%" package-name name))))))

;;; ---------------------------------------------------------------------------
;;; Comparison + diff
;;; ---------------------------------------------------------------------------

(defstruct package-result
  package-name
  registered-p-expected
  package-found-p
  golden-path
  golden-existed-p
  status            ;; :pass | :drift | :missing-golden | :unexpected-missing-package
  added             ;; list of names present live but not in golden
  removed           ;; list of names present in golden but not live
  live-count
  golden-count
  bytes-on-disk)

(defun %parse-golden-text (text package-name)
  "Parse a golden file's text back into a sorted symbol-name list.

Lines starting with `;` are comments; blank lines are ignored. Other lines
must look like `package-name:SYMBOL`. Returns NIL for an empty/comment-only
file (which is the documented stub format)."
  (let ((prefix (concatenate 'string package-name ":"))
        (collected '()))
    (with-input-from-string (in text)
      (loop for line = (read-line in nil nil)
            while line
            do (let ((stripped (string-trim '(#\Space #\Tab #\Return) line)))
                 (cond
                   ((zerop (length stripped)))
                   ((char= (char stripped 0) #\;))
                   ((and (>= (length stripped) (length prefix))
                         (string= prefix stripped :end2 (length prefix)))
                    (push (subseq stripped (length prefix)) collected))
                   (t
                    (error "Malformed golden line for package ~A: ~S"
                           package-name line))))))
    (sort collected #'string<)))

(defun %diff-symbol-lists (live golden)
  "Return (VALUES ADDED REMOVED). ADDED = in LIVE not GOLDEN; REMOVED = vice versa."
  (let ((live-set (make-hash-table :test #'equal))
        (golden-set (make-hash-table :test #'equal))
        (added '())
        (removed '()))
    (dolist (n live) (setf (gethash n live-set) t))
    (dolist (n golden) (setf (gethash n golden-set) t))
    (dolist (n live)
      (unless (gethash n golden-set) (push n added)))
    (dolist (n golden)
      (unless (gethash n live-set) (push n removed)))
    (values (sort added #'string<) (sort removed #'string<))))

(defun %file-byte-size (path)
  (handler-case
      (with-open-file (s path :element-type '(unsigned-byte 8))
        (file-length s))
    (file-error () nil)))

(defun %read-text-if-exists (path)
  (when (probe-file path)
    (with-open-file (in path :direction :input :external-format :utf-8)
      (with-output-to-string (out)
        (loop for line = (read-line in nil nil)
              while line
              do (write-line line out))))))

(defun %write-golden (path text)
  (ensure-directories-exist path)
  (with-open-file (out path
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :external-format :utf-8)
    (write-string text out)))

;;; ---------------------------------------------------------------------------
;;; Per-package processing
;;; ---------------------------------------------------------------------------

(defun %process-package (entry repo-root update-mode-p)
  (destructuring-bind (package-name &key registered-p) entry
    (let* ((package-found-p (not (null (%find-package-by-string package-name))))
           (golden-path (%golden-path repo-root package-name))
           (golden-existed-p (not (null (probe-file golden-path)))))
      (cond
        ;; Expected to exist but doesn't -> hard fail.
        ((and registered-p (not package-found-p))
         (make-package-result
          :package-name package-name
          :registered-p-expected registered-p
          :package-found-p nil
          :golden-path golden-path
          :golden-existed-p golden-existed-p
          :status :unexpected-missing-package
          :live-count 0
          :golden-count 0))
        ;; Either expected-and-present, or not-expected (stub case).
        (t
         (let* ((live-names (when package-found-p
                              (%external-symbol-names package-name)))
                (empty-reason
                  (cond
                    ((and (not registered-p) (not package-found-p))
                     "Subsystem package not yet registered; placeholder for future split.")
                    ((and (not registered-p) package-found-p (null live-names))
                     "Subsystem package registered but exports no symbols today.")
                    ((null live-names)
                     "Package registered with no exports today (intentional stub)."))))
           (cond
             ;; Update mode: rewrite the golden unconditionally.
             (update-mode-p
              (let ((text (%render-golden-text package-name live-names
                                               :empty-stub-reason empty-reason)))
                (%write-golden golden-path text)
                (make-package-result
                 :package-name package-name
                 :registered-p-expected registered-p
                 :package-found-p package-found-p
                 :golden-path golden-path
                 :golden-existed-p golden-existed-p
                 :status :pass
                 :added nil
                 :removed nil
                 :live-count (length live-names)
                 :golden-count (length live-names)
                 :bytes-on-disk (%file-byte-size golden-path))))
             ;; Comparison mode but no golden present yet.
             ((not golden-existed-p)
              (make-package-result
               :package-name package-name
               :registered-p-expected registered-p
               :package-found-p package-found-p
               :golden-path golden-path
               :golden-existed-p nil
               :status :missing-golden
               :added live-names
               :removed nil
               :live-count (length live-names)
               :golden-count 0))
             ;; Comparison mode with a golden present.
             (t
              (let* ((golden-text (%read-text-if-exists golden-path))
                     (golden-names (%parse-golden-text golden-text package-name)))
                (multiple-value-bind (added removed)
                    (%diff-symbol-lists live-names golden-names)
                  (make-package-result
                   :package-name package-name
                   :registered-p-expected registered-p
                   :package-found-p package-found-p
                   :golden-path golden-path
                   :golden-existed-p t
                   :status (if (and (null added) (null removed))
                               :pass
                               :drift)
                   :added added
                   :removed removed
                   :live-count (length live-names)
                   :golden-count (length golden-names)
                   :bytes-on-disk (%file-byte-size golden-path))))))))))))

;;; ---------------------------------------------------------------------------
;;; Reporting
;;; ---------------------------------------------------------------------------

(defun %print-result (result update-mode-p stream)
  (let ((name (package-result-package-name result))
        (live-count (package-result-live-count result))
        (golden-count (package-result-golden-count result)))
    (case (package-result-status result)
      (:pass
       (cond
         (update-mode-p
          (format stream
                  "AMOEBUM_PACKAGE_EXPORT_GOLDEN_UPDATED package=~A count=~D~%"
                  name live-count))
         (t
          (format stream
                  "AMOEBUM_PACKAGE_EXPORT_GOLDEN_OK package=~A count=~D~%"
                  name live-count))))
      (:missing-golden
       (format stream
               "AMOEBUM_PACKAGE_EXPORT_GOLDEN_MISSING package=~A live=~D~%"
               name live-count)
       (format stream
               "  (Run with AMOEBUM_UPDATE_SNAPSHOTS=1 to capture initial golden.)~%"))
      (:drift
       (format stream
               "AMOEBUM_PACKAGE_EXPORT_GOLDEN_DRIFT package=~A added=~D removed=~D live=~D golden=~D~%"
               name
               (length (package-result-added result))
               (length (package-result-removed result))
               live-count
               golden-count)
       (when (package-result-added result)
         (format stream "  ADDED (present live, missing in golden):~%")
         (dolist (n (package-result-added result))
           (format stream "    + ~A:~A~%" name n)))
       (when (package-result-removed result)
         (format stream "  REMOVED (present in golden, missing live):~%")
         (dolist (n (package-result-removed result))
           (format stream "    - ~A:~A~%" name n))))
      (:unexpected-missing-package
       (format stream
               "AMOEBUM_PACKAGE_EXPORT_GOLDEN_PACKAGE_MISSING package=~A~%"
               name)
       (format stream
               "  Package was expected to be registered after loading :amoebum but find-package returned NIL.~%")))))

;;; ---------------------------------------------------------------------------
;;; Public entry points
;;; ---------------------------------------------------------------------------

(defun %posix-getenv (name)
  #+sbcl (sb-ext:posix-getenv name)
  #-sbcl (let ((sym (find-symbol "GETENV" "UIOP")))
           (and sym (funcall (symbol-function sym) name))))

(defun %getenv-truthy-p (name)
  (let ((value (%posix-getenv name)))
    (and value
         (member (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                               value))
                 '("1" "true" "yes" "on")
                 :test #'string=))))

(defun run (&key repo-root update-mode-p (stream *standard-output*))
  "Run the package-export golden comparison. Returns T if all packages pass."
  (let* ((script-truename (%compute-script-truename))
         (effective-repo-root (or repo-root (%script-repo-root script-truename)))
         (effective-update-mode-p (or update-mode-p
                                      (%getenv-truthy-p "AMOEBUM_UPDATE_SNAPSHOTS")))
         (results (mapcar (lambda (entry)
                            (%process-package entry effective-repo-root
                                              effective-update-mode-p))
                          +target-packages+))
         (drift-count 0)
         (missing-count 0)
         (missing-package-count 0))
    (format stream "Package-export golden test (NXT-398) — repo-root=~A mode=~A~%"
            (namestring effective-repo-root)
            (if effective-update-mode-p "UPDATE" "COMPARE"))
    (format stream "Goldens dir: ~A~%" (namestring (%golden-directory effective-repo-root)))
    (terpri stream)
    (dolist (result results)
      (%print-result result effective-update-mode-p stream)
      (case (package-result-status result)
        (:drift (incf drift-count))
        (:missing-golden (incf missing-count))
        (:unexpected-missing-package (incf missing-package-count))))
    (terpri stream)
    (format stream
            "Summary: ~D packages, drift=~D missing-goldens=~D missing-packages=~D~%"
            (length results) drift-count missing-count missing-package-count)
    (cond
      (effective-update-mode-p
       (format stream "AMOEBUM_PACKAGE_EXPORT_GOLDEN_OK mode=update count=~D~%"
               (length results))
       t)
      ((zerop (+ drift-count missing-count missing-package-count))
       (format stream "AMOEBUM_PACKAGE_EXPORT_GOLDEN_OK mode=compare count=~D~%"
               (length results))
       t)
      (t
       (format stream
               "AMOEBUM_PACKAGE_EXPORT_GOLDEN_FAIL drift=~D missing-goldens=~D missing-packages=~D~%"
               drift-count missing-count missing-package-count)
       nil))))

(defun run-and-exit (&key repo-root update-mode-p)
  (let ((ok (run :repo-root repo-root :update-mode-p update-mode-p)))
    #+sbcl (sb-ext:exit :code (if ok 0 1))
    #-sbcl (let ((q (or (find-symbol "QUIT" "UIOP")
                        (find-symbol "EXIT" "UIOP"))))
             (when q (funcall (symbol-function q) (if ok 0 1))))))

;;; ---------------------------------------------------------------------------
;;; Standalone --script bootstrap
;;; ---------------------------------------------------------------------------

(defun %bootstrap-and-load-amoebum (repo-root)
  (let* ((quicklisp-candidates
           (list (merge-pathnames #P"ptui/.tools/quicklisp/setup.lisp" repo-root)
                 (merge-pathnames #P"quicklisp/setup.lisp"
                                  (user-homedir-pathname))))
         (quicklisp-setup
           (find-if (lambda (path) (probe-file path)) quicklisp-candidates)))
    (unless quicklisp-setup
      (error "Quicklisp setup.lisp not found. Tried: ~{~A~^, ~}"
             (mapcar #'namestring quicklisp-candidates)))
    (load quicklisp-setup)
    (require :asdf)
    (let* ((asdf-pkg (or (find-package "ASDF") (error "Missing ASDF package")))
           (load-asd-fn (symbol-function (find-symbol "LOAD-ASD" asdf-pkg)))
           (load-system-fn (symbol-function (find-symbol "LOAD-SYSTEM" asdf-pkg)))
           (warn-sym (or (find-symbol "*COMPILE-FILE-WARNINGS-BEHAVIOUR*" asdf-pkg)
                         (find-symbol "*COMPILE-FILE-WARNINGS-BEHAVIOR*" asdf-pkg))))
      (when warn-sym (setf (symbol-value warn-sym) :ignore))
      (dolist (asd-rel '("pseudopod/pseudopod.asd"
                         "sw4rm-sdk/sw4rm-sdk.asd"
                         "ptui/ptui.asd"
                         "amoebum/amoebum.asd"))
        (funcall load-asd-fn (merge-pathnames asd-rel repo-root)))
      (funcall load-system-fn "amoebum"))
    (unless (find-package :amoebum)
      (error "amoebum system loaded but :amoebum package not registered."))))

(defun %standalone-main ()
  (let* ((script-truename (%compute-script-truename))
         (repo-root (%script-repo-root script-truename)))
    (%bootstrap-and-load-amoebum repo-root)
    (run-and-exit :repo-root repo-root)))

;;; Execute the standalone bootstrap when sourced via `sbcl --script`.
;;; Detection: under --script, the toplevel is loading this file — *load-pathname*
;;; is bound to it. We also gate on the absence of the :amoebum package; if
;;; :amoebum is already loaded (i.e. we were `--load`ed inside an existing image
;;; for use as a callable) we skip the bootstrap and let the caller invoke
;;; `run` / `run-and-exit` explicitly.
(eval-when (:load-toplevel :execute)
  (let* ((load-name (and *load-pathname* (pathname-name *load-pathname*)))
         (script-mode-p
           (and load-name
                (string= load-name "package-export-golden-test")
                (null (find-package :amoebum)))))
    (when script-mode-p
      (%standalone-main))))
