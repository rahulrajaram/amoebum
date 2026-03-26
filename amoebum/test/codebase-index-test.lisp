(in-package :amoebum/test)

;;; ============================================================
;;; I248: Codebase Symbol Indexing via sb-introspect
;;; ============================================================

(def-suite codebase-index-suite :in amoebum-suite)
(in-suite codebase-index-suite)

(defun %i248-write-temp-system (root)
  (let* ((system-name "i248-temp-system")
         (system-root (merge-pathnames #P"i248-temp-system/" root))
         (asd-path (merge-pathnames #P"i248-temp-system.asd" system-root))
         (package-path (merge-pathnames #P"package.lisp" system-root))
         (core-path (merge-pathnames #P"core.lisp" system-root)))
    (ensure-directories-exist (merge-pathnames #P".keep" system-root))
    (%write-text-file
     asd-path
     "(asdf:defsystem \"i248-temp-system\"
  :description \"Temporary test system for tranche I248\"
  :serial t
  :components
  ((:file \"package\")
   (:file \"core\")))
")
    (%write-text-file
     package-path
     "(defpackage :i248-temp
  (:use :cl)
  (:export #:i248-temp-fn
           #:i248-temp-macro
           #:*i248-temp-var*
           #:i248-temp-class))
")
    (%write-text-file
     core-path
     "(in-package :i248-temp)

(defparameter *i248-temp-var* 1)

(defclass i248-temp-class ()
  ((value :initarg :value :accessor i248-temp-value :initform 0)))

(defmacro i248-temp-macro (x)
  `(+ ,x *i248-temp-var*))

(defun i248-temp-fn (x)
  (i248-temp-macro x))
")
    (values system-name system-root asd-path core-path)))

(defun %i248-load-temp-system (asd-path system-name)
  (asdf:load-asd asd-path)
  (asdf:load-system system-name))

(test i248-default-repo-map-token-target-range
  "Default repo-map budget should remain in the tranche target range."
  (is (<= 500 amoebum:+default-repo-map-token-target+ 2000)))

(test i248-indexes-symbols-from-loaded-system
  "Indexer should extract symbols from loaded ASDF systems via sb-introspect."
  (let* ((tmp-root (%make-temp-directory "amoebum-i248"))
         (system-name nil)
         (system-root nil)
         (asd-path nil)
         (core-path nil)
         (index nil))
    (declare (ignore system-root core-path))
    (unwind-protect
        (progn
          (multiple-value-setq (system-name system-root asd-path core-path)
            (%i248-write-temp-system tmp-root))
          (%i248-load-temp-system asd-path system-name)
          (setf index (amoebum:make-codebase-index :project-root tmp-root
                                                   :language :common-lisp))
          (multiple-value-bind (_ stats)
              (amoebum:index-loaded-asdf-systems
               index
               :systems (list system-name)
               :refresh t
               :repo-map-token-target 700)
            (declare (ignore _))
            (is-true (getf stats :reindexed-p))
            (is (> (getf stats :entries 0) 0))
            (is (> (getf stats :files-tracked 0) 0)))
          (let ((functions (amoebum:index-find-symbol index "I248-TEMP-FN"
                                                      :package "I248-TEMP"))
                (macros (amoebum:index-find-symbol index "I248-TEMP-MACRO"
                                                   :package "I248-TEMP"))
                (classes (amoebum:index-find-symbol index "I248-TEMP-CLASS"
                                                    :package "I248-TEMP"))
                (variables (amoebum:index-find-symbol index "*I248-TEMP-VAR*"
                                                      :package "I248-TEMP")))
            (is (>= (length functions) 1))
            (is (>= (length macros) 1))
            (is (>= (length classes) 1))
            (is (>= (length variables) 1))))
      (%delete-directory-tree-safe tmp-root)))

(test i248-detects-changed-files-incrementally
  "Incremental update should report changed files when source mtimes advance."
  (let* ((tmp-root (%make-temp-directory "amoebum-i248"))
         (system-name nil)
         (system-root nil)
         (asd-path nil)
         (core-path nil)
         (index nil))
    (declare (ignore system-root))
    (unwind-protect
        (progn
          (multiple-value-setq (system-name system-root asd-path core-path)
            (%i248-write-temp-system tmp-root))
          (%i248-load-temp-system asd-path system-name)
          (setf index (amoebum:make-codebase-index :project-root tmp-root))
          (multiple-value-bind (_ stats1)
              (amoebum:index-loaded-asdf-systems
               index
               :systems (list system-name)
               :refresh t)
            (declare (ignore _))
            (is (> (getf stats1 :files-changed 0) 0)))
          (multiple-value-bind (_ stats2)
              (amoebum:index-loaded-asdf-systems
               index
               :systems (list system-name)
               :refresh nil)
            (declare (ignore _))
            (is (= 0 (getf stats2 :files-changed 0)))
            (is (not (getf stats2 :reindexed-p))))
          ;; file-write-date resolution is second-level on common filesystems.
          (sleep 1)
          (%write-text-file
           core-path
           "(in-package :i248-temp)

(defparameter *i248-temp-var* 2)

(defclass i248-temp-class ()
  ((value :initarg :value :accessor i248-temp-value :initform 1)))

(defmacro i248-temp-macro (x)
  `(+ ,x *i248-temp-var*))

(defun i248-temp-fn (x)
  (i248-temp-macro x))
")
          (multiple-value-bind (_ stats3)
              (amoebum:index-loaded-asdf-systems
               index
               :systems (list system-name)
               :refresh nil)
            (declare (ignore _))
            (is (> (getf stats3 :files-changed 0) 0))
            (is-true (getf stats3 :reindexed-p)))))
      (%delete-directory-tree-safe tmp-root))))

(test i248-index-slash-command-reports-stats
  "Slash command /index should execute indexing and report concrete stats."
  (let* ((tmp-root (%make-temp-directory "amoebum-i248"))
         (system-name nil)
         (system-root nil)
         (asd-path nil)
         (core-path nil)
         (old-index amoebum:*active-codebase-index*))
    (declare (ignore system-root core-path))
    (unwind-protect
        (progn
          (multiple-value-setq (system-name system-root asd-path core-path)
            (%i248-write-temp-system tmp-root))
          (%i248-load-temp-system asd-path system-name)
          (setf amoebum:*active-codebase-index*
                (amoebum:make-codebase-index :project-root tmp-root
                                             :language :common-lisp))
          (multiple-value-bind (handled result)
              (amoebum:dispatch-slash-command
               (format nil "/index --refresh --system ~A --tokens 700" system-name))
            (is-true handled)
            (let ((output (or (amoebum.commands:slash-command-result-output result) "")))
              (is-true (search "codebase index" output :test #'char-equal))
              (is-true (search "tracked" output :test #'char-equal))
              (is-true (search "repo-map" output :test #'char-equal)))))
      (setf amoebum:*active-codebase-index* old-index)
      (%delete-directory-tree-safe tmp-root))))

(test codebase-index-smoke-sentinel
  (is-true t)
  (format t "CODEBASE_INDEX_SMOKE_OK~%"))
