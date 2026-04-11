(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Codebase Indexer Smoke Tests (I96)
;;; ---------------------------------------------------------------------------

(def-suite indexer-suite :in amoebum-suite
  :description "Codebase indexer smoke tests.")

(in-suite indexer-suite)

(test symbol-entry-creation
  (let ((entry (amoebum.observability:make-symbol-entry
                :name "FOO"
                :package "CL-USER"
                :kind :function
                :file "/tmp/test.lisp"
                :line 42
                :signature "(x y &key z)"
                :doc "A test function.")))
    (is (amoebum.observability:symbol-entry-p entry))
    (is (string= "FOO" (amoebum.observability:symbol-entry-name entry)))
    (is (string= "CL-USER" (amoebum.observability:symbol-entry-package entry)))
    (is (eq :function (amoebum.observability:symbol-entry-kind entry)))
    (is (= 42 (amoebum.observability:symbol-entry-line entry)))))

(test codebase-index-creation
  (let ((idx (amoebum.observability:make-codebase-index :project-root "/tmp/test-project/")))
    (is (amoebum.observability:codebase-index-p idx))
    (is (null (amoebum.observability:codebase-index-entries idx)))
    (is (integerp (amoebum.observability:codebase-index-created-at idx)))))

(test index-cl-package
  (let ((idx (amoebum.observability:make-codebase-index)))
    (amoebum.observability:index-package-symbols idx :cl :external-only t)
    (is (> (length (amoebum.observability:codebase-index-entries idx)) 100))
    ;; CONS should be in there
    (let ((cons-entries (amoebum.observability:index-find-symbol idx "CONS" :package "COMMON-LISP")))
      (is (>= (length cons-entries) 1)))))

(test index-find-symbol-basic
  (let ((idx (amoebum.observability:make-codebase-index)))
    (amoebum.observability:index-package-symbols idx :cl :external-only t)
    (let ((results (amoebum.observability:index-find-symbol idx "CAR")))
      (is (>= (length results) 1))
      (is (string= "CAR" (amoebum.observability:symbol-entry-name (first results)))))))

(test index-find-symbol-by-kind
  (let ((idx (amoebum.observability:make-codebase-index)))
    (amoebum.observability:index-package-symbols idx :cl :external-only t)
    (let ((fns (amoebum.observability:index-find-symbol idx "FORMAT" :kind :function)))
      (is (>= (length fns) 1)))))

(test generate-repo-map-basic
  (let ((idx (amoebum.observability:make-codebase-index)))
    ;; Index a small package
    (amoebum.observability:index-package-symbols idx :amoebum :external-only t)
    (let ((map (amoebum.observability:generate-repo-map idx :max-tokens 1000)))
      (is (stringp map))
      (is (plusp (length map)))
      (is (search "AMOEBUM" map)))))

(test generate-repo-map-token-limit
  (let ((idx (amoebum.observability:make-codebase-index)))
    (amoebum.observability:index-package-symbols idx :cl :external-only t)
    (let ((small (amoebum.observability:generate-repo-map idx :max-tokens 100))
          (large (amoebum.observability:generate-repo-map idx :max-tokens 10000)))
      (is (< (length small) (length large))))))

(test index-statistics
  (let ((idx (amoebum.observability:make-codebase-index)))
    (amoebum.observability:index-package-symbols idx :cl :external-only t)
    (let ((stats (amoebum.observability:index-statistics idx)))
      (is (listp stats))
      (is (> (getf stats :total-entries) 0))
      (is (listp (getf stats :kinds)))
      (is (listp (getf stats :packages))))))

(test index-incremental-file-tracking
  (let ((idx (amoebum.observability:make-codebase-index)))
    ;; Record a mtime
    (setf (gethash "/tmp/test-file.lisp" (amoebum.observability:codebase-index-file-mtimes idx))
          (get-universal-time))
    (is (= 1 (hash-table-count (amoebum.observability:codebase-index-file-mtimes idx))))
    (is (= 1 (getf (amoebum.observability:index-statistics idx) :files-tracked)))))

(test index-directory-nonexistent
  "Indexing a nonexistent directory should not error."
  (let ((idx (amoebum.observability:make-codebase-index)))
    (let ((count (amoebum.observability:index-directory idx "/tmp/nonexistent-amoebum-dir-12345/")))
      (is (= 0 count)))))

(test index-package-symbols-pseudopod
  "Index the pseudopod package."
  (let ((idx (amoebum.observability:make-codebase-index)))
    (amoebum.observability:index-package-symbols idx :pseudopod :external-only t)
    (is (> (length (amoebum.observability:codebase-index-entries idx)) 10))))

(test index-find-by-file-empty
  (let ((idx (amoebum.observability:make-codebase-index)))
    (is (null (amoebum.observability:index-find-by-file idx "/nonexistent/file.lisp")))))

(test symbol-entry-defaults
  (let ((entry (amoebum.observability:make-symbol-entry :name "BAR" :package "TEST")))
    (is (eq :function (amoebum.observability:symbol-entry-kind entry)))
    (is (= 0 (amoebum.observability:symbol-entry-line entry)))
    (is (string= "" (amoebum.observability:symbol-entry-signature entry)))))
