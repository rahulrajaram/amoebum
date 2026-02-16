(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Codebase Indexer Smoke Tests (I96)
;;; ---------------------------------------------------------------------------

(def-suite indexer-suite :in amoebum-suite
  :description "Codebase indexer smoke tests.")

(in-suite indexer-suite)

(test symbol-entry-creation
  (let ((entry (amoebum:make-symbol-entry
                :name "FOO"
                :package "CL-USER"
                :kind :function
                :file "/tmp/test.lisp"
                :line 42
                :signature "(x y &key z)"
                :doc "A test function.")))
    (is (amoebum:symbol-entry-p entry))
    (is (string= "FOO" (amoebum:symbol-entry-name entry)))
    (is (string= "CL-USER" (amoebum:symbol-entry-package entry)))
    (is (eq :function (amoebum:symbol-entry-kind entry)))
    (is (= 42 (amoebum:symbol-entry-line entry)))))

(test codebase-index-creation
  (let ((idx (amoebum:make-codebase-index :project-root "/tmp/test-project/")))
    (is (amoebum:codebase-index-p idx))
    (is (null (amoebum:codebase-index-entries idx)))
    (is (integerp (amoebum:codebase-index-created-at idx)))))

(test index-cl-package
  (let ((idx (amoebum:make-codebase-index)))
    (amoebum:index-package-symbols idx :cl :external-only t)
    (is (> (length (amoebum:codebase-index-entries idx)) 100))
    ;; CONS should be in there
    (let ((cons-entries (amoebum:index-find-symbol idx "CONS" :package "COMMON-LISP")))
      (is (>= (length cons-entries) 1)))))

(test index-find-symbol-basic
  (let ((idx (amoebum:make-codebase-index)))
    (amoebum:index-package-symbols idx :cl :external-only t)
    (let ((results (amoebum:index-find-symbol idx "CAR")))
      (is (>= (length results) 1))
      (is (string= "CAR" (amoebum:symbol-entry-name (first results)))))))

(test index-find-symbol-by-kind
  (let ((idx (amoebum:make-codebase-index)))
    (amoebum:index-package-symbols idx :cl :external-only t)
    (let ((fns (amoebum:index-find-symbol idx "FORMAT" :kind :function)))
      (is (>= (length fns) 1)))))

(test generate-repo-map-basic
  (let ((idx (amoebum:make-codebase-index)))
    ;; Index a small package
    (amoebum:index-package-symbols idx :amoebum :external-only t)
    (let ((map (amoebum:generate-repo-map idx :max-tokens 1000)))
      (is (stringp map))
      (is (plusp (length map)))
      (is (search "AMOEBUM" map)))))

(test generate-repo-map-token-limit
  (let ((idx (amoebum:make-codebase-index)))
    (amoebum:index-package-symbols idx :cl :external-only t)
    (let ((small (amoebum:generate-repo-map idx :max-tokens 100))
          (large (amoebum:generate-repo-map idx :max-tokens 10000)))
      (is (< (length small) (length large))))))

(test index-statistics
  (let ((idx (amoebum:make-codebase-index)))
    (amoebum:index-package-symbols idx :cl :external-only t)
    (let ((stats (amoebum:index-statistics idx)))
      (is (listp stats))
      (is (> (getf stats :total-entries) 0))
      (is (listp (getf stats :kinds)))
      (is (listp (getf stats :packages))))))

(test index-incremental-file-tracking
  (let ((idx (amoebum:make-codebase-index)))
    ;; Record a mtime
    (setf (gethash "/tmp/test-file.lisp" (amoebum:codebase-index-file-mtimes idx))
          (get-universal-time))
    (is (= 1 (hash-table-count (amoebum:codebase-index-file-mtimes idx))))
    (is (= 1 (getf (amoebum:index-statistics idx) :files-tracked)))))

(test index-directory-nonexistent
  "Indexing a nonexistent directory should not error."
  (let ((idx (amoebum:make-codebase-index)))
    (let ((count (amoebum:index-directory idx "/tmp/nonexistent-amoebum-dir-12345/")))
      (is (= 0 count)))))

(test index-package-symbols-pseudopod
  "Index the pseudopod package."
  (let ((idx (amoebum:make-codebase-index)))
    (amoebum:index-package-symbols idx :pseudopod :external-only t)
    (is (> (length (amoebum:codebase-index-entries idx)) 10))))

(test index-find-by-file-empty
  (let ((idx (amoebum:make-codebase-index)))
    (is (null (amoebum:index-find-by-file idx "/nonexistent/file.lisp")))))

(test symbol-entry-defaults
  (let ((entry (amoebum:make-symbol-entry :name "BAR" :package "TEST")))
    (is (eq :function (amoebum:symbol-entry-kind entry)))
    (is (= 0 (amoebum:symbol-entry-line entry)))
    (is (string= "" (amoebum:symbol-entry-signature entry)))))
