(defpackage :ptui.test.search
  (:use :cl :fiveam)
  (:export #:run-all #:ptui-search-suite))

(in-package :ptui.test.search)

(def-suite ptui-search-suite
  :description "PTUI search subsystem tests (glob matcher + search engine).")

(in-suite ptui-search-suite)

(defun %make-temp-directory (prefix)
  (let* ((stamp (format nil "~A-~D-~D"
                        prefix
                        (get-universal-time)
                        (random 1000000)))
         (dir (uiop:ensure-directory-pathname
               (merge-pathnames (concatenate 'string stamp "/")
                               (uiop:temporary-directory)))))
    (uiop:ensure-all-directories-exist (list dir))
    dir))

(defun %with-temp-directory (prefix thunk)
  (let ((root (%make-temp-directory prefix)))
    (unwind-protect
         (funcall thunk root)
      (ignore-errors (uiop:delete-directory-tree root
                                                  :validate t
                                                  :if-does-not-exist :ignore)))))

(defun %write-text-file (path content)
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string content stream))
  path)

(defun %scan-results-relative-paths (result)
  (mapcar #'ptui.search.glob:glob-entry-relative-path
          (ptui.search.glob:glob-scan-result-matches result)))

(defun %make-search-documents ()
  (list
   (ptui.search.engine:make-search-document
    :path "notes.md"
    :content "Line one\nHello world\nFinal line")
   (ptui.search.engine:make-search-document
    :path "guide.txt"
    :content "No match here")))

;;; --- ptui.search.glob tests ---

(test compile-glob-matcher-accepts-brace-expansion
  (let ((matcher (ptui.search.glob:compile-glob-matcher "{foo,bar}.lisp")))
    (is (member "foo.lisp" (ptui.search.glob:glob-matcher-expanded-patterns matcher)
                :test #'string=))
    (is (member "bar.lisp" (ptui.search.glob:glob-matcher-expanded-patterns matcher)
                :test #'string=))
    (is (eql (length (ptui.search.glob:glob-matcher-expanded-patterns matcher)) 2))
    (is (ptui.search.glob:glob-matcher-match-p matcher "foo.lisp"))
    (is (ptui.search.glob:glob-matcher-match-p matcher "bar.lisp"))
    (is (not (ptui.search.glob:glob-matcher-match-p matcher "baz.lisp")))))

(test glob-match-basic-patterns
  (is (ptui.search.glob:glob-match-p "*" "anything"))
  (is (ptui.search.glob:glob-match-p "*.lisp" "main.lisp"))
  (is (ptui.search.glob:glob-match-p "src/**/*.lisp" "src/main.lisp"))
  (is (ptui.search.glob:glob-match-p "src/**/*.lisp" "src/a/b/c/main.lisp"))
  (is (not (ptui.search.glob:glob-match-p "*.lisp" "main.txt")))
  (is (not (ptui.search.glob:glob-match-p "src/**/*.lisp" "test/main.lisp"))))

(test glob-match-character-classes
  (is (ptui.search.glob:glob-match-p "[abc].txt" "a.txt"))
  (is (not (ptui.search.glob:glob-match-p "[abc].txt" "d.txt"))))

(test glob-match-negated-rule-parsing
  (%with-temp-directory "ptui-search-gitignore"
    (lambda (root)
      (let ((path (merge-pathnames ".gitignore" root)))
        (ensure-directories-exist path)
        (with-open-file (stream path :direction :output :if-exists :supersede
                               :if-does-not-exist :create)
          (write-line "*.tmp" stream)
          (write-line "!important.tmp" stream))
        (let ((rules (ptui.search.glob:read-gitignore-rules root)))
          (is (= 2 (length rules)))
          (is (not (ptui.search.glob:glob-ignore-rule-negated-p (first rules))))
          (is (ptui.search.glob:glob-ignore-rule-negated-p (second rules)))
          (is (member "*.tmp" (ptui.search.glob:glob-ignore-rule-patterns (first rules))
                     :test #'string=))
          (is (member "important.tmp" (ptui.search.glob:glob-ignore-rule-patterns (second rules))
                     :test #'string=)))))))

(test read-gitignore-rules-and-scan-ignore-negation
  (%with-temp-directory
   "ptui-search-ignore"
   (lambda (root)
   (let ((gitignore (merge-pathnames ".gitignore" root))
           (path-a (merge-pathnames "a.tmp" root))
           (path-b (merge-pathnames "important.tmp" root))
           (path-c (merge-pathnames "notes.txt" root)))
       (with-open-file (stream gitignore :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
         (write-line "# comments ignored" stream)
         (write-line "" stream)
         (write-line "*.tmp" stream)
         (write-line "!important.tmp" stream))
       (%write-text-file path-a "tmp A")
       (%write-text-file path-b "tmp B")
       (%write-text-file path-c "note")
       (let* ((result (ptui.search.glob:scan-glob-files "*" :root root
                                                      :limit 20
                                                      :respect-gitignore t
                                                      :case-sensitive t))
              (rules (ptui.search.glob:read-gitignore-rules root)))
         (is (= 2 (length rules)))
         (let ((first-rule (first rules))
               (second-rule (second rules)))
           (is (not (ptui.search.glob:glob-ignore-rule-negated-p first-rule)))
           (is (ptui.search.glob:glob-ignore-rule-negated-p second-rule)))
             (let ((paths (%scan-results-relative-paths result)))
               (is (member "notes.txt" paths :test #'string=))
               (is (member "important.tmp" paths :test #'string=))
               (is (member ".gitignore" paths :test #'string=))
               (is (not (member "a.tmp" paths :test #'string=)))
               (is (listp paths))))))))

(test glob-match-case-sensitivity
  (is (not (ptui.search.glob:glob-match-p "FOO.TXT" "foo.txt")))
  (is (ptui.search.glob:glob-match-p "FOO.TXT" "foo.txt" :case-sensitive nil))
  (let ((matcher (ptui.search.glob:compile-glob-matcher "Foo.TXT" :case-sensitive nil)))
    (is (not (ptui.search.glob:glob-matcher-case-sensitive-p matcher)))
    (is (ptui.search.glob:glob-matcher-match-p matcher "foo.txt"))))

;;; --- ptui.search.engine tests ---

(test engine-rank-file-matches-prefix-over-substring
  (let ((results (ptui.search.engine:rank-file-matches
                  "foo"
                  '("afoo.txt" "foo.txt" "foobar.txt"))))
    (is (= 3 (length results)))
    (is (string= "foo.txt" (ptui.search.engine:search-file-match-path (first results))))
    (is (string= "foobar.txt" (ptui.search.engine:search-file-match-path (second results))))
    (is (string= "afoo.txt" (ptui.search.engine:search-file-match-path (third results))))
    (is (> (ptui.search.engine:search-file-match-score (second results))
           (ptui.search.engine:search-file-match-score (third results))))))

(test engine-rank-file-matches-fuzzy-spans
  (let ((matches (ptui.search.engine:rank-file-matches "fbar" '("foobar" "fubar" "barf"))))
    (let ((top (first matches)))
      (is (string= "fubar" (ptui.search.engine:search-file-match-path top)))
      (is (eql :fuzzy (ptui.search.engine:search-file-match-kind top)))
      (is (equal (ptui.search.engine:search-file-match-spans top)
                 '((0 . 1) (2 . 5)))))))

(test engine-rank-file-matches-empty-query
  (let ((results (ptui.search.engine:rank-file-matches "" '("foo" "bar"))))
    (is (= 2 (length results)))
    (is (> (ptui.search.engine:search-file-match-score (first results)) 0))
    (is (> (ptui.search.engine:search-file-match-score (second results)) 0))))

(test search-document-struct-constructor
  (let ((doc (ptui.search.engine:make-search-document :path "notes.md" :content "Hello")))
    (is (ptui.search.engine:search-document-p doc))
    (is (string= "notes.md" (ptui.search.engine:search-document-path doc)))
    (is (string= "Hello" (ptui.search.engine:search-document-content doc)))))

(test engine-rank-file-matches-sorted-by-score
  (let ((results (ptui.search.engine:rank-file-matches
                  "foo"
                  '("zfoo.txt" "foo.txt" "foobar.txt" "afoo.txt"))))
    (is (= 4 (length results)))
    (is (string= "foo.txt"
                 (ptui.search.engine:search-file-match-path (first results))))
    (is (string= "foobar.txt"
                 (ptui.search.engine:search-file-match-path (second results))))
    (is (string= "afoo.txt"
                 (ptui.search.engine:search-file-match-path (third results))))
    (is (>= (ptui.search.engine:search-file-match-score (first results))
            (ptui.search.engine:search-file-match-score (second results)))
        "expected non-increasing score order")))

(test search-document-content-search
  (let ((results (ptui.search.engine:search-content-matches "world" (%make-search-documents)
                                                           :limit 10
                                                           :regex-mode nil
                                                           :case-insensitive nil
                                                           :multiline-mode nil)))
    (is (= 1 (length results)))
    (let ((match (first results)))
      (is (string= "notes.md" (ptui.search.engine:search-content-match-path match)))
      (is (= 1 (ptui.search.engine:search-content-match-line match)))
      (is (= 16 (ptui.search.engine:search-content-match-column match)))
      (is (string= "world" (ptui.search.engine:search-content-match-matched-text match))))))

(test search-content-empty-query-returns-none
  (let ((results (ptui.search.engine:search-content-matches "" (%make-search-documents))))
    (is (null results))))

(test search-content-scan-progress-and-cancellation
  (let* ((documents
           (list
            (ptui.search.engine:make-search-document
             :path "a.txt"
             :content "needle a")
            (ptui.search.engine:make-search-document
             :path "b.txt"
             :content "needle b")
            (ptui.search.engine:make-search-document
             :path "c.txt"
             :content "needle c")))
         (progress-count 0)
         (stop-p nil)
         (result
           (ptui.search.engine:scan-content-matches
            "needle"
            documents
            :regex-mode nil
            :on-match (lambda (_match)
                        (declare (ignore _match))
                        (setf stop-p t))
            :on-progress (lambda (&key done &allow-other-keys)
                           (declare (ignore done))
                           (incf progress-count))
            :cancel-fn (lambda ()
                         stop-p))))
    (is (ptui.search.engine:search-content-scan-result-canceled-p result))
    (is (= 1 (ptui.search.engine:search-content-scan-result-match-count result)))
    (is (>= progress-count 2))))

(defun run-all ()
  (let ((results (run 'ptui-search-suite)))
    (fiveam:explain! results)
    (fiveam:results-status results)))
