(in-package :amoebum/test)

(def-suite extension-manifest-suite
  :description "I233 extension manifest parsing and dependency resolution."
  :in amoebum-suite)

(in-suite extension-manifest-suite)

(defun %manifest (name version &key dependencies provides)
  (amoebum:make-extension-manifest
   :name name
   :version version
   :dependencies (or dependencies '())
   :provides (or provides '())
   :entry-point "main.lisp"))

(test parse-extension-manifest-from-sexp
  (let ((manifest
          (amoebum:parse-extension-manifest-sexp
           '(extension
             (:name "sample-ext"
              :version "1.2.3"
              :author "Amoebum"
              :description "Sample extension"
              :license "MIT"
              :dependencies (("base-ext" ">=1.0") ("ui-ext" "=2.0"))
              :provides (:tools :widgets)
              :entry-point "src/entry.lisp")))))
    (is (string= "sample-ext" (amoebum:extension-manifest-name manifest)))
    (is (string= "1.2.3" (amoebum:extension-manifest-version manifest)))
    (is (string= "Amoebum" (amoebum:extension-manifest-author manifest)))
    (is (equal '(("base-ext" . ">=1.0") ("ui-ext" . "=2.0"))
               (amoebum:extension-manifest-dependencies manifest)))
    (is (equal '(:tools :widgets)
               (amoebum:extension-manifest-provides manifest)))
    (is (string= "src/entry.lisp" (amoebum:extension-manifest-entry-point manifest)))))

(test version-constraint-checking
  (is-true (amoebum:version-satisfies-constraint-p "1.0" "=1.0"))
  (is-false (amoebum:version-satisfies-constraint-p "1.1" "=1.0"))
  (is-true (amoebum:version-satisfies-constraint-p "1.5" ">=1.0"))
  (is-false (amoebum:version-satisfies-constraint-p "0.9" ">=1.0"))
  (is-true (amoebum:version-satisfies-constraint-p "1.5" ">=1.0 <2.0"))
  (is-false (amoebum:version-satisfies-constraint-p "2.0" ">=1.0 <2.0")))

(test dependency-resolution-topological-order
  (let ((manifests (list (%manifest "core" "1.0")
                         (%manifest "hooks" "1.0" :dependencies '(("core" . ">=1.0")))
                         (%manifest "widgets" "1.0" :dependencies '(("core" . "=1.0")))
                         (%manifest "feature" "1.0" :dependencies '(("hooks" . ">=1.0")
                                                                      ("widgets" . ">=1.0"))))))
    (multiple-value-bind (ordered report)
        (amoebum:resolve-extension-manifests manifests)
      (declare (ignore report))
      (let ((names (mapcar #'amoebum:extension-manifest-name ordered)))
        (is (< (position "core" names :test #'string=)
               (position "hooks" names :test #'string=)))
        (is (< (position "core" names :test #'string=)
               (position "widgets" names :test #'string=)))
        (is (< (position "hooks" names :test #'string=)
               (position "feature" names :test #'string=)))
        (is (< (position "widgets" names :test #'string=)
               (position "feature" names :test #'string=)))))))

(test dependency-resolution-missing-dependency-reporting
  (let ((manifests (list (%manifest "feature" "1.0"
                                    :dependencies '(("missing-lib" . ">=1.0"))))))
    (signals error
      (amoebum:resolve-extension-manifests manifests))
    (multiple-value-bind (_ report)
        (amoebum:resolve-extension-manifests manifests :errorp nil)
      (declare (ignore _))
      (is (= 1 (length (getf report :missing))))
      (is (null (getf report :cycles))))))

(test dependency-resolution-cycle-detection
  (let ((manifests (list (%manifest "a" "1.0" :dependencies '(("b" . ">=1.0")))
                         (%manifest "b" "1.0" :dependencies '(("a" . ">=1.0"))))))
    (signals error
      (amoebum:resolve-extension-manifests manifests))
    (multiple-value-bind (_ report)
        (amoebum:resolve-extension-manifests manifests :errorp nil)
      (declare (ignore _))
      (is (= 2 (length (getf report :cycles))))
      (is (member "a" (getf report :cycles) :test #'string=))
      (is (member "b" (getf report :cycles) :test #'string=)))))

(test dependency-resolution-version-mismatch
  (let ((manifests (list (%manifest "core" "1.5")
                         (%manifest "feature" "1.0"
                                    :dependencies '(("core" . ">=2.0"))))))
    (signals error
      (amoebum:resolve-extension-manifests manifests))
    (multiple-value-bind (_ report)
        (amoebum:resolve-extension-manifests manifests :errorp nil)
      (declare (ignore _))
      (is (= 1 (length (getf report :version-mismatches)))))))

(test extension-manifest-smoke-sentinel
  (is-true t)
  (format t "EXTENSION_MANIFEST_SMOKE_OK~%"))
