(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; ASDF Extension Contract Smoke Tests (I100)
;;; ---------------------------------------------------------------------------

(def-suite asdf-extensions-suite :in amoebum-suite
  :description "ASDF extension contract smoke tests.")

(in-suite asdf-extensions-suite)

(defmacro with-clean-asdf-registry (&body body)
  `(let ((amoebum::*asdf-extension-registry* (make-hash-table :test #'equal)))
     ,@body))

(test asdf-extension-struct
  (let ((ext (amoebum::make-asdf-extension
              :system-name "test-ext"
              :description "A test extension"
              :version "1.0")))
    (is (amoebum::asdf-extension-p ext))
    (is (string= "test-ext" (amoebum::asdf-extension-system-name ext)))
    (is (eq :discovered (amoebum::asdf-extension-status ext)))))

(test asdf-extension-list-empty
  (with-clean-asdf-registry
    (is (null (amoebum::list-asdf-extensions)))))

(test asdf-extension-registry-operations
  (with-clean-asdf-registry
    (let ((ext (amoebum::make-asdf-extension :system-name "my-ext")))
      (setf (gethash "my-ext" amoebum::*asdf-extension-registry*) ext)
      (is (= 1 (length (amoebum::list-asdf-extensions))))
      (is (eq ext (amoebum::find-asdf-extension "my-ext")))
      (is (null (amoebum::find-asdf-extension "nonexistent"))))))

(test asdf-extension-filter-by-status
  (with-clean-asdf-registry
    (setf (gethash "loaded-ext" amoebum::*asdf-extension-registry*)
          (amoebum::make-asdf-extension :system-name "loaded-ext" :status :loaded))
    (setf (gethash "disc-ext" amoebum::*asdf-extension-registry*)
          (amoebum::make-asdf-extension :system-name "disc-ext" :status :discovered))
    (is (= 1 (length (amoebum::list-asdf-extensions :status :loaded))))
    (is (= 1 (length (amoebum::list-asdf-extensions :status :discovered))))
    (is (= 2 (length (amoebum::list-asdf-extensions))))))

(test asdf-extension-clear
  (with-clean-asdf-registry
    (setf (gethash "ext" amoebum::*asdf-extension-registry*)
          (amoebum::make-asdf-extension :system-name "ext"))
    (amoebum::clear-asdf-extensions)
    (is (null (amoebum::list-asdf-extensions)))))

(test asdf-extension-unload
  (with-clean-asdf-registry
    (let ((ext (amoebum::make-asdf-extension :system-name "my-ext" :status :loaded)))
      (setf (gethash "my-ext" amoebum::*asdf-extension-registry*) ext)
      (amoebum::unload-asdf-extension "my-ext")
      (is (eq :unloaded (amoebum::asdf-extension-status ext))))))

(test asdf-extension-manifest-path
  (let ((amoebum::*asdf-extension-manifest-path* nil))
    (let ((path (amoebum::%asdf-extension-manifest-path :project-root "/tmp/proj/")))
      (is (pathnamep path))
      (is (search "manifest.sexp" (namestring path))))))

(test asdf-extension-manifest-override
  (let ((amoebum::*asdf-extension-manifest-path* "/tmp/custom-manifest.sexp"))
    (let ((path (amoebum::%asdf-extension-manifest-path)))
      (is (search "custom-manifest" (namestring path))))))

(test asdf-extension-discover-nonexistent
  "Discovering from nonexistent paths should not error."
  (with-clean-asdf-registry
    (let ((amoebum::*asdf-extension-search-paths*
            '("/tmp/nonexistent-asdf-search-12345/")))
      (let ((found (amoebum::discover-asdf-extensions)))
        (is (listp found))))))

(test asdf-extension-manifest-roundtrip
  "Save and load manifest."
  (with-clean-asdf-registry
    (let ((manifest-path (format nil "/tmp/amoebum-test-manifest-~A.sexp"
                                 (get-universal-time))))
      (unwind-protect
           (let ((amoebum::*asdf-extension-manifest-path* manifest-path))
             (setf (gethash "test-sys" amoebum::*asdf-extension-registry*)
                   (amoebum::make-asdf-extension
                    :system-name "test-sys"
                    :description "Test system"
                    :version "1.0"))
             (amoebum::save-asdf-extension-manifest)
             ;; Clear and reload
             (amoebum::clear-asdf-extensions)
             (is (null (amoebum::list-asdf-extensions)))
             (amoebum::load-asdf-extension-manifest)
             (is (= 1 (length (amoebum::list-asdf-extensions))))
             (let ((ext (amoebum::find-asdf-extension "test-sys")))
               (is (string= "Test system" (amoebum::asdf-extension-description ext)))))
        (ignore-errors (delete-file manifest-path))))))

(test asdf-extension-search-paths-default
  (is (listp amoebum::*asdf-extension-search-paths*))
  (is (>= (length amoebum::*asdf-extension-search-paths*) 1)))

(test discover-asdf-systems-in-nonexistent-dir
  (is (null (amoebum::%discover-asdf-systems-in-directory
             "/tmp/nonexistent-dir-12345/"))))

(test asdf-extension-load-known-system
  "Loading a known ASDF system like 'fiveam' should succeed."
  (with-clean-asdf-registry
    (let ((ext (amoebum::load-asdf-extension "fiveam")))
      (is (amoebum::asdf-extension-p ext))
      (is (eq :loaded (amoebum::asdf-extension-status ext))))))
