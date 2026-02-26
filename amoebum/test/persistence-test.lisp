(in-package :amoebum/test)

;;; ============================================================
;;; I249: Image Persistence — Pre-save Cleanup / Post-restore Init
;;; ============================================================

(def-suite persistence-suite :in amoebum-suite)
(in-suite persistence-suite)

(test image-directory-resolvable
  "image-directory should return a pathname."
  (let ((dir (amoebum:image-directory)))
    (is (pathnamep dir))))

(test image-directory-uses-override
  "image-directory should use override when set."
  (let ((amoebum::*image-directory-override* "/tmp/test-images/"))
    (let ((dir (amoebum:image-directory)))
      (is (search "test-images" (namestring dir))))))

(test image-files-empty-on-fresh-dir
  "Image files should be empty for a fresh directory."
  (let ((amoebum::*image-directory-override*
          (merge-pathnames
           (make-pathname :directory '(:relative "amoebum-test-persist-empty"))
           (uiop:ensure-directory-pathname (uiop:temporary-directory)))))
    (is (null (amoebum::%image-files)))))

(test rotate-images-handles-empty-dir
  "rotate-images should not error on empty directory."
  (let ((amoebum::*image-directory-override*
          (merge-pathnames
           (make-pathname :directory '(:relative "amoebum-test-persist-rotate"))
           (uiop:ensure-directory-pathname (uiop:temporary-directory)))))
    (finishes (amoebum:rotate-images))))

(test list-saved-images-returns-alist
  "list-saved-images should return an alist of (name . path)."
  (let ((amoebum::*image-directory-override*
          (merge-pathnames
           (make-pathname :directory '(:relative "amoebum-test-persist-list"))
           (uiop:ensure-directory-pathname (uiop:temporary-directory)))))
    (let ((images (amoebum:list-saved-images)))
      (is (listp images)))))

(test image-pre-save-hooks-execute
  "Pre-save hooks should be called."
  (let ((amoebum::*image-pre-save-hooks* nil)
        (called nil))
    (amoebum:register-image-pre-save-hook
     (lambda () (setf called t)))
    (amoebum::%image-pre-save-cleanup)
    (is (not (null called)))))

(test image-post-restore-hooks-execute
  "Post-restore hooks should be called."
  (let ((amoebum::*image-post-restore-hooks* nil)
        (called nil))
    (amoebum:register-image-post-restore-hook
     (lambda () (setf called t)))
    (amoebum::%image-post-restore-init)
    (is (not (null called)))))

(test image-hooks-error-resilient
  "Hook errors should not propagate."
  (let ((amoebum::*image-pre-save-hooks*
          (list (lambda () (error "hook-fail")))))
    (finishes (amoebum::%image-pre-save-cleanup))))
