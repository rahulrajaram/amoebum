(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Image Save/Restore Smoke Tests (I99)
;;; ---------------------------------------------------------------------------

(def-suite image-suite :in amoebum-suite
  :description "Image save/restore smoke tests.")

(in-suite image-suite)

(test image-directory-default
  (let ((amoebum::*image-directory-override* nil))
    (let ((dir (amoebum::image-directory :project-root "/tmp/test-proj/")))
      (is (pathnamep dir))
      (is (search ".amoebum/images" (namestring dir))))))

(test image-directory-override
  (let ((amoebum::*image-directory-override* "/tmp/custom-images/"))
    (let ((dir (amoebum::image-directory)))
      (is (search "custom-images" (namestring dir))))))

(test image-path-generation
  (let ((amoebum::*image-directory-override* "/tmp/test-images/"))
    (let ((path (amoebum::%image-path "test-image")))
      (is (pathnamep path))
      (is (search "test-image.core" (namestring path))))))

(test list-saved-images-empty
  (let ((amoebum::*image-directory-override* "/tmp/nonexistent-images-12345/"))
    (is (null (amoebum::list-saved-images)))))

(test rotate-images-no-crash
  (let ((amoebum::*image-directory-override* "/tmp/nonexistent-images-12345/"))
    (is (= 0 (amoebum::rotate-images)))))

(test image-max-count-default
  (is (= 5 amoebum::*image-max-count*)))

(test image-pre-save-hooks
  (let ((amoebum::*image-pre-save-hooks* '())
        (called nil))
    (amoebum::register-image-pre-save-hook (lambda () (setf called t)))
    (is (= 1 (length amoebum::*image-pre-save-hooks*)))
    (amoebum::%image-pre-save-cleanup)
    (is (eq t called))))

(test image-post-restore-hooks
  (let ((amoebum::*image-post-restore-hooks* '())
        (called nil))
    (amoebum::register-image-post-restore-hook (lambda () (setf called t)))
    (is (= 1 (length amoebum::*image-post-restore-hooks*)))
    (amoebum::%image-post-restore-init)
    (is (eq t called))))

(test image-pre-save-hooks-error-resilient
  "Pre-save hooks should not crash even if a hook errors."
  (let ((amoebum::*image-pre-save-hooks* '())
        (second-called nil))
    (amoebum::register-image-post-restore-hook (lambda () (setf second-called t)))
    (amoebum::register-image-pre-save-hook (lambda () (error "hook failure")))
    ;; Should not error
    (amoebum::%image-pre-save-cleanup)
    (is t)))

(test image-hook-deduplication
  (let ((amoebum::*image-pre-save-hooks* '()))
    (let ((fn (lambda () nil)))
      (amoebum::register-image-pre-save-hook fn)
      (amoebum::register-image-pre-save-hook fn)
      (is (= 1 (length amoebum::*image-pre-save-hooks*))))))

(test rotate-images-respects-max-count
  "rotate-images with empty dir should return 0 deleted."
  (let ((amoebum::*image-directory-override* "/tmp/nonexistent-rotate-test/"))
    (is (= 0 (amoebum::rotate-images :max-count 3)))))

(test image-files-empty-dir
  (let ((amoebum::*image-directory-override* "/tmp/nonexistent-image-list/"))
    (is (null (amoebum::%image-files)))))

(test image-checkpoint-id
  "Checkpoint IDs should be timestamp-like strings."
  (let ((id (amoebum::%checkpoint-id-from-time)))
    (is (stringp id))
    (is (> (length id) 10))
    (is (search "T" id))))
