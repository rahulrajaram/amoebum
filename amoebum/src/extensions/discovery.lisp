(in-package :amoebum)

;;;; Extension discovery — file enumeration, path/key normalization, target
;;;; matching, and the public DISCOVER-USER-EXTENSION-FILES entry point.
;;;;
;;;; Extracted from extensions/loader.lisp under NXT-386 to separate read-only
;;;; filesystem traversal from runtime load/register orchestration. This module
;;;; depends only on uiop and the legacy extensions.lisp helpers (which it
;;;; redefines authoritatively here as well).
;;;;
;;;; Other extension submodules (manifest, permissions-prep, loader) build on
;;;; the helpers exposed here. No cross-module dependencies in the reverse
;;;; direction.

;; Test and smoke harnesses can bind these to avoid mutating real user paths.
;; Re-declared here so the override hooks live in the same module that owns
;; directory resolution. extensions.lisp already declares them too; the
;; defparameter form is idempotent (preserves the existing value).
(defparameter *extensions-global-directory-override* nil)
(defparameter *extensions-project-directory-override* nil)

(defun %extension-trim (value)
  (if (stringp value)
      (string-trim '(#\Space #\Tab #\Newline #\Return) value)
      ""))

(defun %normalize-pathname (value)
  (cond
    ((pathnamep value) value)
    ((stringp value) (pathname value))
    (t nil)))

(defun %ensure-directory (value)
  (let ((pathname (%normalize-pathname value)))
    (and pathname
         (uiop:ensure-directory-pathname pathname))))

(defun %ensure-string (value)
  (cond
    ((null value) nil)
    ((stringp value) value)
    ((symbolp value) (string-downcase (symbol-name value)))
    (t (princ-to-string value))))

(defun %parse-nonnegative-integer (value)
  (cond
    ((and (integerp value) (>= value 0))
     value)
    ((stringp value)
     (let* ((trimmed (%extension-trim value))
            (parsed (and (plusp (length trimmed))
                         (ignore-errors (parse-integer trimmed)))))
       (and (integerp parsed)
            (>= parsed 0)
            parsed)))
    (t nil)))

(defun %resolve-project-root (&optional project-root)
  (let* ((cfg (ignore-errors (current-config)))
         (candidate (or project-root
                        (and (config-p cfg) (config-project-root cfg))
                        (ignore-errors (uiop:getcwd))
                        *default-pathname-defaults*))
         (directory (%ensure-directory candidate)))
    (or (and directory
             (or (ignore-errors (uiop:ensure-directory-pathname (truename directory)))
                 directory))
        (uiop:ensure-directory-pathname *default-pathname-defaults*))))

(defun %global-extension-directory (&key global-directory)
  (or (%ensure-directory global-directory)
      (%ensure-directory *extensions-global-directory-override*)
      (uiop:ensure-directory-pathname
       (merge-pathnames #P".amoebum/extensions/" (user-homedir-pathname)))))

(defun %project-extension-directory (&key project-root project-directory)
  (or (%ensure-directory project-directory)
      (%ensure-directory *extensions-project-directory-override*)
      (uiop:ensure-directory-pathname
       (merge-pathnames #P".amoebum/extensions/"
                        (%resolve-project-root project-root)))))

(defun %extension-sort-key (path)
  (string-downcase
   (or (file-namestring path)
       (namestring path))))

(defun %canonical-extension-path (path)
  (let* ((pathname (%normalize-pathname path))
         (resolved (and pathname
                        (or (ignore-errors (truename pathname))
                            pathname))))
    (if resolved
        (namestring resolved)
        "")))

(defun %extension-key (path)
  (string-downcase (%canonical-extension-path path)))

(defun %extension-registry-key (name)
  (string-downcase (%extension-trim name)))

(defun %extension-match-target-p (target path-text)
  (let* ((needle (string-downcase (%extension-trim target)))
         (haystack (string-downcase path-text))
         (pathname (%normalize-pathname path-text))
         (filename (and pathname (file-namestring pathname)))
         (stem (and pathname (pathname-name pathname))))
    (and (plusp (length needle))
         (or (string= needle haystack)
             (and filename (string= needle (string-downcase filename)))
             (and stem (string= needle (string-downcase stem)))
             (search needle haystack :test #'char=)))))

(defun %safe-file-write-date (path)
  (ignore-errors
    (file-write-date path)))

(defun %manifest-parent-directory (manifest-path)
  (uiop:pathname-directory-pathname manifest-path))

(defun %list-extension-manifest-files (directory)
  (if (and directory (probe-file directory))
      (let ((files '()))
        (dolist (candidate (directory (merge-pathnames #P"*/extension.lisp" directory)))
          (unless (uiop:directory-pathname-p candidate)
            (push candidate files)))
        (sort files #'string< :key #'%extension-sort-key))
      '()))

(defun %list-legacy-extension-files (directory)
  (if (and directory (probe-file directory))
      (sort
       (remove-if
        (lambda (path)
          (or (uiop:directory-pathname-p path)
              (string-equal (pathname-name path) "extension")))
        (directory (merge-pathnames #P"*.lisp" directory)))
       #'string<
       :key #'%extension-sort-key)
      '()))

(defun discover-user-extension-files (&key project-root global-directory project-directory)
  (let* ((global-path (%global-extension-directory :global-directory global-directory))
         (project-path (%project-extension-directory :project-root project-root
                                                     :project-directory project-directory))
         (global-files (append (%list-extension-manifest-files global-path)
                               (%list-legacy-extension-files global-path)))
         (project-files (append (%list-extension-manifest-files project-path)
                                (%list-legacy-extension-files project-path))))
    (values global-files project-files)))
