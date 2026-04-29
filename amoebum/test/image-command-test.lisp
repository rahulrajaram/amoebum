(in-package :amoebum/test)

(def-suite image-command-suite
  :in amoebum-suite
  :description "NXT-582: /save-image, /load-image, /list-images slash-commands.")

(in-suite image-command-suite)

(defun %image-args (&key name)
  (let ((table (make-hash-table :test #'equal)))
    (when name
      (setf (gethash :NAME table) name))
    table))

(defun %image-invocation (input)
  (amoebum::make-slash-command-invocation
   :input input
   :name (amoebum::%normalize-command-name input)
   :arguments-text ""
   :argument-tokens '()))

(defun %image-empty-context ()
  (amoebum::make-slash-command-context
   :config nil
   :memory-backend nil
   :chat-state nil))

(defun %touch-core-file (dir name)
  (let ((path (merge-pathnames (pathname (format nil "~A.core" name)) dir)))
    (ensure-directories-exist path)
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
      (write-sequence #(0 0 0 0) stream))
    path))

(test save-load-list-image-commands-are-registered
  (is-true (amoebum::find-slash-command "save-image"))
  (is-true (amoebum::find-slash-command "load-image"))
  (is-true (amoebum::find-slash-command "list-images")))

(test list-images-handles-empty-directory
  (let* ((tmp-root (%make-temp-directory "image-cmd-empty"))
         (saved-override amoebum::*image-directory-override*))
    (unwind-protect
         (progn
           (setf amoebum::*image-directory-override* tmp-root)
           (let* ((command (amoebum::find-slash-command "list-images"))
                  (result (funcall (amoebum::slash-command-handler command)
                                   (%image-invocation "/list-images")
                                   (%image-args)
                                   (%image-empty-context))))
             (is-true (amoebum::slash-command-result-p result))
             (let ((output (amoebum::slash-command-result-output result)))
               (is (search "No images found" output)))))
      (setf amoebum::*image-directory-override* saved-override)
      (%delete-directory-tree-safe tmp-root))))

(test list-images-finds-saved-cores
  (let* ((tmp-root (%make-temp-directory "image-cmd-list"))
         (saved-override amoebum::*image-directory-override*))
    (unwind-protect
         (progn
           (setf amoebum::*image-directory-override* tmp-root)
           (%touch-core-file tmp-root "alpha-image")
           (%touch-core-file tmp-root "beta-image")
           (let* ((command (amoebum::find-slash-command "list-images"))
                  (result (funcall (amoebum::slash-command-handler command)
                                   (%image-invocation "/list-images")
                                   (%image-args)
                                   (%image-empty-context))))
             (is-true (amoebum::slash-command-result-p result))
             (let ((output (amoebum::slash-command-result-output result)))
               (is (search "alpha-image" output))
               (is (search "beta-image" output)))))
      (setf amoebum::*image-directory-override* saved-override)
      (%delete-directory-tree-safe tmp-root))))

(test save-image-resolves-default-path-into-override-directory
  ;; Bypass the actual SAVE-LISP-AND-DIE call (which would terminate the
  ;; test process) by exercising %image-resolve-save-path directly.
  (let* ((tmp-root (%make-temp-directory "image-cmd-save"))
         (saved-override amoebum::*image-directory-override*))
    (unwind-protect
         (progn
           (setf amoebum::*image-directory-override* tmp-root)
           (multiple-value-bind (path resolved-name)
               (amoebum::%image-resolve-save-path "" (%image-empty-context))
             (is (stringp resolved-name))
             (is (plusp (length resolved-name)))
             (is (search (namestring tmp-root) (namestring path)))
             (is (search ".core" (namestring path))))
           ;; Explicit name path resolves under override too.
           (multiple-value-bind (path resolved-name)
               (amoebum::%image-resolve-save-path "named-image" (%image-empty-context))
             (is (string= "named-image" resolved-name))
             (is (search "named-image.core" (namestring path)))
             (is (search (namestring tmp-root) (namestring path)))))
      (setf amoebum::*image-directory-override* saved-override)
      (%delete-directory-tree-safe tmp-root))))

(test load-image-prints-launch-command-for-existing-core
  (let* ((tmp-root (%make-temp-directory "image-cmd-load"))
         (saved-override amoebum::*image-directory-override*)
         (core-path nil))
    (unwind-protect
         (progn
           (setf amoebum::*image-directory-override* tmp-root)
           (setf core-path (%touch-core-file tmp-root "ready-image"))
           (let* ((command (amoebum::find-slash-command "load-image"))
                  (result (funcall (amoebum::slash-command-handler command)
                                   (%image-invocation "/load-image ready-image")
                                   (%image-args :name "ready-image")
                                   (%image-empty-context))))
             (is-true (amoebum::slash-command-result-p result))
             (let ((output (amoebum::slash-command-result-output result)))
               (is (search "sbcl --core" output))
               (is (search (namestring core-path) output)))))
      (setf amoebum::*image-directory-override* saved-override)
      (%delete-directory-tree-safe tmp-root))))

(test load-image-reports-missing-target
  (let* ((tmp-root (%make-temp-directory "image-cmd-missing"))
         (saved-override amoebum::*image-directory-override*))
    (unwind-protect
         (progn
           (setf amoebum::*image-directory-override* tmp-root)
           (let* ((command (amoebum::find-slash-command "load-image"))
                  (result (funcall (amoebum::slash-command-handler command)
                                   (%image-invocation "/load-image nope")
                                   (%image-args :name "nope")
                                   (%image-empty-context))))
             (is-true (amoebum::slash-command-result-p result))
             (let ((output (amoebum::slash-command-result-output result)))
               (is (search "No saved image" output)))))
      (setf amoebum::*image-directory-override* saved-override)
      (%delete-directory-tree-safe tmp-root))))
