(in-package :amoebum)

;;; NXT-582: /save-image, /load-image, /list-images slash-commands.
;;;
;;; These wrap the existing image-snapshot infrastructure in
;;; src/checkpoint/image.lisp. The save path delegates to
;;; SAVE-AMOEBUM-IMAGE, which calls SB-EXT:SAVE-LISP-AND-DIE — that call
;;; **terminates the SBCL process** once the .core is written. The
;;; operator must then re-launch the saved image with `sbcl --core <path>`
;;; (or invoke the .core file directly, since :executable t is passed).
;;;
;;; /load-image therefore does NOT restore in-process — it computes and
;;; prints the launch command for the chosen .core. This matches how the
;;; SBCL image runtime actually works.

(defun %image-resolve-name (raw)
  "Return a non-blank image name, defaulting to a fresh checkpoint id."
  (let ((trimmed (%slash-trim raw)))
    (if (zerop (length trimmed))
        (%checkpoint-id-from-time)
        trimmed)))

(defun %image-command-project-root (context)
  (let ((cfg (or (and context (slash-command-context-config context))
                 (%current-config-safe))))
    (and (config-p cfg)
         (config-project-root cfg))))

(defun %image-command-config (context)
  (or (and context (slash-command-context-config context))
      (%current-config-safe)))

(defun %image-resolve-save-path (raw context)
  "Compute the .core path /save-image would write."
  (let ((name (%image-resolve-name raw)))
    (values
     (%image-path name
                  :project-root (%image-command-project-root context)
                  :config (%image-command-config context))
     name)))

(defun %image-resolve-load-path (raw context)
  "Resolve a load target either as a bare name or a literal path."
  (let* ((trimmed (%slash-trim raw)))
    (when (plusp (length trimmed))
      (let ((as-path (probe-file trimmed)))
        (or as-path
            (let ((built (%image-path trimmed
                                      :project-root (%image-command-project-root context)
                                      :config (%image-command-config context))))
              (probe-file built)))))))

(defun %save-image-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((raw (or (gethash :NAME arguments) ""))
         (path (multiple-value-bind (p _name)
                   (%image-resolve-save-path raw context)
                 (declare (ignore _name))
                 p)))
    (handler-case
        (progn
          ;; SAVE-AMOEBUM-IMAGE invokes SB-EXT:SAVE-LISP-AND-DIE which
          ;; terminates the process. The output below is best-effort: it
          ;; will only render if the underlying save returns (e.g. on
          ;; non-SBCL where it raises).
          (save-amoebum-image
           :path path
           :project-root (%image-command-project-root context)
           :config (%image-command-config context))
          (make-slash-command-result
           :output (format nil "Image saved to ~A (process will exit)."
                           (namestring path))))
      (error (condition)
        (make-slash-command-result
         :output (format nil "Image save failed: ~A" condition))))))

(defun %load-image-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((raw (or (gethash :NAME arguments) ""))
         (trimmed (%slash-trim raw)))
    (cond
      ((zerop (length trimmed))
       (make-slash-command-result
        :output (format nil "Usage: /load-image <name|path>~%~A"
                        (with-output-to-string (out)
                          (dolist (entry (list-saved-images
                                          :project-root (%image-command-project-root context)
                                          :config (%image-command-config context)))
                            (format out "  ~A~%" (car entry)))))))
      (t
       (let ((path (%image-resolve-load-path raw context)))
         (if (null path)
             (make-slash-command-result
              :output (format nil "No saved image found for ~S." trimmed))
             (make-slash-command-result
              :output
              (format nil
                      "Image ~A is a saved SBCL core. To restore, exit this~%~
                       process and launch the image directly:~%~
                       ~%  sbcl --core ~A~%~%~
                       (the core was written with :executable t, so you can~%~
                       also run it directly: ~A)"
                      (pathname-name path)
                      (namestring path)
                      (namestring path)))))))))

(defun %format-image-size (bytes)
  (cond
    ((null bytes) "?")
    ((< bytes 1024) (format nil "~DB" bytes))
    ((< bytes (* 1024 1024)) (format nil "~,1FK" (/ bytes 1024.0)))
    ((< bytes (* 1024 1024 1024)) (format nil "~,1FM" (/ bytes (* 1024.0 1024))))
    (t (format nil "~,2FG" (/ bytes (* 1024.0 1024 1024))))))

(defun %format-image-timestamp (universal-time)
  (multiple-value-bind (s mi h d mo y)
      (decode-universal-time (or universal-time 0))
    (declare (ignore s))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D" y mo d h mi)))

(defun %image-file-byte-size (path)
  (handler-case
      (with-open-file (stream path
                              :direction :input
                              :element-type '(unsigned-byte 8))
        (file-length stream))
    (error () nil)))

(defun %list-images-handler (_invocation _arguments context)
  (declare (ignore _invocation _arguments))
  (let* ((dir (image-directory
               :project-root (%image-command-project-root context)
               :config (%image-command-config context)))
         (entries (list-saved-images
                   :project-root (%image-command-project-root context)
                   :config (%image-command-config context))))
    (if (null entries)
        (make-slash-command-result
         :output (format nil "No images found in ~A." (namestring dir)))
        (make-slash-command-result
         :output
         (with-output-to-string (out)
           (format out "Saved images in ~A:~%" (namestring dir))
           (dolist (entry entries)
             (let* ((name (car entry))
                    (path (cdr entry))
                    (size (%image-file-byte-size path))
                    (ts (file-write-date path)))
               (format out "  ~A  ~A  ~A~%"
                       (%format-image-timestamp ts)
                       (%format-image-size size)
                       name))))))))

(defun register-image-slash-commands ()
  (register-slash-command
   (make-slash-command
    :name "save-image"
    :description "Save the current SBCL image to ~/.amoebum/images/<name>.core (terminates the process)."
    :usage "/save-image [name]"
    :parameters
    (list (make-slash-command-parameter
           :name "name"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional image name; defaults to a timestamped id."))
    :handler #'%save-image-handler))
  (register-slash-command
   (make-slash-command
    :name "load-image"
    :description "Print the launch command to restore from a saved image."
    :usage "/load-image <name|path>"
    :parameters
    (list (make-slash-command-parameter
           :name "name"
           :type :string
           :required-p t
           :greedy-p t
           :description "Saved image name (under ~/.amoebum/images/) or absolute .core path."))
    :handler #'%load-image-handler))
  (register-slash-command
   (make-slash-command
    :name "list-images"
    :description "List saved SBCL core images with timestamp and size."
    :usage "/list-images"
    :parameters '()
    :handler #'%list-images-handler))
  t)
