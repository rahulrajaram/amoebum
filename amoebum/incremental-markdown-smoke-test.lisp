(let* ((smoke-file (or *load-truename* *compile-file-truename*))
       (amoebum-dir (and smoke-file (make-pathname :name nil :type nil :defaults smoke-file)))
       (repo-root (and amoebum-dir (truename (merge-pathnames #P"../" amoebum-dir)))))
  (unless repo-root
    (error "Unable to resolve repository root from ~S" smoke-file))

  (let* ((local-quicklisp (merge-pathnames #P"ptui/.tools/quicklisp/setup.lisp" repo-root))
         (fallback-root #P"/home/rahul/Documents/amoebum/")
         (fallback-quicklisp (merge-pathnames #P"ptui/.tools/quicklisp/setup.lisp" fallback-root))
         (quicklisp-setup
           (cond
             ((probe-file local-quicklisp) local-quicklisp)
             ((probe-file fallback-quicklisp) fallback-quicklisp)
             (t (error "Unable to locate quicklisp setup at ~A or ~A."
                       local-quicklisp
                       fallback-quicklisp)))))
    (load quicklisp-setup))
  (require :asdf)

  (let* ((asdf-pkg (find-package "ASDF"))
         (load-asd-fn (symbol-function (find-symbol "LOAD-ASD" asdf-pkg)))
         (load-system-fn (symbol-function (find-symbol "LOAD-SYSTEM" asdf-pkg))))
    (funcall load-asd-fn (merge-pathnames #P"pseudopod/pseudopod.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"sw4rm-sdk/sw4rm-sdk.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"ptui/ptui.asd" repo-root))
    (funcall load-system-fn "pseudopod")
    (funcall load-system-fn "sw4rm-sdk")
    (funcall load-system-fn "ptui")
    (load (merge-pathnames #P"amoebum/src/package.lisp" repo-root))
    (load (merge-pathnames #P"amoebum/src/system-prompt.lisp" repo-root))
    (load (merge-pathnames #P"amoebum/src/ui/streaming.lisp" repo-root)))

  (let* ((amoebum-pkg (or (find-package "AMOEBUM")
                          (error "Missing package AMOEBUM after loading sources.")))
         (symbol-in (lambda (name package)
                      (or (find-symbol name package)
                          (error "Missing symbol ~A in package ~A."
                                 name
                                 (package-name package)))))
         (fn-in (lambda (name package)
                  (symbol-function (funcall symbol-in name package))))
         (make-renderer-fn (funcall fn-in "MAKE-STREAMING-MARKDOWN-RENDERER" amoebum-pkg))
         (append-chunk-fn (funcall fn-in "STREAMING-MARKDOWN-RENDERER-APPEND-CHUNK" amoebum-pkg))
         (render-lines-fn (funcall fn-in "STREAMING-MARKDOWN-RENDERER-RENDER-LINES" amoebum-pkg)))
  (labels ((assert-true (condition format-string &rest format-args)
             (unless condition
               (error (apply #'format nil format-string format-args))))
           (styled-lines->text (styled-lines)
             (with-output-to-string (out)
               (loop for line in styled-lines
                     for line-index from 0 do
                       (when (> line-index 0)
                         (write-char #\Newline out))
                       (dolist (segment line)
                         (write-string (getf segment :text "") out)))))
           (any-segment-matches-p (styled-lines predicate)
             (loop for line in styled-lines thereis
                   (loop for segment in line thereis
                         (funcall predicate segment)))))
    (let ((renderer (funcall make-renderer-fn)))
      (funcall append-chunk-fn renderer "``")
      (let* ((styled-lines
               (funcall render-lines-fn renderer 50 :partialp t :cursor-visible-p nil))
             (plain-text (styled-lines->text styled-lines)))
        (assert-true (string= plain-text "")
                     "Expected incomplete fence marker to remain buffered, got ~S."
                     plain-text)
        (assert-true
         (not (any-segment-matches-p
               styled-lines
               (lambda (segment)
                 (eq (getf segment :role) :assistant-code-fence))))
         "Expected incomplete fence marker to stay buffered, got ~S."
         styled-lines))
      (funcall append-chunk-fn renderer (format nil "`~%"))
      (let ((fence-lines
              (funcall render-lines-fn renderer 50 :partialp t :cursor-visible-p nil)))
        (assert-true
         (any-segment-matches-p
          fence-lines
          (lambda (segment)
            (eq (getf segment :role) :assistant-code-fence)))
         "Expected completed code fence marker after third backtick."))
      (funcall append-chunk-fn renderer (format nil "```~%"))
      (funcall append-chunk-fn renderer (format nil "**b** *i* [lnk](https://example.com)~%"))
      (funcall append-chunk-fn renderer "- item")
      (let* ((styled-lines
               (funcall render-lines-fn renderer 50
                        :partialp t
                        :cursor-visible-p t
                        :cursor-glyph "|"))
             (plain-text (styled-lines->text styled-lines)))
        (assert-true (search "- item|" plain-text :test #'char=)
                     "Expected partial list item and cursor in output, got ~S."
                     plain-text)
        (assert-true
         (any-segment-matches-p
          styled-lines
          (lambda (segment)
            (and (search "b" (getf segment :text "") :test #'char=)
                 (getf segment :boldp))))
         "Expected bold segment in styled output.")
        (assert-true
         (any-segment-matches-p
          styled-lines
          (lambda (segment)
            (and (search "i" (getf segment :text "") :test #'char=)
                 (getf segment :italicp))))
         "Expected italic segment in styled output.")
        (assert-true
         (any-segment-matches-p
          styled-lines
          (lambda (segment)
            (and (search "lnk" (getf segment :text "") :test #'char=)
                 (getf segment :underlinep))))
         "Expected underlined link text segment in styled output.")))
    (format t "INCREMENTAL_MARKDOWN_SMOKE_OK~%"))))
