(in-package :amoebum/test)

(def-suite incremental-markdown-suite
  :description "I227 incremental markdown renderer behavior."
  :in amoebum-suite)

(in-suite incremental-markdown-suite)

(defun %seg-text (segment)
  "Return text from a compact or plist segment."
  (if (amoebum::compact-segment-p segment)
      (amoebum::compact-segment-text segment)
      (getf segment :text "")))

(defun %seg-field (segment field)
  "Return a style field from a compact or plist segment."
  (if (amoebum::compact-segment-p segment)
      (let ((entry (amoebum::lookup-style (amoebum::compact-segment-style-id segment))))
        (ecase field
          (:role (amoebum::style-entry-role entry))
          (:boldp (amoebum::style-entry-boldp entry))
          (:italicp (amoebum::style-entry-italicp entry))
          (:underlinep (amoebum::style-entry-underlinep entry))
          (:invertp (amoebum::style-entry-invertp entry))
          (:dimp (amoebum::style-entry-dimp entry))
          (:strikep (amoebum::style-entry-strikep entry))))
      (getf segment field)))

(defun %styled-lines->text (styled-lines)
  (with-output-to-string (out)
    (loop for line in styled-lines
          for line-index from 0 do
            (when (> line-index 0)
              (write-char #\Newline out))
            (dolist (segment line)
              (write-string (%seg-text segment) out)))))

(defun %styled-lines-any-segment-p (styled-lines predicate)
  (loop for line in styled-lines thereis
        (loop for segment in line thereis
              (funcall predicate segment))))

(test incremental-markdown-renderer-buffers-partials-and-renders-links
  (let ((renderer (amoebum:make-streaming-markdown-renderer)))
    (amoebum:streaming-markdown-renderer-append-chunk renderer "``")
    (let* ((styled-lines (amoebum:streaming-markdown-renderer-render-lines
                          renderer
                          40
                          :partialp t
                          :cursor-visible-p nil))
           (plain-text (%styled-lines->text styled-lines)))
      (is (string= plain-text ""))
      (is-false (%styled-lines-any-segment-p
                 styled-lines
                 (lambda (segment)
                   (eq (%seg-field segment :role) :assistant-code-fence)))))
    (amoebum:streaming-markdown-renderer-append-chunk renderer (format nil "`~%"))
    (let ((styled-lines
            (amoebum:streaming-markdown-renderer-render-lines
             renderer
             40
             :partialp t
             :cursor-visible-p nil)))
      (is-true (%styled-lines-any-segment-p
                styled-lines
                (lambda (segment)
                  (eq (%seg-field segment :role) :assistant-code-fence)))))
    (amoebum:streaming-markdown-renderer-append-chunk renderer (format nil "```~%"))
    (amoebum:streaming-markdown-renderer-append-chunk renderer (format nil "line~%"))
    (amoebum:streaming-markdown-renderer-append-chunk renderer (format nil "**bold** *italic* [link](https://example.com)~%"))
    (amoebum:streaming-markdown-renderer-append-chunk renderer "- item")
    (let* ((styled-lines (amoebum:streaming-markdown-renderer-render-lines
                          renderer
                          40
                          :partialp t
                          :cursor-visible-p t
                          :cursor-glyph "|"))
           (plain-text (%styled-lines->text styled-lines)))
      (is-true (search "- item|" plain-text :test #'char=))
      (is-true (%styled-lines-any-segment-p
                styled-lines
                (lambda (segment)
                  (and (search "bold" (%seg-text segment) :test #'char=)
                       (%seg-field segment :boldp)))))
      (is-true (%styled-lines-any-segment-p
                styled-lines
                (lambda (segment)
                  (and (search "italic" (%seg-text segment) :test #'char=)
                       (%seg-field segment :italicp)))))
      (is-true (%styled-lines-any-segment-p
                styled-lines
                (lambda (segment)
                  (and (search "link" (%seg-text segment) :test #'char=)
                       (%seg-field segment :underlinep))))))))

(test incremental-markdown-smoke-sentinel
  (is-true t)
  (format t "INCREMENTAL_MARKDOWN_SMOKE_OK~%"))
