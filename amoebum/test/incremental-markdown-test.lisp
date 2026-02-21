(in-package :amoebum/test)

(def-suite incremental-markdown-suite
  :description "I227 incremental markdown renderer behavior."
  :in amoebum-suite)

(in-suite incremental-markdown-suite)

(defun %styled-lines->text (styled-lines)
  (with-output-to-string (out)
    (loop for line in styled-lines
          for line-index from 0 do
            (when (> line-index 0)
              (write-char #\Newline out))
            (dolist (segment line)
              (write-string (getf segment :text "") out)))))

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
                   (eq (getf segment :role) :assistant-code-fence)))))
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
                  (eq (getf segment :role) :assistant-code-fence)))))
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
                  (and (search "bold" (getf segment :text "") :test #'char=)
                       (getf segment :boldp)))))
      (is-true (%styled-lines-any-segment-p
                styled-lines
                (lambda (segment)
                  (and (search "italic" (getf segment :text "") :test #'char=)
                       (getf segment :italicp)))))
      (is-true (%styled-lines-any-segment-p
                styled-lines
                (lambda (segment)
                  (and (search "link" (getf segment :text "") :test #'char=)
                       (getf segment :underlinep))))))))

(test incremental-markdown-smoke-sentinel
  (is-true t)
  (format t "INCREMENTAL_MARKDOWN_SMOKE_OK~%"))
