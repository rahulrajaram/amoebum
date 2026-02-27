(defpackage :ptui.text.layout
  (:use :cl)
  (:export
   #:wrap-by-width
   #:truncate-to-width
   #:width-safe-slice))

(in-package :ptui.text.layout)

(defun %concat-clusters (clusters)
  (with-output-to-string (out)
    (dolist (cluster clusters)
      (write-string cluster out))))

(defun width-safe-slice (text start-width end-width &key (engine :auto))
  "Return grapheme-safe TEXT slice spanning display columns [START-WIDTH, END-WIDTH)."
  (check-type text string)
  (check-type start-width (integer 0 *))
  (check-type end-width (integer 0 *))
  (when (> start-width end-width)
    (error "START-WIDTH (~D) cannot exceed END-WIDTH (~D)." start-width end-width))
  (let ((clusters (ptui.text.grapheme:split-graphemes text :engine engine))
        (cursor 0)
        (selected '()))
    (dolist (cluster clusters)
      (let* ((w (ptui.text.width:grapheme-width cluster :engine engine))
             (start cursor)
             (end (+ cursor w)))
        (when (or (and (zerop w)
                       (<= start-width start)
                       (< start end-width))
                  (and (<= start-width start)
                       (<= end end-width)))
          (push cluster selected))
        (unless (zerop w)
          (setf cursor end))))
    (%concat-clusters (nreverse selected))))

(defun %breakable-cluster-p (cluster)
  "Return T if CLUSTER is a space or breakable whitespace (not newline)."
  (and (= (length cluster) 1)
       (let ((ch (char cluster 0)))
         (or (char= ch #\Space)
             (char= ch #\Tab)))))

(defun wrap-by-width (text max-width &key (engine :auto) (preserve-spaces nil))
  "Wrap TEXT into a list of lines, each bounded by MAX-WIDTH display cells.
Prefers breaking at word boundaries (spaces). Falls back to character-level
breaks for words longer than MAX-WIDTH. When PRESERVE-SPACES is T, trailing
spaces are kept on wrapped lines (needed for cursor position mapping)."
  (check-type text string)
  (check-type max-width (integer 1 *))
  (let ((clusters (ptui.text.grapheme:split-graphemes text :engine engine))
        (line-clusters '())
        (line-width 0)
        ;; Track last breakable position for word-boundary wrapping
        (word-clusters '())   ; clusters since last break point
        (word-width 0)        ; width of clusters since last break point
        (lines '()))
    (labels ((%strip-trailing-spaces ()
               ;; Remove trailing breakable clusters from line-clusters (reversed)
               (loop while (and line-clusters
                                (%breakable-cluster-p (first line-clusters)))
                     do (let ((sp-w (ptui.text.width:grapheme-width
                                     (first line-clusters) :engine engine)))
                          (pop line-clusters)
                          (decf line-width sp-w))))
             (flush-line ()
               (unless preserve-spaces (%strip-trailing-spaces))
               (push (%concat-clusters (nreverse line-clusters)) lines)
               (setf line-clusters '()
                     line-width 0
                     word-clusters '()
                     word-width 0))
             (append-cluster (cluster width breakablep)
               (push cluster line-clusters)
               (incf line-width width)
               (if breakablep
                   ;; Reset word tracking after a breakable cluster
                   (setf word-clusters '()
                         word-width 0)
                   (progn
                     (push cluster word-clusters)
                     (incf word-width width)))))
      (if (null clusters)
          (setf lines (list ""))
          (progn
            (dolist (cluster clusters)
              (cond
                ((string= cluster (string #\Newline))
                 (flush-line))
                (t
                 (let* ((width (ptui.text.width:grapheme-width cluster :engine engine))
                        (breakp (%breakable-cluster-p cluster)))
                   (cond
                     ((zerop width)
                      (append-cluster cluster width nil))
                     ((> width max-width)
                      ;; Single cluster wider than line — force its own line
                      (when line-clusters (flush-line))
                      (append-cluster cluster width nil)
                      (flush-line))
                     ((> (+ line-width width) max-width)
                      ;; Would overflow. Try to break at word boundary.
                      (cond
                        ;; Space itself overflows — just break, skip the space
                        (breakp
                         (flush-line))
                        ;; Word boundary available — rewind to it
                        ((and word-clusters
                              (> (- line-width word-width) 0)
                              (<= (+ word-width width) max-width))
                         (let ((saved-word (nreverse word-clusters)))
                           ;; Remove word clusters from line-clusters
                           (setf line-clusters (nthcdr (length saved-word) line-clusters))
                           (decf line-width word-width)
                           (flush-line)
                           ;; Re-add the word clusters
                           (dolist (wc saved-word)
                             (let ((ww (ptui.text.width:grapheme-width wc :engine engine)))
                               (append-cluster wc ww nil)))
                           ;; Add the current cluster
                           (append-cluster cluster width nil)))
                        ;; No word boundary — hard break
                        (t
                         (flush-line)
                         (append-cluster cluster width nil))))
                     (t
                      (append-cluster cluster width breakp)))))))
            (flush-line))))
    (nreverse lines)))

(defun %ellipsis-string (ellipsis)
  (cond
    ((null ellipsis) nil)
    ((eq ellipsis t) "…")
    ((stringp ellipsis) ellipsis)
    (t (error "ELLIPSIS must be NIL, T, or a string. Got: ~S" ellipsis))))

(defun truncate-to-width (text max-width &key (ellipsis nil) (engine :auto))
  "Truncate TEXT to MAX-WIDTH display cells, optionally appending ELLIPSIS."
  (check-type text string)
  (check-type max-width (integer 0 *))
  (when (zerop max-width)
    (return-from truncate-to-width ""))
  (let ((text-width (ptui.text.width:string-width text :engine engine)))
    (if (<= text-width max-width)
        text
        (let ((ellipsis* (%ellipsis-string ellipsis)))
          (if (null ellipsis*)
              (width-safe-slice text 0 max-width :engine engine)
              (let ((ellipsis-width (ptui.text.width:string-width ellipsis* :engine engine)))
                (if (>= ellipsis-width max-width)
                    (width-safe-slice ellipsis* 0 max-width :engine engine)
                    (concatenate
                     'string
                     (width-safe-slice text 0 (- max-width ellipsis-width) :engine engine)
                     ellipsis*))))))))
