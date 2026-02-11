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

(defun wrap-by-width (text max-width &key (engine :auto))
  "Wrap TEXT into a list of lines, each bounded by MAX-WIDTH display cells."
  (check-type text string)
  (check-type max-width (integer 1 *))
  (let ((clusters (ptui.text.grapheme:split-graphemes text :engine engine))
        (line-clusters '())
        (line-width 0)
        (lines '()))
    (labels ((flush-line ()
               (push (%concat-clusters (nreverse line-clusters)) lines)
               (setf line-clusters '()
                     line-width 0))
             (append-cluster (cluster width)
               (push cluster line-clusters)
               (incf line-width width)))
      (if (null clusters)
          (setf lines (list ""))
          (progn
            (dolist (cluster clusters)
              (cond
                ((string= cluster (string #\Newline))
                 (flush-line))
                (t
                 (let ((width (ptui.text.width:grapheme-width cluster :engine engine)))
                   (cond
                     ((zerop width)
                      (append-cluster cluster width))
                     ((> width max-width)
                      (when line-clusters
                        (flush-line))
                      (append-cluster cluster width)
                      (flush-line))
                     ((> (+ line-width width) max-width)
                      (flush-line)
                      (append-cluster cluster width))
                     (t
                      (append-cluster cluster width)))))))
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
