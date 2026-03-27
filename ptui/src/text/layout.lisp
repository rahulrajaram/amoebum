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

(defstruct (wrap-state (:constructor %make-wrap-state (preserve-spaces)))
  (line-clusters '())
  (line-width 0)
  (word-clusters '())
  (word-width 0)
  (lines '())
  (preserve-spaces preserve-spaces :type boolean))

(defun %wrap-newline-cluster-p (cluster)
  (string= cluster (string #\Newline)))

(defun %wrap-strip-trailing-spaces! (state engine)
  (loop while (and (wrap-state-line-clusters state)
                   (%breakable-cluster-p (first (wrap-state-line-clusters state))))
        do (let ((width (ptui.text.width:grapheme-width
                         (first (wrap-state-line-clusters state))
                         :engine engine)))
             (pop (wrap-state-line-clusters state))
             (decf (wrap-state-line-width state) width))))

(defun %wrap-reset-word! (state)
  (setf (wrap-state-word-clusters state) '()
        (wrap-state-word-width state) 0))

(defun %wrap-flush-line! (state engine)
  (unless (wrap-state-preserve-spaces state)
    (%wrap-strip-trailing-spaces! state engine))
  (push (%concat-clusters (nreverse (wrap-state-line-clusters state)))
        (wrap-state-lines state))
  (setf (wrap-state-line-clusters state) '()
        (wrap-state-line-width state) 0)
  (%wrap-reset-word! state))

(defun %wrap-append-cluster! (state cluster width breakablep)
  (push cluster (wrap-state-line-clusters state))
  (incf (wrap-state-line-width state) width)
  (if breakablep
      (%wrap-reset-word! state)
      (progn
        (push cluster (wrap-state-word-clusters state))
        (incf (wrap-state-word-width state) width))))

(defun %wrap-word-break-possible-p (state cluster-width max-width)
  (and (wrap-state-word-clusters state)
       (> (- (wrap-state-line-width state)
             (wrap-state-word-width state))
          0)
       (<= (+ (wrap-state-word-width state) cluster-width) max-width)))

(defun %wrap-reflow-word! (state cluster cluster-width engine)
  (let ((saved-word (nreverse (wrap-state-word-clusters state))))
    (setf (wrap-state-line-clusters state)
          (nthcdr (length saved-word) (wrap-state-line-clusters state)))
    (decf (wrap-state-line-width state) (wrap-state-word-width state))
    (%wrap-flush-line! state engine)
    (dolist (word-cluster saved-word)
      (%wrap-append-cluster! state
                             word-cluster
                             (ptui.text.width:grapheme-width word-cluster :engine engine)
                             nil))
    (%wrap-append-cluster! state cluster cluster-width nil)))

(defun %wrap-overflow-cluster! (state cluster cluster-width breakablep max-width engine)
  (cond
    (breakablep
     (%wrap-flush-line! state engine))
    ((%wrap-word-break-possible-p state cluster-width max-width)
     (%wrap-reflow-word! state cluster cluster-width engine))
    (t
     (%wrap-flush-line! state engine)
     (%wrap-append-cluster! state cluster cluster-width nil))))

(defun %wrap-process-cluster! (state cluster max-width engine)
  (when (%wrap-newline-cluster-p cluster)
    (%wrap-flush-line! state engine)
    (return-from %wrap-process-cluster!))
  (let* ((width (ptui.text.width:grapheme-width cluster :engine engine))
         (breakablep (%breakable-cluster-p cluster)))
    (cond
      ((zerop width)
       (%wrap-append-cluster! state cluster width nil))
      ((> width max-width)
       (when (wrap-state-line-clusters state)
         (%wrap-flush-line! state engine))
       (%wrap-append-cluster! state cluster width nil)
       (%wrap-flush-line! state engine))
      ((> (+ (wrap-state-line-width state) width) max-width)
       (%wrap-overflow-cluster! state cluster width breakablep max-width engine))
      (t
       (%wrap-append-cluster! state cluster width breakablep)))))

(defun wrap-by-width (text max-width &key (engine :auto) (preserve-spaces nil))
  "Wrap TEXT into a list of lines, each bounded by MAX-WIDTH display cells.
Prefers breaking at word boundaries (spaces). Falls back to character-level
breaks for words longer than MAX-WIDTH. When PRESERVE-SPACES is T, trailing
spaces are kept on wrapped lines (needed for cursor position mapping)."
  (check-type text string)
  (check-type max-width (integer 1 *))
  (let ((clusters (ptui.text.grapheme:split-graphemes text :engine engine))
        (state (%make-wrap-state preserve-spaces)))
    (when (null clusters)
      (return-from wrap-by-width (list "")))
    (dolist (cluster clusters)
      (%wrap-process-cluster! state cluster max-width engine))
    (%wrap-flush-line! state engine)
    (nreverse (wrap-state-lines state))))

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
