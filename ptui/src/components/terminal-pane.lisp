(defpackage :ptui.components.terminal-pane
  (:use :cl)
  (:export
   #:terminal-pane-state
   #:make-terminal-pane-state
   #:terminal-pane-title
   #:terminal-pane-lines
   #:terminal-pane-pending-output
   #:terminal-pane-max-lines
   #:terminal-pane-scroll-offset
   #:terminal-pane-status
   #:terminal-pane-empty-message
   #:terminal-pane-append-line
   #:terminal-pane-append-output
   #:terminal-pane-clear
   #:terminal-pane-scroll
   #:terminal-pane-scroll-home
   #:terminal-pane-scroll-end
   #:terminal-pane-visible-lines
   #:terminal-pane-visible-styled-lines
   #:terminal-pane-line-segments
   #:terminal-pane-search-query
   #:terminal-pane-search-results
   #:terminal-pane-search-selected-index
   #:terminal-pane-last-copy
   #:terminal-pane-set-search-query
   #:terminal-pane-selected-search-result
   #:terminal-pane-search-next
   #:terminal-pane-search-prev
   #:terminal-pane-copy-visible
   #:terminal-pane-copy-search-result
   #:terminal-pane-handle-event
   #:make-terminal-pane-widget))

(in-package :ptui.components.terminal-pane)

(defconstant +esc-char+ (code-char 27))

(defparameter +ansi-palette+
  ;; ANSI 16-color table (regular then bright).
  #(#(0 0 0) #(205 49 49) #(13 188 121) #(229 229 16)
    #(36 114 200) #(188 63 188) #(17 168 205) #(229 229 229)
    #(102 102 102) #(241 76 76) #(35 209 139) #(245 245 67)
    #(59 142 234) #(214 112 214) #(41 184 219) #(255 255 255)))

(defstruct (terminal-pane-style
            (:constructor %make-terminal-pane-style
                (&key
                  (fg ptui.core.color:color-default)
                  (bg ptui.core.color:color-default)
                  (boldp nil)
                  (italicp nil)
                  (underlinep nil)
                  (invertp nil)
                  (dimp nil)
                  (strikep nil))))
  (fg ptui.core.color:color-default)
  (bg ptui.core.color:color-default)
  (boldp nil :type boolean)
  (italicp nil :type boolean)
  (underlinep nil :type boolean)
  (invertp nil :type boolean)
  (dimp nil :type boolean)
  (strikep nil :type boolean))

(defstruct (terminal-pane-match
            (:constructor make-terminal-pane-match
                (&key
                  (line-index 0)
                  (column 0)
                  (length 0)
                  (line-text ""))))
  (line-index 0 :type fixnum)
  (column 0 :type fixnum)
  (length 0 :type fixnum)
  (line-text "" :type string))

(defstruct (terminal-pane-state
            (:constructor %make-terminal-pane-state
                (&key
                  (title "terminal")
                  (lines '())
                  (line-segments '())
                  (pending-output "")
                  (pending-segments '())
                  (pending-escape "")
                  (ansi-style (%make-terminal-pane-style))
                  (max-lines 2000)
                  (scroll-offset 0)
                  (status :idle)
                  (search-query "")
                  (search-case-insensitive t)
                  (search-results '())
                  (search-selected-index -1)
                  (last-copy "")
                  (empty-message "[no output]"))))
  (title "terminal" :type string)
  (lines '() :type list)
  (line-segments '() :type list)
  (pending-output "" :type string)
  (pending-segments '() :type list)
  (pending-escape "" :type string)
  (ansi-style (%make-terminal-pane-style) :type terminal-pane-style)
  (max-lines 2000 :type fixnum)
  (scroll-offset 0 :type fixnum)
  (status :idle :type keyword)
  (search-query "" :type string)
  (search-case-insensitive t :type boolean)
  (search-results '() :type list)
  (search-selected-index -1 :type fixnum)
  (last-copy "" :type string)
  (empty-message "[no output]" :type string))

(defun %normalize-line-text (value)
  (let* ((text (typecase value
                 (string value)
                 (pathname (namestring value))
                 (t (princ-to-string value))))
         (length (length text)))
    (if (and (> length 0)
             (char= (char text (1- length)) #\Return))
        (subseq text 0 (1- length))
        text)))

(defun %style->attrs (style)
  (ptui.core.types:make-attrs
   :boldp (terminal-pane-style-boldp style)
   :italicp (terminal-pane-style-italicp style)
   :underlinep (terminal-pane-style-underlinep style)
   :invertp (terminal-pane-style-invertp style)
   :dimp (terminal-pane-style-dimp style)
   :strikep (terminal-pane-style-strikep style)))

(defun %style->cell (style)
  (ptui.core.types:make-cell
   " "
   (terminal-pane-style-fg style)
   (terminal-pane-style-bg style)
   (%style->attrs style)))

(defun %clone-style (style)
  (%make-terminal-pane-style
   :fg (terminal-pane-style-fg style)
   :bg (terminal-pane-style-bg style)
   :boldp (terminal-pane-style-boldp style)
   :italicp (terminal-pane-style-italicp style)
   :underlinep (terminal-pane-style-underlinep style)
   :invertp (terminal-pane-style-invertp style)
   :dimp (terminal-pane-style-dimp style)
   :strikep (terminal-pane-style-strikep style)))

(defun %cell-style= (left right)
  (and (equalp (ptui.core.types:cell-fg left)
               (ptui.core.types:cell-fg right))
       (equalp (ptui.core.types:cell-bg left)
               (ptui.core.types:cell-bg right))
       (equalp (ptui.core.types:attrs-boldp (ptui.core.types:cell-attrs left))
               (ptui.core.types:attrs-boldp (ptui.core.types:cell-attrs right)))
       (equalp (ptui.core.types:attrs-italicp (ptui.core.types:cell-attrs left))
               (ptui.core.types:attrs-italicp (ptui.core.types:cell-attrs right)))
       (equalp (ptui.core.types:attrs-underlinep (ptui.core.types:cell-attrs left))
               (ptui.core.types:attrs-underlinep (ptui.core.types:cell-attrs right)))
       (equalp (ptui.core.types:attrs-invertp (ptui.core.types:cell-attrs left))
               (ptui.core.types:attrs-invertp (ptui.core.types:cell-attrs right)))
       (equalp (ptui.core.types:attrs-dimp (ptui.core.types:cell-attrs left))
               (ptui.core.types:attrs-dimp (ptui.core.types:cell-attrs right)))
       (equalp (ptui.core.types:attrs-strikep (ptui.core.types:cell-attrs left))
               (ptui.core.types:attrs-strikep (ptui.core.types:cell-attrs right)))))

(defun %last-segment (segments)
  (car (last segments)))

(defun %append-segment (segments text style)
  (if (zerop (length text))
      segments
      (let* ((cell (%style->cell style))
             (last (%last-segment segments)))
        (if (and last (%cell-style= (second last) cell))
            (progn
              (setf (first last) (concatenate 'string (first last) text))
              segments)
            (nconc segments (list (list text cell)))))))

(defun %plain-line-segments (line)
  (if (zerop (length line))
      '()
      (list (list line (%style->cell (%make-terminal-pane-style))))))

(defun %trim-lines+segments (lines line-segments max-lines)
  (let ((overflow (- (length lines) max-lines)))
    (if (> overflow 0)
        (values (nthcdr overflow lines)
                (nthcdr overflow line-segments))
        (values lines line-segments))))

(defun %normalize-segments-list (line-segments lines)
  (let ((normalized
          (mapcar
           (lambda (entry line)
             (cond
               ((and (listp entry) entry) entry)
               (t (%plain-line-segments line))))
           (or line-segments '())
           lines)))
    (if (< (length normalized) (length lines))
        (nconc normalized
               (mapcar #'%plain-line-segments
                       (nthcdr (length normalized) lines)))
        normalized)))

(defun make-terminal-pane-state (&key
                                   (title "terminal")
                                   (lines '())
                                   (pending-output "")
                                   (max-lines 2000)
                                   (scroll-offset 0)
                                   (status :idle)
                                   (empty-message "[no output]"))
  (check-type title string)
  (check-type pending-output string)
  (check-type max-lines (integer 1 *))
  (check-type scroll-offset (integer 0 *))
  (check-type empty-message string)
  (let* ((normalized-lines (mapcar #'%normalize-line-text (or lines '())))
         (normalized-segments (%normalize-segments-list '() normalized-lines)))
    (multiple-value-bind (trimmed-lines trimmed-segments)
        (%trim-lines+segments normalized-lines normalized-segments max-lines)
      (%make-terminal-pane-state :title title
                                 :lines trimmed-lines
                                 :line-segments trimmed-segments
                                 :pending-output pending-output
                                 :pending-segments (%plain-line-segments pending-output)
                                 :pending-escape ""
                                 :ansi-style (%make-terminal-pane-style)
                                 :max-lines max-lines
                                 :scroll-offset scroll-offset
                                 :status status
                                 :search-query ""
                                 :search-case-insensitive t
                                 :search-results '()
                                 :search-selected-index -1
                                 :last-copy ""
                                 :empty-message empty-message))))

(defun terminal-pane-title (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-title state))

(defun terminal-pane-lines (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-lines state))

(defun terminal-pane-line-segments (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-line-segments state))

(defun terminal-pane-pending-output (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-pending-output state))

(defun terminal-pane-max-lines (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-max-lines state))

(defun terminal-pane-scroll-offset (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-scroll-offset state))

(defun terminal-pane-status (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-status state))

(defun terminal-pane-search-query (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-search-query state))

(defun terminal-pane-search-results (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-search-results state))

(defun terminal-pane-search-selected-index (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-search-selected-index state))

(defun terminal-pane-last-copy (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-last-copy state))

(defun terminal-pane-empty-message (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-empty-message state))

(defun %display-lines (state &key (include-pending t))
  (check-type state terminal-pane-state)
  (let ((lines (copy-list (or (terminal-pane-state-lines state) '())))
        (pending (terminal-pane-state-pending-output state)))
    (if (and include-pending
             (stringp pending)
             (> (length pending) 0))
        (nconc lines (list pending))
        lines)))

(defun %display-line-segments (state &key (include-pending t))
  (check-type state terminal-pane-state)
  (let ((segments (copy-list (or (terminal-pane-state-line-segments state) '())))
        (pending (terminal-pane-state-pending-segments state)))
    (if (and include-pending
             (listp pending)
             pending)
        (nconc segments (list pending))
        segments)))

(defun %max-scroll-offset (state viewport-height)
  (let ((line-count (length (%display-lines state))))
    (max 0 (- line-count viewport-height))))

(defun %clamp-scroll-offset! (state &key viewport-height)
  (let* ((height (or viewport-height (length (%display-lines state))))
         (max-offset (%max-scroll-offset state (max 1 height))))
    (setf (terminal-pane-state-scroll-offset state)
          (max 0
               (min (terminal-pane-state-scroll-offset state)
                    max-offset))))
  state)

(defun %refresh-status! (state)
  (setf (terminal-pane-state-status state)
        (if (or (terminal-pane-state-lines state)
                (> (length (terminal-pane-state-pending-output state)) 0))
            :active
            :idle))
  state)

(defun %ansi-index->color (index)
  (cond
    ((and (integerp index) (<= 0 index 15))
     (let ((triple (aref +ansi-palette+ index)))
       (ptui.core.color:make-color-rgb
        (aref triple 0) (aref triple 1) (aref triple 2))))
    ((and (integerp index) (<= 16 index 231))
     (let* ((cube (- index 16))
            (r (floor cube 36))
            (g (floor (mod cube 36) 6))
            (b (mod cube 6))
            (levels #(0 95 135 175 215 255)))
       (ptui.core.color:make-color-rgb
        (aref levels r)
        (aref levels g)
        (aref levels b))))
    ((and (integerp index) (<= 232 index 255))
     (let ((level (+ 8 (* 10 (- index 232)))))
       (ptui.core.color:make-color-rgb level level level)))
    (t
     ptui.core.color:color-default)))

(defun %string-split (text delimiter)
  (let ((result '())
        (start 0))
    (loop for index from 0 below (length text) do
      (when (char= (char text index) delimiter)
        (push (subseq text start index) result)
        (setf start (1+ index))))
    (push (subseq text start) result)
    (nreverse result)))

(defun %parse-sgr-codes (params)
  (if (zerop (length params))
      '(0)
      (loop for token in (%string-split params #\;)
            collect (if (zerop (length token))
                        0
                        (or (parse-integer token :junk-allowed t)
                            0)))))

(defun %apply-sgr! (style params)
  (let ((codes (%parse-sgr-codes params)))
    (loop while codes do
      (let ((code (pop codes)))
        (cond
          ((= code 0)
           (setf (terminal-pane-style-fg style) ptui.core.color:color-default
                 (terminal-pane-style-bg style) ptui.core.color:color-default
                 (terminal-pane-style-boldp style) nil
                 (terminal-pane-style-italicp style) nil
                 (terminal-pane-style-underlinep style) nil
                 (terminal-pane-style-invertp style) nil
                 (terminal-pane-style-dimp style) nil
                 (terminal-pane-style-strikep style) nil))
          ((= code 1)
           (setf (terminal-pane-style-boldp style) t))
          ((= code 2)
           (setf (terminal-pane-style-dimp style) t))
          ((= code 3)
           (setf (terminal-pane-style-italicp style) t))
          ((= code 4)
           (setf (terminal-pane-style-underlinep style) t))
          ((= code 7)
           (setf (terminal-pane-style-invertp style) t))
          ((= code 9)
           (setf (terminal-pane-style-strikep style) t))
          ((= code 22)
           (setf (terminal-pane-style-boldp style) nil
                 (terminal-pane-style-dimp style) nil))
          ((= code 23)
           (setf (terminal-pane-style-italicp style) nil))
          ((= code 24)
           (setf (terminal-pane-style-underlinep style) nil))
          ((= code 27)
           (setf (terminal-pane-style-invertp style) nil))
          ((= code 29)
           (setf (terminal-pane-style-strikep style) nil))
          ((<= 30 code 37)
           (setf (terminal-pane-style-fg style) (%ansi-index->color (- code 30))))
          ((= code 39)
           (setf (terminal-pane-style-fg style) ptui.core.color:color-default))
          ((<= 40 code 47)
           (setf (terminal-pane-style-bg style) (%ansi-index->color (- code 40))))
          ((= code 49)
           (setf (terminal-pane-style-bg style) ptui.core.color:color-default))
          ((<= 90 code 97)
           (setf (terminal-pane-style-fg style) (%ansi-index->color (+ 8 (- code 90)))))
          ((<= 100 code 107)
           (setf (terminal-pane-style-bg style) (%ansi-index->color (+ 8 (- code 100)))))
          ((or (= code 38) (= code 48))
           (let ((fgp (= code 38)))
             (when codes
               (let ((mode (pop codes)))
                 (cond
                   ((and (= mode 5) codes)
                    (let ((value (pop codes)))
                      (if fgp
                          (setf (terminal-pane-style-fg style) (%ansi-index->color value))
                          (setf (terminal-pane-style-bg style) (%ansi-index->color value)))))
                   ((and (= mode 2) (>= (length codes) 3))
                    (let ((r (pop codes))
                          (g (pop codes))
                          (b (pop codes)))
                      (when (and (integerp r) (integerp g) (integerp b)
                                 (<= 0 r 255) (<= 0 g 255) (<= 0 b 255))
                        (if fgp
                            (setf (terminal-pane-style-fg style)
                                  (ptui.core.color:make-color-rgb r g b))
                            (setf (terminal-pane-style-bg style)
                                  (ptui.core.color:make-color-rgb r g b))))))))))))))))

(defun %append-pending-run! (state text style)
  (unless (zerop (length text))
    (setf (terminal-pane-state-pending-output state)
          (concatenate 'string
                       (terminal-pane-state-pending-output state)
                       text))
    (setf (terminal-pane-state-pending-segments state)
          (%append-segment (terminal-pane-state-pending-segments state)
                           text
                           style))))

(defun %append-line-internal (state line segments)
  (let* ((normalized (%normalize-line-text line))
         (normalized-segments (if (and (listp segments) segments)
                                  segments
                                  (%plain-line-segments normalized)))
         (next-lines (nconc (terminal-pane-state-lines state)
                            (list normalized)))
         (next-segments (nconc (terminal-pane-state-line-segments state)
                               (list normalized-segments))))
    (multiple-value-bind (trimmed-lines trimmed-segments)
        (%trim-lines+segments next-lines
                              next-segments
                              (terminal-pane-state-max-lines state))
      (setf (terminal-pane-state-lines state) trimmed-lines
            (terminal-pane-state-line-segments state) trimmed-segments)))
  (%clamp-scroll-offset! state)
  (%refresh-status! state)
  state)

(defun %consume-output! (state output)
  (let* ((combined (if (zerop (length (terminal-pane-state-pending-escape state)))
                       output
                       (concatenate 'string
                                    (terminal-pane-state-pending-escape state)
                                    output)))
         (length (length combined))
         (index 0)
         (style (%clone-style (terminal-pane-state-ansi-style state)))
         (run-text ""))
    (setf (terminal-pane-state-pending-escape state) "")
    (labels ((flush-run ()
               (when (> (length run-text) 0)
                 (%append-pending-run! state run-text style)
                 (setf run-text ""))))
      (loop while (< index length) do
        (let ((ch (char combined index)))
          (cond
            ((char= ch +esc-char+)
             (if (and (< (1+ index) length)
                      (char= (char combined (1+ index)) #\[))
                 (let ((end (loop for scan from (+ index 2) below length
                                  for c = (char combined scan)
                                  when (and (char<= #\@ c) (char<= c #\~))
                                    do (return scan))))
                   (if end
                       (progn
                         (flush-run)
                         (when (char= (char combined end) #\m)
                           (%apply-sgr! style
                                        (subseq combined (+ index 2) end)))
                         (setf index (1+ end)))
                       (progn
                         (flush-run)
                         (setf (terminal-pane-state-pending-escape state)
                               (subseq combined index))
                         (setf index length))))
                 (progn
                   (flush-run)
                   (if (= (1+ index) length)
                       (setf (terminal-pane-state-pending-escape state)
                             (string +esc-char+))
                       (%append-pending-run! state (string ch) style))
                   (incf index))))
            ((char= ch #\Newline)
             (flush-run)
             (%append-line-internal state
                                    (terminal-pane-state-pending-output state)
                                    (terminal-pane-state-pending-segments state))
             (setf (terminal-pane-state-pending-output state) ""
                   (terminal-pane-state-pending-segments state) '())
             (incf index))
            ((char= ch #\Return)
             (incf index))
            (t
             (setf run-text (concatenate 'string run-text (string ch)))
             (incf index)))))
      (flush-run))
    (setf (terminal-pane-state-ansi-style state) style)))

(defun terminal-pane-append-line (state line)
  "Append one completed output line."
  (check-type state terminal-pane-state)
  (%append-line-internal state line (%plain-line-segments (%normalize-line-text line)))
  (%recompute-search! state)
  state)

(defun %recompute-search! (state)
  (let ((query (terminal-pane-state-search-query state))
        (case-insensitive (terminal-pane-state-search-case-insensitive state)))
    (if (zerop (length query))
        (setf (terminal-pane-state-search-results state) '()
              (terminal-pane-state-search-selected-index state) -1)
        (let ((matches '())
              (query-length (length query))
              (test (if case-insensitive #'char-equal #'char=)))
          (loop for line in (%display-lines state :include-pending t)
                for line-index from 0 do
                  (let ((cursor 0))
                    (loop
                      for position = (search query line :start2 cursor :test test)
                      while position do
                        (push (make-terminal-pane-match
                               :line-index line-index
                               :column position
                               :length query-length
                               :line-text line)
                              matches)
                        (setf cursor (1+ position)))))
          (setf (terminal-pane-state-search-results state) (nreverse matches))
          (if (terminal-pane-state-search-results state)
              (setf (terminal-pane-state-search-selected-index state)
                    (max 0
                         (min (terminal-pane-state-search-selected-index state)
                              (1- (length (terminal-pane-state-search-results state))))))
              (setf (terminal-pane-state-search-selected-index state) -1)))))
  state)

(defun terminal-pane-append-output (state output)
  "Append raw output chunk, preserving trailing partial line state."
  (check-type state terminal-pane-state)
  (check-type output string)
  (unless (zerop (length output))
    (%consume-output! state output))
  (%recompute-search! state)
  (%refresh-status! state)
  state)

(defun terminal-pane-clear (state)
  "Clear all buffered terminal content."
  (check-type state terminal-pane-state)
  (setf (terminal-pane-state-lines state) '()
        (terminal-pane-state-line-segments state) '()
        (terminal-pane-state-pending-output state) ""
        (terminal-pane-state-pending-segments state) '()
        (terminal-pane-state-pending-escape state) ""
        (terminal-pane-state-ansi-style state) (%make-terminal-pane-style)
        (terminal-pane-state-search-results state) '()
        (terminal-pane-state-search-selected-index state) -1
        (terminal-pane-state-last-copy state) ""
        (terminal-pane-state-scroll-offset state) 0)
  (%refresh-status! state)
  state)

(defun %visible-window-range (state viewport-height &key (include-pending t))
  (let* ((lines (%display-lines state :include-pending include-pending))
         (count (length lines))
         (max-offset (max 0 (- count viewport-height)))
         (offset (max 0 (min (terminal-pane-state-scroll-offset state)
                             max-offset)))
         (end (max 0 (- count offset)))
         (start (max 0 (- end viewport-height))))
    (setf (terminal-pane-state-scroll-offset state) offset)
    (values start end)))

(defun terminal-pane-visible-lines (state &key (viewport-height 12) (include-pending t))
  "Return visible lines for the current scroll offset."
  (check-type state terminal-pane-state)
  (check-type viewport-height (integer 1 *))
  (let ((lines (%display-lines state :include-pending include-pending)))
    (multiple-value-bind (start end)
        (%visible-window-range state viewport-height :include-pending include-pending)
      (if (>= start end)
          '()
          (subseq lines start end)))))

(defun terminal-pane-visible-styled-lines (state &key (viewport-height 12) (include-pending t))
  "Return visible styled line segments aligned with TERMINAL-PANE-VISIBLE-LINES."
  (check-type state terminal-pane-state)
  (check-type viewport-height (integer 1 *))
  (let ((segments (%display-line-segments state :include-pending include-pending)))
    (multiple-value-bind (start end)
        (%visible-window-range state viewport-height :include-pending include-pending)
      (if (>= start end)
          '()
          (subseq segments start end)))))

(defun terminal-pane-scroll (state delta &key (viewport-height 12))
  "Adjust scroll offset by DELTA; positive values scroll back in history."
  (check-type state terminal-pane-state)
  (check-type delta integer)
  (check-type viewport-height (integer 1 *))
  (let* ((max-offset (%max-scroll-offset state viewport-height))
         (next-offset (+ (terminal-pane-state-scroll-offset state) delta)))
    (setf (terminal-pane-state-scroll-offset state)
          (max 0 (min max-offset next-offset))))
  state)

(defun terminal-pane-scroll-home (state &key (viewport-height 12))
  "Jump to the oldest visible window."
  (check-type state terminal-pane-state)
  (check-type viewport-height (integer 1 *))
  (setf (terminal-pane-state-scroll-offset state)
        (%max-scroll-offset state viewport-height))
  state)

(defun terminal-pane-scroll-end (state)
  "Jump to latest output."
  (check-type state terminal-pane-state)
  (setf (terminal-pane-state-scroll-offset state) 0)
  state)

(defun %selected-match (state)
  (let* ((index (terminal-pane-state-search-selected-index state))
         (matches (terminal-pane-state-search-results state)))
    (when (and (>= index 0)
               (< index (length matches)))
      (nth index matches))))

(defun terminal-pane-selected-search-result (state)
  (check-type state terminal-pane-state)
  (%selected-match state))

(defun %scroll-to-line! (state line-index viewport-height)
  (let* ((count (length (%display-lines state :include-pending t)))
         (max-offset (max 0 (- count viewport-height)))
         (current-offset (terminal-pane-state-scroll-offset state))
         (end (max 0 (- count current-offset)))
         (start (max 0 (- end viewport-height))))
    (cond
      ((< line-index start)
       (setf (terminal-pane-state-scroll-offset state)
             (max 0 (min max-offset (- count (+ line-index viewport-height))))))
      ((>= line-index end)
       (setf (terminal-pane-state-scroll-offset state)
             (max 0 (min max-offset (- count (1+ line-index)))))))
    (%clamp-scroll-offset! state :viewport-height viewport-height)))

(defun terminal-pane-set-search-query (state query &key (case-insensitive t) (viewport-height 12))
  (check-type state terminal-pane-state)
  (check-type query string)
  (check-type viewport-height (integer 1 *))
  (setf (terminal-pane-state-search-query state) query
        (terminal-pane-state-search-case-insensitive state) (not (null case-insensitive)))
  (%recompute-search! state)
  (let ((match (%selected-match state)))
    (when match
      (%scroll-to-line! state
                        (terminal-pane-match-line-index match)
                        viewport-height)))
  state)

(defun terminal-pane-search-next (state &key (viewport-height 12))
  (check-type state terminal-pane-state)
  (check-type viewport-height (integer 1 *))
  (let* ((matches (terminal-pane-state-search-results state))
         (count (length matches)))
    (when (> count 0)
      (setf (terminal-pane-state-search-selected-index state)
            (mod (1+ (max 0 (terminal-pane-state-search-selected-index state)))
                 count))
      (let ((match (%selected-match state)))
        (when match
          (%scroll-to-line! state
                            (terminal-pane-match-line-index match)
                            viewport-height)))))
  state)

(defun terminal-pane-search-prev (state &key (viewport-height 12))
  (check-type state terminal-pane-state)
  (check-type viewport-height (integer 1 *))
  (let* ((matches (terminal-pane-state-search-results state))
         (count (length matches)))
    (when (> count 0)
      (setf (terminal-pane-state-search-selected-index state)
            (mod (1- (max 0 (terminal-pane-state-search-selected-index state)))
                 count))
      (let ((match (%selected-match state)))
        (when match
          (%scroll-to-line! state
                            (terminal-pane-match-line-index match)
                            viewport-height)))))
  state)

(defun %join-lines (lines)
  (if (null lines)
      ""
      (reduce (lambda (left right)
                (concatenate 'string left (string #\Newline) right))
              (rest lines)
              :initial-value (first lines))))

(defun terminal-pane-copy-visible (state &key (viewport-height 12) (include-pending t))
  (check-type state terminal-pane-state)
  (check-type viewport-height (integer 1 *))
  (let ((copied (%join-lines
                 (terminal-pane-visible-lines state
                                              :viewport-height viewport-height
                                              :include-pending include-pending))))
    (setf (terminal-pane-state-last-copy state) copied)
    copied))

(defun terminal-pane-copy-search-result (state)
  (check-type state terminal-pane-state)
  (let* ((selected (%selected-match state))
         (copied (if selected
                     (terminal-pane-match-line-text selected)
                     "")))
    (setf (terminal-pane-state-last-copy state) copied)
    copied))

(defun terminal-pane-handle-event (state event &key (viewport-height 12))
  "Apply key navigation event to STATE and return action metadata."
  (check-type state terminal-pane-state)
  (check-type viewport-height (integer 1 *))
  (unless (typep event 'ptui.core.events:key-event)
    (return-from terminal-pane-handle-event
      (list :action :ignored :state state)))
  (let* ((key (ptui.core.events:key-event-key event))
         (text (ptui.core.events:key-event-text? event))
         (page-step (max 1 (1- viewport-height))))
    (cond
      ((eq key :up)
       (terminal-pane-scroll state 1 :viewport-height viewport-height)
       (list :action :scrolled :delta 1 :state state))
      ((eq key :down)
       (terminal-pane-scroll state -1 :viewport-height viewport-height)
       (list :action :scrolled :delta -1 :state state))
      ((eq key :page-up)
       (terminal-pane-scroll state page-step :viewport-height viewport-height)
       (list :action :scrolled :delta page-step :state state))
      ((eq key :page-down)
       (terminal-pane-scroll state (- page-step) :viewport-height viewport-height)
       (list :action :scrolled :delta (- page-step) :state state))
      ((eq key :home)
       (terminal-pane-scroll-home state :viewport-height viewport-height)
       (list :action :scrolled-home :state state))
      ((eq key :end)
       (terminal-pane-scroll-end state)
       (list :action :scrolled-end :state state))
      ((or (eq key :search-next)
           (and (eq key :text) (string= text "n")))
       (terminal-pane-search-next state :viewport-height viewport-height)
       (list :action :search-next
             :state state
             :match (terminal-pane-selected-search-result state)))
      ((or (eq key :search-prev)
           (and (eq key :text) (string= text "N")))
       (terminal-pane-search-prev state :viewport-height viewport-height)
       (list :action :search-prev
             :state state
             :match (terminal-pane-selected-search-result state)))
      ((eq key :search)
       (terminal-pane-set-search-query state
                                       (or text "")
                                       :viewport-height viewport-height)
       (list :action :search-query-updated
             :state state
             :query (terminal-pane-search-query state)
             :match-count (length (terminal-pane-search-results state))))
      ((eq key :copy-visible)
       (let ((copied (terminal-pane-copy-visible
                      state
                      :viewport-height viewport-height)))
         (list :action :copied-visible
               :state state
               :text copied)))
      ((eq key :copy-search-match)
       (let ((copied (terminal-pane-copy-search-result state)))
         (list :action :copied-search-match
               :state state
               :text copied)))
      (t
       (list :action :ignored :state state)))))

(defun %status-line (state)
  (let* ((line-count (length (%display-lines state)))
         (offset (terminal-pane-state-scroll-offset state))
         (partialp (> (length (terminal-pane-state-pending-output state)) 0))
         (matches (length (terminal-pane-state-search-results state))))
    (format nil "~A | ~A | ~D line~:P~@[ | +~D~]~:[~; | partial~]~:[~; | /~D~]"
            (terminal-pane-state-title state)
            (terminal-pane-state-status state)
            line-count
            (and (> offset 0) offset)
            partialp
            (> matches 0)
            matches)))

(defun make-terminal-pane-widget (state &key id key (borderp t) (padding 0) (viewport-height 12))
  "Build a reusable terminal pane element tree from STATE."
  (check-type state terminal-pane-state)
  (check-type viewport-height (integer 1 *))
  (let* ((status-widget (ptui.widgets.core:make-text-widget (%status-line state)))
         (line-widgets
           (let ((visible (terminal-pane-visible-lines state :viewport-height viewport-height))
                 (styled (terminal-pane-visible-styled-lines state :viewport-height viewport-height)))
             (if visible
                 (loop for line in visible
                       for segments in styled
                       collect (ptui.widgets.core:make-text-widget
                                line
                                :styled-segments segments))
                 (list (ptui.widgets.core:make-text-widget
                        (terminal-pane-state-empty-message state))))))
         (content (ptui.widgets.core:make-stack-widget
                   (append (list status-widget) line-widgets)
                   :direction :column
                   :gap 0)))
    (ptui.widgets.core:make-box-widget content
                                       :id id
                                       :key key
                                       :padding padding
                                       :borderp borderp)))
