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
   #:terminal-pane-line-metadata
   #:terminal-pane-pending-line-metadata
   #:terminal-pane-append-line
   #:terminal-pane-append-output
   #:terminal-pane-clear
   #:terminal-pane-scroll
   #:terminal-pane-scroll-home
   #:terminal-pane-scroll-end
   #:terminal-pane-visible-lines
   #:terminal-pane-visible-line-metadata
   #:terminal-pane-visible-styled-lines
   #:terminal-pane-line-segments
   #:terminal-pane-search-query
   #:terminal-pane-search-results
   #:terminal-pane-search-selected-index
   #:terminal-pane-last-copy
   #:terminal-pane-stdin-capture-policy
   #:terminal-pane-stdin-capture-enabled-p
   #:terminal-pane-set-stdin-capture-policy
   #:terminal-pane-drain-stdin-events
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

(defparameter +terminal-pane-severity-order+
  '(:debug :info :warning :error :critical))

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
                  (line-metadata '())
                  (pending-output "")
                  (pending-segments '())
                  (pending-line-metadata '(:severity :info :style :plain))
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
                  (empty-message "[no output]")
                  (stdin-capture-policy :enabled)
                  (stdin-events '()))))
  (title "terminal" :type string)
  (lines '() :type list)
  (line-segments '() :type list)
  (line-metadata '() :type list)
  (pending-output "" :type string)
  (pending-segments '() :type list)
  (pending-line-metadata '(:severity :info :style :plain) :type list)
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
  (empty-message "[no output]" :type string)
  (stdin-capture-policy :enabled :type keyword)
  (stdin-events '() :type list))

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

(defun %normalize-severity (severity)
  (if (member severity +terminal-pane-severity-order+ :test #'eq)
      severity
      :info))

(defun %normalize-style (style)
  (if (keywordp style)
      style
      :plain))

(defun %normalize-stdin-capture-policy (policy)
  (cond
    ((or (eq policy t)
         (eq policy :enabled)
         (eq policy :capture)
         (eq policy :on))
     :enabled)
    ((or (null policy)
         (eq policy :disabled)
         (eq policy :off)
         (eq policy :blocked)
         (eq policy :deny))
     :disabled)
    (t
     :enabled)))

(defun %line-metadata-severity (metadata)
  (%normalize-severity (getf metadata :severity :info)))

(defun %line-metadata-style (metadata)
  (%normalize-style (getf metadata :style :plain)))

(defun %normalize-line-metadata (&key severity style)
  (list :severity (%normalize-severity severity)
        :style (%normalize-style style)))

(defun %normalize-existing-metadata (metadata)
  (%normalize-line-metadata
   :severity (getf metadata :severity :info)
   :style (getf metadata :style :plain)))

(defun %severity-rank (severity)
  (or (position (%normalize-severity severity)
                +terminal-pane-severity-order+
                :test #'eq)
      1))

(defun %merge-line-metadata (existing incoming)
  (let* ((left (if (and (listp existing) existing)
                   existing
                   (%normalize-line-metadata :severity :info :style :plain)))
         (right (if (and (listp incoming) incoming)
                    incoming
                    (%normalize-line-metadata :severity :info :style :plain)))
         (left-severity (%line-metadata-severity left))
         (right-severity (%line-metadata-severity right))
         (left-style (%line-metadata-style left))
         (right-style (%line-metadata-style right))
         (severity (cond
                     ((eq left-severity :info) right-severity)
                     ((eq right-severity :info) left-severity)
                     ((>= (%severity-rank right-severity) (%severity-rank left-severity))
                      right-severity)
                     (t
                      left-severity)))
         (style (cond
                  ((eq left-style right-style) left-style)
                  ((eq left-style :plain) right-style)
                  ((eq right-style :plain) left-style)
                  (t :mixed))))
    (%normalize-line-metadata :severity severity :style style)))

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

(defun %trim-lines+segments+metadata (lines line-segments line-metadata max-lines)
  (let ((overflow (- (length lines) max-lines)))
    (if (> overflow 0)
        (values (nthcdr overflow lines)
                (nthcdr overflow line-segments)
                (nthcdr overflow line-metadata))
        (values lines line-segments line-metadata))))

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

(defun %normalize-metadata-list (line-metadata lines)
  (let ((incoming (copy-list (or line-metadata '()))))
    (loop repeat (length lines)
          for metadata = (if incoming (pop incoming) nil)
          collect (if (and (listp metadata) metadata)
                      (%normalize-existing-metadata metadata)
                      (%normalize-line-metadata :severity :info :style :plain)))))

(defun make-terminal-pane-state (&key
                                   (title "terminal")
                                   (lines '())
                                   (line-metadata '())
                                   (pending-output "")
                                   (pending-line-metadata '(:severity :info :style :plain))
                                   (max-lines 2000)
                                   (scroll-offset 0)
                                   (status :idle)
                                   (empty-message "[no output]")
                                   (stdin-capture-policy :enabled))
  (check-type title string)
  (check-type pending-output string)
  (check-type max-lines (integer 1 *))
  (check-type scroll-offset (integer 0 *))
  (check-type empty-message string)
  (let* ((normalized-lines (mapcar #'%normalize-line-text (or lines '())))
         (normalized-segments (%normalize-segments-list '() normalized-lines))
         (normalized-metadata (%normalize-metadata-list line-metadata normalized-lines)))
    (multiple-value-bind (trimmed-lines trimmed-segments trimmed-metadata)
        (%trim-lines+segments+metadata normalized-lines
                                       normalized-segments
                                       normalized-metadata
                                       max-lines)
      (%make-terminal-pane-state :title title
                                 :lines trimmed-lines
                                 :line-segments trimmed-segments
                                 :line-metadata trimmed-metadata
                                 :pending-output pending-output
                                 :pending-segments (%plain-line-segments pending-output)
                                 :pending-line-metadata (%normalize-existing-metadata
                                                         pending-line-metadata)
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
                                 :empty-message empty-message
                                 :stdin-capture-policy (%normalize-stdin-capture-policy
                                                        stdin-capture-policy)
                                 :stdin-events '()))))

(defun terminal-pane-title (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-title state))

(defun terminal-pane-lines (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-lines state))

(defun terminal-pane-line-segments (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-line-segments state))

(defun terminal-pane-line-metadata (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-line-metadata state))

(defun terminal-pane-pending-output (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-pending-output state))

(defun terminal-pane-pending-line-metadata (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-pending-line-metadata state))

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

(defun terminal-pane-stdin-capture-policy (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-stdin-capture-policy state))

(defun terminal-pane-stdin-capture-enabled-p (state)
  (check-type state terminal-pane-state)
  (eq (terminal-pane-state-stdin-capture-policy state) :enabled))

(defun terminal-pane-set-stdin-capture-policy (state policy)
  (check-type state terminal-pane-state)
  (setf (terminal-pane-state-stdin-capture-policy state)
        (%normalize-stdin-capture-policy policy))
  state)

(defun terminal-pane-drain-stdin-events (state)
  (check-type state terminal-pane-state)
  (let ((events (nreverse (terminal-pane-state-stdin-events state))))
    (setf (terminal-pane-state-stdin-events state) '())
    (values events (length events))))

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

(defun %display-line-metadata (state &key (include-pending t))
  (check-type state terminal-pane-state)
  (let ((metadata (copy-list (or (terminal-pane-state-line-metadata state) '())))
        (pending (terminal-pane-state-pending-line-metadata state)))
    (if (and include-pending
             (> (length (terminal-pane-state-pending-output state)) 0))
        (nconc metadata
               (list (if (and (listp pending) pending)
                         pending
                         (%normalize-line-metadata :severity :info :style :plain))))
        metadata)))

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

(defun %append-pending-run! (state text style line-metadata)
  (unless (zerop (length text))
    (setf (terminal-pane-state-pending-output state)
          (concatenate 'string
                       (terminal-pane-state-pending-output state)
                       text))
    (setf (terminal-pane-state-pending-segments state)
          (%append-segment (terminal-pane-state-pending-segments state)
                           text
                           style))
    (setf (terminal-pane-state-pending-line-metadata state)
          (%merge-line-metadata
           (terminal-pane-state-pending-line-metadata state)
           line-metadata))))

(defun %append-line-internal (state line segments metadata)
  (let* ((normalized (%normalize-line-text line))
         (normalized-segments (if (and (listp segments) segments)
                                  segments
                                  (%plain-line-segments normalized)))
         (normalized-metadata (%normalize-existing-metadata metadata))
         (next-lines (nconc (terminal-pane-state-lines state)
                            (list normalized)))
         (next-segments (nconc (terminal-pane-state-line-segments state)
                               (list normalized-segments)))
         (next-metadata (nconc (terminal-pane-state-line-metadata state)
                               (list normalized-metadata))))
    (multiple-value-bind (trimmed-lines trimmed-segments trimmed-metadata)
        (%trim-lines+segments+metadata next-lines
                                       next-segments
                                       next-metadata
                                       (terminal-pane-state-max-lines state))
      (setf (terminal-pane-state-lines state) trimmed-lines
            (terminal-pane-state-line-segments state) trimmed-segments
            (terminal-pane-state-line-metadata state) trimmed-metadata)))
  (%clamp-scroll-offset! state)
  (%refresh-status! state)
  state)

(defun %consume-output! (state output line-metadata)
  (let* ((combined (if (zerop (length (terminal-pane-state-pending-escape state)))
                       output
                       (concatenate 'string
                                    (terminal-pane-state-pending-escape state)
                                    output)))
         (length (length combined))
         (index 0)
         (style (%clone-style (terminal-pane-state-ansi-style state)))
         (run-text "")
         (metadata (%normalize-existing-metadata line-metadata)))
    (setf (terminal-pane-state-pending-escape state) "")
    (labels ((flush-run ()
               (when (> (length run-text) 0)
                 (%append-pending-run! state run-text style metadata)
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
                       (%append-pending-run! state (string ch) style metadata))
                   (incf index))))
            ((char= ch #\Newline)
             (flush-run)
             (%append-line-internal state
                                    (terminal-pane-state-pending-output state)
                                    (terminal-pane-state-pending-segments state)
                                    (terminal-pane-state-pending-line-metadata state))
             (setf (terminal-pane-state-pending-output state) ""
                   (terminal-pane-state-pending-segments state) '()
                   (terminal-pane-state-pending-line-metadata state)
                   (%normalize-line-metadata :severity :info :style :plain))
             (incf index))
            ((char= ch #\Return)
             (incf index))
            (t
             (setf run-text (concatenate 'string run-text (string ch)))
             (incf index)))))
      (flush-run))
    (setf (terminal-pane-state-ansi-style state) style)))

(defun terminal-pane-append-line (state line &key (severity :info) (style :plain))
  "Append one completed output line."
  (check-type state terminal-pane-state)
  (%append-line-internal state
                         line
                         (%plain-line-segments (%normalize-line-text line))
                         (%normalize-line-metadata :severity severity :style style))
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

(defun terminal-pane-append-output (state output &key (severity :info) (style :plain))
  "Append raw output chunk, preserving trailing partial line state."
  (check-type state terminal-pane-state)
  (check-type output string)
  (unless (zerop (length output))
    (%consume-output! state
                      output
                      (%normalize-line-metadata :severity severity :style style)))
  (%recompute-search! state)
  (%refresh-status! state)
  state)

(defun terminal-pane-clear (state)
  "Clear all buffered terminal content."
  (check-type state terminal-pane-state)
  (setf (terminal-pane-state-lines state) '()
        (terminal-pane-state-line-segments state) '()
        (terminal-pane-state-line-metadata state) '()
        (terminal-pane-state-pending-output state) ""
        (terminal-pane-state-pending-segments state) '()
        (terminal-pane-state-pending-line-metadata state)
        (%normalize-line-metadata :severity :info :style :plain)
        (terminal-pane-state-pending-escape state) ""
        (terminal-pane-state-ansi-style state) (%make-terminal-pane-style)
        (terminal-pane-state-search-results state) '()
        (terminal-pane-state-search-selected-index state) -1
        (terminal-pane-state-last-copy state) ""
        (terminal-pane-state-stdin-events state) '()
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

(defun terminal-pane-visible-line-metadata (state &key (viewport-height 12) (include-pending t))
  "Return visible line metadata aligned with TERMINAL-PANE-VISIBLE-LINES."
  (check-type state terminal-pane-state)
  (check-type viewport-height (integer 1 *))
  (let ((metadata (%display-line-metadata state :include-pending include-pending)))
    (multiple-value-bind (start end)
        (%visible-window-range state viewport-height :include-pending include-pending)
      (if (>= start end)
          '()
          (subseq metadata start end)))))

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

(defun %stdin-capturable-event-p (event)
  (let ((key (ptui.core.events:key-event-key event)))
    (or (eq key :text)
        (member key '(:enter :ctrl-j :tab :backspace :delete :left :right)
                :test #'eq)
        (ptui.core.events:key-event-ctrlp event)
        (ptui.core.events:key-event-altp event))))

(defun %capture-stdin-event! (state event)
  (push event (terminal-pane-state-stdin-events state))
  state)

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
      ((%stdin-capturable-event-p event)
       (if (terminal-pane-stdin-capture-enabled-p state)
           (progn
             (%capture-stdin-event! state event)
             (list :action :stdin-captured
                   :state state
                   :event event
                   :policy (terminal-pane-state-stdin-capture-policy state)))
           (list :action :stdin-blocked
                 :state state
                 :event event
                 :policy (terminal-pane-state-stdin-capture-policy state)
                 :reason :consumer-policy)))
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
                 (styled (terminal-pane-visible-styled-lines state :viewport-height viewport-height))
                 (line-metadata (terminal-pane-visible-line-metadata
                                 state
                                 :viewport-height viewport-height)))
             (if visible
                 (loop for line in visible
                       for segments in styled
                       for metadata in line-metadata
                       collect (ptui.widgets.core:make-text-widget
                                line
                                :styled-segments segments
                                :metadata metadata))
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
