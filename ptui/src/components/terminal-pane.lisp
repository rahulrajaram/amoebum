(defpackage :ptui.components.terminal-pane
  (:use :cl)
  (:import-from :ptui.components.ansi-parser
                #:ansi-style
                #:make-ansi-style
                #:ansi-style-fg
                #:ansi-style-bg
                #:ansi-style-boldp
                #:ansi-style-italicp
                #:ansi-style-underlinep
                #:ansi-style-invertp
                #:ansi-style-dimp
                #:ansi-style-strikep
                #:clone-style
                #:style->attrs
                #:style->cell
                #:cell-style=
                #:apply-sgr!
                #:parse-sgr-codes
                #:ansi-index->color
                #:consume-ansi-output
                #:+esc-char+
                #:+ansi-palette+)
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
   #:terminal-pane-banner-text
   #:terminal-pane-lock-indicator-p
   #:terminal-pane-set-banner
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
   #:terminal-pane-supported-contexts
   #:terminal-pane-context-profile
   #:make-terminal-pane-state-for-context
   #:terminal-pane-handle-event
   #:make-terminal-pane-widget))

(in-package :ptui.components.terminal-pane)

(defparameter +terminal-pane-severity-order+
  '(:debug :info :warning :error :critical))

(defparameter +terminal-pane-context-defaults+
  '((:execution . (:title "execution"
                   :empty-message "[no execution output]"
                   :max-lines 2000
                   :stdin-capture-policy :enabled))
    (:logs . (:title "logs"
              :empty-message "[no logs]"
              :max-lines 5000
              :stdin-capture-policy :disabled))
    (:test-output . (:title "test output"
                    :empty-message "[no test output]"
                    :max-lines 4000
                    :stdin-capture-policy :disabled))))

(defun %normalize-terminal-pane-context (context)
  (cond
    ((member context '(:execution :normal-execution :run :runtime) :test #'eq)
     :execution)
    ((member context '(:logs :log) :test #'eq)
     :logs)
    ((member context '(:test-output :tests :test) :test #'eq)
     :test-output)
    (t
     (error "Unsupported terminal-pane context ~S. Expected one of ~S."
            context
            (mapcar #'car +terminal-pane-context-defaults+)))))

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
                  (ansi-style (make-ansi-style))
                  (max-lines 2000)
                  (scroll-offset 0)
                  (status :idle)
                  banner-text
                  (lock-indicator-p nil)
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
  (ansi-style (make-ansi-style) :type ansi-style)
  (max-lines 2000 :type fixnum)
  (scroll-offset 0 :type fixnum)
  (status :idle :type keyword)
  banner-text
  (lock-indicator-p nil :type boolean)
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

(defun %normalize-banner-text (value)
  (let* ((text (typecase value
                 (null "")
                 (string value)
                 (pathname (namestring value))
                 (symbol (symbol-name value))
                 (t (princ-to-string value))))
         (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
    (and (> (length trimmed) 0)
         trimmed)))

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

(defun %last-segment (segments)
  (car (last segments)))

(defun %append-segment (segments text style)
  (if (zerop (length text))
      segments
      (let* ((cell (style->cell style))
             (last (%last-segment segments)))
        (if (and last (cell-style= (second last) cell))
            (progn
              (setf (first last) (concatenate 'string (first last) text))
              segments)
            (nconc segments (list (list text cell)))))))

(defun %plain-line-segments (line)
  (if (zerop (length line))
      nil
      (list (list line (style->cell (make-ansi-style))))))

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
           line-segments
           lines)))
    (if (< (length normalized) (length lines))
        (nconc normalized
               (mapcar #'%plain-line-segments
                       (nthcdr (length normalized) lines)))
        normalized)))

(defun %normalize-metadata-list (line-metadata lines)
  (let ((incoming (copy-list line-metadata)))
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
                                   banner-text
                                   (lock-indicator-p nil)
                                   (empty-message "[no output]")
                                   (stdin-capture-policy :enabled))
  "Create a new terminal pane state with the given configuration.
Primary constructor — prefer this over raw struct access."
  (check-type title string)
  (check-type pending-output string)
  (check-type max-lines (integer 1 *))
  (check-type scroll-offset (integer 0 *))
  (check-type empty-message string)
  (let* ((normalized-lines (mapcar #'%normalize-line-text lines))
         (normalized-segments (%normalize-segments-list nil normalized-lines))
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
                                 :ansi-style (make-ansi-style)
                                 :max-lines max-lines
                                 :scroll-offset scroll-offset
                                 :status status
                                 :banner-text (%normalize-banner-text banner-text)
                                 :lock-indicator-p (not (null lock-indicator-p))
                                 :search-query ""
                                 :search-case-insensitive t
                                 :search-results nil
                                 :search-selected-index -1
                                 :last-copy ""
                                 :empty-message empty-message
                                 :stdin-capture-policy (%normalize-stdin-capture-policy
                                                        stdin-capture-policy)
                                 :stdin-events nil))))

(defun terminal-pane-supported-contexts ()
  (copy-list (mapcar #'car +terminal-pane-context-defaults+)))

(defun terminal-pane-context-profile (context)
  (copy-list
   (cdr (assoc (%normalize-terminal-pane-context context)
               +terminal-pane-context-defaults+
               :test #'eq))))

(defun make-terminal-pane-state-for-context
    (context
     &key
       (title nil title-supplied-p)
       (lines '())
       (line-metadata '())
       (pending-output "")
       (pending-line-metadata '(:severity :info :style :plain))
       (max-lines nil max-lines-supplied-p)
       (scroll-offset 0)
       (status :idle)
       (empty-message nil empty-message-supplied-p)
       (stdin-capture-policy nil stdin-capture-policy-supplied-p))
  "Construct TERMINAL-PANE-STATE with defaults tuned for CONTEXT."
  (let* ((profile (terminal-pane-context-profile context))
         (resolved-title (if title-supplied-p
                             title
                             (getf profile :title "terminal")))
         (resolved-empty-message (if empty-message-supplied-p
                                     empty-message
                                     (getf profile :empty-message "[no output]")))
         (resolved-max-lines (if max-lines-supplied-p
                                 max-lines
                                 (getf profile :max-lines 2000)))
         (resolved-stdin-capture-policy (if stdin-capture-policy-supplied-p
                                            stdin-capture-policy
                                            (getf profile :stdin-capture-policy
                                                  :enabled))))
    (make-terminal-pane-state
     :title resolved-title
     :lines lines
     :line-metadata line-metadata
     :pending-output pending-output
     :pending-line-metadata pending-line-metadata
     :max-lines resolved-max-lines
     :scroll-offset scroll-offset
     :status status
     :empty-message resolved-empty-message
     :stdin-capture-policy resolved-stdin-capture-policy)))

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

(defun terminal-pane-banner-text (state)
  (check-type state terminal-pane-state)
  (terminal-pane-state-banner-text state))

(defun terminal-pane-lock-indicator-p (state)
  (check-type state terminal-pane-state)
  (not (null (terminal-pane-state-lock-indicator-p state))))

(defun terminal-pane-set-banner (state &key text lock-indicator-p)
  (check-type state terminal-pane-state)
  (setf (terminal-pane-state-banner-text state)
        (%normalize-banner-text text)
        (terminal-pane-state-lock-indicator-p state)
        (not (null lock-indicator-p)))
  state)

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
  (let ((lines (copy-list (terminal-pane-state-lines state)))
        (pending (terminal-pane-state-pending-output state)))
    (if (and include-pending
             (stringp pending)
             (> (length pending) 0))
        (nconc lines (list pending))
        lines)))

(defun %display-line-segments (state &key (include-pending t))
  (check-type state terminal-pane-state)
  (let ((segments (copy-list (terminal-pane-state-line-segments state)))
        (pending (terminal-pane-state-pending-segments state)))
    (if (and include-pending
             (listp pending)
             pending)
        (nconc segments (list pending))
        segments)))

(defun %display-line-metadata (state &key (include-pending t))
  (check-type state terminal-pane-state)
  (let ((metadata (copy-list (terminal-pane-state-line-metadata state)))
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
    (ptui.util.scroll:max-scroll-offset line-count viewport-height)))

(defun %clamp-scroll-offset! (state &key viewport-height)
  (let* ((height (or viewport-height (length (%display-lines state))))
         (line-count (length (%display-lines state))))
    (setf (terminal-pane-state-scroll-offset state)
          (ptui.util.scroll:clamp-scroll-offset
           (terminal-pane-state-scroll-offset state)
           line-count
           (max 1 height))))
  state)

(defun %refresh-status! (state)
  (setf (terminal-pane-state-status state)
        (if (or (terminal-pane-state-lines state)
                (> (length (terminal-pane-state-pending-output state)) 0))
            :active
            :idle))
  state)

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
  (let ((metadata (%normalize-existing-metadata line-metadata)))
    (multiple-value-bind (updated-style updated-pending-escape)
        (consume-ansi-output
         output
         (clone-style (terminal-pane-state-ansi-style state))
         (terminal-pane-state-pending-escape state)
         :on-text (lambda (text style)
                    (%append-pending-run! state text style metadata))
         :on-newline (lambda ()
                       (%append-line-internal
                        state
                        (terminal-pane-state-pending-output state)
                        (terminal-pane-state-pending-segments state)
                        (terminal-pane-state-pending-line-metadata state))
                       (setf (terminal-pane-state-pending-output state) ""
                             (terminal-pane-state-pending-segments state) '()
                             (terminal-pane-state-pending-line-metadata state)
                             (%normalize-line-metadata :severity :info :style :plain)))
         :on-incomplete-escape (lambda (partial)
                                 (declare (ignore partial))))
      (setf (terminal-pane-state-ansi-style state) updated-style
            (terminal-pane-state-pending-escape state) updated-pending-escape))))

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
        (terminal-pane-state-ansi-style state) (make-ansi-style)
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
          nil
          (subseq lines start end)))))

(defun terminal-pane-visible-line-metadata (state &key (viewport-height 12) (include-pending t))
  "Return visible line metadata aligned with TERMINAL-PANE-VISIBLE-LINES."
  (check-type state terminal-pane-state)
  (check-type viewport-height (integer 1 *))
  (let ((metadata (%display-line-metadata state :include-pending include-pending)))
    (multiple-value-bind (start end)
        (%visible-window-range state viewport-height :include-pending include-pending)
      (if (>= start end)
          nil
          (subseq metadata start end)))))

(defun terminal-pane-visible-styled-lines (state &key (viewport-height 12) (include-pending t))
  "Return visible styled line segments aligned with TERMINAL-PANE-VISIBLE-LINES."
  (check-type state terminal-pane-state)
  (check-type viewport-height (integer 1 *))
  (let ((segments (%display-line-segments state :include-pending include-pending)))
    (multiple-value-bind (start end)
        (%visible-window-range state viewport-height :include-pending include-pending)
      (if (>= start end)
          nil
          (subseq segments start end)))))

(defun terminal-pane-scroll (state delta &key (viewport-height 12))
  "Adjust scroll offset by DELTA; positive values scroll back in history."
  (check-type state terminal-pane-state)
  (check-type delta integer)
  (check-type viewport-height (integer 1 *))
  (let* ((line-count (length (%display-lines state)))
         (next-offset
           (ptui.util.scroll:apply-scroll-delta
            (terminal-pane-state-scroll-offset state)
            delta
            line-count
            viewport-height)))
    (setf (terminal-pane-state-scroll-offset state)
          next-offset))
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
  "Apply key navigation event to STATE; returns (:action <keyword> :state STATE ...)."
  (check-type state terminal-pane-state)
  (check-type viewport-height (integer 1 *))
  (unless (typep event 'ptui.core.events:key-event)
    (return-from terminal-pane-handle-event
      (list :action :ignored :state state)))
  (let* ((key (ptui.core.events:key-event-key event))
         (text (ptui.core.events:key-event-text? event)))
    (cond
      ((multiple-value-bind (action delta)
           (ptui.util.scroll:key-scroll-action key :viewport-height viewport-height)
         (case action
           (:delta
            (terminal-pane-scroll state delta :viewport-height viewport-height)
            (return-from terminal-pane-handle-event
              (list :action :scrolled :delta delta :state state)))
           (:home
            (terminal-pane-scroll-home state :viewport-height viewport-height)
            (return-from terminal-pane-handle-event
              (list :action :scrolled-home :state state)))
           (:end
            (terminal-pane-scroll-end state)
            (return-from terminal-pane-handle-event
              (list :action :scrolled-end :state state)))
           (otherwise nil))))
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
         (banner-text (terminal-pane-state-banner-text state))
         (lock-indicator-p (terminal-pane-state-lock-indicator-p state))
         (banner (cond
                   ((and banner-text lock-indicator-p)
                    (format nil "~A [LOCK]" banner-text))
                   (banner-text
                    banner-text)
                   (lock-indicator-p
                    "[LOCK]")
                   (t
                    nil)))
         (partialp (> (length (terminal-pane-state-pending-output state)) 0))
         (matches (length (terminal-pane-state-search-results state))))
    (format nil "~A~@[ | ~A~] | ~A | ~D line~:P~@[ | +~D~]~:[~; | partial~]~:[~; | /~D~]"
            (terminal-pane-state-title state)
            banner
            (terminal-pane-state-status state)
            line-count
            (and (> offset 0) offset)
            partialp
            (> matches 0)
            matches)))

(defun make-terminal-pane-widget (state &key id key (borderp t) (padding 0) (viewport-height 12))
  "Build a composable UI element tree for rendering STATE in a layout."
  (check-type state terminal-pane-state)
  (check-type viewport-height (integer 1 *))
  (let* ((root-id (or id :terminal-pane))
         (status-widget
           (ptui.widgets.core:make-text-widget
            (%status-line state)
            :id (list root-id :status)))
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
                       for index from 0
                       collect (ptui.widgets.core:make-text-widget
                                line
                                :id (list root-id :line index)
                                :styled-segments segments
                                :metadata metadata))
                 (list (ptui.widgets.core:make-text-widget
                        (terminal-pane-state-empty-message state)
                        :id (list root-id :empty))))))
         (content (ptui.widgets.core:make-stack-widget
                   (append (list status-widget) line-widgets)
                   :id (list root-id :content)
                   :direction :column
                   :gap 0)))
    (ptui.widgets.core:make-box-widget content
                                       :id root-id
                                       :key key
                                       :padding padding
                                       :borderp borderp)))
