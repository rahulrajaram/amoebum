(in-package :amoebum)

(defparameter +fuzzy-picker-default-visible-count+ 10)
(defparameter +fuzzy-picker-default-batch-size+ 200)

(defstruct (fuzzy-ignore-rule
            (:constructor %make-fuzzy-ignore-rule
                (&key negated-p scanners)))
  (negated-p nil :type boolean)
  (scanners '() :type list))

(defstruct (fuzzy-match
            (:constructor %make-fuzzy-match
                (&key path score kind spans depth)))
  (path "" :type string)
  (score 0 :type integer)
  (kind :fuzzy :type keyword)
  (spans '() :type list)
  (depth 0 :type fixnum))

(defstruct (fuzzy-picker-state
            (:constructor make-fuzzy-picker-state
                (&key
                  (active-p nil)
                  (project-root nil)
                  (query "")
                  (token-start 0)
                  (token-end 0)
                  (files #())
                  (ignore-rules '())
                  (scan-cursor 0)
                  (scan-complete-p t)
                  (top-results '())
                  (selected-index 0)
                  (visible-count +fuzzy-picker-default-visible-count+)
                  (batch-size +fuzzy-picker-default-batch-size+)
                  (index-ready-p nil)
                  (context-label "@ file")
                  (empty-message "  [none] no matching files")
                  (generation 0))))
  (active-p nil :type boolean)
  (project-root nil)
  (query "" :type string)
  (token-start 0 :type fixnum)
  (token-end 0 :type fixnum)
  (files #() :type vector)
  (ignore-rules '() :type list)
  (scan-cursor 0 :type fixnum)
  (scan-complete-p t :type boolean)
  (top-results '() :type list)
  (selected-index 0 :type fixnum)
  (visible-count +fuzzy-picker-default-visible-count+ :type fixnum)
  (batch-size +fuzzy-picker-default-batch-size+ :type fixnum)
  (index-ready-p nil :type boolean)
  (context-label "@ file" :type string)
  (empty-message "  [none] no matching files" :type string)
  (generation 0 :type fixnum))

(defun %fuzzy-whitespace-char-p (char)
  (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=))

(defun %fuzzy-token-boundary-char-p (char)
  (or (null char)
      (%fuzzy-whitespace-char-p char)
      (find char "([" :test #'char=)))

(defun %fuzzy-default-root ()
  (let ((cfg (ignore-errors (current-config))))
    (or (and (config-p cfg)
             (config-project-root cfg))
        (ignore-errors (uiop:getcwd))
        *default-pathname-defaults*)))

(defun %fuzzy-resolve-root (root)
  (%resolve-search-root (or root (%fuzzy-default-root))))

(defun %fuzzy-path-depth (path)
  (count #\/ path))

(defun %fuzzy-range->spans (indexes)
  (if (null indexes)
      '()
      (let ((ordered (sort (copy-list indexes) #'<))
            (out '()))
        (let ((start (first ordered))
              (previous (first ordered)))
          (dolist (index (rest ordered))
            (if (= index (1+ previous))
                (setf previous index)
                (progn
                  (push (cons start (1+ previous)) out)
                  (setf start index
                        previous index))))
          (push (cons start (1+ previous)) out))
        (nreverse out))))

(defun %fuzzy-build-scanner (pattern)
  (ignore-errors
    (cl-ppcre:create-scanner (%glob->regex pattern))))

(defun %fuzzy-gitignore-pattern-scanners (pattern)
  (let* ((base (%normalize-slashes pattern))
         (anchored-p (and (> (length base) 0)
                          (char= (char base 0) #\/)))
         (trimmed (if anchored-p
                      (subseq base 1)
                      base))
         (directory-only-p (and (> (length trimmed) 0)
                                (char= (char trimmed (1- (length trimmed))) #\/)))
         (trimmed (if directory-only-p
                      (subseq trimmed 0 (1- (length trimmed)))
                      trimmed)))
    (when (plusp (length trimmed))
      (let* ((has-slash-p (position #\/ trimmed))
             (patterns
               (cond
                 (anchored-p
                  (list trimmed))
                 (has-slash-p
                  (list trimmed
                        (concatenate 'string "**/" trimmed)))
                 (t
                  (list trimmed
                        (concatenate 'string "**/" trimmed)))))
             (patterns
               (if directory-only-p
                   (mapcan (lambda (value)
                             (list value
                                   (concatenate 'string value "/**")))
                           patterns)
                   patterns)))
        (remove nil
                (mapcar #'%fuzzy-build-scanner
                        (remove-duplicates patterns :test #'string=)))))))

(defun %fuzzy-gitignore-line-rule (line)
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) (or line ""))))
    (cond
      ((zerop (length trimmed))
       nil)
      ((char= (char trimmed 0) #\#)
       nil)
      (t
       (let* ((negated-p (char= (char trimmed 0) #\!))
              (pattern (if negated-p
                           (subseq trimmed 1)
                           trimmed))
              (scanners (%fuzzy-gitignore-pattern-scanners pattern)))
         (when scanners
           (%make-fuzzy-ignore-rule :negated-p negated-p
                                    :scanners scanners)))))))

(defun %fuzzy-read-gitignore-rules (root)
  (let* ((gitignore-path (merge-pathnames #P".gitignore" root))
         (lines (if (probe-file gitignore-path)
                    (%read-lines gitignore-path)
                    '()))
         (rules '()))
    (dolist (line lines)
      (let ((rule (%fuzzy-gitignore-line-rule line)))
        (when rule
          (push rule rules))))
    (nreverse rules)))

(defun %fuzzy-ignore-rule-matches-p (rule relative-path)
  (loop for scanner in (fuzzy-ignore-rule-scanners rule)
        thereis (cl-ppcre:scan scanner relative-path)))

(defun %fuzzy-ignored-p (rules relative-path)
  (let ((ignored-p (uiop:string-prefix-p ".git/" relative-path)))
    (dolist (rule rules ignored-p)
      (when (%fuzzy-ignore-rule-matches-p rule relative-path)
        (setf ignored-p (not (fuzzy-ignore-rule-negated-p rule)))))))

(defun %fuzzy-index-files (root rules)
  (let ((seen (make-hash-table :test #'equal))
        (results '()))
    (dolist (candidate (%collect-files-recursive root))
      (when (%regular-file-p candidate)
        (let ((relative (%relative-path-text candidate root)))
          (unless (or (gethash relative seen)
                      (%fuzzy-ignored-p rules relative))
            (setf (gethash relative seen) t)
            (push relative results)))))
    (coerce (sort results #'string<) 'vector)))

(defun ensure-fuzzy-picker-index! (state &key root)
  (check-type state fuzzy-picker-state)
  (let* ((resolved-root (%fuzzy-resolve-root root))
         (root-key (%path-text resolved-root))
         (current-root (fuzzy-picker-state-project-root state)))
    (when (or (not (fuzzy-picker-state-index-ready-p state))
              (not (equal current-root root-key)))
      (let ((rules (%fuzzy-read-gitignore-rules resolved-root)))
        (setf (fuzzy-picker-state-project-root state) root-key
              (fuzzy-picker-state-ignore-rules state) rules
              (fuzzy-picker-state-files state) (%fuzzy-index-files resolved-root rules)
              (fuzzy-picker-state-index-ready-p state) t
              (fuzzy-picker-state-scan-cursor state) 0
              (fuzzy-picker-state-scan-complete-p state) nil
              (fuzzy-picker-state-top-results state) '()
              (fuzzy-picker-state-selected-index state) 0)))
    state))

(defun fuzzy-picker-extract-query (input)
  (when (stringp input)
    (let ((at-index (position #\@ input :from-end t)))
      (when at-index
        (let* ((prefix-char (and (> at-index 0)
                                 (char input (1- at-index))))
               (query (subseq input (1+ at-index))))
          (when (and (%fuzzy-token-boundary-char-p prefix-char)
                     (not (position-if #'%fuzzy-whitespace-char-p query)))
            (values query (1+ at-index) (length input))))))))

(defun %fuzzy-depth-weight (depth)
  (max 0 (- 240 (* 20 depth))))

(defun %fuzzy-subsequence-match (query candidate)
  (let ((indexes '())
        (cursor 0)
        (query-length (length query))
        (candidate-length (length candidate)))
    (loop for q-index from 0 below query-length do
      (let ((found nil))
        (loop for c-index from cursor below candidate-length do
          (when (char-equal (char query q-index) (char candidate c-index))
            (setf found c-index
                  cursor (1+ c-index))
            (return)))
        (unless found
          (return-from %fuzzy-subsequence-match (values nil nil)))
        (push found indexes)))
    (let ((ordered (nreverse indexes))
          (score 0)
          (previous -2))
      (dolist (index ordered)
        (incf score 20)
        (when (= index (1+ previous))
          (incf score 14))
        (when (or (zerop index)
                  (find (char candidate (1- index)) "/_-." :test #'char=))
          (incf score 8))
        (when (>= previous 0)
          (decf score (min 6 (max 0 (- index previous 1)))))
        (setf previous index))
      (when ordered
        (incf score (max 0 (- 40 (first ordered)))))
      (values score (%fuzzy-range->spans ordered)))))

(defun %fuzzy-score-path (query candidate)
  (let* ((normalized-query (string-downcase (or query "")))
         (normalized-candidate (string-downcase candidate))
         (query-length (length normalized-query))
         (depth (%fuzzy-path-depth candidate))
         (depth-weight (%fuzzy-depth-weight depth)))
    (cond
      ((zerop query-length)
       (%make-fuzzy-match :path candidate
                          :score (+ 50000 depth-weight (- (length candidate)))
                          :kind :all
                          :spans '()
                          :depth depth))
      ((string= normalized-query normalized-candidate)
       (%make-fuzzy-match :path candidate
                          :score (+ 400000 depth-weight (- (length candidate)))
                          :kind :exact
                          :spans (list (cons 0 (length candidate)))
                          :depth depth))
      ((uiop:string-prefix-p normalized-query normalized-candidate)
       (%make-fuzzy-match :path candidate
                          :score (+ 300000 depth-weight (- (length candidate)))
                          :kind :prefix
                          :spans (list (cons 0 query-length))
                          :depth depth))
      (t
       (let ((substring-index (search normalized-query normalized-candidate)))
         (cond
           (substring-index
            (%make-fuzzy-match :path candidate
                               :score (+ 200000
                                         depth-weight
                                         (* -5 substring-index)
                                         (- (length candidate)))
                               :kind :substring
                               :spans (list (cons substring-index
                                                  (+ substring-index query-length)))
                               :depth depth))
           (t
            (multiple-value-bind (subseq-score spans)
                (%fuzzy-subsequence-match normalized-query normalized-candidate)
              (when subseq-score
                (%make-fuzzy-match :path candidate
                                   :score (+ 100000
                                             depth-weight
                                             subseq-score
                                             (- (length candidate)))
                                   :kind :fuzzy
                                   :spans spans
                                   :depth depth))))))))))

(defun %fuzzy-result-better-p (left right)
  (cond
    ((> (fuzzy-match-score left) (fuzzy-match-score right))
     t)
    ((< (fuzzy-match-score left) (fuzzy-match-score right))
     nil)
    ((< (fuzzy-match-depth left) (fuzzy-match-depth right))
     t)
    ((> (fuzzy-match-depth left) (fuzzy-match-depth right))
     nil)
    (t
     (string< (fuzzy-match-path left)
              (fuzzy-match-path right)))))

(defun %fuzzy-insert-top-result (entry top limit)
  (let* ((combined (cons entry top))
         (sorted (sort combined #'%fuzzy-result-better-p)))
    (if (> (length sorted) limit)
        (subseq sorted 0 limit)
        sorted)))

(defun %fuzzy-reset-scan! (state)
  (setf (fuzzy-picker-state-scan-cursor state) 0
        (fuzzy-picker-state-scan-complete-p state) nil
        (fuzzy-picker-state-top-results state) '()
        (fuzzy-picker-state-selected-index state) 0)
  state)

(defun %fuzzy-selection-max-index (state)
  (max 0 (1- (length (fuzzy-picker-state-top-results state)))))

(defun %fuzzy-clamp-selection! (state)
  (setf (fuzzy-picker-state-selected-index state)
        (min (%fuzzy-selection-max-index state)
             (max 0 (fuzzy-picker-state-selected-index state))))
  state)

(defun fuzzy-picker-set-query! (state query token-start token-end)
  (check-type state fuzzy-picker-state)
  (let ((normalized (if (stringp query) query (princ-to-string query)))
        (safe-start (max 0 (or token-start 0)))
        (safe-end (max 0 (or token-end 0))))
    (unless (and (string= normalized (fuzzy-picker-state-query state))
                 (= safe-start (fuzzy-picker-state-token-start state))
                 (= safe-end (fuzzy-picker-state-token-end state)))
      (setf (fuzzy-picker-state-query state) normalized
            (fuzzy-picker-state-token-start state) safe-start
            (fuzzy-picker-state-token-end state) safe-end
            (fuzzy-picker-state-generation state)
            (1+ (fuzzy-picker-state-generation state)))
      (%fuzzy-reset-scan! state)))
  state)

(defun fuzzy-picker-deactivate! (state)
  (check-type state fuzzy-picker-state)
  (setf (fuzzy-picker-state-active-p state) nil
        (fuzzy-picker-state-query state) ""
        (fuzzy-picker-state-token-start state) 0
        (fuzzy-picker-state-token-end state) 0
        (fuzzy-picker-state-top-results state) '()
        (fuzzy-picker-state-selected-index state) 0
        (fuzzy-picker-state-scan-cursor state) 0
        (fuzzy-picker-state-scan-complete-p state) t)
  state)

(defun fuzzy-picker-sync-input! (state input &key root)
  (check-type state fuzzy-picker-state)
  (ensure-fuzzy-picker-index! state :root root)
  (multiple-value-bind (query token-start token-end)
      (fuzzy-picker-extract-query input)
    (if query
        (progn
          (setf (fuzzy-picker-state-active-p state) t)
          (fuzzy-picker-set-query! state query token-start token-end))
        (fuzzy-picker-deactivate! state)))
  state)

(defun fuzzy-picker-step! (state &key batch-size)
  (check-type state fuzzy-picker-state)
  (when (and (fuzzy-picker-state-active-p state)
             (not (fuzzy-picker-state-scan-complete-p state)))
    (let* ((files (fuzzy-picker-state-files state))
           (total (length files))
           (cursor (fuzzy-picker-state-scan-cursor state))
           (limit (max 1 (or batch-size
                             (fuzzy-picker-state-batch-size state))))
           (processed 0)
           (top (fuzzy-picker-state-top-results state))
           (top-limit (max 1 (fuzzy-picker-state-visible-count state)))
           (query (fuzzy-picker-state-query state)))
      (loop while (and (< cursor total)
                       (< processed limit)) do
        (let* ((candidate (aref files cursor))
               (match (%fuzzy-score-path query candidate)))
          (when match
            (setf top (%fuzzy-insert-top-result match top top-limit))))
        (incf cursor)
        (incf processed))
      (setf (fuzzy-picker-state-scan-cursor state) cursor
            (fuzzy-picker-state-scan-complete-p state) (>= cursor total)
            (fuzzy-picker-state-top-results state) top)
      (%fuzzy-clamp-selection! state)))
  state)

(defun fuzzy-picker-move-selection! (state delta)
  (check-type state fuzzy-picker-state)
  (let* ((current (fuzzy-picker-state-selected-index state))
         (target (+ current (or delta 0))))
    (setf (fuzzy-picker-state-selected-index state)
          (min (%fuzzy-selection-max-index state)
               (max 0 target))))
  state)

(defun fuzzy-picker-home-selection! (state)
  (check-type state fuzzy-picker-state)
  (setf (fuzzy-picker-state-selected-index state) 0)
  state)

(defun fuzzy-picker-end-selection! (state)
  (check-type state fuzzy-picker-state)
  (setf (fuzzy-picker-state-selected-index state)
        (%fuzzy-selection-max-index state))
  state)

(defun fuzzy-picker-selected-match (state)
  (check-type state fuzzy-picker-state)
  (let ((results (fuzzy-picker-state-top-results state))
        (index (fuzzy-picker-state-selected-index state)))
    (and (>= index 0)
         (< index (length results))
         (nth index results))))

(defun fuzzy-picker-selected-path (state)
  (let ((match (fuzzy-picker-selected-match state)))
    (and match
         (fuzzy-match-path match))))

(defun fuzzy-picker-apply-selection (input state replacement)
  (check-type state fuzzy-picker-state)
  (let* ((source (if (stringp input) input (princ-to-string input)))
         (length* (length source))
         (start (min length* (max 0 (fuzzy-picker-state-token-start state))))
         (end (min length* (max start (fuzzy-picker-state-token-end state))))
         (prefix (subseq source 0 start))
         (suffix (subseq source end)))
    (concatenate 'string
                 prefix
                 (or replacement "")
                 suffix)))

(defun fuzzy-picker-render-key (state)
  (check-type state fuzzy-picker-state)
  (if (not (fuzzy-picker-state-active-p state))
      :inactive
      (list :active
            (fuzzy-picker-state-query state)
            (fuzzy-picker-state-selected-index state)
            (fuzzy-picker-state-scan-cursor state)
            (fuzzy-picker-state-scan-complete-p state)
            (mapcar (lambda (match)
                      (list (fuzzy-match-path match)
                            (fuzzy-match-score match)
                            (fuzzy-match-kind match)
                            (fuzzy-match-spans match)))
                    (fuzzy-picker-state-top-results state)))))

(defun %fuzzy-styled-segments (path spans &key selected-p kind)
  (let ((segments
          (list (cons (if selected-p "> " "  ")
                      (if selected-p :context-green :meta))
                (cons (format nil "[~A] "
                              (string-downcase
                               (symbol-name (or kind :fuzzy))))
                      :meta)))
        (cursor 0)
        (base-role (if selected-p :assistant :meta)))
    (dolist (span spans)
      (let ((start (max cursor (car span)))
            (end (min (length path) (cdr span))))
        (when (> start cursor)
          (push (cons (subseq path cursor start) base-role) segments))
        (when (> end start)
          (push (cons (subseq path start end) :context-yellow) segments))
        (setf cursor end)))
    (when (< cursor (length path))
      (push (cons (subseq path cursor) base-role) segments))
    (nreverse segments)))

(defun %fuzzy-segments-text (segments)
  (with-output-to-string (out)
    (dolist (segment segments)
      (write-string (car segment) out))))

(defun %fuzzy-row-element (match index selected-index)
  (let* ((selected-p (= index selected-index))
         (segments (%fuzzy-styled-segments (fuzzy-match-path match)
                                           (fuzzy-match-spans match)
                                           :selected-p selected-p
                                           :kind (fuzzy-match-kind match))))
    (ptui.ui.elements:make-element
     :text
     :id (list :fuzzy-picker-row index (fuzzy-match-path match))
     :props (list :text (%fuzzy-segments-text segments)
                  :role (if selected-p :assistant :meta)
                  :styled-segments segments)
     :children '())))

(defun %fuzzy-header-element (state)
  (let* ((label (or (fuzzy-picker-state-context-label state) "@ file"))
         (query (fuzzy-picker-state-query state))
         (results (fuzzy-picker-state-top-results state))
         (scan-complete-p (fuzzy-picker-state-scan-complete-p state))
         (text (format nil "~A: ~A (~D~A)"
                       label
                       query
                       (length results)
                       (if scan-complete-p "" "+"))))
    (ptui.ui.elements:make-element
     :text
     :id :fuzzy-picker-header
     :props (list :text text
                  :role :meta)
     :children '())))

(defun %fuzzy-empty-element (state)
  (ptui.ui.elements:make-element
   :text
   :id :fuzzy-picker-empty
   :props (list :text (or (fuzzy-picker-state-empty-message state)
                          "  [none] no matching files")
                :role :meta)
   :children '()))

(defun %fuzzy-render-children (state)
  (let ((results (fuzzy-picker-state-top-results state)))
    (if (null results)
        (list (%fuzzy-empty-element state))
        (loop for match in results
              for index from 0
              collect (%fuzzy-row-element match
                                          index
                                          (fuzzy-picker-state-selected-index state))))))

(ptui.widgets.defwidget:defwidget make-fuzzy-picker-widget (state)
  (:memoize nil)
  (box
   (vstack
    (%fuzzy-header-element state)
    (map-widget #'identity (%fuzzy-render-children state)))
   :id :fuzzy-picker
   :border t))
