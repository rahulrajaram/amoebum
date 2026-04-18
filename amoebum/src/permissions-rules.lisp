(in-package :amoebum)

(defun %path-kind (pattern)
  (let* ((raw (%path-string pattern))
         (normalized (and raw
                          (%normalize-slashes (%trim-path-whitespace raw)))))
    (cond
      ((or (null normalized)
           (string= normalized "")
           (member normalized '("*" "**" "**/*" "/*") :test #'string=))
       :wildcard)
      ((and (> (length normalized) 0)
            (char= (char normalized (1- (length normalized))) #\/)
            (not (%contains-glob-char-p normalized)))
       :directory)
      ((%contains-glob-char-p normalized) :glob)
      (t :exact))))

(defun %regex-escape-char (char stream)
  (when (find char "\\.^$|()[]{}+?" :test #'char=)
    (write-char #\\ stream))
  (write-char char stream))

(defun %glob->regex (pattern)
  (let* ((source (or (%normalize-pattern-path pattern) ""))
         (len (length source)))
    (with-output-to-string (stream)
      (write-char #\^ stream)
      (loop for i from 0 below len do
            (let ((ch (char source i)))
              (cond
                ((char= ch #\*)
                 (if (and (< (1+ i) len)
                          (char= (char source (1+ i)) #\*))
                     (progn
                       (incf i)
                       (if (and (< (1+ i) len)
                                (char= (char source (1+ i)) #\/))
                           (progn
                             (incf i)
                             (write-string "(?:[^/]+/)*" stream))
                           (write-string ".*" stream)))
                     (write-string "[^/]*" stream)))
                ((char= ch #\?)
                 (write-string "[^/]" stream))
                ((char= ch #\[)
                 (let ((close (position #\] source :start (1+ i))))
                   (if close
                       (progn
                         (write-string (subseq source i (1+ close)) stream)
                         (setf i close))
                       (%regex-escape-char ch stream))))
                ((char= ch #\{)
                 (let ((close (position #\} source :start (1+ i))))
                   (if close
                       (let ((inner (subseq source (1+ i) close))
                             (alternatives ()))
                         (setf alternatives
                               (uiop:split-string inner :separator ","))
                         (write-string "(?:" stream)
                         (loop for alt in alternatives
                               for idx from 0 do
                                 (when (> idx 0)
                                   (write-char #\| stream))
                                 (write-string (cl-ppcre:quote-meta-chars alt) stream))
                         (write-char #\) stream)
                         (setf i close))
                       (%regex-escape-char ch stream))))
                (t
                 (%regex-escape-char ch stream)))))
      (write-char #\$ stream))))

(defun %command-glob->regex (pattern)
  (let* ((source (or (%normalize-permission-command pattern) ""))
         (len (length source)))
    (with-output-to-string (stream)
      (write-char #\^ stream)
      (loop for i from 0 below len do
            (let ((ch (char source i)))
              (cond
                ((char= ch #\*)
                 (write-string ".*" stream))
                ((char= ch #\?)
                 (write-char #\. stream))
                ((char= ch #\[)
                 (let ((close (position #\] source :start (1+ i))))
                   (if close
                       (progn
                         (write-string (subseq source i (1+ close)) stream)
                         (setf i close))
                       (%regex-escape-char ch stream))))
                ((char= ch #\{)
                 (let ((close (position #\} source :start (1+ i))))
                   (if close
                       (let* ((inner (subseq source (1+ i) close))
                              (alternatives (uiop:split-string inner :separator ",")))
                         (write-string "(?:" stream)
                         (loop for alt in alternatives
                               for idx from 0 do
                                 (when (> idx 0)
                                   (write-char #\| stream))
                                 (write-string (cl-ppcre:quote-meta-chars alt) stream))
                         (write-char #\) stream)
                         (setf i close))
                       (%regex-escape-char ch stream))))
                (t
                 (%regex-escape-char ch stream)))))
      (write-char #\$ stream))))

(defun %path-under-directory-p (path directory-pattern)
  (let* ((path* (%normalize-path path))
         (dir* (%trim-trailing-slash (%normalize-pattern-path directory-pattern)))
         (prefix (if (string= dir* "/")
                     "/"
                     (concatenate 'string dir* "/")))
         (path-folded (%fold-path-identity-text path*))
         (dir-folded (%fold-path-identity-text dir*))
         (prefix-folded (and dir-folded
                             (if (string= dir-folded "/")
                                 "/"
                                 (concatenate 'string dir-folded "/")))))
    (and path*
         dir*
         (or (%path-identity-equal-p path* dir*)
             (uiop:string-prefix-p prefix path*)
             (and path-folded
                  prefix-folded
                  (uiop:string-prefix-p prefix-folded path-folded))))))

(defun %path-matches-pattern-p (path pattern &key (path-normalized-p nil))
  (let ((kind (%path-kind pattern))
        (candidate (if path-normalized-p
                       path
                       (%normalize-path path))))
    (and candidate
         (case kind
           (:wildcard t)
           (:exact (%path-identity-equal-p candidate
                                           (%normalize-pattern-path pattern)))
           (:directory (%path-under-directory-p candidate pattern))
           (:glob (cl-ppcre:scan (%glob->regex pattern) candidate))
           (otherwise nil)))))

(defun %command-matches-pattern-p (command pattern &key (command-normalized-p nil))
  (labels ((%pipeline-segment-candidates (value)
             (let ((canonical (canonicalize-permission-command value)))
               (when canonical
                 (remove-duplicates
                  (loop for segment in (command-canonical-form-commands canonical)
                        for text = (%canonical-argv-string segment)
                        when (and (stringp text)
                                  (> (length text) 0))
                          collect text)
                  :test #'string=))))
           (%matches-single-candidate-p (candidate normalized-pattern kind)
             (case kind
               (:wildcard t)
               (:exact (string= candidate normalized-pattern))
               (:prefix (uiop:string-prefix-p
                         (subseq normalized-pattern 0 (1- (length normalized-pattern)))
                         candidate))
               (:glob (cl-ppcre:scan (%command-glob->regex normalized-pattern)
                                     candidate))
               (:regex (let ((regex-body (%command-regex-body normalized-pattern)))
                         (and regex-body
                              (cl-ppcre:scan regex-body candidate))))
               (otherwise nil))))
    (let* ((candidate (if command-normalized-p
                          command
                          (%normalize-permission-command command)))
           (normalized-pattern (%normalize-permission-command pattern))
           (kind (%command-pattern-kind normalized-pattern))
           (candidates (and candidate
                            (remove-duplicates
                             (cons candidate
                                   (%pipeline-segment-candidates candidate))
                             :test #'string=))))
      (and candidates
           (loop for candidate-text in candidates
                 thereis (%matches-single-candidate-p candidate-text
                                                     normalized-pattern
                                                     kind))))))

(defun %tool-matches-rule-p (tool rule-tool)
  (let ((tool-name (%tool-name tool))
        (rule-tool-name (%tool-name rule-tool)))
    (or (null rule-tool-name)
        (and tool-name (string= tool-name rule-tool-name)))))

(defun %rule-matches-p (rule tool path command canonical-command)
  (and (%tool-matches-rule-p tool (permission-rule-tool rule))
       (let ((rule-path (permission-rule-path rule)))
         (if rule-path
             (%path-matches-pattern-p path rule-path :path-normalized-p t)
             t))
       (let ((rule-command (permission-rule-command rule)))
         (if rule-command
             (%command-matches-pattern-p command
                                         rule-command
                                         :command-normalized-p t)
             t))
       (%rule-arguments-match-p rule canonical-command)))

(defun %path-specificity-score (rule)
  (case (%path-kind (permission-rule-path rule))
    (:exact 300)
    (:glob 200)
    (:directory 100)
    (:wildcard 0)
    (otherwise 0)))

(defun %command-specificity-score (rule)
  (case (%command-pattern-kind (permission-rule-command rule))
    (:exact 300)
    (:prefix 200)
    (:glob 150)
    (:regex 100)
    (:wildcard 0)
    (otherwise 0)))

(defun %argument-pattern-specificity-score (argument-pattern)
  (multiple-value-bind (_selector matcher)
      (%argument-pattern-components argument-pattern)
    (declare (ignore _selector))
    (case (%command-pattern-kind matcher)
      (:exact 70)
      (:prefix 50)
      (:glob 40)
      (:regex 30)
      (:wildcard 10)
      (otherwise 0))))

(defun %argument-specificity-score (rule)
  (let ((arguments (permission-rule-arguments rule)))
    (if arguments
        (+ 25
           (loop for argument-pattern in arguments
                 sum (%argument-pattern-specificity-score argument-pattern)))
        0)))

(defun %specificity-score (rule)
  (+ (%path-specificity-score rule)
     (%command-specificity-score rule)
     (%argument-specificity-score rule)))

(defun %scope-score (rule)
  (case (permission-rule-source rule)
    (:session 30)
    (:extension 20)
    (:project 10)
    (:global 0)
    (otherwise 0)))

(defun %deny-rule-p (rule)
  (eq (permission-rule-effect rule) :deny))

(defun %better-rule-p (candidate best)
  (cond
    ((null best) t)
    ((> (%specificity-score candidate) (%specificity-score best)) t)
    ((< (%specificity-score candidate) (%specificity-score best)) nil)
    ((> (%scope-score candidate) (%scope-score best)) t)
    ((< (%scope-score candidate) (%scope-score best)) nil)
    ((and (%deny-rule-p candidate)
          (not (%deny-rule-p best)))
     t)
    (t nil)))

(defun %permission-rule-id (rule)
  (or (permission-rule-id rule)
      (setf (permission-rule-id rule)
            (format nil "~A-~8,'0X"
                    (string-downcase
                     (symbol-name (or (permission-rule-source rule) :unknown)))
                    (ldb (byte 32 0)
                         (sxhash
                          (list (permission-rule-effect rule)
                                (permission-rule-path rule)
                                (permission-rule-command rule)
                                (permission-rule-tool rule)
                                (permission-rule-arguments rule)
                                (permission-rule-source rule))))))))

(defun %permission-cache-note-invalidation (reason)
  (incf *permission-cache-invalidations*)
  (push (list :timestamp (get-universal-time)
              :reason reason
              :rules-version *permission-rules-version*)
        *permission-cache-invalidation-events*)
  (when (> (length *permission-cache-invalidation-events*)
           *permission-cache-invalidation-events-limit*)
    (setf *permission-cache-invalidation-events*
          (subseq *permission-cache-invalidation-events*
                  0
                  *permission-cache-invalidation-events-limit*))))

(defun clear-permission-cache (&key (reason :manual))
  (clrhash *permission-evaluation-cache*)
  (%permission-cache-note-invalidation reason)
  t)

(defun permission-cache-metrics ()
  (list :hits *permission-cache-hits*
        :misses *permission-cache-misses*
        :invalidations *permission-cache-invalidations*
        :rules-version *permission-rules-version*
        :entries (hash-table-count *permission-evaluation-cache*)))

(defun permission-cache-invalidation-events (&key (limit 20))
  (subseq *permission-cache-invalidation-events*
          0
          (min (max 0 limit)
               (length *permission-cache-invalidation-events*))))

(defun clear-permission-rules ()
  (setf *permission-rules* nil)
  (incf *permission-rules-version*)
  (clear-permission-cache :reason :rules-cleared))

(defun %validate-command-pattern (command-pattern)
  (let ((normalized (%normalize-permission-command command-pattern)))
    (when normalized
      (let ((kind (%command-pattern-kind normalized)))
        (when (eq kind :regex)
          (let ((regex-body (%command-regex-body normalized)))
            (when (or (null regex-body)
                      (string= (%trim-command-whitespace regex-body) ""))
              (error "Command regex pattern must not be empty, got ~S."
                     command-pattern))
            (handler-case
                (cl-ppcre:create-scanner regex-body)
              (error (condition)
                (error "Invalid command regex pattern ~S: ~A"
                       command-pattern
                       condition)))))))
    normalized))

(defun add-permission-rule (&key effect path command tool arguments (source :project))
  (unless (member effect '(:allow :deny) :test #'eq)
    (error "Permission rule EFFECT must be :allow or :deny, got ~S." effect))
  (let ((rule (make-permission-rule :effect effect
                                    :path path
                                    :command (%validate-command-pattern command)
                                    :tool tool
                                    :arguments (%normalize-rule-arguments arguments)
                                    :source source)))
    (%permission-rule-id rule)
    (push rule *permission-rules*)
    (incf *permission-rules-version*)
    (clear-permission-cache :reason :rule-added)
    rule))

(defun %effective-permission-rules (rules)
  (cond
    ((policy-rule-registry-p rules)
     (policy-rule-registry-composed-rules rules))
    ((policy-rule-table-p rules)
     (copy-list (policy-rule-table-rules rules)))
    (t rules)))

(defun %permission-cache-key (phase tool path command rules)
  (when (eq rules *permission-rules*)
    (list :phase phase
          :tool (%tool-name tool)
          :path path
          :command command
          :rules-version *permission-rules-version*)))

(defun %rule-trace-entry (phase rule)
  (when rule
    (list :phase phase
          :matched-rule-id (%permission-rule-id rule)
          :specificity (%specificity-score rule)
          :effect (permission-rule-effect rule)
          :source (permission-rule-source rule)
          :tool (%tool-name (permission-rule-tool rule))
          :path (permission-rule-path rule)
          :command (permission-rule-command rule)
          :arguments (permission-rule-arguments rule))))

(defun %evaluate-rule-phase (phase tool path command rules &key canonical-command)
  (let* ((effective-rules (%effective-permission-rules rules))
         (cache-key (%permission-cache-key phase tool path command rules))
         (cached (and cache-key
                      (multiple-value-list
                       (gethash cache-key *permission-evaluation-cache*)))))
    (when (and cached (second cached))
      (incf *permission-cache-hits*)
      (let* ((entry (first cached))
             (rule (getf entry :rule))
             (trace (append (getf entry :trace)
                            (list :cache :hit))))
        (return-from %evaluate-rule-phase
          (values (and rule (permission-rule-effect rule))
                  trace
                  rule
                  :hit))))
    (incf *permission-cache-misses*)
    (let ((best nil))
      (when (or path command)
        (dolist (rule effective-rules)
          (when (%rule-matches-p rule tool path command canonical-command)
            (when (%better-rule-p rule best)
              (setf best rule)))))
      (let ((trace (append (%rule-trace-entry phase best)
                           (list :cache :miss))))
        (when cache-key
          (setf (gethash cache-key *permission-evaluation-cache*)
                (list :rule best :trace (%rule-trace-entry phase best))))
        (values (and best (permission-rule-effect best))
                trace
                best
                :miss)))))

(defun evaluate-path-permission (&key tool path (rules *permission-rules*) (with-trace-p nil))
  (let ((normalized-path (%normalize-path path)))
    (multiple-value-bind (decision trace)
        (%evaluate-rule-phase :path tool normalized-path nil rules)
      (if with-trace-p
          (values decision trace)
          decision))))

(defun evaluate-command-permission (&key tool command path (rules *permission-rules*)
                                      canonical-command
                                      (with-trace-p nil))
  (let* ((canonical (or canonical-command
                        (and command
                             (canonicalize-permission-command command))))
         (policy-command (%policy-command-text canonical command))
         (normalized-command (%normalize-permission-command policy-command))
         (normalized-path (and path (%normalize-path path))))
    (multiple-value-bind (decision trace)
        (%evaluate-rule-phase :command
                              tool
                              normalized-path
                              normalized-command
                              rules
                              :canonical-command canonical)
      (if with-trace-p
          (values decision trace)
          decision))))
