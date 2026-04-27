(in-package :amoebum)

(defun %parse-integer-safe (text)
  (ignore-errors
    (parse-integer text)))

(defun %parse-history-iso-date (text)
  (when (and (= (length text) 10)
             (char= (char text 4) #\-)
             (char= (char text 7) #\-))
    (let ((year (%parse-integer-safe (subseq text 0 4)))
          (month (%parse-integer-safe (subseq text 5 7)))
          (day (%parse-integer-safe (subseq text 8 10))))
      (when (and year month day)
        (ignore-errors
          (encode-universal-time 0 0 0 day month year))))))

(defun %parse-history-iso-datetime (text)
  (let ((value (if (and (plusp (length text))
                        (char-equal (char text (1- (length text))) #\Z))
                   (subseq text 0 (1- (length text)))
                   text)))
    (when (and (>= (length value) 16)
               (char= (char value 4) #\-)
               (char= (char value 7) #\-)
               (member (char value 10) '(#\T #\Space) :test #'char=)
               (char= (char value 13) #\:))
      (let ((year (%parse-integer-safe (subseq value 0 4)))
            (month (%parse-integer-safe (subseq value 5 7)))
            (day (%parse-integer-safe (subseq value 8 10)))
            (hour (%parse-integer-safe (subseq value 11 13)))
            (minute (%parse-integer-safe (subseq value 14 16)))
            (second (if (and (>= (length value) 19)
                             (char= (char value 16) #\:))
                        (%parse-integer-safe (subseq value 17 19))
                        0)))
        (when (and year month day hour minute second)
          (ignore-errors
            (if (and (plusp (length text))
                     (char-equal (char text (1- (length text))) #\Z))
                (encode-universal-time second minute hour day month year 0)
                (encode-universal-time second minute hour day month year))))))))

(defun parse-history-timestamp (value)
  (cond
    ((null value)
     nil)
    ((integerp value)
     value)
    ((stringp value)
     (let ((trimmed (%conversation-trim value)))
       (or (%parse-integer-safe trimmed)
           (%parse-history-iso-datetime trimmed)
           (%parse-history-iso-date trimmed))))
    (t
     (parse-history-timestamp (princ-to-string value)))))

(defun %history-normalize-role (role)
  (let ((normalized
          (string-downcase
           (cond
             ((null role) "")
             ((stringp role) role)
             ((symbolp role) (symbol-name role))
             (t (princ-to-string role))))))
    (if (member normalized '("system" "user" "assistant" "tool") :test #'string=)
        normalized
        nil)))

(defun %history-normalize-tool-name (tool-name)
  (when tool-name
    (let ((result (normalize-name tool-name)))
      (when (plusp (length result))
        result))))

(defun %history-tool-name (entry)
  (let ((name (conversation-history-entry-name entry)))
    (and name
         (plusp (length (%conversation-trim name)))
         (string-downcase (%conversation-trim name)))))

(defun %history-content-score (query content)
  (let ((needle (%conversation-trim query))
        (haystack (if (stringp content) content "")))
    (if (zerop (length needle))
        (values t 0)
        (let* ((match-index (search needle haystack :test #'char-equal))
               (prefix-match-p (and match-index (zerop match-index)))
               (exact-match-p (and match-index
                                   (= (length needle)
                                      (length (%conversation-trim haystack))))))
          (if (null match-index)
              (values nil 0)
              (values t
                      (+ 500
                         (if exact-match-p 500 0)
                         (if prefix-match-p 250 0)
                         (max 0 (- 200 (min 200 match-index)))
                         (min 100 (length needle)))))))))

(defun %history-entry-matches-tool-p (entry tool-filter)
  (if (null tool-filter)
      t
      (let ((entry-tool (%history-tool-name entry)))
        (and entry-tool
             (search tool-filter entry-tool :test #'char-equal)))))

(defun %history-search-result-better-p (left right)
  (let* ((left-score (conversation-history-search-result-score left))
         (right-score (conversation-history-search-result-score right)))
    (cond
      ((> left-score right-score)
       t)
      ((< left-score right-score)
       nil)
      (t
       (let* ((left-entry (conversation-history-search-result-entry left))
              (right-entry (conversation-history-search-result-entry right))
              (left-ts (conversation-history-entry-timestamp left-entry))
              (right-ts (conversation-history-entry-timestamp right-entry)))
         (cond
           ((> left-ts right-ts)
            t)
           ((< left-ts right-ts)
            nil)
           (t
            (> (conversation-history-search-result-index left)
               (conversation-history-search-result-index right)))))))))

(defstruct (history-search-context
            (:constructor %make-history-search-context
                (&key resolved-role resolved-tool since-ts until-ts
                      max-results entries)))
  resolved-role
  resolved-tool
  since-ts
  until-ts
  max-results
  entries)

(defun %history-search-context (conversation role tool since until limit)
  (%make-history-search-context
   :resolved-role (%history-normalize-role role)
   :resolved-tool (%history-normalize-tool-name tool)
   :since-ts (parse-history-timestamp since)
   :until-ts (parse-history-timestamp until)
   :max-results (if (and (integerp limit) (> limit 0)) limit 20)
   :entries (coerce (conversation-state-entries conversation) 'vector)))

(defun %history-search-valid-range-p (context)
  (let ((since-ts (history-search-context-since-ts context))
        (until-ts (history-search-context-until-ts context)))
    (not (and since-ts until-ts (> since-ts until-ts)))))

(defun %history-entry-time-match-p (entry context)
  (let ((stamp (conversation-history-entry-timestamp entry)))
    (and (or (null (history-search-context-since-ts context))
             (and (integerp stamp)
                  (>= stamp (history-search-context-since-ts context))))
         (or (null (history-search-context-until-ts context))
             (and (integerp stamp)
                  (<= stamp (history-search-context-until-ts context)))))))

(defun %history-entry-role-match-p (entry context)
  (let ((resolved-role (history-search-context-resolved-role context))
        (entry-role (string-downcase
                     (or (conversation-history-entry-role entry)
                         "assistant"))))
    (or (null resolved-role)
        (string= resolved-role entry-role))))

(defun %history-search-entry-result (entries index query context)
  (let ((entry (aref entries index)))
    (when (and (%history-entry-role-match-p entry context)
               (%history-entry-time-match-p entry context)
               (%history-entry-matches-tool-p entry
                                              (history-search-context-resolved-tool context)))
      (multiple-value-bind (content-match-p content-score)
          (%history-content-score query (conversation-history-entry-content entry))
        (when content-match-p
          (make-conversation-history-search-result
           :index index
           :score (+ content-score
                     (if (history-search-context-resolved-role context) 60 0)
                     (if (history-search-context-resolved-tool context) 80 0))
           :entry (%conversation-copy-entry entry)
           :before (and (> index 0)
                        (%conversation-copy-entry (aref entries (1- index))))
           :after (and (< (1+ index) (length entries))
                       (%conversation-copy-entry (aref entries (1+ index))))))))))

(defun %history-search-collect-matches (query context)
  (let ((entries (history-search-context-entries context))
        (matches '()))
    (loop for index from 0 below (length entries) do
      (let ((result (%history-search-entry-result entries index query context)))
        (when result
          (push result matches))))
    matches))

(defun history-search (conversation
                       &key query role tool since until (limit 20))
  (check-type conversation conversation-state)
  (let ((context (%history-search-context conversation role tool since until limit)))
    (unless (%history-search-valid-range-p context)
      (return-from history-search '()))
    (let ((ranked (sort (%history-search-collect-matches query context)
                        #'%history-search-result-better-p)))
      (subseq ranked
              0
              (min (history-search-context-max-results context)
                   (length ranked))))))

(defun conversation-search-history (conversation
                                    &key query role tool since until (limit 20))
  (history-search conversation
                  :query query
                  :role role
                  :tool tool
                  :since since
                  :until until
                  :limit limit))

(defun format-history-timestamp (timestamp)
  (if (integerp timestamp)
      (multiple-value-bind (second minute hour day month year)
          (decode-universal-time timestamp)
        (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                year month day hour minute second))
      "unknown-time"))

(defun format-history-entry-line (entry)
  (check-type entry conversation-history-entry)
  (let* ((role (string-upcase (or (conversation-history-entry-role entry) "assistant")))
         (timestamp (format-history-timestamp
                     (conversation-history-entry-timestamp entry)))
         (content (%conversation-trim (conversation-history-entry-content entry))))
    (format nil "[~A] ~A: ~A"
            timestamp
            role
            (if (%conversation-blank-p content)
                "(empty)"
                content))))
