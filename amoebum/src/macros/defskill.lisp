(in-package :amoebum)

;;; %slash-trim, %slash-blank-p, %normalize-command-name, %command-name-keyword,
;;; register-slash-command, and struct definitions are in src/commands-base.lisp
;;; (loaded before this file).

;;; Forward declarations for pipeline.lisp (loaded after this file).
;;; execute-tool and make-amoebum-context are resolved at call time.

(defparameter *skill-registry* (make-hash-table :test #'equal))
(defparameter *skill-definition-counter* 0)
(defparameter *skill-review-analyzer* nil)
(defparameter *skill-review-schema-version* "amoebum.review.v1")

(defstruct (skill-argument
            (:constructor make-skill-argument
                (&key name
                 variable
                 (type :string)
                 (required-p t)
                 default
                 (default-supplied-p nil)
                 choices
                 (greedy-p nil)
                 prompt
                 description
                 completer)))
  name
  variable
  (type :string)
  (required-p t :type boolean)
  default
  (default-supplied-p nil :type boolean)
  choices
  (greedy-p nil :type boolean)
  prompt
  description
  completer)

(defstruct (skill-metadata
            (:constructor make-skill-metadata
                (&key name
                 description
                 usage
                 (aliases '())
                 (category :general)
                 keybinding
                 (arguments '())
                 handler
                 completer
                 source-file
                 source-line
                 defined-at)))
  name
  description
  usage
  (aliases '() :type list)
  (category :general)
  keybinding
  (arguments '() :type list)
  handler
  completer
  source-file
  source-line
  defined-at)

(defun %skill-now-ms ()
  (truncate (* 1000
               (/ (coerce (get-internal-real-time) 'double-float)
                  (coerce internal-time-units-per-second 'double-float)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %normalize-skill-name (name)
    (%normalize-command-name name))

  (defun %normalize-skill-type (type-spec)
    (cond
      ((or (eq type-spec :string)
           (eq type-spec 'string))
       :string)
      ((or (eq type-spec :integer)
           (eq type-spec 'integer)
           (eq type-spec 'fixnum))
       :integer)
      ((or (eq type-spec :keyword)
           (eq type-spec 'keyword)
           (eq type-spec 'symbol))
       :keyword)
      ((or (eq type-spec :boolean)
           (eq type-spec 'boolean))
       :boolean)
      ((and (consp type-spec)
            (eq (first type-spec) 'member))
       :keyword)
      (t
       :string)))

  (defun %skill-name->variable (name)
    (etypecase name
      (symbol name)
      (string (intern (string-upcase name) *package*)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %parse-skill-declarations (forms)
    (let ((options (list :category :general
                         :keybinding nil
                         :aliases '()
                         :usage nil
                         :args nil
                         :completer nil))
          (remaining forms))
      (loop while (and remaining
                       (consp (first remaining))
                       (keywordp (first (first remaining))))
            do (let ((declaration (first remaining)))
                 (destructuring-bind (keyword value &rest extra) declaration
                   (declare (ignore extra))
                   (unless (member keyword
                                   '(:category :keybinding :aliases :usage :args :completer)
                                   :test #'eq)
                     (error "Unknown DEFSKILL declaration keyword: ~S" keyword))
                   (setf (getf options keyword) value)))
               (setf remaining (rest remaining)))
      (values options remaining)))

  (defun %normalize-skill-argument-spec (spec)
    (labels ((ensure-options (name options)
               (unless (evenp (length options))
                 (error "Skill argument options must be key/value pairs for ~S." name)))
             (normalize (name type options)
               (ensure-options name options)
               (let* ((default-supplied-p (member :default options :test #'eq))
                      (default (getf options :default))
                      (required-explicit-p (member :required options :test #'eq))
                      (required-p
                        (if required-explicit-p
                            (not (null (getf options :required)))
                            (not default-supplied-p)))
                      (greedy-p (not (null (getf options :greedy))))
                      (prompt (getf options :prompt))
                      (description (getf options :description))
                      (raw-choices (getf options :choices))
                      (choices-value
                        (if (and (consp raw-choices)
                                 (eq (first raw-choices) 'quote))
                            (second raw-choices)
                            raw-choices))
                      (type* (or type :string))
                      (choices (if (and (consp type*)
                                        (eq (first type*) 'member)
                                        (null choices-value))
                                   (rest type*)
                                   choices-value)))
                 (list :name (if (symbolp name)
                                 (string-downcase (symbol-name name))
                                 (string-downcase (princ-to-string name)))
                       :variable (%skill-name->variable
                                  (if (symbolp name)
                                      name
                                      (princ-to-string name)))
                       :type (%normalize-skill-type type*)
                       :required-p required-p
                       :default default
                       :default-supplied-p (not (null default-supplied-p))
                       :choices choices
                       :greedy-p greedy-p
                       :prompt (and prompt (princ-to-string prompt))
                       :description (and description (princ-to-string description))
                       :completer (getf options :completer)))))
      (cond
        ((symbolp spec)
         (normalize spec :string '()))
        ((and (consp spec) (symbolp (first spec)))
         (let* ((name (first spec))
                (tail (rest spec))
                (type (when tail (first tail)))
                (type-keyword-p (member type '(:string :integer :keyword :boolean)
                                        :test #'eq))
                (options (if (and type
                                  (keywordp type)
                                  (not type-keyword-p))
                             tail
                             (rest tail)))
                (type* (if (and type
                                (keywordp type)
                                (not type-keyword-p))
                           :string
                           type)))
           (normalize name type* options)))
        (t
         (error "Invalid skill argument spec: ~S" spec)))))

  (defun %skill-handler-symbol (name)
    (intern (format nil "%SKILL-HANDLER-~A"
                    (string-upcase (symbol-name name)))
            (find-package :amoebum)))

  (defun %skill-completer-symbol (name)
    (intern (format nil "%SKILL-COMPLETER-~A"
                    (string-upcase (symbol-name name)))
            (find-package :amoebum)))

  (defun %skill-arg-plist-constant (argument)
    (list :name (getf argument :name)
          :type (getf argument :type)
          :required-p (not (null (getf argument :required-p)))
          :default (getf argument :default)
          :default-supplied-p (not (null (getf argument :default-supplied-p)))
          :prompt (getf argument :prompt)
          :choices (getf argument :choices)
          :greedy-p (not (null (getf argument :greedy-p)))
          :description (getf argument :description)
          :completer (getf argument :completer)))

  (defun %skill-default-usage (name normalized-args)
    (let ((parts '()))
      (dolist (arg normalized-args)
        (let* ((token
                 (if (getf arg :greedy-p)
                     (format nil "<~A...>" (getf arg :name))
                     (format nil "<~A>" (getf arg :name))))
               (rendered
                 (if (getf arg :required-p)
                     token
                     (format nil "[~A]" token))))
          (push rendered parts)))
      (format nil "/~A~@[ ~{~A~^ ~}~]"
              (%normalize-skill-name name)
              (nreverse parts))))

  (defun %normalize-skill-aliases (raw-aliases)
    (cond
      ((null raw-aliases) '())
      ((listp raw-aliases) raw-aliases)
      (t (list raw-aliases))))

  (defun %skill-binding-form (argument)
    (let ((variable (getf argument :variable))
          (arg-name (getf argument :name))
          (default (getf argument :default))
          (default-supplied-p (not (null (getf argument :default-supplied-p)))))
      `(,variable (%skill-argument-value arguments
                                         ,arg-name
                                         ',default
                                         ,default-supplied-p))))

  (defun %skill-argument-constructor-form (argument)
    `(make-skill-argument
      :name ,(getf argument :name)
      :variable ',(getf argument :variable)
      :type ,(getf argument :type)
      :required-p ,(not (null (getf argument :required-p)))
      :default ',(getf argument :default)
      :default-supplied-p ,(not (null (getf argument :default-supplied-p)))
      :choices ',(getf argument :choices)
      :greedy-p ,(not (null (getf argument :greedy-p)))
      :prompt ,(getf argument :prompt)
      :description ,(getf argument :description)
      :completer ,(getf argument :completer)))

  (defun %build-defskill-expansion (name docstring usage aliases category keybinding
                                    arg-plists argument-forms handler-symbol
                                    completer-symbol custom-completer binding-forms
                                    declare-ignorable body-forms)
    `(progn
       (defun ,handler-symbol (invocation arguments context)
         (declare (ignorable invocation context))
         (let ((missing (%skill-missing-required-arguments ',arg-plists arguments)))
           (when missing
             (return-from ,handler-symbol
               (make-slash-command-result
                :echo-input-p t
                :output (%skill-missing-arguments-output
                         ',name
                         ,usage
                         missing)))))
         (let* (,@binding-forms)
           (declare (ignorable ,@declare-ignorable))
           ,@body-forms))
       (defun ,completer-symbol (command invocation index fragment prefix-tokens)
         (declare (ignore command invocation))
         (or (and ,custom-completer
                  (funcall ,custom-completer
                           index
                           fragment
                           prefix-tokens))
             (%skill-default-completer ',arg-plists index fragment prefix-tokens)))
       (register-skill
        (make-skill-metadata
         :name ,(%normalize-skill-name name)
         :description ,docstring
         :usage ,usage
         :aliases ',aliases
         :category ,category
         :keybinding ,keybinding
         :arguments (list ,@argument-forms)
         :handler #',handler-symbol
         :completer #',completer-symbol
         :source-file ,(or *compile-file-truename*
                           *load-truename*
                           nil)
         :source-line nil
         :defined-at (%skill-now-ms))))))

(defun %skill-argument-keyword (argument-name)
  (%command-name-keyword argument-name))

(defun %skill-argument-present-p (arguments argument-name)
  (nth-value 1 (gethash (%skill-argument-keyword argument-name) arguments)))

(defun %skill-argument-value (arguments argument-name default default-supplied-p)
  (multiple-value-bind (value present-p)
      (gethash (%skill-argument-keyword argument-name) arguments)
    (if present-p
        value
        (if default-supplied-p default nil))))

(defun %skill-missing-required-arguments (argument-specs arguments)
  (loop for argument in argument-specs
        for required-p = (not (null (getf argument :required-p)))
        for name = (getf argument :name)
        when (and required-p
                  (not (%skill-argument-present-p arguments name)))
          collect argument))

(defun %skill-argument-type-label (argument)
  (string-downcase
   (symbol-name (or (getf argument :type) :string))))

(defun %skill-missing-arguments-output (skill-name usage missing)
  (let ((usage* (or usage (format nil "/~A" (%normalize-skill-name skill-name)))))
    (with-output-to-string (out)
      (dolist (argument missing)
        (let ((name (getf argument :name)))
          (format out "Missing required argument ~A." name)
          (let ((prompt (getf argument :prompt)))
            (if (and (stringp prompt) (plusp (length (%slash-trim prompt))))
                (format out " ~A~%" prompt)
                (format out " Please provide a ~A value.~%"
                        (%skill-argument-type-label argument)))))
      (format out "Usage: ~A" usage*)))))

(defun %skill-choice-text (choice)
  (cond
    ((symbolp choice) (string-downcase (symbol-name choice)))
    ((stringp choice) choice)
    (t (princ-to-string choice))))

(defun %starts-with-ci-fragment-p (prefix text)
  (%starts-with-ci-p (or prefix "") (or text "")))

(defun %skill-choice-completions (choices fragment)
  (let ((prefix (%slash-trim fragment)))
    (loop for choice in choices
          for text = (%skill-choice-text choice)
          when (%starts-with-ci-fragment-p prefix text)
            collect text)))

(defun %skill-default-argument-completions (argument fragment prefix-tokens)
  (declare (ignore prefix-tokens))
  (or (let ((completer (getf argument :completer)))
        (when (functionp completer)
          (funcall completer fragment)))
      (let ((choices (getf argument :choices)))
        (when choices
          (%skill-choice-completions choices fragment)))
      (when (eq (getf argument :type) :boolean)
        (%skill-choice-completions '("true" "false") fragment))
      '()))

(defun %skill-default-completer (argument-specs index fragment prefix-tokens)
  (let ((argument (nth index argument-specs)))
    (if argument
        (%skill-default-argument-completions argument fragment prefix-tokens)
        '())))

(defun register-skill (metadata)
  (check-type metadata skill-metadata)
  (let* ((name (%normalize-skill-name (skill-metadata-name metadata)))
         (command
           (make-slash-command
            :name name
            :description (skill-metadata-description metadata)
            :usage (skill-metadata-usage metadata)
            :aliases (copy-list (skill-metadata-aliases metadata))
            :parameters
            (mapcar (lambda (argument)
                      (make-slash-command-parameter
                       :name (skill-argument-name argument)
                       :type (skill-argument-type argument)
                       :required-p nil
                       :default (skill-argument-default argument)
                       :choices (skill-argument-choices argument)
                       :greedy-p (skill-argument-greedy-p argument)
                       :description (skill-argument-description argument)))
                    (skill-metadata-arguments metadata))
            :handler (skill-metadata-handler metadata)
            :completer (skill-metadata-completer metadata))))
    (register-slash-command command)
    (setf (gethash name *skill-registry*) metadata)
    metadata))

(defun find-skill (name)
  (gethash (%normalize-skill-name name) *skill-registry*))

(defun list-skills ()
  (sort (loop for metadata being the hash-values of *skill-registry*
              collect metadata)
        #'string<
        :key (lambda (metadata)
               (%normalize-skill-name (skill-metadata-name metadata)))))

(defun describe-skill (name)
  (find-skill name))

(defun %skill-plist-entry (value key)
  (cond
    ((hash-table-p value)
     (or (gethash key value)
         (and (keywordp key)
              (gethash (string-downcase (symbol-name key)) value))
         (and (keywordp key)
              (gethash (string-upcase (symbol-name key)) value))
         (and (stringp key)
              (gethash (intern (string-upcase key) :keyword) value))))
    ((and (listp value) (keywordp (first value)))
     (or (getf value key)
         (and (stringp key)
              (getf value (intern (string-upcase key) :keyword)))
         (and (keywordp key)
              (getf value (string-downcase (symbol-name key))))))
    (t
     nil)))

(defun %skill-json->data (value)
  (cond
    ((or (hash-table-p value)
         (and (listp value) (keywordp (first value))))
     value)
    ((stringp value)
     (handler-case
         (jonathan:parse value :as :hash-table)
       (error ()
         value)))
    (t
     value)))

(defun %skill-permission-mode ()
  (let ((cfg (ignore-errors (current-config))))
    (if (typep cfg 'config)
        (config-permission-mode cfg)
        :supervised)))

(defun %skill-tool-arguments (&rest pairs)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash (string-downcase
                             (if (symbolp key)
                                 (symbol-name key)
                                 (princ-to-string key)))
                            table)
                   value))
    table))

(defun %skill-invoke-tool (tool-name &optional arguments)
  "Invoke a tool through the full permission pipeline.
Routes through EXECUTE-TOOL (the CLOS generic with :before permission
enforcement) rather than INVOKE-TOOL-CALL (which bypasses permissions)."
  (let* ((payload (or arguments (make-hash-table :test #'equal)))
         (json-arguments (jonathan:to-json payload))
         (tool-call (pseudopod:make-tool-call
                     :name tool-name
                     :arguments json-arguments))
         (context (make-amoebum-context
                   :toolset *toolset*
                   :permission-mode (%skill-permission-mode)
                   :event-bus (and (boundp '*event-bus*) *event-bus*)
                   :hook-registry (and (boundp '*hook-registry*) *hook-registry*)
                   :initialize-notifications-p nil)))
    (execute-tool tool-call context)))

(defun %skill-message->text (message)
  (if (pseudopod:message-p message)
      (with-output-to-string (out)
        (loop for part in (pseudopod:message-content message)
              for index from 0 do
                (when (> index 0)
                  (write-char #\Newline out))
                (let ((type (string-downcase
                             (or (pseudopod:content-part-type part) "text"))))
                  (write-string
                   (cond
                     ((string= type "text")
                      (or (pseudopod:content-part-text part) ""))
                     ((string= type "think")
                      (or (pseudopod:content-part-think part) ""))
                     (t
                      (or (pseudopod:content-part-text part)
                          (pseudopod:content-part-think part)
                          "")))
                   out))))
      (princ-to-string message)))

(defun %skill-review-table (&rest pairs)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr do
      (setf (gethash key table) value))
    table))

(defun %skill-review-empty-p (value)
  (or (null value)
      (and (stringp value)
           (zerop (length (%slash-trim value))))))

(defun %skill-review-seq->list (value)
  (cond
    ((vectorp value)
     (loop for item across value collect item))
    ((listp value) value)
    (t '())))

(defun %skill-review-json-string (value)
  (handler-case
      (jonathan:to-json value)
    (error ()
      "{}")))

(defun %skill-review-normalize-severity (value)
  (let* ((raw (%slash-trim (if value (princ-to-string value) "")))
         (normalized (string-downcase raw)))
    (cond
      ((or (string= normalized "critical")
           (string= normalized "blocker"))
       "critical")
      ((or (string= normalized "high")
           (string= normalized "error"))
       "high")
      ((or (string= normalized "medium")
           (string= normalized "med")
           (string= normalized "warning"))
       "medium")
      ((string= normalized "low")
       "low")
      ((or (string= normalized "info")
           (string= normalized "informational"))
       "info")
      (t
       "medium"))))

(defun %skill-review-severity-rank (severity)
  (position (%skill-review-normalize-severity severity)
            '("critical" "high" "medium" "low" "info")
            :test #'string=))

(defun %skill-review-safe-int (value)
  (cond
    ((integerp value) value)
    ((and (stringp value)
          (plusp (length (%slash-trim value))))
     (handler-case
         (parse-integer (%slash-trim value))
       (error ()
         nil)))
    (t
     nil)))

(defun %skill-review-title-from-detail (detail index)
  (let* ((trimmed (%slash-trim (or detail "")))
         (max-len 72))
    (cond
      ((plusp (length trimmed))
       (if (> (length trimmed) max-len)
           (format nil "~A..." (subseq trimmed 0 max-len))
           trimmed))
      (t
       (format nil "Finding ~D" index)))))

(defun %skill-review-findings-from-text (text)
  (let ((findings '()))
    (when (and (stringp text)
               (plusp (length (%slash-trim text))))
      (with-input-from-string (stream text)
        (loop for line = (read-line stream nil nil)
              while line do
                (let ((trimmed (%slash-trim line)))
                  (when (plusp (length trimmed))
                    (let ((matched-p nil))
                      (cl-ppcre:register-groups-bind (severity detail)
                          ("(?i)^(?:[-*]\\s*|\\d+[\\).]\\s*)?(critical|high|medium|low|info|warning|error)\\s*[:\\-]\\s*(.+)$"
                           trimmed)
                        (setf matched-p t)
                        (push (%skill-review-table
                               "severity" (%skill-review-normalize-severity severity)
                               "title" (%skill-review-title-from-detail detail
                                                                        (1+ (length findings)))
                               "detail" (%slash-trim detail))
                              findings))
                      (unless matched-p
                        (cl-ppcre:register-groups-bind (detail)
                            ("^(?:[-*]\\s*|\\d+[\\).]\\s+)(.+)$" trimmed)
                          (push (%skill-review-table
                                 "severity" "medium"
                                 "title" (%skill-review-title-from-detail detail
                                                                          (1+ (length findings)))
                                 "detail" (%slash-trim detail))
                                findings)))))))))
    (nreverse findings)))

(defun %skill-review-normalize-finding (value index)
  (let* ((severity (%skill-review-normalize-severity
                    (or (%skill-plist-entry value :severity)
                        (%skill-plist-entry value "severity")
                        "medium")))
         (title (or (%skill-plist-entry value :title)
                    (%skill-plist-entry value "title")
                    (%skill-plist-entry value :summary)
                    (%skill-plist-entry value "summary")
                    (%skill-plist-entry value :message)
                    (%skill-plist-entry value "message")))
         (detail (or (%skill-plist-entry value :detail)
                     (%skill-plist-entry value "detail")
                     (%skill-plist-entry value :description)
                     (%skill-plist-entry value "description")
                     (%skill-plist-entry value :rationale)
                     (%skill-plist-entry value "rationale")
                     title))
         (file (or (%skill-plist-entry value :file)
                   (%skill-plist-entry value "file")
                   (%skill-plist-entry value :path)
                   (%skill-plist-entry value "path")))
         (line (%skill-review-safe-int
                (or (%skill-plist-entry value :line)
                    (%skill-plist-entry value "line")
                    (%skill-plist-entry value :line-number)
                    (%skill-plist-entry value "line_number"))))
         (title* (%slash-trim (or (and title (princ-to-string title)) "")))
         (detail* (%slash-trim (or (and detail (princ-to-string detail)) ""))))
    (%skill-review-table
     "severity" severity
     "title" (if (plusp (length title*))
                 title*
                 (%skill-review-title-from-detail detail* index))
     "detail" detail*
     "file" (and (not (%skill-review-empty-p file))
                 (%slash-trim (princ-to-string file)))
     "line" line)))

(defun %skill-review-finding< (left right)
  (let* ((left-rank (or (%skill-review-severity-rank (gethash "severity" left))
                        99))
         (right-rank (or (%skill-review-severity-rank (gethash "severity" right))
                         99))
         (left-file (or (gethash "file" left) ""))
         (right-file (or (gethash "file" right) ""))
         (left-line (or (gethash "line" left) 0))
         (right-line (or (gethash "line" right) 0))
         (left-title (or (gethash "title" left) ""))
         (right-title (or (gethash "title" right) "")))
    (or (< left-rank right-rank)
        (and (= left-rank right-rank)
             (or (string< left-file right-file)
                 (and (string= left-file right-file)
                      (or (< left-line right-line)
                          (and (= left-line right-line)
                               (string< left-title right-title)))))))))

(defun %skill-review-normalize-findings (raw-findings &key fallback-text)
  (let* ((seed (append (%skill-review-seq->list raw-findings)
                       (if (or raw-findings
                               (%skill-review-empty-p fallback-text))
                           '()
                           (%skill-review-findings-from-text fallback-text))))
         (normalized
           (loop for item in seed
                 for index from 1
                 collect (cond
                           ((hash-table-p item)
                            (%skill-review-normalize-finding item index))
                           ((and (listp item) (keywordp (first item)))
                            (%skill-review-normalize-finding item index))
                           ((stringp item)
                            (%skill-review-normalize-finding
                             (list :detail item :title (%skill-review-title-from-detail item index))
                             index))
                           (t
                            (%skill-review-normalize-finding
                             (list :detail (princ-to-string item))
                             index)))))
         (sorted (stable-sort (copy-list normalized) #'%skill-review-finding<)))
    (loop for finding in sorted
          for index from 1
          do (setf (gethash "id" finding) (format nil "R~D" index)))
    sorted))

(defun %skill-review-strip-fence (text)
  (let ((trimmed (%slash-trim text)))
    (if (and (>= (length trimmed) 6)
             (uiop:string-prefix-p "```" trimmed)
             (uiop:string-suffix-p "```" trimmed))
        (let* ((first-break (position #\Newline trimmed))
               (body (if first-break
                         (subseq trimmed (1+ first-break))
                         trimmed))
               (last-fence (search "```" body :from-end t)))
          (if last-fence
              (%slash-trim (subseq body 0 last-fence))
              (%slash-trim body)))
        trimmed)))

(defun %skill-review-parse-json-text (text)
  (handler-case
      (jonathan:parse (%skill-review-strip-fence text) :as :hash-table)
    (error ()
      nil)))

(defun %skill-review-coerce-analysis-object (analysis)
  (cond
    ((hash-table-p analysis)
     analysis)
    ((and (listp analysis)
          (keywordp (first analysis)))
     (let ((table (make-hash-table :test #'equal)))
       (loop for (key value) on analysis by #'cddr do
         (let ((name (if (keywordp key)
                         (string-downcase (symbol-name key))
                         (string-downcase (princ-to-string key)))))
           (setf (gethash name table) value)))
       table))
    ((and (stringp analysis)
          (plusp (length (%slash-trim analysis))))
     (%skill-review-parse-json-text analysis))
    (t
     nil)))

(defun %skill-review-build-report (diff-data analysis)
  (let* ((branch (or (%skill-plist-entry diff-data :branch) "unknown"))
         (base (or (%skill-plist-entry diff-data :base-branch) "unknown"))
         (range (or (%skill-plist-entry diff-data :range) ""))
         (diff-summary (or (%skill-plist-entry diff-data :summary) ""))
         (diff-text (or (%skill-plist-entry diff-data :diff) ""))
         (truncated-p (not (null (%skill-plist-entry diff-data :truncated-p))))
         (analysis-text (and (stringp analysis) (%slash-trim analysis)))
         (analysis-object (%skill-review-coerce-analysis-object analysis))
         (raw-summary (or (and analysis-object
                               (or (%skill-plist-entry analysis-object :summary)
                                   (%skill-plist-entry analysis-object "summary")))
                          (and (stringp analysis-text)
                               (plusp (length analysis-text))
                               analysis-text)
                          diff-summary
                          ""))
         (findings (%skill-review-normalize-findings
                    (and analysis-object
                         (or (%skill-plist-entry analysis-object :findings)
                             (%skill-plist-entry analysis-object "findings")))
                    :fallback-text analysis-text))
         (status
           (cond
             ((%skill-review-empty-p diff-text) "missing-diff")
             ((plusp (length findings)) "findings-present")
             (t "no-findings"))))
    (%skill-review-table
     "schema_version" *skill-review-schema-version*
     "status" status
     "branch" branch
     "base_branch" base
     "range" range
     "summary" (if (%skill-review-empty-p raw-summary)
                   "No review summary available."
                   (%slash-trim (princ-to-string raw-summary)))
     "diff_summary" (or diff-summary "")
     "findings_count" (length findings)
     "findings" (coerce findings 'vector)
     "truncated_diff" truncated-p)))

(defun %skill-review-render-human (report)
  (let* ((findings (%skill-review-seq->list (gethash "findings" report)))
         (status (or (gethash "status" report) "no-findings")))
    (with-output-to-string (out)
      (format out "Review report (~A vs ~A):~%"
              (or (gethash "branch" report) "unknown")
              (or (gethash "base_branch" report) "unknown"))
      (format out "Status: ~A~%" status)
      (format out "Summary: ~A~%" (or (gethash "summary" report) ""))
      (if findings
          (progn
            (format out "Findings:~%")
            (loop for finding in findings
                  for index from 1 do
                    (format out "~D. [~A] ~A~%"
                            index
                            (or (gethash "severity" finding) "medium")
                            (or (gethash "title" finding) "Finding"))
                    (when (plusp (length (%slash-trim (or (gethash "file" finding) ""))))
                      (format out "   File: ~A~%"
                              (gethash "file" finding)))
                    (let ((line (gethash "line" finding)))
                      (when (integerp line)
                        (format out "   Line: ~D~%" line)))
                    (let ((detail (%slash-trim (or (gethash "detail" finding) ""))))
                      (when (plusp (length detail))
                        (format out "   Detail: ~A~%" detail)))))
          (format out "Findings: none.~%"))
      (format out "Machine-readable payload:~%```json~%~A~%```"
              (%skill-review-json-string report)))))

(defun %skill-review-fallback (diff-data)
  (let* ((branch (or (%skill-plist-entry diff-data :branch) "unknown"))
         (base (or (%skill-plist-entry diff-data :base-branch) "unknown"))
         (summary (or (%skill-plist-entry diff-data :summary) "No diff summary available."))
         (files (or (%skill-plist-entry diff-data :files-changed) '()))
         (file-count (if (listp files) (length files) 0)))
    (with-output-to-string (out)
      (format out "Review summary (~A vs ~A):~%" branch base)
      (format out "- ~A~%" summary)
      (format out "- Changed files: ~D~%" file-count)
      (when (and (listp files) files)
        (format out "- Files: ~{~A~^, ~}" files)))))

(defun %default-skill-review-analyzer (diff-data &key model)
  (let* ((diff (or (%skill-plist-entry diff-data :diff) ""))
         (summary (or (%skill-plist-entry diff-data :summary) ""))
         (branch (or (%skill-plist-entry diff-data :branch) ""))
         (base (or (%skill-plist-entry diff-data :base-branch) ""))
         (effective-model (or model
                              (ignore-errors (config-model (current-config)))
                              "moonshot-v1-128k")))
    (if (or (not (stringp diff))
            (zerop (length (%slash-trim diff))))
        "No branch diff found to review."
        (handler-case
            (let* ((client (pseudopod:make-client :model effective-model))
                   (prompt (format nil
                                   "Review this git diff and respond with JSON only.~%\
Output schema: {\"summary\":string,\"findings\":[{\"severity\":\"high|medium|low|critical|info\",\"title\":string,\"detail\":string,\"file\":string? ,\"line\":number?}]}~%\
Focus on correctness risks, missing tests, and regressions. If no issues, return findings=[].~2%\
Branch: ~A~%\
Base: ~A~%\
Summary: ~A~2%\
Diff:~%~A"
                                   branch
                                   base
                                   summary
                                   diff))
                   (response (pseudopod:chat-completion*
                              client
                              prompt
                              :system-prompt
                              "You are a strict code reviewer. Return valid JSON only, no markdown. Keep findings concrete and prioritized."))
                   (text (%slash-trim (%skill-message->text response))))
              (if (plusp (length text))
                  text
                  (%skill-review-fallback diff-data)))
          (error ()
            (%skill-review-fallback diff-data))))))

(unless (functionp *skill-review-analyzer*)
  (setf *skill-review-analyzer* #'%default-skill-review-analyzer))

(defun %status-token-usage (context)
  (let* ((chat-state (and (typep context 'slash-command-context)
                          (slash-command-context-chat-state context)))
         (used (and chat-state
                    (ignore-errors
                      (chat-ui-state-context-used-tokens chat-state))))
         (limit (and chat-state
                     (ignore-errors
                       (chat-ui-state-context-window-limit chat-state)))))
    (list :used (or (and (integerp used) used) 0)
          :limit (or (and (integerp limit) limit) +default-context-window-limit+)
          :known-p (and (integerp used) (integerp limit)))))

(defmacro defskill (name argument-specs &body forms)
  (unless (symbolp name)
    (error "DEFSKILL name must be a symbol, got ~S." name))
  (let* ((docstring (and forms (stringp (first forms)) (first forms)))
         (tail (if docstring (rest forms) forms)))
    (multiple-value-bind (declarations body-forms)
        (%parse-skill-declarations tail)
      (when (null body-forms)
        (error "DEFSKILL ~S requires a body." name))
      (let* ((raw-args (or (getf declarations :args) argument-specs '()))
             (normalized-args (mapcar #'%normalize-skill-argument-spec raw-args))
             (arg-plists (mapcar #'%skill-arg-plist-constant normalized-args))
             (usage (or (getf declarations :usage)
                        (%skill-default-usage name normalized-args)))
             (aliases (%normalize-skill-aliases (getf declarations :aliases)))
             (category (or (getf declarations :category) :general))
             (keybinding (getf declarations :keybinding))
             (custom-completer (getf declarations :completer))
             (handler-symbol (%skill-handler-symbol name))
             (completer-symbol (%skill-completer-symbol name))
             (binding-forms (mapcar #'%skill-binding-form normalized-args))
             (declare-ignorable (mapcar (lambda (arg) (getf arg :variable))
                                        normalized-args))
             (argument-forms (mapcar #'%skill-argument-constructor-form normalized-args)))
        (%build-defskill-expansion name docstring usage aliases category keybinding
                                   arg-plists argument-forms handler-symbol
                                   completer-symbol custom-completer binding-forms
                                   declare-ignorable body-forms)))))

(defskill commit ((files :string
                         :required nil
                         :greedy t
                         :description "Optional explicit file paths to stage before committing."))
  "Create a git commit with an AI-generated message from staged diff."
  (:category :git)
  (:usage "/commit [files...]")
  (let* ((file-tokens (slash-command-invocation-argument-tokens invocation))
         (args (if file-tokens
                   (%skill-tool-arguments :files file-tokens)
                   (%skill-tool-arguments)))
         (raw (%skill-invoke-tool "git-commit" args))
         (result (%skill-json->data raw))
         (sha (or (%skill-plist-entry result :sha) "unknown"))
         (branch (or (%skill-plist-entry result :branch) "unknown"))
         (summary (or (%skill-plist-entry result :message-summary) ""))
         (source (or (%skill-plist-entry result :message-source) "unknown")))
    (make-slash-command-result
     :echo-input-p t
     :output (format nil "Created commit ~A on ~A (~A).~@[~%~A~]"
                     sha
                     branch
                     source
                     (and (plusp (length (%slash-trim (princ-to-string summary))))
                          summary)))))

(defskill review ((base-branch :string
                              :required nil
                              :description "Optional base branch override (default auto-detect)."))
  "Analyze current branch diff and return a review summary."
  (:category :git)
  (:usage "/review [base-branch]")
  (let* ((args (if (and (stringp base-branch)
                        (plusp (length (%slash-trim base-branch))))
                   (%skill-tool-arguments :base-branch (%slash-trim base-branch))
                   (%skill-tool-arguments)))
         (raw (%skill-invoke-tool "git-diff-branch" args))
         (diff-data (%skill-json->data raw))
         (diff-text (or (%skill-plist-entry diff-data :diff) ""))
         (analysis
           (if (%skill-review-empty-p diff-text)
               "No branch diff found to review."
               (if (functionp *skill-review-analyzer*)
                   (funcall *skill-review-analyzer*
                            diff-data
                            :model (ignore-errors (config-model (current-config))))
                   (%skill-review-fallback diff-data))))
         (report (%skill-review-build-report diff-data analysis)))
    (make-slash-command-result
     :echo-input-p t
     :output (%skill-review-render-human report)
     :payload report)))

(defskill compact ((keep-last :integer
                         :required nil
                         :default 6
                         :description "How many recent turns to keep verbatim."))
  "Compress conversation context by summarizing older messages."
  (:category :session)
  (:usage "/compact [keep-last-turns]")
  (make-slash-command-result
   :echo-input-p t
   :output nil
   :action :compact-chat
   :payload keep-last))

(defskill status ()
  "Show current config, branch, and token usage status."
  (:category :session)
  (:usage "/status")
  (let* ((cfg (or (ignore-errors (current-config))
                  (ignore-errors (load-config))))
         (mode (if (typep cfg 'config)
                   (config-permission-mode cfg)
                   :unknown))
         (model (if (typep cfg 'config)
                    (config-model cfg)
                    "unknown"))
         (branch-result
           (handler-case
               (%skill-json->data
                (%skill-invoke-tool "git-status" (%skill-tool-arguments)))
             (error ()
               nil)))
         (branch (or (%skill-plist-entry branch-result :branch) "-"))
         (token-usage (%status-token-usage context))
         (used (getf token-usage :used 0))
         (limit (getf token-usage :limit +default-context-window-limit+))
         (known-p (getf token-usage :known-p))
         (percent (context-usage-percent used limit)))
    (make-slash-command-result
     :echo-input-p t
     :output (with-output-to-string (out)
               (format out "Status:~%")
               (format out "- branch: ~A~%" branch)
               (format out "- mode: ~A~%" (string-downcase (symbol-name mode)))
                (format out "- model: ~A~%" model)
                (if known-p
                    (format out "- tokens: ~D/~D (~D%%)" used limit percent)
                    (format out "- tokens: ~D/~D (~D%%, estimated)" used limit percent))))))
