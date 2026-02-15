(in-package :amoebum)

(defparameter *skill-registry* (make-hash-table :test #'equal))
(defparameter *skill-definition-counter* 0)
(defparameter *skill-review-analyzer* nil)

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
          :completer (getf argument :completer))))

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
  (let* ((payload (or arguments (make-hash-table :test #'equal)))
         (json-arguments (jonathan:to-json payload)))
    (pseudopod:invoke-tool-call
     *toolset*
     (pseudopod:make-tool-call
      :name tool-name
      :arguments json-arguments))))

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
                                   "Review this git diff.~%\
Focus on correctness risks, missing tests, and regressions.~2%\
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
                              "You are a strict code reviewer. Keep findings concrete and prioritized."))
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
                                  (nreverse parts)))))
             (aliases (let ((raw (getf declarations :aliases)))
                        (cond
                          ((null raw) '())
                          ((listp raw) raw)
                          (t (list raw)))))
             (category (or (getf declarations :category) :general))
             (keybinding (getf declarations :keybinding))
             (custom-completer (getf declarations :completer))
             (handler-symbol (%skill-handler-symbol name))
             (completer-symbol (%skill-completer-symbol name))
             (binding-forms
               (mapcar (lambda (arg)
                         (let ((variable (getf arg :variable))
                               (arg-name (getf arg :name))
                               (default (getf arg :default))
                               (default-supplied-p (not (null (getf arg :default-supplied-p)))))
                           `(,variable (%skill-argument-value arguments
                                                              ,arg-name
                                                              ',default
                                                              ,default-supplied-p))))
                       normalized-args))
             (declare-ignorable
               (mapcar (lambda (arg) (getf arg :variable)) normalized-args))
             (argument-forms
               (mapcar (lambda (arg)
                         `(make-skill-argument
                           :name ,(getf arg :name)
                           :variable ',(getf arg :variable)
                           :type ,(getf arg :type)
                           :required-p ,(not (null (getf arg :required-p)))
                           :default ',(getf arg :default)
                           :default-supplied-p ,(not (null (getf arg :default-supplied-p)))
                           :choices ',(getf arg :choices)
                           :greedy-p ,(not (null (getf arg :greedy-p)))
                           :prompt ,(getf arg :prompt)
                           :description ,(getf arg :description)
                           :completer ,(getf arg :completer)))
                       normalized-args)))
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
             :defined-at (%skill-now-ms))))))))

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
         (analysis
           (if (functionp *skill-review-analyzer*)
               (funcall *skill-review-analyzer*
                        diff-data
                        :model (ignore-errors (config-model (current-config))))
               (%skill-review-fallback diff-data))))
    (make-slash-command-result
     :echo-input-p t
     :output (if (stringp analysis)
                 analysis
                 (princ-to-string analysis)))))

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
