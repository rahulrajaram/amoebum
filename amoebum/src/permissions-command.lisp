(in-package :amoebum)

(declaim (special *dangerous-command-patterns*))

(defstruct (command-canonical-form
            (:constructor make-command-canonical-form
                (&key raw normalized policy-key executable argv operators wrappers commands
                      ast operator-metadata canonical-signature dangerous-reason-codes)))
  raw
  normalized
  policy-key
  executable
  argv
  operators
  wrappers
  commands
  ast
  operator-metadata
  canonical-signature
  dangerous-reason-codes)

(defparameter *last-command-canonicalization-trace* nil)

(defun %command-string (command)
  (typecase command
    (null nil)
    (string command)
    (symbol (symbol-name command))
    (pathname (namestring command))
    (t (prin1-to-string command))))

(defun %trim-command-whitespace (value)
  (if (stringp value)
      (string-trim '(#\Space #\Tab #\Newline #\Return) value)
      ""))

(defun %command-list-string (command)
  (when (and (listp command) command)
    (%trim-command-whitespace
     (format nil "~{~A~^ ~}"
             (loop for item in command
                   for value = (%trim-command-whitespace (%command-string item))
                   when (and (stringp value)
                             (plusp (length value)))
                     collect value)))))

(defun %command-raw-text (command)
  (let ((raw (if (listp command)
                 (%command-list-string command)
                 (%command-string command))))
    (when raw
      (let ((trimmed (%trim-command-whitespace raw)))
        (when (> (length trimmed) 0)
          trimmed)))))

(defun %shell-whitespace-char-p (char)
  (or (char= char #\Space)
      (char= char #\Tab)
      (char= char #\Newline)
      (char= char #\Return)))

(defun %shell-operator-at (text index)
  (let* ((len (length text))
         (remaining (- len index)))
    (flet ((prefix-p (token)
             (and (>= remaining (length token))
                  (string= token text
                           :start1 0
                           :end1 (length token)
                           :start2 index
                           :end2 (+ index (length token))))))
      (cond
        ((prefix-p "&&") (values "&&" 2))
        ((prefix-p "||") (values "||" 2))
        ((prefix-p "|&") (values "|&" 2))
        ((prefix-p "2>>") (values "2>>" 3))
        ((prefix-p "2>") (values "2>" 2))
        ((prefix-p "&>") (values "&>" 2))
        ((prefix-p ">>") (values ">>" 2))
        ((prefix-p "<<") (values "<<" 2))
        ((find (char text index) "|;&()<>"
               :test #'char=)
         (values (string (char text index)) 1))
        (t
         (values nil 0))))))

(defun %shell-safe-char-p (char)
  (or (and (>= (char-code char) (char-code #\a))
           (<= (char-code char) (char-code #\z)))
      (and (>= (char-code char) (char-code #\A))
           (<= (char-code char) (char-code #\Z)))
      (and (>= (char-code char) (char-code #\0))
           (<= (char-code char) (char-code #\9)))
      (find char "-._/:=+%@,"
            :test #'char=)))

(defun %shell-single-quote (text)
  (with-output-to-string (stream)
    (write-char #\' stream)
    (loop for char across text do
          (if (char= char #\')
              (write-string "'\"'\"'" stream)
              (write-char char stream)))
    (write-char #\' stream)))

(defun %canonical-shell-word (word)
  (let ((text (or word "")))
    (if (and (> (length text) 0)
             (loop for char across text
                   always (%shell-safe-char-p char)))
        text
        (%shell-single-quote text))))

(defstruct (shell-tokenizer-state
            (:constructor make-shell-tokenizer-state
                (&key (tokens '())
                      (in-single-p nil)
                      (in-double-p nil)
                      (escape-next-p nil)
                      (buffer (make-string-output-stream)))))
  tokens
  in-single-p
  in-double-p
  escape-next-p
  buffer)

(defun %emit-shell-token-word (state)
  (let ((value (get-output-stream-string (shell-tokenizer-state-buffer state))))
    (when (> (length value) 0)
      (push (cons :word value) (shell-tokenizer-state-tokens state))))
  state)

(defun %emit-shell-token-operator (state value)
  (push (cons :operator value) (shell-tokenizer-state-tokens state))
  state)

(defun %write-shell-token-char (state char)
  (write-char char (shell-tokenizer-state-buffer state))
  state)

(defun %consume-double-quoted-shell-char (state text index len)
  (let ((char (char text index)))
    (cond
      ((char= char #\\)
       (if (< (1+ index) len)
           (progn
             (incf index)
             (%write-shell-token-char state (char text index)))
           (%write-shell-token-char state char)))
      ((char= char #\")
       (setf (shell-tokenizer-state-in-double-p state) nil))
      (t
       (%write-shell-token-char state char))))
  index)

(defun %consume-unquoted-shell-char (state text index)
  (let ((char (char text index)))
    (cond
      ((char= char #\\)
       (setf (shell-tokenizer-state-escape-next-p state) t))
      ((char= char #\')
       (setf (shell-tokenizer-state-in-single-p state) t))
      ((char= char #\")
       (setf (shell-tokenizer-state-in-double-p state) t))
      ((%shell-whitespace-char-p char)
       (%emit-shell-token-word state))
      (t
       (multiple-value-bind (operator width)
           (%shell-operator-at text index)
         (if operator
             (progn
               (%emit-shell-token-word state)
               (%emit-shell-token-operator state operator)
               (incf index (1- width)))
             (%write-shell-token-char state char)))))
    index))

(defun %tokenize-shell-command (text)
  (let* ((len (length text))
         (state (make-shell-tokenizer-state)))
    (loop for index from 0 below len do
          (let ((char (char text index)))
            (cond
              ((shell-tokenizer-state-escape-next-p state)
               (%write-shell-token-char state char)
               (setf (shell-tokenizer-state-escape-next-p state) nil))
              ((shell-tokenizer-state-in-single-p state)
               (if (char= char #\')
                   (setf (shell-tokenizer-state-in-single-p state) nil)
                   (%write-shell-token-char state char)))
              ((shell-tokenizer-state-in-double-p state)
               (setf index (%consume-double-quoted-shell-char state text index len)))
              (t
               (setf index (%consume-unquoted-shell-char state text index))))))
    (when (shell-tokenizer-state-escape-next-p state)
      (%write-shell-token-char state #\\))
    (%emit-shell-token-word state)
    (nreverse (shell-tokenizer-state-tokens state))))

(defun %separator-operator-p (operator)
  (member operator '("|" "||" "&&" ";" "&")
          :test #'string=))

(defun %redirection-operator-p (operator)
  (member operator '(">" ">>" "<" "<<" "2>" "2>>" "&>")
          :test #'string=))

(defun %canonicalize-shell-tokens (tokens)
  (when tokens
    (with-output-to-string (stream)
      (loop for token in tokens
            for index from 0 do
              (when (> index 0)
                (write-char #\Space stream))
              (ecase (car token)
                (:word
                 (write-string (%canonical-shell-word (cdr token)) stream))
                (:operator
                 (write-string (cdr token) stream)))))))

(defun %tokens->command-segments (tokens)
  (let ((segments '())
        (operators '())
        (current '()))
    (labels ((flush-segment ()
               (when current
                 (push (nreverse current) segments)
                 (setf current '()))))
      (dolist (token tokens)
        (ecase (car token)
          (:word
           (push (cdr token) current))
          (:operator
           (let ((operator (cdr token)))
             (push operator operators)
             (when (%separator-operator-p operator)
               (flush-segment))))))
      (flush-segment))
    (values (nreverse segments) (nreverse operators))))

(defun %tokens->ast (tokens)
  (let ((ast '())
        (current-argv '())
        (current-redirections '())
        (command-index 0))
    (labels ((flush-command ()
               (when (or current-argv current-redirections)
                 (push (list :type :command
                             :index command-index
                             :argv (nreverse current-argv)
                             :redirections (nreverse current-redirections))
                       ast)
                 (incf command-index)
                 (setf current-argv '()
                       current-redirections '()))))
      (dolist (token tokens)
        (ecase (car token)
          (:word
           (push (cdr token) current-argv))
          (:operator
           (let ((operator (cdr token)))
             (if (%separator-operator-p operator)
                 (progn
                   (flush-command)
                   (push (list :type :operator
                               :value operator)
                         ast))
                 (push operator current-redirections))))))
      (flush-command)
      (nreverse ast))))

(defun %command-env-assignment-p (value)
  (and (stringp value)
       (> (length value) 1)
       (let ((equals (position #\= value)))
         (and equals
              (> equals 0)
              (let ((first (char value 0)))
                (or (char= first #\_)
                    (alpha-char-p first)))
              (loop for index from 1 below equals
                    for char = (char value index)
                    always (or (char= char #\_)
                               (alpha-char-p char)
                               (digit-char-p char)))))))

(defun %canonicalize-env-assignments (assignments)
  (let ((table (make-hash-table :test #'equal))
        (keys '()))
    (dolist (assignment assignments)
      (when (%command-env-assignment-p assignment)
        (let ((key (subseq assignment 0 (position #\= assignment))))
          (unless (gethash key table)
            (push key keys))
          (setf (gethash key table) assignment))))
    (loop for key in (sort (copy-list keys)
                           #'string<
                           :key #'string-downcase)
          collect (gethash key table))))

(defun %unwrap-env-command (argv)
  (if (and argv (string= (string-downcase (first argv)) "env"))
      (let ((remaining (rest argv))
            (options '())
            (assignments '()))
        (loop while remaining do
              (let ((token (first remaining)))
                (cond
                  ((string= token "--")
                   (setf remaining (rest remaining))
                   (return))
                  ((and (stringp token)
                        (> (length token) 1)
                        (char= (char token 0) #\-)
                        (not (%command-env-assignment-p token)))
                   (push token options)
                   (setf remaining (rest remaining))
                   (when (and remaining
                              (member token '("-u" "--unset")
                                      :test #'string=))
                     (push (first remaining) options)
                     (setf remaining (rest remaining))))
                  (t
                   (return)))))
        (loop while (and remaining
                         (%command-env-assignment-p (first remaining))) do
              (push (first remaining) assignments)
              (setf remaining (rest remaining)))
        (if remaining
            (values remaining
                    (list :type :env
                          :options (nreverse options)
                          :assignments (%canonicalize-env-assignments
                                        (nreverse assignments))))
            (values argv nil)))
      (values argv nil)))

(defun %unwrap-shell-c-command (argv)
  (let ((program (and argv (string-downcase (first argv)))))
    (if (member program '("bash" "sh")
                :test #'string=)
        (block done
          (let ((remaining (rest argv))
                (options '()))
            (loop while remaining do
                  (let ((token (first remaining)))
                    (cond
                      ((member token '("-c" "-lc" "-cl")
                               :test #'string=)
                       (let ((script (second remaining))
                             (tail (cddr remaining)))
                         (unless (stringp script)
                           (return-from done (values argv nil)))
                         (let* ((nested (canonicalize-permission-command script))
                                (nested-argv (and nested
                                                  (command-canonical-form-argv nested)))
                                (wrapper (list :type :shell
                                               :program program
                                               :options (nreverse (cons token options))
                                               :raw-script script
                                               :normalized-script
                                               (and nested
                                                    (command-canonical-form-normalized nested)))))
                           (return-from done
                             (values (append nested-argv tail) wrapper)))))
                      ((and (stringp token)
                            (> (length token) 1)
                            (char= (char token 0) #\-))
                       (push token options)
                       (setf remaining (rest remaining)))
                      (t
                       (return-from done (values argv nil)))))))
          (values argv nil))
        (values argv nil))))

(defun %canonical-argv-string (argv)
  (when argv
    (format nil "~{~A~^ ~}"
            (mapcar #'%canonical-shell-word argv))))

(defun %command-policy-key (argv wrappers)
  (let ((prefixes
          (loop for wrapper in wrappers
                when (eq (getf wrapper :type) :env)
                  collect (let ((assignments (getf wrapper :assignments))
                                (options (getf wrapper :options)))
                            (string-trim
                             '(#\Space)
                             (format nil "env ~@[~{~A~^ ~} ~]~:[~;~{~A~^ ~}~]"
                                     options
                                     assignments
                                     assignments))))))
    (let ((base (%canonical-argv-string argv)))
      (if (or prefixes base)
          (string-trim '(#\Space)
                       (format nil "~@[~{~A~^ ~} ~]~A"
                               prefixes
                               (or base "")))
          nil))))

(defun %operator-metadata (operators)
  (list :operators operators
        :contains-pipeline (member "|" operators :test #'string=)
        :contains-logical-and (member "&&" operators :test #'string=)
        :contains-logical-or (member "||" operators :test #'string=)
        :contains-separator (member ";" operators :test #'string=)
        :contains-background (member "&" operators :test #'string=)
        :contains-redirection
        (loop for operator in operators
              thereis (%redirection-operator-p operator))))

(defun %wrapper-signature (wrappers)
  (when wrappers
    (format nil "~{~A~^|~}"
            (loop for wrapper in wrappers
                  collect (case (getf wrapper :type)
                            (:env
                             (format nil "env[~{~A~^,~}]"
                                     (or (getf wrapper :assignments) '())))
                            (:shell
                             (format nil "shell[~A]"
                                     (or (getf wrapper :program) "")))
                            (otherwise
                             (format nil "wrapper[~A]"
                                     (or (getf wrapper :type) ""))))))))

(defun %command-canonical-signature (argv wrappers operators)
  (let* ((argv* (%canonical-argv-string argv))
         (wrapper* (%wrapper-signature wrappers))
         (operators* (and operators
                          (format nil "~{~A~^,~}" operators))))
    (format nil "argv=~A|wrappers=~A|operators=~A"
            (or argv* "")
            (or wrapper* "")
            (or operators* ""))))

(defparameter *interactive-command-programs*
  '("vim" "vi" "nvim" "nano" "emacs" "less" "more" "man" "top" "htop" "watch" "tailf"))

(defun %segment->executable (segment)
  (loop for token in segment
        unless (%command-env-assignment-p token)
          do (return (string-downcase token))
        finally (return nil)))

(defun %segment-has-argv-flag-p (segment flag)
  (member flag segment :test #'string=))

(defun %command-danger-reason-codes (canonical &optional (patterns *dangerous-command-patterns*))
  (let* ((canonical* (if (typep canonical 'command-canonical-form)
                         canonical
                         (canonicalize-permission-command canonical)))
         (normalized (and canonical* (command-canonical-form-normalized canonical*)))
         (wrappers (and canonical* (command-canonical-form-wrappers canonical*)))
         (commands (and canonical* (command-canonical-form-commands canonical*)))
         (reasons '()))
    (when (and normalized
               (loop for pattern in patterns
                     thereis (cl-ppcre:scan pattern normalized)))
      (push :dangerous-pattern-match reasons))
    (dolist (command commands)
      (let ((executable (%segment->executable command)))
        (when (and executable
                   (member executable *interactive-command-programs* :test #'string=))
          (push :interactive-command-class reasons))
        (when (and executable
                   (string= executable "ssh")
                   (not (%segment-has-argv-flag-p command "-T")))
          (push :interactive-ssh-session reasons))))
    (when wrappers
      (dolist (wrapper wrappers)
        (when (eq (getf wrapper :type) :shell)
          (let* ((normalized-script (getf wrapper :normalized-script))
                 (nested (and normalized-script
                              (canonicalize-permission-command normalized-script)))
                 (nested-reasons (and nested
                                      (%command-danger-reason-codes nested patterns))))
            (when nested-reasons
              (dolist (reason nested-reasons)
                (push reason reasons))
              (push :shell-wrapper-expanded reasons)))))
      (when (and reasons
                 (find :env wrappers
                       :key (lambda (wrapper) (getf wrapper :type))
                       :test #'eq))
        (push :env-wrapper-expanded reasons)))
    (nreverse (remove-duplicates reasons :test #'eq))))

(defun canonicalize-permission-command (command)
  (let ((raw (%command-raw-text command)))
    (when raw
      (let* ((tokens (if (listp command)
                         (loop for item in command
                               for value = (%trim-command-whitespace (%command-string item))
                               when (and (stringp value) (plusp (length value)))
                                 collect (cons :word value))
                         (%tokenize-shell-command raw)))
             (normalized (%canonicalize-shell-tokens tokens)))
        (multiple-value-bind (commands operators)
            (%tokens->command-segments tokens)
          (let ((primary-argv (copy-list (or (first commands) '())))
                (wrappers '()))
            (loop repeat 8 do
                  (let ((changed-p nil))
                    (multiple-value-bind (after-env env-wrapper)
                        (%unwrap-env-command primary-argv)
                      (when env-wrapper
                        (setf primary-argv after-env
                              changed-p t)
                        (push env-wrapper wrappers)))
                    (multiple-value-bind (after-shell shell-wrapper)
                        (%unwrap-shell-c-command primary-argv)
                      (when shell-wrapper
                        (setf primary-argv after-shell
                              changed-p t)
                        (push shell-wrapper wrappers)))
                    (unless changed-p
                      (return))))
            (let* ((normalized-wrappers (nreverse wrappers))
                   (operator-metadata (%operator-metadata operators))
                   (canonical-signature (%command-canonical-signature primary-argv
                                                                      normalized-wrappers
                                                                      operators))
                   (canonical
                     (make-command-canonical-form
                      :raw raw
                      :normalized normalized
                      :policy-key (%command-policy-key primary-argv normalized-wrappers)
                      :executable (first primary-argv)
                      :argv primary-argv
                      :operators operators
                      :wrappers normalized-wrappers
                      :commands commands
                      :ast (%tokens->ast tokens)
                      :operator-metadata operator-metadata
                      :canonical-signature canonical-signature)))
              (setf (command-canonical-form-dangerous-reason-codes canonical)
                    (%command-danger-reason-codes canonical))
              (setf *last-command-canonicalization-trace*
                    (list :raw (command-canonical-form-raw canonical)
                          :normalized (command-canonical-form-normalized canonical)
                          :policy-key (command-canonical-form-policy-key canonical)
                          :canonical-signature canonical-signature
                          :operator-metadata operator-metadata
                          :wrappers (command-canonical-form-wrappers canonical)
                          :operators (command-canonical-form-operators canonical)
                          :dangerous-reason-codes
                          (command-canonical-form-dangerous-reason-codes canonical)))
              canonical)))))))

(defun command-canonicalization-trace ()
  *last-command-canonicalization-trace*)

(defun %permission-command-cache-key (tool canonical)
  (list :tool (%tool-name tool)
        :canonical-signature (and canonical
                                  (command-canonical-form-canonical-signature canonical))
        :operator-metadata (and canonical
                                (command-canonical-form-operator-metadata canonical))))

(defun %classify-command-arguments (arguments)
  (let ((flags '())
        (positionals '())
        (end-of-options-p nil))
    (dolist (argument arguments)
      (cond
        ((and (not end-of-options-p)
              (string= argument "--"))
         (setf end-of-options-p t))
        ((and (not end-of-options-p)
              (> (length argument) 1)
              (char= (char argument 0) #\-))
         (push argument flags))
        (t
         (push argument positionals))))
    (values (nreverse flags)
            (nreverse positionals))))

(defun %command-argument-profile-from-canonical (canonical)
  (when canonical
    (let* ((argv (copy-list (or (command-canonical-form-argv canonical) '())))
           (program (first argv))
           (arguments (rest argv)))
      (multiple-value-bind (flags positionals)
          (%classify-command-arguments arguments)
        (list :program program
              :argv argv
              :arguments arguments
              :flags flags
              :positionals positionals
              :operators (copy-list (or (command-canonical-form-operators canonical) '()))
              :wrappers (copy-list (or (command-canonical-form-wrappers canonical) '())))))))

(defun permission-command-argument-profile (command)
  "Return a structured argument profile for COMMAND.
COMMAND can be a raw command string/list or an existing COMMAND-CANONICAL-FORM."
  (let ((canonical (if (typep command 'command-canonical-form)
                       command
                       (canonicalize-permission-command command))))
    (%command-argument-profile-from-canonical canonical)))
