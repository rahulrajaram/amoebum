(in-package :amoebum)

;;;; ---------------------------------------------------------------------------
;;;; Skill runtime helpers.
;;;;
;;;; These helpers are called from the body of a `defskill`-expanded handler
;;;; at runtime (argument lookup, missing-required-argument reporting, default
;;;; argument completion). The expanded form references them by name; this
;;;; file MUST be loaded before any file that uses `defskill`.
;;;;
;;;; Behavior is preserved verbatim from the original
;;;; `amoebum/src/macros/defskill.lisp`; only file boundaries change.
;;;; ---------------------------------------------------------------------------

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
