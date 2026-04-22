(in-package :amoebum)

;;;; ---------------------------------------------------------------------------
;;;; Skill registry: structs, globals, and registry mutators.
;;;;
;;;; This module owns the data model for registered skills and the registry
;;;; mutators (`register-skill`, `find-skill`, `list-skills`, `describe-skill`)
;;;; that the expanded `defskill` form calls at load time.
;;;;
;;;; Behavior is preserved verbatim from the original
;;;; `amoebum/src/macros/defskill.lisp`; only file boundaries change.
;;;; ---------------------------------------------------------------------------

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
    (%normalize-command-name name)))

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
