;;;; amoebum/src/fp/frozen.lisp
;;;;
;;;; `defrozen` — a small macro for defining frozen (immutable) records
;;;; in the amoebum functional kernel. Every slot is declared
;;;; `:read-only t`, so attempts to `setf` an accessor signal an error
;;;; at compile/run time. A companion `with-NAME` updater returns a
;;;; fresh instance with the requested slot overrides by invoking the
;;;; keyword constructor with the current values merged on top of the
;;;; user's new values — no mutation anywhere.
;;;;
;;;; Usage:
;;;;   (defrozen point (x y))
;;;;   (defrozen point (x y) (:documentation "A 2D point."))
;;;;
;;;; Expands to:
;;;;   (progn
;;;;     (defstruct (point (:constructor make-point (&key x y))
;;;;                       (:copier copy-point)
;;;;                       (:documentation "A 2D point."))
;;;;       (x nil :read-only t)
;;;;       (y nil :read-only t))
;;;;     (defun with-point (instance &key (x nil x-supplied-p)
;;;;                                      (y nil y-supplied-p))
;;;;       (make-point
;;;;         :x (if x-supplied-p x (point-x instance))
;;;;         :y (if y-supplied-p y (point-y instance)))))

(in-package #:amoebum.fp)

(defun %frozen-symbol (package-hint &rest parts)
  "Intern a new symbol in the same package as PACKAGE-HINT by
concatenating the printed names of PARTS. Used so the macro-generated
`make-NAME`, `with-NAME`, and supplied-p temporaries all live in the
caller's package rather than in `amoebum.fp`."
  (intern (format nil "~{~A~}" (mapcar #'string parts))
          (symbol-package package-hint)))

(defmacro defrozen (name slots &rest options)
  "Define a frozen (immutable) record NAME with SLOTS.

Every slot is emitted as `(slot nil :read-only t)`, so the generated
`defstruct` has no setf writers. OPTIONS are passed through to
`defstruct` (for example `(:documentation \"...\")`). The constructor
is forced to be a `&key` form named `make-NAME`, and a copier named
`copy-NAME` is always generated.

In addition to the struct, a `with-NAME` function is generated. It
takes the original INSTANCE plus `&key` overrides for any subset of
SLOTS and returns a fresh instance where unspecified slots inherit
their current values from INSTANCE. No slot of the original instance
is ever mutated — `with-NAME` calls `make-NAME` from scratch."
  (let* ((ctor (%frozen-symbol name 'make- name))
         (with-fn (%frozen-symbol name 'with- name))
         (accessors (mapcar (lambda (slot)
                              (%frozen-symbol name name '- slot))
                            slots))
         (supplied-ps (mapcar (lambda (slot)
                                (%frozen-symbol name slot '-supplied-p))
                              slots))
         (copier (%frozen-symbol name 'copy- name))
         (instance (gensym "INSTANCE"))
         (struct-options
           `((:constructor ,ctor (&key ,@slots))
             (:copier ,copier)
             ,@options))
         (slot-defs (mapcar (lambda (slot)
                              `(,slot nil :read-only t))
                            slots))
         (key-params
           (mapcar (lambda (slot sp)
                     `(,slot nil ,sp))
                   slots supplied-ps))
         (merged-args
           (loop for slot in slots
                 for sp in supplied-ps
                 for acc in accessors
                 collect (intern (string slot) :keyword)
                 collect `(if ,sp ,slot (,acc ,instance)))))
    `(progn
       (defstruct (,name ,@struct-options)
         ,@slot-defs)
       (defun ,with-fn (,instance &key ,@key-params)
         ,(format nil "Return a fresh ~A derived from INSTANCE with the given slot overrides. The original INSTANCE is not mutated." name)
         (,ctor ,@merged-args))
       ',name)))
