(defpackage :ptui.text.engine
  (:use :cl)
  (:export
   #:text-engine-adapter
   #:make-text-engine-adapter
   #:text-engine-adapter-name
   #:register-text-engine
   #:available-text-engines
   #:engine-available-p
   #:resolve-text-engine
   #:call-split-graphemes
   #:call-codepoint-width))

(in-package :ptui.text.engine)

(defstruct (text-engine-adapter
            (:constructor make-text-engine-adapter
                (name split-graphemes-fn codepoint-width-fn
                      &key (available-p-fn (lambda () t)))))
  (name (error "NAME required") :type keyword)
  (split-graphemes-fn (error "SPLIT-GRAPHEMES-FN required") :type function)
  (codepoint-width-fn (error "CODEPOINT-WIDTH-FN required") :type function)
  (available-p-fn (lambda () t) :type function))

(defparameter *text-engine-registry* (make-hash-table :test #'eq))

(defun register-text-engine (adapter)
  (check-type adapter text-engine-adapter)
  (setf (gethash (text-engine-adapter-name adapter) *text-engine-registry*) adapter)
  (text-engine-adapter-name adapter))

(defun %find-adapter (name)
  (gethash name *text-engine-registry*))

(defun engine-available-p (name)
  (check-type name keyword)
  (let ((adapter (%find-adapter name)))
    (and adapter
         (handler-case
             (not (null (funcall (text-engine-adapter-available-p-fn adapter))))
           (error ()
             nil)))))

(defun available-text-engines (&key (only-available nil))
  (let ((names '()))
    (maphash (lambda (name adapter)
               (declare (ignore adapter))
               (when (or (not only-available) (engine-available-p name))
                 (push name names)))
             *text-engine-registry*)
    (sort names #'string< :key #'symbol-name)))

(defun %ensure-adapter (name)
  (or (%find-adapter name)
      (error "Unknown text engine: ~S. Registered engines: ~S"
             name
             (available-text-engines :only-available nil))))

(defun resolve-text-engine (&optional (engine :auto))
  (check-type engine keyword)
  (case engine
    (:auto
     (cond
       ((engine-available-p :native) :native)
       ((engine-available-p :fallback) :fallback)
       (t
        (error "No available text engine adapters. Registered: ~S"
               (available-text-engines :only-available nil)))))
    (:native
     (if (engine-available-p :native)
         :native
         (if (engine-available-p :fallback)
             :fallback
             (error "Native engine unavailable and no fallback adapter registered."))))
    (t
     (unless (engine-available-p engine)
       (error "Requested text engine ~S is unavailable." engine))
     engine)))

(defun call-split-graphemes (text &key (engine :auto))
  (check-type text string)
  (let* ((resolved (resolve-text-engine engine))
         (adapter (%ensure-adapter resolved))
         (clusters (funcall (text-engine-adapter-split-graphemes-fn adapter) text)))
    (unless (listp clusters)
      (error "Text engine ~S split callback returned non-list: ~S" resolved clusters))
    clusters))

(defun call-codepoint-width (ch &key (engine :auto))
  (check-type ch character)
  (let* ((resolved (resolve-text-engine engine))
         (adapter (%ensure-adapter resolved))
         (width (funcall (text-engine-adapter-codepoint-width-fn adapter) ch)))
    (check-type width (integer 0 *))
    width))
