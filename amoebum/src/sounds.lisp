(in-package :amoebum)

(defstruct (sound-theme
            (:constructor %make-sound-theme
                (&key
                   name
                   parent
                   (mappings (make-hash-table :test #'eq)))))
  (name :standard :type keyword)
  (parent nil :type (or null keyword))
  (mappings (make-hash-table :test #'eq) :type hash-table))

(defparameter *sound-theme-registry* (make-hash-table :test #'eq))
(defparameter *active-sound-theme* :standard)

(defun %normalize-sound-keyword (value label)
  (cond
    ((keywordp value) value)
    ((symbolp value) (intern (string-upcase (symbol-name value)) :keyword))
    ((stringp value)
     (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
       (if (plusp (length trimmed))
           (intern (string-upcase trimmed) :keyword)
           (error "~A must not be blank." label))))
    (t
     (error "~A must be keyword-like, got ~S." label value))))

(defun %coerce-sound-mappings (mappings)
  (let ((table (make-hash-table :test #'eq)))
    (dolist (entry mappings)
      (unless (and (consp entry) (= (length entry) 2))
        (error "Sound mapping must be (category value), got ~S." entry))
      (setf (gethash (%normalize-sound-keyword (first entry) "Sound category")
                     table)
            (second entry)))
    table))

(defun make-sound-theme (&key name parent mappings)
  (%make-sound-theme :name (%normalize-sound-keyword name "Theme name")
                     :parent (and parent
                                  (%normalize-sound-keyword parent "Theme parent"))
                     :mappings (%coerce-sound-mappings mappings)))

(defun register-sound-theme (theme)
  (check-type theme sound-theme)
  (setf (gethash (sound-theme-name theme) *sound-theme-registry*) theme)
  theme)

(defun clear-sound-themes ()
  (clrhash *sound-theme-registry*)
  t)

(defun find-sound-theme (name)
  (when name
    (gethash (%normalize-sound-keyword name "Theme name")
             *sound-theme-registry*)))

(defun list-sound-themes ()
  (sort (loop for theme being the hash-values of *sound-theme-registry*
              collect theme)
        #'string<
        :key (lambda (theme)
               (string-downcase (symbol-name (sound-theme-name theme))))))

(defun list-sound-theme-names ()
  (mapcar #'sound-theme-name (list-sound-themes)))

(defun active-sound-theme ()
  (or (find-sound-theme *active-sound-theme*)
      (find-sound-theme :standard)
      (first (list-sound-themes))))

(defun active-sound-theme-name ()
  (let ((theme (active-sound-theme)))
    (when theme
      (sound-theme-name theme))))

(defun set-active-sound-theme (name)
  (let ((theme (find-sound-theme name)))
    (unless theme
      (error "Unknown sound theme ~S." name))
    (setf *active-sound-theme* (sound-theme-name theme))
    (sound-theme-name theme)))

(defun %resolve-sound-recursively (theme category visited)
  (unless (sound-theme-p theme)
    (return-from %resolve-sound-recursively nil))
  (let ((name (sound-theme-name theme)))
    (when (gethash name visited)
      (return-from %resolve-sound-recursively nil))
    (setf (gethash name visited) t))
  (let ((mappings (sound-theme-mappings theme)))
    (multiple-value-bind (value presentp)
        (gethash category mappings)
      (if presentp
          value
          (multiple-value-bind (wildcard-value wildcard-present-p)
              (gethash :* mappings)
            (if wildcard-present-p
                wildcard-value
                (let ((parent (sound-theme-parent theme)))
                  (if parent
                      (%resolve-sound-recursively (find-sound-theme parent)
                                                  category
                                                  visited)
                      nil))))))))

(defun resolve-sound (theme-or-name category)
  (let* ((theme (if (sound-theme-p theme-or-name)
                    theme-or-name
                    (find-sound-theme theme-or-name)))
         (resolved-category (%normalize-sound-keyword category "Sound category"))
         (visited (make-hash-table :test #'eq)))
    (%resolve-sound-recursively theme resolved-category visited)))

(defun %sound-config-key (category)
  (case category
    (:error :notification-sound-error)
    (:task-complete :notification-sound-task-complete)
    (:approval-needed :notification-sound-approval-needed)
    (otherwise nil)))

(defun %resolve-sound-reference (reference category config)
  (cond
    ((null reference) nil)
    ((or (stringp reference) (pathnamep reference))
     reference)
    ((and (keywordp reference) (eq reference :silence))
     nil)
    ((keywordp reference)
     (let ((config-key (%sound-config-key reference)))
       (and config-key (config-value config-key config))))
    ((symbolp reference)
     (%resolve-sound-reference
      (%normalize-sound-keyword reference "Sound reference")
      category
      config))
    (t
     (let ((fallback-key (%sound-config-key category)))
       (and fallback-key (config-value fallback-key config))))))

(defun resolve-sound-path (theme-or-name category &key config)
  (let* ((cfg (or config (current-config)))
         (resolved-category (%normalize-sound-keyword category "Sound category"))
         (reference (resolve-sound theme-or-name resolved-category)))
    (%resolve-sound-reference reference resolved-category cfg)))

(defun resolve-active-sound-path (category &key config)
  (let ((theme-name (active-sound-theme-name)))
    (and theme-name
         (resolve-sound-path theme-name category :config config))))

(defmacro defsoundtheme (name (&key inherits) &body mappings)
  (let ((mapping-forms '()))
    (dolist (mapping mappings)
      (unless (and (consp mapping)
                   (= (length mapping) 2))
        (error "defsoundtheme mapping must be (category value), got ~S." mapping))
      (push `(list ',(first mapping) ,(second mapping)) mapping-forms))
    `(eval-when (:load-toplevel :execute)
       (register-sound-theme
        (make-sound-theme :name ',name
                          :parent ,(if inherits `',inherits nil)
                          :mappings (list ,@(nreverse mapping-forms)))))))

(defmacro defsound-theme (name options &body mappings)
  `(defsoundtheme ,name ,options ,@mappings))

(defsoundtheme minimal ()
  (:error :error))

(defsoundtheme standard (:inherits minimal)
  (:task-complete :task-complete)
  (:approval-needed :approval-needed))

(defsoundtheme verbose (:inherits standard)
  (:* :task-complete))

(unless (find-sound-theme *active-sound-theme*)
  (setf *active-sound-theme* :standard))
