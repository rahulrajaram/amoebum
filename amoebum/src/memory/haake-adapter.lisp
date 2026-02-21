(in-package :amoebum)

(defclass haake-cli-memory-backend (memory-backend)
  ((command
    :initarg :command
    :reader haake-cli-memory-backend-command)
   (project-id
    :initarg :project-id
    :reader haake-cli-memory-backend-project-id)
   (agent
    :initarg :agent
    :reader haake-cli-memory-backend-agent)))

(defparameter *haake-cli-command-runner* nil)
(defparameter *haake-cli-availability-runner* nil)
(defparameter *haake-cli-status-runner* nil)
(defparameter *haake-cli-capability-runner* nil)

(defun haake-cli-memory-backend-p (value)
  (typep value 'haake-cli-memory-backend))

(defmethod memory-backend-kind ((backend haake-cli-memory-backend))
  (declare (ignore backend))
  :haake-cli)

(defun %slugify-segment (text &key (default "default"))
  (let* ((trimmed (%trim-text (or text "")))
         (downcased (string-downcase trimmed)))
    (if (zerop (length downcased))
        default
        (let ((raw
                (with-output-to-string (out)
                  (loop with pending-separator = nil
                        for char across downcased do
                          (cond
                            ((or (alphanumericp char) (char= char #\_))
                             (when pending-separator
                               (write-char #\- out)
                               (setf pending-separator nil))
                             (write-char char out))
                            (t
                             (setf pending-separator t)))))))
          (if (zerop (length raw))
              default
              raw)))))

(defun %default-haake-project-id (&optional project-root)
  (let* ((root (uiop:ensure-directory-pathname
                (or project-root
                    (config-project-root (current-config))
                    *default-pathname-defaults*)))
         (components (pathname-directory root))
         (leaf (car (last components))))
    (%slugify-segment (if (and leaf (stringp leaf))
                          leaf
                          (and leaf (princ-to-string leaf)))
                      :default "project")))

(defun make-haake-cli-memory-backend (&key command project-id agent project-root)
  (make-instance 'haake-cli-memory-backend
                 :command (or (and (stringp command)
                                   (plusp (length (%trim-text command)))
                                   (%trim-text command))
                              "haake")
                 :project-id (%slugify-segment (or project-id
                                                  (%default-haake-project-id project-root))
                                              :default "project")
                 :agent (or (and (stringp agent)
                                 (plusp (length (%trim-text agent)))
                                 (%trim-text agent))
                            "amoebum")))

(defun %haake-shell-single-quote (text)
  (format nil "'~A'"
          (with-output-to-string (out)
            (loop for char across (or text "") do
              (if (char= char #\')
                  (write-string "'\"'\"'" out)
                  (write-char char out))))))

(defun default-haake-cli-availability-runner (command)
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program
       (list "sh" "-lc"
             (format nil "command -v ~A >/dev/null 2>&1"
                     (%haake-shell-single-quote command)))
       :ignore-error-status t
       :output :string
       :error-output :string)
    (declare (ignore stdout stderr))
    (zerop (or exit-code 1))))

(defun haake-cli-available-p (&key (command "haake"))
  (handler-case
      (funcall (or *haake-cli-availability-runner*
                   #'default-haake-cli-availability-runner)
               command)
    (error ()
      nil)))

(defun default-haake-cli-status-runner (command &key directory)
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program (list command "status")
                        :ignore-error-status t
                        :directory directory
                        :output :string
                        :error-output :string)
    (declare (ignore stdout stderr))
    (zerop (or exit-code 1))))

(defun haake-cli-status-ok-p (&key (command "haake") directory)
  (handler-case
      (funcall (or *haake-cli-status-runner*
                   #'default-haake-cli-status-runner)
               command
               :directory directory)
    (error ()
      nil)))

(defun default-haake-cli-capability-runner (command &key directory)
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program (list command "memory" "--help")
                        :ignore-error-status t
                        :directory directory
                        :output :string
                        :error-output :string)
    (list :exit-code (or exit-code 1)
          :stdout (or stdout "")
          :stderr (or stderr ""))))

(defun haake-cli-compatible-p (&key (command "haake") directory)
  (handler-case
      (let* ((result (funcall (or *haake-cli-capability-runner*
                                  #'default-haake-cli-capability-runner)
                              command
                              :directory directory))
             (combined (string-downcase
                        (format nil "~A~%~A"
                                (or (getf result :stdout) "")
                                (or (getf result :stderr) "")))))
        (and (search "insert" combined :test #'char=)
             (search "query" combined :test #'char=)
             (search "list" combined :test #'char=)
             (search "delete" combined :test #'char=)))
    (error ()
      nil)))

(defun default-haake-cli-command-runner (arguments &key directory input)
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program arguments
                        :ignore-error-status t
                        :directory directory
                        :input (or input "")
                        :output :string
                        :error-output :string)
    (list :exit-code (or exit-code 1)
          :stdout (or stdout "")
          :stderr (or stderr ""))))

(defun %normalize-haake-run-result (result)
  (let ((exit-code (or (getf result :exit-code) 1))
        (stdout (or (getf result :stdout) ""))
        (stderr (or (getf result :stderr) "")))
    (list :exit-code exit-code
          :stdout stdout
          :stderr stderr)))

(defun %haake-cli-run (backend arguments &key input)
  (let* ((command (haake-cli-memory-backend-command backend))
         (runner (or *haake-cli-command-runner*
                     #'default-haake-cli-command-runner))
         (directory (ignore-errors (config-project-root (current-config))))
         (result (%normalize-haake-run-result
                  (funcall runner
                           (append (list command) arguments)
                           :directory directory
                           :input input)))
         (exit-code (getf result :exit-code)))
    (unless (zerop exit-code)
      (error "Haake command failed (exit ~D): ~{~A~^ ~}~@[~%~A~]"
             exit-code
             (append (list command) arguments)
             (let ((stderr (%trim-text (getf result :stderr))))
               (unless (zerop (length stderr))
                 stderr))))
    result))

(defun %topic-scope-descriptor-p (scope)
  (and (consp scope)
       (eq (first scope) :topic)
       (stringp (second scope))
       (plusp (length (%trim-text (second scope))))))

(defun %scope-keyword-from-descriptor (scope)
  (cond
    ((eq scope :global) :global)
    ((and (stringp scope)
          (plusp (length (%trim-text scope))))
     :topic)
    ((%topic-scope-descriptor-p scope) :topic)
    (t :project)))

(defun %haake-scope-path (backend scope)
  (let ((project-id (haake-cli-memory-backend-project-id backend)))
    (cond
      ((eq scope :global)
       "global/preferences")
      ((or (null scope)
           (eq scope :project)
           (eq scope :effective)
           (eq scope :all))
       (format nil "project/~A/preferences" project-id))
      ((%topic-scope-descriptor-p scope)
       (format nil "project/~A/topic/~A"
               project-id
               (%slugify-segment (second scope) :default "topic")))
      ((and (stringp scope)
            (plusp (length (%trim-text scope))))
       (format nil "project/~A/topic/~A"
               project-id
               (%slugify-segment scope :default "topic")))
      (t
       (error "Unsupported Haake scope ~S." scope)))))

(defun %parse-haake-output-line (line scope source)
  (let ((trimmed (%trim-text line)))
    (cond
      ((or (zerop (length trimmed))
           (%string-prefix-p-ci "#" trimmed))
       nil)
      ((search (string #\Tab) trimmed :test #'char=)
       (let* ((split (position #\Tab trimmed))
              (key (%trim-text (subseq trimmed 0 split)))
              (value (%trim-text (subseq trimmed (1+ split)))))
         (when (plusp (length value))
           (make-memory-entry :key (if (plusp (length key))
                                       key
                                       (%normalize-memory-key value))
                              :value value
                              :scope scope
                              :source source))))
      ((and (> (length trimmed) 4)
            (%string-prefix-p-ci "- [" trimmed))
       (%parse-memory-line trimmed scope source))
      ((search "|" trimmed :test #'char=)
       (let* ((split (position #\| trimmed))
              (key (%trim-text (subseq trimmed 0 split)))
              (value (%trim-text (subseq trimmed (1+ split)))))
         (when (plusp (length value))
           (make-memory-entry :key (if (plusp (length key))
                                       key
                                       (%normalize-memory-key value))
                              :value value
                              :scope scope
                              :source source))))
      (t
       (make-memory-entry :key (%normalize-memory-key trimmed)
                          :value trimmed
                          :scope scope
                          :source source)))))

(defun %parse-haake-entries (text scope)
  (%dedupe-memory-entries
   (loop for line in (uiop:split-string (or text "") :separator '(#\Newline))
         for parsed = (%parse-haake-output-line line scope :haake-cli)
         when parsed
           collect parsed)))

(defun %entry-matches-needle-p (entry needle)
  (or (search needle (string-downcase (or (memory-entry-key entry) "")) :test #'char=)
      (search needle (string-downcase (or (memory-entry-value entry) "")) :test #'char=)))

(defun %haake-list-remote-scope (backend scope)
  (let* ((normalized-scope (%scope-keyword-from-descriptor scope))
         (result (%haake-cli-run backend
                                 (list "memory" "list" (%haake-scope-path backend scope))))
         (entries (%parse-haake-entries (getf result :stdout) normalized-scope)))
    (%sort-memory-entries entries)))

(defmethod memory-store ((backend haake-cli-memory-backend) key value
                         &key (scope :project) (source :manual))
  (let* ((normalized-key (or (and key (plusp (length (%trim-text key))) (%trim-text key))
                             (%normalize-memory-key value)))
         (normalized-value (%collapse-whitespace value))
         (normalized-scope (%scope-keyword-from-descriptor scope)))
    (when (zerop (length normalized-value))
      (error "Memory value must not be empty."))
    (%haake-cli-run backend
                    (list "memory"
                          "insert"
                          (%haake-scope-path backend scope)
                          normalized-value
                          "-t"
                          "semantic"
                          "--key"
                          normalized-key
                          "--agent"
                          (haake-cli-memory-backend-agent backend)))
    (let ((entry (make-memory-entry :key normalized-key
                                    :value normalized-value
                                    :scope normalized-scope
                                    :source source)))
      (push entry *session-memory-entries*)
      (%publish-memory-updated backend :store normalized-key normalized-value)
      entry)))

(defmethod memory-query ((backend haake-cli-memory-backend) query
                         &key (scope :effective) (limit 25))
  (let ((needle (string-downcase (%trim-text (or query "")))))
    (cond
      ((or (eq scope :session)
           (eq scope :effective)
           (eq scope :all))
       (let* ((entries (memory-list backend :scope scope))
              (matches (if (zerop (length needle))
                           entries
                           (remove-if-not (lambda (entry)
                                            (%entry-matches-needle-p entry needle))
                                          entries))))
         (if (and (integerp limit) (>= limit 0))
             (subseq matches 0 (min limit (length matches)))
             matches)))
      (t
       (let* ((normalized-scope (%scope-keyword-from-descriptor scope))
              (result (%haake-cli-run backend
                                      (list "memory"
                                            "query"
                                            (%haake-scope-path backend scope)
                                            query
                                            "--limit"
                                            (format nil "~D"
                                                    (if (and (integerp limit) (>= limit 0))
                                                        limit
                                                        25)))))
              (entries (%parse-haake-entries (getf result :stdout) normalized-scope)))
         (if (and (integerp limit) (>= limit 0))
             (subseq entries 0 (min limit (length entries)))
             entries))))))

(defmethod memory-list ((backend haake-cli-memory-backend) &key (scope :effective))
  (cond
    ((eq scope :session)
     (%sort-memory-entries *session-memory-entries*))
    ((eq scope :global)
     (%haake-list-remote-scope backend :global))
    ((eq scope :project)
     (%haake-list-remote-scope backend :project))
    ((or (stringp scope)
         (%topic-scope-descriptor-p scope))
     (%haake-list-remote-scope backend scope))
    ((eq scope :effective)
     (%dedupe-memory-entries
      (append (%haake-list-remote-scope backend :global)
              (%haake-list-remote-scope backend :project))))
    ((eq scope :all)
     (%sort-memory-entries
      (append (memory-list backend :scope :effective)
              (memory-list backend :scope :session))))
    (t
     (error "Unknown memory list scope ~S." scope))))

(defmethod memory-delete ((backend haake-cli-memory-backend) key &key (scope :project))
  (let ((normalized-key (%normalize-memory-key key)))
    (cond
      ((eq scope :session)
       (let ((before (length *session-memory-entries*)))
         (setf *session-memory-entries*
               (remove normalized-key *session-memory-entries*
                       :key #'memory-entry-key
                       :test #'equal))
         (when (< (length *session-memory-entries*) before)
           (%publish-memory-updated backend :delete normalized-key nil)
           t)))
      ((eq scope :effective)
       (or (memory-delete backend normalized-key :scope :project)
           (memory-delete backend normalized-key :scope :global)))
      ((eq scope :all)
       (or (memory-delete backend normalized-key :scope :session)
           (memory-delete backend normalized-key :scope :project)
           (memory-delete backend normalized-key :scope :global)))
      (t
       (%haake-cli-run backend
                       (list "memory"
                             "delete"
                             (%haake-scope-path backend scope)
                             normalized-key))
       (setf *session-memory-entries*
             (remove normalized-key *session-memory-entries*
                     :key #'memory-entry-key
                     :test #'equal))
       (%publish-memory-updated backend :delete normalized-key nil)
       t))))

(defun %haake-forget-remote-scope (backend scope operation)
  (let ((count (length (memory-list backend :scope scope))))
    (%haake-cli-run backend
                    (list "memory"
                          "clear"
                          (%haake-scope-path backend scope)))
    (%publish-memory-updated backend operation nil nil)
    count))

(defmethod memory-forget ((backend haake-cli-memory-backend) &key (scope :session))
  (cond
    ((eq scope :session)
     (let ((count (length *session-memory-entries*)))
       (setf *session-memory-entries* '())
       (%publish-memory-updated backend :clear-session nil nil)
       count))
    ((eq scope :project)
     (%haake-forget-remote-scope backend :project :clear-project))
    ((eq scope :global)
     (%haake-forget-remote-scope backend :global :clear-global))
    ((or (stringp scope)
         (%topic-scope-descriptor-p scope))
     (%haake-forget-remote-scope backend scope :clear-topic))
    ((eq scope :effective)
     (+ (memory-forget backend :scope :project)
        (memory-forget backend :scope :global)))
    ((eq scope :all)
     (+ (memory-forget backend :scope :session)
        (memory-forget backend :scope :project)
        (memory-forget backend :scope :global)))
    (t
     (error "Unknown memory forget scope ~S." scope))))
