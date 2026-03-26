(in-package :amoebum)

;;; ============================================================
;;; Agent Personas: Definition, Discovery, and Manifest
;;;
;;; Persona files are Markdown with YAML frontmatter:
;;;   ---
;;;   name: code-reviewer
;;;   description: Expert at analyzing code for bugs
;;;   capabilities: [code-review, static-analysis]
;;;   model: moonshot-v1-128k
;;;   ---
;;;   You are an expert code reviewer...
;;; ============================================================

;;; --- Persona definition struct ---

(defstruct (persona-definition
            (:constructor %make-persona-definition
                (&key name description capabilities model
                      system-prompt file-path scope)))
  (name "" :type string)
  (description "" :type string)
  (capabilities nil :type list)
  (model nil :type (or null string))
  (system-prompt "" :type string)
  (file-path nil :type (or null pathname))
  (scope :project :type keyword))

;;; --- YAML frontmatter parsing ---

(defun %extract-yaml-frontmatter (text)
  "Split '---\\n...\\n---\\nbody' into (values frontmatter-string body-string).
   Returns (values nil text) if no frontmatter found."
  (let ((trimmed (string-left-trim '(#\Space #\Tab #\Newline #\Return) (or text ""))))
    (unless (and (>= (length trimmed) 3)
                 (string= "---" trimmed :end2 3))
      (return-from %extract-yaml-frontmatter (values nil trimmed)))
    ;; Find closing ---
    (let* ((after-first (subseq trimmed 3))
           (newline-pos (position #\Newline after-first)))
      (unless newline-pos
        (return-from %extract-yaml-frontmatter (values nil trimmed)))
      (let* ((body-start (1+ newline-pos))
             (rest-text (subseq after-first body-start))
             ;; Find closing --- on its own line
             (close-pos (search (format nil "~%---") rest-text)))
        (unless close-pos
          ;; Try at start of rest-text (closing --- is first line)
          (if (and (>= (length rest-text) 3)
                   (string= "---" rest-text :end2 3))
              (values "" (string-left-trim '(#\Newline #\Return)
                                           (subseq rest-text 3)))
              (return-from %extract-yaml-frontmatter (values nil trimmed))))
        (let ((frontmatter (subseq rest-text 0 close-pos))
              (body (string-left-trim '(#\Newline #\Return)
                                      (subseq rest-text (+ close-pos 4)))))
          (values frontmatter body))))))

(defun %parse-yaml-list-value (text)
  "Parse '[a, b, c]' into a list of trimmed strings."
  (let ((trimmed (string-trim '(#\Space #\Tab) (or text ""))))
    (when (and (>= (length trimmed) 2)
               (char= (char trimmed 0) #\[)
               (char= (char trimmed (1- (length trimmed))) #\]))
      (let ((inner (subseq trimmed 1 (1- (length trimmed)))))
        (mapcar (lambda (s) (string-trim '(#\Space #\Tab #\") s))
                (cl-ppcre:split "\\s*,\\s*" inner))))))

(defun %parse-yaml-frontmatter (text)
  "Minimal YAML parser: 'key: value' lines, 'key: [a, b]' lists.
   Returns alist: ((\"name\" . \"code-reviewer\") ...)."
  (let ((result '()))
    (with-input-from-string (stream (or text ""))
      (loop for line = (read-line stream nil nil)
            while line do
              (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
                (when (and (plusp (length trimmed))
                           (not (char= (char trimmed 0) #\#)))
                  (let ((colon-pos (position #\: trimmed)))
                    (when colon-pos
                      (let* ((key (string-trim '(#\Space #\Tab)
                                               (subseq trimmed 0 colon-pos)))
                             (raw-value (string-trim '(#\Space #\Tab)
                                                     (subseq trimmed (1+ colon-pos)))))
                        (when (plusp (length key))
                          (let ((list-value (%parse-yaml-list-value raw-value)))
                            (push (cons key (or list-value
                                                (string-trim '(#\") raw-value)))
                                  result))))))))))
    (nreverse result)))

(defun %yaml-alist-get (key alist)
  "Case-insensitive lookup in YAML alist."
  (cdr (assoc key alist :test #'string-equal)))

;;; --- Persona file parser ---

(defun parse-persona-file (path)
  "Parse a persona file at PATH -> persona-definition or NIL on error."
  (handler-case
      (let* ((text (uiop:read-file-string path :external-format :utf-8)))
        (multiple-value-bind (frontmatter body)
            (%extract-yaml-frontmatter text)
          (unless frontmatter
            (return-from parse-persona-file nil))
          (let* ((alist (%parse-yaml-frontmatter frontmatter))
                 (name (%yaml-alist-get "name" alist))
                 (description (%yaml-alist-get "description" alist)))
            (unless (and name (plusp (length name)))
              (return-from parse-persona-file nil))
            (unless (and description (plusp (length description)))
              (return-from parse-persona-file nil))
            (let ((capabilities (%yaml-alist-get "capabilities" alist))
                  (model (%yaml-alist-get "model" alist)))
              (%make-persona-definition
               :name name
               :description description
               :capabilities (if (listp capabilities) capabilities nil)
               :model (and (stringp model) (plusp (length model)) model)
               :system-prompt (string-trim '(#\Space #\Tab #\Newline #\Return)
                                           (or body ""))
               :file-path (pathname path)
               :scope :project)))))
    (error ()
      nil)))

;;; --- Persona discovery ---

(defun %persona-directory-files (directory)
  "Return list of .md files in DIRECTORY, or NIL if directory doesn't exist."
  (when (and directory (ignore-errors (probe-file directory)))
    (ignore-errors
      (uiop:directory-files
       (uiop:ensure-directory-pathname directory)
       "*.md"))))

(defun discover-persona-files (&key project-root)
  "Scan ~/.amoebum/agents/*.md and <root>/.amoebum/agents/*.md.
   Returns list of persona-definition structs, global first, then project.
   Project-scope personas override same-name globals."
  (let* ((home (user-homedir-pathname))
         (global-dir (merge-pathnames #P".amoebum/agents/" home))
         (root (or project-root
                   (ignore-errors (config-project-root (current-config)))
                   *default-pathname-defaults*))
         (project-dir (merge-pathnames #P".amoebum/agents/"
                                       (uiop:ensure-directory-pathname root)))
         (global-files (%persona-directory-files global-dir))
         (project-files (%persona-directory-files project-dir))
         (personas '())
         (seen-names (make-hash-table :test #'equal)))
    ;; Parse project files first (higher priority)
    (dolist (path project-files)
      (let ((persona (parse-persona-file path)))
        (when persona
          (setf (persona-definition-scope persona) :project)
          (setf (gethash (string-downcase (persona-definition-name persona))
                         seen-names)
                t)
          (push persona personas))))
    ;; Parse global files, skip if name already seen
    (dolist (path global-files)
      (let ((persona (parse-persona-file path)))
        (when persona
          (setf (persona-definition-scope persona) :global)
          (unless (gethash (string-downcase (persona-definition-name persona))
                           seen-names)
            (push persona personas)))))
    (nreverse personas)))

(defun find-persona-by-name (name personas)
  "Case-insensitive name lookup in PERSONAS list.
   Project scope overrides global if both exist."
  (let ((target (string-downcase (string-trim '(#\Space #\Tab) (or name "")))))
    (or (find-if (lambda (p)
                   (and (eq (persona-definition-scope p) :project)
                        (string= target
                                 (string-downcase (persona-definition-name p)))))
                 personas)
        (find-if (lambda (p)
                   (string= target
                            (string-downcase (persona-definition-name p))))
                 personas))))

(defun persona-manifest-lines (personas)
  "Format manifest for system prompt injection.
   Returns list of strings like:
   - code-reviewer (global): Expert at analyzing code..."
  (mapcar (lambda (p)
            (format nil "- ~A (~(~A~)): ~A"
                    (persona-definition-name p)
                    (persona-definition-scope p)
                    (persona-definition-description p)))
          personas))
