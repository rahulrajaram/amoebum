(in-package :amoebum)

;;;; Extension loader runtime — registry state, load orchestration, and
;;;; hot-reload watch loop.
;;;;
;;;; This file holds the residual orchestration after NXT-386 split out the
;;;; discovery, manifest-metadata, and permissions/sandbox-prep clusters into
;;;; sibling submodules:
;;;;   extensions/discovery.lisp        — file enumeration & path helpers
;;;;   extensions/manifest.lisp         — manifest parsing + metadata builders
;;;;   extensions/permissions-prep.lisp — permissions, sandbox, validation
;;;;
;;;; Public entry points (re-exported via :amoebum.extensions facade) preserved:
;;;;   load-user-extensions / reload-user-extensions
;;;;   check-extension-hot-reload / start-extension-hot-reload / stop-extension-hot-reload
;;;;   list-extensions / list-extension-registry / describe-extension
;;;;   enable-user-extension / disable-user-extension / extension-disabled-p
;;;;   list-extension-report / list-loaded-extensions / extension-report-summary
;;;;   known-user-extension-paths / known-user-extension-names
;;;;
;;;; The NXT-375 Result-pilot (%prepare-extension-load-attempt-result,
;;;; %process-extension-attempt-result, %process-extension-candidate-result)
;;;; is preserved verbatim — load-bearing for the second-seam Hulisti decision
;;;; in NXT-389.

(defparameter *extension-registry* (make-hash-table :test #'equal))
(defparameter *extension-watch-snapshot* (make-hash-table :test #'equal))
(defparameter *extension-hot-reload-enabled-p* t)
(defparameter *extension-hot-reload-interval-seconds* 1.0d0)
(defparameter *extension-hot-reload-thread* nil)
(defparameter *extension-hot-reload-running-p* nil)

(defstruct (extension-registry-entry
            (:constructor make-extension-registry-entry
                (&key name
                 version
                 dependencies
                 entry-point
                 manifest-path
                 extension-root
                 scope
                 permissions
                 package-name
                 enabled-p
                 tool-count
                 hook-count
                 status
                 loaded-at
                 last-write-date
                 message)))
  name
  version
  (dependencies '() :type list)
  entry-point
  manifest-path
  extension-root
  (scope :project :type keyword)
  (permissions '() :type list)
  package-name
  (enabled-p t :type boolean)
  (tool-count 0 :type integer)
  (hook-count 0 :type integer)
  (status :loaded :type keyword)
  (loaded-at 0 :type integer)
  last-write-date
  message)

(defstruct (extension-load-attempt
            (:constructor make-extension-load-attempt
                (&key scope
                 file
                 source-path
                 metadata
                 record-path
                 entry-point
                 extension-package
                 package-name)))
  scope
  file
  source-path
  metadata
  record-path
  entry-point
  extension-package
  package-name)

;;;; ---------------------------------------------------------------------
;;;; Read-side queries over registry / load report.

(defun extension-disabled-p (path)
  (not (null (gethash (%extension-key path) *disabled-extensions*))))

(defun list-extension-report ()
  (copy-list *extension-load-report*))

(defun list-loaded-extensions ()
  (copy-list *loaded-extensions*))

(defun list-extension-registry ()
  (let ((entries '()))
    (maphash (lambda (_key entry)
               (declare (ignore _key))
               (push entry entries))
             *extension-registry*)
    (sort entries #'string<
          :key (lambda (entry)
                 (string-downcase (or (extension-registry-entry-name entry) ""))))))

(defun %extension-entry->plist (entry)
  (let* ((manifest-path (extension-registry-entry-manifest-path entry))
         (entry-point (extension-registry-entry-entry-point entry))
         (path (or manifest-path entry-point)))
    (list :name (extension-registry-entry-name entry)
          :version (extension-registry-entry-version entry)
          :status (extension-registry-entry-status entry)
          :scope (extension-registry-entry-scope entry)
          :enabled-p (extension-registry-entry-enabled-p entry)
          :tool-count (extension-registry-entry-tool-count entry)
          :hook-count (extension-registry-entry-hook-count entry)
          :dependencies (copy-list (extension-registry-entry-dependencies entry))
          :path path
          :entry-point entry-point
          :manifest-path manifest-path
          :extension-root (extension-registry-entry-extension-root entry)
          :last-write-date (extension-registry-entry-last-write-date entry)
          :message (extension-registry-entry-message entry))))

(defun list-extensions ()
  (mapcar #'%extension-entry->plist
          (list-extension-registry)))

(defun %extension-designator-text (designator)
  (%extension-trim
   (cond
     ((null designator) "")
     ((stringp designator) designator)
     ((symbolp designator) (symbol-name designator))
     (t (princ-to-string designator)))))

(defun %extension-entry-matches-p (entry target)
  (let ((name (or (getf entry :name) ""))
        (path (or (getf entry :path) ""))
        (entry-point (or (getf entry :entry-point) ""))
        (manifest-path (or (getf entry :manifest-path) "")))
    (or (%extension-match-target-p target name)
        (%extension-match-target-p target path)
        (%extension-match-target-p target entry-point)
        (%extension-match-target-p target manifest-path))))

(defun describe-extension (designator)
  (let ((target (%extension-designator-text designator)))
    (when (plusp (length target))
      (find-if (lambda (entry)
                 (%extension-entry-matches-p entry target))
               (list-extensions)))))

(defun extension-report-summary (&optional (report *extension-load-report*))
  (let ((loaded 0)
        (errors 0)
        (disabled 0))
    (dolist (entry report)
      (case (extension-load-record-status entry)
        (:loaded (incf loaded))
        (:error (incf errors))
        (:disabled (incf disabled))))
    (list :total (length report)
          :loaded loaded
          :errors errors
          :disabled disabled)))

(defun known-user-extension-paths ()
  (let ((seen (make-hash-table :test #'equal))
        (paths '()))
    (labels ((remember (path-text)
               (let ((trimmed (%extension-trim path-text)))
                 (when (plusp (length trimmed))
                   (let ((key (string-downcase trimmed)))
                     (unless (gethash key seen)
                       (setf (gethash key seen) t)
                       (push trimmed paths)))))))
      (dolist (path-text *extension-last-discovered*)
        (remember path-text))
      (dolist (entry *extension-load-report*)
        (remember (extension-load-record-path entry))
        (remember (extension-load-record-manifest-path entry))))
    (nreverse paths)))

(defun known-user-extension-names ()
  (let ((seen (make-hash-table :test #'equal))
        (names '()))
    (labels ((remember (value)
               (let ((trimmed (%extension-trim value)))
                 (when (plusp (length trimmed))
                   (let ((key (string-downcase trimmed)))
                     (unless (gethash key seen)
                       (setf (gethash key seen) t)
                       (push trimmed names)))))))
      (dolist (entry (list-extension-registry))
        (remember (extension-registry-entry-name entry)))
      (dolist (entry *extension-load-report*)
        (remember (extension-load-record-name entry))))
    (nreverse names)))

;;;; ---------------------------------------------------------------------
;;;; Enable/disable.

(defun %disable-extension-path (path-text)
  (setf (gethash (%extension-key path-text) *disabled-extensions*) t))

(defun %enable-extension-path (path-text)
  (remhash (%extension-key path-text) *disabled-extensions*))

(defun %matched-registry-paths-by-target (target)
  (let ((paths '()))
    (dolist (entry (list-extension-registry))
      (let ((entry-name (extension-registry-entry-name entry))
            (entry-path (or (extension-registry-entry-manifest-path entry)
                            (extension-registry-entry-entry-point entry))))
        (when (and entry-path
                   (or (%extension-match-target-p target entry-path)
                       (%extension-match-target-p target entry-name)))
          (push entry-path paths))))
    (nreverse paths)))

(defun disable-user-extension (target)
  (let* ((trimmed (%extension-trim target))
         (known-paths (known-user-extension-paths))
         (disabled '()))
    (cond
      ((zerop (length trimmed))
       (values '() 0))
      ((string-equal trimmed "all")
       (dolist (path-text (append known-paths (%matched-registry-paths-by-target "all")))
         (%disable-extension-path path-text)
         (push path-text disabled))
       (values (remove-duplicates (nreverse disabled) :test #'string-equal)
               (length (remove-duplicates disabled :test #'string-equal))))
      (t
       (dolist (path-text known-paths)
         (when (%extension-match-target-p trimmed path-text)
           (%disable-extension-path path-text)
           (push path-text disabled)))
       (dolist (path-text (%matched-registry-paths-by-target trimmed))
         (%disable-extension-path path-text)
         (push path-text disabled))
       (when (and (null disabled) (probe-file trimmed))
         (let ((resolved (%canonical-extension-path trimmed)))
           (when (plusp (length resolved))
             (%disable-extension-path resolved)
             (push resolved disabled))))
       (let ((result (remove-duplicates (nreverse disabled) :test #'string-equal)))
         (values result (length result)))))))

(defun enable-user-extension (target)
  (let* ((trimmed (%extension-trim target))
         (known-paths (known-user-extension-paths))
         (enabled '()))
    (cond
      ((zerop (length trimmed))
       (values '() 0))
      ((string-equal trimmed "all")
       (dolist (path-text known-paths)
         (%enable-extension-path path-text)
         (push path-text enabled))
       (values (remove-duplicates (nreverse enabled) :test #'string-equal)
               (length (remove-duplicates enabled :test #'string-equal))))
      (t
       (dolist (path-text known-paths)
         (when (%extension-match-target-p trimmed path-text)
           (%enable-extension-path path-text)
           (push path-text enabled)))
       (dolist (path-text (%matched-registry-paths-by-target trimmed))
         (%enable-extension-path path-text)
         (push path-text enabled))
       (when (and (null enabled) (probe-file trimmed))
         (let ((resolved (%canonical-extension-path trimmed)))
           (when (plusp (length resolved))
             (%enable-extension-path resolved)
             (push resolved enabled))))
       (let ((result (remove-duplicates (nreverse enabled) :test #'string-equal)))
         (values result (length result)))))))

;;;; ---------------------------------------------------------------------
;;;; Event publication helpers.

(defun %publish-extension-loaded (path scope)
  (publish (current-event-bus)
           (make-extension-loaded-event :path path :scope scope)))

(defun %publish-extension-error (path scope condition-text)
  (publish (current-event-bus)
           (make-extension-error-event :path path
                                       :scope scope
                                       :condition condition-text)))

;;;; ---------------------------------------------------------------------
;;;; Entry-point loading & registry write.

(defun %load-extension-file-isolated (resolved-entry-point metadata extension-package)
  (let ((extension-name (or (getf metadata :name) "unknown-extension")))
    (unless (probe-file resolved-entry-point)
      (error "Extension ~A entry-point not found: ~A."
             extension-name
             resolved-entry-point))
    (with-open-file (stream resolved-entry-point :direction :input :external-format :utf-8)
      (let ((*package* extension-package)
            (*read-eval* nil))
        (loop for form = (read stream nil :eof)
              until (eq form :eof)
              do (progn
                   (%validate-extension-form form metadata extension-package)
                   (eval form))))))
  :isolated-load)

(defun %load-entry-point (resolved-entry-point metadata extension-package)
  (cond
    ((pathnamep resolved-entry-point)
     (if (eq (getf metadata :kind) :manifest)
         (%load-extension-file-isolated resolved-entry-point metadata extension-package)
         (progn
           (load resolved-entry-point :verbose nil :print nil)
           :direct-load)))
    ((stringp resolved-entry-point)
     (asdf:load-system resolved-entry-point)
     :asdf)
    (t
     (error "Unsupported entry-point type ~S." resolved-entry-point))))

(defun %register-extension (metadata scope status message &key extension-package)
  (let* ((name (getf metadata :name))
         (key (%extension-registry-key name))
         (entry-point (%resolve-entry-point metadata))
         (tool-count 0)
         (hook-count 0)
         (manifest-path (getf metadata :manifest-path))
         (recorded-path (or manifest-path
                            (and (pathnamep entry-point)
                                 (%canonical-extension-path entry-point))
                            (%ensure-string entry-point)))
         (entry (make-extension-registry-entry
                 :name name
                 :version (getf metadata :version)
                 :dependencies (copy-list (getf metadata :dependencies))
                 :entry-point (if (pathnamep entry-point)
                                  (%canonical-extension-path entry-point)
                                  (%ensure-string entry-point))
                 :manifest-path (and manifest-path (%canonical-extension-path manifest-path))
                 :extension-root (%canonical-extension-path (getf metadata :extension-root))
                 :scope scope
                 :permissions (copy-list (or (getf metadata :permissions) '()))
                 :package-name (and extension-package (package-name extension-package))
                 :enabled-p (not (extension-disabled-p recorded-path))
                 :tool-count tool-count
                 :hook-count hook-count
                 :status status
                 :loaded-at (get-universal-time)
                 :last-write-date (getf metadata :last-write-date)
                 :message message)))
    (multiple-value-setq (tool-count hook-count)
      (%resolve-definition-counts metadata entry-point))
    (setf (extension-registry-entry-tool-count entry) tool-count
          (extension-registry-entry-hook-count entry) hook-count)
    (setf (gethash key *extension-registry*) entry)
    entry))

;;;; ---------------------------------------------------------------------
;;;; Per-candidate orchestration (NXT-375 Result pilot lives here).

(defun %make-extension-load-attempt-for-metadata (scope file metadata)
  (let* ((entry-point (%resolve-entry-point metadata))
         (extension-package (%metadata-extension-package metadata)))
    (make-extension-load-attempt
     :scope scope
     :file file
     :source-path (%canonical-extension-path file)
     :metadata metadata
     :record-path (%metadata-record-path metadata)
     :entry-point entry-point
     :extension-package extension-package
     :package-name (and extension-package
                        (package-name extension-package)))))

(defun %prepare-extension-load-attempt (scope file)
  (%make-extension-load-attempt-for-metadata scope
                                             file
                                             (%source-file->metadata file)))

(defun %prepare-extension-load-attempt-result (scope file)
  (handler-case
      (amoebum.fp:make-ok :value (%prepare-extension-load-attempt scope file))
    (error (condition)
      (amoebum.fp:make-err :value condition))))

(defun %recover-extension-load-attempt (scope file)
  (let* ((metadata (%fallback-error-metadata file))
         (extension-package (and (eq (getf metadata :kind) :manifest)
                                 (ignore-errors (%ensure-extension-package metadata))))
         (entry-point (%resolve-entry-point metadata)))
    (make-extension-load-attempt
     :scope scope
     :file file
     :source-path (%canonical-extension-path file)
     :metadata metadata
     :record-path (%metadata-record-path metadata)
     :entry-point entry-point
     :extension-package extension-package
     :package-name (and extension-package
                        (package-name extension-package)))))

(defun %attempt-entry-point-text (attempt)
  (let ((entry-point (extension-load-attempt-entry-point attempt)))
    (if (pathnamep entry-point)
        (%canonical-extension-path entry-point)
        (%ensure-string entry-point))))

(defun %attempt-manifest-path-text (attempt)
  (let ((manifest-path (getf (extension-load-attempt-metadata attempt) :manifest-path)))
    (and manifest-path
         (%canonical-extension-path manifest-path))))

(defun %make-extension-load-record-from-attempt (attempt status &key message)
  (let ((metadata (extension-load-attempt-metadata attempt)))
    (make-extension-load-record
     :path (extension-load-attempt-record-path attempt)
     :scope (extension-load-attempt-scope attempt)
     :name (getf metadata :name)
     :version (getf metadata :version)
     :dependencies (copy-list (or (getf metadata :dependencies) '()))
     :permissions (copy-list (or (getf metadata :permissions) '()))
     :package-name (extension-load-attempt-package-name attempt)
     :entry-point (%attempt-entry-point-text attempt)
     :manifest-path (%attempt-manifest-path-text attempt)
     :status status
     :message message)))

(defun %register-extension-attempt (attempt status message)
  (%register-extension (extension-load-attempt-metadata attempt)
                       (extension-load-attempt-scope attempt)
                       status
                       message
                       :extension-package (extension-load-attempt-extension-package attempt)))

(defun %extension-attempt-disabled-p (attempt)
  (extension-disabled-p (extension-load-attempt-record-path attempt)))

(defun %handle-disabled-extension-attempt (attempt)
  (%register-extension-attempt attempt :disabled "disabled by /extensions disable")
  (values (%make-extension-load-record-from-attempt attempt
                                                    :disabled
                                                    :message "disabled by /extensions disable")
          nil))

(defun %load-enabled-extension-attempt (attempt)
  (let ((metadata (extension-load-attempt-metadata attempt)))
    (when (eq (getf metadata :kind) :manifest)
      (%ensure-extension-permissions-approved metadata
                                             (extension-load-attempt-scope attempt)))
    (%load-entry-point (extension-load-attempt-entry-point attempt)
                       metadata
                       (extension-load-attempt-extension-package attempt))
    (%register-extension-attempt attempt :loaded nil)
    (%publish-extension-loaded (extension-load-attempt-record-path attempt)
                               (extension-load-attempt-scope attempt))
    (let ((record (%make-extension-load-record-from-attempt attempt :loaded)))
      (values record record))))

(defun %process-extension-attempt-result (attempt)
  (if (%extension-attempt-disabled-p attempt)
      (amoebum.fp:make-ok
       :value (multiple-value-list (%handle-disabled-extension-attempt attempt)))
      (handler-case
          (amoebum.fp:make-ok
           :value (multiple-value-list (%load-enabled-extension-attempt attempt)))
        (error (condition)
          (amoebum.fp:make-err :value condition)))))

(defun %process-extension-candidate-result (candidate)
  (let ((scope (car candidate))
        (file (cdr candidate)))
    (amoebum.fp:result-bind
     (%prepare-extension-load-attempt-result scope file)
     #'%process-extension-attempt-result)))

(defun %handle-extension-load-error (candidate condition)
  (let* ((scope (car candidate))
         (file (cdr candidate))
         (attempt (%recover-extension-load-attempt scope file))
         (message (princ-to-string condition)))
    (format *error-output*
            "Extension load failed (~A): ~A~%"
            (extension-load-attempt-source-path attempt)
            message)
    (%publish-extension-error (extension-load-attempt-source-path attempt)
                              scope
                              message)
    (%register-extension-attempt attempt :error message)
    (values (%make-extension-load-record-from-attempt attempt
                                                      :error
                                                      :message message)
            nil)))

(defun %process-extension-candidate (candidate)
  (let ((result (%process-extension-candidate-result candidate)))
    (if (amoebum.fp:ok-p result)
        (values-list (amoebum.fp:ok-value result))
        (%handle-extension-load-error candidate
                                      (amoebum.fp:err-value result)))))

;;;; ---------------------------------------------------------------------
;;;; Top-level coordinator state machine.

(defun %reset-extension-loader-state (candidates)
  (clrhash *extension-registry*)
  (setf *extension-last-discovered*
        (mapcar (lambda (entry)
                  (%canonical-extension-path (cdr entry)))
                candidates)))

(defun %finalize-extension-loader-state (report loaded
                                         &key project-root global-directory project-directory
                                           start-hot-reload)
  (setf *extension-load-report* (nreverse report)
        *loaded-extensions* (nreverse loaded))
  (%rebuild-extension-watch-snapshot)
  (if start-hot-reload
      (start-extension-hot-reload :project-root project-root
                                  :global-directory global-directory
                                  :project-directory project-directory)
      (stop-extension-hot-reload))
  *extension-load-report*)

(defun %collect-extension-candidates (&key project-root global-directory project-directory)
  (multiple-value-bind (global-files project-files)
      (discover-user-extension-files :project-root project-root
                                     :global-directory global-directory
                                     :project-directory project-directory)
    (append (loop for file in global-files collect (cons :global file))
            (loop for file in project-files collect (cons :project file)))))

;; %REBUILD-EXTENSION-WATCH-SNAPSHOT moved to extensions/hot-reload.lisp
;; (NXT-387). It owns *EXTENSION-WATCH-SNAPSHOT* alongside the watch
;; thread; %FINALIZE-EXTENSION-LOADER-STATE invokes it as a forward
;; reference (resolved at runtime when load-user-extensions runs).

(defun load-user-extensions (&key project-root global-directory project-directory
                                  (start-hot-reload *extension-hot-reload-enabled-p*))
  (let* ((candidates (%collect-extension-candidates :project-root project-root
                                                    :global-directory global-directory
                                                    :project-directory project-directory))
         (report '())
         (loaded '()))
    (%reset-extension-loader-state candidates)
    (dolist (entry candidates)
      (multiple-value-bind (record loaded-record)
          (handler-case
              (%process-extension-candidate entry)
            (error (condition)
              (%handle-extension-load-error entry condition)))
        (push record report)
        (when loaded-record
          (push loaded-record loaded))))
    (%finalize-extension-loader-state report
                                      loaded
                                      :project-root project-root
                                      :global-directory global-directory
                                      :project-directory project-directory
                                      :start-hot-reload start-hot-reload)))

(defun reload-user-extensions (&key project-root global-directory project-directory
                                    (start-hot-reload *extension-hot-reload-enabled-p*))
  (load-user-extensions :project-root project-root
                        :global-directory global-directory
                        :project-directory project-directory
                        :start-hot-reload start-hot-reload))

;; CHECK-EXTENSION-HOT-RELOAD, START-EXTENSION-HOT-RELOAD, and
;; STOP-EXTENSION-HOT-RELOAD moved to extensions/hot-reload.lisp
;; (NXT-387). Public API is preserved; the watch-thread runtime now
;; lives in its own module so loader.lisp can stay focused on the
;; registry, load-orchestration, and Result pilot.
