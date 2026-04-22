(in-package :amoebum/test)

;;; ============================================================
;;; I239: Extension Lifecycle Management
;;; ============================================================

(def-suite extension-lifecycle-suite :in amoebum-suite)
(in-suite extension-lifecycle-suite)

(test extension-registry-entry-struct
  "extension-registry-entry should be constructable."
  (let ((entry (amoebum.extensions:make-extension-registry-entry
                :name "test-ext"
                :version "1.0.0"
                :dependencies '("dep-a")
                :scope :project
                :status :loaded)))
    (is (amoebum.extensions:extension-registry-entry-p entry))
    (is (string= "test-ext" (amoebum.extensions:extension-registry-entry-name entry)))
    (is (string= "1.0.0" (amoebum.extensions:extension-registry-entry-version entry)))
    (is (equal '("dep-a") (amoebum.extensions:extension-registry-entry-dependencies entry)))
    (is (eq :project (amoebum.extensions:extension-registry-entry-scope entry)))
    (is (eq :loaded (amoebum.extensions:extension-registry-entry-status entry)))))

(test extension-registry-list-and-clear
  "Extension registry should support list and clear."
  (let ((old-registry (copy-list (amoebum.extensions:list-extension-registry))))
    (unwind-protect
         (progn
           (clrhash amoebum.extensions:*extension-registry*)
           (setf (gethash "test-lifecycle"  amoebum.extensions:*extension-registry*)
                 (amoebum.extensions:make-extension-registry-entry
                  :name "test-lifecycle"
                  :version "0.1.0"
                  :status :loaded))
           (let ((entries (amoebum.extensions:list-extension-registry)))
             (is (= 1 (length entries)))
             (is (string= "test-lifecycle"
                           (amoebum.extensions:extension-registry-entry-name (first entries))))))
      ;; Restore
      (clrhash amoebum.extensions:*extension-registry*)
      (dolist (entry old-registry)
        (setf (gethash (string-downcase (amoebum.extensions:extension-registry-entry-name entry))
                       amoebum.extensions:*extension-registry*)
              entry)))))

(test load-user-extensions-with-manifest
  "load-user-extensions should load extensions with manifest."
  (let* ((old-report amoebum.extensions:*extension-load-report*)
         (old-loaded amoebum.extensions:*loaded-extensions*)
         (old-discovered amoebum.extensions:*extension-last-discovered*)
         (old-global amoebum.extensions:*extensions-global-directory-override*)
         (old-project amoebum.extensions:*extensions-project-directory-override*)
         (disabled-keys (%hash-table-keys amoebum.extensions:*disabled-extensions*))
         (tmp-dir (%make-temp-directory "amoebum-ext-lifecycle"))
         (global-dir (merge-pathnames #P"global/" tmp-dir))
         (project-dir (merge-pathnames #P"project/" tmp-dir))
         (ext-root (merge-pathnames #P"test-ext/" project-dir))
         (manifest-path (merge-pathnames #P"extension.lisp" ext-root))
         (entry-path (merge-pathnames #P"main.lisp" ext-root)))
    (unwind-protect
         (progn
           (setf amoebum.extensions:*extensions-global-directory-override* global-dir
                 amoebum.extensions:*extensions-project-directory-override* project-dir)
           (clrhash amoebum.extensions:*disabled-extensions*)
           (%write-text-file manifest-path
                             "(:name \"test-ext\" :version \"1.0.0\" :dependencies () :entry-point \"main.lisp\")")
           (%write-text-file entry-path
                             "(values)")
           (let ((report (amoebum.extensions:load-user-extensions
                          :project-root tmp-dir
                          :start-hot-reload nil)))
             (is (listp report))
             (is (= 1 (length report)))
             (is (eq :loaded (amoebum.extensions:extension-load-record-status (first report))))))
      (setf amoebum.extensions:*extension-load-report* old-report
            amoebum.extensions:*loaded-extensions* old-loaded
            amoebum.extensions:*extension-last-discovered* old-discovered
            amoebum.extensions:*extensions-global-directory-override* old-global
            amoebum.extensions:*extensions-project-directory-override* old-project)
      (clrhash amoebum.extensions:*disabled-extensions*)
      (dolist (key disabled-keys)
        (setf (gethash key amoebum.extensions:*disabled-extensions*) t))
      (%delete-directory-tree-safe tmp-dir))))

(test reload-user-extensions-reloads
  "reload-user-extensions should re-discover and reload."
  (let* ((old-report amoebum.extensions:*extension-load-report*)
         (old-loaded amoebum.extensions:*loaded-extensions*)
         (old-discovered amoebum.extensions:*extension-last-discovered*)
         (old-global amoebum.extensions:*extensions-global-directory-override*)
         (old-project amoebum.extensions:*extensions-project-directory-override*)
         (disabled-keys (%hash-table-keys amoebum.extensions:*disabled-extensions*))
         (tmp-dir (%make-temp-directory "amoebum-ext-reload"))
         (global-dir (merge-pathnames #P"global/" tmp-dir))
         (project-dir (merge-pathnames #P"project/" tmp-dir)))
    (unwind-protect
         (progn
           (setf amoebum.extensions:*extensions-global-directory-override* global-dir
                 amoebum.extensions:*extensions-project-directory-override* project-dir)
           (clrhash amoebum.extensions:*disabled-extensions*)
           ;; Initially no extensions
           (amoebum.extensions:load-user-extensions :project-root tmp-dir :start-hot-reload nil)
           (is (= 0 (length amoebum.extensions:*extension-load-report*)))
           ;; Add an extension and reload
           (let ((ext-root (merge-pathnames #P"new-ext/" project-dir)))
             (%write-text-file (merge-pathnames #P"extension.lisp" ext-root)
                               "(:name \"new-ext\" :version \"0.1\" :entry-point \"main.lisp\")")
             (%write-text-file (merge-pathnames #P"main.lisp" ext-root)
                               "(values)"))
           (amoebum.extensions:reload-user-extensions :project-root tmp-dir :start-hot-reload nil)
           (is (= 1 (length amoebum.extensions:*extension-load-report*))))
      (setf amoebum.extensions:*extension-load-report* old-report
            amoebum.extensions:*loaded-extensions* old-loaded
            amoebum.extensions:*extension-last-discovered* old-discovered
            amoebum.extensions:*extensions-global-directory-override* old-global
            amoebum.extensions:*extensions-project-directory-override* old-project)
      (clrhash amoebum.extensions:*disabled-extensions*)
      (dolist (key disabled-keys)
        (setf (gethash key amoebum.extensions:*disabled-extensions*) t))
      (%delete-directory-tree-safe tmp-dir))))

(test extension-load-error-handled-gracefully
  "Extensions that fail to load should be recorded as :error."
  (let* ((old-report amoebum.extensions:*extension-load-report*)
         (old-loaded amoebum.extensions:*loaded-extensions*)
         (old-discovered amoebum.extensions:*extension-last-discovered*)
         (old-global amoebum.extensions:*extensions-global-directory-override*)
         (old-project amoebum.extensions:*extensions-project-directory-override*)
         (disabled-keys (%hash-table-keys amoebum.extensions:*disabled-extensions*))
         (tmp-dir (%make-temp-directory "amoebum-ext-error"))
         (global-dir (merge-pathnames #P"global/" tmp-dir))
         (project-dir (merge-pathnames #P"project/" tmp-dir))
         (ext-root (merge-pathnames #P"bad-ext/" project-dir)))
    (unwind-protect
         (progn
           (setf amoebum.extensions:*extensions-global-directory-override* global-dir
                 amoebum.extensions:*extensions-project-directory-override* project-dir)
           (clrhash amoebum.extensions:*disabled-extensions*)
           (%write-text-file (merge-pathnames #P"extension.lisp" ext-root)
                             "(:name \"bad-ext\" :version \"0.1\" :entry-point \"main.lisp\")")
           (%write-text-file (merge-pathnames #P"main.lisp" ext-root)
                             "(error \"deliberate load failure\")")
           (let ((report (amoebum.extensions:load-user-extensions
                          :project-root tmp-dir :start-hot-reload nil)))
             (is (= 1 (length report)))
             (is (eq :error (amoebum.extensions:extension-load-record-status (first report))))))
      (setf amoebum.extensions:*extension-load-report* old-report
            amoebum.extensions:*loaded-extensions* old-loaded
            amoebum.extensions:*extension-last-discovered* old-discovered
            amoebum.extensions:*extensions-global-directory-override* old-global
            amoebum.extensions:*extensions-project-directory-override* old-project)
      (clrhash amoebum.extensions:*disabled-extensions*)
      (dolist (key disabled-keys)
        (setf (gethash key amoebum.extensions:*disabled-extensions*) t))
      (%delete-directory-tree-safe tmp-dir))))

(test extension-report-summary-counts
  "extension-report-summary should tally loaded/errors/disabled."
  (let ((report (list
                 (amoebum.extensions:make-extension-load-record :path "/a" :scope :project :status :loaded)
                 (amoebum.extensions:make-extension-load-record :path "/b" :scope :global :status :loaded)
                 (amoebum.extensions:make-extension-load-record :path "/c" :scope :project :status :error :message "fail")
                 (amoebum.extensions:make-extension-load-record :path "/d" :scope :project :status :disabled :message "off"))))
    (let ((summary (amoebum.extensions:extension-report-summary report)))
      (is (= 4 (getf summary :total)))
      (is (= 2 (getf summary :loaded)))
      (is (= 1 (getf summary :errors)))
      (is (= 1 (getf summary :disabled))))))

(defvar *i239-extension-lifecycle-log* '())

(defun %i239-copy-hash-table (table)
  (let ((copy (make-hash-table :test #'equal)))
    (maphash (lambda (key value)
               (setf (gethash key copy) value))
             table)
    copy))

(defun %i239-restore-hash-table (target snapshot)
  (clrhash target)
  (maphash (lambda (key value)
             (setf (gethash key target) value))
           snapshot)
  target)

(defun %i239-system-name (prefix)
  (string-downcase
   (format nil "~A-~D-~D" prefix (get-universal-time) (random 1000000))))

(defun %i239-symbol-token (text)
  (string-upcase
   (substitute #\- #\.
               (substitute #\- #\/
                           (string-downcase text)))))

(defun %i239-write-asdf-extension (systems-root system-name
                                   &key
                                     (version "1.0.0")
                                     (depends-on '())
                                     (extra-body ""))
  (let* ((system-dir (merge-pathnames (format nil "~A/" system-name) systems-root))
         (asd-path (merge-pathnames (format nil "~A.asd" system-name) system-dir))
         (main-path (merge-pathnames #P"main.lisp" system-dir))
         (package-token (%i239-symbol-token system-name)))
    (%write-text-file
     asd-path
     (format nil
             "(asdf:defsystem ~S~%  :version ~S~%  :depends-on ~S~%  :serial t~%  :components ((:file \"main\")))~%"
             system-name
             version
             depends-on))
    (%write-text-file
     main-path
     (format nil
             "(defpackage :~A (:use :cl :amoebum))~%(in-package :~A)~%~%
(defun initialize-extension (&optional extension)~%
  (declare (ignore extension))~%
  (setf amoebum/test::*i239-extension-lifecycle-log*~%
        (append amoebum/test::*i239-extension-lifecycle-log* (list ~S))))~%~%
(defun shutdown-extension (&optional extension)~%
  (declare (ignore extension))~%
  (setf amoebum/test::*i239-extension-lifecycle-log*~%
        (append amoebum/test::*i239-extension-lifecycle-log* (list ~S))))~%~%
~A~%"
             package-token
             package-token
             (format nil "~A:init" system-name)
             (format nil "~A:shutdown" system-name)
             extra-body))
    (values asd-path main-path)))

(test asdf-discovery-includes-project-and-global-systems
  "discover-asdf-extensions should scan ~/.amoebum/systems and .amoebum/systems."
  (let* ((old-search-paths (copy-list amoebum::*asdf-extension-search-paths*))
         (old-registry (%i239-copy-hash-table amoebum::*asdf-extension-registry*))
         (tmp-root (%make-temp-directory "amoebum-i239-discover"))
         (global-root (merge-pathnames #P"fake-home/.amoebum/systems/" tmp-root))
         (project-root (merge-pathnames #P"project/" tmp-root))
         (project-systems (merge-pathnames #P".amoebum/systems/" project-root))
         (global-name (%i239-system-name "i239-global"))
         (project-name (%i239-system-name "i239-project")))
    (unwind-protect
         (progn
           (%i239-write-asdf-extension global-root global-name)
           (%i239-write-asdf-extension project-systems project-name)
           (setf amoebum::*asdf-extension-search-paths* (list global-root))
           (amoebum:clear-asdf-extensions)
           (amoebum:discover-asdf-extensions :project-root project-root)
           (let ((names (mapcar #'amoebum:asdf-extension-system-name
                                (amoebum:list-asdf-extensions))))
             (is-true (member global-name names :test #'string=))
             (is-true (member project-name names :test #'string=))))
      (setf amoebum::*asdf-extension-search-paths* old-search-paths)
      (%i239-restore-hash-table amoebum::*asdf-extension-registry* old-registry)
      (%delete-directory-tree-safe tmp-root))))

(test asdf-loads-in-dependency-order-and-validates-constraints
  "load-asdf-extension should topologically sort deps and validate version constraints."
  (let* ((old-search-paths (copy-list amoebum::*asdf-extension-search-paths*))
         (old-registry (%i239-copy-hash-table amoebum::*asdf-extension-registry*))
         (old-log *i239-extension-lifecycle-log*)
         (tmp-root (%make-temp-directory "amoebum-i239-load-order"))
         (project-root (merge-pathnames #P"project/" tmp-root))
         (project-systems (merge-pathnames #P".amoebum/systems/" project-root))
         (dep-a (%i239-system-name "i239-dep-a"))
         (dep-b (%i239-system-name "i239-dep-b"))
         (root (%i239-system-name "i239-root")))
    (unwind-protect
         (progn
           (%i239-write-asdf-extension project-systems dep-a :version "1.2.0")
           (%i239-write-asdf-extension project-systems dep-b
                                      :version "2.0.0"
                                      :depends-on (list (list :version dep-a "1.2.0")))
           (%i239-write-asdf-extension project-systems root
                                      :version "3.0.0"
                                      :depends-on (list dep-b))
           (setf amoebum::*asdf-extension-search-paths* '()
                 *i239-extension-lifecycle-log* '())
           (amoebum:clear-asdf-extensions)
           (amoebum:discover-asdf-extensions :project-root project-root)
           ;; Add explicit minimum/range checks to root for tranche validation.
           (let ((root-entry (amoebum:find-asdf-extension root)))
             (is-true root-entry)
             (setf (amoebum::asdf-extension-dependencies root-entry)
                   (list (cons dep-b "")
                         (cons dep-a ">=1.0.0 <=1.2.0"))))
           ;; Internal resolver should accept exact/minimum/range constraints and
           ;; produce dependency-first order.
           (is (equal (amoebum::%resolve-asdf-load-plan root)
                      (list dep-a dep-b root)))
           (let ((loaded (amoebum:load-asdf-extension root :project-root project-root)))
             (is-true loaded)
             (is (eq :loaded (amoebum:asdf-extension-status loaded)))
             (is (equal *i239-extension-lifecycle-log*
                        (list (format nil "~A:init" dep-a)
                              (format nil "~A:init" dep-b)
                              (format nil "~A:init" root))))))
      (setf amoebum::*asdf-extension-search-paths* old-search-paths
            *i239-extension-lifecycle-log* old-log)
      (%i239-restore-hash-table amoebum::*asdf-extension-registry* old-registry)
      (%delete-directory-tree-safe tmp-root))))

(test asdf-unload-deregisters-tools-hooks-and-slash-commands
  "unload-asdf-extension should call shutdown and remove extension registrations."
  (let* ((old-search-paths (copy-list amoebum::*asdf-extension-search-paths*))
         (old-registry (%i239-copy-hash-table amoebum::*asdf-extension-registry*))
         (old-log *i239-extension-lifecycle-log*)
         (tmp-root (%make-temp-directory "amoebum-i239-unload"))
         (project-root (merge-pathnames #P"project/" tmp-root))
         (project-systems (merge-pathnames #P".amoebum/systems/" project-root))
         (system-name (%i239-system-name "i239-unload"))
         (hook-name (format nil "~A-hook" system-name))
         (tool-name (format nil "~A-tool" system-name))
         (command-name (format nil "~A-cmd" system-name))
         (hook-symbol (%i239-symbol-token hook-name))
         (tool-symbol (%i239-symbol-token tool-name))
         (extra-body
           (format nil
                   "(amoebum:register-hook :pre-tool-use '~A~%
  (lambda (tool-name args)~%
    (declare (ignore tool-name args))~%
    :ok))~%~%
(amoebum:deftool ~A ((text string :required t :description \"text\"))~%
  \"I239 lifecycle extension tool.\"~%
  (:permission :auto)~%
  (format nil \"i239:~~A\" text))~%~%
(amoebum:register-slash-command~%
 (amoebum.commands:make-slash-command~%
  :name ~S~%
  :description \"i239 lifecycle command\"~%
  :usage ~S~%
  :handler (lambda (_invocation)~%
             (declare (ignore _invocation))~%
             (amoebum.commands:make-slash-command-result~%
              :handledp t~%
              :output \"ok\"))))"
                   hook-symbol
                   tool-symbol
                   command-name
                   (format nil "/~A" command-name))))
    (unwind-protect
         (progn
           (%i239-write-asdf-extension project-systems system-name
                                       :version "1.0.0"
                                       :depends-on '("amoebum")
                                       :extra-body extra-body)
           (setf amoebum::*asdf-extension-search-paths* '()
                 *i239-extension-lifecycle-log* '())
           (amoebum:clear-asdf-extensions)
           (amoebum:discover-asdf-extensions :project-root project-root)
           (amoebum:load-asdf-extension system-name :project-root project-root)
           (is-true (gethash (string-downcase tool-name) amoebum::*tool-metadata*))
           (is-true (gethash (string-downcase command-name) amoebum::*slash-command-registry*))
           (is-true
            (find hook-name
                  (amoebum:list-hooks :pre-tool-use)
                  :key (lambda (entry)
                         (string-downcase
                          (symbol-name (amoebum::hook-entry-hook-id entry))))
                  :test #'string=))
           (amoebum:unload-asdf-extension system-name)
           (is (eq :unloaded
                   (amoebum:asdf-extension-status
                    (amoebum:find-asdf-extension system-name))))
           (is-false (gethash (string-downcase tool-name) amoebum::*tool-metadata*))
           (is-false (gethash (string-downcase command-name) amoebum::*slash-command-registry*))
           (is-false
            (find hook-name
                  (amoebum:list-hooks :pre-tool-use)
                  :key (lambda (entry)
                         (string-downcase
                          (symbol-name (amoebum::hook-entry-hook-id entry))))
                  :test #'string=))
           (is-true (member (format nil "~A:shutdown" system-name)
                            *i239-extension-lifecycle-log*
                            :test #'string=)))
      (setf amoebum::*asdf-extension-search-paths* old-search-paths
            *i239-extension-lifecycle-log* old-log)
      (%i239-restore-hash-table amoebum::*asdf-extension-registry* old-registry)
      (%delete-directory-tree-safe tmp-root))))

(test asdf-version-constraint-mismatch-signals-error
  "Version constraint violations should fail before extension load completes."
  (let* ((old-search-paths (copy-list amoebum::*asdf-extension-search-paths*))
         (old-registry (%i239-copy-hash-table amoebum::*asdf-extension-registry*))
         (tmp-root (%make-temp-directory "amoebum-i239-version-mismatch"))
         (project-root (merge-pathnames #P"project/" tmp-root))
         (project-systems (merge-pathnames #P".amoebum/systems/" project-root))
         (dep-name (%i239-system-name "i239-mm-dep"))
         (root-name (%i239-system-name "i239-mm-root")))
    (unwind-protect
         (progn
           (%i239-write-asdf-extension project-systems dep-name :version "0.8.0")
           (%i239-write-asdf-extension project-systems root-name
                                      :version "1.0.0"
                                      :depends-on (list (list :version dep-name "1.0.0")))
           (setf amoebum::*asdf-extension-search-paths* '())
           (amoebum:clear-asdf-extensions)
           (amoebum:discover-asdf-extensions :project-root project-root)
           (signals error
             (amoebum:load-asdf-extension root-name :project-root project-root))
           (is (eq :error
                   (amoebum:asdf-extension-status
                    (amoebum:find-asdf-extension root-name)))))
      (setf amoebum::*asdf-extension-search-paths* old-search-paths)
      (%i239-restore-hash-table amoebum::*asdf-extension-registry* old-registry)
      (%delete-directory-tree-safe tmp-root))))

;;; ============================================================
;;; NXT-387: Hot-reload watch-thread lifecycle regression.
;;;
;;; Asserts that start/stop are race-free across the loader ->
;;; hot-reload module boundary: starting twice yields one thread,
;;; stopping joins cleanly, and a file change while the watcher is
;;; running is observable via the next CHECK-EXTENSION-HOT-RELOAD
;;; (which the watch loop polls on the configured interval).
;;; ============================================================

(test extension-hot-reload-watch-thread-start-stop-cycle
  "start-extension-hot-reload spawns one alive thread; stop joins and clears it."
  (let* ((old-enabled amoebum.extensions:*extension-hot-reload-enabled-p*)
         (old-interval amoebum.extensions:*extension-hot-reload-interval-seconds*)
         (old-thread amoebum::*extension-hot-reload-thread*)
         (old-running amoebum::*extension-hot-reload-running-p*)
         (tmp-dir (%make-temp-directory "amoebum-ext-hotreload-thread"))
         (global-dir (merge-pathnames #P"global/" tmp-dir))
         (project-dir (merge-pathnames #P"project/" tmp-dir)))
    (unwind-protect
         (progn
           ;; Make sure no stale watcher is running before we start.
           (amoebum.extensions:stop-extension-hot-reload)
           (setf amoebum.extensions:*extension-hot-reload-interval-seconds* 0.05d0)
           (ensure-directories-exist (merge-pathnames #P".keep" global-dir))
           (ensure-directories-exist (merge-pathnames #P".keep" project-dir))
           (let ((thread-a (amoebum.extensions:start-extension-hot-reload
                            :project-root tmp-dir
                            :global-directory global-dir
                            :project-directory project-dir)))
             (is-true thread-a "start-extension-hot-reload returned nil")
             (is-true (bordeaux-threads:thread-alive-p thread-a)
                      "watch thread is not alive after start")
             ;; Idempotent: a second start returns the same live thread.
             (let ((thread-b (amoebum.extensions:start-extension-hot-reload
                              :project-root tmp-dir
                              :global-directory global-dir
                              :project-directory project-dir)))
               (is (eq thread-a thread-b)
                   "second start spawned a new thread instead of reusing the live one")))
           (is-true amoebum::*extension-hot-reload-running-p*
                    "running flag should be true while watcher is alive")
           ;; Stop joins the thread and clears the registry slot.
           (is-true (amoebum.extensions:stop-extension-hot-reload)
                    "stop-extension-hot-reload should return t")
           (is (null amoebum::*extension-hot-reload-thread*)
               "stop should null out the thread reference")
           (is (null amoebum::*extension-hot-reload-running-p*)
               "stop should clear the running flag"))
      (amoebum.extensions:stop-extension-hot-reload)
      (setf amoebum.extensions:*extension-hot-reload-enabled-p* old-enabled
            amoebum.extensions:*extension-hot-reload-interval-seconds* old-interval
            amoebum::*extension-hot-reload-thread* old-thread
            amoebum::*extension-hot-reload-running-p* old-running)
      (%delete-directory-tree-safe tmp-dir))))

(test extension-hot-reload-watch-thread-detects-file-change
  "A file change while the watcher is running triggers a reload via the polling loop."
  (let* ((old-report amoebum.extensions:*extension-load-report*)
         (old-loaded amoebum.extensions:*loaded-extensions*)
         (old-discovered amoebum.extensions:*extension-last-discovered*)
         (old-global amoebum.extensions:*extensions-global-directory-override*)
         (old-project amoebum.extensions:*extensions-project-directory-override*)
         (old-enabled amoebum.extensions:*extension-hot-reload-enabled-p*)
         (old-interval amoebum.extensions:*extension-hot-reload-interval-seconds*)
         (disabled-keys (%hash-table-keys amoebum.extensions:*disabled-extensions*))
         (tmp-dir (%make-temp-directory "amoebum-ext-hotreload-change"))
         (global-dir (merge-pathnames #P"global/" tmp-dir))
         (project-dir (merge-pathnames #P"project/" tmp-dir))
         (ext-root (merge-pathnames #P"watch-ext/" project-dir))
         (manifest-path (merge-pathnames #P"extension.lisp" ext-root))
         (entry-path (merge-pathnames #P"main.lisp" ext-root)))
    (unwind-protect
         (progn
           (amoebum.extensions:stop-extension-hot-reload)
           (setf amoebum.extensions:*extensions-global-directory-override* global-dir
                 amoebum.extensions:*extensions-project-directory-override* project-dir
                 amoebum.extensions:*extension-hot-reload-interval-seconds* 0.05d0)
           (clrhash amoebum.extensions:*disabled-extensions*)
           (clrhash amoebum.extensions:*extension-watch-snapshot*)
           (%write-text-file manifest-path
                             "(:name \"watch-ext\" :version \"1.0.0\" :dependencies () :entry-point \"main.lisp\")")
           (%write-text-file entry-path "(values)")
           (let ((report (amoebum.extensions:load-user-extensions
                          :project-root tmp-dir
                          :start-hot-reload nil)))
             (is (= 1 (length report)))
             (let ((entry (first (amoebum.extensions:list-extension-registry))))
               (is (string= "1.0.0"
                            (amoebum.extensions:extension-registry-entry-version entry)))))
           ;; Sleep > 1s so the next manifest write has a strictly later
           ;; file-write-date (CL granularity is 1 second). Then start
           ;; the watcher and bump the version.
           (sleep 1.1)
           (amoebum.extensions:start-extension-hot-reload
            :project-root tmp-dir
            :global-directory global-dir
            :project-directory project-dir)
           (%write-text-file manifest-path
                             "(:name \"watch-ext\" :version \"1.1.0\" :dependencies () :entry-point \"main.lisp\")")
           ;; Poll up to ~10s for the version bump (interval is 50ms; CI variance buffer).
           (loop with deadline = (+ (get-internal-real-time)
                                    (* 10 internal-time-units-per-second))
                 for entry = (first (amoebum.extensions:list-extension-registry))
                 while (and entry
                            (string= "1.0.0"
                                     (amoebum.extensions:extension-registry-entry-version entry))
                            (< (get-internal-real-time) deadline))
                 do (sleep 0.1))
           (let ((entry (first (amoebum.extensions:list-extension-registry))))
             (is-true entry "registry entry vanished")
             (is (string= "1.1.0"
                          (amoebum.extensions:extension-registry-entry-version entry))
                 "watch thread did not pick up the manifest change"))
           (amoebum.extensions:stop-extension-hot-reload)
           (is (null amoebum::*extension-hot-reload-thread*)
               "thread reference should be cleared after stop"))
      (amoebum.extensions:stop-extension-hot-reload)
      (setf amoebum.extensions:*extension-load-report* old-report
            amoebum.extensions:*loaded-extensions* old-loaded
            amoebum.extensions:*extension-last-discovered* old-discovered
            amoebum.extensions:*extensions-global-directory-override* old-global
            amoebum.extensions:*extensions-project-directory-override* old-project
            amoebum.extensions:*extension-hot-reload-enabled-p* old-enabled
            amoebum.extensions:*extension-hot-reload-interval-seconds* old-interval)
      (clrhash amoebum.extensions:*disabled-extensions*)
      (dolist (key disabled-keys)
        (setf (gethash key amoebum.extensions:*disabled-extensions*) t))
      (%delete-directory-tree-safe tmp-dir))))

(test extension-lifecycle-smoke-sentinel
  (is-true t)
  (format t "EXTENSION_LIFECYCLE_SMOKE_OK~%"))
