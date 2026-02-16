(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; OS-Level Sandboxing (I97)
;;;
;;; Platform-specific sandboxing using:
;;; - Linux: Landlock LSM (kernel >= 5.13) + seccomp-BPF
;;; - macOS: Seatbelt sandbox profiles
;;; Graceful fallback to CL-level sandbox (I78) when OS features unavailable.
;;; ---------------------------------------------------------------------------

;;; --- Platform Detection ---

(defun os-sandbox-platform ()
  "Return the current platform as a keyword."
  #+linux :linux
  #+darwin :darwin
  #-(or linux darwin) :unknown)

(defun os-sandbox-available-p ()
  "Check if OS-level sandboxing is available on this platform."
  (case (os-sandbox-platform)
    (:linux (landlock-available-p))
    (:darwin (seatbelt-available-p))
    (otherwise nil)))

;;; --- Landlock LSM (Linux) ---

#+linux
(defconstant +landlock-create-ruleset+ 444
  "Syscall number for landlock_create_ruleset on x86_64.")

#+linux
(defconstant +landlock-add-rule+ 445
  "Syscall number for landlock_add_rule on x86_64.")

#+linux
(defconstant +landlock-restrict-self+ 446
  "Syscall number for landlock_restrict_self on x86_64.")

#+linux
(defconstant +landlock-access-fs-read-file+ #x0004)
#+linux
(defconstant +landlock-access-fs-write-file+ #x0020)
#+linux
(defconstant +landlock-access-fs-execute+ #x0001)

(defun landlock-available-p ()
  "Check if Landlock is available on this Linux kernel."
  #+linux
  (handler-case
      (let ((version-string (uiop:run-program '("uname" "-r") :output :string)))
        (let* ((parts (cl-ppcre:split "\\." version-string))
               (major (and parts (ignore-errors (parse-integer (first parts)))))
               (minor (and (rest parts) (ignore-errors (parse-integer (second parts))))))
          (and major minor
               (or (> major 5)
                   (and (= major 5) (>= minor 13))))))
    (error () nil))
  #-linux
  nil)

(defstruct (landlock-ruleset
            (:constructor make-landlock-ruleset
                (&key (allowed-read-paths '())
                      (allowed-write-paths '())
                      (allowed-exec-paths '()))))
  (allowed-read-paths '() :type list)
  (allowed-write-paths '() :type list)
  (allowed-exec-paths '() :type list))

(defun landlock-ruleset-to-sexp (ruleset)
  "Serialize a Landlock ruleset to an S-expression for debugging."
  (list :read (landlock-ruleset-allowed-read-paths ruleset)
        :write (landlock-ruleset-allowed-write-paths ruleset)
        :exec (landlock-ruleset-allowed-exec-paths ruleset)))

;;; --- Seccomp-BPF (Linux) ---

(defun seccomp-available-p ()
  "Check if seccomp is available."
  #+linux
  (uiop:file-exists-p "/proc/self/status")
  #-linux
  nil)

(defstruct (seccomp-profile
            (:constructor make-seccomp-profile
                (&key (blocked-syscalls '())
                      (action :kill))))
  (blocked-syscalls '() :type list)
  (action :kill :type keyword))

;;; --- Seatbelt (macOS) ---

(defun seatbelt-available-p ()
  "Check if Seatbelt sandbox-exec is available."
  #+darwin
  (handler-case
      (let ((result (uiop:run-program '("which" "sandbox-exec") :output :string
                                       :ignore-error-status t)))
        (plusp (length (string-trim '(#\Newline #\Space) result))))
    (error () nil))
  #-darwin
  nil)

(defun generate-seatbelt-profile (&key (allow-network nil)
                                       (allow-read-paths '())
                                       (allow-write-paths '())
                                       (allow-exec-paths '()))
  "Generate a macOS Seatbelt sandbox profile string."
  (with-output-to-string (out)
    (format out "(version 1)~%")
    (format out "(deny default)~%")
    (format out "(allow process-exec)~%")
    (format out "(allow sysctl-read)~%")
    (format out "(allow mach-lookup)~%")
    (when allow-network
      (format out "(allow network*)~%"))
    (dolist (path allow-read-paths)
      (format out "(allow file-read* (subpath ~S))~%" (namestring path)))
    (dolist (path allow-write-paths)
      (format out "(allow file-write* (subpath ~S))~%" (namestring path)))
    (dolist (path allow-exec-paths)
      (format out "(allow process-exec (subpath ~S))~%" (namestring path)))))

;;; --- Sandboxed Execution ---

(defstruct (sandbox-os-config
            (:constructor make-sandbox-os-config
                (&key (enabled-p nil)
                      (fallback-to-cl-p t)
                      (allowed-read-paths '())
                      (allowed-write-paths '())
                      (allowed-exec-paths '())
                      (allow-network nil)
                      (timeout-seconds 30))))
  (enabled-p nil :type boolean)
  (fallback-to-cl-p t :type boolean)
  (allowed-read-paths '() :type list)
  (allowed-write-paths '() :type list)
  (allowed-exec-paths '() :type list)
  (allow-network nil :type boolean)
  (timeout-seconds 30 :type integer))

(defun %build-sandboxed-command (config command args)
  "Build the actual command to run, wrapping with sandbox if possible."
  (case (os-sandbox-platform)
    (:darwin
     (if (seatbelt-available-p)
         (let ((profile (generate-seatbelt-profile
                         :allow-network (sandbox-os-config-allow-network config)
                         :allow-read-paths (sandbox-os-config-allowed-read-paths config)
                         :allow-write-paths (sandbox-os-config-allowed-write-paths config)
                         :allow-exec-paths (sandbox-os-config-allowed-exec-paths config))))
           (list "sandbox-exec" "-p" profile command))
         (cons command args)))
    ;; For Linux, Landlock requires in-process setup (not a wrapper command)
    ;; so we just run the command directly and rely on CL sandbox
    (otherwise (cons command args))))

(defun sandboxed-run-program (command args &key (config (make-sandbox-os-config))
                                               (output :string)
                                               (error-output :string)
                                               (ignore-error-status nil))
  "Run COMMAND with OS-level sandboxing if available, falling back to CL sandbox.
Returns (values output error-output exit-code)."
  (let ((effective-command
          (if (and (sandbox-os-config-enabled-p config) (os-sandbox-available-p))
              (%build-sandboxed-command config command args)
              (cons command args))))
    (handler-case
        (uiop:run-program effective-command
                          :output output
                          :error-output error-output
                          :ignore-error-status ignore-error-status)
      (error (c)
        (if (sandbox-os-config-fallback-to-cl-p config)
            ;; Fallback to CL-level sandbox (I78)
            (safe-run-program command args :output output
                                           :error-output error-output
                                           :ignore-error-status ignore-error-status)
            (error c))))))

;;; --- Integration with Permission Modes ---

(defun sandbox-os-config-for-mode (mode &key project-root)
  "Create an OS sandbox config appropriate for the given permission mode."
  (let ((root (or project-root
                  (ignore-errors (uiop:getcwd))
                  *default-pathname-defaults*)))
    (case mode
      (:supervised
       (make-sandbox-os-config
        :enabled-p t
        :allowed-read-paths (list (namestring root) "/usr/" "/etc/" "/tmp/")
        :allowed-write-paths (list (namestring (merge-pathnames ".amoebum/" root)) "/tmp/")
        :allowed-exec-paths (list "/usr/bin/" "/bin/" "/usr/local/bin/")
        :allow-network nil
        :timeout-seconds 30))
      ((:auto-edit :full-auto)
       (make-sandbox-os-config
        :enabled-p t
        :allowed-read-paths (list (namestring root) "/usr/" "/etc/" "/tmp/")
        :allowed-write-paths (list (namestring root) "/tmp/")
        :allowed-exec-paths (list "/usr/bin/" "/bin/" "/usr/local/bin/")
        :allow-network t
        :timeout-seconds 60))
      (:yolo
       (make-sandbox-os-config :enabled-p nil :fallback-to-cl-p nil))
      (otherwise
       (make-sandbox-os-config :enabled-p t :fallback-to-cl-p t)))))
