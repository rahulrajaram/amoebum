(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; OS Sandbox Smoke Tests (I97)
;;; ---------------------------------------------------------------------------

(def-suite os-sandbox-suite :in amoebum-suite
  :description "OS-level sandboxing smoke tests.")

(in-suite os-sandbox-suite)

(test os-sandbox-platform-detection
  (let ((platform (amoebum::os-sandbox-platform)))
    (is (keywordp platform))
    (is (member platform '(:linux :darwin :unknown)))))

(test os-sandbox-available-check
  (let ((available (amoebum::os-sandbox-available-p)))
    (is (or (eq available t) (eq available nil)))))

(test landlock-ruleset-creation
  (let ((rs (amoebum::make-landlock-ruleset
             :allowed-read-paths '("/tmp/" "/usr/")
             :allowed-write-paths '("/tmp/")
             :allowed-exec-paths '("/usr/bin/"))))
    (is (amoebum::landlock-ruleset-p rs))
    (is (= 2 (length (amoebum::landlock-ruleset-allowed-read-paths rs))))
    (is (= 1 (length (amoebum::landlock-ruleset-allowed-write-paths rs))))))

(test landlock-ruleset-to-sexp
  (let* ((rs (amoebum::make-landlock-ruleset
              :allowed-read-paths '("/tmp/")
              :allowed-write-paths '("/out/")))
         (sexp (amoebum::landlock-ruleset-to-sexp rs)))
    (is (listp sexp))
    (is (equal '("/tmp/") (getf sexp :read)))
    (is (equal '("/out/") (getf sexp :write)))))

(test seccomp-profile-creation
  (let ((prof (amoebum::make-seccomp-profile
               :blocked-syscalls '(:ptrace :mount)
               :action :kill)))
    (is (amoebum::seccomp-profile-p prof))
    (is (eq :kill (amoebum::seccomp-profile-action prof)))))

(test seatbelt-profile-generation
  (let ((profile (amoebum::generate-seatbelt-profile
                  :allow-network t
                  :allow-read-paths '("/tmp/test/")
                  :allow-write-paths '("/tmp/out/"))))
    (is (stringp profile))
    (is (search "(version 1)" profile))
    (is (search "(deny default)" profile))
    (is (search "(allow network*)" profile))
    (is (search "/tmp/test/" profile))
    (is (search "/tmp/out/" profile))))

(test seatbelt-profile-no-network
  (let ((profile (amoebum::generate-seatbelt-profile :allow-network nil)))
    (is (null (search "(allow network*)" profile)))))

(test sandbox-os-config-creation
  (let ((cfg (amoebum::make-sandbox-os-config
              :enabled-p t
              :allowed-read-paths '("/tmp/")
              :timeout-seconds 60)))
    (is (amoebum::sandbox-os-config-p cfg))
    (is (eq t (amoebum::sandbox-os-config-enabled-p cfg)))
    (is (= 60 (amoebum::sandbox-os-config-timeout-seconds cfg)))))

(test sandbox-os-config-defaults
  (let ((cfg (amoebum::make-sandbox-os-config)))
    (is (eq nil (amoebum::sandbox-os-config-enabled-p cfg)))
    (is (eq t (amoebum::sandbox-os-config-fallback-to-cl-p cfg)))
    (is (eq nil (amoebum::sandbox-os-config-allow-network cfg)))
    (is (= 30 (amoebum::sandbox-os-config-timeout-seconds cfg)))))

(test sandbox-os-config-for-supervised-mode
  (let ((cfg (amoebum::sandbox-os-config-for-mode :supervised :project-root "/tmp/proj/")))
    (is (eq t (amoebum::sandbox-os-config-enabled-p cfg)))
    (is (eq nil (amoebum::sandbox-os-config-allow-network cfg)))
    (is (member "/tmp/" (amoebum::sandbox-os-config-allowed-read-paths cfg) :test #'string=))))

(test sandbox-os-config-for-yolo-mode
  (let ((cfg (amoebum::sandbox-os-config-for-mode :yolo)))
    (is (eq nil (amoebum::sandbox-os-config-enabled-p cfg)))
    (is (eq nil (amoebum::sandbox-os-config-fallback-to-cl-p cfg)))))

(test sandbox-os-config-for-auto-edit
  (let ((cfg (amoebum::sandbox-os-config-for-mode :auto-edit :project-root "/tmp/proj/")))
    (is (eq t (amoebum::sandbox-os-config-enabled-p cfg)))
    (is (eq t (amoebum::sandbox-os-config-allow-network cfg)))))

(test sandboxed-run-simple-command
  "Smoke test: run echo via sandboxed-run-program."
  (let ((cfg (amoebum::make-sandbox-os-config :enabled-p nil)))
    (let ((output (amoebum::sandboxed-run-program "echo" '("hello")
                                                   :config cfg
                                                   :output :string)))
      (is (search "hello" output)))))

(test landlock-available-p-no-error
  "landlock-available-p should not error on any platform."
  (is (or (eq (amoebum::landlock-available-p) t)
          (eq (amoebum::landlock-available-p) nil))))
