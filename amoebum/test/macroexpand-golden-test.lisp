(in-package :amoebum/test)

(def-suite macroexpand-golden-suite :in amoebum-suite
  :description "Golden macroexpand regressions for R013 defskill/deftool refactors.")

(in-suite macroexpand-golden-suite)

(defun %macroexpand-golden-root ()
  (merge-pathnames #P"test/snapshots/macroexpand/"
                   (uiop:ensure-directory-pathname
                    (asdf:system-source-directory "amoebum"))))

(defun %macroexpand-update-snapshots-p ()
  "Return true when AMOEBUM_UPDATE_SNAPSHOTS=1 is set in the environment.
When true, %assert-macroexpand-golden rewrites the golden file instead of
asserting equality, mirroring the PTUI_UPDATE_SNAPSHOTS escape hatch."
  (let ((env (uiop:getenv "AMOEBUM_UPDATE_SNAPSHOTS")))
    (and env (string= env "1"))))

(defun %render-macroexpand-1 (form)
  ;; Reset the gensym counter so reruns produce byte-identical output for
  ;; macros that introduce gensym'd lambda parameters (e.g. defkeys).
  (let ((*gensym-counter* 0))
    (with-output-to-string (stream)
      (let ((*package* (find-package :amoebum))
            (*print-case* :downcase)
            (*print-pretty* t)
            (*print-right-margin* 100)
            (*print-circle* nil)
            (*print-readably* t)
            (*compile-file-truename* nil)
            (*load-truename* #P"/dev/stdin"))
        (pprint (macroexpand-1 form) stream)))))

(defun %normalize-golden-text (text)
  (string-right-trim '(#\Newline #\Return) text))

(defun %macroexpand-golden-form (source)
  (let ((*package* (find-package :cl-user)))
    (read-from-string source)))

(defun %write-macroexpand-golden (path actual)
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string actual stream))
  path)

(defun %assert-macroexpand-golden (form relative-path &key reset-deftool-p)
  "Compare MACROEXPAND-1 of FORM against the golden snapshot at RELATIVE-PATH
under amoebum/test/snapshots/macroexpand/.

When AMOEBUM_UPDATE_SNAPSHOTS=1 the golden is overwritten in place. On
mismatch the assertion fails with a MACROEXPAND_GOLDEN_DRIFT marker that
includes the snapshot path and the AMOEBUM_UPDATE_SNAPSHOTS hint."
  (when reset-deftool-p
    (amoebum::reset-deftool-compile-validation-state))
  (let* ((path (merge-pathnames relative-path (%macroexpand-golden-root)))
         (actual (%render-macroexpand-1 form)))
    (cond
      ((%macroexpand-update-snapshots-p)
       (%write-macroexpand-golden path actual)
       (is-true t (format nil "MACROEXPAND_GOLDEN_UPDATED ~A" (namestring path))))
      (t
       (let ((expected (%normalize-golden-text (uiop:read-file-string path)))
             (actual* (%normalize-golden-text actual)))
         (is (string= expected actual*)
             "MACROEXPAND_GOLDEN_DRIFT path=~A~%  set AMOEBUM_UPDATE_SNAPSHOTS=1 to update if intentional."
             (namestring path)))))))

;;; ---------------------------------------------------------------------------
;;; Original R013 goldens (kept verbatim for backwards compatibility).
;;; ---------------------------------------------------------------------------

(test deftool-macroexpand-matches-r013-golden
  (handler-bind ((warning
                   (lambda (condition)
                     (declare (ignore condition))
                     (let ((restart (find-restart 'muffle-warning)))
                       (when restart
                         (invoke-restart restart))))))
    (%assert-macroexpand-golden
     (%macroexpand-golden-form
      "(amoebum:deftool r013-golden-tool
          ((path pathname :description \"Absolute path\" :required t)
           (limit (or null integer) :description \"Line limit\" :default nil)
           (dry-run boolean :description \"Dry run\" :default nil)
           (mode (member :fast :slow) :description \"Mode\" :default :fast))
        \"Golden macroexpand tool\"
        (:permission :auto)
        (:dangerous t)
        (:category :smoke)
        (:timeout 9)
        (list :path path :limit limit :dry-run dry-run :mode mode))")
     #P"deftool.sexp"
     :reset-deftool-p t)))

(test defskill-macroexpand-matches-r013-golden
  (%assert-macroexpand-golden
   (%macroexpand-golden-form
    "(amoebum:defskill r013-golden-skill
        ((count :integer :required t :prompt \"Count:\" :choices '(1 2 3))
         (mode :keyword :required nil :default :fast :choices '(:fast :slow)
          :description \"Mode\"))
      \"Golden macroexpand skill\"
      (:category :smoke)
      (:usage \"/r013-golden-skill <count> [<mode>]\")
      (format nil \"count=~D mode=~A\" count mode))")
   #P"defskill.sexp"))

;;; ---------------------------------------------------------------------------
;;; NXT-391: pre-decomposition guardrails for defskill / defkeys / deftool.
;;;
;;; Each macro gets three representative call-site shapes captured as a frozen
;;; macroexpand snapshot. NXT-392/393/395 will decompose the bodies of these
;;; macros — pre/post macroexpand output MUST stay byte-identical (whitespace
;;; only). On drift, the failure prints MACROEXPAND_GOLDEN_DRIFT plus the
;;; AMOEBUM_UPDATE_SNAPSHOTS=1 hint.
;;;
;;; Synthetic forms are used (instead of redefining the live registry entries)
;;; so the test suite does not stomp on real /commit, /status, git-status,
;;; chat-mode, etc. The shapes mirror real call sites in:
;;;   - amoebum/src/macros/defskill.lisp (lines 902, 928, 955, 968)
;;;   - amoebum/src/tools/git.lisp        (lines 801, 809, 819)
;;;   - amoebum/src/macros/defkeys.lisp   (lines 885, 891, 897)
;;; ---------------------------------------------------------------------------

(defmacro %with-muffled-macro-warnings (&body body)
  `(handler-bind ((warning
                    (lambda (condition)
                      (declare (ignore condition))
                      (let ((restart (find-restart 'muffle-warning)))
                        (when restart
                          (invoke-restart restart))))))
     ,@body))

;; ---- defskill: 3 representative shapes ----

(test defskill-macroexpand-zero-args-golden
  (%with-muffled-macro-warnings
    (%assert-macroexpand-golden
     (%macroexpand-golden-form
      "(amoebum:defskill r013-golden-skill-zero-args ()
          \"Print a zero-argument skill response.\"
          (:category :session)
          (:usage \"/r013-golden-skill-zero-args\")
          (make-slash-command-result :echo-input-p t :output \"ok\"))")
     #P"defskill-zero-args.sexp")))

(test defskill-macroexpand-greedy-string-golden
  (%with-muffled-macro-warnings
    (%assert-macroexpand-golden
     (%macroexpand-golden-form
      "(amoebum:defskill r013-golden-skill-greedy-string
          ((files :string :required nil :greedy t :description \"Optional explicit file paths.\"))
          \"Greedy string skill.\"
          (:category :git)
          (:usage \"/r013-golden-skill-greedy-string [files...]\")
          (make-slash-command-result :echo-input-p t :output (format nil \"~S\" files)))")
     #P"defskill-greedy-string.sexp")))

(test defskill-macroexpand-multi-typed-golden
  (%with-muffled-macro-warnings
    (%assert-macroexpand-golden
     (%macroexpand-golden-form
      "(amoebum:defskill r013-golden-skill-multi-typed
          ((keep-last :integer :required nil :default 6 :description \"How many turns to keep.\")
           (mode :keyword :required nil :default :fast :choices '(:fast :slow) :description \"Mode.\")
           (label :string :required t :prompt \"Label:\"))
          \"Multi-typed skill.\"
          (:aliases (\"r013-mt-skill\"))
          (:category :session)
          (:keybinding \"C-c m\")
          (:usage \"/r013-golden-skill-multi-typed <label> [keep-last] [mode]\")
          (make-slash-command-result
            :echo-input-p t
            :output (format nil \"~A keep=~D mode=~A\" label keep-last mode)))")
     #P"defskill-multi-typed.sexp")))

;; ---- deftool: 3 representative shapes ----

(test deftool-macroexpand-zero-args-golden
  (%with-muffled-macro-warnings
    (%assert-macroexpand-golden
     (%macroexpand-golden-form
      "(amoebum:deftool r013-golden-tool-zero-args ()
          \"Zero-argument tool.\"
          (:permission :auto)
          (:dangerous nil)
          (:category :smoke)
          (:timeout 30)
          (list :ok t))")
     #P"deftool-zero-args.sexp"
     :reset-deftool-p t)))

(test deftool-macroexpand-optional-string-golden
  (%with-muffled-macro-warnings
    (%assert-macroexpand-golden
     (%macroexpand-golden-form
      "(amoebum:deftool r013-golden-tool-optional-string
          ((base-branch (or null string)
                        :description \"Optional base branch override.\"
                        :default nil))
          \"Single optional string tool.\"
          (:permission :auto)
          (:dangerous nil)
          (:category :smoke)
          (:timeout 60)
          (list :base-branch base-branch))")
     #P"deftool-optional-string.sexp"
     :reset-deftool-p t)))

(test deftool-macroexpand-multi-typed-golden
  (%with-muffled-macro-warnings
    (%assert-macroexpand-golden
     (%macroexpand-golden-form
      "(amoebum:deftool r013-golden-tool-multi-typed
          ((files (or null list string)
                  :description \"Optional explicit files.\"
                  :default nil)
           (model (or null string)
                  :description \"Model override.\"
                  :default nil)
           (amend boolean
                  :description \"Request amend mode.\"
                  :default nil)
           (allow-amend boolean
                        :description \"Confirm amend.\"
                        :default nil))
          \"Dangerous multi-typed tool.\"
          (:permission :full-auto)
          (:dangerous t)
          (:category :smoke)
          (:timeout 180)
          (list :files files :model model :amend amend :allow-amend allow-amend))")
     #P"deftool-multi-typed.sexp"
     :reset-deftool-p t)))

;; ---- defkeys: 3 representative shapes ----

(test defkeys-macroexpand-trivial-golden
  (%with-muffled-macro-warnings
    (%assert-macroexpand-golden
     (%macroexpand-golden-form
      "(amoebum:defkeys r013-golden-keys-trivial
          \"Trivial keymap.\"
          (\"RET\" state :description \"Submit.\")
          (\"C-c\" state :description \"Quit.\")
          (\"TAB\" state :description \"Complete.\"))")
     #P"defkeys-trivial.sexp")))

(test defkeys-macroexpand-with-guard-golden
  (%with-muffled-macro-warnings
    (%assert-macroexpand-golden
     (%macroexpand-golden-form
      "(amoebum:defkeys r013-golden-keys-with-guard
          \"Guarded keymap.\"
          (\"RET\" state :description \"Submit if ready.\" :when t)
          (\"ESC\" state :description \"Cancel.\" :when nil))")
     #P"defkeys-with-guard.sexp")))

(test defkeys-macroexpand-chord-golden
  (%with-muffled-macro-warnings
    (%assert-macroexpand-golden
     (%macroexpand-golden-form
      "(amoebum:defkeys r013-golden-keys-chord
          \"Chord keymap.\"
          (\"C-x\" state :description \"Prefix placeholder.\")
          (\"M-x\" state :description \"Meta prefix placeholder.\")
          (\"C-S-up\" state :description \"Control-shift-up placeholder.\"))")
     #P"defkeys-chord.sexp")))
