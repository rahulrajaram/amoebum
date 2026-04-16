#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
QUICKLISP_SETUP="${QUICKLISP_SETUP:-${HOME}/quicklisp/setup.lisp}"

fail() {
  echo "PACKAGE_SURFACE_AUDIT_ERROR: $*" >&2
  exit 1
}

if [[ -f "${REPO_ROOT}/ptui/.tools/quicklisp/setup.lisp" ]]; then
  QUICKLISP_SETUP="${REPO_ROOT}/ptui/.tools/quicklisp/setup.lisp"
fi

[[ -f "${QUICKLISP_SETUP}" ]] || fail "quicklisp setup not found at ${QUICKLISP_SETUP}"
command -v sbcl >/dev/null 2>&1 || fail "sbcl not found on PATH"

timeout 180 sbcl --noinform --non-interactive --script /dev/stdin "${REPO_ROOT}" "${QUICKLISP_SETUP}" <<'EOF'
(labels ((sorted-unique-symbol-names (symbol-names)
           (sort (remove-duplicates (copy-list symbol-names) :test #'string=) #'string<))
         (external-symbol-p (package-name symbol-name)
           (multiple-value-bind (symbol status)
               (find-symbol symbol-name (find-package package-name))
             (and symbol (eq status :external))))
         (external-symbol-count (package-name)
           (let ((count 0))
             (do-external-symbols (_ (find-package package-name) count)
               (declare (ignore _))
               (incf count))))
         (load-system-tree (repo-root quicklisp-setup)
           (load quicklisp-setup)
           (require :asdf)
           (let* ((asdf-pkg (or (find-package "ASDF")
                                (error "Missing package ASDF")))
                  (load-asd-fn (symbol-function (or (find-symbol "LOAD-ASD" asdf-pkg)
                                                    (error "Missing ASDF LOAD-ASD symbol"))))
                  (load-system-fn (symbol-function (or (find-symbol "LOAD-SYSTEM" asdf-pkg)
                                                       (error "Missing ASDF LOAD-SYSTEM symbol")))))
             (dolist (asd-path '("pseudopod/pseudopod.asd"
                                 "sw4rm-sdk/sw4rm-sdk.asd"
                                 "ptui/ptui.asd"
                                 "amoebum/amoebum.asd"))
               (funcall load-asd-fn (merge-pathnames asd-path repo-root)))
             (funcall load-system-fn :amoebum)))
         (audit-group (spec)
           (let* ((group (getf spec :group))
                  (package-name (getf spec :package))
                  (symbol-names (sorted-unique-symbol-names (getf spec :symbols)))
                  (root-reexports (sorted-unique-symbol-names (getf spec :root-reexports '())))
                  (actual-count (external-symbol-count package-name))
                  (declared-count (length symbol-names))
                  (missing-package-symbols
                    (loop for symbol-name in symbol-names
                          unless (external-symbol-p package-name symbol-name)
                          collect symbol-name))
                  (missing-root-symbols
                    (loop for symbol-name in root-reexports
                          unless (external-symbol-p :amoebum symbol-name)
                          collect symbol-name))
                  (unexpected-root-symbols
                    (loop for symbol-name in symbol-names
                          unless (member symbol-name root-reexports :test #'string=)
                          when (external-symbol-p :amoebum symbol-name)
                          collect symbol-name))
                  (status (if (and (= actual-count declared-count)
                                   (null missing-package-symbols)
                                   (null missing-root-symbols)
                                   (null unexpected-root-symbols))
                              "PASS"
                              "FAIL")))
             (format t
                     "PACKAGE_SURFACE_AUDIT group=~(~A~) package=~A declared=~D actual=~D root_reexports=~D status=~A~%"
                     group package-name declared-count actual-count (length root-reexports) status)
             (when missing-package-symbols
               (format t "PACKAGE_SURFACE_AUDIT_DETAIL group=~(~A~) missing_package_symbols=~S~%"
                       group missing-package-symbols))
             (when missing-root-symbols
               (format t "PACKAGE_SURFACE_AUDIT_DETAIL group=~(~A~) missing_root_symbols=~S~%"
                       group missing-root-symbols))
             (when unexpected-root-symbols
               (format t "PACKAGE_SURFACE_AUDIT_DETAIL group=~(~A~) unexpected_root_symbols=~S~%"
                       group unexpected-root-symbols))
             (string= status "PASS"))))
  (let* ((argv (or #+sbcl sb-ext:*posix-argv* #-sbcl nil))
         (repo-root-arg (or (and argv (second argv)) ""))
         (quicklisp-arg (or (and argv (third argv)) ""))
         (repo-root (and (plusp (length repo-root-arg))
                         (truename repo-root-arg))))
    (unless repo-root
      (error "Unable to resolve repository root from ~S" repo-root-arg))
    (load-system-tree repo-root quicklisp-arg)
    (let* ((root-count (external-symbol-count :amoebum))
           (root-max amoebum.internal::+amoebum-root-export-max+)
           (root-status (if (<= root-count root-max) "PASS" "FAIL"))
           (groups-ok
             (every #'identity
                    (mapcar #'audit-group
                            amoebum.internal::+amoebum-package-surface-groups+))))
      (format t "PACKAGE_SURFACE_AUDIT root_package=:amoebum exports=~D max=~D status=~A~%"
              root-count root-max root-status)
      (finish-output)
      (unless (and groups-ok (string= root-status "PASS"))
        (sb-ext:exit :code 1))
      (format t "PACKAGE_SURFACE_AUDIT_OK groups=~D root_exports=~D~%"
              (length amoebum.internal::+amoebum-package-surface-groups+)
              root-count)
      (finish-output))))
EOF
