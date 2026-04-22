#!/usr/bin/env bash
set -euo pipefail

# package-surface-audit.sh
#
# NXT-416: previously this wrapper invoked SBCL with
# `sbcl --noinform --non-interactive --script /dev/stdin <<EOF ... EOF`.
# On Debian SBCL 2.2.9, that pattern silently swallows the script body
# and exits 0, meaning the audit script never actually executed and the
# wrapper appeared to pass.
#
# The fix in this file:
#   1. Replace `--script /dev/stdin <<EOF` with the working
#      `--eval '(load "<tmpfile>")' --quit -- <args>` pattern.
#   2. The Lisp body emits a sentinel:
#        PACKAGE_SURFACE_AUDIT_OK groups=<n> root_exports=<n>
#      after every group has been audited and passed.
#   3. The bash wrapper greps stdout for that sentinel before exiting 0.
#      If the sentinel is missing the wrapper exits non-zero with
#        PACKAGE_SURFACE_AUDIT_FAIL reason=audit-did-not-execute
#      so silent-pass cannot recur.

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

RUNNER_SCRIPT="$(mktemp -t package-surface-audit-XXXXXX.lisp)"
OUT_LOG="$(mktemp -t package-surface-audit-out-XXXXXX.log)"
trap 'rm -f "${RUNNER_SCRIPT}" "${OUT_LOG}"' EXIT INT TERM

cat > "${RUNNER_SCRIPT}" <<'LISP'
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
         (script-arg (index)
           (let* ((argv (or #+sbcl sb-ext:*posix-argv* #-sbcl nil))
                  (sep (position "--" argv :test #'string=))
                  (tail (when sep (nthcdr (1+ sep) argv))))
             (and tail (nth index tail))))
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
  (let* ((repo-root-arg (or (script-arg 0) ""))
         (quicklisp-arg (or (script-arg 1) ""))
         (repo-root (and (plusp (length repo-root-arg))
                         (truename repo-root-arg))))
    (unless repo-root
      (format *error-output*
              "PACKAGE_SURFACE_AUDIT_FAIL reason=missing-repo-root arg=~S~%"
              repo-root-arg)
      (sb-ext:exit :code 2))
    (load-system-tree repo-root quicklisp-arg)
    ;; AMOEBUM.INTERNAL symbols are looked up dynamically via FIND-SYMBOL
    ;; rather than read at compile time. Without this, LOAD on the audit
    ;; source file fails with "Package AMOEBUM.INTERNAL does not exist"
    ;; because the reader processes all forms before any of them execute.
    ;; (NXT-416: this only became visible once the wrapper actually ran
    ;; SBCL — the prior --script /dev/stdin path silently no-op'd.)
    (let* ((internal-pkg (or (find-package "AMOEBUM.INTERNAL")
                             (error "Package AMOEBUM.INTERNAL not found after load-system :amoebum")))
           (root-max-sym (or (find-symbol "+AMOEBUM-ROOT-EXPORT-MAX+" internal-pkg)
                             (error "Symbol +AMOEBUM-ROOT-EXPORT-MAX+ not found in AMOEBUM.INTERNAL")))
           (groups-sym (or (find-symbol "+AMOEBUM-PACKAGE-SURFACE-GROUPS+" internal-pkg)
                           (error "Symbol +AMOEBUM-PACKAGE-SURFACE-GROUPS+ not found in AMOEBUM.INTERNAL")))
           (root-count (external-symbol-count :amoebum))
           (root-max (symbol-value root-max-sym))
           (groups (symbol-value groups-sym))
           (root-status (if (<= root-count root-max) "PASS" "FAIL"))
           (groups-ok (every #'identity (mapcar #'audit-group groups))))
      (format t "PACKAGE_SURFACE_AUDIT root_package=:amoebum exports=~D max=~D status=~A~%"
              root-count root-max root-status)
      (finish-output)
      (unless (and groups-ok (string= root-status "PASS"))
        (format t "PACKAGE_SURFACE_AUDIT_FAIL reason=group-or-root-failure groups_ok=~A root_status=~A~%"
                groups-ok root-status)
        (finish-output)
        (sb-ext:exit :code 1))
      (format t "PACKAGE_SURFACE_AUDIT_OK groups=~D root_exports=~D~%"
              (length groups) root-count)
      (finish-output))))
LISP

set +e
timeout 180 sbcl --noinform --non-interactive \
  --eval "(load \"${RUNNER_SCRIPT}\")" \
  --quit \
  -- "${REPO_ROOT}" "${QUICKLISP_SETUP}" 2>&1 | tee "${OUT_LOG}"
RC=${PIPESTATUS[0]}
set -e

if (( RC != 0 )); then
  echo "PACKAGE_SURFACE_AUDIT_FAIL reason=sbcl-exit-${RC}" >&2
  exit 1
fi

if ! grep -q '^PACKAGE_SURFACE_AUDIT_OK groups=' "${OUT_LOG}"; then
  echo "PACKAGE_SURFACE_AUDIT_FAIL reason=audit-did-not-execute" >&2
  echo "  (expected sentinel 'PACKAGE_SURFACE_AUDIT_OK groups=...' was not present in stdout)" >&2
  exit 1
fi
