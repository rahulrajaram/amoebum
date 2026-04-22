#!/usr/bin/env bash
# bin/check-import-cycles.sh
#
# NXT-397: Package-level import-cycle guardrail.
#
# Builds a directed graph of amoebum package dependencies (edges P -> Q
# whenever P :uses Q or :import-from Q in a defpackage form), runs
# Tarjan's strongly-connected-components algorithm, and fails if any
# non-trivial SCC (size >= 2) or self-loop is found.
#
# This catches cycles introduced by the post-delegation facade splits
# (NXT-382 .. NXT-396) before they reach `make build` or focused
# verifiers. The current tree's ratcheted baseline is zero cycles.
#
# Output markers (line-prefix style, matching bin/line-count-audit.sh
# and bin/package-surface-audit.sh):
#
#   IMPORT_CYCLE_AUDIT_OK   cycles=0 packages=<n> edges=<m>
#   IMPORT_CYCLE_AUDIT_FAIL cycles=<n> packages=<n> edges=<m>
#   IMPORT_CYCLE_DETAIL     cycle=<a -> b -> ... -> a>
#
# Exit codes: 0 on success, 1 on cycle detected, 2 on usage/setup error.
#
# Implementation note: bash + awk parsing of Common Lisp s-expressions
# is brittle (string escapes, comments, nested parens). The script
# delegates parsing to a self-contained SBCL `--script` snippet that
# uses `cl:read` on the package-definition files directly. The bash
# wrapper only sets up paths, captures the marker line, and returns
# the right exit code.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

fail() {
  echo "IMPORT_CYCLE_AUDIT_ERROR: $*" >&2
  exit 2
}

usage() {
  cat <<'EOF'
Usage:
  bin/check-import-cycles.sh [--list-files]

Detects directed cycles in the amoebum package import graph by parsing
defpackage forms in amoebum/src/package*.lisp (and other known package
definition files). Emits IMPORT_CYCLE_AUDIT_OK on success or
IMPORT_CYCLE_AUDIT_FAIL with per-cycle detail on failure.

Options:
  --list-files   Print the package files that would be scanned and exit.
EOF
}

# Files to parse for defpackage forms. Order does not matter; the SBCL
# script reads each file's top-level forms with `cl:read`. Add new
# package-definition files here as they are introduced (e.g. when a
# subsystem gets its own package.lisp during a future split).
PACKAGE_FILES=(
  "amoebum/src/package.lisp"
  "amoebum/src/package-domains.lisp"
  "amoebum/src/fp/package.lisp"
  "amoebum/src/test-support/globals-fixture.lisp"
)

case "${1:-}" in
  --help|-h)
    usage
    exit 0
    ;;
  --list-files)
    if [[ $# -ne 1 ]]; then
      usage >&2
      exit 2
    fi
    for relpath in "${PACKAGE_FILES[@]}"; do
      printf '%s\n' "${REPO_ROOT}/${relpath}"
    done
    exit 0
    ;;
  "")
    ;;
  *)
    usage >&2
    exit 2
    ;;
esac

if [[ $# -gt 0 ]]; then
  usage >&2
  exit 2
fi

command -v sbcl >/dev/null 2>&1 || fail "sbcl not found on PATH"

# Verify all package files exist before invoking SBCL — keeps error
# reporting in bash where we can format it consistently.
for relpath in "${PACKAGE_FILES[@]}"; do
  if [[ ! -f "${REPO_ROOT}/${relpath}" ]]; then
    fail "package file not found: ${relpath}"
  fi
done

# Stream the file list as separate argv entries to the SBCL script.
#
# NOTE: `--script` already implies `--non-interactive`. Adding both
# silences stdout under some SBCL builds (Debian's 2.2.9 in particular)
# when the script is delivered via heredoc on /dev/stdin. Use only
# `--script`.
sbcl_args=(sbcl --noinform --script /dev/stdin "${REPO_ROOT}")
for relpath in "${PACKAGE_FILES[@]}"; do
  sbcl_args+=("${relpath}")
done

"${sbcl_args[@]}" <<'LISP'
;;;; check-import-cycles.lisp (embedded SBCL --script payload)
;;;;
;;;; Argv:
;;;;   argv[0]    = sbcl
;;;;   argv[1]    = repo root (absolute pathname string)
;;;;   argv[2..]  = relative paths to package-definition .lisp files
;;;;
;;;; Output: marker lines on stdout. Exits 0 on no-cycle, 1 on cycle.

(in-package #:cl-user)

(defun %argv ()
  #+sbcl sb-ext:*posix-argv*
  #-sbcl (error "Only SBCL is supported for the import-cycle audit."))

(defun %normalize-package-name (designator)
  "Coerce a defpackage designator (string, symbol, or keyword) to a
canonical lowercase string. We do not require the package to actually
be defined — we are working off the source forms, not a live image."
  (cond
    ((stringp designator) (string-downcase designator))
    ((symbolp designator) (string-downcase (symbol-name designator)))
    (t (string-downcase (princ-to-string designator)))))

(defun %defpackage-head-p (stream)
  "Peek at STREAM's next non-whitespace position. Return T iff the next
form looks like a `(defpackage ...)` list. Restores stream position so
the caller can `read` normally on T or skip on NIL."
  (let ((mark (file-position stream)))
    (unwind-protect
         (handler-case
             (progn
               ;; Skip whitespace and Lisp `;` line comments to find
               ;; the next significant character. This is a deliberate
               ;; subset of full Lisp tokenization — we only need to
               ;; identify `(defpackage ...)` heads at top level, so
               ;; we don't bother with `#|...|#` block comments
               ;; (which the package files don't use) or readtable
               ;; modifiers like `#+`.
               (loop for c = (peek-char nil stream nil :eof)
                     do (cond
                          ((eq c :eof) (return-from %defpackage-head-p nil))
                          ((member c '(#\Space #\Tab #\Newline #\Return))
                           (read-char stream))
                          ((char= c #\;)
                           (read-line stream nil :eof))
                          (t (return))))
               (let ((open (peek-char nil stream nil :eof)))
                 (unless (and (characterp open) (char= open #\())
                   (return-from %defpackage-head-p nil))
                 ;; Read the `(`
                 (read-char stream)
                 ;; Read the head symbol with a permissive reader.
                 (let* ((*package* (find-package "KEYWORD"))
                        (*read-eval* nil)
                        (head (handler-case (read stream nil :eof)
                                (error () :eof))))
                   (and (symbolp head)
                        (string-equal (symbol-name head) "DEFPACKAGE")))))
           (error () nil))
      (file-position stream mark))))

(defun %read-defpackage-forms (path)
  "Return the list of defpackage forms in PATH.

Strategy: peek at every top-level form. If it starts with `defpackage`,
read it normally. Otherwise, skip it with `*read-suppress*` so we
never resolve symbol prefixes that may name not-yet-loaded packages
(which would raise PACKAGE-ERROR and force us to abort the file).

This makes the audit robust to files like `globals-fixture.lisp` that
mix a defpackage form with body code referencing other amoebum
packages by qualified-name."
  (handler-case
      (let ((forms '()))
        (with-open-file (stream path :direction :input
                                     :external-format :utf-8)
          (loop
            (let ((next (peek-char t stream nil :eof)))
              (when (eq next :eof) (return)))
            (cond
              ((%defpackage-head-p stream)
               (let ((*package* (find-package "KEYWORD"))
                     (*read-eval* nil))
                 (let ((form (handler-case (read stream nil :eof)
                               (error (c)
                                 (format *error-output*
                                         "IMPORT_CYCLE_AUDIT_WARN file=~A read-error=~A~%"
                                         path c)
                                 :eof))))
                   (cond
                     ((eq form :eof) (return))
                     (t (push form forms))))))
              (t
               ;; Skip the form silently. *read-suppress* makes `read`
               ;; consume the next form without interning symbols or
               ;; raising on unknown packages.
               (let ((*read-suppress* t)
                     (*package* (find-package "KEYWORD"))
                     (*read-eval* nil))
                 (handler-case (read stream nil :eof)
                   (error (c)
                     (format *error-output*
                             "IMPORT_CYCLE_AUDIT_WARN file=~A skip-error=~A~%"
                             path c)
                     (return))))))))
        (nreverse forms))
    (file-error (c)
      (format *error-output* "IMPORT_CYCLE_AUDIT_ERROR file=~A error=~A~%"
              path c)
      (sb-ext:exit :code 2))))

(defun %defpackage-form-p (form)
  (and (consp form)
       (symbolp (first form))
       (string-equal (symbol-name (first form)) "DEFPACKAGE")))

(defun %clause-keyword-p (clause-head keyword-name)
  (and (symbolp clause-head)
       (string-equal (symbol-name clause-head) keyword-name)))

(defun %extract-edges (defpackage-form)
  "Given a (defpackage NAME &rest CLAUSES) form, return
(NAME-STRING . LIST-OF-DEPENDENCY-NAME-STRINGS).

Edges are derived from :use, :import-from, and :shadowing-import-from
clauses. The CL standard package is included as a node but its
dependencies (none) make it a sink — that is fine.

We deliberately do NOT include :inherit-from-superclasses or other
exotic clauses; they are not used in this codebase and would require
defpackage-plus support."
  (let* ((name (%normalize-package-name (second defpackage-form)))
         (deps '()))
    (dolist (clause (cddr defpackage-form))
      (when (consp clause)
        (let ((head (first clause)))
          (cond
            ;; (:use pkg ...) — every following entry is a dependency.
            ((%clause-keyword-p head "USE")
             (dolist (used (rest clause))
               (push (%normalize-package-name used) deps)))
            ;; (:import-from pkg sym ...) — first entry is the dep.
            ((%clause-keyword-p head "IMPORT-FROM")
             (when (rest clause)
               (push (%normalize-package-name (second clause)) deps)))
            ;; (:shadowing-import-from pkg sym ...) — first entry is the dep.
            ((%clause-keyword-p head "SHADOWING-IMPORT-FROM")
             (when (rest clause)
               (push (%normalize-package-name (second clause)) deps)))))))
    (cons name (remove-duplicates (nreverse deps) :test #'string=))))

(defun %collect-graph (file-paths)
  "Parse every package file and return two values:
  1. A hash-table NAME -> list of dependency NAMEs.
  2. The total edge count.

If the same package name appears in multiple files (it should not),
their dependency lists are merged."
  (let ((graph (make-hash-table :test #'equal))
        (edge-count 0))
    (dolist (path file-paths)
      (dolist (form (%read-defpackage-forms path))
        (when (%defpackage-form-p form)
          (let* ((entry (%extract-edges form))
                 (name (car entry))
                 (deps (cdr entry))
                 (existing (gethash name graph)))
            (setf (gethash name graph)
                  (remove-duplicates (append existing deps) :test #'string=))))))
    ;; Ensure every dep appears as a node so SCC has a complete vertex
    ;; set, then count edges.
    (let ((all-nodes '()))
      (maphash (lambda (k v) (push k all-nodes) (dolist (d v) (push d all-nodes)))
               graph)
      (dolist (n (remove-duplicates all-nodes :test #'string=))
        (unless (nth-value 1 (gethash n graph))
          (setf (gethash n graph) '()))))
    (maphash (lambda (k v) (declare (ignore k)) (incf edge-count (length v)))
             graph)
    (values graph edge-count)))

;;;; Tarjan's SCC algorithm.
;;;;
;;;; A cycle is any SCC of size >= 2, plus any node with a self-loop
;;;; (an SCC of size 1 where the node depends on itself).

(defun %tarjan-scc (graph)
  "Return a list of strongly-connected-components, each an opaque list
of node-name strings. Order within an SCC is the discovery order."
  (let ((index 0)
        (indices (make-hash-table :test #'equal))
        (lowlinks (make-hash-table :test #'equal))
        (on-stack (make-hash-table :test #'equal))
        (stack '())
        (sccs '()))
    (labels
        ((strong-connect (v)
           (setf (gethash v indices) index
                 (gethash v lowlinks) index)
           (incf index)
           (push v stack)
           (setf (gethash v on-stack) t)
           (dolist (w (gethash v graph))
             (cond
               ((not (nth-value 1 (gethash w indices)))
                (strong-connect w)
                (setf (gethash v lowlinks)
                      (min (gethash v lowlinks) (gethash w lowlinks))))
               ((gethash w on-stack)
                (setf (gethash v lowlinks)
                      (min (gethash v lowlinks) (gethash w indices))))))
           (when (= (gethash v lowlinks) (gethash v indices))
             (let ((scc '()))
               (loop
                 (let ((w (pop stack)))
                   (setf (gethash w on-stack) nil)
                   (push w scc)
                   (when (string= w v) (return))))
               (push scc sccs)))))
      (maphash (lambda (v _)
                 (declare (ignore _))
                 (unless (nth-value 1 (gethash v indices))
                   (strong-connect v)))
               graph))
    (nreverse sccs)))

(defun %cycles-from-sccs (graph sccs)
  "Filter SCCs to actual cycles. An SCC is a cycle iff size >= 2 OR
the single node has a self-loop edge."
  (loop for scc in sccs
        when (or (>= (length scc) 2)
                 (and (= (length scc) 1)
                      (member (first scc) (gethash (first scc) graph)
                              :test #'string=)))
        collect scc))

(defun %format-cycle (cycle)
  "Render a cycle as `a -> b -> c -> a` for human readability."
  (with-output-to-string (out)
    (let ((nodes (append cycle (list (first cycle)))))
      (loop for (n . rest) on nodes
            do (write-string n out)
               (when rest (write-string " -> " out))))))

(defun main ()
  (let* ((argv (%argv))
         (repo-root-arg (or (and argv (second argv))
                            (progn
                              (format *error-output*
                                      "IMPORT_CYCLE_AUDIT_ERROR missing repo root argument~%")
                              (sb-ext:exit :code 2))))
         (rel-files (cddr argv))
         (repo-root (truename repo-root-arg))
         (paths (loop for relpath in rel-files
                      collect (merge-pathnames relpath repo-root))))
    (when (null paths)
      (format *error-output*
              "IMPORT_CYCLE_AUDIT_ERROR no package files supplied~%")
      (sb-ext:exit :code 2))
    (multiple-value-bind (graph edge-count) (%collect-graph paths)
      (let* ((sccs (%tarjan-scc graph))
             (cycles (%cycles-from-sccs graph sccs))
             (package-count (hash-table-count graph)))
        (cond
          ((null cycles)
           (format t "IMPORT_CYCLE_AUDIT_OK cycles=0 packages=~D edges=~D~%"
                   package-count edge-count)
           (finish-output)
           (sb-ext:exit :code 0))
          (t
           (dolist (cycle cycles)
             (format t "IMPORT_CYCLE_DETAIL cycle=~A~%" (%format-cycle cycle)))
           (format t "IMPORT_CYCLE_AUDIT_FAIL cycles=~D packages=~D edges=~D~%"
                   (length cycles) package-count edge-count)
           (finish-output)
           (sb-ext:exit :code 1)))))))

(main)
LISP
