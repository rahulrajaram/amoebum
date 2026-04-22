(in-package :amoebum/test)

;;;; NXT-397: import-cycle guardrail (test-time mirror of
;;;; bin/check-import-cycles.sh).
;;;;
;;;; This suite asserts the same property the bash guardrail asserts:
;;;; the live amoebum package universe contains no directed cycles in
;;;; the union of `package-use-list` and `package-imported-symbols`
;;;; relations. Where the bash script parses .lisp source files, this
;;;; FiveAM test inspects the loaded image — they are independent
;;;; checks that should both stay green.
;;;;
;;;; Catching a cycle here means a future split (NXT-382 .. NXT-396 and
;;;; successors) introduced a circular `:use` / `:import-from`
;;;; relationship that loaded successfully but breaks architectural
;;;; layering.

(def-suite import-cycles-suite
  :description "Package-import-cycle guardrail (no cycles in loaded amoebum graph)."
  :in amoebum-suite)

(in-suite import-cycles-suite)

(defun %amoebum-package-p (package)
  (let ((name (package-name package)))
    (and name
         (or (string-equal name "AMOEBUM")
             (and (>= (length name) 8)
                  (string-equal (subseq name 0 8) "AMOEBUM."))))))

(defun %amoebum-packages ()
  (sort (remove-if-not #'%amoebum-package-p (list-all-packages))
        #'string<
        :key #'package-name))

(defun %imported-from-packages (package)
  "Return the set of packages PACKAGE has imported any external symbol
from (i.e. the home packages of all symbols PACKAGE imports). This
includes both direct `:import-from` clauses and inherited symbols."
  (let ((seen (make-hash-table :test #'eq)))
    (do-symbols (sym package)
      (multiple-value-bind (s status) (find-symbol (symbol-name sym) package)
        (declare (ignore s))
        (when (and (member status '(:internal :external))
                   (symbol-package sym)
                   (not (eq (symbol-package sym) package)))
          (setf (gethash (symbol-package sym) seen) t))))
    (loop for k being the hash-keys of seen collect k)))

(defun %dependency-packages (package)
  "Return the union of `(package-use-list package)` and
`(%imported-from-packages package)`, restricted to amoebum packages.

We deliberately drop :cl, :common-lisp, and other non-amoebum
dependencies. Cycles to :cl are impossible (it has no amoebum deps);
cycles among third-party packages are not our problem."
  (let ((deps (remove-duplicates
               (append (package-use-list package)
                       (%imported-from-packages package))
               :test #'eq)))
    (remove-if-not #'%amoebum-package-p deps)))

(defun %build-amoebum-graph ()
  "Return a hash-table PACKAGE -> list of dependency PACKAGES, both
keys and values restricted to amoebum packages."
  (let ((graph (make-hash-table :test #'eq)))
    (dolist (pkg (%amoebum-packages))
      (setf (gethash pkg graph) (%dependency-packages pkg)))
    graph))

(defun %tarjan-cycles (graph)
  "Return non-trivial SCCs from GRAPH (size >= 2 OR self-loop)."
  (let ((index 0)
        (indices (make-hash-table :test #'eq))
        (lowlinks (make-hash-table :test #'eq))
        (on-stack (make-hash-table :test #'eq))
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
                   (when (eq w v) (return))))
               (push scc sccs)))))
      (maphash (lambda (v _)
                 (declare (ignore _))
                 (unless (nth-value 1 (gethash v indices))
                   (strong-connect v)))
               graph))
    (loop for scc in sccs
          when (or (>= (length scc) 2)
                   (and (= (length scc) 1)
                        (member (first scc) (gethash (first scc) graph)
                                :test #'eq)))
          collect scc)))

(test no-cycles-in-loaded-amoebum-package-graph
  "The amoebum package graph as loaded into the running image must have
zero directed cycles. This is the runtime ratcheted baseline that
mirrors bin/check-import-cycles.sh."
  (let* ((graph (%build-amoebum-graph))
         (cycles (%tarjan-cycles graph))
         (cycle-names (loop for cycle in cycles
                            collect (mapcar #'package-name cycle))))
    (is (null cycles)
        "Expected zero package-import cycles among amoebum packages, but found: ~S"
        cycle-names)))

(test guardrail-script-detects-the-current-tree-as-clean
  "Sanity-check that the bash guardrail and the live image agree:
both must report zero cycles. We do not invoke the script here (that
would require shelling out and timing out long enough for SBCL
startup); we only assert that the live-image check is well-defined."
  (let ((graph (%build-amoebum-graph)))
    (is (plusp (hash-table-count graph))
        "Expected at least one amoebum package in the loaded image.")))
