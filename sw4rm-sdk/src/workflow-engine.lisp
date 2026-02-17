;;;; workflow-engine.lisp
;;;; Local DAG workflow builder/executor utilities.

(in-package :sw4rm-sdk)

(defstruct workflow-node
  "Workflow node definition."
  (node-id nil :type string)
  (action nil :type (or null function))
  (depends-on nil :type list)
  (condition nil :type (or null function))
  (metadata nil :type list))

(defstruct workflow-edge
  "Directed dependency edge between nodes."
  (from-node nil :type string)
  (to-node nil :type string)
  (condition nil :type (or null function)))

(defstruct workflow-definition
  "Reusable workflow specification."
  (workflow-id nil :type string)
  (nodes nil :type list)
  (edges nil :type list)
  (metadata nil :type list))

(defstruct workflow-run
  "Runtime state for a workflow execution."
  (run-id nil :type string)
  (workflow-id nil :type string)
  (node-states (make-hash-table :test #'equal) :type hash-table)
  (node-order nil :type list)
  (context (make-hash-table :test #'equal) :type hash-table)
  (status :pending :type keyword)
  (started-at nil :type (or null integer))
  (finished-at nil :type (or null integer))
  (error nil :type t))

(defclass workflow-engine ()
  ((definitions
    :initform (make-hash-table :test #'equal)
    :accessor workflow-engine-definitions)
   (runs
    :initform (make-hash-table :test #'equal)
    :accessor workflow-engine-runs)
   (lock
    :initform (bt:make-lock "workflow-engine-lock")
    :accessor workflow-engine-lock)))

(defun make-workflow-engine ()
  "Construct a workflow engine."
  (make-instance 'workflow-engine))

(defun workflow-node-map (workflow)
  "Build node-id -> node table for WORKFLOW."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (node (workflow-definition-nodes workflow))
      (setf (gethash (workflow-node-node-id node) table) node))
    table))

(defun add-node (workflow node)
  "Return a copy of WORKFLOW with NODE appended."
  (check-type workflow workflow-definition)
  (check-type node workflow-node)
  (let ((nodes (append (workflow-definition-nodes workflow) (list node))))
    (make-workflow-definition
     :workflow-id (workflow-definition-workflow-id workflow)
     :nodes nodes
     :edges (copy-list (workflow-definition-edges workflow))
     :metadata (copy-list (workflow-definition-metadata workflow)))))

(defun add-edge (workflow edge)
  "Return a copy of WORKFLOW with EDGE appended."
  (check-type workflow workflow-definition)
  (check-type edge workflow-edge)
  (make-workflow-definition
   :workflow-id (workflow-definition-workflow-id workflow)
   :nodes (copy-list (workflow-definition-nodes workflow))
   :edges (append (workflow-definition-edges workflow) (list edge))
   :metadata (copy-list (workflow-definition-metadata workflow))))

(defun %incoming-edges (workflow node-id)
  (remove-if-not (lambda (edge) (string= (workflow-edge-to-node edge) node-id))
                 (workflow-definition-edges workflow)))

(defun %outgoing-edges (workflow node-id)
  (remove-if-not (lambda (edge) (string= (workflow-edge-from-node edge) node-id))
                 (workflow-definition-edges workflow)))

(defun topological-sort (workflow)
  "Return node IDs in a valid topological order.

Signals VALIDATION-ERROR when cycles are detected."
  (let* ((nodes (workflow-definition-nodes workflow))
         (remaining (copy-list (mapcar #'workflow-node-node-id nodes)))
         (sorted nil))
    (loop
      while remaining
      do
         (let ((progress nil))
           (dolist (node-id (copy-list remaining))
             (let ((incoming (%incoming-edges workflow node-id)))
               (when (every (lambda (edge)
                              (member (workflow-edge-from-node edge) sorted :test #'string=))
                            incoming)
                 (push node-id sorted)
                 (setf remaining (remove node-id remaining :test #'string=))
                 (setf progress t))))
           (unless progress
             (error 'validation-error
                    :message "Workflow graph contains a cycle"
                    :field "workflow.edges"
                    :constraint "acyclic graph"))))
    (nreverse sorted)))

(defun %edge-active-p (edge context)
  (if (workflow-edge-condition edge)
      (funcall (workflow-edge-condition edge) context)
      t))

(defun %node-ready-p (workflow run node-id)
  (let ((incoming (%incoming-edges workflow node-id)))
    (every (lambda (edge)
             (let ((from-state (gethash (workflow-edge-from-node edge)
                                        (workflow-run-node-states run))))
               (or (not (%edge-active-p edge (workflow-run-context run)))
                   (member from-state '(:completed :skipped) :test #'eq))))
           incoming)))

(defun %execute-node (run node)
  (let* ((node-id (workflow-node-node-id node))
         (context (workflow-run-context run))
         (condition-fn (workflow-node-condition node))
         (action (workflow-node-action node)))
    (handler-case
        (if (and condition-fn (not (funcall condition-fn context)))
            (setf (gethash node-id (workflow-run-node-states run)) :skipped)
            (progn
              (setf (gethash node-id (workflow-run-node-states run)) :running)
              (let ((result (if action
                                (funcall action context node)
                                (list :ok t :node-id node-id))))
                (setf (gethash node-id context) result))
              (setf (gethash node-id (workflow-run-node-states run)) :completed)))
      (error (condition)
        (setf (gethash node-id (workflow-run-node-states run)) :failed)
        (setf (workflow-run-status run) :failed)
        (setf (workflow-run-error run) condition)
        (error condition)))))

(defun execute-workflow (workflow &key context)
  "Execute WORKFLOW and return WORKFLOW-RUN."
  (check-type workflow workflow-definition)
  (let* ((run (make-workflow-run
               :run-id (generate-uuid)
               :workflow-id (workflow-definition-workflow-id workflow)
               :status :running
               :started-at (get-universal-time)
               :context (or context (make-hash-table :test #'equal))))
         (order (topological-sort workflow))
         (nodes-by-id (workflow-node-map workflow))
         (pending (copy-list order)))
    (dolist (node-id order)
      (setf (gethash node-id (workflow-run-node-states run)) :pending))
    (loop
      while pending
      do
         (let ((ready nil))
           (dolist (node-id pending)
             (when (%node-ready-p workflow run node-id)
               (push node-id ready)))
           (when (null ready)
             (setf (workflow-run-status run) :failed)
             (setf (workflow-run-error run)
                   (make-condition 'validation-error
                                   :message "No schedulable nodes remain (cycle or unsatisfied condition)"
                                   :field "workflow"
                                   :constraint "schedulable"))
             (return))
           (dolist (node-id (nreverse ready))
             (%execute-node run (gethash node-id nodes-by-id))
             (push node-id (workflow-run-node-order run))
             (setf pending (remove node-id pending :test #'string=))))
      finally
         (unless (eq (workflow-run-status run) :failed)
           (setf (workflow-run-status run) :completed))
         (setf (workflow-run-finished-at run) (get-universal-time)))
    run))

(defun serialize-workflow-state (run)
  "Serialize WORKFLOW-RUN state to a plist."
  (check-type run workflow-run)
  (let ((states nil)
        (context-items nil))
    (maphash (lambda (key value) (push (cons key value) states))
             (workflow-run-node-states run))
    (maphash (lambda (key value) (push (cons key value) context-items))
             (workflow-run-context run))
    (list :run-id (workflow-run-run-id run)
          :workflow-id (workflow-run-workflow-id run)
          :node-states states
          :node-order (reverse (workflow-run-node-order run))
          :context context-items
          :status (workflow-run-status run)
          :started-at (workflow-run-started-at run)
          :finished-at (workflow-run-finished-at run))))

(defun restore-workflow-state (serialized)
  "Restore a WORKFLOW-RUN from SERIALIZED state."
  (let ((run (make-workflow-run
              :run-id (getf serialized :run-id)
              :workflow-id (getf serialized :workflow-id)
              :status (or (getf serialized :status) :pending)
              :started-at (getf serialized :started-at)
              :finished-at (getf serialized :finished-at))))
    (dolist (pair (getf serialized :node-states))
      (setf (gethash (car pair) (workflow-run-node-states run)) (cdr pair)))
    (dolist (pair (getf serialized :context))
      (setf (gethash (car pair) (workflow-run-context run)) (cdr pair)))
    (setf (workflow-run-node-order run) (reverse (copy-list (getf serialized :node-order))))
    run))

(defun register-workflow (engine workflow)
  "Register WORKFLOW in ENGINE by workflow-id."
  (check-type engine workflow-engine)
  (check-type workflow workflow-definition)
  (bt:with-lock-held ((workflow-engine-lock engine))
    (setf (gethash (workflow-definition-workflow-id workflow)
                   (workflow-engine-definitions engine))
          workflow))
  workflow)

(defun run-workflow (engine workflow-id &key context)
  "Execute a registered workflow and record run history."
  (check-type engine workflow-engine)
  (bt:with-lock-held ((workflow-engine-lock engine))
    (let ((workflow (gethash workflow-id (workflow-engine-definitions engine))))
      (unless workflow
        (error 'rpc-error
               :message (format nil "Workflow '~A' not found" workflow-id)
               :status-code "NOT_FOUND"
               :details "unknown workflow-id"))
      (let ((run (execute-workflow workflow :context context)))
        (setf (gethash (workflow-run-run-id run) (workflow-engine-runs engine)) run)
        run))))

(defun get-workflow-run (engine run-id)
  "Fetch a prior workflow run by RUN-ID."
  (check-type engine workflow-engine)
  (gethash run-id (workflow-engine-runs engine)))

(defun list-workflow-runs (engine &key status)
  "List workflow runs, optionally filtered by STATUS."
  (check-type engine workflow-engine)
  (let ((runs nil))
    (maphash (lambda (_ run)
               (declare (ignore _))
               (when (or (null status) (eq (workflow-run-status run) status))
                 (push run runs)))
             (workflow-engine-runs engine))
    runs))

(defmacro defworkflow (name &body forms)
  "Define a workflow definition constant from declarative forms.

Supported forms:
  (:node <id> [:depends-on (<id> ...)] [:condition fn] [:action fn] ...metadata...)
  (:edge <from> <to> [:condition fn])"
  (let ((node-forms nil)
        (edge-forms nil))
    (dolist (form forms)
      (case (first form)
        (:node
         (let* ((node-id (second form))
                (plist (copy-list (cddr form)))
                (depends-on (or (getf plist :depends-on) nil))
                (condition (getf plist :condition))
                (action (getf plist :action))
                (metadata (copy-list plist)))
           (remf metadata :depends-on)
           (remf metadata :condition)
           (remf metadata :action)
           (push `(make-workflow-node
                   :node-id ,node-id
                   :depends-on ',depends-on
                   :condition ,condition
                   :action ,action
                   :metadata ',metadata)
                 node-forms)
           (dolist (dependency depends-on)
             (push `(make-workflow-edge
                     :from-node ,dependency
                     :to-node ,node-id)
                   edge-forms))))
        (:edge
         (let ((from (second form))
               (to (third form))
               (condition (getf (cdddr form) :condition)))
           (push `(make-workflow-edge
                   :from-node ,from
                   :to-node ,to
                   :condition ,condition)
                 edge-forms)))
        (otherwise
         (error "Unsupported defworkflow form: ~S" form))))
    `(defparameter ,name
       (make-workflow-definition
        :workflow-id ,(string-downcase (symbol-name name))
        :nodes (list ,@(nreverse node-forms))
        :edges (list ,@(nreverse edge-forms))))))

(defun make-feature-workflow-template ()
  "Built-in feature implementation workflow template."
  (make-workflow-definition
   :workflow-id "feature-implementation"
   :nodes (list (make-workflow-node :node-id "analyze")
                (make-workflow-node :node-id "plan")
                (make-workflow-node :node-id "implement")
                (make-workflow-node :node-id "test")
                (make-workflow-node :node-id "review")
                (make-workflow-node :node-id "document"))
   :edges (list (make-workflow-edge :from-node "analyze" :to-node "plan")
                (make-workflow-edge :from-node "plan" :to-node "implement")
                (make-workflow-edge :from-node "implement" :to-node "test")
                (make-workflow-edge :from-node "test" :to-node "review")
                (make-workflow-edge :from-node "review" :to-node "document"))))

(defun make-bugfix-workflow-template ()
  "Built-in bug-fix workflow template."
  (make-workflow-definition
   :workflow-id "bug-fix"
   :nodes (list (make-workflow-node :node-id "reproduce")
                (make-workflow-node :node-id "diagnose")
                (make-workflow-node :node-id "fix")
                (make-workflow-node :node-id "regression-test")
                (make-workflow-node :node-id "review"))
   :edges (list (make-workflow-edge :from-node "reproduce" :to-node "diagnose")
                (make-workflow-edge :from-node "diagnose" :to-node "fix")
                (make-workflow-edge :from-node "fix" :to-node "regression-test")
                (make-workflow-edge :from-node "regression-test" :to-node "review"))))

(defun make-refactor-workflow-template ()
  "Built-in refactor workflow template."
  (make-workflow-definition
   :workflow-id "refactor"
   :nodes (list (make-workflow-node :node-id "analyze-scope")
                (make-workflow-node :node-id "plan-refactor")
                (make-workflow-node :node-id "implement-refactor")
                (make-workflow-node :node-id "verify")
                (make-workflow-node :node-id "review"))
   :edges (list (make-workflow-edge :from-node "analyze-scope" :to-node "plan-refactor")
                (make-workflow-edge :from-node "plan-refactor" :to-node "implement-refactor")
                (make-workflow-edge :from-node "implement-refactor" :to-node "verify")
                (make-workflow-edge :from-node "verify" :to-node "review"))))
