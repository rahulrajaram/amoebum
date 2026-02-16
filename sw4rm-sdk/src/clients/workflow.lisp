;;;; workflow.lisp - Workflow client stubs for SW4RM SDK
;;;;
;;;; Local stubs that preserve workflow-client call signatures used by tests
;;;; while signaling UNIMPLEMENTED RPC errors.

(in-package :sw4rm-sdk)

(defclass workflow-client (base-client)
  ()
  (:documentation "In-process workflow client stub.

All workflow RPC methods are intentionally unimplemented and signal
RPC-ERROR with status code \"UNIMPLEMENTED\" to preserve a stable
fallback behavior until the full client transport is wired in."))

(defun %unimplemented-workflow-error ()
  (error 'rpc-error
         :message "Workflow API is not implemented in this runtime"
         :status-code "UNIMPLEMENTED"
         :details "Local workflow client stubs are pending implementation"))

(defgeneric submit-dag (client dag)
  (:documentation "Submit a workflow DAG for execution."))

(defgeneric get-workflow-status (client workflow-id)
  (:documentation "Get workflow status by ID."))

(defgeneric cancel-workflow (client workflow-id reason)
  (:documentation "Cancel an active workflow by ID."))

(defgeneric resume-workflow (client workflow-id node-id)
  (:documentation "Resume a suspended workflow node by ID."))

(defgeneric list-workflows (client &key status)
  (:documentation "List workflows optionally filtered by status."))

(defmethod submit-dag ((client workflow-client) dag)
  (declare (ignore dag))
  (%unimplemented-workflow-error))

(defmethod get-workflow-status ((client workflow-client) workflow-id)
  (declare (ignore workflow-id))
  (%unimplemented-workflow-error))

(defmethod cancel-workflow ((client workflow-client) workflow-id reason)
  (declare (ignore workflow-id reason))
  (%unimplemented-workflow-error))

(defmethod resume-workflow ((client workflow-client) workflow-id node-id)
  (declare (ignore workflow-id node-id))
  (%unimplemented-workflow-error))

(defmethod list-workflows ((client workflow-client) &key status)
  (declare (ignore status))
  (%unimplemented-workflow-error))

