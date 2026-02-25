(in-package :pseudopod)

(defclass tool-execution-context ()
  ((toolset :initarg :toolset
            :initform (make-toolset)
            :accessor context-toolset)
   (metadata :initarg :metadata
             :initform nil
             :accessor context-metadata))
  (:documentation "Base execution context for tool-call method combinations."))

(defgeneric execute-tool (tool-call context)
  (:documentation
   "Execute TOOL-CALL in CONTEXT through the tool execution pipeline."))

(defmethod execute-tool ((tool-call tool-call)
                         (context tool-execution-context))
  "Primary method delegates to INVOKE-TOOL-CALL in pseudopod."
  (invoke-tool-call (context-toolset context) tool-call))
