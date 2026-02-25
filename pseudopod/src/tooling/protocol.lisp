(in-package :pseudopod)

(defclass tool-execution-context ()
  ((toolset
    :initarg :toolset
    :accessor tool-execution-context-toolset
    :documentation "Tool registry used to resolve and execute tool calls.")
   (metadata
    :initarg :metadata
    :initform nil
    :accessor tool-execution-context-metadata
    :documentation "Opaque extension metadata attached by higher layers."))
  (:documentation "Base context passed to EXECUTE-TOOL specializations."))

(defgeneric execute-tool (tool-call context)
  (:documentation
   "Execute TOOL-CALL in CONTEXT.

Pseudopod defines the dispatch contract only. Consumers provide primary and
optional :around/:before/:after methods to compose policy, observability, and
dispatch behavior."))
