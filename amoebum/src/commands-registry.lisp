(in-package :amoebum)

(defun register-builtin-slash-commands ()
  (register-core-slash-commands)
  (register-memory-slash-commands)
  ;; NXT-577: /deftool registers a new tool in *toolset* live from chat.
  (register-deftool-slash-command)
  (register-agent-slash-commands)
  (register-extension-slash-commands)
  ;; NXT-582: /save-image, /load-image, /list-images.
  (register-image-slash-commands)
  (register-session-slash-commands)
  (register-hook-slash-commands)
  (register-notification-slash-commands)
  (register-phase5-slash-commands)
  (register-slash-command
   (make-slash-command
    :name "history"
    :description "Search persisted conversation history by content/role/tool/time."
    :usage "/history [query...] [--role system|user|assistant|tool] [--tool NAME] [--since TIMESTAMP] [--until TIMESTAMP] [--limit N]"
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional query text and filters."))
    :handler #'amoebum.commands.history:%history-handler))
  (register-slash-command
   (make-slash-command
    :name "index"
    :description "Generate or refresh the codebase symbol index and repo map."
    :usage "/index [--refresh] [--tokens N] [--system NAME ...]"
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional flags: --refresh, --tokens N (300-8000), --system NAME."))
    :handler #'amoebum.commands.index:%index-handler))
  (register-slash-command
   (make-slash-command
    :name "self-modify"
    :description "Propose and evaluate self-modification forms with sandboxed approval workflow."
    :usage "/self-modify <lisp-form> | /self-modify approve|deny <id> | /self-modify edit-approve <id> <lisp-form> | /self-modify pending"
    :parameters
    (list (make-slash-command-parameter
           :name "form"
           :type :string
           :required-p t
           :greedy-p t
           :description "Either a Lisp form or a self-modify subcommand payload."))
    :handler #'amoebum.commands.self-modify:%self-modify-handler))
  (register-slash-command
   (make-slash-command
    :name "permissions"
    :description "Inspect permission cache/decision traces and path approval memory."
    :usage "/permissions [stats|session [once|session|always]|reset [session|all]|log [limit]|explain [decision-id|latest]]"
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional action: stats, session/reset, log, or explain <decision-id>."))
    :handler #'amoebum.commands.permissions:%permissions-handler
    :completer #'amoebum.commands.permissions:%permissions-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "heap"
    :description "Full GC + heap snapshot: memory summary and top instance types."
    :usage "/heap [top-n]"
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional: number of top instance types to show (default 30)."))
    :handler #'%heap-handler))
  t)

(register-builtin-slash-commands)
