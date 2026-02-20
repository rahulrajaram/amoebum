#.(progn (require :asdf) nil)

(let* ((smoke-file (or *load-truename* *compile-file-truename*))
       (amoebum-dir (and smoke-file (make-pathname :name nil :type nil :defaults smoke-file)))
       (repo-root (and amoebum-dir (truename (merge-pathnames #P"../" amoebum-dir)))))
  (unless repo-root
    (error "Unable to resolve repository root from ~S" smoke-file))

  (load (merge-pathnames #P"ptui/.tools/quicklisp/setup.lisp" repo-root))
  (require :asdf)

  (let* ((asdf-pkg (or (find-package "ASDF")
                       (error "Missing package ASDF")))
         (load-asd-sym (or (find-symbol "LOAD-ASD" asdf-pkg)
                           (error "Missing symbol LOAD-ASD in ASDF package")))
         (load-system-sym (or (find-symbol "LOAD-SYSTEM" asdf-pkg)
                              (error "Missing symbol LOAD-SYSTEM in ASDF package")))
         (load-asd-fn (symbol-function load-asd-sym))
         (load-system-fn (symbol-function load-system-sym)))
    (funcall load-asd-fn (merge-pathnames #P"pseudopod/pseudopod.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"ptui/ptui.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"amoebum/amoebum.asd" repo-root))
    (funcall load-system-fn "amoebum"))

  (let* ((amoebum-pkg (or (find-package "AMOEBUM")
                          (error "Missing package AMOEBUM after load.")))
         (pseudopod-pkg (or (find-package "PSEUDOPOD")
                            (error "Missing package PSEUDOPOD after load.")))
         (uiop-pkg (or (find-package "UIOP")
                       (find-package "ASDF/UTILITY")
                       (error "Missing UIOP package after requiring ASDF.")))
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn-in
           (lambda (name package)
             (symbol-function (funcall symbol-in name package))))
         (ensure-directory-pathname
           (symbol-function (funcall symbol-in "ENSURE-DIRECTORY-PATHNAME" uiop-pkg)))
         (temporary-directory
           (symbol-function (funcall symbol-in "TEMPORARY-DIRECTORY" uiop-pkg)))
         (make-lsp-server-spec-fn (funcall fn-in "MAKE-LSP-SERVER-SPEC" amoebum-pkg))
         (lsp-language-id-for-path-fn (funcall fn-in "LSP-LANGUAGE-ID-FOR-PATH" amoebum-pkg))
         (lsp-server-spec-for-path-fn (funcall fn-in "LSP-SERVER-SPEC-FOR-PATH" amoebum-pkg))
         (lsp-server-spec-command-fn (funcall fn-in "LSP-SERVER-SPEC-COMMAND" amoebum-pkg))
         (make-lsp-client-fn (funcall fn-in "MAKE-LSP-CLIENT" amoebum-pkg))
         (lsp-open-document-fn (funcall fn-in "LSP-OPEN-DOCUMENT" amoebum-pkg))
         (lsp-send-request-fn (funcall fn-in "LSP-SEND-REQUEST" amoebum-pkg))
         (lsp-client-connection-fn (funcall fn-in "LSP-CLIENT-CONNECTION" amoebum-pkg))
         (lsp-server-connection-process-fn
          (funcall fn-in "LSP-SERVER-CONNECTION-PROCESS" amoebum-pkg))
         (lsp-server-connection-restart-count-fn
          (funcall fn-in "LSP-SERVER-CONNECTION-RESTART-COUNT" amoebum-pkg))
         (lsp-client-stop-fn (funcall fn-in "LSP-CLIENT-STOP" amoebum-pkg))
         (find-tool-fn (funcall fn-in "FIND-TOOL" pseudopod-pkg))
         (tool-definition-fn-fn (funcall fn-in "TOOL-DEFINITION-FN" pseudopod-pkg))
         (toolset-sym (funcall symbol-in "*TOOLSET*" amoebum-pkg))
         (lsp-tool-client-sym (funcall symbol-in "*LSP-TOOL-CLIENT*" amoebum-pkg))
         (lsp-tool-client-project-root-sym
          (funcall symbol-in "*LSP-TOOL-CLIENT-PROJECT-ROOT*" amoebum-pkg))
         (lsp-tool-diagnostics-cache-sym
          (funcall symbol-in "*LSP-TOOL-DIAGNOSTICS-CACHE*" amoebum-pkg))
         (reset-lsp-tool-runtime-fn (funcall fn-in "RESET-LSP-TOOL-RUNTIME" amoebum-pkg)))
    (labels
        ((assert-true (condition format-string &rest format-args)
           (unless condition
             (error (apply #'format nil format-string format-args))))
         (contains-substring-p (needle haystack)
           (and (stringp haystack)
                (search needle haystack :test #'char-equal)))
         (make-args (&rest key-values)
           (let ((args (make-hash-table :test #'equal)))
             (loop for (key value) on key-values by #'cddr do
               (setf (gethash key args) value))
             args))
         (invoke-tool (tool-name &rest key-values)
           (let* ((toolset (symbol-value toolset-sym))
                  (tool (funcall find-tool-fn toolset tool-name)))
             (assert-true tool "Expected tool ~S to be registered." tool-name)
             (funcall (funcall tool-definition-fn-fn tool)
                      (apply #'make-args key-values))))
         (command-available-p (command)
           (handler-case
               (progn
                 (uiop:run-program (list command "--version")
                                   :ignore-error-status t
                                   :output :string
                                   :error-output :string)
                 t)
             (error ()
               nil)))
         (resolve-python-command ()
           (or (and (command-available-p "python3") "python3")
               (and (command-available-p "python") "python")
               (error "LSP smoke test requires python3 or python.")))
         (wait-until (predicate timeout-seconds &optional (poll-seconds 0.05d0))
           (let ((deadline (+ (get-internal-real-time)
                              (ceiling (* timeout-seconds
                                          internal-time-units-per-second)))))
             (loop
               do
                  (when (funcall predicate)
                    (return t))
                  (when (>= (get-internal-real-time) deadline)
                    (return nil))
                  (sleep poll-seconds))))
         (process-running-p (process)
           #+sbcl
           (and process (ignore-errors (sb-ext:process-alive-p process)))
           #-sbcl
           (declare (ignore process))
           #-sbcl
           nil)
         (write-file-text (path lines)
           (ensure-directories-exist path)
           (with-open-file (stream path
                                   :direction :output
                                   :if-does-not-exist :create
                                   :if-exists :supersede
                                   :external-format :utf-8)
             (dolist (line lines)
               (format stream "~A~%" line))))
         (write-mock-lsp-server-script (script-path)
           (write-file-text
            script-path
            '("import json"
              "import os"
              "import sys"
              ""
              "log_path = sys.argv[1]"
              "last_open_uri = None"
              ""
              "def log(event, payload=None):"
              "    with open(log_path, \"a\", encoding=\"utf-8\") as handle:"
              "        handle.write(json.dumps({\"event\": event, \"payload\": payload}) + \"\\n\")"
              ""
              "def read_message():"
              "    headers = {}"
              "    while True:"
              "        line = sys.stdin.buffer.readline()"
              "        if not line:"
              "            return None"
              "        if line in (b\"\\r\\n\", b\"\\n\"):"
              "            break"
              "        if b\":\" not in line:"
              "            continue"
              "        key, value = line.split(b\":\", 1)"
              "        headers[key.strip().lower()] = value.strip()"
              "    length = int(headers.get(b\"content-length\", b\"0\"))"
              "    payload = sys.stdin.buffer.read(length)"
              "    if len(payload) != length:"
              "        return None"
              "    return json.loads(payload.decode(\"utf-8\"))"
              ""
              "def send_message(message):"
              "    payload = json.dumps(message, separators=(\",\", \":\")).encode(\"utf-8\")"
              "    header = f\"Content-Length: {len(payload)}\\r\\n\\r\\n\".encode(\"ascii\")"
              "    sys.stdout.buffer.write(header)"
              "    sys.stdout.buffer.write(payload)"
              "    sys.stdout.buffer.flush()"
              ""
              "def send_publish_diagnostics(uri):"
              "    send_message({"
              "        \"jsonrpc\": \"2.0\","
              "        \"method\": \"textDocument/publishDiagnostics\","
              "        \"params\": {"
              "            \"uri\": uri,"
              "            \"diagnostics\": ["
              "                {"
              "                    \"range\": {\"start\": {\"line\": 3, \"character\": 0}, \"end\": {\"line\": 3, \"character\": 5}},"
              "                    \"severity\": 2,"
              "                    \"source\": \"mock-lsp\","
              "                    \"message\": \"Mock warning from publishDiagnostics\""
              "                }"
              "            ]"
              "        }"
              "    })"
              ""
              "while True:"
              "    message = read_message()"
              "    if message is None:"
              "        break"
              "    method = message.get(\"method\")"
              "    request_id = message.get(\"id\")"
              "    params = message.get(\"params\") or {}"
              ""
              "    if method == \"initialize\":"
              "        log(\"initialize\")"
              "        if request_id is not None:"
              "            send_message({\"jsonrpc\": \"2.0\", \"id\": request_id, \"result\": {\"capabilities\": {}}})"
              "        continue"
              ""
              "    if method == \"initialized\":"
              "        log(\"initialized\")"
              "        continue"
              ""
              "    if method == \"textDocument/didOpen\":"
              "        text_document = params.get(\"textDocument\") or {}"
              "        uri = text_document.get(\"uri\")"
              "        log(\"didOpen\", uri)"
              "        if uri:"
              "            last_open_uri = uri"
              "            send_publish_diagnostics(uri)"
              "        continue"
              ""
              "    if method == \"textDocument/definition\":"
              "        if request_id is not None:"
              "            send_message({"
              "                \"jsonrpc\": \"2.0\","
              "                \"id\": request_id,"
              "                \"result\": {"
              "                    \"uri\": (params.get(\"textDocument\") or {}).get(\"uri\"),"
              "                    \"range\": {\"start\": {\"line\": 0, \"character\": 4}, \"end\": {\"line\": 0, \"character\": 10}}"
              "                }"
              "            })"
              "        continue"
              ""
              "    if method == \"textDocument/references\":"
              "        if request_id is not None:"
              "            uri = (params.get(\"textDocument\") or {}).get(\"uri\")"
              "            send_message({"
              "                \"jsonrpc\": \"2.0\","
              "                \"id\": request_id,"
              "                \"result\": ["
              "                    {\"uri\": uri, \"range\": {\"start\": {\"line\": 0, \"character\": 4}, \"end\": {\"line\": 0, \"character\": 10}}},"
              "                    {\"uri\": uri, \"range\": {\"start\": {\"line\": 3, \"character\": 8}, \"end\": {\"line\": 3, \"character\": 14}}}"
              "                ]"
              "            })"
              "        continue"
              ""
              "    if method == \"textDocument/documentSymbol\":"
              "        if request_id is not None:"
              "            uri = (params.get(\"textDocument\") or {}).get(\"uri\")"
              "            send_message({"
              "                \"jsonrpc\": \"2.0\","
              "                \"id\": request_id,"
              "                \"result\": ["
              "                    {"
              "                        \"name\": \"answer\","
              "                        \"kind\": 12,"
              "                        \"detail\": \"answer() -> int\","
              "                        \"selectionRange\": {\"start\": {\"line\": 0, \"character\": 4}, \"end\": {\"line\": 0, \"character\": 10}},"
              "                        \"range\": {\"start\": {\"line\": 0, \"character\": 0}, \"end\": {\"line\": 1, \"character\": 13}}"
              "                    },"
              "                    {"
              "                        \"name\": \"value\","
              "                        \"kind\": 13,"
              "                        \"selectionRange\": {\"start\": {\"line\": 3, \"character\": 0}, \"end\": {\"line\": 3, \"character\": 5}},"
              "                        \"range\": {\"start\": {\"line\": 3, \"character\": 0}, \"end\": {\"line\": 3, \"character\": 16}},"
              "                        \"uri\": uri"
              "                    }"
              "                ]"
              "            })"
              "        continue"
              ""
              "    if method == \"workspace/symbol\":"
              "        if request_id is not None:"
              "            query = (params.get(\"query\") or \"\").lower()"
              "            symbols = ["
              "                {"
              "                    \"name\": \"answer\","
              "                    \"kind\": 12,"
              "                    \"containerName\": \"sample\","
              "                    \"location\": {\"uri\": last_open_uri, \"range\": {\"start\": {\"line\": 0, \"character\": 4}, \"end\": {\"line\": 0, \"character\": 10}}}"
              "                },"
              "                {"
              "                    \"name\": \"value\","
              "                    \"kind\": 13,"
              "                    \"containerName\": \"sample\","
              "                    \"location\": {\"uri\": last_open_uri, \"range\": {\"start\": {\"line\": 3, \"character\": 0}, \"end\": {\"line\": 3, \"character\": 5}}}"
              "                }"
              "            ]"
              "            if query:"
              "                symbols = [entry for entry in symbols if query in (entry.get(\"name\") or \"\").lower()]"
              "            send_message({\"jsonrpc\": \"2.0\", \"id\": request_id, \"result\": symbols})"
              "        continue"
              ""
              "    if method == \"textDocument/hover\":"
              "        if request_id is not None:"
              "            send_message({"
              "                \"jsonrpc\": \"2.0\","
              "                \"id\": request_id,"
              "                \"result\": {"
              "                    \"contents\": {\"kind\": \"markdown\", \"value\": \"`answer() -> int`\\n\\nReturns the answer.\"},"
              "                    \"range\": {\"start\": {\"line\": 3, \"character\": 8}, \"end\": {\"line\": 3, \"character\": 14}}"
              "                }"
              "            })"
              "        continue"
              ""
              "    if method == \"textDocument/diagnostic\":"
              "        if request_id is not None:"
              "            uri = ((params.get(\"textDocument\") or {}).get(\"uri\"))"
              "            send_message({"
              "                \"jsonrpc\": \"2.0\","
              "                \"id\": request_id,"
              "                \"result\": {"
              "                    \"kind\": \"full\","
              "                    \"items\": ["
              "                        {"
              "                            \"uri\": uri,"
              "                            \"range\": {\"start\": {\"line\": 1, \"character\": 4}, \"end\": {\"line\": 1, \"character\": 10}},"
              "                            \"severity\": 1,"
              "                            \"source\": \"mock-lsp\","
              "                            \"message\": \"Mock error from pull diagnostics\""
              "                        }"
              "                    ]"
              "                }"
              "            })"
              "        continue"
              ""
              "    if method == \"workspace/mockEcho\":"
              "        if request_id is not None:"
              "            send_message({\"jsonrpc\": \"2.0\", \"id\": request_id, \"result\": {\"echo\": params, \"pid\": os.getpid()}})"
              "        continue"
              ""
              "    if method == \"shutdown\":"
              "        log(\"shutdown\")"
              "        if request_id is not None:"
              "            send_message({\"jsonrpc\": \"2.0\", \"id\": request_id, \"result\": {\"ok\": True}})"
              "        continue"
              ""
              "    if method == \"exit\":"
              "        log(\"exit\")"
              "        break"
              ""
              "    if request_id is not None:"
              "        send_message({\"jsonrpc\": \"2.0\", \"id\": request_id, \"error\": {\"code\": -32601, \"message\": \"method not found\"}})")))
         (make-json-object (&rest pairs)
           (let ((object (make-hash-table :test #'equal)))
             (loop for (key value) on pairs by #'cddr do
               (setf (gethash key object) value))
             object))
         (read-json-lines (path)
           (when (probe-file path)
             (with-open-file (stream path :direction :input :external-format :utf-8)
               (let* ((jonathan-package (or (find-package :jonathan)
                                            (error "Missing package JONATHAN.")))
                      (parse-symbol (or (find-symbol "PARSE" jonathan-package)
                                        (error "Missing JONATHAN:PARSE.")))
                      (parse-fn (symbol-function parse-symbol)))
                 (loop for line = (read-line stream nil nil)
                       while line
                       collect (funcall parse-fn line :as :hash-table))))))
         (count-log-event (entries event-name)
           (count event-name
                  entries
                  :test #'string=
                  :key (lambda (entry) (gethash "event" entry)))))
      (let* ((python-command (resolve-python-command))
             (tmp-root
               (funcall ensure-directory-pathname
                        (merge-pathnames
                         (make-pathname :directory `(:relative ,(format nil "amoebum-i55-~A"
                                                                        (get-universal-time))))
                         (funcall temporary-directory))))
             (script-path (merge-pathnames #P"mock-lsp-server.py" tmp-root))
             (python-log-path (merge-pathnames #P"python-lsp-events.jsonl" tmp-root))
             (rust-log-path (merge-pathnames #P"rust-lsp-events.jsonl" tmp-root))
             (source-path (merge-pathnames #P"sample.py" tmp-root))
             (rust-source-path (merge-pathnames #P"sample.rs" tmp-root))
             (client nil))
        (assert-true (string= (funcall lsp-language-id-for-path-fn "probe.py") "python")
                     "Expected .py default language-id to resolve to python.")
        (assert-true (string= (funcall lsp-language-id-for-path-fn "probe.rs") "rust")
                     "Expected .rs default language-id to resolve to rust.")
        (assert-true (string= (funcall lsp-language-id-for-path-fn "probe.cpp") "cpp")
                     "Expected .cpp default language-id to resolve to cpp.")
        (let* ((py-spec (funcall lsp-server-spec-for-path-fn "probe.py"))
               (rs-spec (funcall lsp-server-spec-for-path-fn "probe.rs"))
               (cpp-spec (funcall lsp-server-spec-for-path-fn "probe.cpp")))
          (assert-true (string= (funcall lsp-server-spec-command-fn py-spec) "pyright-langserver")
                       "Expected .py default command to be pyright-langserver.")
          (assert-true (string= (funcall lsp-server-spec-command-fn rs-spec) "rust-analyzer")
                       "Expected .rs default command to be rust-analyzer.")
          (assert-true (string= (funcall lsp-server-spec-command-fn cpp-spec) "clangd")
                       "Expected .cpp default command to be clangd."))

        (write-mock-lsp-server-script script-path)
        (write-file-text source-path
                         '("def answer() -> int:"
                           "    return 42"
                           ""
                           "value = answer()"))
        (write-file-text rust-source-path
                         '("fn answer() -> i32 {"
                           "    42"
                           "}"
                           ""
                           "fn main() {"
                           "    let _ = answer();"
                           "}"))

        (setf client
              (funcall make-lsp-client-fn
                       :project-root tmp-root
                       :server-specs (list (funcall make-lsp-server-spec-fn
                                                    :name "pyright"
                                                    :language-id "python"
                                                    :command python-command
                                                    :args (list (namestring script-path)
                                                                (namestring python-log-path))
                                                    :file-extensions '(".py"))
                                           (funcall make-lsp-server-spec-fn
                                                    :name "rust-analyzer"
                                                    :language-id "rust"
                                                    :command python-command
                                                    :args (list (namestring script-path)
                                                                (namestring rust-log-path))
                                                    :file-extensions '(".rs")))
                       :initialize-timeout-seconds 2.0d0
                       :request-timeout-seconds 2.0d0
                       :restart-backoff-base-seconds 0.05d0
                       :restart-backoff-max-seconds 0.2d0))
        (assert-true (null (funcall lsp-client-connection-fn client "python"))
                     "Expected lazy startup: no Python connection before first use.")
        (assert-true (null (funcall lsp-client-connection-fn client "rust"))
                     "Expected lazy startup: no Rust connection before first use.")

        (unwind-protect
             (progn
               (funcall lsp-open-document-fn client source-path)
               (let* ((response (funcall lsp-send-request-fn
                                         client
                                         source-path
                                         "workspace/mockEcho"
                                         :params (make-json-object "phase" "initial")))
                      (result (and response (gethash "result" response))))
                 (assert-true (hash-table-p result)
                              "Expected initial mockEcho response result hash-table.")
                 (assert-true (hash-table-p (gethash "echo" result))
                              "Expected initial mockEcho echo payload."))
               (funcall lsp-open-document-fn client rust-source-path)
               (let* ((response (funcall lsp-send-request-fn
                                         client
                                         rust-source-path
                                         "workspace/mockEcho"
                                         :params (make-json-object "phase" "rust-initial")))
                      (result (and response (gethash "result" response))))
                 (assert-true (hash-table-p result)
                              "Expected rust mockEcho response result hash-table."))

               (let* ((python-connection (funcall lsp-client-connection-fn client "python"))
                      (rust-connection (funcall lsp-client-connection-fn client "rust"))
                      (python-process (and python-connection
                                           (funcall lsp-server-connection-process-fn
                                                    python-connection)))
                      (rust-process (and rust-connection
                                         (funcall lsp-server-connection-process-fn
                                                  rust-connection))))
                 (assert-true python-process
                              "Expected active Python LSP process after didOpen.")
                 (assert-true rust-process
                              "Expected active Rust LSP process after didOpen.")
                 (assert-true (not (eq python-process rust-process))
                              "Expected separate processes for python and rust LSP servers."))

               (let* ((connection (funcall lsp-client-connection-fn client "python"))
                      (process (and connection
                                    (funcall lsp-server-connection-process-fn connection))))
                 (assert-true process
                              "Expected active Python LSP process after didOpen.")
                 #+sbcl
                 (ignore-errors (sb-ext:process-kill process 9))
                 #-sbcl
                 (error "LSP smoke test requires SBCL process-kill support.")
                 (assert-true
                  (wait-until (lambda ()
                                #+sbcl
                                (not (ignore-errors (sb-ext:process-alive-p process)))
                                #-sbcl
                                t)
                              2.0d0)
                  "Expected killed LSP process to exit.")
                 (let* ((response (funcall lsp-send-request-fn
                                           client
                                           source-path
                                           "workspace/mockEcho"
                                           :params (make-json-object "phase" "restart")))
                        (result (and response (gethash "result" response)))
                        (connection-after (funcall lsp-client-connection-fn client "python"))
                        (process-after (and connection-after
                                            (funcall lsp-server-connection-process-fn
                                                     connection-after)))
                        (restart-count (and connection-after
                                            (funcall lsp-server-connection-restart-count-fn
                                                     connection-after))))
                   (assert-true (hash-table-p result)
                                "Expected restart mockEcho response result hash-table.")
                   (assert-true process-after
                               "Expected restart to relaunch Python LSP process.")
                 (assert-true (and restart-count (>= restart-count 1))
                              "Expected restart-count >= 1 after forced crash, got ~S."
                              restart-count)))

               (setf (symbol-value lsp-tool-client-sym) client
                     (symbol-value lsp-tool-client-project-root-sym) tmp-root)
               (clrhash (symbol-value lsp-tool-diagnostics-cache-sym))

               (let* ((definition-result (invoke-tool "lsp-goto-definition"
                                                      "path" (namestring source-path)
                                                      "line" 4
                                                      "column" 10))
                      (definitions (getf definition-result :definitions))
                      (definition (first definitions)))
                 (assert-true (>= (length definitions) 1)
                              "Expected lsp-goto-definition to return at least one location.")
                 (assert-true (string= (getf definition :path) (namestring source-path))
                              "Expected definition path to match source path, got ~S."
                              definition)
                 (assert-true (= (getf definition :line) 1)
                              "Expected definition line=1, got ~S." definition)
                 (assert-true (= (getf definition :column) 5)
                              "Expected definition column=5, got ~S." definition))

               (let* ((references-result (invoke-tool "lsp-find-references"
                                                      "path" (namestring source-path)
                                                      "line" 4
                                                      "column" 10))
                      (references (getf references-result :references)))
                 (assert-true (>= (length references) 2)
                              "Expected at least two references, got ~S."
                              references))

               (let* ((document-symbols-result (invoke-tool "lsp-document-symbols"
                                                            "path" (namestring source-path)))
                      (symbols (getf document-symbols-result :symbols))
                      (first-symbol (first symbols)))
                 (assert-true (>= (length symbols) 2)
                              "Expected at least two document symbols, got ~S."
                              symbols)
                 (assert-true (string= (getf first-symbol :name) "answer")
                              "Expected first document symbol to be answer, got ~S."
                              first-symbol)
                 (assert-true (eq (getf first-symbol :kind) :function)
                              "Expected first document symbol kind :function, got ~S."
                              first-symbol))

               (let* ((workspace-symbols-result (invoke-tool "lsp-workspace-symbols"
                                                             "path" (namestring source-path)
                                                             "query" "answer"))
                      (symbols (getf workspace-symbols-result :symbols))
                      (answer-entry (find "answer" symbols
                                          :test #'string=
                                          :key (lambda (entry) (getf entry :name)))))
                 (assert-true (>= (length symbols) 1)
                              "Expected at least one workspace symbol, got ~S."
                              symbols)
                 (assert-true answer-entry
                              "Expected workspace symbol query to include answer, got ~S."
                              symbols))

               (let* ((hover-result (invoke-tool "lsp-hover"
                                                 "path" (namestring source-path)
                                                 "line" 4
                                                 "column" 10))
                      (contents (getf hover-result :contents)))
                 (assert-true (contains-substring-p "answer() -> int" contents)
                              "Expected hover contents to include signature, got ~S."
                              contents))

               (sleep 0.1d0)
               (let* ((project-diagnostics (invoke-tool "lsp-diagnostics"))
                      (project-items (getf project-diagnostics :diagnostics)))
                 (assert-true (eq (getf project-diagnostics :scope) :project)
                              "Expected project diagnostics scope, got ~S."
                              project-diagnostics)
                 (assert-true (listp project-items)
                              "Expected project diagnostics list payload, got ~S."
                              project-items))

               (let* ((file-diagnostics (invoke-tool "lsp-diagnostics"
                                                     "path" (namestring source-path)))
                      (diagnostics (getf file-diagnostics :diagnostics))
                      (first-diagnostic (first diagnostics)))
                 (assert-true (eq (getf file-diagnostics :scope) :file)
                              "Expected file diagnostics scope, got ~S."
                              file-diagnostics)
                 (assert-true (>= (length diagnostics) 1)
                              "Expected at least one diagnostic for file, got ~S."
                              file-diagnostics)
                 (assert-true (eq (getf first-diagnostic :severity) :error)
                              "Expected first diagnostic severity :error, got ~S."
                              first-diagnostic)
                 (assert-true (= (getf first-diagnostic :line) 2)
                              "Expected diagnostic line=2, got ~S."
                              first-diagnostic))

               (sleep 0.1d0)
               (let* ((python-entries (read-json-lines python-log-path))
                      (rust-entries (read-json-lines rust-log-path))
                      (python-initialize-count (count-log-event python-entries "initialize"))
                      (python-did-open-count (count-log-event python-entries "didOpen"))
                      (rust-initialize-count (count-log-event rust-entries "initialize"))
                      (rust-did-open-count (count-log-event rust-entries "didOpen")))
                 (assert-true (>= python-initialize-count 2)
                              "Expected at least two python initialize events (startup + restart), got ~S."
                              python-initialize-count)
                 (assert-true (>= python-did-open-count 2)
                              "Expected python didOpen resync on restart, got ~S."
                              python-did-open-count)
                 (assert-true (>= rust-initialize-count 1)
                              "Expected rust initialize event, got ~S."
                              rust-initialize-count)
                 (assert-true (>= rust-did-open-count 1)
                              "Expected rust didOpen event, got ~S."
                              rust-did-open-count))

               (let* ((python-connection (funcall lsp-client-connection-fn client "python"))
                      (rust-connection (funcall lsp-client-connection-fn client "rust"))
                      (python-process (and python-connection
                                           (funcall lsp-server-connection-process-fn
                                                    python-connection)))
                      (rust-process (and rust-connection
                                         (funcall lsp-server-connection-process-fn
                                                  rust-connection))))
                 (let ((stopped-client client))
                   (funcall lsp-client-stop-fn stopped-client)
                   (assert-true (null (funcall lsp-client-connection-fn stopped-client "python"))
                                "Expected no python connection after client stop.")
                   (assert-true (null (funcall lsp-client-connection-fn stopped-client "rust"))
                                "Expected no rust connection after client stop."))
                 (assert-true (wait-until (lambda ()
                                            (not (process-running-p python-process)))
                                          2.0d0)
                              "Expected Python LSP process to exit on client stop.")
                 (assert-true (wait-until (lambda ()
                                            (not (process-running-p rust-process)))
                                          2.0d0)
                              "Expected Rust LSP process to exit on client stop.")
                 (setf client nil)))
          (when client
            (ignore-errors
              (funcall lsp-client-stop-fn client)))
          (ignore-errors
            (funcall reset-lsp-tool-runtime-fn))))
      (format t "AMOEBUM_LSP_SMOKE_OK~%"))))
