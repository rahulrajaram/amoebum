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
         (temporary-directory-fn (funcall fn-in "TEMPORARY-DIRECTORY" uiop-pkg))
         (ensure-directory-pathname-fn (funcall fn-in "ENSURE-DIRECTORY-PATHNAME" uiop-pkg))
         (find-tool-fn (funcall fn-in "FIND-TOOL" pseudopod-pkg))
         (tool-definition-fn-fn (funcall fn-in "TOOL-DEFINITION-FN" pseudopod-pkg))
         (toolset-sym (funcall symbol-in "*TOOLSET*" amoebum-pkg))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (rate-limit-sym (funcall symbol-in "*WEB-SEARCH-LAST-REQUEST-AT*" amoebum-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (contains-substring-p (needle haystack)
               (and (stringp haystack)
                    (search needle haystack :test #'char-equal)))
             (write-text-file (path content)
               (ensure-directories-exist path)
               (with-open-file (stream path
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create
                                       :external-format :utf-8)
                 (write-string content stream)))
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
                   (error "Web search smoke test requires python3 or python.")))
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
             (read-json-lines (path)
               (when (probe-file path)
                 (with-open-file (stream path :direction :input :external-format :utf-8)
                   (let* ((jonathan-pkg (or (find-package :jonathan)
                                            (error "Missing package JONATHAN.")))
                          (parse-symbol (or (find-symbol "PARSE" jonathan-pkg)
                                            (error "Missing JONATHAN:PARSE.")))
                          (parse-fn (symbol-function parse-symbol)))
                     (loop for line = (read-line stream nil nil)
                           while line
                           collect (funcall parse-fn line :as :hash-table))))))
             (write-server-script (path)
               (write-text-file
                path
                (format nil
                        (concatenate
                         'string
                         "import http.server~%"
                         "import json~%"
                         "import time~%"
                         "import urllib.parse~%"
                         "import sys~%"
                         "~%"
                         "port_path = sys.argv[1]~%"
                         "log_path = sys.argv[2]~%"
                         "~%"
                         "class Handler(http.server.BaseHTTPRequestHandler):~%"
                         "    def log_message(self, format, *args):~%"
                         "        return~%"
                         "~%"
                         "    def _write(self, status, body, content_type):~%"
                         "        payload = body.encode('utf-8')~%"
                         "        self.send_response(status)~%"
                         "        self.send_header('Content-Type', content_type)~%"
                         "        self.send_header('Content-Length', str(len(payload)))~%"
                         "        self.end_headers()~%"
                         "        self.wfile.write(payload)~%"
                         "~%"
                         "    def do_GET(self):~%"
                         "        parsed = urllib.parse.urlparse(self.path)~%"
                         "        params = urllib.parse.parse_qs(parsed.query)~%"
                         "        query = params.get('q', [''])[0]~%"
                         "        with open(log_path, 'a', encoding='utf-8') as handle:~%"
                         "            handle.write(json.dumps({'ts': time.time(), 'path': parsed.path, 'query': query}) + '\\n')~%"
                         "~%"
                         "        if parsed.path == '/search':~%"
                         "            payload = json.dumps({~%"
                         "                'results': [~%"
                         "                    {'title': 'Allowed Result', 'url': 'https://allowed.example/page', 'content': 'Allowed snippet'},~%"
                         "                    {'title': 'Blocked Result', 'url': 'https://blocked.example/secret', 'content': 'Blocked snippet'}~%"
                         "                ]~%"
                         "            })~%"
                         "            self._write(200, payload, 'application/json')~%"
                         "            return~%"
                         "~%"
                         "        if parsed.path in ('/html', '/html/') :~%"
                         "            html = \"\"\"<html><body>~%"
                         "<a class='result__a' href='https://duckduckgo.com/l/?uddg=https%3A%2F%2Fddg.example%2Fguide'>DDG Result</a>~%"
                         "<div class='result__snippet'>DDG snippet for fallback backend.</div>~%"
                         "</body></html>\"\"\"~%"
                         "            self._write(200, html, 'text/html')~%"
                         "            return~%"
                         "~%"
                         "        self._write(503, 'backend unavailable', 'text/plain')~%"
                         "~%"
                         "server = http.server.ThreadingHTTPServer(('127.0.0.1', 0), Handler)~%"
                         "with open(port_path, 'w', encoding='utf-8') as port_file:~%"
                         "    port_file.write(str(server.server_address[1]))~%"
                         "server.serve_forever()~%")))))
      (let* ((python-command (resolve-python-command))
             (tmp-root
               (funcall ensure-directory-pathname-fn
                        (merge-pathnames
                         (make-pathname :directory `(:relative ,(format nil "amoebum-i57-~A"
                                                                        (get-universal-time))))
                         (funcall temporary-directory-fn))))
             (server-script (merge-pathnames #P"mock-web-server.py" tmp-root))
             (port-file (merge-pathnames #P"server.port" tmp-root))
             (log-file (merge-pathnames #P"requests.jsonl" tmp-root))
             (process nil))
        (write-server-script server-script)
        (setf process
              (uiop:launch-program (list python-command
                                         (namestring server-script)
                                         (namestring port-file)
                                         (namestring log-file))
                                   :output :stream
                                   :error-output :stream
                                   :ignore-error-status t))
        (unwind-protect
            (progn
              (assert-true (wait-until (lambda () (probe-file port-file)) 5.0d0)
                           "Timed out waiting for mock web server startup.")
              (let* ((port (parse-integer
                            (string-trim '(#\Space #\Tab #\Newline #\Return)
                                         (uiop:read-file-string port-file
                                                                :external-format :utf-8))))
                     (base-url (format nil "http://127.0.0.1:~D" port)))
                (funcall setconfig-fn :permission-mode :full-auto)
                (funcall setconfig-fn :web-search-searxng-url
                         (format nil "~A/search" base-url))
                (funcall setconfig-fn :web-search-duckduckgo-url
                         (format nil "~A/html/" base-url))
                (funcall setconfig-fn :web-search-block-domains "blocked.example")
                (funcall setconfig-fn :web-search-allow-domains nil)
                (setf (symbol-value rate-limit-sym) 0.0d0)

                (let* ((search-result (invoke-tool "web-search"
                                                   "query" "amoebum web search"
                                                   "backend" :searxng
                                                   "limit" 5))
                       (results (getf search-result :results))
                       (markdown (getf search-result :markdown)))
                  (assert-true (= (length results) 1)
                               "Expected domain block-list to filter blocked result.")
                  (assert-true (contains-substring-p "[Allowed Result](https://allowed.example/page)"
                                                     markdown)
                               "Expected markdown result hyperlink with allowed result.")
                  (assert-true (contains-substring-p "Source: allowed.example" markdown)
                               "Expected markdown output to include source attribution."))

                (invoke-tool "web-search"
                             "query" "rate limit probe"
                             "backend" :searxng
                             "limit" 2)

                (let* ((log-lines (read-json-lines log-file))
                       (searx-calls
                         (remove-if-not
                          (lambda (entry)
                            (string= (gethash "path" entry) "/search"))
                          log-lines)))
                  (assert-true (>= (length searx-calls) 2)
                               "Expected at least two /search requests for rate-limit check.")
                  (let* ((first-ts (gethash "ts" (first searx-calls)))
                         (second-ts (gethash "ts" (second searx-calls)))
                         (delta (- second-ts first-ts)))
                    (assert-true (>= delta 0.95d0)
                                 "Expected >=1 second spacing between requests, observed ~,3F sec."
                                 delta)))

                (funcall setconfig-fn :web-search-searxng-url
                         (format nil "~A/unavailable" base-url))
                (let* ((fallback-result (invoke-tool "web-search"
                                                     "query" "fallback check"
                                                     "backend" :auto
                                                     "limit" 3))
                       (backend (getf fallback-result :backend))
                       (fallback-errors (getf fallback-result :fallback-errors))
                       (markdown (getf fallback-result :markdown)))
                  (assert-true (eq backend :duckduckgo)
                               "Expected auto backend fallback to duckduckgo, got ~S."
                               backend)
                  (assert-true (and fallback-errors (plusp (length fallback-errors)))
                               "Expected fallback errors to capture failed SearXNG attempt.")
                  (assert-true (contains-substring-p "[DDG Result](https://ddg.example/guide)"
                                                     markdown)
                               "Expected fallback markdown to include DuckDuckGo result."))))
          (when process
            (ignore-errors (uiop:terminate-process process))
            (ignore-errors (uiop:wait-process process))))))
    (format t "AMOEBUM_WEB_SEARCH_SMOKE_OK~%")))
