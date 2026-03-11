(defpackage :ptui.runtime.session-multiplex
  (:use :cl)
  (:import-from :bordeaux-threads
                #:make-lock
                #:with-lock-held)
  (:export
   #:session-view-state
   #:make-session-view-state
   #:session-view-state-cursor-row
   #:session-view-state-cursor-col
   #:session-view-state-scroll-offset
   #:session-view-state-conversation-history
   #:multiplex-session
   #:make-multiplex-session
   #:multiplex-session-user-id
   #:multiplex-session-backend
   #:multiplex-session-view-state
   #:multiplex-session-server-socket
   #:multiplex-session-client-socket
   #:multiplex-session-connected-via-unix-socket-p
   #:session-multiplexer
   #:make-session-multiplexer
   #:session-multiplexer-tool-registry
   #:session-multiplexer-shared-state
   #:session-multiplexer-session-count
   #:session-multiplexer-list-user-ids
   #:session-multiplexer-socket-running-p
   #:start-session-multiplexer-socket
   #:stop-session-multiplexer-socket
   #:connect-user-session
   #:disconnect-user-session
   #:find-user-session
   #:update-session-cursor
   #:update-session-scroll
   #:append-session-history
   #:register-shared-tool
   #:shared-tool-registered-p
   #:put-shared-state
   #:get-shared-state))

(in-package :ptui.runtime.session-multiplex)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl
  (ignore-errors
    (require :sb-bsd-sockets)))

(defstruct (session-view-state
            (:constructor make-session-view-state
                (&key (cursor-row 0)
                      (cursor-col 0)
                      (scroll-offset 0)
                      (conversation-history '()))))
  (cursor-row 0 :type integer)
  (cursor-col 0 :type integer)
  (scroll-offset 0 :type integer)
  (conversation-history '() :type list))

(defstruct (multiplex-session
            (:constructor make-multiplex-session
                (&key user-id backend view-state
                      server-socket client-socket
                      (connected-via-unix-socket-p nil))))
  (user-id "" :type string)
  (backend nil)
  (view-state (make-session-view-state) :type session-view-state)
  (server-socket nil)
  (client-socket nil)
  (connected-via-unix-socket-p nil :type boolean))

(defstruct (session-multiplexer
            (:constructor %make-session-multiplexer
                (&key lock sessions tool-registry shared-state
                      backend-factory socket-path listener-socket socket-running-p)))
  (lock (make-lock "ptui-session-multiplexer-lock"))
  (sessions (make-hash-table :test #'equal))
  (tool-registry (make-hash-table :test #'equal))
  (shared-state (make-hash-table :test #'equal))
  (backend-factory (lambda (user-id)
                     (declare (ignore user-id))
                     nil)
                   :type function)
  (socket-path nil)
  (listener-socket nil)
  (socket-running-p nil :type boolean))

(defun make-session-multiplexer (&key backend-factory)
  (%make-session-multiplexer
   :backend-factory (or backend-factory
                        (lambda (user-id)
                          (declare (ignore user-id))
                          nil))))

(defun %unix-socket-supported-p ()
  #+sbcl
  (not (null (find-package :sb-bsd-sockets)))
  #-sbcl
  nil)

(defun %make-local-socket ()
  #+sbcl
  (make-instance 'sb-bsd-sockets:local-socket :type :stream :protocol 0)
  #-sbcl
  (error "Unix sockets are not supported on this Lisp implementation."))

(defun %socket-bind (socket path)
  #+sbcl
  (sb-bsd-sockets:socket-bind socket path)
  #-sbcl
  (declare (ignore socket path))
  #-sbcl
  (error "Unix sockets are not supported on this Lisp implementation."))

(defun %socket-listen (socket backlog)
  #+sbcl
  (sb-bsd-sockets:socket-listen socket backlog)
  #-sbcl
  (declare (ignore socket backlog))
  #-sbcl
  (error "Unix sockets are not supported on this Lisp implementation."))

(defun %socket-connect (socket path)
  #+sbcl
  (sb-bsd-sockets:socket-connect socket path)
  #-sbcl
  (declare (ignore socket path))
  #-sbcl
  (error "Unix sockets are not supported on this Lisp implementation."))

(defun %socket-accept (socket)
  #+sbcl
  (sb-bsd-sockets:socket-accept socket)
  #-sbcl
  (declare (ignore socket))
  #-sbcl
  (error "Unix sockets are not supported on this Lisp implementation."))

(defun %safe-socket-close (socket)
  (when socket
    (ignore-errors
      #+sbcl
      (sb-bsd-sockets:socket-close socket)
      #-sbcl
      nil)))

(defun %safe-delete-socket-path (path)
  (when (and path (probe-file path))
    (ignore-errors
      (delete-file path))))

(defun session-multiplexer-session-count (multiplexer)
  (check-type multiplexer session-multiplexer)
  (with-lock-held ((session-multiplexer-lock multiplexer))
    (hash-table-count (session-multiplexer-sessions multiplexer))))

(defun session-multiplexer-list-user-ids (multiplexer)
  (check-type multiplexer session-multiplexer)
  (with-lock-held ((session-multiplexer-lock multiplexer))
    (let ((ids '()))
      (maphash (lambda (user-id session)
                 (declare (ignore session))
                 (push user-id ids))
               (session-multiplexer-sessions multiplexer))
      (sort ids #'string<))))

(defun %open-session-socket-pair (multiplexer)
  (let* ((listener (session-multiplexer-listener-socket multiplexer))
         (socket-path (session-multiplexer-socket-path multiplexer))
         (client (%make-local-socket)))
    (handler-case
        (progn
          (%socket-connect client socket-path)
          (multiple-value-bind (server _peername)
              (%socket-accept listener)
            (declare (ignore _peername))
            (values server client t)))
      (error (condition)
        (%safe-socket-close client)
        (error condition)))))

(defun start-session-multiplexer-socket (multiplexer socket-path &key (backlog 16))
  "Start a Unix-domain listener for multi-user session connections."
  (check-type multiplexer session-multiplexer)
  (check-type socket-path string)
  (unless (%unix-socket-supported-p)
    (error "Unix sockets are not supported on this Lisp implementation."))
  (when (session-multiplexer-socket-running-p multiplexer)
    (stop-session-multiplexer-socket multiplexer))
  (ensure-directories-exist socket-path)
  (%safe-delete-socket-path socket-path)
  (let ((listener (%make-local-socket)))
    (handler-case
        (progn
          (%socket-bind listener socket-path)
          (%socket-listen listener backlog)
          (with-lock-held ((session-multiplexer-lock multiplexer))
            (setf (session-multiplexer-socket-path multiplexer) socket-path
                  (session-multiplexer-listener-socket multiplexer) listener
                  (session-multiplexer-socket-running-p multiplexer) t))
          socket-path)
      (error (condition)
        (%safe-socket-close listener)
        (%safe-delete-socket-path socket-path)
        (error condition)))))

(defun %resolve-backend (multiplexer user-id backend)
  (or backend
      (funcall (session-multiplexer-backend-factory multiplexer) user-id)))

(defun connect-user-session (multiplexer user-id &key backend)
  "Register USER-ID with an isolated backend and per-user TUI view state."
  (check-type multiplexer session-multiplexer)
  (check-type user-id string)
  (with-lock-held ((session-multiplexer-lock multiplexer))
    (when (gethash user-id (session-multiplexer-sessions multiplexer))
      (error "Session already exists for user ~S." user-id)))
  (let ((backend-instance (%resolve-backend multiplexer user-id backend))
        (server-socket nil)
        (client-socket nil)
        (connected-via-unix-socket-p nil))
    (when (session-multiplexer-socket-running-p multiplexer)
      (multiple-value-setq (server-socket client-socket connected-via-unix-socket-p)
        (%open-session-socket-pair multiplexer)))
    (with-lock-held ((session-multiplexer-lock multiplexer))
      (when (gethash user-id (session-multiplexer-sessions multiplexer))
        (%safe-socket-close server-socket)
        (%safe-socket-close client-socket)
        (error "Session already exists for user ~S." user-id))
      (let ((session (make-multiplex-session
                      :user-id user-id
                      :backend backend-instance
                      :view-state (make-session-view-state)
                      :server-socket server-socket
                      :client-socket client-socket
                      :connected-via-unix-socket-p connected-via-unix-socket-p)))
        (setf (gethash user-id (session-multiplexer-sessions multiplexer)) session)
        session))))

(defun find-user-session (multiplexer user-id)
  (check-type multiplexer session-multiplexer)
  (check-type user-id string)
  (with-lock-held ((session-multiplexer-lock multiplexer))
    (gethash user-id (session-multiplexer-sessions multiplexer))))

(defun disconnect-user-session (multiplexer user-id)
  (check-type multiplexer session-multiplexer)
  (check-type user-id string)
  (let ((session nil))
    (with-lock-held ((session-multiplexer-lock multiplexer))
      (setf session (gethash user-id (session-multiplexer-sessions multiplexer)))
      (when session
        (remhash user-id (session-multiplexer-sessions multiplexer))))
    (when session
      (%safe-socket-close (multiplex-session-server-socket session))
      (%safe-socket-close (multiplex-session-client-socket session))
      t)))

(defun stop-session-multiplexer-socket (multiplexer)
  (check-type multiplexer session-multiplexer)
  (let ((listener nil)
        (socket-path nil)
        (sessions-to-close '()))
    (with-lock-held ((session-multiplexer-lock multiplexer))
      (setf listener (session-multiplexer-listener-socket multiplexer)
            socket-path (session-multiplexer-socket-path multiplexer))
      (maphash (lambda (_user-id session)
                 (declare (ignore _user-id))
                 (push session sessions-to-close))
               (session-multiplexer-sessions multiplexer))
      (clrhash (session-multiplexer-sessions multiplexer))
      (setf (session-multiplexer-listener-socket multiplexer) nil
            (session-multiplexer-socket-path multiplexer) nil
            (session-multiplexer-socket-running-p multiplexer) nil))
    (%safe-socket-close listener)
    (dolist (session sessions-to-close)
      (%safe-socket-close (multiplex-session-server-socket session))
      (%safe-socket-close (multiplex-session-client-socket session)))
    (%safe-delete-socket-path socket-path)
    t))

(defun %ensure-session (multiplexer user-id)
  (or (find-user-session multiplexer user-id)
      (error "No session exists for user ~S." user-id)))

(defun update-session-cursor (multiplexer user-id row col)
  (check-type row integer)
  (check-type col integer)
  (with-lock-held ((session-multiplexer-lock multiplexer))
    (let* ((session (or (gethash user-id (session-multiplexer-sessions multiplexer))
                        (error "No session exists for user ~S." user-id)))
           (view-state (multiplex-session-view-state session)))
      (setf (session-view-state-cursor-row view-state) row
            (session-view-state-cursor-col view-state) col)
      view-state)))

(defun update-session-scroll (multiplexer user-id scroll-offset)
  (check-type scroll-offset integer)
  (with-lock-held ((session-multiplexer-lock multiplexer))
    (let* ((session (or (gethash user-id (session-multiplexer-sessions multiplexer))
                        (error "No session exists for user ~S." user-id)))
           (view-state (multiplex-session-view-state session)))
      (setf (session-view-state-scroll-offset view-state) scroll-offset)
      view-state)))

(defun append-session-history (multiplexer user-id entry)
  (with-lock-held ((session-multiplexer-lock multiplexer))
    (let* ((session (or (gethash user-id (session-multiplexer-sessions multiplexer))
                        (error "No session exists for user ~S." user-id)))
           (view-state (multiplex-session-view-state session)))
      (setf (session-view-state-conversation-history view-state)
            (append (session-view-state-conversation-history view-state)
                    (list entry)))
      view-state)))

(defun register-shared-tool (multiplexer tool-id descriptor)
  (check-type multiplexer session-multiplexer)
  (with-lock-held ((session-multiplexer-lock multiplexer))
    (setf (gethash tool-id (session-multiplexer-tool-registry multiplexer)) descriptor))
  descriptor)

(defun shared-tool-registered-p (multiplexer tool-id)
  (check-type multiplexer session-multiplexer)
  (with-lock-held ((session-multiplexer-lock multiplexer))
    (nth-value 1
               (gethash tool-id (session-multiplexer-tool-registry multiplexer)))))

(defun put-shared-state (multiplexer key value)
  (check-type multiplexer session-multiplexer)
  (with-lock-held ((session-multiplexer-lock multiplexer))
    (setf (gethash key (session-multiplexer-shared-state multiplexer)) value)))

(defun get-shared-state (multiplexer key &optional default)
  (check-type multiplexer session-multiplexer)
  (with-lock-held ((session-multiplexer-lock multiplexer))
    (gethash key (session-multiplexer-shared-state multiplexer) default)))
