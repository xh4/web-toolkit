(in-package :websocket)

(defclass server ()
  ((port
    :initarg :port
    :initform nil
    :accessor server-port)
   (endpoints
    :initarg :endpoints
    :initform nil
    :accessor server-endpoints)
   (clack-handler
    :initform nil
    :accessor server-clack-handler)))

(defmacro define-server (name &key port endpoints)
  `(progn
     (defclass ,name (server) ())
     (if (boundp ',name)
         (setf (server-port ,name) ,port
               (server-endpoints ,name) ,endpoints)
       (defvar ,name (make-instance ',name
                                    :port ,port
                                    :endpoints ,endpoints)))))

(defun clack-entry (env server)
  (let ((ws (wsd:make-server env))
        (endpoint nil)
        (session nil))
    (wsd:on :open ws
            (lambda ()
              (let* ((endpoints (server-endpoints server))
                     (uri (quri:uri (getf env :request-uri)))
                     (path (quri:uri-path uri))
                     (query (quri:uri-query uri)))
                (loop for ep in endpoints
                   when (equal (endpoint-path ep) path)
                   do (setf endpoint ep))
                (when endpoint
                  (let ((session-class (or (endpoint-session-class endpoint)
                                            'session)))
                    (let ((ss (make-instance session-class :ws ws)))
                      (setf session ss)
                      ;; Allow user to config the session object using
                      ;; the ON-OPEN method.
                      (let ((result (on-open endpoint session)))
                        (when (typep result 'session)
                          (setf session result)))))))))
    (wsd:on :close ws
            (lambda (&key code reason)
              (when endpoint
                (on-close endpoint reason))))
    (wsd:on :error ws
            (lambda (error)
              (when endpoint
                (on-error endpoint error))))
    (wsd:on :message ws
            (lambda (message)
              (when session
                (on-message session message))))
    (lambda (responder)
      (declare (ignore responder))
      (wsd:start-connection ws))))

(defgeneric start-server (server &key)
  (:method ((server server) &key)
    (with-slots (clack-handler) server
      (if clack-handler
          clack-handler
          (setf clack-handler
                (clack:clackup
                 (lambda (env)
                   (funcall 'clack-entry env server))
                 :server :hunchentoot
                 :port (server-port server)))))))

(defgeneric stop-server (server &key)
  (:method ((server server) &key)
    (when-let (clack-handler (server-clack-handler server))
      (clack:stop clack-handler)
      (setf (server-clack-handler server) nil)
      t)))
