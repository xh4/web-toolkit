(in-package :websocket)

(defclass endpoint ()
  ((path
    :initarg :path
    :initform nil
    :accessor endpoint-path)
   (session-class
    :initarg :session-class
    :initform 'session
    :accessor endpoint-session-class)))


(defgeneric on-open (endpoint session))

(defgeneric on-close (endpoint &optional reason))

(defgeneric on-error (endpoint error))


(defmacro define-endpoint (name &key path session-class)
  `(progn
     (defclass ,name (endpoint) ())
     (unless (boundp ',name)
       (setf ,name (make-instance ',name
                                  :path ,path
                                  :session-class ,session-class)))))

(define-session my-session () ())

(define-endpoint my-endpoint
    :path "/"
    :session-class 'my-session)


(defclass remote-endpoint () ())

(defgeneric send-text (remote-endpoint text))

(defgeneric send-binary (remote-endpoint data))

(defgeneric send-ping (remote-endpoint data))

(defgeneric send-pong (remote-endpoint data))
