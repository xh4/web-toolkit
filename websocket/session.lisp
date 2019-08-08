(in-package :websocket)

(defclass session ()
  ((connection
    :initarg :connection
    :reader session-connection)))

(defgeneric close-session (session &optional reason)
  (:method ((session session) &optional reason)
    ))

(defgeneric send-text (session text &key)
  (:method ((session session) text &key)
    ))

(defgeneric send-binary (session data &key)
  (:method ((session session) data &key)
    ))

(defgeneric ping (session &optional data &key)
  (:method ((session session) &optional data &key)
    ))

(defgeneric send-pong (session data &key)
  (:method ((session session) data &key)
    (error "unimplemented")))


(defgeneric on-message (session message))


(defmacro define-session (name &rest arguments)
  `(defclass ,name (session)
     ,@arguments))
