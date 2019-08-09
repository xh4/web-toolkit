(in-package :websocket)

(defclass session ()
  ((connection
    :initarg :connection
    :reader session-connection)))

(defgeneric close-session (session &optional reason)
  (:method ((session session) &optional reason)
    (with-slots (connection) session
      (close-connection connection reason))))

(defgeneric send-text (session text &key)
  (:method ((session session) text &key)
    (with-slots (connection) session
      (send-frame connection +text-frame+
                  (flex:string-to-octets text
                                         :external-format :utf-8)))))

(defgeneric send-binary (session data &key)
  (:method ((session session) data &key)
    (with-slots (connection) session
      (send-frame connection +binary-frame+ data))))

(defgeneric ping (session &optional data &key)
  (:method ((session session) &optional data &key)
    (with-slots (connection) session
      (send-frame connection +ping+ nil))))

(defgeneric send-pong (session data &key)
  (:method ((session session) data &key)
    (error "unimplemented")))


(defgeneric on-message (session message))


(defmacro define-session (name &rest arguments)
  `(defclass ,name (session)
     ,@arguments))
