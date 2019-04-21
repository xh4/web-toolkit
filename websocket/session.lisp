(in-package :websocket)

(defclass session ()
  ((ws
    :initarg :ws
    :reader session-ws
    :documentation "The WS object from websocket-driver.
https://github.com/fukamachi/websocket-driver")))

(defgeneric close-session (session &optional reason)
  (:documentation "Close the current conversation. If REASON is provided,
give a reason for the closure, otherwise close with a normal status code and no reason phrase.")
  (:method ((session session) &optional reason)
    (wsd:close-connection (session-ws session) reason)))

(defgeneric send-text (session text &key)
  (:method ((session session) text &key)
    (wsd:send-text (session-ws session) text)))

(defgeneric send-binary (session data &key)
  (:method ((session session) data &key)
    (wsd:send-binary (session-ws session) data)))

(defgeneric send-ping (session data &key)
  (:method ((session session) data &key)
    (wsd:send-ping (session-ws session) data)))

(defgeneric send-pong (session data &key)
  (:method ((session session) data &key)
    (error "unimplemented")))


(defgeneric on-message (session message))


(defmacro define-session (name &rest args)
  `(defclass ,name (session)
     ,@args))
