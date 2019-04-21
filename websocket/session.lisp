(in-package :websocket)


(defclass session ()
  ())

(defgeneric close-session (session &optional reason)
  (:documentation "Close the current conversation. If REASON is provided,
give a reason for the closure, otherwise close with a normal status code and no reason phrase.")
  (:method ((session session) &optional reason)
    ))

(defgeneric on-message (session message))


(defmacro define-session (name &rest args)
  `(defclass ,name (session)
     ,@args))
