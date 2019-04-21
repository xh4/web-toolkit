(in-package :websocket)

(define-session my-session ())

(defmethod on-message ((session my-session) message)
  (format t "Received: ~A~%" message)
  (send-text session message))

(define-endpoint my-endpoint
    :path "/foo"
    :session-class 'my-session)

(defmethod on-open ((endpoint my-endpoint) session)
  (format t "Open: ~A~%" session))

(defmethod on-close ((endpoint my-endpoint) &optional reason)
  (format t "Close: ~A~%" reason))

(defmethod on-error ((endpoint my-endpoint) error)
  (format t "Error: ~A~%" error))

(define-server my-server
    :port 4000
    :endpoints (list my-endpoint))
