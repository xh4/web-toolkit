(in-package :websocket)

(define-session test-echo-session ())

(defmethod on-message ((session test-echo-session) message)
  (format t "Received: ~A~%" message)
  (typecase message
    (string (send-text session message))
    (pathname (send-binary session (read-file-into-byte-vector message)))))

(define-endpoint test-echo-endpoint
    :session-class 'test-echo-session)

(defmethod on-open ((endpoint test-echo-endpoint) session)
  (format t "Open: ~A~%" session)
  )

(defmethod on-close ((endpoint test-echo-endpoint) session &optional reason)
  (format t "Close: ~A~%" reason))

(defmethod on-error ((endpoint test-echo-endpoint) session error)
  (format t "Error: ~A~%" error))

(http:define-server test-echo-server
    :handler (http:router
              (:get "/" test-echo-endpoint))
    :listeners (list
                (http:listener :port 8003)))
