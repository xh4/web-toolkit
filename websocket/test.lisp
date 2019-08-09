(in-package :websocket)

(define-session test-session ())

(defmethod on-message ((session test-session) message)
  (format t "Received: ~A~%" message)
  (send-text session message))

(define-endpoint test-endpoint
    :session-class 'test-session)

(defmethod on-open ((endpoint test-endpoint) session)
  (format t "Open: ~A~%" session)
  (send-text session "Hello from server"))

(defmethod on-close ((endpoint test-endpoint) session &optional reason)
  (format t "Close: ~A~%" reason))

(defmethod on-error ((endpoint test-endpoint) session error)
  (format t "Error: ~A~%" error))

(http:define-handler test-handler () ())

(defmethod handle ((handler test-handler) (request request))
  (setf (response-status *response*) 200)
  (setf (header-field *response* "Content-Type") "text/html")
  (setf (response-body *response*)
        (html:serialize
         (html:document
          (html:html
           (html:head
            (html:title "WebSocket"))
           (html:body
            (html:h1 "WebSocket")
            (html:script :src "/static/test-websocket.js")))))))

(http:define-server test-server
    :handler (http:router
              (:get "/" test-handler)
              (:get "/websocket" test-endpoint)
              (:static :prefix "static" :location "/home/xh/"))
    :listeners (list
                (http:listener :port 8002)))

;; http://118.190.145.4:8002
