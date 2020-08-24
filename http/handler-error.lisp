(in-package :http)

(defclass error-handler (handler)
  ()
  (:metaclass handler-class)
  (:function
   (lambda (handler request)
     (block nil
       (handler-bind
           ((error (lambda (error)
                     (restart-case
                         (handle-error *handler* request error)
                       (reply-error ()
                         (return (reply-error *handler* request error))))
                     (return (reply-error *handler* request error)))))
         (call-next-handler))))))

(defgeneric handle-error (handler request error)
  (:method ((handler error-handler) (request request) error)
    ;; (invoke-debugger error)
    ))

(defgeneric reply-error (handler request error)
  (:method ((handler error-handler) (request request) error)
    (reply
     (status 500)
     "Internal Server Error")))
