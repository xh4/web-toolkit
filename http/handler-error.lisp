(in-package :http)

(defclass error-handler (handler)
  ()
  (:metaclass handler-class)
  (:function
   (lambda (handler request)
     (restart-case
         (handler-bind ((error (lambda (error)
                                 (invoke-restart 'reply-error handler request error))))
           (call-next-handler))
       (reply-error (error-handler request error)
         (handle-error error-handler request error))))))

(defgeneric handle-error (handler request error)
  (:method ((handler error-handler) (request request) error)
    (reply (status 500))
    (let ((accept (header-field-value
                   (find-header-field "Accept" request))))
      (when (or (search "text/html" accept)
                (equal "*/*" accept))
        (reply
         (html:document
          (html:html
           (html:head)
           (html:body
            (html:h1 "Internal Server Error")))))))))
