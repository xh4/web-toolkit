(in-package :http)

(define-handler error-handler ()
  ()
  (:function
   (lambda (handler request)
     (handler-bind ((error (lambda (error)
                             (use-value (handle-error handler request error)))))
       (restart-case (call-next-handler)
         (use-value (value) value))))))

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
