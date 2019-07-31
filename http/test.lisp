(in-package :http)

(define-handler my-handler () ())

(defmethod handle ((handler my-handler) (request request))
  (setf (response-status *response*) 200)
  (appendf (response-header *response*)
           (list (make-instance 'header-field
                                :name "Content-Type"
                                :value "text/plain")))
  (setf (response-body *response*) "my handler "))

(define-handler your-handler (my-handler) ())

(defmethod handle ((handler your-handler) (request request))
  (setf (response-body *response*)
        (format nil "~A ~A"
                (response-body *response*)
                " your handler2")))

(define-server test-server
    :handler (router
              (:get "/you" your-handler)
              (:get "/me" my-handler))
    :listeners (list
                (listener :port 8001)))
