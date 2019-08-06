(in-package :website)

(define-handler website-handler () ())

(defmethod handle ((handler website-handler) (request request))
  (declare (ignore handler request))
  (setf (response-status *response*) 200)
  (setf (header-field *response* "Content-Type") "text/html")
  (setf (response-body *response*)
        (html:serialize
         (document
          (html
           (head
            (title "Lisp Web Toolkit"))
           (body
            (h1 "Lisp Web Toolkit")))))))

(define-server website-server
    :handler website-handler
    :listeners (list
                (listener :port 8080)))

(defun start-server ()
  (http:start-server website-server))

(defun stop-server ()
    (http:stop-server website-server))
