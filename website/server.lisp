(in-package :website)

(define-handler website-handler () ())

(defmethod handle ((handler website-handler) (request request))
  (declare (ignore handler request))
  (setf (response-status *response*) 200)
  (setf (header-field *response* "Content-Type") "text/html")
  (setf (response-body *response*) "Lisp Web Toolkit"))

(define-server website-server
    :handler website-handler
    :listeners (list
                (listener :port 8004)))

(defun start-server ()
  (http:start-server website-server))

(defun stop-server ()
  (http:stop-server website-server))
