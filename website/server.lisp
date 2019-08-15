(in-package :website)

(defparameter *navbar*
  (navbar :class "navbar-expand-lg navbar-light bg-light"
          (navbar-brand "Lisp Web Toolkit")))

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
            (meta :charset "utf-8")
            (title "Lisp Web Toolkit")
            (link :rel "stylesheet"
                  :href "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"))
           (body
            (render *navbar*)))))))

(define-server website-server
    :handler website-handler
    :listeners (list
                (listener :port 8080)))

(defun start-server ()
  (http:start-server website-server))

(defun stop-server ()
    (http:stop-server website-server))
