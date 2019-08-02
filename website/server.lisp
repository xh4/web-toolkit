(in-package :website)

(define-handler website-handler () ())

(defmethod handle ((handler website-handler) (request request))
  (declare (ignore handler request))
  (setf (response-status *response*) 200)
  (setf (header-field *response* "Content-Type") "text/html")
  (setf (response-body *response*)
        (html:serialize
         (html:document
          (html:html
           (html:head
            (html:title (html:text "Lisp Web Toolkit")))
           (html:body
            (html:h1 (html:text "Lisp Web Toolkit"))))))))

(define-server website-server
    :handler website-handler
    :listeners (list
                (listener :port 8080)))

(defun start-server ()
  (http:start-server website-server))

(defun stop-server ()
    (http:stop-server website-server))
