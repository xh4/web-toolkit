(in-package :http)

(define-handler default-handler ()
  ())

(defmethod handle ((handler default-handler) request)
  (reply
   (status 200)
   (header "Content-Type" "text/plain")
   "Lisp Web Toolkit"))
