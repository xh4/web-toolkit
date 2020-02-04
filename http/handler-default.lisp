(in-package :http)

(define-handler default-handler ()
  ())

(defmethod handle ((handler default-handler) request)
  (reply "Lisp Web Toolkit"))
