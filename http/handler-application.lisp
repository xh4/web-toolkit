(in-package :http)

(defclass application-handler (error-handler)
  ()
  (:metaclass handler-class)
  (:function (lambda () (call-next-handler))))
