(in-package :http)

;; https://developer.mozilla.org/en-US/docs/Web/HTTP/Redirections

;; https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Location

(define-condition redirect ()
  ((location
    :initarg :location
    :initform ""
    :accessor redirect-location)
   (status
    :initarg :status
    :initform :temporary-redirect
    :accessor redirect-status)))

(defmacro redirect (location &key (status :temporary-redirect))
  `(signal 'redirect
           :location ,location
           :status (or ,status :temporary-redirect)))
