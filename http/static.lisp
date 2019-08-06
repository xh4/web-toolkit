(in-package :http)

(define-handler static-handler ()
  ((prefix
    :initarg :prefix
    :initform nil)
   (location
    :initarg :location
    :initform nil)))

(defmethod handle ((handler static-handler) (request request))
  (let* ((prefix (slot-value handler 'prefix))
         (location (slot-value handler 'location))
         (relative-path (path-trim-prefix prefix (request-uri request)))
         (pathname (merge-pathnames relative-path location)))
    (setf (response-body *response*) pathname)))
