(in-package :website)

(define-handler website-handler () ())

(defmethod handle ((handler website-handler) (request request))
  (declare (ignore handler request))
  (redirect "https://github.com/xh4/web-toolkit" :status :see-other))

(define-server website-server
    :handler website-handler
    :listeners (list
                (listener :port 8080)))

(defun start-server ()
  (http:start-server website-server))

(defun stop-server ()
    (http:stop-server website-server))
