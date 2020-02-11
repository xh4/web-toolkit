(in-package :cl-user)
(defpackage find-port
  (:use :cl)
  (:export #:port-open-p
           #:find-port
           #:*default-interface*))
(in-package :find-port)

(defvar *default-interface* "127.0.0.1")

(defun port-open-p (port &key (interface *default-interface*))
  "Determine if the port is open."
  (handler-case
      (let ((socket (usocket:socket-listen interface port
                                           :reuse-address t)))
        (usocket:socket-close socket))
    (usocket:address-in-use-error (condition)
      (declare (ignore condition))
      nil)))

(defun find-port (&key (min 40000) (max 50000) (interface *default-interface*))
  "Return the first available port in a range of port numbers."
  (loop for port from min to max until (port-open-p port :interface interface)
        finally (return port)))
