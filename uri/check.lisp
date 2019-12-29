(in-package :uri)

(defun check-uri-scheme (scheme)
  (typecase scheme
    (symbol (string-downcase (symbol-name scheme)))
    (string scheme)
    (null nil)
    (t (error "URI scheme must be string or symbol or null"))))

(defun check-uri-userinfo (userinfo)
  userinfo)

(defun check-uri-host (host)
  host)

(defun check-uri-port (port)
  (typecase port
    (integer port)
    (string (handler-case
                (parse-integer port)
              (error (e)
                (declare (ignore e))
                (error "Unable to parse port to integer"))))
    (t (error "Port must be integer or string"))))

(defun check-uri-path (path)
  (when path
    (if (stringp path)
        path
        (error "Path must be string or null"))))

(defun check-uri-fragment (fragment)
  (when fragment
    (if (stringp fragment)
        fragment
        (error "fragment must be string or null"))))
