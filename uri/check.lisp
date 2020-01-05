(in-package :uri)

(defun check-scheme (scheme)
  (typecase scheme
    (null nil)
    (symbol (check-scheme (string-downcase (symbol-name scheme))))
    (string (if (parser-match-all-p (.scheme) scheme)
                (string-downcase scheme)
                (error "Invalid URI scheme")))
    (t (error "URI scheme must be string or symbol or null"))))

(defun check-userinfo (userinfo)
  (typecase userinfo
    (null nil)
    (string userinfo)
    (t (error "URI userinfo must be string or null"))))

(defun check-host (host)
  (typecase host
    (null nil)
    (string (string-downcase host))
    (t (error "URI host must be string or null"))))

(defun check-port (port)
  (typecase port
    (null nil)
    (integer port)
    (string (handler-case
                (parse-integer port)
              (error (e)
                (declare (ignore e))
                (error "Unable to parse port to integer"))))
    (t (error "URI port must be integer or string or null"))))

(defun check-path (path)
  (typecase path
    (null nil)
    (string path)
    (t (error "URI path must be string or null"))))

(defun check-query (query)
  (typecase query
    (null nil)
    (string query)
    (list query)
    (t (error "URI query must be string or list or null"))))

(defun check-fragment (fragment)
  (typecase fragment
    (null nil)
    (string fragment)
    (t (error "URI fragment must be string or null"))))
