(in-package :uri)

(defclass uri ()
  ((scheme
    :initarg :scheme
    :initform nil)
   (userinfo
    :initarg :userinfo
    :initform nil)
   (host
    :initarg :host
    :initform nil)
   (port
    :initarg :port
    :initform nil)
   (path
    :initarg :path
    :initform nil)
   (query
    :initarg :query
    :initform nil)
   (fragment
    :initarg :fragment
    :initform nil)
   ;; The cached printable representation of the URI
   (string
    :initarg :string
    :initform nil)))

(defun make-uri (&rest arguments &key scheme userinfo host port path query fragment)
  (handler-bind ((error (lambda (e)
                          (error 'construct-uri-error
                                 :arguments arguments
                                 :message (format nil "~A" e)))))
    (let ((scheme (check-scheme scheme))
          (userinfo (check-userinfo userinfo))
          (host (check-host host))
          (port (check-port port))
          (path (check-path path))
          (query (check-query query))
          (fragment (check-fragment fragment)))
      (make-instance 'uri
                     :scheme scheme
                     :userinfo userinfo
                     :host host
                     :port port
                     :path path
                     :query query
                     :fragment fragment))))
