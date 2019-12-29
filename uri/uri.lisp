(in-package :uri)

(defclass uri ()
  (;;
   (scheme
    :initarg :scheme
    :initform nil
    :accessor uri-scheme)
   (userinfo
    :initarg :userinfo
    :initform nil
    :accessor uri-userinfo)
   (host
    :initarg :host
    :initform nil
    :accessor uri-host)
   (port
    :initarg :port
    :initform nil
    :accessor uri-port)
   (path
    :initarg :path
    :initform nil
    :accessor uri-path)
   (query
    :initarg :query
    :initform nil
    :accessor uri-query)
   (fragment
    :initarg :fragment
    :initform nil
    :accessor uri-fragment)

   ;; the cached printable representation of the URI
   ;; It might be different than the original string, because of percent
   ;; encoding.  Use of slot setf methods may reset this slot to nil,
   ;; causing it to be recomputed when needed.
   (string
    :initarg :string
    :initform nil
    :accessor uri-string)))

(defmethod initialize-instance :after ((uri uri) &key)
  )

(defun make-uri (&key scheme userinfo host port path fragment)
  (let ((scheme (check-uri-scheme scheme))
        (userinfo (check-uri-userinfo userinfo))
        (host (check-uri-host host))
        (port (check-uri-port port))
        (path (check-uri-path path))
        (fragment (check-uri-fragment fragment)))
    (make-instance 'uri
                   :scheme scheme
                   :userinfo userinfo
                   :host host
                   :port port
                   :path path
                   :fragment fragment)))

(defun uri-authority (uri)
  (with-slots (host userinfo port) uri
    (when (uri-host uri)
      (concatenate 'string
                   userinfo
                   (when userinfo "@")
                   host
                   (when port ":")
                   (format nil "~A" port)))))
