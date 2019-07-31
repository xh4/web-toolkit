(in-package :http)

(defclass request ()
  ((method
    :initarg :method
    :initform nil
    :accessor request-method)
   (uri
    :initarg :uri
    :initform nil
    :accessor request-uri)
   (header
    :initarg :header
    :initform nil
    :accessor request-header)
   (body
    :initarg :body
    :initform nil
    :accessor request-body)))

(defgeneric request-method (request))

(defgeneric (setf request-method) (value request))

;; scheme + host + port + path + query
(defgeneric request-uri (request))

(defgeneric (setf request-uri) (value request))

(defgeneric request-version (request))

(defgeneric (setf request-version) (value request))

(defgeneric request-header (request))

(defgeneric (setf request-header) (header request))

(defgeneric request-body (request))

(defgeneric (setf request-body) (body request))


(defparameter *methods* '(:get :post :put :delete :head))
