(in-package :wt.http)


(defgeneric request-method (request))

(defgeneric (setf request-method) (value request))

;; scheme + host + port + path + query
(defgeneric request-url (request))

(defgeneric (setf request-url) (value request))

(defgeneric request-version (request))

(defgeneric request-header (request))

(defgeneric (setf request-header) (header request))

(defgeneric request-body (request))

(defgeneric (setf request-body) (body request))


(defparameter *http-methods* '(:get :post :put :delete :head))
