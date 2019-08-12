(in-package :http)

(defun request (url &key (method :get))
  (multiple-value-bind (stream status-code headers)
      (drakma:http-request url :method method
                           :want-stream t)
    (let ((header (make-instance 'header))
          (status (gethash status-code *status-code-mapping-table*)))
      (loop for (name . value) in headers
         do
           (setf (header-field header name) value))
      (let ((response (make-instance 'response
                                     :status status
                                     :header header
                                     :body stream)))
        response))))

(defun get (url)
  (request url :method :get))

(defun post (url)
  (request url :method :post))

(defun put (url)
  (request url :method :put))

(defun delete (url)
  (request url :method :delete))

(defun head (url)
  (request url :method :head))
