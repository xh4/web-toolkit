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

(defgeneric request-uri (request))

(defgeneric (setf request-uri) (value request))

(defgeneric request-version (request))

(defgeneric (setf request-version) (value request))

(defgeneric request-header (request))

(defgeneric (setf request-header) (header request))

(defgeneric request-body (request))

(defgeneric (setf request-body) (body request))

(defmethod header-fields ((request request))
  (let ((header (request-header request)))
    (header-fields header)))

(defmethod (setf header-fields) (value (request request))
  (let ((header (request-header request)))
    (setf (header-fields header) value)))

(defmethod find-header-field ((request request) name)
  (find-header-field (request-header request) name))

(defparameter *methods* '(:get :post :put :delete :head))

(defun read-request-line (stream &key (parse t))
  (let ((line (handler-case
                  (read-line stream)
                ((or end-of-file #-:lispworks usocket:timeout-error) nil))))
    (unless (emptyp line)
      (if parse
          (parse-request-line line)
          line))))

(defun parse-request-line (line)
  (cl-ppcre:split "\\s+" line :limit 3))

(defun write-request-line (stream method request-uri http-version)
  (check-type method (or string symbol))
  (check-type request-uri string)
  (check-type http-version string)
  (when (symbolp method)
    (setf method (symbol-name method)))
  (let ((line (format nil "~A ~A ~A" method request-uri http-version)))
    (write-sequence (babel:string-to-octets line) stream)
    (write-sequence +crlf+ stream)
    (+ (length line) (length +crlf+))))

(defun read-request-header (stream)
  (read-header stream))

(defun write-request-header (stream header)
  (write-header stream header))

(defun write-request-body (stream body)
  (write-sequence body stream))

(defun read-request-body (stream request)
  (let ((request-header (request-header request)))
    (let ((content-length (header-field-value
                           (find-header-field
                            request-header
                            "Content-Length"))))
      (if content-length
          (setf content-length (parse-integer content-length))
          (setf content-length 0))
      (when (plusp content-length)
        (let ((body (make-array 1 :element-type '(unsigned-byte 8))))
          (read-sequence body stream))))))

(defun write-request-body (stream request)
  (let ((body (request-body request)))
    (typecase body
      (string (length (write-sequence (babel:string-to-octets body) stream)))
      (vector (length (write-sequence body stream))))))

(defun read-request (stream)
  (let ((request-line (read-request-line stream)))
    (when request-line
      (let ((request (make-instance 'request)))
        (destructuring-bind (method uri version) request-line
          (setf (request-method request) method
                (request-uri request) uri))
        (let ((request-header (read-header stream)))
          (setf (request-header request) request-header))
        (setf (request-body request) stream)
        request))))

(defgeneric write-request (stream request)
  (:method (stream (request request))
    (let ((method (request-method request))
          (uri (request-uri request))
          (header (request-header request))
          (body (request-body request)))
      (let ((request-uri (or (uri-path uri) "/")))
        (when-let ((query (uri-query uri :decode nil)))
          (setf request-uri (concatenate 'string request-uri "?" query)))
        (write-request-line stream method request-uri "HTTP/1.1")
        (write-header stream header)
        (write-request-body stream request)))))
