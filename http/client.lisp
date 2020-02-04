(in-package :http)

(defun request (uri &key (method :get) header content)
  (let ((request-uri (process-request-uri uri)))
    (let ((request-header (process-request-header header content)))
      (let ((request-body (process-request-content content)))
        (let ((request (make-instance 'request
                                      :method method
                                      :uri request-uri
                                      :header request-header
                                      :body request-body)))
          (let ((connection (open-connection uri)))
            (let ((output-stream (connection-output-stream connection))
                  (input-stream (connection-input-stream connection)))
              (write-request output-stream request)
              (force-output output-stream)
              (let ((response (read-response input-stream)))
                response))))))))

(defmacro define-http-request-method (symbol)
  `(defun ,symbol (uri)
     (request uri :method ,(make-keyword symbol))))

(define-http-request-method get)

(define-http-request-method post)

(define-http-request-method put)

(define-http-request-method delete)

(define-http-request-method head)

(defun process-request-uri (uri)
  (typecase uri
    (uri (setf uri (uri-string uri)))
    (string uri)))

(defun process-request-header (header content)
  (make-instance 'header))

(defun process-request-content (content)
  )

(defun open-connection (uri)
  (let ((host (uri-host uri))
        (port (or (uri-port uri) 80)))
    (unless host (error "Missing host in URI"))
    (let ((socket (usocket:socket-connect
                   host
                   port
                   :element-type '(unsigned-byte 8))))
      (let ((connection (make-connection socket)))
        connection))))

(defmacro with-persistent-connections ()
  )
