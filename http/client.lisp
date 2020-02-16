(in-package :http)

(defun request (uri &key (method :get) header content)
  (let ((request (make-request uri method header content)))
    (let ((connection (open-connection uri)))
      (let ((output-stream (connection-output-stream connection))
            (input-stream (connection-input-stream connection)))
        (write-request output-stream request)
        (force-output output-stream)
        (let ((response (read-response input-stream)))
          response)))))

(defmacro define-request-method (symbol)
  `(defun ,symbol (uri)
     (request uri :method ,(make-keyword symbol))))

(define-request-method get)

(define-request-method post)

(define-request-method put)

(define-request-method delete)

(define-request-method head)

(defun make-request (uri method header content)
  (let ((request-uri (process-request-uri uri)))
    (let ((request-header (process-request-header header uri content)))
      (let ((request-body (process-request-content content)))
        (let ((request (make-instance 'request
                                      :method method
                                      :uri request-uri
                                      :version "HTTP/1.1"
                                      :header request-header
                                      :body request-body)))
          request)))))

(defun process-request-uri (uri)
  (unless (uri-host uri) (error "Missing host in URI ~S" uri))
  (typecase uri
    (uri (setf uri (uri-string uri)))
    (string uri)))

(defun process-request-header (header uri content)
  (let ((new-header (make-instance 'header)))
    (set-header-field new-header (header-field "Host" (uri-host uri)))
    (set-header-field new-header (header-field "Connection" "close"))
    ;; TODO: forbid user from setting some header fields
    (typecase header
      (header (loop for header-field in (header-fields header)
                 do (set-header-field new-header header-field)))
      (list (when header
              (loop for (name . value) in header
                 do (set-header-field new-header (header-field name value))))))
    new-header))

(defun process-request-content (content)
  )

#-lispworks
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

#+lispworks
(defun open-connection (uri)
  (let ((https-p (equal "https" (uri-scheme uri))))
    (let ((host (uri-host uri))
          (port (or (uri-port uri) (if https-p 443 80))))

      (let ((socket (comm:connect-to-tcp-server
                     host port)))
        (let ((stream (make-instance 'comm:socket-stream
                                     :socket socket
                                     :direction :io
                                     :element-type '(unsigned-byte 8))))
          (when https-p
            (comm:attach-ssl stream
                             :ssl-ctx t
                             :ssl-side :client))
          (setf (stream:stream-read-timeout stream) 10
                (stream:stream-write-timeout stream) 10)
          (let ((connection (make-instance 'connection
                                           :socket socket
                                           :input-stream stream
                                           :output-stream stream)))
            connection))))))

(defmacro with-persistent-connections ()
  )
