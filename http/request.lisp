(in-package :http)

(defclass request (message)
  ((method
    :initarg :method
    :initform nil
    :accessor request-method)
   (uri
    :initarg :uri
    :initform nil
    :accessor request-uri)
   (version
     :initarg :version
     :initform nil
     :accessor request-version)
   (header
    :initarg :header
    :initform (make-instance 'header)
    :accessor request-header)
   (body
    :initarg :body
    :initform nil
    :accessor request-body)))

(defgeneric request-method (request))

(defgeneric (setf request-method) (value request))

(defgeneric request-uri (request))

(defgeneric (setf request-uri) (value request))

(defmethod uri-scheme ((request request))
  (uri-scheme (request-uri request)))

(defmethod uri-userinfo ((request request))
  (uri-userinfo (request-uri request)))

(defmethod uri-host ((request request) &key decode)
  (uri-host (request-uri request) :decode decode))

(defmethod uri-port ((request request))
  (uri-port (request-uri request)))

(defmethod uri-path ((request request) &key decode)
  (uri-path (request-uri request) :decode decode))

(defmethod uri-query ((request request) &key (type :alist) (decode nil))
  (uri-query (request-uri request) :type type :decode decode))

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

(defmethod find-header-field (name (request request))
  (find-header-field name (request-header request)))

(defmethod set-header-field ((request request) header-field)
  (set-header-field (request-header request) header-field))

(defparameter *methods* '(:get :post :put :delete :head :patch :options))

(defgeneric request-body-present-p (request)
  (:method ((request request))
    (message-body-present-p request)))

(defun read-request-line (stream &key (parse t))
  (let ((line (handler-case
                  (read-line stream)
                ((or end-of-file #-:lispworks usocket:timeout-error) nil))))
    (unless (emptyp line)
      (if parse
          (parse-request-line line)
          line))))

(defun parse-request-line (line)
  (let ((result (cl-ppcre:split "\\s+" line :limit 3)))
    (if (= (length result) 3)
        result
        nil)))

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

(defun read-request-body (stream request)
  (let ((request-header (request-header request)))
    (let ((content-length (header-field-value
                           (find-header-field "Content-Length" request-header))))
      (if content-length
          (setf content-length (parse-integer content-length))
          (setf content-length 0))
      (when (plusp content-length)
        (let ((body (make-array 1 :element-type '(unsigned-byte 8))))
          (read-sequence body stream))))))

(defgeneric read-request-body-into-vector (request)
  (:method ((request request))
    (read-message-body-into-vector request)))

(defgeneric read-request-body-into-temporary-file (request)
  (:method ((request request))
    (read-message-body-into-temporary-file request)))

(defgeneric pipe-reqeust-body-chunks (request)
  (:method ((request request))
    (pipe-message-body-chunks request)))

(defgeneric pipe-request-body-chunks-as-vector (request)
  (:method ((request request))
    (pipe-message-body-chunks-as-vector request)))

(defgeneric read-request-form-data (request)
  (:method ((request request))
    (let ((content-type (header-field-value
                         (find-header-field "Content-Type" request))))
      (cond
        ((search "application/x-www-form-urlencoded" content-type)
         (read-request-urlencoded-form-data request))
        ((search "multipart/form-data" content-type)
         (read-request-multipart-form-data request))
        (t (error "Request does not carry form data"))))))

(defun read-request-urlencoded-form-data (request)
  (let ((data (babel:octets-to-string
               (read-request-body-into-vector request))))
    (let ((uri (uri :query data)))
      ;; FIXME: Handle `+` character
      (let ((alist (uri-query uri :type :alist)))
        (let ((form (make-instance 'form)))
          (loop for (name . value) in alist
             do (appendf (form-fields form)
                         (list (form-field name value))))
          form)))))

(defun read-request-multipart-form-data (request)
  (let* ((content-type (header-field-value
                        (find-header-field "Content-Type" request)))
         (boundary (first
                   (coerce
                    (nth-value
                     1
                     (cl-ppcre:scan-to-strings
                      "boundary=(.*)"
                      content-type))
                    'list))))
    (let ((stream (request-body request)))
      (handler-bind ()
        (let ((alist (read-multipart-form-data stream boundary)))
          (let ((form (make-instance 'form)))
            (loop for (name . value) in alist
               do (appendf (form-fields form)
                           (list (form-field name value))))
            form))))))

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
                (request-uri request) uri
                (request-version request) version))
        (let ((request-header (read-header stream)))
          (setf (request-header request) request-header))
        (if (or (message-body-present-p request)
                (search "close" (header-field-value
                                 (find-header-field "Connection" request))))
            (if (transfer-encoding-chunked-p request)
                (setf (request-body request)
                      (make-instance 'stream :upstream (chunga:make-chunked-stream stream)))
                (when-let ((content-length (header-field-value
                                            (find-header-field "Content-Length" request))))
                  (progn (setf content-length (parse-integer content-length))
                         (setf (request-body request)
                               (make-instance 'stream :upstream stream :length content-length)))))
            (setf (request-body request) stream))
        request))))

(defgeneric write-request (stream request)
  (:method (stream (request request))
    (let ((method (request-method request))
          (uri (request-uri request))
          (version (request-version request))
          (header (request-header request)))
      (let ((request-uri (uri-string uri :path (or (uri-path uri) "/"))))
        (write-request-line stream method request-uri version)
        (write-header stream header)
        (write-request-body stream request)))))
