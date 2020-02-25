(in-package :http)

(defclass message ()
  ((header
    :initarg :header
    :initform nil
    :accessor message-header)
   (body
    :initarg :body
    :initform nil
    :accessor message-body)))

(defgeneric message-body-present-p (message)
  (:method ((message message))
    (or (find-header-field "Content-Length" message)
        (find-header-field "Transfer-Encoding" message))))

(defgeneric transfer-encoding-chunked-p (message)
  (:method ((message message))
    (search "chunked" (header-field-value
                       (find-header-field "Transfer-Encoding" message)))))

(defgeneric read-message-body-into-vector (message)
  (:method ((message message))
    (let ((stream (message-body message)))
      (check-type stream cl:stream)
      (let ((content-length (header-field-value
                             (find-header-field "Content-Length" message))))
        ;; TODO: read chunked stream
        (unless content-length
          (error "Missing content length when read message body into vector"))
        (setf content-length (parse-integer content-length))
        (alexandria::read-stream-content-into-byte-vector
         stream 'alexandria::%length content-length)))))

(defgeneric read-message-body-into-string (message)
  (:method ((message message))
    (let ((header (message-header message))
          (octets (read-message-body-into-vector message)))
      (let ((content-type (find-header-field "Content-Type" header)))
        ;; TODO: respect charset
        (babel:octets-to-string octets)))))

(defgeneric read-message-body-into-temporary-file (message)
  (:method ((message message))
    (let ((stream (message-body message)))
      (check-type stream cl:stream)
      (let ((content-length (header-field-value
                             (find-header-field "Content-Length" message))))
        (when content-length (setf content-length (parse-integer content-length)))
        (uiop:with-temporary-file (:stream output-stream
                                   :pathname pathname
                                   :keep t
                                   :element-type '(unsigned-byte 8))
          (alexandria::copy-stream stream output-stream :end content-length :finish-output t)
          pathname)))))

;; TODO: implement this
(defgeneric pipe-message-body-chunks (message))

;; TODO: implement this
(defgeneric pipe-message-body-chunks-as-vector (message))

(defgeneric write-message-header (stream message)
  (:method (stream (message message))
    (let ((header (message-header message)))
      (write-header stream header))))

(defgeneric write-message-body (stream message)
  (:method (stream (message message))
    (let ((body (message-body message)))
      (typecase body
        (vector (length (write-sequence body stream)))
        (pathname (with-open-file (input-stream body :element-type '(unsigned-byte 8))
                    (alexandria::copy-stream input-stream stream)))
        (null 0)
        (t (error "Unable to write body of type ~A" (type-of body)))))))
