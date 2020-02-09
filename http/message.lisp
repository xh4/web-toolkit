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
        (when content-length (setf content-length (parse-integer content-length)))
        (alexandria::read-stream-content-into-byte-vector
         stream 'alexandria::%length content-length)))))

(defgeneric read-message-body-into-string (message)
  (:method ((message message))
    (let ((octets (read-message-body-into-vector message)))
      (babel:octets-to-string octets))))

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

(defgeneric pipe-message-body-chunks (message))

(defgeneric pipe-message-body-chunks-as-vector (message))
