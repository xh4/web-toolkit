(in-package :http)

(defclass response ()
  ((status
    :initarg :status
    :initform nil
    :accessor response-status)
   (header
    :initarg :header
    :initform (make-instance 'header)
    :accessor response-header)
   (body
    :initarg :body
    :initform nil
    :accessor response-body)))

(defvar *response* nil)

(defgeneric response-status (response))

(defgeneric (setf response-status) (value response))

(defgeneric response-header (response))

(defgeneric (setf response-header) (value response))

(defgeneric response-body (response))

(defgeneric (setf response-body) (value response))

(defmethod (setf response-status) ((value integer) response)
  (let ((status (gethash value
                         *status-code-mapping-table*)))
    (setf (response-status response) status)))

(defmethod (setf response-status) ((value symbol) response)
  (let ((status (gethash (make-keyword value)
                         *status-keyword-mapping-table*)))
    (setf (response-status response) status)))

(defmethod (setf response-status) ((value status) response)
  (setf (slot-value response 'status) value))

(defmethod header-fields ((response response))
  (let ((header (response-header response)))
    (header-fields header)))

(defmethod (setf header-fields) (value (response response))
  (let ((header (response-header response)))
    (setf (header-fields header) value)))

(defmethod find-header-field (name (response response))
  (find-header-field name (response-header response)))

(defmethod set-header-field ((response response) header-field)
  (set-header-field (response-header response) header-field))

(defun read-status-line (stream &key (parse t))
  (let ((line (read-line stream)))
    (unless (emptyp line)
      (if parse
          (parse-status-line line)
          line))))

(defun parse-status-line (line)
  (let ((result (cl-ppcre:split "\\s+" line :limit 3)))
    (if (= (length result) 3)
        (destructuring-bind (http-version status-code reason-phase) result
          (list http-version (parse-integer status-code) reason-phase))
        nil)))

(defun write-status-line (stream http-version status-code reason-phase)
  (check-type http-version string)
  (check-type status-code (or string integer))
  (check-type reason-phase string)
  (let ((line (format nil "~A ~A ~A" http-version status-code reason-phase)))
    (write-sequence (babel:string-to-octets line) stream)
    (write-sequence +crlf+ stream)
    (+ (length line) (length +crlf+))))

(defun write-response-header (stream response)
  (let ((header (response-header response)))
    (write-header stream header)))

(defun read-response-body (stream header)
  (let ((content-length (header-field-value
                         (find-header-field "Content-Length" header))))
    (when content-length (setf content-length (parse-integer content-length)))
    (when (and content-length (plusp content-length))
      (alexandria::read-stream-content-into-byte-vector
       stream 'alexandria::%length content-length))))

(defgeneric write-response-body (stream response)
  (:method (stream (response response))
    (let ((body (response-body response)))
      (typecase body
        (string (length (write-sequence (babel:string-to-octets body) stream)))
        (vector (length (write-sequence body stream)))
        (t 0)))))

(defun read-response (stream)
  (let ((status-line (read-status-line stream)))
    (let ((header (read-header stream)))
      (let ((body (read-response-body stream header)))
        (let ((status-code (second status-line)))
          (make-instance 'response
                         :status status-code
                         :header header
                         :body body))))))

(defgeneric write-response (stream response)
  (:method (stream (response response))
    (+
     (write-status-line stream "HTTP/1.1"
                        (status-code (response-status response))
                        (status-reason-phrase (response-status response)))
     (write-response-header stream response)
     (write-response-body stream response))))
